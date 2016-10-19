(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE FtpCommands;

        (********************************************************)
        (*                                                      *)
        (*       Command interpreter for ftp server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            23 August 1997                  *)
        (*  Last edited:        2 July 2015                     *)
        (*  Status:             All commands now implemented    *)
        (*                                                      *)
        (********************************************************)

        (********************************************************)
        (* UTF-8 TRANSLATION STATUS:                            *)
        (*                                                      *)
        (*   Have converted all commands.                       *)
        (*                                                      *)
        (********************************************************)

(********************************************************************************)
(*                        COMPLIANCE WITH THE STANDARDS                         *)
(********************************************************************************)
(*                                                                              *)
(* The type/mode/structure options I've actually implemented are:               *)
(*                                                                              *)
(*       TYPE - ASCII Non-print (default), UNICODE, IMAGE, LOCAL 8              *)
(*       MODE - Stream (default)                                                *)
(*       STRUCTURE - File (default), Record                                     *)
(*                                                                              *)
(* This meets the minimum requirements specified in RFC959 and RFC1123.         *)
(*                                                                              *)
(* I have now implemented all commands in the standard RFC 959, as amended by   *)
(* RFC 1123, except for the type/mode/structure combinations that are relevant  *)
(* to computer systems (e.g. PDP-10) that are no longer supported.              *)
(*                                                                              *)
(* In addition we support RFC 2389 (FEAT and OPTS) and RFC 2640 (LANG and       *)
(* UTF-8 support) and much of RFC 3659 (MDTM, SIZE, REST, TVFS).                *)
(*                                                                              *)
(* The commmands I have implemented and tested are:                             *)
(*                                                                              *)
(*      ABOR, ACCT, ALLO, APPE, CDUP, CWD, DELE, FEAT, HELP, LANG, LIST, MDTM,  *)
(*      MKD, MODE (S only), NLST, NOOP, OPTS, PASS, PASV, PORT, PWD, QUIT,      *)
(*      REIN, REST, RETR, RMD, RNFR, RNTO, SITE, SIZE, SMNT, STAT, STOR, STOU,  *)
(*      STRU (F or R), SYST, TYPE (I or A or L 8 or U), USER                    *)
(*                                                                              *)
(* I've also implemented the obsolete commands XCUP, XCWD, XMKD, XPWD, and      *)
(* XRMD, which are used by some pre-1985 clients (e.g. Windows XP).             *)
(*                                                                              *)
(* A draft standard covering TYPE U exists with the name                        *)
(*         draft-klensin-ftp-typeu-00                                           *)
(* The draft expires in January 2009.                                           *)
(*                                                                              *)
(* Also added: SITE UTIME   (not covered by a standard)                         *)
(*             HOST   (RFC 7151)                                                *)
(*                                                                              *)
(********************************************************************************)

FROM SYSTEM IMPORT CARD8, CARD16, CARD32, CAST, ADDRESS, ADR;

IMPORT Strings, IOChan, ChanConsts, RndFile, OS2, FileOps;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64, Max64,
    (* proc *)  Compare64;

FROM Sockets IMPORT
    (* type *)  Socket, SockAddr,
    (* proc *)  send;

FROM Internet IMPORT
    (* type *)  InternetSocketAddress;

FROM InetUtilities IMPORT
    (* proc *)  Synch, ToLower, ConvertCard64, AddEOL;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL, ConvertCard;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  SetProcname, OpenLogContext, SetSyslogHost,
                StartTransactionLogging, LogTransaction, LogTransactionL;

FROM LoggedOnUsers IMPORT
    (* type *)  ClientDataPointer,
    (* proc *)  StartSession, EndSession, RememberCommand, CommandDone,
                ListAllUsers, NewUser, RemoveUser, AbortDataOperations,
                KillUser, CheckForHammerer, AddressForbidden;

FROM CodePage IMPORT
    (* proc *)  TranslateFromUTF8, TranslateToUTF8;

FROM FtpTransfers IMPORT
    (* type *)  ClientFileInfo,
    (* proc *)  SetLogLevel, SetHost, PasswordOK, RecordLoginTime,
                SendMessageFile, SendDirectoryMessage,
                SetPort, SetTransferType, SetTransferMode, EnterPassiveMode,
                SetFileStructure, SetDirectory, SetFileName,
                ListingIsPossible, SendDirectory, FileAvailable, IsDirectory,
                UnacceptablePort, FileOrDirectoryVisible, SendFile,
                CanWrite, AcceptFile, AppendFile, CanDelete, DeleteFile,
                RenameTo, SetRestartPoint, CanRemoveDirectory,
                DeleteDirectory, MakeDirectory, GetFileDate, GetSize,
                GetSizeTypeAndName, GetSizeTypeAndNameU, ResetCount, SetFileDateTime,
                CurrentDirectory, RealCurrentDirectory,
                GetCurrentPermissions, AllocateFileSize,
                CreateUniqueFileName, CloseDataPort, SendUserStatus;

FROM FDUsers IMPORT
    (* type *)  UserCategory;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* proc *)  OpenNewFile1, CloseFile;

FROM SplitScreen IMPORT
    (* proc *)  ReleaseScreen, RegainScreen;

FROM MyClock IMPORT
    (* proc *)  StringToPackedDateTime;

FROM Names IMPORT
    (* type *)  FilenameString, FilenameIndex;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    Digits = CharSet{'0'..'9'};

TYPE
    FourChar = ARRAY [0..3] OF CHAR;

    ClientState = (Idle, WaitingForPassword, LoggedIn, CommFailure, MustExit);

    (* The session record.  The fields are:                             *)
    (*     logID       Identifier for transaction logging               *)
    (*     ClientData  Structure set up on opening a session to keep    *)
    (*                 track of the current state of this session.      *)
    (*     user        User handle that is given to us by StartSession  *)
    (*                 and that can be used in FtpTransfer calls.       *)
    (*     socket      The command socket                               *)
    (*     usernumber  A number to use in welcome messages.             *)
    (*     LoginAttempts                                                *)
    (*                 How many times this user has attempted to log in *)
    (*                    during this session.                          *)
    (*     state       To track whether the user is currently logged in.*)
    (*     userOK      TRUE iff a valid username has been received      *)
    (*                    (but we don't tell the client this, we wait   *)
    (*                    for the PASS command before rejecting him).   *)
    (*     IsManager   TRUE iff this user is a manager.                 *)
    (*     MSGenabled  TRUE iff this user is willing to accept MSG files.*)
    (*     WasLogging  Old value of Logit                               *)
    (*     LogIt       TRUE iff we want to log this session in the      *)
    (*                     transaction log.                             *)

    Session = POINTER TO
                  RECORD
                      logID: TransactionLogID;
                      ClientData: ClientDataPointer;
                      user: ClientFileInfo;
                      socket: Socket;
                      usernumber: CARDINAL;
                      LoginAttempts: CARD8;
                      state: ClientState;
                      userOK, IsManager, MSGenabled, WasLogging, LogIt: BOOLEAN;
                  END (*RECORD*);

    (* Note that both the ClientData and the user components are        *)
    (* created by OpenSession, and they both remain valid until we call *)
    (* CloseSession.  When the client credentials change, as for        *)
    (* example when a PASS command is received, the user details must   *)
    (* be updated by calls to RemoveUser and/or NewUser as needed.      *)

    (* Commands, for the command parser. *)

    CmdType = (unknown, notloggedin,
               abor, acct, allo, appe, cdup, cwd, dele, feat, help, host, lang, list,
               mdtm, mkd, mode, nlst, noop, opts, pasw, pass, pasv, port, pwd,
               quit, rein, rest, retr, rmd, rnfr, rnto, site, size, smnt, stat,
               stor, stou, stru, syst, type, user, xcup, xcwd, xmkd, xpwd, xrmd);

    KeywordArray = ARRAY CmdType OF FourChar;

(********************************************************************************)

CONST
    KeywordList = KeywordArray {'????', '????',
                               'ABOR', 'ACCT', 'ALLO', 'APPE', 'CDUP', 'CWD ',
                               'DELE', 'FEAT', 'HELP', 'HOST', 'LANG', 'LIST',
                               'MDTM', 'MKD ', 'MODE', 'NLST', 'NOOP', 'OPTS',
                               'P@SW', 'PASS', 'PASV', 'PORT',
                               'PWD ', 'QUIT', 'REIN', 'REST', 'RETR', 'RMD ',
                               'RNFR', 'RNTO', 'SITE', 'SIZE', 'SMNT', 'STAT',
                               'STOR', 'STOU', 'STRU', 'SYST', 'TYPE', 'USER',
                               'XCUP', 'XCWD', 'XMKD', 'XPWD', 'XRMD'};

(********************************************************************************)

VAR
    (* Option to have more detailed transaction logging, for debugging. *)

    ExtraLogging: CARDINAL;

    (* Option to log the SITE MNGR commands. *)

    LogSITEMNGR: BOOLEAN;

    (* Option to hide passwords in the transaction log. *)

    HidePasswords: BOOLEAN;

    (* Next file name for a temporary file. *)

    NextName: FilenameString;

    (* Mutex for NextName manipulation. *)

    NextNameLock: Lock;

    (* Mutex to serialise execution of child processes. *)

    ExecLock: Lock;

    (* Event semaphore by which an external program requests a shutdown. *)

    ExternalShutdownRequest: OS2.HEV;

(********************************************************************************)
(*                           TRANSACTION LOGGING                                *)
(********************************************************************************)

PROCEDURE SetTransactionLogLevel (level: CARDINAL;
                                  VAR (*IN*) filename: ARRAY OF CHAR;
                                  VAR (*IN*) SyslogHost: ARRAY OF CHAR);

    (* Option to control transaction logging: 0 for none, 1 for disk,   *)
    (* 2 for screen, 4 for pipe, 8 for syslog, and combinations.        *)

    BEGIN
        IF SyslogHost[0] <> Nul THEN
            SetSyslogHost (SyslogHost);
        END (*IF*);
        StartTransactionLogging (FTPDctx, filename, level);
    END SetTransactionLogLevel;

(********************************************************************************)

PROCEDURE HidePasswordsInLog (hide: BOOLEAN);

    (* If hide is TRUE, we log the PASS command as PASS ******.  *)

    BEGIN
        HidePasswords := hide;
    END HidePasswordsInLog;

(********************************************************************************)

PROCEDURE SetExtraLogging (level: CARDINAL);

    (* Option to control how detailed the transaction logging is: 0 for normal, *)
    (* higher values for more detail, but the actual significance of the        *)
    (* level could vary from time to time, because this option is intended      *)
    (* for debugging.                                                           *)

    BEGIN
        ExtraLogging := level;
    END SetExtraLogging;

(************************************************************************)

PROCEDURE SuppressLogging (VAR (*INOUT*) sess: Session);

    (* Temporarily turns off logging. *)

    BEGIN
        sess^.WasLogging := sess^.LogIt;
        sess^.LogIt := FALSE;
    END SuppressLogging;

(************************************************************************)

PROCEDURE RestoreLogging (VAR (*INOUT*) sess: Session);

    (* Reverses the effect of the SuppressLogging call. *)

    BEGIN
        sess^.LogIt := sess^.WasLogging;
    END RestoreLogging;

(********************************************************************************)
(*                         STARTING A NEW SESSION                               *)
(********************************************************************************)

PROCEDURE OpenSession (CommandSocket: Socket;  UserNumber, IPaddr: CARDINAL;
                         LogID: TransactionLogID;  KeepAlive: Semaphore): Session;

    (* Creates a new session state record.  During lengthy operations           *)
    (* we have to do a Signal(KeepAlive) every so often in order to stop the    *)
    (* session from timing out.                                                 *)

    VAR result: Session;

    BEGIN
        NEW (result);
        WITH result^ DO
            logID := LogID;
            ClientData := StartSession (CommandSocket, UserNumber,
                                        IPaddr, LogID, KeepAlive, user);
            EVAL (SetHost (user, ""));
            socket := CommandSocket;
            state := Idle;
            usernumber := UserNumber;
            LoginAttempts := 0;
            userOK := FALSE;
            IsManager := FALSE;
            MSGenabled := TRUE;
            WasLogging := TRUE;
            LogIt := TRUE;
        END (*WITH*);
        RETURN result;
    END OpenSession;

(********************************************************************************)

PROCEDURE CloseSession (VAR (*INOUT*) S: Session);

    (* Destroys the session state record. *)

    BEGIN
        IF S <> NIL THEN
            IF S^.state <> Idle THEN
                RemoveUser (S^.ClientData);
            END (*IF*);
            EndSession (S^.ClientData);
            DISPOSE (S);
        END (*IF*);
    END CloseSession;

(********************************************************************************)

PROCEDURE KillDataChannel (S: Session);

    (* Aborts the data transfer, if any, now in progress for this session. *)

    BEGIN
        IF S <> NIL THEN

            (* BUG: sometimes we are passing an invalid pointer as              *)
            (* S^.ClientData.  It looks to me as if S itself is valid, but      *)
            (* maybe not.                                                       *)

            AbortDataOperations (S^.ClientData);
        END (*IF*);
    END KillDataChannel;

(********************************************************************************)

PROCEDURE RequestShutdown;

    (* Used by the SITE MNGR EXIT and SITE MNGR GXIT commands.  The server      *)
    (* sends a signal to itself to shut down.                                   *)

    CONST semName = "\SEM32\FTPSERVER\SHUTDOWN";

    BEGIN
        IF OS2.DosOpenEventSem (semName, ExternalShutdownRequest) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ExternalShutdownRequest, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (ExternalShutdownRequest);
        OS2.DosCloseEventSem (ExternalShutdownRequest);
    END RequestShutdown;

(********************************************************************************)
(*                       SENDING REPLY BACK TO CLIENT                           *)
(********************************************************************************)

PROCEDURE Reply2 (session: Session;  message1, message2: ARRAY OF CHAR);

    (* Sends all of message1, followed by message2, followed by end-of-line.    *)
    (* If the operation fails, session^.state is set to CommFailure.            *)

    VAR buffer: ARRAY [0..511] OF CHAR;  length: CARDINAL;

    BEGIN
        Strings.Assign (message1, buffer);
        Strings.Append (message2, buffer);
        IF session^.LogIt THEN
            LogTransaction (session^.logID, buffer);
        END (*IF*);
        length := AddEOL (buffer);
        IF send (session^.socket, buffer, length, 0) = MAX(CARDINAL) THEN
            session^.state := CommFailure;
        END (*IF*);
        (*Synch (session^.socket);*)
    END Reply2;

(********************************************************************************)

PROCEDURE Reply3 (session: Session;  message1, message2, message3: ARRAY OF CHAR);

    (* Like Reply2, but with an extra string message3 before the end-of-line.   *)

    VAR buffer: ARRAY [0..511] OF CHAR;  length: CARDINAL;

    BEGIN
        Strings.Assign (message1, buffer);
        Strings.Append (message2, buffer);
        Strings.Append (message3, buffer);
        IF session^.LogIt THEN
            LogTransaction (session^.logID, buffer);
        END (*IF*);
        length := AddEOL (buffer);
        IF send (session^.socket, buffer, length, 0) = MAX(CARDINAL) THEN
            session^.state := CommFailure;
        END (*IF*);
        (*Synch (session^.socket);*)
    END Reply3;

(********************************************************************************)

PROCEDURE Reply (session: Session;  message: ARRAY OF CHAR);

    (* Like Reply2, except that there is no message2. *)

    VAR buffer: ARRAY [0..511] OF CHAR;  length: CARDINAL;

    BEGIN
        Strings.Assign (message, buffer);
        IF session^.LogIt THEN
            LogTransaction (session^.logID, buffer);
        END (*IF*);
        length := AddEOL (buffer);
        IF send (session^.socket, buffer, length, 0) = MAX(CARDINAL) THEN
            session^.state := CommFailure;
        END (*IF*);
        (*Synch (session^.socket);*)
    END Reply;

(********************************************************************************)
(*                              COMMAND PARSER                                  *)
(********************************************************************************)

PROCEDURE ParseCommand (VAR (*INOUT*) Command: ARRAY OF CHAR): CmdType;

    (* Works out what the command is.  On return, Command holds the command     *)
    (* parameters, if any, or is unchanged if the command is unrecognised.      *)

    VAR k: [0..3];

    (****************************************************************************)

    PROCEDURE Compare4 (n: CmdType): INTEGER;

        (* Compares the first four characters of Command with KeywordList[n].   *)
        (* Returns >0 if Command[0..3] > KeywordList[n], and so on.             *)

        VAR ch1, ch2: CHAR;

        BEGIN
            k := 0;
            LOOP
                ch1 := Command[k];  ch2 := KeywordList[n][k];
                IF ch1 > ch2 THEN RETURN +1
                ELSIF ch1 < ch2 THEN RETURN -1
                ELSIF k = 3 THEN RETURN 0
                END (*IF*);
                INC (k);
            END (*LOOP*);
        END Compare4;

    (****************************************************************************)

    VAR m: CARDINAL;  test: INTEGER;
        first, result, last: CmdType;

    BEGIN
        (* Special case: add a space to a three-character command, to make the  *)
        (* search algorithm simpler.  This won't interfere with parameter       *)
        (* handling, because such commands don't have parameters.               *)

        IF Command[3] = Nul THEN
            Command[3] := ' ';  Command[4] := Nul;
        END (*IF*);

        (* Watch out for lower case. *)

        FOR k := 0 TO 3 DO
            Command[k] := CAP(Command[k]);
        END (*FOR*);

        (* Go through the keyword list to find a match with the command.  *)
        (* In this version I'm using a binary search.                     *)

        first := MIN(CmdType);  last := MAX(CmdType);
        REPEAT
            result := VAL (CmdType, (ORD(first) + ORD(last)) DIV 2);
            test := Compare4 (result);
            IF test < 0 THEN
                IF result = MIN(CmdType) THEN
                    test := 0;    (* A 'match' on the command 'unknown' *)
                ELSE
                    last := result;  DEC(last);
                END (*IF*);
            ELSIF test > 0 THEN
                first := result;
                IF first = MAX(CmdType) THEN
                    DEC (last);       (* to force loop exit *)
                ELSE
                    INC (first);
                END (*IF*);
            END (*IF*);
        UNTIL (test = 0) OR (first > last);

        IF test <> 0 THEN
            result := unknown;
        END (*IF*);

        (* Strip out the command characters and precisely one following *)
        (* space, leaving only the parameters.                          *)

        IF result <> unknown THEN
            IF Command[3] = ' ' THEN m := 4;
            ELSE m := 5;
            END (*IF*);
            Strings.Delete (Command, 0, m);
        END (*IF*);

        RETURN result;

    END ParseCommand;

(********************************************************************************)
(*                     HANDLERS FOR SOME ERROR CONDITIONS                       *)
(********************************************************************************)

PROCEDURE NotImplemented (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Default handler for anything I haven't yet implemented. *)

    BEGIN
        Reply2 (session, "502 Not implemented ", Command);
    END NotImplemented;

(********************************************************************************)

PROCEDURE NoSuchCommand (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Command is not a recognised command. *)

    BEGIN
        Reply2 (session, "500 Unknown command ", Command);
    END NoSuchCommand;

(********************************************************************************)

PROCEDURE NotLoggedIn (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Command is illegal because user is not yet logged in. *)

    BEGIN
        Reply2 (session, "530 Not logged in ", Command);
    END NotLoggedIn;

(************************************************************************)
(*                    EXECUTING AN EXTERNAL PROGRAM                     *)
(************************************************************************)

PROCEDURE IncrementName (VAR (*INOUT*) current: FilenameString;  N: CARDINAL);

    (* Changes current to a new file name. *)

    BEGIN
        IF N = 0 THEN
            current := "00000000";
        ELSIF current[N] = '9' THEN
            current[N] := 'A';
        ELSIF current[N] = 'Z' THEN
            IncrementName (current, N-1);
        ELSE
            INC (current[N]);
        END (*IF*);
    END IncrementName;

(********************************************************************************)

PROCEDURE CreateCommandFile (VAR (*OUT*) name: FilenameString): FileOps.ChanId;

    (* Creates a new ????????.cmd file. *)

    VAR cid: FileOps.ChanId;
        duplicate: BOOLEAN;

    BEGIN
        REPEAT
            Obtain (NextNameLock);
            name := NextName;
            Strings.Append (".cmd", name);
            cid := OpenNewFile1 (name, duplicate);
            IncrementName(NextName, LENGTH(NextName) - 1);
            Release (NextNameLock);
        UNTIL NOT duplicate;
        RETURN cid;
    END CreateCommandFile;

(********************************************************************************)

PROCEDURE ExecProg (session: Session;
                    VAR (*IN*) ProgName, Params: ARRAY OF CHAR);

    (* This procedure executes the specified program on behalf of the   *)
    (* client.  The client thread remains blocked until the command     *)
    (* completes.  This procedure provides the response to the client.  *)

    CONST ONLength = 256;

    VAR result, pos: CARDINAL;
        ExitStatus: OS2.RESULTCODES;
        ArgString, cmdfilename: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        f: FileOps.ChanId;

    BEGIN
        IF ProgName[0] = Nul THEN
            Reply (session, "501 Nothing to execute.");
            RETURN;
        END (*IF*);

        (* Create a temporary command file, and put commands into it   *)
        (* to set the directory and then invoke the program.           *)

        f := CreateCommandFile(cmdfilename);
        IF f = NoSuchChannel THEN
            Reply (session, "451 Cannot create command file.");
            RETURN;
        END (*IF*);

        RealCurrentDirectory (session^.user, ArgString);
        IF ArgString[0] <> Nul THEN
            IF ArgString[1] = ':' THEN
                FileOps.FWriteChar (f, '@');
                FileOps.FWriteChar (f, ArgString[0]);
                FileOps.FWriteChar (f, ':');
                FileOps.FWriteLn (f);
                Strings.Delete (ArgString, 0, 2);
            END (*IF*);
            FileOps.FWriteString (f, "@CD ");
            IF ArgString[0] = Nul THEN
                ArgString := "\";
            END (*IF*);
            FileOps.FWriteString (f, ArgString);
            FileOps.FWriteLn (f);
        END (*IF*);
        FileOps.FWriteChar (f, '@');
        FileOps.FWriteString (f, ProgName);
        IF Params[0] <> Nul THEN
            FileOps.FWriteChar (f, " ");
            FileOps.FWriteString (f, Params);
        END (*IF*);
        FileOps.FWriteLn (f);
        CloseFile (f);

        ArgString := "CMD /C ";
        Strings.Append (cmdfilename, ArgString);

        (* Special rule for ArgString: it must be terminated by two Nul *)
        (* characters, and the program name and arguments must also be  *)
        (* separated by a Nul.  We have to insert the separating Nul    *)
        (* after everything else has been done, otherwise it would mess *)
        (* up the Strings.Append operation.                             *)

        pos := LENGTH(ArgString) + 1;
        IF pos <= MAX(FilenameIndex) THEN
            ArgString[pos] := Nul;
        END (*IF*);
        ArgString[3] := Nul;

        (* Execute the command file. *)

        Obtain (ExecLock);
        ReleaseScreen;
        result := OS2.DosExecPgm (FailureObjectName, ONLength,
                                  OS2.EXEC_SYNC, ArgString, NIL,
                                  ExitStatus, "CMD.EXE");
        Release (ExecLock);
        RegainScreen;

        IF (result = 0) OR (result = 457) THEN

            (* Starting in background (code 457) is not an error. *)

            result := ExitStatus.codeResult;
            ArgString := "200 Program terminated with result ";
        ELSE
            ArgString := "200 Command failed, exit status ";
        END (*IF*);

        pos := LENGTH (ArgString);
        ConvertCard (result, ArgString, pos);
        ArgString[pos] := Nul;

        (* Delete the temporary command file. *)

        FileOps.DeleteFile (cmdfilename);

        Reply (session, ArgString);

    END ExecProg;

(********************************************************************************)

(*
PROCEDURE OldExecProg (session: Session;
                    VAR (*IN*) ProgName, Params: ARRAY OF CHAR);

    (* This procedure executes the specified program on behalf of the   *)
    (* client.  The client thread remains blocked until the command     *)
    (* completes.  This procedure provides the response to the client.  *)

    CONST ONLength = 256;

    TYPE
        QDataType = RECORD
                        sessID, procID: OS2.USHORT;
                    END (*RECORD*);

    VAR rc: CARDINAL;
        CmdName, ArgString, cmdfilename: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        QName: ARRAY [0..32] OF CHAR;
        StartData: OS2.STARTDATA;
        idSession, length, pos: CARDINAL;
        priority: CARD8;
        pQData: POINTER TO QDataType;
        pid: OS2.PID;
        f: FileOps.ChanId;
        hq: OS2.HQUEUE;
        Request: OS2.REQUESTDATA;
        ptib: OS2.PTIB;
        ppib: OS2.PPIB;

    BEGIN
        IF ProgName[0] = Nul THEN
            Reply (session, "501 Nothing to execute.");
            RETURN;
        END (*IF*);

        (* Create a temporary command file, and put commands into it   *)
        (* to set the directory and then invoke the program.           *)

        f := CreateCommandFile(cmdfilename);
        IF f = NoSuchChannel THEN
            Reply (session, "451 Cannot create command file.");
            RETURN;
        END (*IF*);

        RealCurrentDirectory (session^.user, ArgString);
        IF ArgString[0] <> Nul THEN
            IF ArgString[1] = ':' THEN
                FileOps.FWriteChar (f, ArgString[0]);
                FileOps.FWriteChar (f, ':');
                FileOps.FWriteLn (f);
                Strings.Delete (ArgString, 0, 2);
            END (*IF*);
            FileOps.FWriteString (f, "CD ");
            IF ArgString[0] = Nul THEN
                ArgString := "\";
            END (*IF*);
            FileOps.FWriteString (f, ArgString);
            FileOps.FWriteLn (f);
        END (*IF*);
        FileOps.FWriteString (f, ProgName);
        IF Params[0] <> Nul THEN
            FileOps.FWriteChar (f, " ");
            FileOps.FWriteString (f, Params);
        END (*IF*);
        FileOps.FWriteLn (f);

        CloseFile (f);

        CmdName := "CMD.EXE";
        ArgString := "/C ";
        Strings.Append (cmdfilename, ArgString);

        (* Create the queue that will allow us to find out when the     *)
        (* child process terminates.  Note that we need exclusive       *)
        (* access both when using this queue and when running the       *)
        (* child process.                                               *)

        Obtain (ExecLock);
        QName := "\queues\ftpserver.que";
        rc := OS2.DosCreateQueue (hq, OS2.QUE_FIFO + OS2.QUE_CONVERT_ADDRESS,
                                           QName);

        (* Execute the command file. *)

        WITH StartData DO
            Length     :=  SIZE(OS2.STARTDATA);
            Related    :=  OS2.SSF_RELATED_CHILD;
            FgBg       :=  OS2.SSF_FGBG_FORE;
            TraceOpt   :=  OS2.SSF_TRACEOPT_NONE;
            PgmTitle   :=  NIL;
            PgmName    :=  ADR(CmdName);
            PgmInputs  :=  ADR(ArgString);
            TermQ      :=  ADR(QName);
            Environment:=  NIL;
            InheritOpt :=  OS2.SSF_INHERTOPT_PARENT;
            SessionType:=  OS2.SSF_TYPE_WINDOWABLEVIO;
            IconFile   :=  NIL;
            PgmHandle  :=  0;
            PgmControl :=  OS2.SSF_CONTROL_MINIMIZE (*+ OS2.SSF_CONTROL_NOAUTOCLOSE*);
            InitXPos   :=  30;
            InitYPos   :=  40;
            InitXSize  :=  200;
            InitYSize  :=  140;
            Reserved   :=  0;
            ObjectBuffer  :=  ADR(FailureObjectName);
            ObjectBuffLen :=  ONLength;
        END (*WITH*);

        rc := OS2.DosStartSession (StartData, idSession, pid);

        (* Starting in background (code 457) is not an error. *)

        IF (rc <> 0) AND (rc <> 457) THEN
            Reply (session, "200 Command failed.");
            RETURN;
        END (*IF*);

        (* Wait for the child session to finish. *)

        rc := OS2.DosGetInfoBlocks (ptib, ppib);
        Request.pid := ppib^.pib_ulpid;
        rc := OS2.DosReadQueue (hq, Request, length, pQData,
                                    0, FALSE, priority, 0);
        IF rc = 0 THEN
            rc := pQData^.procID;
        END (*IF*);
        ArgString := "200 Program terminated with result ";
        pos := LENGTH (ArgString);
        ConvertCard (rc, ArgString, pos);
        ArgString[pos] := Nul;
        (*DEALLOCATE (pQData, length);*)
        rc := OS2.DosCloseQueue (hq);
        Release (ExecLock);

        (* Delete the temporary command file. *)

        FileOps.DeleteFile (cmdfilename);

        Reply (session, ArgString);

    END OldExecProg;
*)

(********************************************************************************)
(*                     HANDLERS FOR THE INDIVIDUAL COMMANDS                     *)
(********************************************************************************)

PROCEDURE ABOR (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* The ABOR command does nothing, in effect, because we don't have          *)
    (* asynchronous processing of control and data streams in this version.     *)

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "226 Command aborted");
    END ABOR;

(********************************************************************************)

PROCEDURE ACCT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "202 Superfluous at this site");
    END ACCT;

(********************************************************************************)

PROCEDURE ALLO (session: Session;  VAR (*IN*) size: ARRAY OF CHAR);

    BEGIN
        IF AllocateFileSize (session^.user, size) THEN
            Reply (session, "200 ALLO OK");
        ELSE
            Reply (session, "550 This is over your file size limit");
        END (*IF*);
    END ALLO;

(********************************************************************************)

PROCEDURE APPE (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR HavePermission, SizeTooBig: BOOLEAN;

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF CanWrite (session^.user, FALSE, HavePermission) THEN
            Reply (session, "150 OK, opening data connection.");
            IF AppendFile (session^.user, SizeTooBig) THEN
                Reply (session, "226 Transfer complete, closing data connection.");
            ELSIF SizeTooBig THEN
                Reply (session, "552 File size limit exceeded.");
            ELSE
                Reply (session, "451 Transfer failed.");
            END (*IF*);
        ELSIF HavePermission THEN
            Reply (session, "452 Not enough free space.");
        ELSE
            Reply (session, "553 Permission denied.");
        END (*IF*);
    END APPE;

(********************************************************************************)

PROCEDURE CDUP (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        IF SetDirectory (session^.user, session^.MSGenabled, "..") THEN
            Reply (session, "250 CWD OK.");
        ELSE
            Reply (session, "550 Cannot change directory.");
        END (*IF*);
    END CDUP;

(********************************************************************************)

PROCEDURE CWD (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        IF SetDirectory (session^.user, session^.MSGenabled, Params) THEN
            Reply (session, "250 CWD OK");
        ELSE
            Reply (session, "550 Cannot change directory.");
        END (*IF*);
    END CWD;

(********************************************************************************)

PROCEDURE DELE (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF CanDelete (session^.user) THEN
            IF DeleteFile (session^.user) THEN
                Reply (session, "250 File deleted.");
            ELSE
                Reply (session, "550 Deletion failed.");
            END (*IF*);
        ELSE
            Reply (session, "550 Permission denied.");
        END (*IF*);
    END DELE;

(********************************************************************************)

PROCEDURE FEAT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* All defined and specified commands and features not defined in [RFC959], *)
    (* or this document, must be included in the FEAT command output in the     *)
    (* form specified in the document that defines the extension.               *)

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)

        Reply (session, "211-Extensions supported:");
        Reply (session, " LANG EN*");
        Reply (session, " MDTM");
        Reply (session, " SITE UTIME");
        Reply (session, " REST STREAM");
        Reply (session, " SIZE");
        Reply (session, " TVFS");
        Reply (session, " TYPE A;I;U;L 8");
        Reply (session, " HOST");
        Reply (session, " UTF8");
        Reply (session, " XCUP");
        Reply (session, " XCWD");
        Reply (session, " XMKD");
        Reply (session, " XPWD");
        Reply (session, " XRMD");
        Reply (session, "211 END");

    END FEAT;

(********************************************************************************)

PROCEDURE HELP (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)

        Reply (session, "214-The following commands are recognized.");
        Reply (session, "   ABOR    ACCT    ALLO    APPE    CDUP    CWD     DELE");
        Reply (session, "   FEAT    HELP    HOST    LANG    LIST    MDTM    MKD");
        Reply (session, "   MODE    NLST    NOOP    OPTS    PASS    PASV    PORT");
        Reply (session, "   PWD     QUIT    REIN    REST    RETR    RMD     RNFR");
        Reply (session, "   RNTO    SITE    SIZE    SMNT    STAT    STOR    STOU");
        Reply (session, "   STRU    SYST    TYPE    USER    XCUP    XCWD    XMKD");
        Reply (session, "   XPWD    XRMD");
        Reply (session, "214 End of list.");

    END HELP;

(********************************************************************************)

PROCEDURE HOST (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        IF SetHost(session^.user, Params) THEN
            Reply (session, "220 Hostname recorded.");
            IF session^.state <> Idle THEN
                RemoveUser (session^.ClientData);
                session^.state := Idle;
            END (*IF*);
            session^.LogIt := TRUE;
        ELSE
            Reply (session, "502 HOST command failed.");
        END (*IF*);
    END HOST;

(********************************************************************************)

PROCEDURE LANG (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        IF ((CAP(Params[0]) = 'E') AND (CAP(Params[0]) = 'N'))
                           OR (Params[0] = Nul) THEN
            Reply (session, "200 Language set to English.");
        ELSE
            Reply (session, "504 Sorry, I don't speak your language.");
        END (*IF*);
    END LANG;

(********************************************************************************)

PROCEDURE LIST (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, TRUE);
        IF ListingIsPossible (session^.user) THEN
            Reply (session, "150 OK, opening data connection.");
            IF SendDirectory (session^.user, FALSE, TRUE) THEN
                CloseDataPort (session^.user);
                Reply (session, "226 Transfer complete.");
            ELSE
                CloseDataPort (session^.user);
                Reply (session, "451 Transfer failed.");
            END (*IF*);
        ELSE
            Reply (session, "550 Directory not available.");
        END (*IF*);
    END LIST;

(********************************************************************************)

PROCEDURE MDTM (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR result: ARRAY [0..13] OF CHAR;

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        GetFileDate (session^.user, result);
        IF result[0] = Nul THEN
            Reply2 (session, "550 Can't find file ", Params);
        ELSE
            Reply2 (session, "213 ", result);
        END (*IF*);
    END MDTM;

(********************************************************************************)

PROCEDURE MKD (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF MakeDirectory (session^.user) THEN
            Reply3 (session, '257 "', Params, '" created');
        ELSE
            Reply (session, "550 Permission denied.");
        END (*IF*);
    END MKD;

(********************************************************************************)

PROCEDURE MODE (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Parameter can be S, B, or C.  I've implemented only S.     *)

    VAR option: CHAR;

    BEGIN
        option := CAP(Params[0]);
        IF (option = 'S') THEN
            SetTransferMode (session^.user, option);
            Reply (session, "200 OK");
        ELSE
            Reply (session, "504 Not supported");
        END (*IF*);
    END MODE;

(********************************************************************************)

PROCEDURE NLST (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, TRUE);
        IF ListingIsPossible (session^.user) THEN
            Reply (session, "150 OK, opening data connection.");
            IF SendDirectory (session^.user, FALSE, FALSE) THEN
                CloseDataPort (session^.user);
                Reply (session, "226 Transfer complete, closing data connection.");
            ELSE
                CloseDataPort (session^.user);
                Reply (session, "451 Transfer failed.");
            END (*IF*);
        ELSE
            Reply (session, "550 File not available.");
        END (*IF*);
    END NLST;

(********************************************************************************)

PROCEDURE NOOP (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "200 NOOP successfully executed!");
    END NOOP;

(********************************************************************************)

PROCEDURE OPTS (session: Session;  VAR (*IN*) command: ARRAY OF CHAR);

    (* OPTS must be implemented, per RFC2389, whenever FEAT is implemented. *)

    (****************************************************************************)

    PROCEDURE RestrictedParameter (S: CharSet): BOOLEAN;

    (* Returns TRUE iff command consists of characters in S. *)

    VAR k: CARDINAL;

    BEGIN
        IF command[0] = Nul THEN
            RETURN FALSE;
        ELSE
            k := 0;
            LOOP
                IF command[k] = Nul THEN
                    RETURN TRUE;
                ELSIF NOT(command[k] IN S) THEN
                    RETURN FALSE;
                END (*IF*);
                INC (k);
            END (*LOOP*);
        END (*IF*);
    END RestrictedParameter;

    (****************************************************************************)

    VAR cmd: CmdType;  result: (good, bad, done, nothandled);

    BEGIN
        cmd := ParseCommand (command);
        CASE cmd OF
           |
               unknown, notloggedin:

                    Reply (session, "501 Unrecognised command.");
                    result := done;
           |
               abor, acct, cdup, feat, help, noop, pasw, pasv, pwd,
                  quit, rein, rest, smnt, stou, syst, xcup, xpwd:
                    IF command[0] = Nul THEN
                        result := good;
                    ELSE
                        Reply (session, "501 This command has no options.");
                        result := done;
                    END (*IF*);
           |
               allo:
                    IF RestrictedParameter(Digits) THEN
                        result := good;
                    ELSE
                        Reply (session, "501 A numeric parameter is required.");
                        result := done;
                    END (*IF*);
           |
               appe, cwd, dele, list, mdtm, mkd, nlst, retr, rmd,
                  rnfr, rnto, size, stat, stor, xcwd, xmkd, xrmd:
                    IF command[0] = Nul THEN
                        Reply (session, "501 A filename is required.");
                        result := done;
                    ELSE
                        result := good;
                    END (*IF*);
           |
               host:
                    IF command[0] = Nul THEN
                        Reply (session, "200 A host name or domain name is optional.");
                        result := done;
                    ELSE
                        result := good;
                    END (*IF*);
           |
               lang:
                    IF (command[0] = Nul)
                      OR ((command[0] = 'E') AND (command[0] = 'N')) THEN
                        result := good;
                    ELSE
                        Reply (session, "501 Only LANG EN is supported.");
                        result := done;
                    END (*IF*);
           |
               mode:
                    IF command[0] = 'S' THEN
                        result := good;
                    ELSE
                        Reply (session, "501 Only mode S is supported.");
                        result := done;
                    END (*IF*);
           |
               opts:
                    Reply (session, "501 Cannot use OPTS with this command.");
                    result := done;
           |
               pass, user:
                    IF command[0] = Nul THEN
                        Reply (session, "501 A character string is required.");
                        result := done;
                    ELSE
                        result := good;
                    END (*IF*);
           |
               port:
                    IF RestrictedParameter(Digits + CharSet{','}) THEN
                        result := good;
                    ELSE
                        Reply (session, "501 A string of numbers is required.");
                        result := done;
                    END (*IF*);
           |
               site:
                    Reply (session, "501 The SITE commands are confidential.");
                    result := done;
           |
               stru:
                    IF (CAP(command[0]) = 'F') OR (CAP(command[0]) = 'R') THEN
                        result := good;
                    ELSE
                        Reply (session, "501 Only F and R are supported.");
                        result := done;
                    END (*IF*);
           |
               type:
                    IF (CAP(command[0]) = 'A') OR (CAP(command[0]) = 'I')
                             OR (CAP(command[0]) = 'U')
                             OR ((CAP(command[0]) = 'L') AND (command[1] = '8')) THEN
                        result := good;
                    ELSE
                        Reply (session, "501 We accept A, I, U, or L8.");
                        result := done;
                    END (*IF*);
           |
        ELSE
            result := bad;
        END (*CASE*);

        IF result = bad THEN
            Reply (session, "501 Option not supported.");
        ELSIF result = nothandled THEN
            Reply (session, "451 Sorry, can't check that one for now.");
        ELSIF result = good THEN
            Reply (session, "200 Command is supported.");
        END (*IF*);

    END OPTS;

(********************************************************************************)

PROCEDURE PASS (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR message: ARRAY [0..255] OF CHAR;

    BEGIN
        IF session^.state = WaitingForPassword THEN
            IF Params[0] = '-' THEN
                session^.MSGenabled := FALSE;
                Strings.Delete (Params, 0, 1);
            END (*IF*);
            IF PasswordOK (session^.user, Params) THEN
                IF session^.MSGenabled THEN
                    SendMessageFile (session^.socket, "Welcome.MSG", "230",
                                     session^.usernumber, session^.user, FALSE);
                    SendDirectoryMessage (session^.user, "230");
                END (*IF*);
                Reply (session, "230 Login OK.");
                session^.state := LoggedIn;
                RecordLoginTime (session^.user);
            ELSE
                INC (session^.LoginAttempts);
                IF session^.LoginAttempts > 3 THEN
                    Reply (session, "530 Too many login attempts.");
                    CheckForHammerer (session^.ClientData);
                    session^.state := MustExit;
                ELSE
                    Sleep (2000);
                    Reply (session, "530 Login failed.");
                END (*IF*);
            END (*IF*);
        ELSE
            Reply (session, "503 Bad sequence of commands.");
        END (*IF*);

        (* Log bad passwords. *)

        IF (session^.state <> LoggedIn) AND session^.userOK THEN
            message := "Bad password '";
            Strings.Append (Params, message);
            Strings.Append ("'", message);
            LogTransaction (session^.logID, message);
        END (*IF*);

    END PASS;

(********************************************************************************)

PROCEDURE PASV (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    VAR myaddr: SockAddr;  PortString: ARRAY [0..23] OF CHAR;

    (****************************************************************************)

    PROCEDURE Decode;

        (* Converts the information in myaddr to a text form in PortString. *)

        TYPE Raw = ARRAY [0..SIZE(InternetSocketAddress)-1] OF CARD8;

        VAR j, place: CARDINAL;  Bytes: Raw;

        (************************************************************************)

        PROCEDURE Convert (value: CARDINAL);

            BEGIN
                IF value > 9 THEN Convert (value DIV 10) END(*IF*);
                PortString[place] := CHR(ORD('0') + value MOD 10);
                INC (place);
            END Convert;

        (************************************************************************)

        BEGIN     (* body of Decode *)
            Bytes := CAST (Raw, myaddr.in_addr);
            place := 0;
            FOR j := 2 TO 5 DO
                Convert (Bytes[j]);
                PortString[place] := ',';  INC(place);
            END (*FOR*);
            Convert (Bytes[0]);
            PortString[place] := ',';  INC(place);
            Convert (Bytes[1]);
            PortString[place] := Nul;
        END Decode;

    (****************************************************************************)

    BEGIN     (* body of PASV *)
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        IF EnterPassiveMode (session^.user, myaddr) THEN
            Decode;
            Reply3 (session, "227 Entering passive mode (", PortString, ")");
        ELSE
            Reply (session, "502 PASV did not succeed");
        END (*IF*);
    END PASV;

(********************************************************************************)

PROCEDURE PORT (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR Arg: RECORD
                 CASE :BOOLEAN OF
                     FALSE: byte: ARRAY [0..5] OF CARD8;
                     |TRUE: addr: CARD32;  port: CARD16;
                 END (*CASE*);
             END (*RECORD*);

    VAR j, pos: CARDINAL;  val: CARD8;  Error: BOOLEAN;

    BEGIN
        pos := 0;  Error := FALSE;
        FOR j := 0 TO 5 DO
            val := 0;
            WHILE Params[pos] IN Digits DO
                val := 10*val + (ORD(Params[pos]) - ORD('0'));  INC(pos);
            END (*WHILE*);
            Arg.byte[j] := val;
            IF (pos > HIGH(Params)) OR (Params[pos] = Nul) THEN
                Error := Error OR (j<>5)
            ELSIF Params[pos] = ',' THEN INC(pos)
            ELSE Error := TRUE;
            END (*IF*);
        END (*FOR*);
        IF Error THEN
            Reply (session, "501 Syntax error.");
        ELSIF UnacceptablePort (session^.user, Arg.addr, Arg.port) THEN
            Reply (session, "504 Unacceptable parameters in PORT command.");
        ELSE
            SetPort (session^.user, Arg.addr, Arg.port);
            Reply (session, "200 Data port set");
        END (*IF*);
    END PORT;

(********************************************************************************)

PROCEDURE PWD (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    VAR DirString, DirStringU: ARRAY [0..511] OF CHAR;

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        CurrentDirectory (session^.user, DirString);
        TranslateToUTF8 (DirString, DirStringU);
        Reply3 (session, '257 "', DirString, '" is current directory.');
    END PWD;

(********************************************************************************)

PROCEDURE QUIT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "221 Goodbye.");
        session^.state := MustExit;
    END QUIT;

(********************************************************************************)

PROCEDURE REIN (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        WITH session^ DO
            IF state <> Idle THEN
                RemoveUser (ClientData);
            END (*IF*);
            EVAL (SetHost (user, ""));
            LogIt := TRUE;
            state := Idle;
        END (*WITH*);
        Reply (session, "220 Ready for new user");
    END REIN;

(********************************************************************************)

PROCEDURE REST (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        IF Params[0] IN Digits THEN
            SetRestartPoint (session^.user, Params);
            Reply (session, "350 Restart marker set.");
        ELSE
            Reply (session, "501 Restart marker must be numeric.");
        END (*IF*);
    END REST;

(********************************************************************************)

PROCEDURE RETR (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR size: CARD64;  pos: CARDINAL;  IsASCII: BOOLEAN;
        name: ARRAY [0..255] OF CHAR;
        reply: ARRAY [0..1023] OF CHAR;

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF FileAvailable (session^.user) THEN

            (* Construct a message showing the transfer type, name, and size. *)

            GetSizeTypeAndNameU (session^.user, size, IsASCII, name);
            Strings.Assign ("150 Opening ", reply);
            IF IsASCII THEN
                Strings.Append ("ASCII", reply);
            ELSE
                Strings.Append ("BINARY", reply);
            END (*IF*);
            Strings.Append (" mode data connection", reply);
            IF name[0] <> Nul THEN
                Strings.Append (" for file ", reply);
                Strings.Append (name, reply);
            END (*IF*);
            IF Compare64(size, Zero64) > 0 THEN
                Strings.Append (" (", reply);
                pos := Strings.Length(reply);
                ConvertCard64 (size, reply, pos);
                IF pos <= 1023 THEN
                    reply[pos] := Nul;
                END (*IF*);
                Strings.Append (" bytes)", reply);
            END (*IF*);
            Strings.Append (".", reply);
            Reply (session, reply);

            (* Send the file. *)

            IF SendFile (session^.user) THEN
                Reply (session, "226 Transfer complete, closing data connection.");
            ELSE
                Reply (session, "426 Transfer failed.");
            END (*IF*);

        ELSE
            Reply (session, "550 File not available.");
        END (*IF*);
    END RETR;

(********************************************************************************)

PROCEDURE RMD (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF CanRemoveDirectory (session^.user) THEN
            IF DeleteDirectory (session^.user) THEN
                Reply (session, "250 Directory deleted.");
            ELSE
                Reply (session, "550 Deletion failed.");
            END (*IF*);
        ELSE
            Reply (session, "550 Permission denied.");
        END (*IF*);
    END RMD;

(********************************************************************************)

PROCEDURE RNFR (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF FileOrDirectoryVisible (session^.user) THEN
            Reply (session, "350 Send new name.");
        ELSE
            Reply (session, "550 File not found.");
        END (*IF*);
    END RNFR;

(********************************************************************************)

PROCEDURE RNTO (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Remark: it is possible for the target name to imply a move from one      *)
    (* directory to another.  We count on procedure RenameTo to check that      *)
    (* the user has the appropriate permissions.                                *)

    VAR NewName: FilenameString;

    BEGIN
        TranslateFromUTF8 (Params, NewName);
        IF RenameTo (session^.user, NewName) THEN
            Reply (session, "250 Rename succeeded.");
        ELSE
            Reply (session, "553 Permission denied.");
        END (*IF*);
    END RNTO;

(********************************************************************************)

PROCEDURE SITE (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Only supported commands are PERM and the SITE MNGR group. *)

    TYPE KeywordNumber = [0..8];
         KeywordArray = ARRAY KeywordNumber OF FourChar;

    CONST KeywordList = KeywordArray {'    ', 'EXEC', 'EXIT', 'GXIT', 'KILL',
                                      'LIST', 'MNGR', 'PERM', 'UTIM'};

    (****************************************************************************)

    PROCEDURE StripLeadingSpaces;

        (* Removes the leading spaces from Params. *)

        VAR m: CARDINAL;

        BEGIN
            m := 0;
            WHILE (m < HIGH(Params)) AND (Params[m] = " ") DO INC(m) END (*WHILE*);
            IF m > 0 THEN
                Strings.Delete (Params, 0, m);
            END (*IF*);
        END StripLeadingSpaces;

    (****************************************************************************)

    PROCEDURE Compare4 (n: KeywordNumber): INTEGER;

        (* Compares the first four characters of Params with KeywordList[n].   *)
        (* Returns >0 if Params[0..3] > KeywordList[n], and so on.             *)

        VAR ch1, ch2: CHAR;  k: [0..3];

        BEGIN
            k := 0;
            LOOP
                ch1 := Params[k];  ch2 := KeywordList[n][k];
                IF ch1 > ch2 THEN RETURN +1
                ELSIF ch1 < ch2 THEN RETURN -1
                ELSIF k = 3 THEN RETURN 0
                END (*IF*);
                INC (k);
            END (*LOOP*);
        END Compare4;

    (****************************************************************************)

    PROCEDURE FindKeyWord(): CARDINAL;

        VAR first, middle, last: CARDINAL;  test: INTEGER;

        BEGIN
            (* Go through the keyword list to find a match with the command.  *)
            (* In this version I'm using a binary search.                     *)

            first := 0;  last := MAX(KeywordNumber);
            LOOP
                middle := (first + last) DIV 2;
                test := Compare4 (middle);
                IF test > 0 THEN
                    first := middle + 1;
                ELSIF (test < 0) AND (middle > 0) THEN
                    last := middle - 1;
                ELSE
                    RETURN middle;
                END (*IF*);
                IF first > last THEN RETURN 0 END (*IF*);
            END (*LOOP*);

        END FindKeyWord;

    (****************************************************************************)

    PROCEDURE ParseUtimeParams (VAR (*OUT*) filename: ARRAY OF CHAR;
                                VAR (*OUT*) datetime: ARRAY OF CARDINAL): BOOLEAN;

        (* Extracts the UTIME parameters from Params, returns TRUE iff the  *)
        (* parameters are syntactically correct.                            *)

        VAR k, pos: CARDINAL;
            formatOK: BOOLEAN;

        (************************************************************************)

        PROCEDURE RemoveTrailingSpaces;

            BEGIN
                WHILE (k > 0) AND (Params[k-1] = ' ') DO
                    DEC (k);
                END (*WHILE*);
                Params[k] := Nul;
            END RemoveTrailingSpaces;

        (************************************************************************)

        PROCEDURE ConvertDateTime (N: CARDINAL;  UTC: BOOLEAN): BOOLEAN;

            (* Converts 14-digit string starting at Buffer[pos], stores date in *)
            (* datetime[N] and time in datetime[N+1], returns TRUE if found.    *)

            VAR success: BOOLEAN;

            BEGIN
                success := StringToPackedDateTime (Params, pos, UTC,
                                                   datetime[N], datetime[N+1]);
                RETURN success;
            END ConvertDateTime;

        (************************************************************************)

        VAR UTC, found: BOOLEAN;

        BEGIN
            UTC := FALSE;
            formatOK := FALSE;
            k := LENGTH(Params);
            RemoveTrailingSpaces;
            IF k > 0 THEN

                (* Optional (?) UTC flag. *)

                Strings.FindPrev (' ', Params, k-1, found, pos);
                IF found AND (k-pos = 4) AND (CAP(Params[pos+1]) = 'U')
                                         AND (CAP(Params[pos+2]) = 'T')
                                         AND (CAP(Params[pos+3]) = 'C') THEN
                    UTC := TRUE;
                    k := pos;
                    RemoveTrailingSpaces;
                    found := k > 0;
                    IF found THEN
                        Strings.FindPrev (' ', Params, k-1, found, pos);
                    END (*IF*);
                END (*IF*);

                (* Date/time created. *)

                IF found AND (k-pos = 15) THEN
                    INC (pos);
                    formatOK := ConvertDateTime (4, UTC);
                    IF formatOK THEN
                        datetime[0] := datetime[4];
                        datetime[2] := datetime[4];
                        datetime[1] := datetime[5];
                        datetime[3] := datetime[5];
                        k := pos - 1;
                        RemoveTrailingSpaces;
                        found := k > 0;
                        IF found THEN
                            Strings.FindPrev (' ', Params, k-1, found, pos);
                            INC (pos);
                        END (*IF*);

                        (* Date/time accessed. *)

                        IF found AND (k-pos = 14) AND ConvertDateTime(2, UTC) THEN
                            k := pos;
                            RemoveTrailingSpaces;
                            found := k > 0;
                            IF found THEN
                                Strings.FindPrev (' ', Params, k-1, found, pos);
                                INC (pos);
                            END (*IF*);

                            (* Date/time modified. *)

                            IF found AND (k-pos = 14) AND ConvertDateTime(0, UTC) THEN
                                k := pos;
                                RemoveTrailingSpaces;

                            END (*IF*);

                        END (*IF*);

                    END (*IF*);

                END (*IF*);

            END (*IF*);

            formatOK := formatOK AND (k > 0);
            IF formatOK THEN
                Strings.Assign (Params, filename);
            END (*IF*);

            RETURN formatOK;

        END ParseUtimeParams;

    (****************************************************************************)

    VAR permstring: ARRAY [0..3] OF CHAR;
        fname: ARRAY [0..511] OF CHAR;
        pos: CARDINAL;  found: BOOLEAN;
        datetime: ARRAY [0..5] OF CARDINAL;
        pPib: OS2.PPIB;  pTib: OS2.PTIB;

    BEGIN
        Strings.Capitalize (Params);
        CASE FindKeyWord() OF

          |  6:  (* SITE MNGR *)
                 IF NOT(LogSITEMNGR) THEN
                     SuppressLogging (session);
                 END (*IF*);
                 IF session^.IsManager THEN
                     Strings.Delete (Params, 0, 4);
                     StripLeadingSpaces;
                     CASE FindKeyWord() OF

                       | 1:  (* SITE MNGR EXEC *)

                             Strings.Delete (Params, 0, 4);
                             StripLeadingSpaces;
                             Strings.Assign (Params, fname);
                             Strings.FindNext (' ', Params, 0, found, pos);
                             IF found THEN
                                 Params[pos] := Nul;
                                 Strings.Delete (fname, 0, pos+1);
                             ELSE
                                 fname := "";
                             END (*IF*);
                             ExecProg (session, Params, fname);

                       | 2:  (* SITE MNGR EXIT *)

                             Reply (session, "200 Closing FtpServer.");
                             OS2.DosGetInfoBlocks (pTib, pPib);

                             (* Request shutdown twice for rapid shutdown. *)

                             RequestShutdown;
                             RequestShutdown;

                       | 3:  (* SITE MNGR GXIT *)

                             Reply (session, "200 FtpServer gradual shutdown.");
                             OS2.DosGetInfoBlocks (pTib, pPib);
                             RequestShutdown;

                       | 4:  (* SITE MNGR KILL *)

                             Strings.Delete (Params, 0, 4);
                             StripLeadingSpaces;
                             KillUser (Params);
                             Reply (session, "200 Kill in progress.");

                       | 5:  (* SITE MNGR LIST *)

                             Reply (session, "200-List of current users");
                             ListAllUsers (session^.socket);
                             Reply (session, "200 End of list.");
                     ELSE
                         NotImplemented (session, Params);
                     END (*CASE*);
                 ELSE
                     Reply (session, "550 Permission denied.");
                 END (*IF*);
                 IF NOT(LogSITEMNGR) THEN
                     RestoreLogging (session);
                 END (*IF*);

          |  7:  (* SITE PERM *)

                 GetCurrentPermissions (session^.user, permstring);
                 Reply2 (session, "200 Current permissions: ", permstring);

          |  8:  (* SITE UTIME *)

                 (* SITE UTIME filename atime mtime ctime UTC"  *)
                 (* Timestamps are modified/accessed/created,   *)
                 (* formatted as YYYYMMDDHHMMSS                 *)

                 IF CAP(Params[4]) = 'E' THEN
                     Strings.Delete (Params, 0, 5);
                     StripLeadingSpaces;
                     IF ParseUtimeParams (fname, datetime) THEN
                         SetFileName (session^.user, fname, FALSE);
                         IF CanWrite (session^.user, FALSE, found) THEN
                             SetFileDateTime (session^.user, datetime);
                             Reply (session, "200 Date and time updated.");
                         ELSE
                             Reply (session, "553 You need write privilege.");
                         END (*IF*);
                     ELSE
                         Reply (session, "501 Invalid parameters for SITE UTIME");
                     END (*IF*);
                 ELSE
                     NotImplemented (session, Params);
                 END (*IF*);

        ELSE
                 NotImplemented (session, Params);
        END (*CASE*);

    END SITE;

(********************************************************************************)

PROCEDURE Size (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Note: non-standard command. *)

    VAR size: CARD64;  pos: CARDINAL;  buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        size := GetSize (session^.user);
        IF Compare64(size, Max64) = 0 THEN
            Reply (session, "550 Can't find file");
        ELSE
            buffer := "213 ";  pos := 4;
            ConvertCard64 (size, buffer, pos);
            IF pos < SIZE(buffer) THEN
                buffer[pos] := Nul;
            END (*IF*);
            Reply (session, buffer);
        END (*IF*);
    END Size;

(********************************************************************************)

PROCEDURE SMNT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "202 Superfluous at this site");
    END SMNT;

(********************************************************************************)

PROCEDURE STAT (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR IsDir: BOOLEAN;

    BEGIN
        IF Params[0] <> Nul THEN

            (* Response should be the same as for the LIST command, except that *)
            (* we use the control connection rather than the data connection.   *)

            SetFileName (session^.user, Params, TRUE);
            IF ListingIsPossible (session^.user) THEN
                IsDir := IsDirectory (session^.user);
                IF IsDir THEN
                    Reply (session, "212-Directory status.");
                ELSE
                    Reply (session, "213-File status.");
                END (*IF*);
                EVAL (SendDirectory (session^.user, TRUE, TRUE));
                IF IsDir THEN
                    Reply (session, "212 End of status.");
                ELSE
                    Reply (session, "213 End of status.");
                END (*IF*);
            ELSE
                Reply (session, "450 That file or directory is not available to you.");
            END (*IF*);

        ELSE

            Strings.Assign ('STAT', Params);
            Reply (session, "211-FTP server status.");
            SendUserStatus (session^.user);
            Reply (session, "211 End of status.");

        END (*IF*);

    END STAT;

(********************************************************************************)

PROCEDURE STOR (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR HavePermission, SizeTooBig: BOOLEAN;

    BEGIN
        SetFileName (session^.user, Params, FALSE);
        IF CanWrite (session^.user, TRUE, HavePermission) THEN
            Reply (session, "150 OK, opening data connection.");
            IF AcceptFile (session^.user, SizeTooBig) THEN
                Reply (session, "226 Transfer complete, closing data connection.");
            ELSIF SizeTooBig THEN
                Reply (session, "552 File size limit exceeded.");
            ELSE
                Reply (session, "451 Transfer failed.");
            END (*IF*);
        ELSIF HavePermission THEN
            Reply (session, "452 Not enough free space.");
        ELSE
            Reply (session, "553 Permission denied.");
        END (*IF*);
    END STOR;

(********************************************************************************)

PROCEDURE STOU (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    VAR Name: ARRAY [0..255] OF CHAR;
        HavePermission, SizeTooBig: BOOLEAN;

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        HavePermission := FALSE;
        IF CreateUniqueFileName (session^.user, Name)
                       AND CanWrite (session^.user, TRUE, HavePermission) THEN
            Reply2 (session, "150 FILE: ", Name);
            IF AcceptFile (session^.user, SizeTooBig) THEN
                Reply (session, "226 Transfer complete, closing data connection.");
            ELSIF SizeTooBig THEN
                Reply (session, "552 File size limit exceeded.");
            ELSE
                Reply (session, "451 Transfer failed.");
            END (*IF*);
        ELSIF HavePermission THEN
            Reply (session, "452 Not enough free space.");
        ELSE
            Reply (session, "553 Permission denied.");
        END (*IF*);
    END STOU;

(********************************************************************************)

PROCEDURE STRU (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Parameter can be F, R, or P.  I've implemented only F and R.     *)

    VAR option: CHAR;

    BEGIN
        option := CAP(Params[0]);
        IF (option = 'F') OR (option = 'R') THEN
            SetFileStructure (session^.user, option);
            Reply (session, "200 OK");
        ELSE
            Reply (session, "504 Not supported");
        END (*IF*);
    END STRU;

(********************************************************************************)

PROCEDURE SYST (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* The reply here should really be "215 OS/2", but that seems to make       *)
    (* WebExplorer confused, so we have to fake being a Unix system.            *)

    BEGIN
        dummy[0] := dummy[0];    (* To avoid a compiler warning. *)
        Reply (session, "215 UNIX type:OS/2");
    END SYST;

(********************************************************************************)

PROCEDURE Type (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    (* Options supported are 'A', 'I', 'U', and 'L 8'.  *)

    VAR option: CHAR;

    BEGIN
        option := CAP(Params[0]);
        IF (option = 'L') AND (Params[2] = '8') THEN
            option := 'I';
        END (*IF*);
        IF (option = 'A') OR (option = 'I') OR (option = 'U') THEN
            SetTransferType (session^.user, option);
            Reply (session, "200 OK");
        ELSE
            Reply (session, "504 Not supported");
        END (*IF*);
    END Type;

(********************************************************************************)

PROCEDURE USER (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR message: ARRAY [0..255] OF CHAR;

    BEGIN
        WITH session^ DO
            IF state <> Idle THEN
                RemoveUser (ClientData);
                state := Idle;
            END (*IF*);
            IsManager := FALSE;
            ToLower (Params);
            IF AddressForbidden (ClientData, Params) THEN
                Reply (session, "530 Cannot log in from that address.");
                session^.state := MustExit;
            ELSE
                userOK := TRUE;
                CASE NewUser(ClientData, Params, LogIt) OF
                  | NoPasswordNeeded:
                              SendMessageFile (session^.socket, "Welcome.MSG", "230",
                                               session^.usernumber, session^.user, FALSE);
                              SendDirectoryMessage (session^.user, "230");
                              Reply (session, "230 User logged in, proceed.");
                              session^.state := LoggedIn;
                              RecordLoginTime (session^.user);
                  | GuestUser:
                              Reply (session, "331 Send e-mail address as password.");
                              session^.state := WaitingForPassword;
                  | NormalUser:
                              Reply (session, "331 Password required.");
                              session^.state := WaitingForPassword;
                  | NoSuchUser:
                              message := "Bad username '";
                              Strings.Append (Params, message);
                              Strings.Append ("'", message);
                              LogTransaction (session^.logID, message);

                              (* Special case: if the username is faulty, we let the    *)
                              (* client send a password anyway.  That makes it harder   *)
                              (* for invaders to know whether they guessed the wrong    *)
                              (* username or the wrong password.                        *)

                              userOK := FALSE;
                              Reply (session, "331 Password required.");
                              session^.state := WaitingForPassword;
                  | Manager:
                              Reply (session, "331 Password required.");
                              session^.state := WaitingForPassword;
                              session^.IsManager := TRUE;
                  | OverflowUser:
                              Reply (session, "421 Too many users, try again later.");
                              session^.state := MustExit;
                END (*CASE*);
            END (*IF*);
        END (*WITH*);
    END USER;

(********************************************************************************)
(*                      THE MAIN COMMAND DISPATCHER                             *)
(********************************************************************************)

TYPE
    HandlerProc = PROCEDURE (Session, VAR (*IN*) ARRAY OF CHAR);
    HandlerArray = ARRAY CmdType OF HandlerProc;

CONST
    HandlerList = HandlerArray {NoSuchCommand, NotLoggedIn,
                               ABOR, ACCT, ALLO, APPE, CDUP, CWD,
                               DELE, FEAT, HELP, HOST, LANG, LIST, MDTM, MKD, MODE,
                               NLST, NOOP, OPTS, PASV, PASS, PASV, PORT,
                               PWD, QUIT, REIN, REST, RETR, RMD,
                               RNFR, RNTO, SITE, Size, SMNT, STAT,
                               STOR, STOU, STRU, SYST, Type, USER,
                               CDUP, CWD, MKD, PWD, RMD};

(********************************************************************************)

PROCEDURE HandleCommand (S: Session;  Command: ARRAY OF CHAR;
                                           VAR (*OUT*) Quit, EndSession: BOOLEAN);

    (* Executes one user command.  Returns with Quit = TRUE if this was a QUIT  *)
    (* command.  Returns with EndSession = TRUE if the command is one that      *)
    (* closes the session (this includes QUIT), or if the connection is lost.   *)

    VAR cmd: CmdType;  LogCmd: BOOLEAN;
        temp: ARRAY [0..3] OF CHAR;
        CommandCopy: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (Command, CommandCopy);
        cmd := ParseCommand (Command);

        (* If the user is not yet logged in, only HELP, HOST, USER, PASS, and QUIT are legal. *)

        IF (S^.state <> LoggedIn) AND (cmd <> help) AND (cmd <> host) AND (cmd <> quit)
                            AND (cmd <> user) AND (cmd <> pass) THEN
            cmd := notloggedin;
        END (*IF*);

        (* Echo command to transaction log. *)

        IF (cmd = pass) THEN
            Strings.Assign ("PASS ******", CommandCopy);
        END (*IF*);
        IF S^.LogIt THEN
            LogCmd := TRUE;
            IF (cmd = site) AND NOT LogSITEMNGR THEN
                Strings.Extract (Command, 0, 4, temp);
                Strings.Capitalize (temp);
                LogCmd := NOT Strings.Equal (temp, "MNGR");
            END (*IF*);
            IF LogCmd THEN
                LogTransaction (S^.logID, CommandCopy);
            END (*IF*);
        END (*IF*);
        RememberCommand (S^.ClientData, CommandCopy);

        (* Call the handler. *);

        ResetCount (S^.user);
        HandlerList[cmd] (S, Command);
        Quit := cmd = quit;
        EndSession := CommandDone(S^.ClientData)
                        OR (S^.state = CommFailure) OR (S^.state = MustExit);
        IF S^.state = CommFailure THEN
            LogTransactionL (S^.logID, "Connection lost");
        END (*IF*);

    END HandleCommand;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

PROCEDURE EnableLogSiteMngr (enable: BOOLEAN);

    (* Enables or disables the logging of SITE MNGR commands. *)

    BEGIN
        LogSITEMNGR := enable;
    END EnableLogSiteMngr;

(********************************************************************************)

BEGIN
    ExtraLogging := 0;
    ExternalShutdownRequest := 0;
    LogSITEMNGR := FALSE;
    HidePasswords := FALSE;
    CreateLock (ExecLock);
    CreateLock (NextNameLock);
    NextName := "00000000";
    SetProcname ("FtpServer", 11);
    FTPDctx := OpenLogContext();
END FtpCommands.

