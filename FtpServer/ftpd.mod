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

MODULE Ftpd;

        (********************************************************)
        (*                                                      *)
        (*                    FTP server                        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 August 1997                  *)
        (*  Last edited:        27 March 2014                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC, CARD8, CARD16, CAST;

IMPORT FV, TextIO, Strings, OS2;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM FtpdSession IMPORT
    (* proc *)  SetVersion, SetTransactionLogging, SetMaxUsers, SetTimeout,
                DisableTelnetCompatibility, NewSession, NumberOfUsers;

FROM LoggedOnUsers IMPORT
    (* proc *)  SetGuestLimit, LoadSecurityParameters, KillAllUsers;

FROM FtpCommands IMPORT
    (* var  *)  FTPDctx,
    (* proc *)  SetExtraLogging, HidePasswordsInLog, EnableLogSiteMngr;

FROM FtpTransfers IMPORT
    (* proc *)  SetLogLevel, SetFreeSpaceThreshold, SetBehindFirewall,
                SetPassivePortRange, EnableTaggedCheck;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY,
    (* proc *)  inet_addr;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, AddressFamily, SocketType, SockAddr,
    (* proc *)  socket, bind, sock_errno, psock_errno, soclose,
                accept, connect, listen, getpeername,
                sock_init, so_cancel, setsockopt;

FROM TransLog IMPORT
    (* proc *)  TransactionLogID,
    (* type *)  CreateLogID, LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM FtpdINI IMPORT
    (* proc *)  SetINIFileName, SetHashMax, OpenINIFile, CloseINIFile,
                GetINIFileName, INIFileExists;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIGetString;

FROM InetUtilities IMPORT
    (* proc *)  Swap2, IPToString, ConvertDecimal, AppendCard,
                GetLastError, WaitForSocket;

FROM Names IMPORT
    (* type *)  HostName, FilenameIndex, FilenameString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, SetBoundary, ClearScreen, WriteStringAt,
                WriteString, WriteLn, WriteChar, WriteInt, WriteCard;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IOR, IAND;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM CtrlC IMPORT
    (* type *)  BreakHandler,
    (* proc *)  SetBreakHandler;

(********************************************************************************)

CONST
    DefaultPort = 21;
    DefaultMaxUsers = 10;
    DefaultTimeout = 900;               (* seconds   *)
    DefaultLogLevel = 32;
    DefaultFreeSpaceThreshold = 10;     (* megabytes *)
    DefaultGuestLimit = DefaultMaxUsers-1;

TYPE
    ParameterSet = SET OF ['A'..'Z'];

VAR
    AlreadySet: ParameterSet;
    MainSocket: Socket;
    CalledFromInetd: BOOLEAN;
    RapidShutdown: BOOLEAN;
    ScreenEnabled: BOOLEAN;
    BindAddr, ServerPort: CARDINAL;

    (* Transaction log level as set by the 'D' and/or 'L' options.  This  *)
    (* value is meaningless if 'D' and/or 'L' are not in AlreadySet.      *)

    CmdLevel: CARDINAL;

    (* Semaphore that is signalled by the Ctrl/C handler or the     *)
    (* ExternalShutdownRequest handler.                             *)

    ShutdownRequest: Semaphore;

    (* Flag to say that a shutdown is in progress. *)

    ShutdownInProgress: BOOLEAN;

    (* Event semaphore to trigger updater task. *)

    UpdaterFlag: OS2.HEV;

    (* Event semaphore by which an external program requests a shutdown. *)

    ExternalShutdownRequest: OS2.HEV;

    (* Termination semaphore used to ensure that all tasks terminate    *)
    (* in an orderly manner.                                            *)

    TaskDone: Semaphore;

(********************************************************************************)

PROCEDURE ShutdownChecker;

    (* A separate task that waits for a shutdown request.  The shutdown can be  *)
    (* triggered by a global event semaphore or by a keyboard Ctrl/C.           *)

    VAR StillRunning: BOOLEAN;

    BEGIN
        StillRunning := TRUE;
        LOOP
            Wait (ShutdownRequest);
            RapidShutdown := ShutdownInProgress;
            ShutdownInProgress := TRUE;
            Sleep (500);
            IF StillRunning THEN
                so_cancel (MainSocket);
                MainSocket := NotASocket;
                StillRunning := FALSE;
            END (*IF*);
            IF RapidShutdown THEN
                EXIT (*LOOP*);
            END(*IF*);
        END (*LOOP*);
        Signal (TaskDone);
    END ShutdownChecker;

(********************************************************************************)

PROCEDURE ["C"] ControlCHandler(): BOOLEAN;

    (* Intercepts a Ctrl/C from the keyboard. *)

    BEGIN
        Signal (ShutdownRequest);
        RETURN TRUE;
    END ControlCHandler;

(********************************************************************************)

PROCEDURE GetParameters;

    (* Picks up program arguments from the command line.  *)

    VAR j: CARDINAL;
        args: ChanId;
        Options, INIFileName: FilenameString;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            LOOP
                IF Options[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = MAX(FilenameIndex) THEN
                    Options[j] := CHR(0);  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END SkipBlanks;

    (****************************************************************************)

    PROCEDURE GetNumber(): CARDINAL;

        BEGIN
            SkipBlanks;
            RETURN ConvertDecimal (Options, j);
        END GetNumber;

    (****************************************************************************)

    PROCEDURE GetString (VAR (*OUT*) result: ARRAY OF CHAR);

        TYPE CharSet = SET OF CHAR;
        CONST
            Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
            Stoppers = CharSet {Nul, ' ', CR, LF};

        VAR k: CARDINAL;

        BEGIN
            SkipBlanks;  k := 0;
            WHILE (j <= MAX(FilenameIndex)) AND NOT (Options[j] IN Stoppers) DO
                result[k] := Options[j];
                INC(j);  INC(k);
            END (*WHILE*);
        END GetString;

    (****************************************************************************)

    VAR level, pos: CARDINAL;  TNImode: BOOLEAN;
        tail: ARRAY [0..3] OF CHAR;

    BEGIN
        AlreadySet := ParameterSet{};
        CmdLevel := 0;
        TNImode := FALSE;
        INIFileName := "FTPD.INI";
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, Options);
            j := 0;  SkipBlanks;
            LOOP
                CASE CAP(Options[j]) OF
                    CHR(0):   EXIT (*LOOP*);
                  | '-':      INC (j);
                  | 'D':      INC (j);  level := GetNumber() MOD 4;
                                        CmdLevel := CmdLevel MOD 16 + 16*level;
                                        INCL (AlreadySet, 'D');
                  | 'F':      INC (j);  SetFreeSpaceThreshold (1024*GetNumber());
                                        INCL (AlreadySet, 'F');
                  | 'G':      INC (j);  SetGuestLimit (GetNumber());
                                        INCL (AlreadySet, 'G');
                  | 'I':      INC (j);  GetString (INIFileName);
                                        pos := LENGTH(INIFileName)-4;
                                        Strings.Extract (INIFileName,
                                                         pos, 4, tail);
                                        Strings.Capitalize (tail);
                                        IF Strings.Equal (tail, ".TNI") THEN
                                            TNImode := TRUE;
                                        END (*IF*);
                  | 'L':      INC (j);  level := GetNumber() MOD 4;
                                        CmdLevel := 16*(CmdLevel DIV 16)
                                                            + 4*level;
                                        INCL (AlreadySet, 'L');
                  | 'M':      INC (j);  SetMaxUsers (GetNumber());
                                        INCL (AlreadySet, 'M');
                  | 'P':      INC (j);  ServerPort := GetNumber();
                                        INCL (AlreadySet, 'P');
                  | 'S':      INC (j);  TNImode := TRUE;
                                        pos := LENGTH(INIFileName)-4;
                                        Strings.Extract (INIFileName,
                                                         pos, 4, tail);
                                        Strings.Capitalize (tail);
                                        IF Strings.Equal (tail, ".INI") THEN
                                            INIFileName[pos] := CHR(0);
                                            Strings.Append (".TNI", INIFileName);
                                        END (*IF*);
                  | 'T':      INC (j);  pos := GetNumber();
                                        IF pos = 0 THEN
                                            DEC(j);  Options[j] := 'S';
                                        ELSE
                                            SetTimeout (pos);
                                            INCL (AlreadySet, 'T');
                                        END (*IF*);
                  | 'X':      INC (j);  SetExtraLogging (1);
                  | '0'..'9': MainSocket := GetNumber();
                              CalledFromInetd := TRUE;
                ELSE
                    IF ScreenEnabled THEN
                        WriteString ("Unknown option ");
                        WriteChar (Options[j]);  WriteLn;
                    END (*IF*);
                    INC(j);
                END (*CASE*);
                SkipBlanks;
            END (*LOOP*);
        END (*IF*);

        (*TNImode := TRUE;*)   (* while debugging *)
        IF TNImode THEN
            INIFileName := "FTPD.TNI";
        END (*IF*);
        SetINIFileName (INIFileName, TNImode);

    END GetParameters;

(************************************************************************)

PROCEDURE LoadUpdateableINIData;

    (* Loads configuration parameters from the INI file.  These are not *)
    (* all of the parameters; only those that may be modified while the *)
    (* server is running.  The parameters that can be set only at       *)
    (* program startup are set by LoadINIData (see below).  The present *)
    (* procedure is called by LoadINIData, but it may also be called    *)
    (* at other times.                                                  *)

    VAR hini: HINI;
        CheckTaggers, BehindFirewall, LimitPASVPorts, TelnetDisable,
                                      LogSITEMNGR, HidePasswords: BOOLEAN;
        MaxUsers, GuestLimit, TimeoutLimit,
           MinLocalAddr, MaxLocalAddr, FirewallIPAddr, TransLevel: CARDINAL;
        MinPort, MaxPort: CARD16;
        TransLogName: FilenameString;
        SyslogHost: HostName;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    BEGIN
        SYSapp := "$SYS";
        CheckTaggers := FALSE;
        BehindFirewall := FALSE;
        LimitPASVPorts := FALSE;
        TelnetDisable := FALSE;
        LogSITEMNGR := TRUE;
        HidePasswords := FALSE;
        MaxUsers := DefaultMaxUsers;
        TimeoutLimit := DefaultTimeout;
        MinLocalAddr := 0;
        MaxLocalAddr := MAX(CARDINAL);
        FirewallIPAddr := 0;
        MinPort := Swap2(49152);  MaxPort := Swap2(65535);
        TransLevel := CmdLevel;
        TransLogName := "FTPTRANS.LOG";
        SyslogHost := "";

        hini := OpenINIFile();

        IF NOT ('M' IN AlreadySet) THEN
            EVAL(GetItem ("MaxUsers", MaxUsers));
            SetMaxUsers (MaxUsers);
        END (*IF*);

        IF NOT ('G' IN AlreadySet) THEN
            GuestLimit := MaxUsers - 1;
            EVAL(GetItem ("GuestLimit", GuestLimit));
            SetGuestLimit (GuestLimit);
        END (*IF*);

        IF NOT ('T' IN AlreadySet) THEN
            EVAL(GetItem ("TimeOut", TimeoutLimit));
            SetTimeout (TimeoutLimit);
        END (*IF*);

        EVAL(GetItem ("TelnetDisable", TelnetDisable));
        EVAL(GetItem ("CheckTaggers", CheckTaggers));
        EVAL(GetItem ("BehindFirewall", BehindFirewall));
        EVAL(GetItem ("MinLocalAddr", MinLocalAddr));
        EVAL(GetItem ("MaxLocalAddr", MaxLocalAddr));
        EVAL(GetItem ("FirewallIPAddr", FirewallIPAddr));
        EVAL(GetItem ("MinPort", MinPort));
        EVAL(GetItem ("MaxPort", MaxPort));
        EVAL(GetItem ("TransLevel", TransLevel));
        EVAL(GetItem ("LogSITEMNGR", LogSITEMNGR));
        EVAL(GetItem ("HidePasswords", HidePasswords));
        IF NOT INIGetString (hini, SYSapp, "TransLogName", TransLogName)
                   OR (TransLogName[0] = CHR(0)) THEN
            TransLogName := "FTPTRANS.LOG";
        END (*IF*);
        EVAL (INIGetString (hini, SYSapp, "SyslogHost", SyslogHost));

        IF NOT GetItem ("LimitPASVPorts", LimitPASVPorts) THEN
            LimitPASVPorts := BehindFirewall;
        END (*IF*);

        CloseINIFile (hini);

        (* TransLevel is actually an array of eight bits:               *)
        (*    bit 0          transaction log to disk   (obsolescent)    *)
        (*    bit 1          transaction log to screen (obsolescent)    *)
        (*    bit 2          enable user transfer logging               *)
        (*    bit 3          enable common.log transfer logging         *)
        (*    bit 4          transaction log to disk                    *)
        (*    bit 5          transaction log to screen                  *)
        (*    bit 6          transaction log to pipe                    *)
        (*    bit 7          transaction log to syslog                  *)
        (* That is, TransLevel DIV 16 controls the transaction logging, *)
        (* while TransLevel MOD 16 DIV 4 controls the transfer logging. *)
        (* Note that these bits can be overridden by CmdLevel.          *)
        (* The redundant bits 0 and 1 are a transition arrangement;     *)
        (* originally bits 0 and 1 controlled transaction logging, so   *)
        (* we have to take the OR of two ways to specify the disk and   *)
        (* screen options.                                              *)

        IF 'D' IN AlreadySet THEN
            TransLevel := CmdLevel;
        END (*IF*);
        TransLevel := IOR (TransLevel DIV 16, TransLevel MOD 4);
        IF NOT ScreenEnabled THEN
            TransLevel := IAND (TransLevel, 13);
        END (*IF*);
        SetTransactionLogging (TransLevel, TransLogName, SyslogHost);
        EnableLogSiteMngr (LogSITEMNGR);
        HidePasswordsInLog (HidePasswords);

        LoadSecurityParameters;
        DisableTelnetCompatibility (TelnetDisable);
        EnableTaggedCheck (CheckTaggers);
        SetPassivePortRange (LimitPASVPorts, MinPort, MaxPort);
        SetBehindFirewall (BehindFirewall, MinLocalAddr, MaxLocalAddr,
                                                         FirewallIPAddr);
    END LoadUpdateableINIData;

(************************************************************************)

PROCEDURE LoadINIData(): BOOLEAN;

    (* Loads setup parameters from "ftpd.ini".  *)

    VAR hini: HINI;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    PROCEDURE GetString (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, SYSapp, name, variable);
        END GetString;

    (********************************************************************)

    VAR FreeSpaceThreshold, UserLogging, level, TransLevel, HashMax: CARDINAL;
        UseTNI: BOOLEAN;
        CommonLogName, UserLogName, INIFileName: FilenameString;

    BEGIN
        SYSapp := "$SYS";
        CommonLogName := "COMMON.LOG";
        UserLogName := "FTPUSERS.LOG";
        IF NOT ('P' IN AlreadySet) THEN
            ServerPort := DefaultPort;
        END (*IF*);
        UserLogging := DefaultLogLevel;
        BindAddr := 0;
        TransLevel := 0;
        HashMax := 0;
        FreeSpaceThreshold := DefaultFreeSpaceThreshold;

        GetINIFileName (INIFileName, UseTNI);
        IF NOT INIFileExists() THEN
            WriteString ("Missing file ");
            WriteString (INIFileName);
            WriteString (", please run Setup first.");
            WriteLn;
            RETURN FALSE;
        END (*IF*);

        hini := OpenINIFile();
        IF INIValid(hini) THEN
            IF NOT ('P' IN AlreadySet) THEN
                EVAL(GetItem ("ServerPort", ServerPort));
            END (*IF*);
            EVAL(GetItem ("TransLevel", TransLevel));
            EVAL(GetItem ("LogLevel", UserLogging));
            EVAL(GetItem ("BindAddr", BindAddr));
            EVAL(GetItem ("SpaceThreshold", FreeSpaceThreshold));
            EVAL(GetItem ("HashMax", HashMax));
            IF NOT GetString ("CommonLogName", CommonLogName) THEN
                CommonLogName := "COMMON.LOG";
            END (*IF*);
            IF NOT GetString ("UserLogName", UserLogName) THEN
                UserLogName := "FTPUSERS.LOG";
            END (*IF*);
            CloseINIFile (hini);
        ELSE
            WriteString ("Can't open ");
            IF UseTNI THEN
                WriteChar ('T');
            ELSE
                WriteChar ('I');
            END (*IF*);
            WriteString ("NI file, error code ");
            WriteCard (GetLastError());
            WriteLn;
            RETURN FALSE;
        END (*IF*);

        IF 'L' IN AlreadySet THEN
            level := CmdLevel;
        ELSE
            level := TransLevel;
        END (*IF*);
        SetLogLevel (level MOD 16 DIV 4, UserLogging,
                            CommonLogName, UserLogName);
        IF NOT ('F' IN AlreadySet) THEN
            SetFreeSpaceThreshold (1024*FreeSpaceThreshold);
        END (*IF*);
        SetHashMax (HashMax);
        LoadUpdateableINIData;
        RETURN TRUE;

    END LoadINIData;

(********************************************************************************)

PROCEDURE WriteHostID (ID: ARRAY OF LOC);

    VAR result: ARRAY [0..16] OF CHAR;

    BEGIN
        IPToString (ID, TRUE, result);
        WriteString (result);
    END WriteHostID;

(********************************************************************************)

PROCEDURE RunTheServer;

    (*  OPERATING AS A SERVER                                                       *)
    (*     1. (Compulsory) Call "bind" to bind the socket with a local address.     *)
    (*        You can usually afford to specify INADDR_ANY as the machine           *)
    (*        address, but you'd normally bind to a specific port number.           *)
    (*     2. Call "listen" to indicate your willingness to accept connections.     *)
    (*     3. Call "accept", getting a new socket (say ns) from the client.         *)
    (*     4. Use procedures "send" and "recv" to transfer data, using socket ns.   *)
    (*        (Meanwhile, your original socket remains available to accept          *)
    (*        more connections, so you can continue with more "accept" operations   *)
    (*        in parallel with these data operations.  If so, you should of course  *)
    (*        be prepared to run multiple threads.)                                 *)
    (*     5. Use "soclose(ns)" to terminate the session with that particular       *)
    (*        client.                                                               *)
    (*     6. Use "soclose" on your original socket to clean up at the end.         *)

    VAR ns: Socket;  myaddr, client: SockAddr;
        temp: CARDINAL;
        LogID: TransactionLogID;
        LogLine: ARRAY [0..255] OF CHAR;

    BEGIN
        RapidShutdown := FALSE;
        IF sock_init() <> 0 THEN
            IF ScreenEnabled THEN
                WriteString ("No network.");
            END (*IF*);
            RETURN;
        END (*IF*);

        LogID := CreateLogID (FTPDctx, "       ");
        IF CalledFromInetd THEN

            IF ScreenEnabled THEN
                WriteString ("FtpServer started from inetd, socket ");
                WriteInt (MainSocket);  WriteLn;
            END (*IF*);
            LogTransactionL (LogID, "Server started.");
            temp := SIZE(client);
            getpeername (MainSocket, client, temp);
            IF NOT NewSession (MainSocket, client, LogID) THEN
                LogTransactionL (LogID, "Failed to create client session.");
            END (*IF*);

        ELSE

            MainSocket := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

            (* Allow reuse of the port we're binding to. *)

            temp := 1;
            setsockopt (MainSocket, 0FFFFH, 4, temp, SIZE(CARDINAL));

            LogLine := "FtpServer v";
            Strings.Append (FV.version, LogLine);
            IF ScreenEnabled THEN
                ClearScreen;  SetBoundary (2, 30);
                UpdateTopScreenLine (0, LogLine);
                UpdateTopScreenLine (20, "Copyright (C) 1998-2014 Peter Moylan");
                UpdateTopScreenLine (62, "Users: 0");

                WriteString ("Listening on ");
                IF BindAddr = 0 THEN WriteString ("all interfaces,");
                ELSE WriteHostID (BindAddr);
                END (*IF*);
                WriteString (" port ");
                WriteCard (ServerPort);
                WriteString (", socket ");
                WriteInt(MainSocket);  WriteString ("     ");  WriteLn;
                EVAL (SetBreakHandler(ControlCHandler));
            END (*IF*);
            Strings.Append (" started", LogLine);
            LogTransaction (LogID, LogLine);

            (* Now have the socket, bind to our machine. *)

            WITH myaddr DO
                family := AF_INET;
                WITH in_addr DO
                    port := Swap2 (ServerPort);
                    addr := BindAddr;
                    zero := Zero8;
                END (*WITH*);
            END (*WITH*);

            IF bind (MainSocket, myaddr, SIZE(myaddr)) THEN
                Strings.Assign ("Socket error ", LogLine);
                AppendCard (sock_errno(), LogLine);
                LogTransaction (LogID, LogLine);
                IF ScreenEnabled THEN
                    WriteString ("                             Cannot bind to server port, exiting.");
                    WriteLn;
                END (*IF*);
                RapidShutdown := TRUE;

            ELSE

                (* Go into listening mode. *)

                IF listen (MainSocket, 1) THEN
                    Strings.Assign ("Socket error ", LogLine);
                    AppendCard (sock_errno(), LogLine);
                    LogTransaction (LogID, LogLine);
                END (*IF*);

                WHILE WaitForSocket (MainSocket, MAX(CARDINAL)) > 0 DO
                    temp := SIZE(client);
                    ns := accept (MainSocket, client, temp);
                    IF ns <> NotASocket THEN

                        (* Allow the reception of out-of-band data. *)

                        temp := 1;
                        IF setsockopt (ns, 0FFFFH, 0100H, temp, SIZE(CARDINAL)) THEN
                        END (*IF*);

                        (* Start the client session. *)

                        IF NOT NewSession (ns, client, LogID) THEN
                            LogTransactionL (LogID, "Failed to create client session");
                        END (*IF*);
                    END (*IF*);
                END (*WHILE*);

            END (*IF*);

            IF soclose(MainSocket) THEN
                (*psock_errno ("");*)
            END (*IF*);

        END (*IF*);

        (* End of operation, shut down the server. *)

        IF NOT RapidShutdown THEN
            IF (NOT CalledFromInetd) AND (NumberOfUsers() > 0) THEN
                KillAllUsers (FALSE);
                LogTransactionL (LogID, "Waiting for existing users to finish");
            END (*IF*);
            WHILE (NumberOfUsers() > 0) AND NOT RapidShutdown DO
                Sleep (1000);
            END (*WHILE*);
        END (*IF*);

        (* End of gentle prodding, kill any remaining users. *)

        RapidShutdown := TRUE;
        KillAllUsers (TRUE);
        LogLine := "FtpServer v";
        Strings.Append (FV.version, LogLine);
        Strings.Append (" closing down", LogLine);
        LogTransaction (LogID, LogLine);
        WHILE NumberOfUsers() > 0 DO
            Sleep (400);
        END (*WHILE*);

    END RunTheServer;

(********************************************************************************)
(*                    TASK TO CATCH UPDATES TO THE INI DATA                     *)
(********************************************************************************)

PROCEDURE INIChangeDetector;

    (* Runs as a separate task.  Rereads some of the configuration data each    *)
    (* time a public event semaphore tells us that there's been a change.       *)

    CONST semName = "\SEM32\FTPSERVER\UPDATED";

    VAR count: CARDINAL;

    BEGIN
        UpdaterFlag := 0;
        IF OS2.DosOpenEventSem (semName, UpdaterFlag) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, UpdaterFlag, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            OS2.DosWaitEventSem (UpdaterFlag, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (UpdaterFlag, count);
            IF NOT ShutdownInProgress THEN
                LoadUpdateableINIData;
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(UpdaterFlag);
        Signal (TaskDone);

    END INIChangeDetector;

(********************************************************************************)
(*                   TASK TO CATCH EXTERNAL SHUTDOWN REQUESTS                   *)
(********************************************************************************)

PROCEDURE ShutdownRequestDetector;

    (* Runs as a separate task.  Detects a signal on the global event semaphore *)
    (* by which an external program can request a shutdown.                     *)

    CONST semName = "\SEM32\FTPSERVER\SHUTDOWN";

    VAR count: CARDINAL;

    BEGIN
        ExternalShutdownRequest := 0;
        IF OS2.DosOpenEventSem (semName, ExternalShutdownRequest) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ExternalShutdownRequest, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        (* We treat a signal on this event semaphore in the same way as we      *)
        (* treat a CTRL/C.  In particular, we accept it more than once.         *)

        WHILE NOT RapidShutdown DO
            OS2.DosWaitEventSem (ExternalShutdownRequest, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (ExternalShutdownRequest, count);
            Signal (ShutdownRequest);
            IF count > 0 THEN
                Signal (ShutdownRequest);
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(ExternalShutdownRequest);

    END ShutdownRequestDetector;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

PROCEDURE main;

    (* We make this a separate procedure because an EXCEPTIONREGISTRATIONRECORD *)
    (* is, according to the documentation, required to be on the stack.         *)

    VAR exRegRec: OS2.EXCEPTIONREGISTRATIONRECORD;
        ExceptqActive: BOOLEAN;

    BEGIN
        ExceptqActive := InstallExceptq (exRegRec);
        RunTheServer;
        IF ExceptqActive THEN
            UninstallExceptq (exRegRec);
        END (*IF*);
    END main;

(********************************************************************************)

BEGIN
    ScreenEnabled := NotDetached();
    CalledFromInetd := FALSE;
    SetVersion (FV.version);
    GetParameters;
    ShutdownInProgress := FALSE;
    RapidShutdown := FALSE;
    CreateSemaphore (ShutdownRequest, 0);
    CreateSemaphore (TaskDone, 0);
    EVAL(CreateTask (ShutdownChecker, 1, "ftp kbd"));
    EVAL(CreateTask (INIChangeDetector, 2, "update"));
    EVAL(CreateTask (ShutdownRequestDetector, 2, "shutdown"));
    IF LoadINIData() THEN
        main();
    END (*IF*);
    ShutdownInProgress := TRUE;
    OS2.DosPostEventSem (UpdaterFlag);   (* to terminate INIChangeDetector *);
    Wait (TaskDone);
    Signal (ShutdownRequest);
    Wait (TaskDone);
END Ftpd.

