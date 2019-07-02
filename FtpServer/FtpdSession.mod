(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
(*  Copyright (C) 2018   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE FtpdSession;

        (********************************************************)
        (*                                                      *)
        (*       Part of the ftp server - handles a session     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 August 1997                  *)
        (*  Last edited:        16 December 2018                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT STextIO;   (* while debugging *)

FROM SYSTEM IMPORT CARD32, INT32, ADDRESS;

IMPORT ChanConsts, RndFile, IOChan, Strings;

FROM OS2 IMPORT
    (* const*)  FERR_DISABLEHARDERR,
    (* proc *)  DosError;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FtpCommands IMPORT
    (* type *)  Session,
    (* var  *)  FTPDctx,
    (* proc *)  SetTransactionLogLevel, OpenSession, CloseSession,
                HandleCommand, KillDataChannel;

FROM LoggedOnUsers IMPORT
    (* proc *)  ClientAddressAcceptable;

FROM FtpTransfers IMPORT
    (* proc *)  VersionIs, NotifyMaxUsers, SendMessageFile, CloseSocket;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  send, recv, soclose, so_cancel;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, LogTransaction, LogTransactionL, DiscardLogID;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCard, AddEOL;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString;

FROM SplitScreen IMPORT
    (* proc *)  WriteStringAt;

FROM Queues IMPORT
    (* type *)  Queue,
    (* proc *)  CreateQueue, DestroyQueue, AddToQueue, TakeFromQueue;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Signal;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Timer IMPORT
    (* proc *)  Sleep, TimedWaitSpecial;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release, CreateTask,
                CreateTask1, TaskExit;

(************************************************************************)

CONST ClientPriority = 3;

TYPE
    (* String form of a session identifier. *)

    SevenChar = ARRAY [0..7] OF CHAR;

    (* Data used in creating a new instance of the session handler task. *)

    NewSessionPointer = POINTER TO
                           RECORD
                               socket: Socket;
                               IPAddress: CARDINAL;
                           END (*RECORD*);

    (* Data needed by the timeout checker task. *)

    KeepAlivePointer = POINTER TO
                           RECORD
                               SocketOpen, dying: BOOLEAN;
                               WatchdogRunning: BOOLEAN;
                               TimedOut: BOOLEAN;
                               sem: Semaphore;
                               socket: Socket;
                               session: Session;
                               SessionID: SevenChar;
                           END (*RECORD*);

(************************************************************************)

VAR
    (* A queue of KeepAlive records, which are passed to the    *)
    (* TimeoutChecker tasks as they are created.                *)

    KeepAliveQueue: Queue;

    (* Timeout delay, in milliseconds, and its mutual exclusion lock. *)

    MaxTime: CARDINAL;
    MaxTimeLock: Lock;

    (* Maximum allowed number of simultaneous users.  For critical *)
    (* section we use lock UserCountLock, see below.               *)

    MaxUsers: CARDINAL;

    (* Count of active users, and a lock to protect it. *)

    UserCount: CARDINAL;
    UserCountLock: Lock;

    (* Flag to control whether we write the number of users to the screen. *)

    LogToScreen: BOOLEAN;

    (* An option to disable compatibility with Telnet. *)

    TelnetDisable: BOOLEAN;

    (* Version number. *)

    version: ARRAY [0..15] OF CHAR;

(************************************************************************)
(*                   SETTING MISCELLANEOUS PARAMETERS                   *)
(************************************************************************)

PROCEDURE SetVersion (v: ARRAY OF CHAR);

    (* Stores the version number. *)

    BEGIN
        Strings.Assign (v, version);
        VersionIs (v);
    END SetVersion;

(************************************************************************)

PROCEDURE DisableTelnetCompatibility (val: BOOLEAN);

    (* Disables recognition of Telnet IAC character iff val is TRUE. *)

    BEGIN
        TelnetDisable := val;
    END DisableTelnetCompatibility;

(************************************************************************)

PROCEDURE SetTransactionLogging (level: CARDINAL;
                                 VAR (*IN*) logfile: ARRAY OF CHAR;
                                 VAR (*IN*) SyslogHost: ARRAY OF CHAR);

    (* Option to control transaction logging: 0 for none, 1 for disk,   *)
    (* 2 for screen, 4 for pipe, 8 for translog, and combinations.      *)

    BEGIN
        IF level > 15 THEN
            level := 15;
        END (*IF*);
        LogToScreen := ODD (level DIV 2);
        SetTransactionLogLevel (level, logfile, SyslogHost);
    END SetTransactionLogging;

(************************************************************************)

PROCEDURE SetMaxUsers (limit: CARDINAL);

    (* Sets an upper limit on the number of simultaneous users. *)

    BEGIN
        Obtain (UserCountLock);
        MaxUsers := limit;
        Release (UserCountLock);
        NotifyMaxUsers (limit);
    END SetMaxUsers;

(************************************************************************)

PROCEDURE SetTimeout (seconds: CARDINAL);

    (* Specifies how long a session can be idle before it is forcibly   *)
    (* closed.                                                          *)

    CONST
        limit_in_ms = MAX(CARD32) DIV 2;
        limit = limit_in_ms DIV 1000;

    BEGIN
        Obtain (MaxTimeLock);
        IF seconds > limit THEN
            MaxTime := limit_in_ms;
        ELSE
            MaxTime := 1000*seconds;
        END (*IF*);
        Release (MaxTimeLock);
    END SetTimeout;

(************************************************************************)
(*                UPDATING THE COUNT OF ACTIVE SESSIONS                 *)
(************************************************************************)

PROCEDURE UpdateCount (increment: INTEGER): CARDINAL;

    (* Updates the count of the number of users, and returns the new    *)
    (* count.  Special case: if this would take us beyond the MaxUsers  *)
    (* limit, then the count is not updated and the returned value      *)
    (* is zero.                                                         *)

    VAR value, pos: CARDINAL;  Buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        Obtain (UserCountLock);
        IF increment > 0 THEN INC (UserCount, increment);
        ELSIF increment < 0 THEN DEC (UserCount, -increment)
        END (*IF*);
        value := UserCount;
        IF value > MaxUsers THEN
            DEC (UserCount, increment);  value := 0;
        ELSIF LogToScreen THEN
            pos := 0;
            ConvertCard (value, Buffer, pos);
            Buffer[pos] := ' ';  INC(pos);
            Buffer[pos] := CHR(0);
            WriteStringAt (0, 69, Buffer);
        END (*IF*);
        Release (UserCountLock);
        RETURN value;
    END UpdateCount;

(************************************************************************)

PROCEDURE NumberOfUsers(): CARDINAL;

    (* Returns the number of users who are currently logged on. *)

    BEGIN
        RETURN UpdateCount(0);
    END NumberOfUsers;

(************************************************************************)
(*                         THE WATCHDOG TIMER                           *)
(************************************************************************)

PROCEDURE TimeoutChecker;

    (* A new instance of this task is created for each client session.  *)
    (* It kills the corresponding SessionHandler task if more than      *)
    (* MaxTime milliseconds have passed since the last Signal() on the  *)
    (* session's KeepAlive semaphore.                                   *)

    (* This is a workaround.  I would have preferred to set the         *)
    (* timeout in the socket options, but I haven't yet figured out     *)
    (* how to do it.  An older version of IBM's sockets documentation   *)
    (* gave details on the send and receive timeouts, but this seems    *)
    (* to have disappeared from later versions of the documentation.    *)

    VAR p: KeepAlivePointer;  LogID: TransactionLogID;
        TimeLimit, KillCount: CARDINAL;

    BEGIN
        p := TakeFromQueue (KeepAliveQueue);
        p^.WatchdogRunning := TRUE;
        p^.SessionID[0] := 'W';
        LogID := CreateLogID (FTPDctx, p^.SessionID);
        LogTransactionL (LogID, "Watchdog timer started");
        REPEAT
            Obtain (MaxTimeLock);
            TimeLimit := MaxTime;
            Release (MaxTimeLock);
            TimedWaitSpecial (p^.sem, TimeLimit, p^.TimedOut);
        UNTIL p^.TimedOut OR p^.dying;
        IF p^.TimedOut THEN
            LogTransactionL (LogID, "Timeout, killing data channel");
            KillDataChannel (p^.session);
            LogTransactionL (LogID, "Cancelling client socket");
            IF p^.socket <> NotASocket THEN
                so_cancel (p^.socket);
            END (*IF*);
            p^.socket := NotASocket;
        ELSE
            LogTransactionL (LogID, "Session terminated without timing out");
            Sleep(50);     (* Let session close socket itself *)
        END (*IF*);

        (* Wait for the socket to be closed. *)

        IF p^.SocketOpen THEN
            LogTransactionL (LogID, "Waiting for client socket to be closed");
        END (*IF*);
        KillCount := 0;
        WHILE p^.SocketOpen DO
            Sleep (5000);

            (* For the really stubborn cases, keep trying to cancel *)
            (* the sockets.                                         *)

            IF p^.SocketOpen THEN
                LogTransactionL (LogID, "Still trying to cancel client socket");
                KillDataChannel (p^.session);
                so_cancel (p^.socket);
                p^.socket := NotASocket;
            END (*IF*);
            INC (KillCount);

            IF KillCount >= 10 THEN
                LogTransactionL (LogID, "Unable to cancel client socket, giving up");
                p^.SocketOpen := FALSE;
            END (*IF*);

        END (*WHILE*);

        LogTransactionL (LogID, "Watchdog timer closing");
        DiscardLogID (LogID);

        (* Note that the KeepAlive record still exists.  We leave it    *)
        (* up to the client thread to discard it.                       *)

        p^.WatchdogRunning := FALSE;

    END TimeoutChecker;

(************************************************************************)
(*                       THE SESSION TASK CODE                          *)
(************************************************************************)

PROCEDURE SessionHandler (arg: ADDRESS);

    (* The task that handles a client session, i.e. this is where all   *)
    (* the real work is done.  There might be several instances of this *)
    (* task running, one for each session that is still open.           *)

    CONST NUL = CHR(0);  CR = CHR(13);  LF = CHR(10);
          BannedHostDelay = 30*1000 (*milliseconds*);

    VAR S: Socket;
        LogID: TransactionLogID;
        CmdBuffer: ARRAY [0..511] OF CHAR;

        (* Temporary buffer for BuildCommand.  Strictly speaking this   *)
        (* should be a static array, to hold surplus characters that    *)
        (* belong to the next command, but in practice FTP clients      *)
        (* never send a command until after getting the response to     *)
        (* the last command.                                            *)

        TmpBuffer: ARRAY [0..127] OF CHAR;
        tmppos, tmplength, length: CARDINAL;  ch: CHAR;

    (********************************************************************)

    PROCEDURE StoreChar (ch: CHAR);

        (* Appends ch to CmdBuffer, except where this would cause       *)
        (* buffer overflow.                                             *)

        BEGIN
            IF length < SIZE(CmdBuffer) THEN
                CmdBuffer[length] := ch;  INC(length);
            END (*IF*);
        END StoreChar;

    (********************************************************************)

    PROCEDURE BuildCommand(): BOOLEAN;

        (* Gets the incoming command and stores it it CmdBuffer, taking *)
        (* care of special handling of Telnet control codes and CR, LF  *)
        (* handling.  In theory we should be able to accept the Telnet  *)
        (* codes SYNCH and IP, while also refusing Telnet negotiations  *)
        (* by sending DONT/WONT replies.  In practice it seems that I   *)
        (* can get away with ignoring these requirements; I suspect     *)
        (* that FTP client implementers ignore Telnet compatibility.    *)

        (* If we receive CR then the way we handle it depends on what   *)
        (* follows:                                                     *)
        (*     CR LF       interpret as end-of-line                     *)
        (*     CR NUL      accept the CR as a normal data character,    *)
        (*                   discard the NUL                            *)
        (*     CR NUL LF   special case of the above: store as CR LF,   *)
        (*                   but don't interpret it as end of line.     *)
        (*     CR          (followed by anything else) Should not       *)
        (*                   occur, but I'll choose to store it.        *)

        (* Except in the special case of CR NUL LF, any LF will be      *)
        (* taken as a line terminator.  The standards forbid a bare     *)
        (* LF, but I suspect that some old clients use it.              *)
        (* NUL is ignored everywhere except after CR.                   *)

        VAR IACreceived, IgnoreNext: BOOLEAN;
            FoundCR, FoundCRNul: BOOLEAN;

        BEGIN
            length := 0;
            IACreceived := FALSE;  IgnoreNext := FALSE;
            FoundCR := FALSE;  FoundCRNul := FALSE;

            LOOP
                IF tmppos >= tmplength THEN
                    tmplength := recv (S, TmpBuffer, SIZE(TmpBuffer), 0);
                    IF (tmplength = MAX(CARDINAL)) OR (tmplength = 0) THEN
                        RETURN FALSE;
                    END (*IF*);
                    tmppos := 0;
                END (*IF*);
                ch := TmpBuffer[tmppos];  INC(tmppos);

                (* This next section skips over Telnet control codes (which we  *)
                (* don't really want to know about).  A Telnet control code is  *)
                (* two or three bytes long, where the first byte is CHR(255).   *)

                (* Special case: the option 'TelnetDisable' forces us to treat  *)
                (* CHR(255) as a normal character rather than the start of a    *)
                (* Telnet control code.  This option is needed to support       *)
                (* the Windows-1251 character set, which is widely used in      *)
                (* Russia and in which - in violation of the Telnet standard -  *)
                (* the code CHR(255) means a lower-case 'ya'.                   *)

                IF IgnoreNext THEN
                    IgnoreNext := FALSE;
                ELSIF IACreceived THEN
                    IACreceived := FALSE;
                    IF ORD(ch) = 255 THEN
                        IF length < SIZE(CmdBuffer) THEN
                            CmdBuffer[length] := ch;  INC(length);
                        END (*IF*);
                    ELSIF ORD(ch) > 250 THEN
                        IgnoreNext := TRUE;
                    END (*IF*);
                ELSIF (ORD(ch) = 255) THEN
                    IACreceived := NOT TelnetDisable;

                (* See comments at the beginning of this procedure      *)
                (* about how CR and LF should be handled.               *)

                ELSIF FoundCR THEN
                    IF ch = CR THEN
                        StoreChar(CR);
                    ELSE
                        FoundCR := FALSE;
                        IF ch = LF THEN
                            StoreChar (NUL);
                            RETURN TRUE;
                        ELSIF ch = NUL THEN
                            FoundCRNul := TRUE;
                            StoreChar(CR);
                        ELSE
                            StoreChar(ch);
                        END (*IF*);
                    END (*IF*);
                ELSIF ch = CR THEN
                    FoundCR := TRUE;
                    FoundCRNul := FALSE;
                ELSE
                    IF (ch = LF) AND NOT FoundCRNul THEN
                        StoreChar (NUL);
                        RETURN TRUE;
                    ELSE
                        StoreChar (ch);
                        FoundCRNul := FALSE;
                    END (*IF*);
                END (*IF*);

            END (*LOOP*);

        END BuildCommand;

    (********************************************************************)

    VAR NSP: NewSessionPointer;
        Sess: Session;
        IPAddress, UserNumber, j: CARDINAL;  KA: KeepAlivePointer;
        KeepAliveSemaphore: Semaphore;
        Quit, EndSession: BOOLEAN;
        IPBuffer: ARRAY [0..16] OF CHAR;
        SessionNumber: SevenChar;
        LogMessage: ARRAY [0..127] OF CHAR;

    BEGIN                   (* Body of SessionHandler *)

        DosError (FERR_DISABLEHARDERR);
        NSP := arg;
        S := NSP^.socket;
        IPAddress := NSP^.IPAddress;
        DISPOSE (NSP);

        (* Create the log file ID for this session. *)

        CardinalToString (S, SessionNumber, 7);  SessionNumber[7] := CHR(0);
        LogID := CreateLogID (FTPDctx, SessionNumber);

        (* Initial transaction log message. *)

        IPToString (IPAddress, TRUE, IPBuffer);
        Strings.Assign ("New client ", LogMessage);
        Strings.Append (IPBuffer, LogMessage);
        LogTransaction (LogID, LogMessage);

        (* Check for acceptable client address. *)

        IF NOT ClientAddressAcceptable (IPAddress) THEN
            Sleep (BannedHostDelay);
            Strings.Assign ("Client address rejected ", LogMessage);
            Strings.Append (IPBuffer, LogMessage);
            LogTransaction (LogID, LogMessage);
            CmdBuffer := "421 Connection refused";
            j := AddEOL (CmdBuffer);
            EVAL (send (S, CmdBuffer, j, 0));
            CloseSocket (S, LogID);
            DiscardLogID (LogID);
            TaskExit;
        END (*IF*);

        UserNumber := UpdateCount (+1);

        (* Check for too many users. *)

        IF UserNumber = 0 THEN
            LogTransactionL (LogID, "Too many users");
            CmdBuffer := "421 User limit exceeded, try again later";
            j := AddEOL (CmdBuffer);
            EVAL (send (S, CmdBuffer, j, 0));
            CloseSocket (S, LogID);
            DiscardLogID (LogID);
            TaskExit;
        END (*IF*);

        (* Create the session information structure. *)

        CreateSemaphore (KeepAliveSemaphore, 0);
        Sess := OpenSession (S, UserNumber, IPAddress, LogID, KeepAliveSemaphore);

        (* Create an instance of the TimeoutChecker task. *)

        NEW (KA);
        WITH KA^ DO
            SocketOpen := TRUE;  socket := S;
            WatchdogRunning := TRUE;
            dying := FALSE;
            sem := KeepAliveSemaphore;
            session := Sess;
            SessionID := SessionNumber;
            TimedOut := FALSE;
        END (*WITH*);

        IF CreateTask (TimeoutChecker, ClientPriority+2, "ftpd timeout") THEN
            AddToQueue (KeepAliveQueue, KA);
        ELSE
            LogTransactionL (LogID, "Failed to create watchdog thread");
            CmdBuffer := "421 Out of thread resources, try again later";
            j := AddEOL (CmdBuffer);
            EVAL (send (S, CmdBuffer, j, 0));
            CloseSession (Sess);
            CloseSocket (S, LogID);
            DestroySemaphore (KeepAliveSemaphore);
            DISPOSE (KA);
            EVAL (UpdateCount(-1));
            DiscardLogID (LogID);
            TaskExit;
        END (*IF*);

        (* Send the "welcome" message. *)

        SendMessageFile (S, "Welcome0.MSG", "220", UserNumber, NIL, FALSE);
        CmdBuffer := "220 FtpServer ";
        Strings.Append (version, CmdBuffer);
        Strings.Append (" ready", CmdBuffer);
        j := AddEOL (CmdBuffer);
        EndSession := send (S, CmdBuffer, j, 0) = MAX(CARDINAL);

        (* Here's the main command processing loop.  We leave it when   *)
        (* the client issues a QUIT command, or when socket             *)
        (* communications are lost, or when we get a timeout on the     *)
        (* KeepAlive semaphore.                                         *)

        tmppos := 0;  tmplength := 0;  Quit := FALSE;
        LOOP
            IF EndSession THEN EXIT(*LOOP*) END(*IF*);
            IF BuildCommand() THEN
                Signal (KeepAliveSemaphore);
                HandleCommand (Sess, CmdBuffer, Quit, EndSession);
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        (* Work out whether the session was terminated by a QUIT, or a  *)
        (* timeout, or a communications failure.                        *)

        IF KA^.TimedOut THEN
            LogTransactionL (LogID, "Timed out");
        ELSIF Quit THEN
            LogTransactionL (LogID, "Session terminated by QUIT from client");
        ELSIF EndSession THEN
            LogTransactionL (LogID, "Session terminated");
        ELSE
            LogTransactionL (LogID, "Socket closed or communication failure");
        END (*IF*);

        (* Note potential race condition.  When we close the session,   *)
        (* Sess is NIL but the watchdog thread also has a copy of this  *)
        (* pointer.  It's important to clear the copy so that the       *)
        (* watchdog does not try to use an obsolete pointer.            *)

        KA^.session := NIL;
        CloseSession (Sess);
        KA^.dying := TRUE;  Signal (KA^.sem);
        CloseSocket (S, LogID);
        KA^.SocketOpen := FALSE;
        WHILE KA^.WatchdogRunning DO
            Signal (KA^.sem);
            Sleep (500);
        END (*WHILE*);
        DestroySemaphore (KA^.sem);
        DISPOSE (KA);
        EVAL (UpdateCount(-1));
        DiscardLogID (LogID);
        TaskExit;

    END SessionHandler;

(************************************************************************)

PROCEDURE NewSession (S: Socket;  addr: SockAddr;
                         LogID: TransactionLogID): BOOLEAN;

    (* Starts and runs a client session.  The session runs in a         *)
    (* separate thread; this procedure returns after starting the       *)
    (* session, it does not wait until the session is over.             *)

    VAR NSP: NewSessionPointer;  success: BOOLEAN;

    BEGIN
        NEW (NSP);
        WITH NSP^ DO
            socket := S;  IPAddress := addr.in_addr.addr;
        END (*WITH*);
        success := CreateTask1 (SessionHandler, ClientPriority, "ftpd session", NSP);
        IF NOT success THEN
            CloseSocket (S, LogID);
            DISPOSE (NSP);
        END (*IF*);
        RETURN success;
    END NewSession;

(************************************************************************)
(*                        MODULE INITIALISATION                         *)
(************************************************************************)

BEGIN
    CreateLock (MaxTimeLock);
    CreateLock (UserCountLock);
    Obtain (UserCountLock);  UserCount := 0;  Release (UserCountLock);
    CreateQueue (KeepAliveQueue);
    LogToScreen := FALSE;
    TelnetDisable := FALSE;
FINALLY
    DestroyLock (MaxTimeLock);
    DestroyLock (UserCountLock);

    (* Don't destroy KeepAliveQueue because timeout checker might still be running. *)

    (*DestroyQueue (KeepAliveQueue);*)

END FtpdSession.

