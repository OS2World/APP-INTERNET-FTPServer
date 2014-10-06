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

IMPLEMENTATION MODULE FtpTransfers;

        (********************************************************)
        (*                                                      *)
        (*          FTP server: file operations                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 August 1997                  *)
        (*  Last edited:        25 May 2014                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CARD8, CARD16, LOC, ADDRESS, CAST;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64, Max64,
    (* proc *)  Add64, Sum64, Compare64, FLOAT64;

IMPORT Volume, FileSys, Strings, SysClock;

FROM TimeConv IMPORT
    (* proc *)  millisecs;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, AddressFamily, SocketType, SockAddr,
    (* proc *)  socket, connect, send, recv, soclose, setsockopt,
                bind, listen, getsockname, accept,
                getpeername, so_cancel, sock_errno;

FROM Conversions IMPORT
    (* proc *)  Card64ToString;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY;

FROM InetUtilities IMPORT
    (* proc *)  Swap2, Swap4, Synch, OpenLogFile,
                CloseLogFile, IPToString, ConvertDecimal64, WaitForDataSocket,
                AppendString, ConvertCard, ConvertCardZ, AppendCard, AddEOL;

FROM Inet2Misc IMPORT
    (* proc *)  AddressToHostName;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM CodePage IMPORT
    (* proc *)  TranslateFromUTF8, TranslateToUTF8;

FROM NameLookup IMPORT
    (* proc *)  StartNameLookup, CancelNameLookup, GetName;

FROM FDUsers IMPORT
    (* type *)  User, UserCategory, FName, ListingOption, ListingOptions,
    (* proc *)  ReadUserData, DestroyUserData, PasswordAcceptable,
                NoteLoginTime,
                MakeFName, DiscardFName, SameDrive, SameParent,
                SetWorkingDirectory, ListDirectory, FileOrDirExists,
                HaveSeenMessage, OpenForReading, OpenForWriting, OpenForAppend,
                RemoveFile, IsAFile, IsADirectory, MayListFiles,
                CanSeeFileOrDir,
                CanReadFile, CanWriteFile, CanCreateDirectory, CanDeleteFile,
                RenameIsLegal, Rename, CanDeleteDirectory, RemoveDirectory,
                SetDateTime, GetDateTime, CreateDirectory, GetFileSize,
                NameOfCurrentDirectory, RealNameOfCurrentDirectory,
                PermissionString, MakeFullName, MakeShortName,
                SpaceAvailable, GetUserNumber;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry, FileAttribute,
    (* proc *)  OpenOldFile, CloseFile, SetPosition, StartPosition,
                WriteRaw, ReadRaw,
                FWriteChar, FWriteString, FWriteLn, FWriteCard, FWriteLJCard,
                FWriteLJCard64, FWriteZCard, SetFileSize,
                FirstDirEntry, NextDirEntry, DirSearchDone;

FROM Names IMPORT
    (* type *)  HostName, FilenameString;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM OS2 IMPORT
    (* proc *)  DosError, DosSleep,
    (* const*)  FERR_DISABLEHARDERR;

(********************************************************************************)

CONST
    Nul = CHR(0);
    MillisecondsPerDay = FLOAT (24 * 60 * 60 * 1000);
    ASCIIasInitialDefault = TRUE;
    TraceOpenSockets = FALSE;

TYPE
    CharSet = SET OF CHAR;

    FileNameString = ARRAY [0..255] OF CHAR;

    (* The LogEntry type is used for user logging. *)

    Operation = (upload, partupload, download, partdownload, delete, makedir);

    LogEntryPointer = POINTER TO LogEntry;

    LogEntry = RECORD
                   next: LogEntryPointer;
                   what: Operation;
                   bytes: CARD64;
                   duration: REAL;
                   filename: FileNameString;
               END (*RECORD*);

    (* The fields in the ClientFileInfo record have the following meanings.     *)
    (*   user            The data structure that keeps track of the user        *)
    (*                   permissions.                                           *)
    (*   UserNum         User number.  Has no significance except as the        *)
    (*                    identification number to be used in the welcome       *)
    (*                    message.                                              *)
    (*   TransLogID      ID for the transaction log.                            *)
    (*   KickMe          A semaphore.  The session will time out unless we      *)
    (*                    do a Signal on it every so often.                     *)
    (*   iname           File name to be used for the next transfer operation.  *)
    (*   flags           Options for a directory listing                        *)
    (*   CommandSocket   Socket used for the command channel.                   *)
    (*   ServerName      A SockAddr structure for the default server-side       *)
    (*                     data port.                                           *)
    (*   ClientPort      The remote client address to use for data transfers.   *)
    (*   SpeedLimit      Upper limit on download rate, in bytes per second.     *)
    (*                     Must be nonzero.                                     *)
    (*   UpSpeedLimit    Like SpeedLimit, but for upload rate.                  *)
    (*   UpSizeLimit     Upper limit, in bytes, of uploaded file size.          *)
    (*   LineStructured  TRUE iff current transfer type is ASCII or similar     *)
    (*   StreamMode      TRUE iff current transfer mode is stream               *)
    (*   PassiveOperation  TRUE iff we've entered Passive Mode                  *)
    (*   IsManager       TRUE if this user is a manager                         *)
    (*   AnonUser        TRUE iff user password is e-mail address               *)
    (*   AllowForFirewall TRUE iff we should use the "behind firewall" strategy *)
    (*   LogIt           TRUE iff we allow logging for this user                *)
    (*   RestartPointSet TRUE iff the RestartPoint variable (see below) has     *)
    (*                     been explicitly given a value since the last         *)
    (*                     transfer, as distinct from just defaulting back to   *)
    (*                     zero.                                                *)
    (*   FileStructure   File structure code, ignored at present.  In fact I    *)
    (*                    don't think this will ever have any use, so maybe     *)
    (*                    this field can be eliminated.                         *)
    (*   RestartPoint    Place to resume an interrupted transfer.               *)
    (*   AmountToAllocate  Prespecified file size.                              *)
    (*   Count           Progress of the current transfer.  The fields are      *)
    (*                      access lock, bytes transferred, total to transfer.  *)
    (*   Log             Saved details for user logging.                        *)
    (*                                                                          *)
    (* The Log record includes the following fields:                            *)
    (*   ClientIP        IP address of the client                               *)
    (*   StartTime       Time when the user logged in                           *)
    (*   UserName        User identification                                    *)
    (*   tagged          We have detected a probable pirate                     *)
    (*   entrycount      Number of entries in the transaction record list       *)
    (*   first, last     Head and tail of a linked list of transaction records  *)

    ClientFileInfo = POINTER TO ClientInfoRecord;
    ClientInfoRecord = RECORD
                           user: User;
                           UserNum: CARDINAL;
                           TransLogID: TransactionLogID;
                           KickMe: Semaphore;
                           iname: FName;
                           flags: ListingOptions;
                           CommandSocket: Socket;
                           ServerName: SockAddr;
                           ClientPort: RECORD
                                         socket: Socket;
                                         host: CARDINAL;
                                         port: CARD16;
                                     END (*RECORD*);
                           SpeedLimit: CARDINAL;
                           UpSpeedLimit: CARDINAL;
                           UpSizeLimit: CARD64;
                           LineStructured, StreamMode: BOOLEAN;
                           PassiveOperation: BOOLEAN;
                           IsManager, AnonUser: BOOLEAN;
                           AllowForFirewall: BOOLEAN;
                           LogIt: BOOLEAN;
                           RestartPointSet: BOOLEAN;
                           FileStructure: CHAR;
                           RestartPoint, AmountToAllocate: CARD64;
                           Count: RECORD
                                      access: Lock;
                                      amount, total: CARD64;
                                  END (*RECORD*);
                           Log: RECORD
                                    ClientIP: CARDINAL;
                                    StartTime: SysClock.DateTime;
                                    UserName: ARRAY [0..31] OF CHAR;
                                    tagged: BOOLEAN;
                                    entrycount: CARDINAL;
                                    first, last: LogEntryPointer;
                                END (*RECORD*);
                       END (*RECORD*);

(********************************************************************************)

CONST NUL = CHR(0);  CR = CHR(13);  LF = CHR(10);  CtrlZ = CHR(26);
      Digits = CharSet{'0'..'9'};

VAR MaxUsers: CARDINAL;
    MaxUsersLock: Lock;
    FreeSpaceThreshold: CARDINAL;

    (* Logging options.  Note that LogLevel=0 will force UserLogEnabled *)
    (* to FALSE.                                                        *)

    LogLevel: CARDINAL;
    UserLogEnabled, CommonLogEnabled: BOOLEAN;

    (* Log file names. *)

    UserLogFileName: FilenameString;
    CommonLogFileName: FilenameString;

    (* Option to have special behaviour when a 'tagged' directory is made. *)

    DoTagCheck: BOOLEAN;

    (* Option to alter behaviour to that appropriate behind a firewall. *)

    FWParams: RECORD
                  access: Lock;
                  BehindFirewall: BOOLEAN;
                  LocalIPAddrMin: CARDINAL;               (* network byte order *)
                  LocalIPAddrMax: CARDINAL;               (* network byte order *)
                  FirewallIPAddr: CARDINAL;               (* network byte order *)
              END (*RECORD*);

    (* The range of ports that may be used for passive transfers.  If restrict  *)
    (* is FALSE then we let the sockets interface choose a port for us.         *)

    PASVPorts: RECORD
                   access: Lock;
                   restrict: BOOLEAN;
                   MinDataPort, MaxDataPort: CARD16;       (* network byte order *)
               END (*RECORD*);

    (* Version number. *)

    version: ARRAY [0..15] OF CHAR;

(********************************************************************************)
(*                             TRANSFER LOGGING                                 *)
(********************************************************************************)

PROCEDURE SetLogLevel (option, level: CARDINAL;
                       VAR (*IN*) CommonLogName, UserLogName: FilenameString);

    (* Sets the amount of logging we want.  The first parameter says    *)
    (* what kind of log:                                                *)
    (*       0          no log                                          *)
    (*       1          user log                                        *)
    (*       2          common.log                                      *)
    (*       3          both user log and common.log                    *)
    (* The second parameter says how much detail to log.                *)

    BEGIN
        UserLogEnabled := ODD(option) AND (level > 0);
        CommonLogEnabled := option > 1;
        LogLevel := level;
        IF CommonLogName[0] <> Nul THEN
            CommonLogFileName := CommonLogName;
        END (*IF*);
        IF UserLogName[0] <> Nul THEN
            UserLogFileName := UserLogName;
        END (*IF*);
    END SetLogLevel;

(********************************************************************************)

PROCEDURE FWriteTime (cid: ChanId;  Time: SysClock.DateTime);

    BEGIN
        FWriteZCard (cid, Time.year, 4);  FWriteChar (cid, '-');
        FWriteZCard (cid, Time.month, 2);  FWriteChar (cid, '-');
        FWriteZCard (cid, Time.day, 2);  FWriteChar (cid, ' ');
        FWriteZCard (cid, Time.hour, 2);  FWriteChar (cid, ':');
        FWriteZCard (cid, Time.minute, 2);  FWriteChar (cid, ':');
        FWriteZCard (cid, Time.second, 2);
    END FWriteTime;

(********************************************************************************)

PROCEDURE FWriteTime2 (cid: ChanId;  Time: SysClock.DateTime);

    (* Like FWriteTime, but slightly different format. *)

    TYPE MonthNameType = ARRAY [0..12] OF ARRAY [0..2] OF CHAR;

    CONST MonthName = MonthNameType {'???', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
                                     'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
                                     'Dec'};

    VAR k: CARDINAL;

    BEGIN
        FWriteZCard (cid, Time.day, 2);  FWriteChar (cid, '/');
        k := Time.month;
        IF k > 12 THEN k := 0 END (*IF*);
        FWriteString (cid, MonthName[k]);
        FWriteChar (cid, '/');
        FWriteZCard (cid, Time.year, 4);  FWriteChar (cid, ':');
        FWriteZCard (cid, Time.hour, 2);  FWriteChar (cid, ':');
        FWriteZCard (cid, Time.minute, 2);  FWriteChar (cid, ':');
        FWriteZCard (cid, Time.second, 2);
        IF Time.zone <> -1 THEN
            IF Time.zone < 0 THEN FWriteString (cid, " +");
            ELSE FWriteString (cid, " -");
            END (*IF*);
            k := ABS (Time.zone);
            FWriteZCard (cid, k DIV 60, 2);
            FWriteZCard (cid, k MOD 60, 2);
        END (*IF*);
    END FWriteTime2;

(********************************************************************************)

PROCEDURE MakeCommonLogEntry (SS: ClientFileInfo;  kind: Operation;
                                                                count: CARD64);

    (* Adds an entry to the common.log file.    *)

    VAR cid: ChanId;  Now: SysClock.DateTime;
        Buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        SysClock.GetClock (Now);
        GetName (SS^.Log.ClientIP,  Buffer, TRUE);
        IF Buffer[0] = NUL THEN
            IPToString (SS^.Log.ClientIP, FALSE, Buffer);
        END (*IF*);

        cid := OpenLogFile (CommonLogFileName);
        FWriteString (cid, Buffer);
        FWriteString (cid, " - ");
        FWriteString (cid, SS^.Log.UserName);
        FWriteString (cid, " [");
        FWriteTime2 (cid, Now);
        FWriteString (cid, '] "');
        IF (kind = upload) OR (kind = partupload) THEN
            FWriteString (cid, "POST ");
        ELSE
            FWriteString (cid, "GET ");
        END (*IF*);
        MakeFullName (SS^.iname, Buffer);
        FWriteString (cid, Buffer);
        FWriteString (cid, ' FTP" ');
        IF (kind = upload) OR (kind = download) THEN
            FWriteString (cid, "200 ");
        ELSE
            FWriteString (cid, "404 ");
        END (*IF*);
        FWriteLJCard64 (cid, count);
        FWriteString (cid, ' "-" "ftp client"');
        FWriteLn (cid);
        CloseLogFile (cid);
    END MakeCommonLogEntry;

(********************************************************************************)

PROCEDURE KillDirectory (dirname: FileNameString);

    (* Deletes a directory and all of its contents. *)

    VAR D: DirectoryEntry;  mask: FileNameString;
        result: CARDINAL;  found: BOOLEAN;

    BEGIN
        mask := dirname;
        Strings.Append ('\*', mask);
        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found DO
            mask := dirname;
            Strings.Append ('\', mask);
            Strings.Append (D.name, mask);
            IF Strings.Equal(D.name, '.') OR Strings.Equal(D.name, '..') THEN
                (* do nothing *)
            ELSIF directory IN D.attr THEN
                KillDirectory (mask);
            ELSE
                FileSys.Remove (mask, found);
            END (*IF*);
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);
        Volume.RmvDir (dirname, result);
    END KillDirectory;

(********************************************************************************)

PROCEDURE DirectoryEmpty (dirname: FileNameString): BOOLEAN;

    (* Returns TRUE if this directory contains nothing except *)
    (* zero or more empty subdirectories.                     *)

    VAR D: DirectoryEntry;  mask: FileNameString;
        found, NoFiles: BOOLEAN;

    BEGIN
        NoFiles := TRUE;
        mask := dirname;
        Strings.Append ('\*', mask);
        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found AND NoFiles DO
            mask := dirname;
            Strings.Append ('\', mask);
            Strings.Append (D.name, mask);
            IF Strings.Equal(D.name, '.') OR Strings.Equal(D.name, '..') THEN
                (* do nothing *)
            ELSIF directory IN D.attr THEN
                NoFiles := DirectoryEmpty (mask);
            ELSE
                NoFiles := FALSE;
            END (*IF*);
            found := NoFiles AND NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);
        RETURN NoFiles;
    END DirectoryEmpty;

(********************************************************************************)

PROCEDURE FlushLog (SS: ClientFileInfo);

    (* Deletes the user log file, also removes any uploaded files or created    *)
    (* directories if the 'tagged' flag is set.                                 *)

    VAR current: LogEntryPointer;
        result: BOOLEAN;

    BEGIN
        WITH SS^.Log DO
            current := first;
            WHILE current <> NIL DO
                IF DoTagCheck AND (current^.what = makedir) THEN
                    tagged := tagged OR DirectoryEmpty(current^.filename);
                END (*IF*);
                IF tagged THEN
                    WITH current^ DO
                        CASE what OF
                            | upload, partupload:
                                       FileSys.Remove (filename, result);
                            | makedir:
                                       KillDirectory (filename);
                        ELSE
                            (* no action needed in other cases *)
                        END (*CASE*);
                    END (*WITH*);
                END (*IF*);
                first := current;  current := first^.next;
                DEC (entrycount);
                DISPOSE (first);
            END (*WHILE*);
        END (*WITH*);
    END FlushLog;

(********************************************************************************)

PROCEDURE WriteLog (SS: ClientFileInfo);

    (* Writes the details for this session to the user log file. *)

    VAR cid: ChanId;  Now: SysClock.DateTime;
        current: LogEntryPointer;  buffer: ARRAY [0..16] OF CHAR;
        NodeName: FileNameString;  rate: CARDINAL;
        done: BOOLEAN;

    BEGIN
        cid := OpenLogFile (UserLogFileName);
        WITH SS^.Log DO

            (* Login details. *)

            FWriteLn (cid);
            FWriteString (cid, UserName);  FWriteString (cid, "  ");
            IPToString (ClientIP, TRUE, buffer);
            FWriteString (cid, buffer);

            GetName (ClientIP,  NodeName, FALSE);
            IF NodeName[0] <> NUL THEN
                FWriteString (cid, " ");
                FWriteString (cid, NodeName);
            END (*IF*);
            FWriteLn (cid);

            FWriteString (cid, "Logged in:    ");
            FWriteTime (cid, StartTime);  FWriteLn (cid);

            (* Now the details of the actual files transferred. *)

            current := first;
            WHILE current <> NIL DO
                WITH current^ DO
                    CASE what OF
                        | upload:       FWriteString (cid, "Uploaded:     ");
                                        IF SS^.Log.tagged THEN
                                            FileSys.Remove (filename, done);
                                        END (*IF*);
                        | partupload:   FWriteString (cid, "Tried to put: ");
                                        IF SS^.Log.tagged THEN
                                            FileSys.Remove (filename, done);
                                        END (*IF*);
                        | download:     FWriteString (cid, "Downloaded:   ");
                        | partdownload: FWriteString (cid, "Tried to get: ");
                        | delete:       FWriteString (cid, "Deleted:      ");
                        | makedir:      FWriteString (cid, "Created dir:  ");
                                        IF SS^.Log.tagged THEN
                                            KillDirectory (filename);
                                        END (*IF*);
                    END (*CASE*);
                    FWriteString (cid, filename);
                    IF what < delete THEN
                        FWriteString (cid, " (");  FWriteLJCard64 (cid, bytes);
                        FWriteString (cid, " bytes, ");

                        (* Duration is in milliseconds; report it as seconds to *)
                        (* two decimal places.                                  *)

                        WHILE duration < 0.0 DO
                            duration := duration + MillisecondsPerDay;
                        END (*WHILE*);
                        duration := duration/1000.0;
                        IF duration <= 0.005 THEN
                            rate := 0;
                        ELSE
                            rate := TRUNC (FLOAT64(bytes)/duration + 0.5);
                        END (*IF*);
                        FWriteLJCard (cid, TRUNC(duration));
                        FWriteChar (cid, '.');
                        duration := duration - FLOAT(TRUNC(duration));
                        FWriteZCard (cid, TRUNC(100.0*duration + 0.5), 2);
                        FWriteString (cid, " seconds");
                        IF rate <> 0 THEN
                            FWriteString (cid, ", ");  FWriteLJCard (cid, rate);
                            FWriteString (cid, " bytes/s");
                        END (*IF*);
                        FWriteString (cid, ")");
                    END (*IF*);
                    FWriteLn (cid);
                END (*WITH*);
                first := current;  current := first^.next;
                DEC (entrycount);
                DISPOSE (first);
            END (*WHILE*);

            (* Logout details. *)

            IF SS^.Log.tagged THEN
                FWriteString (cid, "Note:         Pirate detected, uploaded files deleted");
                FWriteLn (cid);
            END (*IF*);
            SysClock.GetClock (Now);
            FWriteString (cid, "Finished:     ");
            FWriteTime (cid, Now);  FWriteLn (cid);

        END (*WITH*);
        CloseLogFile (cid);
    END WriteLog;

(********************************************************************************)

PROCEDURE AddToLog (SS: ClientFileInfo;  kind: Operation;
                                                count: CARD64;  time: REAL);

    (* Adds a record to the user's transfer log, also updates the common log    *)
    (* if appropriate.                                                          *)

    VAR p: LogEntryPointer;

    BEGIN
        IF CommonLogEnabled AND SS^.LogIt AND (kind <> delete) THEN
            MakeCommonLogEntry (SS, kind, count);
        END (*IF*);
        IF (UserLogEnabled AND SS^.LogIt) OR DoTagCheck THEN
            NEW (p);
            WITH p^ DO
                next := NIL;  what := kind;  bytes := count;
                duration := time;
                MakeFullName (SS^.iname, filename);
            END (*WITH*);
            WITH SS^.Log DO
                IF first = NIL THEN first := p
                ELSE last^.next := p;
                END (*IF*);
                INC (entrycount);
                last := p;
            END (*WITH*);
            IF SS^.Log.entrycount > 99 THEN
                IF UserLogEnabled AND SS^.LogIt THEN
                    WriteLog (SS);
                ELSE
                    FlushLog (SS);
                END (*IF*);
            END (*IF*);
        END (*IF*);
    END AddToLog;

(********************************************************************************)
(*                      LOGGING IN AND RELATED OPERATIONS                       *)
(********************************************************************************)

PROCEDURE EnableTaggedCheck (enable: BOOLEAN);

    (* Enables or disables special action on 'tagged' directories. *)

    BEGIN
        DoTagCheck := enable;
    END EnableTaggedCheck;

(********************************************************************************)

PROCEDURE NotifyMaxUsers (limit: CARDINAL);

    (* Limit on number of simultaneous users. *)

    BEGIN
        Obtain (MaxUsersLock);
        MaxUsers := limit;
        Release (MaxUsersLock);
    END NotifyMaxUsers;

(********************************************************************************)

PROCEDURE SetFreeSpaceThreshold (kilobytes: CARDINAL);

    (* The amount of space that must be unused on a drive before a client       *)
    (* can write to that drive.                                                 *)

    BEGIN
        FreeSpaceThreshold := kilobytes;
    END SetFreeSpaceThreshold;

(********************************************************************************)

PROCEDURE CreateSession (S: Socket;  UserNumber: CARDINAL;  LogID: TransactionLogID;
                               KeepAlive: Semaphore): ClientFileInfo;

    (* Creates a new session state record.  Some of the user information is   *)
    (* still missing, but will be filled in when FindUser is called.          *)

    CONST LoopbackAddress = 1 + 256*(0 + 256*(0 + 256*(127)));      (* 127.0.0.1 *)

    VAR result: ClientFileInfo;  peer: SockAddr;  size, temp: CARDINAL;

    BEGIN
        NEW (result);
        WITH result^ DO
            user := NIL;
            TransLogID := LogID;
            KickMe := KeepAlive;
            SysClock.GetClock (Log.StartTime);
            flags := ListingOptions{};
            iname := NIL;
            CommandSocket := S;
            size := SIZE(SockAddr);
            getsockname (S, ServerName, size);
            WITH ServerName.in_addr DO
                IF port > 0 THEN
                    port := Swap2 (Swap2(port) - 1);
                END (*IF*);
            END (*WITH*);
            UserNum := UserNumber;
            SpeedLimit := MAX(CARDINAL);
            UpSpeedLimit := MAX(CARDINAL);
            UpSizeLimit := Max64;
            IsManager := FALSE;  AnonUser := TRUE;
            LineStructured := ASCIIasInitialDefault;  StreamMode := TRUE;
            LogIt := TRUE;  RestartPointSet := FALSE;
            PassiveOperation := FALSE;  FileStructure := 'F';
            RestartPoint := Zero64;
            AmountToAllocate := Zero64;
            WITH Count DO
                CreateLock (access);
                amount := Zero64;  total := Zero64;
            END (*IF*);
            WITH Log DO
                size := SIZE(peer);
                IF getpeername (S, peer, size) THEN
                    ClientIP := 0;
                ELSE
                    ClientIP := peer.in_addr.addr;
                END (*IF*);
                temp := Swap4(ClientIP);
                StartNameLookup (ClientIP);
                Strings.Assign ("?", UserName);
                entrycount := 0;
                tagged := FALSE;
                first := NIL;  last := NIL;
            END (*WITH*);
            WITH ClientPort DO
                host := Log.ClientIP;  port := 0;
                socket := NotASocket;
            END (*WITH*);
            WITH FWParams DO
                Obtain (access);
                AllowForFirewall := BehindFirewall AND (temp <> LoopbackAddress)
                                    AND ((temp < Swap4(LocalIPAddrMin))
                                         OR (temp > Swap4(LocalIPAddrMax)));
                Release (access);
            END (*WITH*);
        END (*WITH*);
        RETURN result;
    END CreateSession;

(********************************************************************************)

PROCEDURE RecordLoginTime (SS: ClientFileInfo);

    (* Puts the current date/time as the "last login time" for this user.       *)

    BEGIN
        NoteLoginTime (SS^.user);
    END RecordLoginTime;

(********************************************************************************)

PROCEDURE CloseSession (VAR (*INOUT*) SS: ClientFileInfo);

    (* Destroys SS, also updates the user log. *)

    BEGIN
        IF SS <> NIL THEN
            CloseDataPort (SS);
            IF UserLogEnabled AND SS^.LogIt
                         AND ((LogLevel > 2) OR (SS^.Log.first <> NIL)) THEN
                WriteLog (SS);
            ELSE
                FlushLog (SS);
                CancelNameLookup (SS^.Log.ClientIP);
            END (*IF*);
            DiscardFName (SS^.iname);
            DestroyUserData (SS^.user);
            DestroyLock (SS^.Count.access);
            DISPOSE (SS);
        END(*IF*);
    END CloseSession;

(********************************************************************************)

PROCEDURE SendUserStatus (SS: ClientFileInfo);

    (* Sends a reply to the STAT command. *)

    VAR S: Socket;  pos: CARDINAL;
        buffer: ARRAY [0..255] OF CHAR;
        IPbuffer: ARRAY [0..17] OF CHAR;

    BEGIN
        S := SS^.CommandSocket;
        buffer := "  FtpServer for OS/2 and eCS, version ";
        Strings.Append (version, buffer);
        pos := AddEOL(buffer);
        EVAL (send (S, buffer, pos, 0));

        buffer := "  Server: ";
        IPToString (SS^.ServerName.in_addr.addr, TRUE, IPbuffer);
        Strings.Append (IPbuffer, buffer);
        Strings.Append ("    Client: ", buffer);
        IPToString (SS^.ClientPort.host, TRUE, IPbuffer);
        Strings.Append (IPbuffer, buffer);
        pos := AddEOL(buffer);
        EVAL (send (S, buffer, pos, 0));

        buffer := "  Logged in as ";
        IF SS^.IsManager THEN
            Strings.Append ("manager", buffer);
        ELSIF SS^.AnonUser THEN
            Strings.Append ("anonymous user", buffer);
        ELSE
            Strings.Append ("normal user", buffer);
        END (*IF*);
        pos := AddEOL(buffer);
        EVAL (send (S, buffer, pos, 0));

        buffer := "  TYPE: ";
        IF SS^.LineStructured THEN
            Strings.Append ("ASCII, FORM: Nonprint", buffer);
        ELSE
            Strings.Append ("Binary", buffer);
        END (*IF*);
        Strings.Append ("  STRUCTURE: ", buffer);
        IF SS^.FileStructure = 'F' THEN
            Strings.Append ("File", buffer);
        ELSE
            Strings.Append ("Record", buffer);
        END (*IF*);
        IF SS^.StreamMode THEN
            Strings.Append ("  MODE: Stream", buffer);
        END (*IF*);
        pos := AddEOL(buffer);
        EVAL (send (S, buffer, pos, 0));

        buffer := "  Current transfer: ";
        WITH SS^.Count DO
            Obtain (access);
            Card64ToString (amount, IPbuffer, 18);
            Strings.Append (IPbuffer, buffer);
            Strings.Append (" of ", buffer);
            Card64ToString (total, IPbuffer, 18);
            Strings.Append (IPbuffer, buffer);
            Release (access);
        END (*WITH*);
        Strings.Append (" bytes", buffer);
        pos := AddEOL(buffer);
        EVAL (send (S, buffer, pos, 0));

    END SendUserStatus;

(********************************************************************************)

PROCEDURE GetUserName (SS: ClientFileInfo;  VAR (*OUT*) Name: ARRAY OF CHAR);

    (* Returns the user name (the one we use for logging).  *)

    BEGIN
        Strings.Assign (SS^.Log.UserName, Name);
    END GetUserName;

(********************************************************************************)

PROCEDURE FindUser (Name: ARRAY OF CHAR;
                     VAR (*INOUT*) SessionInfo: ClientFileInfo;
                     VAR (*OUT*) category: UserCategory;
                     VAR (*OUT*) LogSession: BOOLEAN);

    (* Input: Name contains the argument of the USER command.        *)
    (*        S is this session's command socket.                    *)
    (* Output: Access permission data for that user.                 *)

    BEGIN
        WITH SessionInfo^ DO
            DiscardFName (iname);
            user := ReadUserData (Name, SpeedLimit, UpSpeedLimit,
                                           UpSizeLimit, category, LogSession);
            IF category <> NoSuchUser THEN

                (* Note: don't set the port, because a default port is  *)
                (* usually set before this procedure is called.         *)

                LineStructured := ASCIIasInitialDefault;  StreamMode := TRUE;
                IsManager := category = Manager;
                AnonUser := category = GuestUser;
                PassiveOperation := FALSE;  RestartPointSet := FALSE;
                FileStructure := 'F';
                RestartPoint := Zero64;
                AmountToAllocate := Zero64;
                Strings.Assign (Name, Log.UserName);
                LogIt := LogSession;
            END (*IF*);
        END (*WITH*);

    END FindUser;

(********************************************************************************)

PROCEDURE PasswordOK (SS: ClientFileInfo;  VAR (*IN*) pass: ARRAY OF CHAR): BOOLEAN;

    (* Tests for a password match. *)

    BEGIN
        IF SS^.AnonUser THEN
            Strings.Assign (pass, SS^.Log.UserName);
        END (*IF*);
        RETURN PasswordAcceptable (SS^.user, pass);
    END PasswordOK;

(********************************************************************************)

PROCEDURE CloseUser (SS: ClientFileInfo);

    (* Returns the user to a "not logged in" state. *)

    BEGIN
        IF SS <> NIL THEN
            DiscardFName (SS^.iname);
            DestroyUserData (SS^.user);
        END(*IF*);
    END CloseUser;

(********************************************************************************)

PROCEDURE KillDataChannel (SS: ClientFileInfo);

    (* Aborts the data transfer, if any, now in progress for this session. *)

    BEGIN
        IF SS <> NIL THEN
            WITH SS^.ClientPort DO
                IF socket <> NotASocket THEN
                    so_cancel (socket);
                    socket := NotASocket;
                END (*IF*);
            END (*WITH*);
        END (*IF*);
    END KillDataChannel;

(************************************************************************)
(*                 CLOSING A SOCKET, WITH ERROR CHECK                   *)
(************************************************************************)

PROCEDURE CloseSocket (S: Socket;  LogID: TransactionLogID);

    (* Closes socket S, logs an error message if the close fails. *)

    VAR code: CARDINAL;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        IF soclose(S) THEN
            code := sock_errno();
            Strings.Assign ("soclose failed, error code ", message);
            AppendCard (code, message);
            LogTransaction (LogID, message);
        ELSIF TraceOpenSockets THEN
            Strings.Assign ("Closed socket ", message);
            AppendCard (S, message);
            LogTransaction (LogID, message);
        END (*IF*);
    END CloseSocket;

(********************************************************************************)
(*                            SENDING A FILE                                    *)
(********************************************************************************)

PROCEDURE SendFile1 (S: Socket;  filename, prefix: ARRAY OF CHAR;
                            UserNumber: CARDINAL;  SS: ClientFileInfo;
                            VAR (*INOUT*) PosInLine: CARDINAL;
                            RelativeInclude: BOOLEAN;  nesting: CARDINAL);
                                                                          FORWARD;

(********************************************************************************)

PROCEDURE SendAMessage (S: Socket;  cid: ChanId;  prefix: ARRAY OF CHAR;
                                       UserNumber: CARDINAL;  SS: ClientFileInfo;
                                       VAR (*INOUT*) PosInLine: CARDINAL;
                                       RelativeInclude: BOOLEAN;  nesting: CARDINAL);

    (* Sends the contents of a text file (which the caller must have already    *)
    (* opened) to socket S.  Each line has "prefix-" prepended to it, and there *)
    (* is also some macro expansion of % codes in the file.                     *)

    (* PosInLine counts how many characters we are up to in the current line,   *)
    (* on both entry and exit.                                                  *)

    (* RelativeInclude governs the interpretation of %i arguments.              *)
    (* The nesting parameter gives the permissible level of %i nesting.         *)

    CONST LineSize = 79;

    VAR buffer: ARRAY [0..LineSize+10] OF CHAR;  pos: CARDINAL;

    (****************************************************************************)

    PROCEDURE AppendNumber (N: CARDINAL;  VAR (*OUT*) NoOfChars: CARDINAL);

        (* Appends the string value of N to the buffer, with MAX(CARDINAL)      *)
        (* translated to 'unlimited'.  On return NoOfChars is set to the number *)
        (* of characters in what was appended.                                  *)

        BEGIN
            IF N = MAX(CARDINAL) THEN
                AppendString ("unlimited", buffer, pos);
                NoOfChars := 9;
            ELSE
                NoOfChars := pos;
                ConvertCard (N, buffer, pos);
                NoOfChars := pos - NoOfChars;
            END (*IF*);
        END AppendNumber;

    (****************************************************************************)

    PROCEDURE ReadChar (VAR (*OUT*) ch: CHAR): BOOLEAN;

        VAR amount: CARDINAL;

        BEGIN
            ReadRaw (cid, ch, SIZE(ch), amount);
            RETURN (amount > 0);
        END ReadChar;

    (****************************************************************************)

    PROCEDURE ReadDelimitedString (VAR (*OUT*) filename: ARRAY OF CHAR): BOOLEAN;

        (* Reads a string surrounded by delimiters, i.e. the first character    *)
        (* is discarded and we continue until we get the same character (which  *)
        (* is also discarded) or end-of-line.                                   *)

        TYPE CharSet = SET OF CHAR;

        VAR success: BOOLEAN;  ch: CHAR;  j: CARDINAL;  stoppers: CharSet;

        BEGIN
            stoppers := CharSet {CR, LF, CtrlZ};
            success := ReadChar(ch);
            IF success THEN
                INCL (stoppers, ch);  j := 0;
                LOOP
                    success := ReadChar(ch);
                    IF NOT success OR (ch IN stoppers) THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                    IF j <= HIGH(filename) THEN
                        filename[j] := ch;  INC(j);
                    END (*IF*);
                END (*LOOP*);
                IF j <= HIGH(filename) THEN
                    filename[j] := NUL;
                END (*IF*);
            END (*IF*);
            RETURN success;
        END ReadDelimitedString;

    (****************************************************************************)

    VAR ch: CHAR;  continue, OptionFlag: BOOLEAN;
        localtime: SysClock.DateTime;
        CommandSocket: Socket;
        peer: SockAddr;
        GroupUserNumber, GroupLimit, ClientIP: CARDINAL;
        j, count, CheckPoint: CARDINAL;
        filename: FileNameString;
        iname: FName;
        tmpbuffer: HostName;

    BEGIN
        IF SS = NIL THEN
            GroupUserNumber := UserNumber;
            Obtain (MaxUsersLock);
            GroupLimit := MaxUsers;
            Release (MaxUsersLock);
            CommandSocket := S;
            count := SIZE(peer);
            IF getpeername (S, peer, count) THEN
                ClientIP := 0;
            ELSE
                ClientIP := peer.in_addr.addr;
            END (*IF*);
        ELSE
            GetUserNumber (SS^.user, GroupUserNumber, GroupLimit);
            CommandSocket := SS^.CommandSocket;
            ClientIP := SS^.Log.ClientIP;
        END (*IF*);

        OptionFlag := FALSE;  ch := NUL;  pos := 0;  count := 0;
        REPEAT
            continue := ReadChar (ch);
            IF continue THEN
                CheckPoint := PosInLine;
                IF (PosInLine = 0) AND (ch <> CtrlZ) THEN
                    AppendString (prefix, buffer, pos);
                    buffer[pos] := '-';  INC(pos);
                    INC (PosInLine, LENGTH(prefix) + 1);
                END (*IF*);
                IF ch = LF THEN
                    buffer[pos] := NUL;  pos := AddEOL (buffer);
                    continue := WaitForDataSocket (TRUE, S, CommandSocket)
                                  AND (send (S, buffer, pos, 0) <> MAX(CARDINAL));
                    pos := 0;
                    PosInLine := 0;
                ELSIF ch = CtrlZ THEN
                    continue := FALSE;
                ELSIF ch <> CR THEN
                    IF OptionFlag THEN
                        CASE ch OF
                          | 'a': IPToString (ClientIP, TRUE, tmpbuffer);
                                 AppendString (tmpbuffer, buffer, pos);
                                 INC (PosInLine, LENGTH(tmpbuffer));
                          | 'A': AddressToHostName (ClientIP, tmpbuffer);
                                 AppendString (tmpbuffer, buffer, pos);
                                 INC (PosInLine, LENGTH(tmpbuffer));
                          | 'i': continue := ReadDelimitedString(filename);
                                 IF continue AND RelativeInclude THEN
                                     (* RelativeInclude = TRUE implies that  *)
                                     (* SS is not NIL.                       *)
                                     iname := MakeFName (SS^.user, filename);
                                     IF CanReadFile (SS^.user, iname, SS^.IsManager) THEN
                                         MakeFullName (iname, filename);
                                     ELSE
                                         continue := FALSE;
                                     END (*IF*);
                                 END (*IF*);
                                 IF continue THEN
                                     IF (pos > 0) AND WaitForDataSocket (TRUE, S, CommandSocket) THEN
                                         EVAL (send (S, buffer, pos, 0));
                                         pos := 0;
                                     END (*IF*);
                                     IF nesting > 0 THEN
                                         SendFile1 (S, filename, prefix,
                                                    UserNumber, SS, PosInLine,
                                                    RelativeInclude, nesting-1);
                                     END (*IF*);
                                 END (*IF*);
                          | 'm': AppendNumber (GroupLimit, count);
                                 INC (PosInLine, count);
                          | 'M': Obtain (MaxUsersLock);
                                 AppendNumber (MaxUsers, count);
                                 Release (MaxUsersLock);
                                 INC (PosInLine, count);
                          | 't','T':
                                 SysClock.GetClock (localtime);
                                 ConvertCardZ (localtime.hour, buffer, 2, pos);
                                 buffer[pos] := ':';  INC(pos);
                                 ConvertCardZ (localtime.minute, buffer, 2, pos);
                                 INC (PosInLine, 5);
                          | 'u': AppendNumber (GroupUserNumber, count);
                                 INC (PosInLine, count);
                          | 'U': AppendNumber (UserNumber, count);
                                 INC (PosInLine, count);
                          | 'v': AppendString (version, buffer, pos);
                                 INC (PosInLine, LENGTH(version));
                          | '%': buffer[pos] := '%';  INC(pos);
                                 INC (PosInLine);
                          | ELSE
                                 buffer[pos] := '%';  INC(pos);
                                 buffer[pos] := ch;  INC(pos);
                                 INC (PosInLine, 2);
                        END (*CASE*);
                        OptionFlag := FALSE;
                    ELSIF ch = '%' THEN
                        OptionFlag := TRUE;
                    ELSE
                        buffer[pos] := ch;  INC(pos);
                        INC (PosInLine);
                    END (*IF*);
                END (*IF*);

                (* Handle special case of line overflow. *)

                IF PosInLine > LineSize THEN
                    tmpbuffer[0] := buffer[CheckPoint];  buffer[CheckPoint] := CR;
                    tmpbuffer[1] := buffer[CheckPoint+1];  buffer[CheckPoint+1] := LF;
                    continue := WaitForDataSocket (TRUE, S, CommandSocket)
                                  AND (send (S, buffer, CheckPoint+2, 0) <> MAX(CARDINAL));
                    buffer[CheckPoint] := tmpbuffer[0];
                    buffer[CheckPoint+1] := tmpbuffer[1];
                    count := pos - CheckPoint;
                    pos := 0;
                    AppendString (prefix, buffer, pos);
                    buffer[pos] := '-';  INC(pos);
                    FOR j := CheckPoint TO CheckPoint+count-1 DO
                        buffer[pos] := buffer[j];  INC(pos);
                    END (*IF*);
                    PosInLine := count + LENGTH(prefix);
                END (*IF*);
            END (*IF*);
        UNTIL NOT continue;

        (* Send out anything still left in the output buffer. *)

        IF pos > 0 THEN
            INC (PosInLine, pos);
            IF WaitForDataSocket (TRUE, S, CommandSocket) THEN
                EVAL (send (S, buffer, pos, 0));
            END (*IF*);
        END (*IF*);

    END SendAMessage;

(********************************************************************************)

PROCEDURE SendFile1 (S: Socket;  filename, prefix: ARRAY OF CHAR;
                                   UserNumber: CARDINAL;  SS: ClientFileInfo;
                                   VAR (*INOUT*) PosInLine: CARDINAL;
                                   RelativeInclude: BOOLEAN;  nesting: CARDINAL);

    (* Sends the contents of a text file to socket S.  Each line has "prefix-"  *)
    (* prepended to it, and there is also some macro expansion of % macro codes *)
    (* in the file.  If the file can't be opened, nothing is sent.              *)

    (* PosInLine counts how many characters we are up to in the current line,   *)
    (* on both entry and exit.                                                  *)

    (* RelativeInclude governs the interpretation of %i arguments.              *)
    (* The nesting parameter gives the permissible level of %i nesting.         *)

    VAR cid: ChanId;

    BEGIN
        cid := OpenOldFile (filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            SendAMessage (S, cid, prefix, UserNumber, SS, PosInLine,
                                          RelativeInclude, nesting);
            CloseFile (cid);
        END (*IF*);
    END SendFile1;

(********************************************************************************)

PROCEDURE SendMessageFile (S: Socket;  filename, prefix: ARRAY OF CHAR;
                                       UserNumber: CARDINAL;  SS: ClientFileInfo;
                                       RelativeInclude: BOOLEAN);

    (* Sends the contents of a text file to socket S.  Each line has "prefix-"  *)
    (* prepended to it, and there is also some macro expansion of % macro codes *)
    (* in the file.  If the file can't be opened, nothing is sent.              *)

    (* RelativeInclude = TRUE means that filename arguments in a %i macro must  *)
    (* be specified in the client's view of the file system, and the client     *)
    (* must have read permission for the file.                                  *)

    VAR CRLF: ARRAY [0..1] OF CHAR;
        PosInLine: CARDINAL;
        CommandSocket: Socket;

    BEGIN
        PosInLine := 0;
        SendFile1 (S, filename, prefix, UserNumber, SS,
                              PosInLine, RelativeInclude, 5);
        IF PosInLine <> 0 THEN
            IF SS = NIL THEN
                CommandSocket := S;
            ELSE
                CommandSocket := SS^.CommandSocket;
            END (*IF*);
            CRLF[0] := CR;  CRLF[1] := LF;
            IF WaitForDataSocket (TRUE, S, CommandSocket) THEN
                EVAL (send (S, CRLF, 2, 0));
            END (*IF*);
        END (*IF*);
    END SendMessageFile;

(********************************************************************************)

PROCEDURE SendDirectoryMessage (SS: ClientFileInfo;  prefix: ARRAY OF CHAR);

    (* Sends the user a copy of Dir.MSG, if it exists in the user's     *)
    (* current directory and the user hasn't already seen it.           *)

    VAR msgname: FName;  filename: FileNameString;

    BEGIN
        IF NOT HaveSeenMessage (SS^.user) THEN
            msgname := MakeFName (SS^.user, "DIR.MSG");
            MakeFullName (msgname, filename);
            DiscardFName (msgname);
            SendMessageFile (SS^.CommandSocket, filename, prefix,
                                             SS^.UserNum, SS, TRUE);
        END (*IF*);
    END SendDirectoryMessage;

(********************************************************************************)
(*                           FILE TRANSFER SETUP                                *)
(********************************************************************************)

PROCEDURE SetTransferType (SS: ClientFileInfo;  option: CHAR);

    (* Sets transfer type to 'A'=ASCII or 'I'=Image or 'U'=UNICODE.  *)

    BEGIN
        SS^.LineStructured := (option = 'A') OR (option = 'U');
    END SetTransferType;

(********************************************************************************)

PROCEDURE SetTransferMode (SS: ClientFileInfo;  option: CHAR);

    (* Sets transfer mode to 'S'=stream or (others not implemented). *)

    BEGIN
        SS^.StreamMode := (option = 'S');
    END SetTransferMode;

(********************************************************************************)

PROCEDURE SetFileStructure (SS: ClientFileInfo;  option: CHAR);

    (* Sets file structure to 'F'=file or 'R'=record.  In this implementation   *)
    (* these are treated identically.  Page structure is not implemented.       *)

    BEGIN
        SS^.FileStructure := option;
    END SetFileStructure;

(********************************************************************************)

PROCEDURE UnacceptablePort (SS: ClientFileInfo;  IPaddr, port: CARDINAL): BOOLEAN;

    (* Returns TRUE if the port number is too low, or the user is a guest  *)
    (* user attempting to do a three-cornered transfer.  The IPaddr and    *)
    (* port arguments are in network byte order.                           *)

    BEGIN
        RETURN (Swap2(port) < 1024)
                   OR (SS^.AnonUser AND (IPaddr <> SS^.ClientPort.host));
    END UnacceptablePort;

(********************************************************************************)

PROCEDURE SetPort (SS: ClientFileInfo;  IPaddr: CARDINAL;  port: CARDINAL);

    (* Sets the client address for future data transfers. *)

    BEGIN
        WITH SS^ DO
            ClientPort.host := IPaddr;
            ClientPort.port := port;
        END (*WITH*);
    END SetPort;

(********************************************************************************)

PROCEDURE SetRestartPoint (SS: ClientFileInfo;  marker: ARRAY OF CHAR);

    (* Sets a restart marker for a subsequent RETR or STOR operation.  *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        SS^.RestartPoint := ConvertDecimal64 (marker, j);
        SS^.RestartPointSet := TRUE;
    END SetRestartPoint;

(********************************************************************************)

PROCEDURE AllocateFileSize (SS: ClientFileInfo;  amount: ARRAY OF CHAR): BOOLEAN;

    (* Sets the file size for a subsequent STOR or APPE.  Returns FALSE if      *)
    (* the requested file size is too big.                                      *)

    VAR j: CARDINAL;
        size: CARD64;

    BEGIN
        j := 0;
        size := ConvertDecimal64 (amount, j);
        IF Compare64 (size, SS^.UpSizeLimit) > 0 THEN
            RETURN FALSE;
        ELSE
            SS^.AmountToAllocate := size;
            RETURN TRUE;
        END (*IF*);
    END AllocateFileSize;

(********************************************************************************)

PROCEDURE IncPort (VAR (*INOUT*) port: ARRAY OF LOC): BOOLEAN;

    (* Increments a 16-bit port number that is in network byte order.  Returns  *)
    (* FALSE if this would take us beyond PASVPorts.MaxDataPort.                *)
    (* Assumption: the calling thread has exclusive access to PASVPorts.        *)

    VAR portval, limit: CARD16;

    BEGIN
        portval := 256*CAST(CARD8,port[0]) + CAST(CARD8,port[1]);
        WITH PASVPorts DO
            limit := 256*(MaxDataPort MOD 256) + (MaxDataPort DIV 256);
        END (*WITH*);
        IF portval >= limit THEN
            RETURN FALSE;
        END (*IF*);
        INC (portval);
        port[0] := CAST(LOC, portval DIV 256);
        port[1] := CAST(LOC, portval MOD 256);
        RETURN TRUE;
    END IncPort;

(********************************************************************************)

PROCEDURE EnterPassiveMode (SS: ClientFileInfo;  VAR (*OUT*) myaddr: SockAddr): BOOLEAN;

    (* Creates a data socket, binds it to a local port, and then listens.       *)
    (* On return myaddr holds the port details.                                 *)

    VAR s: Socket;  size: CARDINAL;  failure: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        CloseDataPort (SS);
        s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
        SS^.ClientPort.socket := s;
        size := SIZE(myaddr);
        myaddr := SS^.ServerName;

        IF TraceOpenSockets THEN
            Strings.Assign ("Opening data socket ", message);
            AppendCard (s, message);
            LogTransaction (SS^.TransLogID, message);
        END (*IF*);

        (* Choose a port number. *)

        WITH PASVPorts DO
            Obtain (access);
            IF restrict THEN
                myaddr.in_addr.port := MinDataPort;
                REPEAT
                    failure := bind (s, myaddr, size);
                UNTIL NOT failure OR NOT IncPort(myaddr.in_addr.port);
                failure := failure OR listen (s, 1);
            ELSE
                myaddr.in_addr.port := INADDR_ANY;
                failure := bind (s, myaddr, size) OR listen (s, 1)
                                         OR getsockname (s, myaddr, size);
            END (*IF*);
            Release (access);
        END (*WITH*);

        (* In the "AllowForFirewall" case, we also need to modify       *)
        (* our local address.                                           *)

        IF SS^.AllowForFirewall THEN
            WITH FWParams DO
                Obtain (access);
                myaddr.in_addr.addr := FirewallIPAddr;
                Release (access);
            END (*WITH*);
        END (*IF*);

        WITH SS^ DO
            PassiveOperation := NOT failure;
            IF PassiveOperation THEN
                RETURN TRUE;
            ELSE
                WITH ClientPort DO
                    CloseSocket (socket, SS^.TransLogID);
                    socket := NotASocket;
                END (*WITH*);
                RETURN FALSE;
            END (*IF*);
        END (*WITH*);

    END EnterPassiveMode;

(********************************************************************************)

PROCEDURE OpenDataPort (SS: ClientFileInfo): BOOLEAN;

    CONST SO_LINGER = 0080H;          (* linger on close if data present *)

    VAR s, ns: Socket;  peer: SockAddr;  option, size: CARDINAL;
        (*LingerData: ARRAY [0..1] OF CARDINAL;*)
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        IF SS^.PassiveOperation THEN

            (* Phase 2: we have our port, now accept a connection. *)

            s := SS^.ClientPort.socket;
            IF WaitForDataSocket (FALSE, s, SS^.CommandSocket) THEN
                size := SIZE(peer);
                ns := accept (s, peer, size);
            ELSE
                ns := NotASocket;
            END (*IF*);
            SS^.PassiveOperation := FALSE;
            IF ns = NotASocket THEN
                RETURN FALSE;
            ELSE
                SS^.ClientPort.socket := ns;
                CloseSocket (s, SS^.TransLogID);
                s := ns;
            END (*IF*);

        ELSE

            (* Normal, non-passive operation. *)

            CloseDataPort (SS);
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
            SS^.ClientPort.socket := s;

            (* Bind to the data port at our end, allowing reuse of the port     *)
            (* we're binding to.  No error check is needed; if the bind fails,  *)
            (* we can carry on without it.                                      *)

            option := MAX(CARDINAL);
            setsockopt (s, 0FFFFH, 4, option, SIZE(CARDINAL));
            bind (s, SS^.ServerName, SIZE(SockAddr));

            (* Socket open, connect to the client. *)

            WITH peer DO
                family := AF_INET;
                WITH in_addr DO
                    port := SS^.ClientPort.port;
                    addr := SS^.ClientPort.host;
                    zero := Zero8;
                END (*WITH*);
            END (*WITH*);

            IF connect (s, peer, SIZE(peer)) THEN

                (* Connection failed. *)

                RETURN FALSE;

            END (*IF*);

        END (*IF PassiveOperation*);

        (* "Linger on close" is disabled for now, because there is at least     *)
        (* one version of the TCP/IP stack that can't handle it.                *)

        (*
        LingerData[0] := 1;  LingerData[1] := MAX(CARDINAL);
        IF setsockopt (s, 0FFFFH, SO_LINGER, LingerData, SIZE(LingerData)) THEN
            WriteError;
        END (*IF*);
        *)

        IF TraceOpenSockets THEN
            Strings.Assign ("Opening data socket ", message);
            AppendCard (s, message);
            LogTransaction (SS^.TransLogID, message);
        END (*IF*);

        RETURN TRUE;

    END OpenDataPort;

(********************************************************************************)

PROCEDURE CloseDataPort (SS: ClientFileInfo);

    (* Closes the data port used by this client, if it was still open. *)

    BEGIN
        WITH SS^.ClientPort DO
            IF socket <> NotASocket THEN
                CloseSocket (socket, SS^.TransLogID);
                socket := NotASocket;
            END (*IF*);
        END (*WITH*);
    END CloseDataPort;

(********************************************************************************)
(*                          FILE TRANSFER OPERATIONS                            *)
(********************************************************************************)

PROCEDURE AmountTransferred (SS: ClientFileInfo;  VAR (*OUT*) amount, total: CARD64);

    (* Reports back the number of bytes transferred, and the total number       *)
    (* of bytes in the file, for the current transfer.                          *)

    BEGIN
        WITH SS^ DO
            Obtain (Count.access);
            amount := Count.amount;
            total := Count.total;
            Release (Count.access);
        END (*WITH*);
    END AmountTransferred;

(********************************************************************************)

PROCEDURE ResetCount (SS: ClientFileInfo);

    (* Set the byte count back to zero. *)

    BEGIN
        WITH SS^.Count DO
            Obtain (access);
            amount := Zero64;  total := Zero64;
            Release (access);
        END (*WITH*);
    END ResetCount;

(********************************************************************************)

PROCEDURE SendAscii (CommandSocket, S: Socket;  VAR (*IN*) source: ARRAY OF LOC;
                                                   amount: CARDINAL): CARDINAL;

    (* Sends "amount" bytes of data from source to S.  The value returned is    *)
    (* the actual number of characters sent; this could be different from       *)
    (* amount because of things like conversion of line terminators.  If the    *)
    (* transfer fails, we return a result of MAX(CARDINAL).                     *)

    VAR j, k, count, N: CARDINAL;  ch: CHAR;  AtEOL, AtEOF: BOOLEAN;
        CRLF: ARRAY[0..1] OF CHAR;

    BEGIN
        CRLF[0] := CR;  CRLF[1] := LF;
        k := 0;  count := 0;  AtEOL := FALSE;  AtEOF := FALSE;
        REPEAT
            (* Skip past any carriage returns. *)

            WHILE (k < amount) AND (CAST(CHAR,source[k]) = CR) DO
                INC (k);
            END (*WHILE*);

            j := k;

            (* Scan to next CR or LF or end-of-file. *)

            LOOP
                IF k >= amount THEN
                    AtEOF := TRUE;  EXIT(*LOOP*);
                END(*IF*);
                ch := CAST (CHAR, source[k]);
                IF ch = LF THEN
                    AtEOL := TRUE;  EXIT;
                ELSIF ch = CtrlZ THEN
                    AtEOF := TRUE;  EXIT;
                ELSIF ch = CR THEN
                    EXIT;
                END (*IF*);
                INC (k);
            END (*LOOP*);

            (* Send the data from position j to position k-1. *)

            IF k > j THEN
                IF WaitForDataSocket (TRUE, S, CommandSocket) THEN
                    N := send (S, source[j], k-j, 0);
                ELSE
                    N := MAX(CARDINAL);
                END (*IF*);
                IF N = MAX(CARDINAL) THEN
                    count := MAX(CARDINAL);
                    AtEOF := TRUE;  AtEOL := FALSE;
                ELSE
                    INC (count, N);
                END (*IF*);
            END (*IF*);

            (* Send the end-of-line code, if needed. *)

            IF AtEOL THEN
                AtEOF := (NOT WaitForDataSocket (TRUE, S, CommandSocket))
                           OR (send (S, CRLF, 2, 0) = MAX(CARDINAL));
                IF AtEOF THEN
                    count := MAX(CARDINAL);
                ELSE
                    AtEOL := FALSE;
                    INC (k);  INC (count, 2);
                END (*IF*);
            END (*IF*);

        UNTIL AtEOF;

        RETURN count;

    END SendAscii;

(********************************************************************************)

PROCEDURE PutFile (SS: ClientFileInfo;  id: ChanId;
                   RateLimit: REAL;
                   VAR (*OUT*) BytesSent: CARD64): BOOLEAN;

    (* Sends a file on a previously opened socket SS^.DataPort.socket. *)

    CONST BufferSize = 2048;

    VAR BuffPtr: POINTER TO ARRAY [0..BufferSize-1] OF LOC;
        xferred: CARDINAL;  success: BOOLEAN;
        starttime, duration: REAL;
        S: Socket;

    BEGIN
        S := SS^.ClientPort.socket;
        NEW (BuffPtr);
        BytesSent := Zero64;  success := TRUE;
        starttime := FLOAT(millisecs());
        LOOP
            ReadRaw (id, BuffPtr^, BufferSize, xferred);
            IF xferred = 0 THEN
                EXIT(*LOOP*)
            END (*IF*);
            Signal (SS^.KickMe);
            IF SS^.LineStructured THEN
                xferred := SendAscii (SS^.CommandSocket, S, BuffPtr^, xferred);
            ELSIF WaitForDataSocket (TRUE, S, SS^.CommandSocket) THEN
                xferred := send (S, BuffPtr^, xferred, 0);
            ELSE
                xferred := MAX(CARDINAL);
            END (*IF*);
            IF xferred = MAX(CARDINAL) THEN
                success := FALSE;
            ELSE
                Add64 (BytesSent, xferred);
            END (*IF*);
            IF NOT success THEN
                EXIT(*LOOP*)
            END (*IF*);
            WITH SS^.Count DO
                Obtain (access);
                Add64 (amount, xferred);
                Release (access);
            END (*WITH*);

            (* If the transfer is too fast, insert a time delay. *)
            (* RateLimit is in bytes per millisecond.            *)

            duration := FLOAT(millisecs()) - starttime;
            WHILE duration < 0.0 DO
                duration := duration + MillisecondsPerDay;
            END (*WHILE*);
            IF FLOAT64(BytesSent) > RateLimit * duration THEN
                DosSleep (TRUNC(FLOAT64(BytesSent)/RateLimit - duration));
            END (*IF*);

        END (*LOOP*);
        DISPOSE (BuffPtr);
        RETURN success;
    END PutFile;

(********************************************************************************)

PROCEDURE GetFile (SS: ClientFileInfo;  CommandSocket, S: Socket;  id: ChanId;
                   RateLimit: REAL;  KeepAlive: Semaphore;
                   VAR (*OUT*) TooBig: BOOLEAN;  VAR (*OUT*) totalbytes: CARD64): BOOLEAN;

    (* Retrieves a file on a previously opened socket.  Returns TRUE iff the    *)
    (* transfer was successful.  Sets TooBig=TRUE and aborts the operation if   *)
    (* we are going to go over the user's file size limit.                      *)

    CONST BufferSize = 2048;

    VAR BuffPtr: POINTER TO ARRAY [0..BufferSize-1] OF LOC;
        xferred: CARDINAL;  success: BOOLEAN;
        starttime, duration: REAL;

    BEGIN
        NEW (BuffPtr);
        totalbytes := Zero64;
        success := TRUE;
        TooBig := FALSE;
        starttime := FLOAT(millisecs());
        LOOP
            Signal (KeepAlive);
            IF WaitForDataSocket (FALSE, S, CommandSocket) THEN
                xferred := recv (S, BuffPtr^, BufferSize, 0);
            ELSE
                xferred := MAX(CARDINAL);
            END (*IF*);
            IF xferred = 0 THEN EXIT(*LOOP*)
            ELSIF xferred = MAX(CARDINAL) THEN
                success := FALSE;
                EXIT(*LOOP*);
            END(*IF*);
            IF SpaceAvailable(SS^.iname) <= FreeSpaceThreshold THEN
                success := FALSE;
                EXIT (*LOOP*);
            END (*IF*);
            Signal (KeepAlive);
            Add64 (totalbytes, xferred);

            IF Compare64 (totalbytes, SS^.UpSizeLimit) > 0 THEN
                TooBig := TRUE;
                success := FALSE;
                EXIT (*LOOP*);
            END (*IF*);

            WITH SS^.Count DO
                Obtain (access);
                Add64 (amount, xferred);
                Release (access);
            END (*WITH*);
            WriteRaw (id, BuffPtr^, xferred);

            (* If the transfer is too fast, insert a time delay. *)
            (* RateLimit is in bytes per millisecond.            *)

            duration := FLOAT(millisecs()) - starttime;
            WHILE duration < 0.0 DO
                duration := duration + MillisecondsPerDay;
            END (*WHILE*);
            IF FLOAT64(totalbytes) > RateLimit * duration THEN
                DosSleep (TRUNC(FLOAT64(totalbytes)/RateLimit - duration));
            END (*IF*);

        END (*LOOP*);
        DISPOSE (BuffPtr);
        RETURN success;
    END GetFile;

(********************************************************************************)

PROCEDURE SendDirectory (SS: ClientFileInfo;  UseControlConnection: BOOLEAN;
                                              ShowAllDetails: BOOLEAN): BOOLEAN;

    (* Sends a directory listing as specified by the last SetFileName call.     *)
    (* Sends it via the control socket if UseControlConnection is TRUE, and via *)
    (* the data connection otherwise.                                           *)
    (* Returns TRUE iff the transfer was successful.                            *)
    (* Returned file names are in UTF-8 encoding.                               *)

    BEGIN
        IF UseControlConnection THEN
            WITH SS^ DO
                IF ShowAllDetails THEN
                    flags := flags + ListingOptions{ListDotDot, ShowDetails};
                END (*IF*);
                IF IsManager THEN INCL(flags, SystemAndHidden) END (*IF*);
                ListDirectory (CommandSocket, SS^.user, iname, flags);
            END (*WITH*);
            RETURN TRUE;
        ELSIF OpenDataPort (SS) THEN
            WITH SS^ DO
                IF ShowAllDetails THEN
                    flags := flags + ListingOptions{ListDotDot, ShowDetails};
                END (*IF*);
                IF IsManager THEN INCL(flags, SystemAndHidden) END (*IF*);
                ListDirectory (ClientPort.socket, SS^.user, iname, flags);
                Synch (ClientPort.socket);
            END (*WITH*);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END SendDirectory;

(********************************************************************************)

PROCEDURE SendFile (SS: ClientFileInfo): BOOLEAN;

    (* Sends the file whose name was specified by the last SetFileName call to  *)
    (* the client.  Returns TRUE for a successful send.                         *)

    VAR cid: ChanId;  count: CARD64;  starttime: REAL;
        success: BOOLEAN;

    BEGIN
        success := FALSE;
        IF OpenDataPort (SS) THEN
            count := GetFileSize (SS^.iname, TRUE);
            IF Compare64 (count, Max64) = 0 THEN
                CloseDataPort (SS);
            ELSIF Compare64 (SS^.RestartPoint, count) >= 0 THEN
                SS^.RestartPoint := Zero64;
                SS^.RestartPointSet := FALSE;
                success := TRUE;
                CloseDataPort (SS);
                AddToLog (SS, download, Zero64, 0.0);
            ELSIF OpenForReading (cid, SS^.iname) THEN
                WITH SS^.Count DO
                    Obtain (access);
                    amount := SS^.RestartPoint;
                    total := count;
                    Release (access);
                END (*WITH*);
                SetPosition (cid, Sum64(StartPosition(cid), SS^.RestartPoint));
                SS^.RestartPoint := Zero64;
                SS^.RestartPointSet := FALSE;
                starttime := FLOAT(millisecs());
                success := PutFile (SS, cid, 0.001*FLOAT(SS^.SpeedLimit), count);
                CloseFile (cid);
                CloseDataPort (SS);
                IF success THEN
                    AddToLog (SS, download, count, FLOAT(millisecs()) - starttime);
                ELSIF LogLevel > 1 THEN
                    AddToLog (SS, partdownload, count, FLOAT(millisecs()) - starttime);
                END (*IF*);
            ELSE
                CloseDataPort (SS);
            END (*IF*);
        END (*IF*);
        DiscardFName (SS^.iname);
        RETURN success;
    END SendFile;

(********************************************************************************)

PROCEDURE AllNumeric (name: FileNameString): BOOLEAN;

    (* Returns TRUE iff 'name' contains only digits or periods.  *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > 255) OR (name[j] = NUL) THEN RETURN TRUE
            ELSIF NOT ((name[j] = '.') OR (name[j] IN Digits)) THEN RETURN FALSE
            ELSE INC(j);
            END (*IF*);
        END (*LOOP*);
    END AllNumeric;

(********************************************************************************)

PROCEDURE AcceptFile (SS: ClientFileInfo;  VAR (*OUT*) SizeTooBig: BOOLEAN): BOOLEAN;

    (* Reads the file whose name was specified by the last SetFileName call     *)
    (* from the client.  Returns TRUE for a successful transfer.                *)
    (* Convention (and I hope I get it right this time): if file already exists *)
    (* then we first delete the old file iff the restart point has not been     *)
    (* explicitly set.  In this case the caller will have already have checked  *)
    (* that the user has delete permission for the file.                        *)

    VAR cid: ChanId;  count: CARD64;  starttime: REAL;
        RelName: FileNameString;  pos: CARDINAL;  success: BOOLEAN;

    BEGIN
        SizeTooBig := FALSE;
        IF DoTagCheck AND NOT SS^.Log.tagged THEN

            (* Check for pirates.  We set the 'tagged' flag if the      *)
            (* uploaded file name is 'space.asp' or '*test.ptf', or if  *)
            (* it is purely numeric (where we count a '.' as a digit).  *)

            MakeShortName (SS^.iname, RelName);
            Strings.Capitalize (RelName);
            SS^.Log.tagged := Strings.Equal (RelName, 'SPACE.ASP')
                                  OR AllNumeric (RelName);
            IF NOT SS^.Log.tagged THEN
                Strings.FindNext ('TEST.PTF', RelName, 0, SS^.Log.tagged, pos);
            END (*IF*);
            IF SS^.Log.tagged THEN
                SS^.UpSpeedLimit := 32;
            END (*IF*);
        END (*IF*);

        (* Now for the actual transfer. *)

        IF OpenDataPort (SS) THEN
            IF NOT SS^.RestartPointSet THEN
                EVAL(RemoveFile (SS^.iname));
            END (*IF*);
            SS^.RestartPointSet := FALSE;
            IF OpenForWriting (cid, SS^.iname) THEN
                WITH SS^.Count DO
                    Obtain (access);
                    amount := SS^.RestartPoint;
                    total := SS^.AmountToAllocate;
                    Release (access);
                END (*WITH*);
                IF Compare64 (SS^.AmountToAllocate, Zero64) <> 0 THEN
                    SetFileSize (cid, SS^.AmountToAllocate);
                END (*IF*);
                SS^.AmountToAllocate := Zero64;
                SetPosition (cid, Sum64(StartPosition(cid), SS^.RestartPoint));
                SS^.RestartPoint := Zero64;
                starttime := FLOAT(millisecs());
                success := GetFile (SS, SS^.CommandSocket, SS^.ClientPort.socket, cid,
                                    0.001*FLOAT(SS^.UpSpeedLimit), SS^.KickMe,
                                    SizeTooBig, count);
                CloseFile (cid);

                (* In the "too big" case, we have to close and delete the   *)
                (* file.  This is different from the other failure cases,   *)
                (* where we allow a partially uploaded file in order to     *)
                (* allow for the possibility of a restart.                  *)

                IF SizeTooBig THEN
                    EVAL (DeleteFile (SS));
                END (*IF*);

                CloseDataPort (SS);
                IF success THEN
                    AddToLog (SS, upload, count, FLOAT(millisecs()) - starttime);
                ELSIF LogLevel > 1 THEN
                    AddToLog (SS, partupload, count, FLOAT(millisecs()) - starttime);
                END (*IF*);
                DiscardFName (SS^.iname);
                RETURN success;
            ELSE
                CloseDataPort (SS);
                DiscardFName (SS^.iname);
                RETURN FALSE;
            END (*IF*);
        ELSE
            DiscardFName (SS^.iname);
            RETURN FALSE;
        END (*IF*);
    END AcceptFile;

(********************************************************************************)

PROCEDURE AppendFile (SS: ClientFileInfo;  VAR (*OUT*) SizeTooBig: BOOLEAN): BOOLEAN;

    (* Like AcceptFile, except that if the file already exists then the new     *)
    (* data are appended to the end of the file.                                *)

    VAR cid: ChanId;  count: CARD64;  starttime: REAL;
        success: BOOLEAN;

    BEGIN
        success := FALSE;
        SizeTooBig := FALSE;
        IF OpenDataPort (SS) THEN
            IF OpenForAppend (cid, SS^.iname) THEN
                IF Compare64 (SS^.AmountToAllocate, Zero64) <> 0 THEN
                    SetFileSize (cid, SS^.AmountToAllocate);
                END (*IF*);
                SS^.AmountToAllocate := Zero64;
                starttime := FLOAT(millisecs());
                success := GetFile (SS, SS^.CommandSocket, SS^.ClientPort.socket, cid,
                                    0.001*FLOAT(SS^.UpSpeedLimit), SS^.KickMe,
                                    SizeTooBig, count);
                CloseFile (cid);

                (* In the "too big" case, we have to close and delete the   *)
                (* file.  This is different from the other failure cases,   *)
                (* where we allow a partially uploaded file in order to     *)
                (* allow for the possibility of a restart.                  *)

                IF SizeTooBig THEN
                    EVAL (DeleteFile (SS));
                END (*IF*);
                CloseDataPort (SS);

                IF success THEN
                    AddToLog (SS, upload, count, FLOAT(millisecs()) - starttime);
                ELSIF LogLevel > 1 THEN
                    AddToLog (SS, partupload, count, FLOAT(millisecs()) - starttime);
                END (*IF*);
            ELSE
                CloseDataPort (SS);
            END (*IF*);
        END (*IF*);
        DiscardFName (SS^.iname);
        RETURN success;
    END AppendFile;

(********************************************************************************)

PROCEDURE DeleteFile (SS: ClientFileInfo): BOOLEAN;

    (* Deletes the file whose name was specified by the last SetFileName call   *)
    (* from the client.  Returns TRUE for a successful deletion.                *)

    VAR result: BOOLEAN;

    BEGIN
        result := RemoveFile (SS^.iname);
        IF result THEN
            AddToLog (SS, delete, Zero64, 0.0);
        END (*IF*);
        DiscardFName (SS^.iname);
        RETURN result;
    END DeleteFile;

(********************************************************************************)

PROCEDURE RenameTo (SS: ClientFileInfo;  NewName: ARRAY OF CHAR): BOOLEAN;

    (* Renames to NewName the file or directory whose name was specified by the *)
    (* last SetFileName call from the client.  Returns TRUE for success.        *)

    VAR dstName: FName;  result: BOOLEAN;

    BEGIN
        dstName := MakeFName (SS^.user, NewName);
        IF SameDrive (SS^.iname, dstName) THEN
            IF SameParent (SS^.iname, dstName) THEN
                result := RenameIsLegal (SS^.user, SS^.iname, SS^.IsManager);
            ELSIF IsADirectory (SS^.iname, SS^.IsManager) THEN
                result := CanDeleteDirectory (SS^.user, SS^.iname)
                          AND CanCreateDirectory (SS^.user, dstName);
            ELSIF IsAFile (SS^.iname, SS^.IsManager) THEN
                result := CanDeleteFile (SS^.user, SS^.iname, SS^.IsManager)
                          AND CanWriteFile (SS^.user, dstName, SS^.IsManager, TRUE);
            ELSE
                result := FALSE;
            END (*IF*);
        ELSE
            result := FALSE;
        END (*IF*);
        result := result AND Rename (SS^.iname, dstName);
        DiscardFName (dstName);
        RETURN result;
    END RenameTo;

(********************************************************************************)
(*                           DIRECTORY OPERATIONS                               *)
(********************************************************************************)

PROCEDURE TwelvePluspCheck (name: FName): BOOLEAN;

    (* Returns TRUE iff the relative name is a 12-digit string followed by      *)
    (* a lower-case 'p'.                                                        *)

    VAR RelName: FileNameString;  j: CARDINAL;

    BEGIN
        MakeShortName (name, RelName);
        IF LENGTH(RelName) <> 13 THEN
            RETURN FALSE;
        ELSIF RelName[12] <> 'p' THEN
            RETURN FALSE;
        ELSE
            j := 0;
            LOOP
                IF j = 12 THEN RETURN TRUE
                ELSIF NOT (RelName[j] IN Digits) THEN RETURN FALSE
                ELSE INC(j);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);
    END TwelvePluspCheck;

(********************************************************************************)

PROCEDURE MakeDirectory (SS: ClientFileInfo): BOOLEAN;

    (* Creates a directory as specified by the last SetFileName call.  Returns  *)
    (* TRUE for a successful operation.                                         *)

    VAR result: BOOLEAN;  dirname: FileNameString;  pos: CARDINAL;

    BEGIN
        IF DoTagCheck AND NOT SS^.Log.tagged THEN

            (* Check for pirates.  We set the 'tagged' flag if the new  *)
            (* directory name includes the substring 'tag', or if it    *)
            (* is a 12-digit numeric string followed by 'p'.            *)

            SS^.Log.tagged := TwelvePluspCheck(SS^.iname);
            IF NOT SS^.Log.tagged THEN
                MakeFullName (SS^.iname, dirname);
                Strings.Capitalize (dirname);
                Strings.FindNext ('TAG', dirname, 0, SS^.Log.tagged, pos);
            END (*IF*);
            IF SS^.Log.tagged THEN
                SS^.UpSpeedLimit := 32;
            END (*IF*);
        END (*IF*);
        result := CreateDirectory (SS^.iname);
        IF result THEN
            AddToLog (SS, makedir, Zero64, 0.0);
        END (*IF*);
        RETURN result;
    END MakeDirectory;

(********************************************************************************)

PROCEDURE DeleteDirectory (SS: ClientFileInfo): BOOLEAN;

    (* Deletes the file whose name was specified by the last SetFileName call   *)
    (* from the client.  Returns TRUE for a successful deletion.                *)

    BEGIN
        RETURN RemoveDirectory (SS^.iname);
    END DeleteDirectory;

(********************************************************************************)

PROCEDURE CurrentDirectory (SS: ClientFileInfo;  VAR (*OUT*) DirString: ARRAY OF CHAR);

    (* Gives back the name of the current directory for this user.  *)

    BEGIN
        NameOfCurrentDirectory (SS^.user, DirString);
    END CurrentDirectory;

(********************************************************************************)

PROCEDURE RealCurrentDirectory (SS: ClientFileInfo;  VAR (*OUT*) DirString: ARRAY OF CHAR);

    (* Gives back the name of the current directory for this user.  Note that   *)
    (* the present procedure returns the directory name in the host file system *)
    (* while CurrentDirectory returns a virtual directory name.                 *)

    BEGIN
        IF (SS = NIL) OR (SS^.user = CAST(User,NIL)) THEN
            DirString[0] := Nul;
        ELSE
            RealNameOfCurrentDirectory (SS^.user, DirString);
        END (*IF*);
    END RealCurrentDirectory;

(********************************************************************************)

PROCEDURE SetDirectoryL (SS: ClientFileInfo;  MessageEnabled: BOOLEAN;
                                              DirString: ARRAY OF CHAR): BOOLEAN;

    (* Changes user to the specified directory.  The pathname can be absolute   *)
    (* (starting with '/') or relative to the current directory.                *)
    (* Returns FALSE if the requested directory does not exist, or if the user  *)
    (* does not have the right to see it.                                       *)
    (* DirString uses our local character set.                                  *)

    VAR success: BOOLEAN;  iname: FName;

    BEGIN
        iname := MakeFName (SS^.user, DirString);
        success := SetWorkingDirectory (SS^.user, iname);
        DiscardFName (iname);

        (* Give the user a copy of Dir.MSG, if it exists and is wanted. *)

        IF success AND MessageEnabled THEN
            SendDirectoryMessage (SS, "250");
        END (*IF*);

        RETURN success;

    END SetDirectoryL;

(********************************************************************************)

PROCEDURE SetDirectory (SS: ClientFileInfo;  MessageEnabled: BOOLEAN;
                                              DirString: ARRAY OF CHAR): BOOLEAN;

    (* Changes user to the specified directory.  The pathname can be absolute   *)
    (* (starting with '/') or relative to the current directory.  Returns FALSE *)
    (* if the requested directory does not exist, or if the user does not       *)
    (* have the right to see it.                                                *)
    (* DirString is a UTF-8 directory name.                                     *)

    VAR LocalDirName: FilenameString;

    BEGIN
        TranslateFromUTF8 (DirString, LocalDirName);
        RETURN SetDirectoryL (SS, MessageEnabled, LocalDirName);
    END SetDirectory;

(********************************************************************************)

PROCEDURE GetCurrentPermissions (SS: ClientFileInfo;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Returns a string indicating read/write/delete permissions for the        *)
    (* user's current directory.                                                *)

    BEGIN
        PermissionString (SS^.user, result);
    END GetCurrentPermissions;

(********************************************************************************)

PROCEDURE SetFileNameL (SS: ClientFileInfo;  NameString: ARRAY OF CHAR;
                                                         InterpretFlags: BOOLEAN);

    (* Specifies the name of the file that will next be involved in a data      *)
    (* transfer for this user.  If InterpretFlags is TRUE, also detects some    *)
    (* Unix-like arguments and sets SS^.flags.                                  *)
    (* NameString uses our local character set.                                 *)

    (* The Unix-like arguments have the meanings:                               *)
    (*     F   a trailing '/' will be added to directory names.                 *)
    (*     R   recurse on subdirectories.                                       *)
    (*     a   in standard Unix 'ls', this means to include names starting with *)
    (*         a '.'.  In OS/2 the leading '.' has no special significance,     *)
    (*         so we interpret this command as meaning that the '..' should be  *)
    (*         included in the directory listing.                               *)
    (*     d   when NameString is a directory name, we list its name rather     *)
    (*         than the contents of the directory.                              *)
    (*     l   detailed listing: mode, number of links, owner, size in bytes,   *)
    (*         time of last modification.  For the number of links we have a    *)
    (*         dummy entry, and the 'owner' field is filled with the RASH code. *)
    (* These arguments are meaningful only for the LIST and NLST commands.      *)
    (* Otherwise they have no effect.                                           *)

    (* Earlier I had supported the following as well, but I've decided to       *)
    (* drop it as being irrelevant to our purposes.                             *)
    (*     A   not documented in Unix man, not sure why I added this; but       *)
    (*         apparently it was supposed to hide filenames starting with '.'.  *)

    VAR j: CARDINAL;

    BEGIN
        WITH SS^ DO

            flags := ListingOptions {MayExpand};
            IF InterpretFlags THEN
                WHILE NameString[0] = '-' DO
                    j := 1;
                    WHILE (NameString[j] <> NUL) AND (NameString[j] <> ' ') DO
                        CASE NameString[j] OF
                          |  'F':  INCL (flags, AddSlash);
                          |  'R':  INCL (flags, Recurse);
                          |  'a':  INCL (flags, ListDotDot);
                          |  'd':  EXCL (flags, MayExpand);
                          |  'l':  INCL (flags, ShowDetails);

                            (* I'm not sure whether I need to handle the following: *)
                            (*    -D   generate output suited to Emacs' dired mode  *)

                        ELSE
                            (* Ignore options that are irrelevant to us. *)
                        END (*CASE*);
                        INC (j);
                    END (*WHILE*);
                    WHILE NameString[j] = ' ' DO
                        INC (j);
                    END (*WHILE*);
                    Strings.Delete (NameString, 0, j);
                END (*WHILE*);
            END (*IF*);

            DiscardFName (iname);
            iname := MakeFName (user, NameString);

        END (*WITH*);

   END SetFileNameL;

(********************************************************************************)

PROCEDURE SetFileName (SS: ClientFileInfo;  NameString: ARRAY OF CHAR;
                                                         InterpretFlags: BOOLEAN);

    (* Specifies the name of the file that will next be involved in a data      *)
    (* transfer for this user.  If InterpretFlags is TRUE, also detects some    *)
    (* Unix-like arguments and sets SS^.flags.                                  *)
    (* NameString is a UTF-8 file name.                                         *)

    VAR LocalName: FilenameString;

    BEGIN
        TranslateFromUTF8 (NameString, LocalName);

        (* Remark: if LocalName happens to include some flag specifications,    *)
        (* that is not a problem because those are all in 7-bit ASCII so will   *)
        (* not be modified by TranslateFromUTF8.                                *)

        SetFileNameL (SS, LocalName, InterpretFlags);

    END SetFileName;

(********************************************************************************)

PROCEDURE CreateUniqueFileName (SS: ClientFileInfo;
                                 VAR (*OUT*) NameString: ARRAY OF CHAR): BOOLEAN;

    (* Like SetFileName, except that we create the name internally, making      *)
    (* sure that it's unique for the current directory.  The result is FALSE    *)
    (* if we're not able to create the unique name.                             *)

    CONST numstart = 3;

    (****************************************************************************)

    PROCEDURE increment (endpos: CARDINAL): BOOLEAN;

        (* Increments the ASCII decimal number in SS^.FileName[numstart..endpos].*)
        (* Returns FALSE if the number overflows.                                *)

        BEGIN
            IF endpos < numstart THEN
                RETURN FALSE;
            ELSIF NameString[endpos] <> '9' THEN
                INC (NameString[endpos]);
                RETURN TRUE;
            ELSE
                NameString[endpos] := '0';
                RETURN increment (endpos-1);
            END (*IF*);
        END increment;

    (****************************************************************************)

    CONST lastdigit = 7;

    VAR nameOK: BOOLEAN;

    BEGIN
        Strings.Assign ("FTP00000", NameString);
        nameOK := TRUE;
        SS^.iname := MakeFName (SS^.user, NameString);
        WHILE nameOK AND FileOrDirExists (SS^.iname, TRUE) DO
            nameOK := increment (lastdigit);
            DiscardFName (SS^.iname);
            SS^.iname := MakeFName (SS^.user, NameString);
        END (*WHILE*);
        RETURN nameOK;
    END CreateUniqueFileName;

(********************************************************************************)

PROCEDURE SetFileDateTime (SS: ClientFileInfo;  datetime: ARRAY OF CARDINAL);

    (* Sets the modified/accessed/created timestamps for the currently selected *)
    (* file, from an array of 6 numbers already encoded into the format the     *)
    (* filesystem uses for timestamps.                                          *)

    BEGIN
        SetDateTime (SS^.iname, datetime);
    END SetFileDateTime;

(********************************************************************************)

PROCEDURE GetFileDate (SS: ClientFileInfo;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Returns the date/time of the file's directory entry, in a string of the  *)
    (* form "yyyymmddhhmmss" (exactly 14 characters).  If the file is not       *)
    (* accessible, the result is the null string.                               *)

    BEGIN
        GetDateTime (SS^.iname, result);
    END GetFileDate;

(********************************************************************************)

PROCEDURE GetSize (SS: ClientFileInfo): CARD64;

    (* Returns the size in bytes of the file specified in SetFileName.  If the  *)
    (* file is not accessible, the result is returned as MAX(CARD64).           *)

    BEGIN
        RETURN GetFileSize (SS^.iname, FALSE);
    END GetSize;

(********************************************************************************)

PROCEDURE GetSizeTypeAndName (SS: ClientFileInfo;  VAR (*OUT*) size: CARD64;
                              VAR (*OUT*) IsASCII: BOOLEAN;
                              VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the size and name of the currently chosen file, and a flag       *)
    (* to say whether the transfer type is ASCII.  We've already confirmed      *)
    (* that the caller is allowed to have this information.                     *)

    BEGIN
        size := GetFileSize (SS^.iname, TRUE);
        IsASCII := SS^.LineStructured;
        MakeShortName (SS^.iname, name);
    END GetSizeTypeAndName;

(********************************************************************************)

PROCEDURE GetSizeTypeAndNameU (SS: ClientFileInfo;  VAR (*OUT*) size: CARD64;
                              VAR (*OUT*) IsASCII: BOOLEAN;
                              VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the size and UTF-8 name of the currently chosen file, and a flag *)
    (* to say whether the transfer type is ASCII.  We've already confirmed      *)
    (* that the caller is allowed to have this information.                     *)

    VAR namebuff: FilenameString;

    BEGIN
        size := GetFileSize (SS^.iname, TRUE);
        IsASCII := SS^.LineStructured;
        MakeShortName (SS^.iname, namebuff);
        TranslateToUTF8 (namebuff, name);
    END GetSizeTypeAndNameU;

(********************************************************************************)

PROCEDURE ListingIsPossible (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff this user is allowed to see a listing of the file(s)    *)
    (* specified in the last SetFileName call.                                  *)

    BEGIN
        RETURN MayListFiles (SS^.iname);
    END ListingIsPossible;

(********************************************************************************)

PROCEDURE FileAvailable (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff the file exists and the user is allowed to read it.  *)

    BEGIN
        WITH SS^ DO
            RETURN CanReadFile (SS^.user, iname, IsManager);
        END (*WITH*);
    END FileAvailable;

(********************************************************************************)

PROCEDURE FileOrDirectoryVisible (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff the file or directory exists and is visible. *)

    BEGIN
        WITH SS^ DO
            RETURN CanSeeFileOrDir (user, iname, IsManager);
        END (*WITH*);
    END FileOrDirectoryVisible;

(********************************************************************************)

PROCEDURE IsDirectory (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff the name specified in the last SetFileName refers to    *)
    (* a directory, rather than to a file.  There are no guarantees in the      *)
    (* case where the name is inaccessible for any reason.                      *)

    BEGIN
        RETURN IsADirectory (SS^.iname, SS^.IsManager);
    END IsDirectory;

(********************************************************************************)

PROCEDURE CanWrite (SS: ClientFileInfo;  CheckExists: BOOLEAN;
                                   VAR (*OUT*) HavePermission: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff user can write the file whose name was last set.  If    *)
    (* CheckExists is TRUE and the file already exists, we must also have       *)
    (* permission to delete the old file of that name.  Exception: if the       *)
    (* restart point is nonzero then we don't need the delete permission.       *)
    (* Special case: returning with HavePermission = TRUE but a function result *)
    (* of FALSE means that we would have had permission to write the file but   *)
    (* have run out of disk space.                                              *)

    BEGIN
        WITH SS^ DO
            IF Compare64 (RestartPoint, Zero64) <> 0 THEN
                CheckExists := FALSE;
            END (*IF*);
            HavePermission := CanWriteFile (SS^.user, iname, IsManager, CheckExists);
            RETURN HavePermission
                    AND (SpaceAvailable(iname) > FreeSpaceThreshold);
        END (*WITH*);
    END CanWrite;

(********************************************************************************)

PROCEDURE CanDelete (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff user can delete the file whose name was last set.  *)

    BEGIN
        WITH SS^ DO
            RETURN CanDeleteFile (SS^.user, iname, IsManager);
        END (*WITH*);
    END CanDelete;

(********************************************************************************)

PROCEDURE CanRemoveDirectory (SS: ClientFileInfo): BOOLEAN;

    (* Returns TRUE iff user can delete the directory whose name was last set.  *)

    BEGIN
        RETURN CanDeleteDirectory (SS^.user, SS^.iname);
    END CanRemoveDirectory;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

PROCEDURE VersionIs (v: ARRAY OF CHAR);

    (* Stores the version number. *)

    BEGIN
        Strings.Assign (v, version);
    END VersionIs;

(************************************************************************)

PROCEDURE SetPassivePortRange (limit: BOOLEAN;  portmin, portmax: CARD16);

    (* If limit is TRUE, then portmin and portmax are the minimum and   *)
    (* maximum port values, respectively, that are permitted for        *)
    (* passive ftp.  If limit is FALSE, we let the tcp/ip stack choose  *)
    (* a port for us.                                                   *)

    BEGIN
        WITH PASVPorts DO
            Obtain (access);
            restrict := limit;
            IF NOT restrict THEN
                portmin := INADDR_ANY;
                portmax := INADDR_ANY;
            END (*IF*);
            MinDataPort := portmin;
            MaxDataPort := portmax;
            Release (access);
        END (*WITH*);
    END SetPassivePortRange;

(************************************************************************)

PROCEDURE SetBehindFirewall (enable: BOOLEAN;
                            MinLocalAddr, MaxLocalAddr, IPAddr: CARDINAL);

    (* If enable is TRUE, then IPAddr is the address that we report in  *)
    (* the PASV response.  Exception: if the client address is in the   *)
    (* range MinLocalAddr to MaxLocalAddr, inclusive, the "behind       *)
    (* firewall" rules are not applied.  The three numeric values are   *)
    (* in network byte order.                                           *)
    (* If enable is FALSE, the numeric parameters have no effect.       *)

    BEGIN
        WITH FWParams DO
            Obtain (access);
            LocalIPAddrMin := MinLocalAddr;
            LocalIPAddrMax := MaxLocalAddr;
            BehindFirewall := enable;
            IF BehindFirewall THEN
                FirewallIPAddr := IPAddr;
            END (*IF*);
            Release (access);
        END (*WITH*);
    END SetBehindFirewall;

(************************************************************************)

BEGIN
    LogLevel := 1;  MaxUsers := MAX(CARDINAL);
    CreateLock (MaxUsersLock);
    UserLogEnabled := TRUE;  CommonLogEnabled := FALSE;
    DoTagCheck := FALSE;
    CommonLogFileName := "COMMON.LOG";
    UserLogFileName := "FTPUSERS.LOG";
    WITH PASVPorts DO
        CreateLock (access);
        restrict := TRUE;
        MinDataPort := Swap2(49152);  MaxDataPort := Swap2(65535);
    END (*WITH*);
    WITH FWParams DO
        CreateLock (access);
        BehindFirewall := FALSE;
        FirewallIPAddr := 0;
        LocalIPAddrMin := 0;
        LocalIPAddrMax := 0;
    END (*WITH*);
END FtpTransfers.

