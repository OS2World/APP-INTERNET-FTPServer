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

IMPLEMENTATION MODULE LoggedOnUsers;

        (********************************************************)
        (*                                                      *)
        (*     Keeps track of the currently logged-on users     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 October 1997                 *)
        (*  Last edited:        5 February 2015                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT SysClock, Strings;

FROM SYSTEM IMPORT LOC, CARD8;

FROM Types IMPORT
    (* type *)  CARD64;

FROM FtpTransfers IMPORT
    (* type *)  ClientFileInfo,
    (* proc *)  CreateSession, CloseSession, FindUser, GetUserName,
                SetPort, AmountTransferred, CloseUser, KillDataChannel;

FROM FDUsers IMPORT
    (* type *)  UserCategory;

FROM Names IMPORT
    (* type *)  UserName;

FROM IPFilter IMPORT
    (* proc *)  UpdateMasterHostFilter, CheckMasterIPFilter,
                CheckUserIPFilter;

FROM Semaphores IMPORT
    (* type *)  Semaphore;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FtpdINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, ItemSize, INIGet, INIGetTrusted;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM InetUtilities IMPORT
    (* proc *)  IPToString, ConvertCard64, ConvertCardZ, AddEOL;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  getpeername, so_cancel, send;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM Conversions IMPORT
    (* proc *)  CardinalToString, StringToCardinal;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

(********************************************************************************)

CONST
    Nul = CHR(0);
    HammerTimeWindow = 10;     (* minutes *)

TYPE
    (* A place to store information about a particular session.                 *)
    (*   SessionID        a unique session identifier                           *)
    (*   LastCommand      the last command this user has issued                 *)
    (*   FileInfo         the user's file permissions, current options, etc.    *)
    (*   StartTime        the time when this session started, for logging       *)
    (*   ClientIPAddress  the client's IP address, for logging                  *)
    (*   IsGuest          TRUE iff this is a guest user                         *)

    ClientDataPointer = POINTER TO
                          RECORD
                              SessionID: ARRAY [0..5] OF CHAR;
                              LastCommand: ARRAY [0..1023] OF CHAR;
                              FileInfo: ClientFileInfo;
                              StartTime: SysClock.DateTime;
                              ClientIPAddress: CARDINAL;
                              IsGuest: BOOLEAN;
                              CommandInProgress: BOOLEAN;
                          END (*RECORD*);

    (* A record used to form a linked list of all ClientDataPointer     *)
    (* objects which are currently active.                              *)

    SessionPointer = POINTER TO
                         RECORD
                             this: ClientDataPointer;
                             next: SessionPointer;
                         END (*RECORD*);

    (* A record used in the checking of which IP addresses to allow and *)
    (* which to exclude.                                                *)

    RecordKind = CARD8[0..1];

    AddressListPointer = POINTER TO
                             RECORD
                                 allow: BOOLEAN;
                                 type: RecordKind;
                                 IPAddress, mask: CARDINAL;
                                 next: AddressListPointer;
                             END (*RECORD*);

    (* Records of IP addresses having an excessive rate of login failure.  *)
    (* The 'time' field records the time until which this record remains   *)
    (* "live"; if there's no further login failure before that time then   *)
    (* we're no longer treating this address as suspect. The 'count' field *)
    (* records how many login failures we've had so far.  To keep the list *)
    (* clean, we remove expired entries as a side-effect of each check.    *)

    HammerListPointer = POINTER TO
                            RECORD
                                next: HammerListPointer;
                                time: CARDINAL;
                                address: CARDINAL;
                                count: CARDINAL;
                            END (*RECORD*);

(********************************************************************************)

VAR
    (* List of active sessions, and a lock to protect it. *)

    SessionList: RECORD
                     access: Lock;
                     head, tail: SessionPointer;
                 END (*RECORD*);

    (* Count of number of guest users, a lock to protect it, and  *)
    (* a limit on the number of guest users.                      *)

    GuestCount: CARDINAL;
    GuestCountLock: Lock;
    GuestLimit: CARDINAL;

    (* List of IP addresses that we have blacklisted,  *)
    (* and a lock to protect it.                       *)

    Blacklist2: AddressListPointer;
    AddressCheckLock: Lock;

    (* List of IP addresses with excessive login failure rate,  *)
    (* ordered by expiry time.                                  *)

    HammerList: RECORD
                    access: Lock;
                    head: HammerListPointer;
                END (*RECORD*);

    (* Limit on number of simultaneous connections from the same IP address. *)
    (* We also use AddressCheckLock for critical section protection for this *)
    (* variable.                                                             *)

    SameIPLimit: CARDINAL;

    (* Flag to say that shutdown is in progress. *)

    ShutdownRequested: BOOLEAN;

(********************************************************************************)

PROCEDURE SetGuestLimit (limit: CARDINAL);

    (* Sets the limit on the number of simultaneous guest users. *)

    BEGIN
        Obtain(GuestCountLock);
        GuestLimit := limit;
        Release(GuestCountLock);
    END SetGuestLimit;

(********************************************************************************)

PROCEDURE StartSession (CommandSocket: Socket;  UserNumber, IPaddr: CARDINAL;
                        LogID: TransactionLogID;  KeepAlive: Semaphore;
                        VAR (*OUT*) UserHandle: ClientFileInfo): ClientDataPointer;

    (* Creates a new session state record.  During lengthy operations           *)
    (* we have to do a Signal(KeepAlive) every so often in order to stop the    *)
    (* session from timing out.  The returned UserHandle can be used as a user  *)
    (* identifier in calls to module FtpTransfers; it remains valid until       *)
    (* EndSession is called.                                                    *)

    VAR SP: SessionPointer;  p: ClientDataPointer;
        peer: SockAddr;  size: CARDINAL;

    BEGIN
        (* Create the FtpTransfer data structure. *)

        UserHandle := CreateSession (CommandSocket, UserNumber,
                                                LogID, KeepAlive);

        (* Create this module's data structure. *)

        NEW (p);
        WITH p^ DO
            CardinalToString (CommandSocket, SessionID, 6);
            LastCommand := "";
            FileInfo := UserHandle;
            SysClock.GetClock (StartTime);
            ClientIPAddress := IPaddr;
            IsGuest := FALSE;
            CommandInProgress := FALSE;
        END (*WITH*);

        (* Add it to our list of active sessions. *)

        NEW (SP);
        WITH SP^ DO
            this := p;  next := NIL;
        END (*WITH*);
        Obtain (SessionList.access);
        IF SessionList.head = NIL THEN
            SessionList.head := SP;
        ELSE
            SessionList.tail^.next := SP;
        END (*IF*);
        SessionList.tail := SP;
        Release (SessionList.access);

        (* Set the initial default data port. *)

        size := SIZE(peer);
        IF getpeername (CommandSocket, peer, size) THEN
        END (*IF*);
        SetPort (p^.FileInfo, peer.in_addr.addr, peer.in_addr.port);

        RETURN p;

    END StartSession;

(********************************************************************************)

PROCEDURE IdentifySession (CommandSocket: Socket): ClientDataPointer;

    (* Returns the session corresponding to this socket number. *)

    VAR ID: ARRAY [0..5] OF CHAR;  current: SessionPointer;

    BEGIN
        CardinalToString (CommandSocket, ID, 6);
        Obtain (SessionList.access);
        current := SessionList.head;
        WHILE (current <> NIL) AND NOT Strings.Equal (current^.this^.SessionID, ID) DO
            current := current^.next;
        END (*WHILE*);
        Release (SessionList.access);
        IF current = NIL THEN RETURN NIL
        ELSE RETURN current^.this;
        END (*IF*);
    END IdentifySession;

(********************************************************************************)

PROCEDURE ClientAddressAcceptable (IPAddress: CARDINAL): BOOLEAN;

    (* Returns TRUE if we are willing to accept connections from this address.  *)
    (* At this stage we have not yet decided whether to accept the new client,  *)
    (* so a client record for the new session does not yet exist.               *)

    VAR current: AddressListPointer;  acceptable: BOOLEAN;
        SP: SessionPointer;  count: CARDINAL;

    BEGIN
        (* Scan the secondary blacklist. *)

        Obtain (AddressCheckLock);
        current := Blacklist2;
        WHILE (current <> NIL)
                   AND (IPAddress <> current^.IPAddress) DO
            current := current^.next;
        END (*WHILE*);
        acceptable := current = NIL;
        Release (AddressCheckLock);

        (* Scan the master host filter to see whether this is an acceptable client. *)

        acceptable := acceptable AND CheckMasterIPFilter (IPAddress);

        (* If we've passed all checks so far, we still have to see whether      *)
        (* there are too many existing users with the same address.             *)

        IF acceptable THEN
            Obtain (SessionList.access);
            SP := SessionList.head;  count := 0;
            WHILE SP <> NIL DO
                IF SP^.this^.ClientIPAddress = IPAddress THEN
                    INC (count);
                END (*IF*);
                SP := SP^.next;
            END (*WHILE*);
            Release (SessionList.access);
            Obtain (AddressCheckLock);
            acceptable := count < SameIPLimit;
            Release (AddressCheckLock);
        END (*IF*);

        RETURN acceptable;

    END ClientAddressAcceptable;

(********************************************************************************)

PROCEDURE AddressForbidden (p: ClientDataPointer;  user: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if this address is not on our list of acceptable list for this client. *)

    VAR result: BOOLEAN;
        hini: HINI;

    BEGIN
        result := FALSE;
        hini := OpenINIFile();
        IF INIValid (hini) THEN
            result := NOT CheckUserIPFilter (hini, user, p^.ClientIPAddress);
            CloseINIFile (hini);
        END (*IF*);
        RETURN result;
    END AddressForbidden;

(********************************************************************************)

PROCEDURE EndSession (VAR (*INOUT*) p: ClientDataPointer);

    (* Discards the session information. *)

    VAR previous, current: SessionPointer;

    BEGIN
        IF p <> NIL THEN
            RemoveUser (p);
            CloseSession (p^.FileInfo);
            Obtain (SessionList.access);
            previous := NIL;  current := SessionList.head;
            LOOP
                IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
                IF current^.this = p THEN
                    IF previous = NIL THEN
                        SessionList.head := current^.next;
                    ELSE
                        previous^.next := current^.next;
                    END (*IF*);
                    IF current^.next = NIL THEN
                        SessionList.tail := previous;
                    END (*IF*);
                    DISPOSE (current);
                    EXIT (*LOOP*);
                END (*IF*);
                previous := current;  current := current^.next;
            END (*LOOP*);
            Release (SessionList.access);
            DISPOSE (p);
        END (*IF*);
    END EndSession;

(********************************************************************************)

PROCEDURE NewUser (client: ClientDataPointer;
                   Username: ARRAY OF CHAR;
                   VAR (*OUT*) LogSession: BOOLEAN): UserCategory;

    (* We assume that a session has already been established, but that there    *)
    (* is not currently a logged-in user.  This procedure produces an updated   *)
    (* session state that reflects the Username.                                *)

    (* We assume that a session has already been established, but that there    *)
    (* is not currently a logged-in user.  This procedure produces an updated   *)
    (* session state that reflects the Username.                                *)

    VAR category: UserCategory;

    BEGIN
        FindUser (Username, client^.FileInfo, category, LogSession);
        IF category = GuestUser THEN
            Obtain (GuestCountLock);
            IF GuestCount < GuestLimit THEN
                INC (GuestCount);
                client^.IsGuest := TRUE;
            ELSE
                category := OverflowUser;
            END (*IF*);
            Release (GuestCountLock);
        END (*IF*);
        RETURN category;
    END NewUser;

(********************************************************************************)

PROCEDURE RememberCommand (client: ClientDataPointer;
                           VAR (*IN*) CommandString: ARRAY OF CHAR);

    (* Stores CommandString as the last issued command. *)

    BEGIN
        Strings.Assign (CommandString, client^.LastCommand);
        client^.CommandInProgress := TRUE;
    END RememberCommand;

(********************************************************************************)

PROCEDURE CommandDone (client: ClientDataPointer): BOOLEAN;

    (* Notes that we have finished processing the command specified in          *)
    (* procedure RememberCommand.  Returns TRUE iff there is a shutdown in      *)
    (* progress.                                                                *)

    BEGIN
        client^.CommandInProgress := FALSE;
        RETURN ShutdownRequested;
    END CommandDone;

(********************************************************************************)

PROCEDURE AbortDataOperations (client: ClientDataPointer);

    (* Aborts the data transfer, if any, now in progress for this session. *)

    BEGIN
        IF client <> NIL THEN
            KillDataChannel (client^.FileInfo);

            (* On this call we appear to passing an invalid FileInfo value; except      *)
            (* that sometimes we don't get to do the call, apparently because of an     *)
            (* invalid client pointer.                                                  *)

        END (*IF*);
    END AbortDataOperations;

(********************************************************************************)

PROCEDURE KillUser (ID: ARRAY OF CHAR);

    (* Attempts to kill the session whose ID is given.  This involves aborting the      *)
    (* data operations, and then cancelling operations on the session's command         *)
    (* socket.  The cancel will cause a higher-level module to detect that the          *)
    (* session is no longer active, and then it is up to that higher-level module       *)
    (* to clean up neatly.  Because we count on higher-level software to do the         *)
    (* operations that will lead to an EndSession call, the present procedure does      *)
    (* not actually remove the session from the list of active sessions.                *)

    VAR client: ClientDataPointer;  CommandSocket: Socket;

    BEGIN
        CommandSocket := StringToCardinal (ID);
        client := IdentifySession (CommandSocket);
        IF client <> NIL THEN
            KillDataChannel (client^.FileInfo);
            so_cancel (CommandSocket);
        END (*IF*);
    END KillUser;

(****************************************************************************************)

PROCEDURE KillAllUsers (RightNow: BOOLEAN);

    (* Aborts the data operations, and then cancels operations on the command socket,   *)
    (* for all client sessions known to us.  This will cause a higher-level module to   *)
    (* close those sessions.                                                            *)

    VAR p: SessionPointer;  client: ClientDataPointer;

    BEGIN
        ShutdownRequested := TRUE;
        Obtain (SessionList.access);
        p := SessionList.head;
        WHILE p <> NIL DO
            client := p^.this;
            IF (client <> NIL) AND (RightNow OR NOT client^.CommandInProgress) THEN
                KillDataChannel (client^.FileInfo);
                so_cancel (StringToCardinal (client^.SessionID));
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
        Release (SessionList.access);
    END KillAllUsers;

(****************************************************************************************)

PROCEDURE RemoveUser (client: ClientDataPointer);

    (* Logs out the user, but does not terminate the session.  *)

    BEGIN
        IF client^.IsGuest THEN
            Obtain (GuestCountLock);
            DEC (GuestCount);
            client^.IsGuest := FALSE;
            Release (GuestCountLock);
        END (*IF*);
        CloseUser (client^.FileInfo);
    END RemoveUser;

(****************************************************************************************)

PROCEDURE ConvertDateTime (Time: SysClock.DateTime;  VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                                     VAR (*INOUT*) pos: CARDINAL);

    (* Stores the date/time value as a string starting at buffer[pos], updates pos. *)

    BEGIN
        ConvertCardZ (Time.year, buffer, 4, pos);
        buffer[pos] := '-';  INC(pos);
        ConvertCardZ (Time.month, buffer, 2, pos);
        buffer[pos] := '-';  INC(pos);
        ConvertCardZ (Time.day, buffer, 2, pos);
        buffer[pos] := ' ';  INC(pos);
        ConvertCardZ (Time.hour, buffer, 2, pos);
        buffer[pos] := ':';  INC(pos);
        ConvertCardZ (Time.minute, buffer, 2, pos);
        buffer[pos] := ':';  INC(pos);
        ConvertCardZ (Time.second, buffer, 2, pos);
        buffer[pos] := Nul;
    END ConvertDateTime;

(********************************************************************************)

PROCEDURE ListAllUsers (S: Socket);

    (* Sends a user listing to socket S. *)

    CONST Space = ' ';
        LineBufferSize = 1024;

    VAR p: SessionPointer;  CDP: ClientDataPointer;
        j: CARDINAL;  amount, total: CARD64;
        TempBuffer: ARRAY [0..63] OF CHAR;
        LineBuffer: ARRAY [0..LineBufferSize-1] OF CHAR;

    BEGIN
        Obtain (SessionList.access);
        p := SessionList.head;
        WHILE p <> NIL DO
            CDP := p^.this;
            LineBuffer := " ";
            Strings.Append (CDP^.SessionID, LineBuffer);
            Strings.Append (" ", LineBuffer);
            IPToString (CDP^.ClientIPAddress, TRUE, TempBuffer);
            Strings.Append (TempBuffer, LineBuffer);
            j := LENGTH(LineBuffer);
            WHILE j < 26 DO
                LineBuffer[j] := ' ';  INC(j);
            END (*WHILE*);
            ConvertDateTime (CDP^.StartTime, LineBuffer, j);
            LineBuffer[j] := Space;  INC(j);
            LineBuffer[j] := Nul;
            GetUserName (CDP^.FileInfo, TempBuffer);
            Strings.Append (TempBuffer, LineBuffer);
            IF CDP^.LastCommand[0] <> Nul THEN
                Strings.Append (" '", LineBuffer);
                Strings.Append (CDP^.LastCommand, LineBuffer);
                Strings.Append ("'", LineBuffer);
                AmountTransferred (CDP^.FileInfo, amount, total);
                j := Strings.Length(LineBuffer);
                IF j < LineBufferSize THEN
                    LineBuffer[j] := Space;  INC(j);
                    ConvertCard64 (amount, LineBuffer, j);
                END (*IF*);
                IF j < LineBufferSize THEN
                    LineBuffer[j] := Space;  INC(j);
                    ConvertCard64 (total, LineBuffer, j);
                END (*IF*);
                IF j < LineBufferSize THEN
                    LineBuffer[j] := Nul;
                END (*IF*);
            END (*IF*);
            j := AddEOL (LineBuffer);
            EVAL (send (S, LineBuffer, j, 0));
            p := p^.next;
        END (*WHILE*);
        Release (SessionList.access);
    END ListAllUsers;

(********************************************************************************)
(*                         OPERATIONS ON THE IP FILTER                          *)
(********************************************************************************)

PROCEDURE AddToBlacklist2 (address: CARDINAL);

    (* Adds the given address to our secondary blacklist. *)

    VAR p: AddressListPointer;

    BEGIN
        Obtain (AddressCheckLock);
        NEW (p);
        WITH p^ DO
            allow := FALSE;
            type := 1;
            IPAddress := address;
            mask := MAX(CARDINAL);
            next := Blacklist2;
        END (*WITH*);
        Blacklist2 := p;
        Release (AddressCheckLock);
    END AddToBlacklist2;

(********************************************************************************)

PROCEDURE ClearObsoleteHammerEntries(): CARDINAL;

    (* Removes expired entries on the Hammer list.  We assume that the caller   *)
    (* has exclusive access to this list.  Also returns the current time,       *)
    (* measured in minutes from an arbitrary origin.                            *)

    VAR current: HammerListPointer;
        now: CARDINAL;

    BEGIN
        (* Current time, to nearest minute. *)

        now := (time() + 30) DIV 60;

        current := HammerList.head;
        WHILE (current <> NIL) AND (current^.time < now) DO
            HammerList.head := current^.next;
            DISPOSE (current);
            current := HammerList.head;
        END (*WHILE*);

        RETURN now;

    END ClearObsoleteHammerEntries;

(********************************************************************************)

PROCEDURE UpdateHammerList (endtime, IPAddress: CARDINAL);

    (* Records a new entry into the list of suspect IP addresses.    *)
    (* We assume that the caller has exclusive access to this list.  *)

    CONST CountLimit = 3;

    VAR previous, current, next: HammerListPointer;
        action: (KeepChecking, DoNothing, Insert, Remove);

    BEGIN
        previous := NIL;  next := NIL;
        current := HammerList.head;
        action := KeepChecking;
        REPEAT
            IF current = NIL THEN
                action := Insert;
            ELSE
                next := current^.next;
                IF current^.address = IPAddress THEN

                    (* We have an existing entry for the same address. *)
                    (* Update its count and time, or blacklist this    *)
                    (* address if the count is already too high.       *)

                    IF current^.count >= CountLimit THEN
                        AddToBlacklist2 (IPAddress);
                        action := Remove;
                    ELSE
                        INC (current^.count);
                        current^.time := endtime;
                        action := DoNothing;
                    END (*IF*);

                ELSIF current^.time > endtime THEN
                    action := Insert;
                ELSE
                    previous := current;  current := next;
                END (*IF*);

            END (*IF*);

        UNTIL action <> KeepChecking;

        (* Now insert a new entry or delete the old one, as required. *)

        IF action = Insert THEN

            (* Add a new list entry between previous and current.   *)

            NEW (current);
            current^.address := IPAddress;
            current^.time := endtime;
            current^.count := 0;
            IF previous = NIL THEN
                current^.next := HammerList.head;
                HammerList.head := current;
            ELSE
                current^.next := previous^.next;
                previous^.next := current;
            END (*IF*);

        ELSIF action = Remove THEN

            (* Remove the list entry between previous and next.   *)

            IF previous = NIL THEN
                HammerList.head := next;
            ELSE
                previous^.next := next;
            END (*IF*);
            DISPOSE (current);

        END (*IF*);

    END UpdateHammerList;

(************************************************************************)

PROCEDURE IsInternal (address: CARDINAL): BOOLEAN;

    (* Returns TRUE iff address (in network byte order) is one of the   *)
    (* addresses reserved for internal LAN use.  These are:             *)
    (*     (Class A) 10.*.*.*                                           *)
    (*     (Class B) 172.16.*.* through 172.31.*.*                      *)
    (*     (Class C) 192.168.*.*                                        *)
    (* Note that the addresses are stored in bigendian order but the    *)
    (* processor does its calculations in littleendian order.  That is  *)
    (* why the numbers below appear to be back to front.                *)

    BEGIN
        RETURN (address = 127 + 1*256*256*256)
             OR (IAND(address, 0FFH) = 10)
             OR (IAND(address, 010FFH) = 172 + 256*16)
             OR (IAND(address, 0FFFFH) = 192 + 256*168);
    END IsInternal;

(************************************************************************)

PROCEDURE CheckForHammerer (client: ClientDataPointer);

    (* This is called after a login failure (wrong password, etc.).  If we      *)
    (* find repeated failures in the short term, we add the client IP address   *)
    (* to our blacklist.                                                        *)

    VAR now: CARDINAL;

    BEGIN
        IF NOT IsInternal (client^.ClientIPAddress) THEN
            Obtain (HammerList.access);
            IF HammerTimeWindow > 0 THEN
                now := ClearObsoleteHammerEntries();
                UpdateHammerList (now + HammerTimeWindow, client^.ClientIPAddress);
            END (*IF*);
            Release (HammerList.access);
        END (*IF*);
    END CheckForHammerer;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

PROCEDURE LoadSecurityParameters;

    (* Reads the "same IP limit" and the client IP address filter from  *)
    (* the INI file.  Old values, if any, are discarded.                *)

    VAR hini: HINI;
    SYSapp: ARRAY [0..4] OF CHAR;

    BEGIN
        SYSapp := "$SYS";
        hini := OpenINIFile();
        IF INIValid (hini) THEN
            IF NOT INIGet (hini, SYSapp, "SameIPLimit", SameIPLimit) THEN
                SameIPLimit := MAX(CARDINAL);
            END (*IF*);
            UpdateMasterHostFilter (hini);
            CloseINIFile (hini);
        END (*IF*);
    END LoadSecurityParameters;

(************************************************************************)

BEGIN
    ShutdownRequested := FALSE;
    WITH SessionList DO
        head := NIL;  tail := NIL;
        CreateLock (access);
    END (*WITH*);
    CreateLock (AddressCheckLock);
    SameIPLimit := MAX(CARDINAL);
    Blacklist2 := NIL;
    GuestCount := 0;  GuestLimit := 0;
    CreateLock (GuestCountLock);
    WITH HammerList DO
        head := NIL;
        CreateLock (access);
    END (*WITH*);
END LoggedOnUsers.

