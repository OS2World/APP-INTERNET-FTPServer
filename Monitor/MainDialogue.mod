(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
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

IMPLEMENTATION MODULE MainDialogue;

        (****************************************************************)
        (*                    Monitor for FtpServer                     *)
        (*                        Main dialogue                         *)
        (*                                                              *)
        (*    Started:        29 March 2000                             *)
        (*    Last edited:    19 December 2018                          *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT
    (* type *)  ADDRESS, INT16,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Init, INIData, Misc, Strings, KillServer, About;

FROM Arith64 IMPORT
    (* const*)  Zero64,
    (* type *)  CARD64,
    (* proc *)  Compare64, Add64, Diff64, LongDiv64, ShortDiv;

FROM FtpCl2 IMPORT
    (* proc *)  GetHostAddresses, HostAddress, ConnectToServer, Login,
                SendCommand, GetResponse, SetMessageWindow,
                SetupDialogue, SamplingInterval, LoadINIData;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertDecimal, ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  StringToIPAddress, IPToString;

FROM MonINI IMPORT
    (* proc *)  GetINIFileName;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateTask1, CreateLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  IAND, EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST ShowRaw = FALSE;          (* special case for debugging *)

TYPE
    CharSet = SET OF CHAR;

CONST
    Digits = CharSet {'0'..'9'};
    StringSize = 512;

CONST
    Nul = CHR(0);

    WM_DISPLAYUSERS   = OS2.WM_USER + 1;
    WM_DOSETUP        = OS2.WM_USER + 2;
    WM_SHUTDOWN       = OS2.WM_USER + 3;
    WM_SHUTDOWNDONE   = OS2.WM_USER + 4;
    WM_KILLUSER       = OS2.WM_USER + 5;        (* obsolete, I think *)
    WM_MULTIKILLUSERS = OS2.WM_USER + 6;
    WM_KILLSERVER     = OS2.WM_USER + 7;

TYPE
    String = ARRAY [0..StringSize-1] OF CHAR;

    UserDataPtr = POINTER TO UserDataRecord;

    (* The fields in a UserDataRecord record are:                   *)
    (*                                                              *)
    (*   ID            session ID as reported by FtpServer          *)
    (*   IPaddress     address of the client                        *)
    (*   amount        number of bytes transferred                  *)
    (*   total         total bytes to be transferred                *)
    (*   speed         our estimate of transfer speed               *)
    (*   selected      TRUE if selected in listbox                  *)
    (*   strval        initially data supplied by the server,       *)
    (*                    then what is displayed in the listbox     *)
    (*   LastCommand   last command executed by client              *)

    UserDataRecord = RECORD
                         next: UserDataPtr;
                         ID: CARDINAL;
                         IPaddress: CARDINAL;
                         amount, total: CARD64;
                         speed: CARDINAL;
                         selected: BOOLEAN;
                         strval: String;
                         LastCommand: String;
                     END (*RECORD*);

    ListofListsPtr = POINTER TO
                      RECORD
                          nextlist: ListofListsPtr;
                          thislist: UserDataPtr;
                      END (*IF*);

    KillList = POINTER TO
                   RECORD
                       next: KillList;
                       this: CARDINAL;    (* user ID *)
                   END (*RECORD*);

VAR
    (* Switch entry data. *)

    SwitchData : OS2.SWCNTRL;

    (* Character to separate thousands in decimal number. *)

    ThousandsSeparator: CHAR;

    (* Semaphore to tell the TalkToServer task that it's OK to proceed. *)

    OKToConnect: Semaphore;

    (* Option to keep the dialogue window on top. *)

    AlwaysOnTop: BOOLEAN;

    (* Flag to prevent a recursion in the window procedure. *)

    IsPuttingOnTop: BOOLEAN;

    (* Flag to tell the TalkToServer task to log out. *)

    DropConnection: BOOLEAN;

    (* SamplingInterval, in milliseconds. *)

    CycleTime: CARDINAL;

    (* Data used when killing a user.  To kill, put the ID into IDtoKill, and   *)
    (* set KillFlag to TRUE.  The command has been accepted when KillFlag       *)
    (* goes FALSE again.                                                        *)

    KillFlag: BOOLEAN;
    IDtoKill: CARDINAL;

    (* List of lists of user records waiting to be displayed.  Normally there   *)
    (* would be at most two lists here - the currently displayed one, and the   *)
    (* next update - but we allow for more to avoid list corruption in the      *)
    (* case where there's been a slowdown in processing.                        *)

    MasterList: RECORD
                    access: Lock;
                    head: ListofListsPtr;
                END (*RECORD*);

    (* Queue of commands to the action handler. *)

    ActionQueue: OS2.HMQ;

    (* Semaphore to say that action queue task has initialised. *)

    synch: Semaphore;

    (* Flag to say that the server should be shut down. *)

    ShutdownOption: CHAR;

    (* Flag to tell all Monitor tasks to terminate. *)

    ShutdownRequest: BOOLEAN;

    (* Semaphore by which the subsidiary tasks say that they have finished. *)

    AllDone: Semaphore;

(********************************************************************************)
(*                   CONVERTING SERVER REPLY TO A USERDATA RECORD               *)
(********************************************************************************)

PROCEDURE MakeDummyRecord (VAR (*IN*) text: ARRAY OF CHAR): UserDataPtr;

    (* For debugging: makes a record holding the raw text.  *)

    VAR p: UserDataPtr;

    BEGIN
        NEW (p);
        WITH p^ DO
            next := NIL;
            ID := 0;
            IPaddress := 0;
            LastCommand := "";
            amount := Zero64;
            total := Zero64;
            Strings.Assign (text, strval);
        END (*WITH*);
        RETURN p;
    END MakeDummyRecord;

(********************************************************************************)

PROCEDURE MakeUserRecord (VAR (*IN*) text: ARRAY OF CHAR): UserDataPtr;

    (* Creates a new user data record from the given text. *)

    VAR p: UserDataPtr;  j, j1, j2: CARDINAL;
        buffer: ARRAY [0..31] OF CHAR;
        found: BOOLEAN;

    (****************************************************************************)

    PROCEDURE SkipSpaces;
        BEGIN
            WHILE (j < StringSize) AND (p^.strval[j] = ' ') DO
                INC (j);
            END (*WHILE*);
        END SkipSpaces;

    (****************************************************************************)

    BEGIN
        NEW(p);
        p^.selected := FALSE;
        Strings.Assign (text, p^.strval);

        (* Decode the numeric session ID. *)

        j := 0;  SkipSpaces;
        p^.ID := ConvertDecimal (p^.strval, j);
        SkipSpaces;

        (* Decode the numeric IP address. *)

        p^.IPaddress := StringToIPAddress (p^.strval, j);
        WHILE (p^.strval[j] <> Nul) AND (p^.strval[j] <> " ") DO
            INC (j);
        END (*WHILE*);
        SkipSpaces;

        IF j > 0 THEN
            Strings.Delete (p^.strval, 0, j);
        END (*IF*);

        (* Move past the date, time, and username. *)

        Strings.FindNext (' ', p^.strval, 20, found, j1);
        IF found THEN
            REPEAT
                INC (j1);
            UNTIL (j1 >= StringSize) OR (p^.strval[j1] <> ' ');

            (* Extract the "last command" field. *)

            IF j1 >= StringSize THEN
                buffer[0] := Nul;
            ELSE
                buffer[0] := p^.strval[j1];
            END (*IF*);
            buffer[1] := Nul;
            Strings.FindNext (buffer, p^.strval, j1+1, found, j2);
            IF found AND (j2 > j1) THEN
                Strings.Extract (p^.strval, j1+1, j2-j1-1, p^.LastCommand);
                Strings.Delete (p^.strval, j1, j2-j1+1);
            ELSE
                p^.LastCommand := "";
            END (*IF*);

            (* Decode the amount and total fields. *)

            j := j1;  SkipSpaces;
            p^.amount := Misc.StringToCard64 (p^.strval, j);
            SkipSpaces;
            p^.total := Misc.StringToCard64 (p^.strval, j);
            Strings.Delete (p^.strval, j1, j-j1);
        ELSE
            p^.LastCommand := "";
            p^.amount := Zero64;  p^.total := Zero64;
        END (*IF*);

        (* Speed will be calculated later. *)

        p^.speed := 0;

        (* Put IP address back onto text line. *)

        Strings.Append (" (", p^.strval);
        IPToString (p^.IPaddress, FALSE, buffer);
        Strings.Append (buffer, p^.strval);
        Strings.Append (")", p^.strval);

        RETURN p;

    END MakeUserRecord;

(********************************************************************************)
(*                          USERDATA LIST OPERATIONS                            *)
(********************************************************************************)

PROCEDURE DiscardList (VAR (*INOUT*) head: UserDataPtr);

    (* Disposes of the list records. *)

    VAR p: UserDataPtr;

    BEGIN
        WHILE head <> NIL DO
            p := head^.next;  DISPOSE(head);  head := p;
        END (*WHILE*);
    END DiscardList;

(********************************************************************************)

PROCEDURE InsertInList (VAR (*INOUT*) head: UserDataPtr;  VAR (*IN*) text: ARRAY OF CHAR);

    (* Adds a new record to a userdata list. *)

    VAR previous, p0, p, current: UserDataPtr;

    BEGIN
        p := MakeUserRecord (text);
        IF ShowRaw THEN
            p0 := MakeDummyRecord (text);
            p0^.next := p;
        END (*IF*);

        (* Work out the insertion point that will result in     *)
        (* the list being ordered by ID value.                  *)

        previous := NIL;  current := head;
        WHILE (current <> NIL) AND (current^.ID < p^.ID) DO
            previous := current;  current := current^.next;
        END (*WHILE*);

        (* Insert between previous^ and current^. *)

        p^.next := current;
        IF ShowRaw THEN
            p := p0;
        END (*IF*);
        IF previous = NIL THEN
            head := p;
        ELSE
            previous^.next := p;
        END (*IF*);

    END InsertInList;

(********************************************************************************)

PROCEDURE AddToMasterList (head: UserDataPtr);

    (* Adds a new list to the list of lists. *)

    VAR p, q: ListofListsPtr;

    BEGIN
        NEW (q);
        q^.nextlist := NIL;  q^.thislist := head;

        Obtain(MasterList.access);
        p := MasterList.head;
        IF p = NIL THEN
            MasterList.head := q;
        ELSE
            WHILE p^.nextlist <> NIL DO
                p := p^.nextlist;
            END (*WHILE*);
            p^.nextlist := q;
        END (*IF*);
        Release(MasterList.access);
    END AddToMasterList;

(********************************************************************************)
(*                        COMMUNICATION WITH FTP SERVER                         *)
(********************************************************************************)

PROCEDURE TalkToServer (dialoguewindow: ADDRESS);

    (* Task that deals with all the ftp traffic. *)

    VAR status, dlgwin: OS2.HWND;
        text1, ServerIP: ARRAY [0..1023] OF CHAR;
        Connected, LoggedIn, MoreToCome, TimedOut: BOOLEAN;
        j, NumberOfAddresses, address: CARDINAL;
        hab: OS2.HAB;      (* This thread's anchor block handle     *)
        hmq: OS2.HMQ;      (* This thread's message queue handle    *)
        head: UserDataPtr;

    BEGIN
        hab := OS2.WinInitialize(0);
        hmq := OS2.WinCreateMsgQueue (hab, 0);

        dlgwin := CAST (OS2.HWND, dialoguewindow);
        status := OS2.WinWindowFromID (dlgwin, DID.StatusLine);
        OS2.WinSetWindowText (status, " Not yet connected to server");
        SetMessageWindow (status);
        LoggedIn := FALSE;
        CycleTime := 1000*SamplingInterval();

        LOOP
            Wait (OKToConnect);

            (* Not yet connected to server. *)

            Connected := FALSE;
            WHILE NOT (DropConnection OR Connected) DO
                NumberOfAddresses := GetHostAddresses();
                j := 0;  MoreToCome := FALSE;
                IF NumberOfAddresses = 0 THEN
                    OS2.WinSetWindowText (status, "Can't resolve server address");
                    TimedWait (OKToConnect, CycleTime, TimedOut);
                ELSE
                    REPEAT
                        address := HostAddress(j);
                        Strings.Assign ("Trying ", text1);
                        IPToString (address, TRUE, ServerIP);
                        Strings.Append (ServerIP, text1);
                        OS2.WinSetWindowText (status, text1);
                        Connected := ConnectToServer (address);
                        IF NOT Connected THEN
                            INC (j);
                            IF j < NumberOfAddresses THEN
                                TimedWait (OKToConnect, 1000, TimedOut);
                            ELSE
                                TimedWait (OKToConnect, CycleTime, TimedOut);
                            END (*IF*);
                        END (*IF*);
                    UNTIL Connected OR DropConnection OR (j >= NumberOfAddresses);
                END (*IF*);
            END (*WHILE*);

            (* Now connected, log in. *)

            LOOP
                IF DropConnection THEN EXIT(*LOOP*) END(*IF*);
                OS2.WinSetWindowText (status, "Attempting to log in to server");
                IF Login() THEN
                    LoggedIn := TRUE;  EXIT(*LOOP*);
                ELSE
                    OS2.WinSetWindowText (status, "Username/password rejected");
                END(*IF*);
                TimedWait (OKToConnect, CycleTime, TimedOut);
            END (*LOOP*);

            IF LoggedIn AND NOT DropConnection THEN
                Strings.Assign ("Logged in to ", text1);
                Strings.Append (ServerIP, text1);
                OS2.WinSetWindowText (status, text1);
            END (*IF*);

            (* This is the main loop where we keep polling the server. *)

            LOOP
                IF DropConnection THEN EXIT(*LOOP*) END(*IF*);
                IF ShutdownOption = 'G' THEN
                    LoggedIn := SendCommand ("SITE MNGR GXIT");
                    ShutdownOption := Nul;
                ELSIF ShutdownOption = 'Q' THEN
                    LoggedIn := NOT SendCommand ("SITE MNGR EXIT");
                    ShutdownOption := Nul;
                ELSIF KillFlag THEN
                    Strings.Assign ("SITE MNGR KILL ", text1);
                    j := Strings.Length (text1);
                    ConvertCard (IDtoKill, text1, j);
                    text1[j] := Nul;
                    IF SendCommand (text1) THEN
                        REPEAT
                            LoggedIn := GetResponse (text1, MoreToCome);
                        UNTIL NOT (LoggedIn AND MoreToCome);
                    ELSE
                        LoggedIn := FALSE;
                    END (*IF*);
                    KillFlag := FALSE;
                ELSIF SendCommand ("SITE MNGR LIST") THEN
                    head := NIL;
                    REPEAT
                        IF GetResponse (text1, MoreToCome) THEN

                            (* Discard the initial and final comment lines. *)

                            IF MoreToCome AND NOT (text1[0] IN Digits) THEN
                                InsertInList (head, text1);
                            END (*IF*);
                        ELSE
                            MoreToCome := FALSE;
                            Connected := FALSE;
                            LoggedIn := FALSE;
                        END (*IF*);
                    UNTIL NOT MoreToCome;
                    IF LoggedIn THEN
                        AddToMasterList(head);
                        OS2.WinPostMsg (dlgwin, WM_DISPLAYUSERS, NIL, NIL);
                    ELSE
                        DiscardList(head);
                    END (*IF*);
                ELSE
                    LoggedIn := FALSE;
                    Connected := FALSE;
                END (*IF*);
                IF NOT LoggedIn THEN
                    EXIT (*LOOP*);
                END (*IF*);
                TimedWait (OKToConnect, CycleTime, TimedOut);
            END (*LOOP*);

            (* We reach this point either if the connection is lost or if the   *)
            (* user wants to log out.                                           *)

            IF DropConnection THEN
                IF Connected THEN
                    OS2.WinSetWindowText (status, "Logging out");
                    IF SendCommand ("QUIT") THEN
                        REPEAT
                        UNTIL NOT GetResponse(text1, MoreToCome) OR NOT MoreToCome;
                    END (*IF*);
                END (*IF*);
                OS2.WinSetWindowText (status, "Logged out");
                LoggedIn := FALSE;  DropConnection := FALSE;
                IF ShutdownRequest THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSE
                OS2.WinSetWindowText (status, "Connection lost");
                Sleep (5000);
                Signal (OKToConnect);
            END (*IF*);

        END (*LOOP*);

        Signal (AllDone);

    END TalkToServer;

(************************************************************************)
(*                          ACTION DISPATCHER                           *)
(************************************************************************)

PROCEDURE ActionHandler (hwnd0: ADDRESS);

    TYPE A3 = ARRAY [0..2] OF CHAR;
    CONST ShutdownCode = A3{Nul, 'G', 'Q'};

    (* Runs as a separate task, responding to pushbutton messages. *)

    VAR hab: OS2.HAB;      (* This thread's anchor block handle     *)
        msg: OS2.QMSG;
        hwnd: OS2.HWND;
        code: CARDINAL;
        list, next: KillList;

    BEGIN
        hab := OS2.WinInitialize(0);
        ActionQueue := OS2.WinCreateMsgQueue (hab, 0);
        hwnd := CAST (OS2.HWND, hwnd0);
        Signal (synch);
        LOOP
            OS2.WinGetMsg (hab, msg, 0, 0, 0);

            IF msg.msg = WM_DOSETUP THEN
                DropConnection := TRUE;
                Signal (OKToConnect);
                WHILE DropConnection DO
                    Sleep (60);
                END (*WHILE*);
                OS2.WinSetVisibleRegionNotify (hwnd, FALSE);
                AlwaysOnTop := SetupDialogue (hwnd);
                IF AlwaysOnTop THEN
                    OS2.WinSetVisibleRegionNotify (hwnd, TRUE);
                END (*IF*);
                CycleTime := 1000*SamplingInterval();
                DropConnection := FALSE;
                Signal (OKToConnect);

            ELSIF msg.msg = WM_KILLUSER THEN
                WHILE KillFlag DO
                    Sleep (300);
                END (*WHILE*);
                IDtoKill := OS2.LONGFROMMP(msg.mp1);
                KillFlag := TRUE;
                Signal (OKToConnect);
                Signal (OKToConnect);

            ELSIF msg.msg = WM_MULTIKILLUSERS THEN
                list := msg.mp1;
                WHILE list <> NIL DO
                    WHILE KillFlag DO
                        Sleep (300);
                    END (*WHILE*);
                    IDtoKill := list^.this;
                    KillFlag := TRUE;
                    Signal (OKToConnect);
                    Signal (OKToConnect);
                    next := list^.next;
                    DISPOSE (list);
                    list := next;
                END (*WHILE*);

            ELSIF msg.msg = WM_KILLSERVER THEN
                code := KillServer.RunDialogue(hwnd);
                ShutdownOption := ShutdownCode[code];
                Signal (OKToConnect);
                IF code = 2 THEN
                    AddToMasterList(NIL);
                    OS2.WinPostMsg (hwnd, WM_DISPLAYUSERS, NIL, NIL);
                END (*IF*);
                Signal (OKToConnect);

            ELSIF msg.msg = WM_SHUTDOWN THEN
                ShutdownRequest := TRUE;  DropConnection := TRUE;
                Signal (OKToConnect);
                Wait (AllDone);
                EXIT (*LOOP*);

            END (*IF*);

        END (*LOOP*);

        OS2.WinPostMsg (hwnd, WM_SHUTDOWNDONE, NIL, NIL);

    END ActionHandler;

(************************************************************************)
(*                       UPDATING THE DISPLAY                           *)
(************************************************************************)

PROCEDURE UpdateSpeed (q1, q2: UserDataPtr);

    (* On entry q1^ is the previous data record and q2^ is the new one. *)
    (* We update the speed estimate q2^.speed.                          *)

    VAR amount: CARD64;  time: CARDINAL;

    BEGIN
        IF Compare64 (q2^.amount, q1^.amount) >= 0 THEN
            amount := CAST (CARD64, Diff64 (q2^.amount, q1^.amount));
        ELSE
            amount := Zero64;
        END (*IF*);
        time := CycleTime DIV 1000;
        Add64 (amount, time DIV 2);
        IF amount.high <> 0 THEN
            q2^.speed := MAX(CARDINAL) DIV time;
        ELSE
            q2^.speed := ShortDiv(amount, time);
            IF q2^.speed > (MAX(CARDINAL) - q1^.speed - 4) DIV 7 THEN
                q2^.speed := MAX(CARDINAL) DIV 8;
            ELSE
                q2^.speed := (7*q2^.speed + q1^.speed + 4) DIV 8;
            END (*IF*);
        END (*IF*);
    END UpdateSpeed;

(************************************************************************)

PROCEDURE UpdateDisplay (disp: OS2.HWND);

    (* Replaces the information in the display window, which should     *)
    (* be consistent with the first entry in the master list, by the    *)
    (* second entry in the master list.  (And the master list is        *)
    (* updated, with the old first entry thrown away.)                  *)

    VAR p: ListofListsPtr;  q1, q2: UserDataPtr;  index: INT16;

    BEGIN
        Obtain (MasterList.access);

        (* If there are fewer than two lists on the master list, we     *)
        (* must have a synchronisation problem, so we don't do anything.*)

        p := MasterList.head;
        IF (p <> NIL) AND (p^.nextlist <> NIL) THEN
            MasterList.head := p^.nextlist;
            q1 := p^.thislist;  q2 := p^.nextlist^.thislist;

            (* q1 is the old list, q2 is the new one. *)

            (* Go through the two lists, checking for mismatches. *)

            index := 0;
            WHILE (q1 <> NIL) OR (q2 <> NIL) DO
                IF (q1 = NIL) OR ((q2 <> NIL) AND (q1^.ID > q2^.ID)) THEN
                    (* New item to be added. *)
                    OS2.WinSendMsg (disp, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(q2^.strval));
                    OS2.WinSendMsg (disp, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMULONG(ORD(FALSE)));
                    q2 := q2^.next;
                    INC (index);
                ELSIF (q2 = NIL) OR (q1^.ID < q2^.ID) THEN
                    (* Old entry to be removed. *)
                    OS2.WinSendMsg (disp, OS2.LM_DELETEITEM,
                          OS2.MPFROMSHORT(index), NIL);
                    q1 := q1^.next;
                ELSE
                    (* Match, update existing item. *)

                    q2^.selected := q1^.selected;
                    UpdateSpeed (q1, q2);
                    OS2.WinSendMsg (disp, OS2.LM_SETITEMTEXT,
                          OS2.MPFROMSHORT(index), ADR(q2^.strval));
                    IF q2^.selected THEN
                        OS2.WinSendMsg (disp, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMULONG(ORD(TRUE)));
                    ELSE
                        OS2.WinSendMsg (disp, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMULONG(ORD(FALSE)));
                    END (*IF*);
                    q1 := q1^.next;  q2 := q2^.next;
                    INC (index);
                END (*IF*);
            END (*WHILE*);

            DiscardList (p^.thislist);
            DISPOSE (p);

        END (*IF*);

        Release (MasterList.access);

    END UpdateDisplay;

(************************************************************************)

PROCEDURE ConvertCard64 (NN: CARD64;  VAR (*INOUT*) buffer: ARRAY OF CHAR;
                       VAR (*INOUT*) pos: CARDINAL);

    (* Converts NN to decimal string, starting at buffer[pos].  The     *)
    (* subscript pos is updated.  We don't use the equivalent "Misc"    *)
    (* operation because we want to include thousands separators.       *)

    (********************************************************************)

    PROCEDURE Conv1 (N: CARDINAL);

        (* Single-digit number. *)

        BEGIN
            buffer[pos] := CHR(N + ORD('0'));
            INC (pos);
        END Conv1;

    (********************************************************************)

    PROCEDURE Conv3 (N: CARDINAL);

        (* Exactly three digits, with leading zero fill. *)

        BEGIN
            Conv1 (N DIV 100);  N := N MOD 100;
            Conv1 (N DIV 10);
            Conv1 (N MOD 10);
        END Conv3;

    (********************************************************************)

    PROCEDURE ConvNLZsmall (N: CARDINAL);

        (* No leading zero fill, N guaranteed < 1000. *)

        BEGIN
            IF N > 9 THEN
                ConvNLZsmall (N DIV 10);
                N := N MOD 10;
            END (*IF*);
            Conv1(N);
        END ConvNLZsmall;

    (********************************************************************)

    PROCEDURE ConvNLZ (NN: CARD64);

        (* No leading zero fill. *)

        VAR Q: CARD64;  R: CARDINAL;

        BEGIN
            IF (NN.high = 0) AND (NN.low < 1000) THEN
                ConvNLZsmall (NN.low);
            ELSE
                LongDiv64 (NN, 1000, Q, R);
                ConvNLZ (Q);
                buffer[pos] := ThousandsSeparator;  INC(pos);
                Conv3 (R);
            END (*IF*);
        END ConvNLZ;

    (********************************************************************)

    BEGIN
        ConvNLZ (NN);
    END ConvertCard64;

(************************************************************************)

PROCEDURE ConvertCard (N: CARDINAL;  VAR (*INOUT*) buffer: ARRAY OF CHAR;
                       VAR (*INOUT*) pos: CARDINAL);

    (* Converts N to decimal string, starting at buffer[pos].  The      *)
    (* subscript pos is updated.  We don't use the equivalent "Misc"    *)
    (* operation because we want to include thousands separators.       *)

    VAR NN: CARD64;

    BEGIN
        NN.high := 0;  NN.low := N;
        ConvertCard64 (NN, buffer, pos);
    END ConvertCard;

(************************************************************************)

PROCEDURE DetailsOfSelectedItem (hwnd: OS2.HWND);

    (* Fills in the fields that refer to the first selected item. *)

    VAR index: INT16;  p: UserDataPtr;  j: CARDINAL;
        buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        index := OS2.SHORT1FROMMP(OS2.WinSendDlgItemMsg (hwnd, DID.UserList,
                          OS2.LM_QUERYSELECTION, OS2.MPFROMSHORT(0), NIL));
        IF index = OS2.LIT_NONE THEN
            OS2.WinSetDlgItemText (hwnd, DID.SessionID, "");
            OS2.WinSetDlgItemText (hwnd, DID.LastCommand, "");
            OS2.WinSetDlgItemText (hwnd, DID.AmountBox, "");
            OS2.WinSetDlgItemText (hwnd, DID.SpeedBox, "");
        ELSE
            Obtain (MasterList.access);
            p := MasterList.head^.thislist;
            WHILE (p <> NIL) AND (index > 0) DO
                p := p^.next;  DEC(index);
            END (*WHILE*);
            IF p <> NIL THEN
                OS2.WinSetDlgItemShort (hwnd, DID.SessionID, p^.ID, FALSE);
                OS2.WinSetDlgItemText (hwnd, DID.LastCommand, p^.LastCommand);


                    (* While testing. *)
                    (*
                    p^.amount.high := 500;  p^.amount.low := 0;
                    p^.total := p^.amount;
                    p^.speed := MAX(CARDINAL) DIV 8;
                    *)

                IF (Compare64(p^.amount, Zero64)=0)
                            AND (Compare64(p^.total, Zero64)=0) THEN
                    buffer := "";
                ELSE
                    j := 0;  ConvertCard64 (p^.amount, buffer, j);
                    IF Compare64(p^.total, Zero64) <> 0 THEN
                        buffer[j] := ' ';  INC(j);
                        buffer[j] := '/';  INC(j);
                        buffer[j] := ' ';  INC(j);
                        ConvertCard64 (p^.total, buffer, j);
                    END (*IF*);
                    buffer[j] := Nul;
                    Strings.Append (" bytes", buffer);
                END (*IF*);
                OS2.WinSetDlgItemText (hwnd, DID.AmountBox, buffer);

                IF (p^.speed = 0) THEN
                    buffer := "";
                ELSE
                    j := 0;  ConvertCard (p^.speed, buffer, j);
                    buffer[j] := Nul;
                    Strings.Append (" bytes/sec", buffer);
                END (*IF*);
                OS2.WinSetDlgItemText (hwnd, DID.SpeedBox, buffer);

            END (*IF*);
            Release (MasterList.access);
        END (*IF*);
    END DetailsOfSelectedItem;

(************************************************************************)

PROCEDURE AdjustListWindowSize (pFrameSwp: OS2.PSWP);

    VAR val: CARDINAL;

    BEGIN
        WITH pFrameSwp^ DO
            IF IAND (OS2.SWP_SIZE, fl) <> 0 THEN
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.UserList),
                                     0, 0, 0, cx-16, cy-150, OS2.SWP_SIZE);
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.Label1),
                                     0, 8, cy-54, 0, 0, OS2.SWP_MOVE);
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.SessionID),
                                     0, 100, cy-54, 0, 0, OS2.SWP_MOVE);
                val := cx DIV 2;
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.AmountBox),
                                     0, val, cy-54, val-8, 17, OS2.SWP_MOVE+OS2.SWP_SIZE);
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.SpeedBox),
                                     0, val, cy-71, val-8, 17, OS2.SWP_MOVE+OS2.SWP_SIZE);
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.Label2),
                                     0, 8, cy-88, 0, 0, OS2.SWP_MOVE);
                OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd,DID.LastCommand),
                                     0, 128, cy-88, 0, 0, OS2.SWP_MOVE);
            END (*IF*);
        END (*WITH*);
    END AdjustListWindowSize;

(************************************************************************)

PROCEDURE WriteToStatus (hwnd: OS2.HWND;  VAR (*IN*) msg: ARRAY OF CHAR;  value: INTEGER);

    (* For debugging. *)

    VAR status: OS2.HWND;
        N: CARDINAL;

    BEGIN
        status := OS2.WinWindowFromID (hwnd, DID.StatusLine);
        IF value < 0 THEN
            Strings.Append ("-", msg);
            value := -value;
        END (*IF*);
        N := Strings.Length(msg);
        ConvertCard (value, msg, N);
        msg[N] := Nul;
        OS2.WinSetWindowText (status, msg);
    END WriteToStatus;

(************************************************************************)

PROCEDURE UpdateSelectionFlags (hwnd:OS2.HWND);

    VAR index, j: INT16;
        p: UserDataPtr;

    BEGIN
        Obtain (MasterList.access);
        index := OS2.LIT_NONE;  j := 0;
        p := MasterList.head^.thislist;
        WHILE p <> NIL DO
            index := OS2.SHORT1FROMMP(OS2.WinSendDlgItemMsg (hwnd,
                  DID.UserList,
                  OS2.LM_QUERYSELECTION, OS2.MPFROMSHORT(index), NIL));

            (*
            msg := "index = ";
            WriteToStatus (hwnd, msg, index);
            *)

            IF index = OS2.LIT_NONE THEN
                index := MAX(INT16);
            END (*IF*);
            WHILE (p <> NIL) AND (j < index) DO
                p^.selected := FALSE;  INC(j);
                p := p^.next;
            END (*WHILE*);
            IF p <> NIL THEN
                p^.selected := TRUE;  INC(j);
                p := p^.next;
            END (*IF*);
        END (*WHILE*);
        Release (MasterList.access);
    END UpdateSelectionFlags;

(************************************************************************)
(*                           MAIN DIALOGUE                              *)
(************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc (hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR p: UserDataPtr;
        NotificationCode: CARDINAL;
        head, current, tail: KillList;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   IF AlwaysOnTop THEN
                       OS2.WinSetVisibleRegionNotify (hwnd, TRUE);
                   ELSE
                       OS2.WinSetVisibleRegionNotify (hwnd, FALSE);
                   END (*IF*);
                   EVAL(CreateTask1 (ActionHandler, 1, "Monitor subsid",
                        CAST(ADDRESS,hwnd)));
                   Wait (synch);
                   EVAL(CreateTask1 (TalkToServer, 2, "Talk to server",
                        CAST(ADDRESS,hwnd)));
                   Signal (OKToConnect);

           |  OS2.WM_CLOSE:
                   OS2.WinPostQueueMsg(ActionQueue, WM_SHUTDOWN, NIL, NIL);

           |  OS2.WM_VRNENABLED:
                   IF NOT IsPuttingOnTop THEN
                       IsPuttingOnTop := TRUE;
                       OS2.WinSetWindowPos (hwnd, OS2.HWND_TOP, 0, 0, 0, 0, OS2.SWP_ZORDER);
                       IsPuttingOnTop := FALSE;
                       RETURN NIL;
                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_ACTIVATE:
                   OS2.WinSetWindowPos (hwnd, OS2.HWND_TOP, 0, 0, 0, 0, OS2.SWP_ZORDER);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  WM_SHUTDOWNDONE:
                   OS2.WinSetVisibleRegionNotify (hwnd, FALSE);
                   OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

           |  WM_DISPLAYUSERS:
                   UpdateDisplay (OS2.WinWindowFromID(hwnd, DID.UserList));
                   DetailsOfSelectedItem (hwnd);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_ADJUSTFRAMEPOS:
                   AdjustListWindowSize (mp1);

           |  OS2.WM_CHAR:
                   IF (IAND (OS2.ULONGFROMMP(mp1), OS2.KC_VIRTUALKEY + OS2.KC_KEYUP) = OS2.KC_VIRTUALKEY)
                                  AND (OS2.ULONGFROMMP(mp2) DIV 65536 = OS2.VK_DELETE) THEN
                        OS2.WinPostMsg (hwnd, OS2.WM_COMMAND, OS2.MPFROMULONG(DID.KillUserButton), NIL);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1) DIV 65536;
                   IF (NotificationCode = OS2.LN_SELECT) THEN
                       UpdateSelectionFlags (hwnd);
                       DetailsOfSelectedItem (hwnd);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | OS2.DID_CANCEL:
                          (* Ignore accidental use of Esc key. *)
                          RETURN NIL;

                     | DID.AboutButton:
                          About.Create (hwnd);

                     | DID.SetupButton:
                          OS2.WinPostQueueMsg(ActionQueue, WM_DOSETUP, NIL, NIL);

                     | DID.KillUserButton:

                          (* Instead of querying the listbox, we should be able to      *)
                          (* take the details from the 'selected' flags.                *)

                          Obtain (MasterList.access);
                          p := MasterList.head^.thislist;
                          head := NIL;  tail := NIL;
                          WHILE p <> NIL DO
                              IF p^.selected THEN

                                  (* Add this user to the kill list. *)

                                  NEW (current);
                                  current^.this := p^.ID;
                                  current^.next := NIL;
                                  IF head = NIL THEN
                                      head := current;
                                  ELSE
                                      tail^.next := current;
                                  END (*IF*);
                                  tail := current;
                              END (*IF*);
                              p := p^.next;
                          END (*WHILE*);
                          Release (MasterList.access);

                          IF head <> NIL THEN
                              OS2.WinPostQueueMsg(ActionQueue, WM_MULTIKILLUSERS,
                                                                   head, NIL);
                          END (*IF*);

                     | DID.KillServerButton:
                          OS2.WinPostQueueMsg(ActionQueue, WM_KILLSERVER, NIL, NIL);

                     | DID.ExitButton:
                          OS2.WinPostQueueMsg(ActionQueue, WM_SHUTDOWN, NIL, NIL);

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

        ELSE    (* default must call WinDefWindowProc() *)
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        RETURN NIL;

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE Create;

    (* Creates the main dialogue box. *)

    VAR hwnd: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;
        TNImode: BOOLEAN;
        INIname: ARRAY [0..511] OF CHAR;

    BEGIN
        AlwaysOnTop := LoadINIData();
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP,    (* parent *)
                       OS2.HWND_DESKTOP,   (* owner *)
                       MainDialogueProc,   (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.MainDialogue,   (* dialogue ID *)
                       NIL);               (* creation parameters *)

        (* Put us on the visible task list. *)

        OS2.WinQueryWindowProcess (hwnd, pid, tid);
        SwitchData.hwnd := hwnd;
        WITH SwitchData DO
            hwndIcon      := 0;
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "FTP Monitor";
            bProgType     := 0;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (Init.MainHab(), SwitchData);
        GetINIFileName (INIname, TNImode);
        INIData.SetInitialWindowPosition (hwnd, INIname, "Main", TNImode);
        INIData.SetInitialWindowSize (hwnd, INIname, "Main", TNImode);
        OS2.WinShowWindow (hwnd, TRUE);

        OS2.WinProcessDlg(hwnd);
        INIData.StoreWindowPosition (hwnd, INIname, "Main", TNImode);
        INIData.StoreWindowSize (hwnd, INIname, "Main", TNImode);
        OS2.WinDestroyWindow (hwnd);

    END Create;

(**************************************************************************)

PROCEDURE SetThousandsSeparator;

    (* Sets the value of the global character ThousandsSeparator. *)

    VAR cc: OS2.COUNTRYCODE;
        ci: OS2.COUNTRYINFO;
        cbActual: CARDINAL;

    BEGIN
        cc.country := 0;
        cc.codepage := 0;
        IF OS2.DosQueryCtryInfo (SIZE(ci), cc, ci, cbActual) = 0 THEN
            ThousandsSeparator := ci.szThousandsSeparator[0];
        ELSE
            ThousandsSeparator := ' ';
        END (*IF*);
    END SetThousandsSeparator;

(**************************************************************************)

BEGIN
    AlwaysOnTop := FALSE;
    IsPuttingOnTop := FALSE;
    CycleTime := 1000;
    SetThousandsSeparator;
    CreateSemaphore (synch, 0);
    WITH MasterList DO
        CreateLock(access);
        NEW (head);
        head^.nextlist := NIL;  head^.thislist := NIL;
    END (*WITH*);
    CreateSemaphore (OKToConnect, 0);
    CreateSemaphore (AllDone, 0);
    DropConnection := FALSE;  KillFlag := FALSE;
    ShutdownOption := Nul;
END MainDialogue.

