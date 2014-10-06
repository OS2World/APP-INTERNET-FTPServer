(**************************************************************************)
(*                                                                        *)
(*  Text-mode setup for FtpServer                                         *)
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

IMPLEMENTATION MODULE Security;

        (********************************************************)
        (*                                                      *)
        (*           Part of FtpServer Setup program            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 February 1998                 *)
        (*  Last edited:        22 January 2014                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT Strings;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIGet, INIGetTrusted, INIPut, INIPutBinary, ItemSize;

FROM InetUtilities IMPORT
    (* proc *)  ConvertCard, Swap4;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen,
                RemoveVirtualScreen, RemoveScreenGroup, VirtualScreenOf,
                EnableScreenSwitching;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, HighlightOn, HighlightOff, CursorBackward,
                CursorForward, LBAppend, WindowOf, LBCurrent,
                LBDeleteCurrent, LBInsertBefore, ReplaceCurrent;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  CardinalField, ScreenEdit;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor, SetActivePage,
                WriteString, WriteLn, GetKey, GetScreenSize, EditString,
                InsertMode, SaveCursor, ColourSwap;

FROM Keyboard IMPORT
    (* proc *)  PutBack, StuffKeyboardBuffer;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);  CR = CHR(13);  Esc = CHR(27);

TYPE
    AddrPtr = POINTER TO AddrRecord;
    AddrRecord = RECORD
                     previous, next: AddrPtr;
                     address, mask: CARDINAL;
                 END (*RECORD*);

    RecordKind = CARD8[0..1];

    ListPtr = POINTER TO ListRecord;
    ListRecord = RECORD
                     previous, next: ListPtr;
                     allow: BOOLEAN;
                     CASE type: RecordKind OF
                       |  0:  (* end record, no detail needed *)
                       |  1:  (* address and mask *)
                              address, mask: CARDINAL;
                     END (*CASE*);
                 END (*RECORD*);

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* The screen group for our screen page.  *)

    OurGroup: ScreenGroup;

    (* The screen page used by this module. *)

    OurPage: VirtualScreen;

    (* Header and footer windows. *)

    TopBar, BottomBar: Window;

    (* Limit on number of users from the same IP address. *)

    SameIPLimit: CARDINAL;

    (* Handle for our INI file. *)

    hini: HINI;

    (* List head for the allow/exclude list. *)

    MasterListHead: ListPtr;

    (* IP address list listbox height. *)

    BoxHeight: CARDINAL;

    (* An often-needed string constant. *)

    SYSapp: ARRAY [0..4] OF CHAR;

(************************************************************************)
(*          GETTING/PUTTING IP ADDRESS CONTROLS FROM INI FILE           *)
(************************************************************************)

PROCEDURE LoadIPAddressList (IPAllow: BOOLEAN): ListPtr;

    (* Constructs an IP address list from the INI file data.  It's      *)
    (* either the "allow" or the "deny" list, depending on the          *)
    (* Boolean parameter.  This procedure uses the old-format INI data, *)
    (* but when we later save the data we'll save it in the new format. *)

    VAR option: ARRAY [0..7] OF CHAR;  j, size, originalsize: CARDINAL;
        bufptr: POINTER TO ARRAY [0..1023] OF CARDINAL;
        head, current: ListPtr;

    BEGIN
        IF IPAllow THEN
            Strings.Assign ("IPAllow", option);
        ELSE
            Strings.Assign ("IPDeny", option);
        END (*IF*);
        head := NIL;
        IF ItemSize (hini, SYSapp, option, size) THEN
            IF size <> 0 THEN
                ALLOCATE (bufptr, size);
                originalsize := size;
                IF INIGetTrusted (hini, SYSapp, option, bufptr^, size) THEN
                    j := 0;  current := NIL;
                    WHILE size > 0 DO
                        IF current = NIL THEN
                            NEW (head);
                            head^.previous := NIL;
                            current := head;
                        ELSE
                            NEW (current^.next);
                            current^.next^.previous := current;
                            current := current^.next;
                        END (*IF*);
                        current^.next := NIL;
                        current^.allow := IPAllow;
                        current^.type := 1;
                        current^.address := bufptr^[j];  INC(j);
                        current^.mask := bufptr^[j];  INC(j);
                        DEC (size, 2*SIZE(CARDINAL));
                        current^.address := IAND (current^.address, current^.mask);
                    END (*WHILE*);
                END (*IF*);
                DEALLOCATE (bufptr, originalsize);
            END (*IF*);
        END (*IF*);
        RETURN head;
    END LoadIPAddressList;

(************************************************************************)

PROCEDURE LoadMasterList;

    (* Constructs the master allow/deny list from the INI file data.    *)

    VAR option: ARRAY [0..7] OF CHAR;  j, size: CARDINAL;
        bufptr: POINTER TO ARRAY [0..1023] OF LOC;
        current: ListPtr;

    (********************************************************************)

    PROCEDURE Get (VAR (*OUT*) result: ARRAY OF LOC);

        (* Reads a value from bufptr^[j], updates j. *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(result) DO
                result[k] := bufptr^[j];  INC(j);
            END (*FOR*);
        END Get;

    (********************************************************************)

    BEGIN
        Strings.Assign ("IPfilter", option);
        MasterListHead := NIL;
        IF ItemSize (hini, SYSapp, option, size) THEN
            IF size <> 0 THEN
                ALLOCATE (bufptr, size);
                IF INIGetTrusted (hini, SYSapp, option, bufptr^, size) THEN
                    j := 0;  current := NIL;
                    LOOP
                        IF current = NIL THEN
                            NEW (MasterListHead);
                            MasterListHead^.previous := NIL;
                            current := MasterListHead;
                        ELSE
                            NEW (current^.next);
                            current^.next^.previous := current;
                            current := current^.next;
                        END (*IF*);
                        current^.next := NIL;
                        Get (current^.allow);
                        Get (current^.type);
                        IF current^.type > 1 THEN
                            current^.type := 1;
                        END (*IF*);
                        CASE current^.type OF
                          | 0:  EXIT (*LOOP*);
                          | 1:  Get (current^.address);
                                Get (current^.mask);
                                current^.address := IAND (current^.address, current^.mask);
                        END (*CASE*);
                    END (*LOOP*);
                END (*IF*);
                DEALLOCATE (bufptr, size);
            END (*IF*);
        ELSE

            (* IPfilter data do not exist, so load the data from the    *)
            (* old-format lists.                                        *)

            MasterListHead := LoadIPAddressList (TRUE);
            current := MasterListHead;
            IF current = NIL THEN
                MasterListHead := LoadIPAddressList (FALSE);
            ELSE
                WHILE current^.next <> NIL DO
                    current := current^.next;
                END (*WHILE*);
                current^.next := LoadIPAddressList (FALSE);
                IF current^.next <> NIL THEN
                    current^.next^.previous := current;
                END (*IF*);
            END (*IF*);
        END (*IF*);

        (* Ensure that the list is terminated with a catch-all entry. *)

        current := MasterListHead;
        IF current = NIL THEN
            NEW (current);  current^.previous := NIL;
            MasterListHead := current;
        ELSE
            WHILE current^.next <> NIL DO
                current := current^.next;
            END (*WHILE*);
            IF current^.type = 0 THEN
                current := NIL;
            ELSE
                NEW (current^.next);
                current^.next^.previous := current;
                current := current^.next;
            END (*IF*);
        END (*IF*);
        IF current <> NIL THEN
            current^.next := NIL;
            current^.allow := TRUE;
            current^.type := 0;
        END (*IF*);

    END LoadMasterList;

(************************************************************************)

PROCEDURE StoreMasterList (head: ListPtr);

    (* Stores the master allow/deny list to the INI file. *)

    VAR option: ARRAY [0..7] OF CHAR;  j, size: CARDINAL;
        bufptr: POINTER TO ARRAY [0..1023] OF LOC;
        current: ListPtr;

    (********************************************************************)

    PROCEDURE Put (value: ARRAY OF LOC);

        (* Stores a value into bufptr^[j], updates j. *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(value) DO
                bufptr^[j] := value[k];  INC(j);
            END (*FOR*);
        END Put;

    (********************************************************************)

    BEGIN
        Strings.Assign ("IPfilter", option);
        size := 0;  current := head;
        WHILE current <> NIL DO
            INC (size, SIZE(BOOLEAN) + SIZE(RecordKind));
            CASE current^.type OF
              |  0:  (* nothing more to add *)
              |  1:  (* address and mask *)
                     INC (size, 2*SIZE(CARDINAL));
            END (*CASE*);
            current := current^.next;
        END (*WHILE*);
        IF size = 0 THEN bufptr := NIL
        ELSE ALLOCATE (bufptr, size);
        END (*IF*);
        current := head;  j := 0;
        WHILE current <> NIL DO
            Put (current^.allow);  Put (current^.type);
            CASE current^.type OF
              |  0:  (* nothing more to add *)
              |  1:  (* address and mask *)
                     Put (current^.address);
                     Put (current^.mask);
            END (*CASE*);
            current := current^.next;
        END (*WHILE*);
        INIPutBinary (hini, SYSapp, "IPfilter", bufptr^, size);
        IF bufptr <> NIL THEN
            DEALLOCATE (bufptr, size);
        END (*IF*);
    END StoreMasterList;

(************************************************************************)
(*          CONVERTING AN ADDRESS RECORD TO HUMAN-READABLE FORM         *)
(************************************************************************)

PROCEDURE CardToDotted (value: ARRAY OF LOC;  VAR (*OUT*) text: ARRAY OF CHAR;
                                              VAR (*INOUT*) place: CARDINAL);

    (* Converts the four-byte value to a string of the form a.b.c.d,    *)
    (* starting at text[place].  On return, place is the first location *)
    (* in the text array that hasn't been used.                         *)

    VAR j: [0..2];

    BEGIN
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,value[j]), text, place);
            text[place] := '.';  INC(place);
        END (*FOR*);
        ConvertCard (CAST(CARD8,value[3]), text, place);
        IF place <= HIGH(text) THEN
            text[place] := Nul;
        END (*IF*);
    END CardToDotted;

(************************************************************************)

PROCEDURE CardinalToDotted (value: CARDINAL;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* Converts the value to a string of the form a.b.c.d    *)

    VAR place: CARDINAL;

    BEGIN
        place := 0;
        CardToDotted (value, text, place);
    END CardinalToDotted;

(************************************************************************)

PROCEDURE AddrRecordToText (p: ListPtr;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* Converts p^ to a text string. *)

    CONST MaskPos = 25;

    VAR place: CARDINAL;

    BEGIN
        IF p^.allow THEN Strings.Assign ("Allow   ", text)
        ELSE Strings.Assign ("Refuse  ", text)
        END (*IF*);
        IF p^.type = 0 THEN
            Strings.Append ("all", text);
            IF p^.previous <> NIL THEN
                Strings.Append (" others", text);
            END (*IF*);
        ELSE
            place := 8;
            CardToDotted (p^.address, text, place);
            WHILE place < MaskPos DO
                text[place] := ' ';  INC (place);
            END (*WHILE*);
            CardToDotted (p^.mask, text, place);
        END (*IF*);
    END AddrRecordToText;

(************************************************************************)
(*             CONVERTING A STRING TO AN ADDRESS RECORD                 *)
(************************************************************************)

PROCEDURE SkipBlanks (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL);

    (* Increases j until text[j] is not a space character. *)

    BEGIN
        WHILE text[j] = ' ' DO INC(j) END(*WHILE*);
    END SkipBlanks;

(************************************************************************)

PROCEDURE DottedToCard (text: ARRAY OF CHAR;
                                 VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts a dotted-quad text string to a cardinal value. *)

    VAR result: CARDINAL;  j: [0..3];

    (********************************************************************)

    PROCEDURE ConvertOneComponent(): CARDINAL;

        TYPE CharSet = SET OF CHAR;
        CONST Digits = CharSet {'0'..'9'};

        VAR answer: CARDINAL;

        BEGIN
            answer := 0;
            WHILE text[pos] IN Digits DO
                answer := 10*answer + (ORD(text[pos]) - ORD('0'));
                INC (pos);
            END (*WHILE*);
            RETURN answer;
        END ConvertOneComponent;

    (********************************************************************)

    BEGIN
        result := 0;
        FOR j := 0 TO 3 DO
            result := 256*result + ConvertOneComponent();
            IF text[pos] = '.' THEN INC(pos) END(*IF*);
        END (*FOR*);
        RETURN Swap4(result);
    END DottedToCard;

(************************************************************************)

PROCEDURE StringToCard (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL): CARDINAL;

    (* Picks up an IP address, starting at text[j], updates j. *)

    VAR result: CARDINAL;

    BEGIN
        IF text[j] = '[' THEN INC(j) END(*IF*);
        result := DottedToCard (text, j);
        IF text[j] = ']' THEN INC(j) END(*IF*);
        RETURN result;
    END StringToCard;

(************************************************************************)

PROCEDURE StringToIPAddress (text: ARRAY OF CHAR): CARDINAL;

    (* Picks up an IP address. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        RETURN StringToCard (text, j);
    END StringToIPAddress;

(************************************************************************)

PROCEDURE StringToAddrRecord (string: ARRAY OF CHAR;  result: ListPtr);

    (* Interprets string, result stored in result^. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;  result^.type := 1;
        SkipBlanks (string, j);
        result^.address := StringToCard (string, j);
        SkipBlanks (string, j);
        IF (j <= HIGH(string)) AND (string[j] <> Nul) THEN
            result^.mask := StringToCard (string, j);
        ELSE
            result^.mask := MAX(CARDINAL);
        END (*IF*);
    END StringToAddrRecord;

(************************************************************************)
(*                     EDITING AN ADDRESS RECORD                        *)
(************************************************************************)

PROCEDURE EditAddrRecord (LB: ListBox;  p: ListPtr);

    (* Allows screen editing of the address and mask values. *)

    VAR text: ARRAY [0..127] OF CHAR;  w, bottombar: Window;  ch: CHAR;
        row, col: CARDINAL;  OurPage: VirtualScreen;

    BEGIN
        EnableScreenSwitching (FALSE);
        HighlightOff (LB);
        AddrRecordToText (p, text);
        Strings.Delete (text, 0, 8);
        w := WindowOf(LB);
        OurPage := VirtualScreenOf (w);
        SaveCursor (w, row, col);
        OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (bottombar, OurPage);
        LOOP
            WriteLn (bottombar);  WriteString (bottombar, " A allow  R refuse");
            ColourSwap (w, row, col, 7);
            ch := GetKey(w);

            (* Valid keys at this point are A, R, return, esc, cursor   *)
            (* up or down, cursor right or left.                        *)

            IF CAP(ch) = 'A' THEN p^.allow := TRUE;
            ELSIF CAP(ch) = 'R' THEN p^.allow := FALSE;
            ELSIF (ch = CR) OR (ch = Esc) THEN EXIT(*LOOP*);
            ELSIF ch = Nul THEN
                ch := GetKey(w);
                IF (ch = "H") OR (ch = "P") THEN   (* cursor up or down *)
                    PutBack (ch);  PutBack (Nul);
                    EXIT (*LOOP*);
                ELSIF ch = "M" THEN   (* cursor right - go to next step *)
                ELSIF ch = "K" THEN (* cursor left - go back to looking for A/R *)
                ELSE
                    PutBack (ch);  PutBack (Nul);
                END (*IF*);
            ELSE
                PutBack (ch);
            END (*IF*);
            SetCursor (w, row, col);
            IF p^.allow THEN WriteString (w, "Allow   ")
            ELSE WriteString (w, "Refuse  ")
            END (*IF*);
            ColourSwap (w, row, col, 7);

            (* Having done with the allow/refuse part, edit the rest    *)
            (* of the record, unless it's an end record.                *)

            IF p^.type <> 0 THEN
                WriteLn (bottombar);
                WriteString (bottombar, " Edit IP address and mask");
                ColourSwap (w, row, col, 7);
                SetCursor (w, row, col+8);
                EditString (w, text, 128, 60);
                StringToAddrRecord (text, p);
                p^.address := IAND (p^.address, p^.mask);
            END (*IF*);

            (* Cursor left keeps us in the loop, anything else finishes *)
            (* the job.  Exception: for an end record, it's only A/R    *)
            (* that keep us in the loop.                                *)

            ch := GetKey(w);
            ColourSwap (w, row, col, 7);
            IF p^.type = 0 THEN
                PutBack (ch);
                IF (CAP(ch) <> 'A') AND (CAP(ch) <> 'R') THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF ch = Nul THEN
                ch := GetKey(w);
                IF ch <> "K" THEN
                    PutBack (ch);  PutBack (Nul);
                    EXIT (*LOOP*);
                END (*IF*);
                ColourSwap (w, row, col, 7);
            ELSE
                PutBack (ch);
                EXIT (*LOOP*);
            END (*IF*);

        END (*LOOP*);

        CloseWindow (bottombar);
        HighlightOn (LB);
        EnableScreenSwitching (TRUE);

    END EditAddrRecord;

(************************************************************************)
(*                    EDITING THE ALLOW/REFUSE LISTS                    *)
(************************************************************************)

PROCEDURE IPListEditor (w: Window;  LB: ListBox;
                                VAR (*INOUT*) current: ListPtr);

    (********************************************************************)

    PROCEDURE UpARow(): BOOLEAN;
        BEGIN
            IF (current = NIL) OR (current^.previous = NIL) THEN RETURN FALSE;
            ELSE
                current := current^.previous;
                RETURN CursorBackward (LB);
            END (*IF*);
        END UpARow;

    (********************************************************************)

    PROCEDURE DownARow(): BOOLEAN;
        BEGIN
            IF (current = NIL) OR (current^.next = NIL)
                        OR (current^.type = 0) THEN RETURN FALSE
            ELSE
                current := current^.next;
                EVAL (CursorForward(LB));
                RETURN TRUE;
            END (*IF*);
        END DownARow;

    (********************************************************************)

    VAR ch: CHAR;  text: ARRAY [0..79] OF CHAR;
        j: CARDINAL;  q: ListPtr;

    BEGIN
        HighlightOn (LB);
        LOOP
            ch := GetKey(w);
            IF CAP(ch) = 'E' THEN                    (* E = edit *)
                IF current <> NIL THEN
                    EditAddrRecord (LB, current);
                    AddrRecordToText (current, text);
                    ReplaceCurrent (LB, text);
                END (*IF*);
            ELSIF CAP(ch) = 'I' THEN                    (* I = insert *)
                NEW (q);
                WITH q^ DO
                    previous := NIL;  next := current;
                    type := 1;  allow := TRUE;
                    address := 0;  mask := MAX(CARDINAL);
                END (*WITH*);
                IF current <> NIL THEN
                    q^.previous := current^.previous;
                    current^.previous := q;
                END (*IF*);
                IF q^.previous = NIL THEN
                    MasterListHead := q;
                ELSE
                    q^.previous^.next := q;
                END (*IF*);
                AddrRecordToText (q, text);
                LBInsertBefore (LB, text);
                current := q;
                PutBack ('E');
            ELSIF CAP(ch) = 'P' THEN                    (* P = promote *)
                IF (current^.previous <> NIL) AND (current^.next <> NIL) THEN
                    q := current^.previous;
                    AddrRecordToText (q, text);
                    ReplaceCurrent (LB, text);
                    AddrRecordToText (current, text);
                    IF q^.previous = NIL THEN
                        MasterListHead := current;
                    ELSE
                        q^.previous^.next := current;
                    END (*IF*);
                    current^.next^.previous := q;
                    current^.previous := q^.previous;
                    q^.next := current^.next;
                    q^.previous := current;
                    current^.next := q;
                    IF CursorBackward (LB) THEN
                        ReplaceCurrent (LB, text);
                    END (*IF*);
                END (*IF*);
            ELSIF CAP(ch) = 'X' THEN                    (* X = exit *)
                PutBack (ch);
                EXIT (*LOOP*);
            ELSIF ch = Nul THEN
                ch := GetKey(w);
                IF (ch = "K") OR (ch = "M") THEN        (* cursor left/right *)
                    PutBack (ch);  PutBack (Nul);
                    EXIT (*LOOP*);
                ELSIF ch = "H" THEN                     (* cursor up *)
                    IF NOT UpARow() THEN
                        PutBack ("H");  PutBack (Nul);
                        EXIT (*LOOP*);
                    END (*IF*);
                ELSIF ch = "P" THEN                     (* cursor down *)
                    EVAL (DownARow());
                ELSIF ch = "G" THEN                     (* home *)
                    WHILE UpARow() DO
                    END (*WHILE*);
                ELSIF ch = "O" THEN                     (* end *)
                    WHILE DownARow() DO
                    END (*WHILE*);
                ELSIF ch = "I" THEN                     (* page up *)
                    j := BoxHeight-1;
                    WHILE (j > 0) AND UpARow() DO
                        DEC (j);
                    END (*WHILE*);
                ELSIF ch = "Q" THEN                     (* page down *)
                    j := BoxHeight-1;
                    WHILE (j > 0) AND DownARow() DO
                        DEC (j);
                    END (*WHILE*);
                ELSIF ch = 'S' THEN                     (* Del = delete *)
                    IF (current <> NIL) AND (current^.type <> 0) THEN
                        q := current;
                        IF current^.previous = NIL THEN
                            MasterListHead := current^.next;
                        ELSE
                            current^.previous^.next := current^.next;
                        END (*IF*);
                        IF current^.next = NIL THEN
                            current := current^.previous;
                        ELSE
                            current^.next^.previous := current^.previous;
                            current := current^.next;
                        END (*IF*);
                        DISPOSE (q);
                        LBDeleteCurrent (LB);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
        HighlightOff (LB);
    END IPListEditor;

(************************************************************************)
(*                          MAIN EDITING TASK                           *)
(************************************************************************)

PROCEDURE SecurityEditor;

    (* Runs as a separate task.  This is for allowing the user to       *)
    (* edit the "same IP limit" and the IP permit/deny lists.           *)

    CONST BoxTop = 6;  BoxLeft = 2;  BoxWidth = 73;

    VAR w, bottombar: Window;  R: Structure;
        ch: CHAR;  EditIPList: BOOLEAN;
        IPControl: RECORD
                       win: Window;
                       LB: ListBox;
                       current: ListPtr;
                   END (*RECORD*);
        text: ARRAY [0..39] OF CHAR;

    BEGIN
        IF NOT INIGet (hini, SYSapp, "SameIPLimit", SameIPLimit) THEN
            SameIPLimit := MAX(CARDINAL);
        END (*IF*);;

        OpenWindowHidden (w, black, white, 2, 4, 10, 70, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);
        SetCursor (w, 1, 2);  WriteString (w, "Same IP limit");
        R := CardinalField (SameIPLimit, 1, 32, 12);

        BoxHeight := ScreenRows - BoxTop - 6;
        WITH IPControl DO
            OpenWindowHidden (win, black, white, BoxTop, BoxTop+BoxHeight+3,
                 BoxLeft, BoxLeft+BoxWidth+2, noframe, nodivider);
            MapToVirtualScreen (win, OurPage);
            InsertMode (win, FALSE);
            SetCursor (win, 1, 6);
            WriteString (win, "CONTROLS ON CLIENT IP ADDRESS");
            LB := CreateListBox (win, 3, 1, BoxHeight, BoxWidth);
            current := MasterListHead;
            WHILE current <> NIL DO
                AddrRecordToText (current, text);
                LBAppend (LB, text);
                current := current^.next;
            END (*WHILE*);
            current := MasterListHead;
        END (*WITH*);

        LOOP
            ScreenEdit (w, R, EditIPList);

            (* Check the character that took us off the edge.  If it's *)
            (* "cursor down", proceed to editing the IP lists.         *)

            IF EditIPList THEN
                PutBack ('X');
            ELSE
                ch := GetKey (w);
                IF ch = Nul THEN
                    ch := GetKey (w);
                    EditIPList := ch = 'P';
                END (*IF*);
            END (*IF*);

            IF EditIPList THEN
                OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 48, noframe, nodivider);
                MapToVirtualScreen (bottombar, OurPage);
                WriteString (bottombar, " E edit  I insert  P promote  Del delete  X exit");
                LOOP
                    WITH IPControl DO
                        IPListEditor (win, LB, current);
                    END (*WITH*);

                    (* Check the character that took us off the edge.  *)
                    (* Cursor up means go back to editing the R        *)
                    (* structure.                                      *)

                    ch := GetKey (w);
                    IF CAP(ch) = 'X' THEN                       (* X = exit *)
                        SetActivePage (0);
                        StuffKeyboardBuffer (CHR(27));
                    ELSIF ch = Nul THEN
                        ch := GetKey (w);
                        IF ch = 'H' THEN EXIT (*LOOP*);
                        END (*IF*);
                    END (*IF*);

                END (*LOOP*);

                CloseWindow (bottombar);

            END (*IF*);

        END (*LOOP*);

    END SecurityEditor;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

PROCEDURE StartSecurityEditor(): BOOLEAN;

    (* Initialisation of the Security page. *)

    BEGIN
        hini := OurINIHandle();
        LoadMasterList;
        RETURN CreateTask (SecurityEditor, 2, "Security editor");
    END StartSecurityEditor;

(************************************************************************)

PROCEDURE CreateHeadings;

    BEGIN
        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);
        WriteString (TopBar, "    FTPSERVER SECURITY SETUP");
        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, OurPage);
        WriteString (BottomBar, " Esc exit");
        SetCursor (BottomBar, 0, 55);
        WriteString (BottomBar, "F4,F5 previous/next page");
    END CreateHeadings;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    SYSapp := "$SYS";
    GetScreenSize (ScreenRows, dummy);
    OurGroup := CreateScreenGroup (1);
    OurPage := CreateVirtualScreen (OurGroup);
    CreateHeadings;
FINALLY
    INIPut (hini, SYSapp, "SameIPLimit", SameIPLimit);
    CloseWindow (BottomBar);
    StoreMasterList (MasterListHead);
    CloseWindow (TopBar);
    RemoveVirtualScreen (OurPage);
    RemoveScreenGroup (OurGroup);
END Security.

