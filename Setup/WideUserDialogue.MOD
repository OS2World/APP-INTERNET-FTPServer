(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE WideUserDialogue;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*                 User page of the notebook                    *)
        (*                                                              *)
        (*        Started:        14 September 2017                     *)
        (*        Last edited:    6 October 2017                        *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, CAST, ADR;

IMPORT OS2, DID, Strings, Remote, CommonSettings;

FROM FSUINI IMPORT
    (* proc *)  OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIFetch, INIGetString;

FROM QuickSortModule IMPORT
    (* type *)  CompareProc,
    (* proc *)  QuickSort;

FROM MiscPMOS IMPORT
    (* proc *)  Compare;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    field = (fuser, ftype, flogin);
    FieldwidthType = ARRAY field OF CARDINAL;

CONST
    Nul = CHR(0);
    NameLength = 256;
    Fieldwidth = FieldwidthType {48, 16, 32};
    (*Fieldwidth = FieldwidthType {8, 16, 8};       (* while testing *)*)

TYPE
    UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser, NormalUser,
                    Manager, UserTemplate);
    NameString = ARRAY [0..NameLength-1] OF CHAR;

    UserRecord = RECORD
                    name: NameString;
                    type: ARRAY [0..Fieldwidth[ftype]-1] OF CHAR;
                    lastlogin: ARRAY [0..Fieldwidth[flogin]-1] OF CHAR;
                 END (*RECORD*);

    RecordArray = ARRAY [0..MAX(CARDINAL) DIV 1024] OF UserRecord;

VAR
    ChangeInProgress: BOOLEAN;

    (* The number of list entries. *)

    UserCount: CARDINAL;

    (* Pointer to the large array of user information. *)

    UserData: POINTER TO RecordArray;

    (* Are we sorting in descending order?  *)

    Descending: ARRAY field OF BOOLEAN;

(************************************************************************)
(*                            FOR TESTING                               *)
(************************************************************************)

PROCEDURE CardToHex (N: CARDINAL;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* For debugging.  32-bit cardinal to 8-char hexadecimal. *)

    VAR j, digit: CARDINAL;

    BEGIN
        text[8] := Nul;
        FOR j := 7 TO 0 BY -1 DO
            digit := N MOD 16;
            N := N DIV 16;
            IF digit < 10 THEN
                text[j] := CHR(ORD('0') + digit);
            ELSE
                text[j] := CHR(ORD('A') -10 + digit);
            END (*IF*);
        END (*FOR*);
    END CardToHex;

(************************************************************************)

PROCEDURE WriteHex (hwnd: OS2.HWND;  to: CARDINAL;  N: CARDINAL);

    (* For debugging.  Write N in hexadecimal to debug1 or debug2 field. *)

    VAR text: ARRAY [0..8] OF CHAR;
        w: OS2.HWND;

    BEGIN
        CardToHex (N, text);
        IF to = 1 THEN
            w := OS2.WinWindowFromID (hwnd, DID.debug1);
        ELSE
            w := OS2.WinWindowFromID (hwnd, DID.debug2);
        END (*IF*);
        OS2.WinSetWindowText (w, text);
    END WriteHex;

(************************************************************************)
(*                        DISPLAY DATA IN LISTBOX                       *)
(************************************************************************)

PROCEDURE ShowValues (hwnd: OS2.HWND);

    (* Formats UserData details and displays them in the listbox.  *)

    VAR displayline: NameString;

    (********************************************************************)

    PROCEDURE PadTo (N: CARDINAL);

        (* Adds trailing spaces to displayline to get to position N. *)

        VAR size: CARDINAL;

        BEGIN
            size := Strings.Length (displayline);
            IF size >= N THEN
                size := N - 1;
                displayline[size] := Nul;
            END (*IF*);
            WHILE size < N DO
                Strings.Append (" ", displayline);
                INC (size);
            END (*WHILE*);
            displayline[size] := Nul;
        END PadTo;

    (********************************************************************)

    VAR j: CARDINAL;

    BEGIN
        OS2.WinSendDlgItemMsg (hwnd, DID.WUlist, OS2.LM_DELETEALL, NIL, NIL);
        IF UserCount > 0 THEN
            FOR j := 0 TO UserCount-1 DO

                displayline := UserData^[j].name;
                PadTo (Fieldwidth[fuser]);
                Strings.Append (UserData^[j].type, displayline);
                PadTo (Fieldwidth[fuser] + Fieldwidth[ftype]);
                Strings.Append (UserData^[j].lastlogin, displayline);

                OS2.WinSendDlgItemMsg (hwnd, DID.WUlist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(j), ADR(displayline));

            END (*FOR*);
        END (*IF*);
    END ShowValues;

(************************************************************************)
(*                               SORTING                                *)
(************************************************************************)

PROCEDURE CompareUsername (p, q: ADDRESS): BOOLEAN;

    (* Returns TRUE iff the p^ >= q^, if we are sorting in ascending    *)
    (* order.  For descending order, returns TRUE iff p^ <= q^.         *)

    VAR p0, q0: POINTER TO UserRecord;
        val: INTEGER;

    BEGIN
        p0 := p;  q0 := q;
        val := Compare (p0^.name, q0^.name);
        IF Descending[fuser] THEN val := -val END(*IF*);
        RETURN val >= 0;
    END CompareUsername;

(************************************************************************)

PROCEDURE CompareType (p, q: ADDRESS): BOOLEAN;

    (* Returns TRUE iff the p^ >= q^, if we are sorting in ascending    *)
    (* order.  For descending order, returns TRUE iff p^ <= q^.         *)

    VAR p0, q0: POINTER TO UserRecord;
        val: INTEGER;

    BEGIN
        p0 := p;  q0 := q;
        val := Compare (p0^.type, q0^.type);
        IF Descending[ftype] THEN val := -val END(*IF*);
        RETURN val >= 0;
    END CompareType;

(************************************************************************)

PROCEDURE CompareLastLogin (p, q: ADDRESS): BOOLEAN;

    (* Returns TRUE iff the p^ >= q^, if we are sorting in ascending    *)
    (* order.  For descending order, returns TRUE iff p^ <= q^.         *)

    VAR p0, q0: POINTER TO UserRecord;
        val: INTEGER;

    BEGIN
        p0 := p;  q0 := q;

        (* Beware of "never" special case. *)

        IF p0^.lastlogin = "never" THEN
            IF q0^.lastlogin = "never" THEN
                val := 0;
            ELSE
                val := -1;
            END (*IF*);
        ELSIF q0^.lastlogin = "never" THEN
            val := +1;
        ELSE
            val := Compare (p0^.lastlogin, q0^.lastlogin);
        END (*IF*);

        IF Descending[flogin] THEN val := -val END(*IF*);
        RETURN val >= 0;

    END CompareLastLogin;

(************************************************************************)

PROCEDURE DoSort (f: field);

    (* Sorts the list using the given field as the sort key.  *)

    VAR Compare: CompareProc;

    BEGIN
        Descending[f] := NOT Descending[f];
        IF f = fuser THEN
            Compare := CompareUsername;
        ELSIF f = ftype THEN
            Compare := CompareType;
        ELSE
            Compare := CompareLastLogin;
        END (*IF*);
        QuickSort (UserData^, UserCount-1, SIZE(UserRecord), Compare);
    END DoSort;

(************************************************************************)
(*             INITIAL LOAD AND FINAL STORE OF THE USER DATA            *)
(************************************************************************)

PROCEDURE LoadUserData (lbox: OS2.HWND);

    (* Fills the global UserData structure, by taking the usernames     *)
    (* from the listbox whose handle is lbox, and the other data from   *)
    (* INI files.  Note that there can be multiple INI files depending  *)
    (* on the 'HashMax' INI value.                                      *)

    VAR j, N: CARDINAL;
        Active: BOOLEAN;
        category: UserCategory;

    BEGIN
        (* Work out how many users there are in the list. *)

        N := CAST(CARDINAL, OS2.WinSendMsg (lbox, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        UserCount := N;
        IF N = 0 THEN RETURN END(*IF*);

        (* Fill in the UserData details. *)

        ALLOCATE (UserData, N*SIZE(UserRecord));
        FOR j := 0 TO N-1 DO
            WITH UserData^[j] DO
                EVAL (OS2.WinSendMsg (lbox, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2SHORT(j, NameLength), ADR(name)));
                OpenINIForUser (name);

                (* Get and encode the user type. *)

                IF (NOT INIFetch (name, "Category", category))
                               OR (category = NoSuchUser) THEN
                    category := NormalUser;
                END (*IF*);
                IF NOT INIFetch (name, "Active", Active) THEN
                    Active := TRUE;
                END (*IF*);

                CASE category OF
                     NoSuchUser:        type := "invalid";
                  |  NoPasswordNeeded:  type := "no password";
                  |  GuestUser:         type := "guest";
                  |  NormalUser:        type := "user";
                  |  Manager:           type := "manager";
                  |  UserTemplate:      type := "template";
                   ELSE
                                        type := "unknown";
                END (*CASE*);
                IF NOT Active THEN
                    Strings.Append (", inactive", type);
                END (*IF*);

                (* Get the last login timestamp. *)

                IF NOT INIGetString (name, "LastLogin", lastlogin) THEN
                    lastlogin := "never";
                END (*IF*);

            END (*WITH*);

            CloseINIFile;

        END (*FOR*);

    END LoadUserData;

(************************************************************************)

PROCEDURE StoreUserData (lbox: OS2.HWND);

    (* Stores the usernames back into the lbox.  These will be the      *)
    (* same names as were originally loaded, but in a different order.  *)

    VAR j, N: CARDINAL;

    BEGIN
        N := UserCount;
        IF N > 0 THEN
            FOR j := 0 TO N-1 DO
                EVAL(OS2.WinSendMsg(lbox, OS2.LM_SETITEMTEXT,
                          OS2.MPFROMLONG(j), UserData^[j].name));
            END (*FOR*);
            DEALLOCATE (UserData, N*SIZE(UserRecord));
        END (*IF*);
    END StoreUserData;

(************************************************************************)
(*                           DIALOGUE PROCEDURE                         *)
(************************************************************************)

PROCEDURE SetListboxFont (hwnd: OS2.HWND;
                            VAR (*IN*) fontname: CommonSettings.FontName);

    (* Sets the font for the listbox. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (OS2.WinWindowFromID (hwnd, DID.WUlist),
                                  OS2.PP_FONTNAMESIZE, bufsize, fontname);
    END SetListboxFont;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR font: CommonSettings.FontName;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinSetWindowText (hwnd, "Sort users");
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CloneUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), FALSE);
            Strings.Assign ("8.Courier", font);
            SetListboxFont (hwnd, font);
            ShowValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_COMMAND THEN

            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.WUdone:
                   OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                   RETURN NIL;

              | DID.namebutton:
                    DoSort(fuser);
                    ShowValues (hwnd);
                    RETURN NIL;

              | DID.typebutton:
                    DoSort(ftype);
                    ShowValues (hwnd);
                    RETURN NIL;

              | DID.lastloginbutton:
                    DoSort(flogin);
                    ShowValues (hwnd);
                    RETURN NIL;

            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*CASE*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(************************************************************************)

PROCEDURE SortDlg (owner, list: OS2.HWND);

    (* Run dialogue to allow sorting of the usernames in list.  *)

    VAR hwnd: OS2.HWND;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        LoadUserData (list);
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.WideUserDialogue,        (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "WideUserDialogue");

        OS2.WinProcessDlg(hwnd);
        StoreUserData (list);

        Remote.StoreWindowPosition (hwnd, "WideUserDialogue", TRUE);
        OS2.WinDestroyWindow (hwnd);

    END SortDlg;

(************************************************************************)

VAR f: field;

BEGIN
    ChangeInProgress := FALSE;
    FOR f := MIN(field) TO MAX(field) DO
        Descending[f] := TRUE;
    END (*FOR*);
END WideUserDialogue.

