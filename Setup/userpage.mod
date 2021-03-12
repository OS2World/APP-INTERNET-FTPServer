(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE UserPage;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*                 User page of the notebook                    *)
        (*                                                              *)
        (*        Started:        11 October 1999                       *)
        (*        Last edited:    16 December 2020                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, Strings, DID, BasicPage, EditUser, CommonSettings, WideUserDialogue, WU2Dialogue;

FROM FSUINI IMPORT
    (* proc *)  SetHashMax, IsMultiFileMode, ChangeINIFilename, RestoreINIFilename,
                TNImode, OpenINIFile, OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  ItemSize, INIFetch, INIGetCard, INIPut,
                GetStringList, NextString, CloseStringList, INIDeleteApp;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ConvertCard;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

TYPE
    NameString = ARRAY [0..NameLength-1] OF CHAR;

VAR
    pagehandle: OS2.HWND;
    UserCount: CARDINAL;
    ChangeInProgress: BOOLEAN;

(************************************************************************)

(*
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
*)

(************************************************************************)

PROCEDURE NumberOfUsers (): CARDINAL;

    (* Returns the number of users. *)

    BEGIN
        RETURN UserCount;
    END NumberOfUsers;

(************************************************************************)

PROCEDURE DisplayUserCount (hwnd: OS2.HWND);

    (* Displays the value of global variable UserCount. *)

    VAR pos: CARDINAL;  buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        buffer := "Total ";
        pos := LENGTH(buffer);
        ConvertCard (UserCount, buffer, pos);
        buffer[pos] := Nul;
        OS2.WinSetDlgItemText (hwnd, DID.UserCount, buffer);
    END DisplayUserCount;

(************************************************************************)
(*                   LOADING AND STORING THE LIST OF NAMES              *)
(************************************************************************)

TYPE NameList = POINTER TO
                    RECORD
                        next:   NameList;
                        seqnum: CARD16;
                        name:   NameString;
                    END (*RECORD*);

(************************************************************************)

PROCEDURE SortList (VAR (*INOUT*) list: NameList;  N: CARDINAL);

    (* Sorts list of N elements in increasing order of seqnum.  *)

    VAR j: CARDINAL;
        list2, prev, current: NameList;

    BEGIN
        IF N < 2 THEN
            RETURN;
        END (*IF*);

        (* Split the list into two sublists. *)

        current := list;  prev := NIL;
        FOR j := 0 TO (N-2) DIV 2 DO
            prev := current;
            current := current^.next;
        END (*FOR*);
        list2 := current;
        prev^.next := NIL;

        (* Sort the two sublists. *)

        SortList (list, N DIV 2);
        SortList (list2, N - N DIV 2);

        (* Merge the two sorted lists. *);

        prev := NIL;
        current := list;
        WHILE list2 <> NIL DO
            WHILE (current <> NIL) AND (current^.seqnum < list2^.seqnum) DO
                prev := current;
                current := current^.next;
            END (*WHILE*);

            IF prev = NIL THEN
                list := list2;
            ELSE
                prev^.next := list2;
            END (*IF*);
            prev := list2;
            list2 := current;
            current := prev^.next;

        END (*WHILE*);

    END SortList;

(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the user list with data from the INI files.  Note that     *)
    (* there can be multiple INI files depending on the 'HashMax' INI   *)
    (* value.  The order in which we will put the entries into the      *)
    (* listbox will depend on recorded sequence numbers, which          *)
    (* unfortunately adds to the complexity of this procedure.          *)

    VAR D: DirectoryEntry;
        state: StringReadState;
        username:  NameString;
        mask: ARRAY [0..127] OF CHAR;
        N: CARDINAL;
        found, MultiFileMode: BOOLEAN;
        list, current, next, tail: NameList;

    BEGIN
        UserCount := 0;
        list := NIL;  tail := NIL;
        OpenINIFile;
        MultiFileMode := INIGetCard ('$SYS', 'HashMax', N) AND (N > 0);
        SetHashMax (N);
        IF MultiFileMode THEN
            CloseINIFile;
            IF TNImode() THEN
                mask := "FTPD*.TNI";
            ELSE
                mask := "FTPD*.INI";
            END (*IF*);
            found := FirstDirEntry (mask, FALSE, FALSE, TRUE, D);
        ELSE
            found := TRUE;
        END (*IF*);

        (* Each time around this loop we process one INI file. *)

        WHILE found DO

            IF MultiFileMode THEN
                ChangeINIFilename (D.name);
                OpenINIFile;
            END (*IF*);

            (* Collect all the usernames in this file. *)

            GetStringList ('', '', state);
            LOOP
                NextString (state, username);
                IF username[0] = Nul THEN

                    EXIT (*LOOP*);

                ELSIF (username[0] <> '$') AND (username[0] <> '?') THEN

                    (* Add the name to our list. *)

                    INC (UserCount);
                    NEW (current);
                    current^.next := NIL;
                    Strings.Assign (username, current^.name);

                    (* Get sequence number. *)

                    IF NOT INIFetch (current^.name, "seqnum", current^.seqnum) THEN
                        current^.seqnum := MAX(CARD16);
                    END (*IF*);

                    IF tail = NIL THEN
                        list := current;
                    ELSE
                        tail^.next := current;
                    END (*IF*);
                    tail := current;

                END (*IF*);

            END (*LOOP*);

            CloseStringList (state);
            CloseINIFile;
            found := MultiFileMode AND NextDirEntry (D);

        END (*WHILE*);

        IF MultiFileMode THEN

            DirSearchDone (D);

            (* Restore the original INI file name. *)

            RestoreINIFilename;

        END (*IF*);

        (* At this point we have collected all usernames in all of our  *)
        (* INI files, but we still haven't put them into the listbox.   *)
        (* That has been delayed until now because we want to look up   *)
        (* sequence numbers to work out the order.                      *)

        (* It looks as if the listbox doesn't like out-of-order         *)
        (* insertions, so we need to sort the list before doing the     *)
        (* listbox insertions.  Note that the sort will put all entries *)
        (* without sequence numbers at the end of the list.             *)

        SortList (list, UserCount);

        (* Now that we have entries in the correct order, put them in   *)
        (* the listbox.  The sequence numbers are no longer needed,     *)
        (* because they will be recalculated at a later stage.          *)

        current := list;
        WHILE current <> NIL DO
            next := current^.next;
            Strings.Assign (current^.name, username);
            DISPOSE (current);

            (* Add name to the listbox. *)

            OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_END), ADR(username));
            current := next;
        END (*WHILE*);

        (* By now the list should be empty. *)

    END LoadValues;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Most of the INI file updating is done as part of the button      *)
    (* processing, but we take the opportunity to assign user sequence  *)
    (* numbers as Setup is about to exit.  That will ensure that the    *)
    (* next time we run setup the order will be unchanged.              *)

    CONST Skip = MAX(CARD16);

    VAR lbox: OS2.HWND;
        j, k, N: CARDINAL;
        name: NameString;
        list, p, tail: NameList;

    BEGIN
        lbox := OS2.WinWindowFromID(hwnd,DID.userlist);

        (* Work out how many users there are in the list. *)

        N := CAST(CARDINAL, OS2.WinSendMsg (lbox, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        IF N = 0 THEN RETURN END(*IF*);

        (* Read the listbox into the NameList, assign sequence numbers. *)
        (* Skip any entries for which name = "?".                       *)

        k := 0;
        list := NIL;  tail := NIL;
        FOR j := 0 TO N-1 DO
            EVAL (OS2.WinSendMsg (lbox, OS2.LM_QUERYITEMTEXT,
                            OS2.MPFROM2SHORT(j, NameLength), ADR(name)));
            IF name[0] <> '?' THEN
                NEW (p);
                p^.next := NIL;
                p^.name := name;
                p^.seqnum := k;
                INC (k);
                IF list = NIL THEN
                    list := p;
                ELSE
                    tail^.next := p;
                END (*IF*);
                tail := p;
            END (*IF*);
        END (*FOR*);

        (* Now store sequence numbers from the linked list to the INI   *)
        (* file(s).  A sequence number of Skip (=MAX(CARD16)) means     *)
        (* that we have already dealt with this entry.                  *)

        WHILE list <> NIL DO
            IF list^.seqnum <> Skip THEN
                name := list^.name;
                IF name[0] <> '?' THEN
                    OpenINIForUser (name);
                    INIPut (name, "seqnum", list^.seqnum);
                    list^.seqnum := Skip;

                    (* Deal with any other users in this INI file. *)

                    p := list^.next;
                    WHILE p <> NIL DO
                        IF (p^.seqnum <> Skip)
                                    AND ItemSize (p^.name, '', k)
                                                    AND (k <> 0) THEN
                            INIPut (p^.name, "seqnum", p^.seqnum);
                            p^.seqnum := Skip;
                        END (*IF*);
                        p := p^.next;
                    END (*WHILE*);
                    CloseINIFile;
                END (*IF*);
            END (*IF*);
            p := list^.next;
            DISPOSE (list);
            list := p;
        END (*WHILE*);

    END StoreData;

(************************************************************************)
(*                        ENABLE/DISABLE BUTTONS                        *)
(************************************************************************)

PROCEDURE ItemSelected (hwnd: OS2.HWND;  selected: BOOLEAN);

    (* Enables/disables some buttons, depending on "selected". *)

    BEGIN
        IF selected THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CloneUserButton), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CloneUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), FALSE);
            OS2.WinSetFocus (OS2.HWND_DESKTOP,
                             OS2.WinWindowFromID(hwnd, DID.AddUserButton));
        END (*IF*);
    END ItemSelected;

(************************************************************************)
(*                          THE DIALOGUE PROCEDURE                      *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        code: CARDINAL;
        listwindow: OS2.HWND;
        name: NameString;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CloneUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), FALSE);
            LoadValues (hwnd);
            DisplayUserCount (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_COMMAND THEN

            listwindow := OS2.WinWindowFromID(hwnd,DID.userlist);
            index := OS2.LONGFROMMR(
                      OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_QUERYSELECTION, NIL, NIL));
            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.AddUserButton:
                   name := "";
                   IF index = OS2.LIT_NONE THEN
                       index := 0;
                   ELSE
                       INC(index);
                   END (*IF*);
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   IF EditUser.Edit(listwindow, name, FALSE) THEN
                       (* We have a new entry. *)
                       OpenINIForUser (name);
                       CloseINIFile;
                       OS2.WinSendMsg (listwindow, OS2.LM_SETITEMTEXT,
                                   OS2.MPFROMSHORT(index), ADR(name));
                       INC (UserCount);
                       DisplayUserCount (hwnd);
                   ELSE
                       (* Empty name, delete it. *)
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                             OS2.MPFROMSHORT(DID.DeleteUserButton), NIL);
                   END (*IF*);

              | DID.CloneUserButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   IF EditUser.Edit(listwindow, name, TRUE) THEN
                       OpenINIForUser (name);
                       CloseINIFile;
                       INC (index);
                       OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                              OS2.MPFROMSHORT(index), ADR(name));
                       OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                       INC (UserCount);
                       DisplayUserCount (hwnd);
                   END (*IF*);

              | DID.EditUserButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   EVAL (EditUser.Edit(listwindow, name, FALSE));
                   (* Name or category might have changed. *)
                   OpenINIForUser (name);
                   CloseINIFile;
                   OS2.WinSendMsg (listwindow, OS2.LM_SETITEMTEXT,
                               OS2.MPFROMSHORT(index), ADR(name));

              | DID.DeleteUserButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   ItemSelected (hwnd, FALSE);
                   IF name[0] <> Nul THEN
                       OpenINIForUser (name);
                       INIDeleteApp (name);
                       CloseINIFile;
                       DEC (UserCount);
                       DisplayUserCount (hwnd);
                   END (*IF*);

              | DID.SortButton:
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                                          OS2.MPFROMSHORT(OS2.LIT_NONE), NIL);
                   WideUserDialogue.SortDlg (hwnd, listwindow);

              | DID.SortButton2:
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                                          OS2.MPFROMSHORT(OS2.LIT_NONE), NIL);
                   WU2Dialogue.SortDlg (hwnd, listwindow);

            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*CASE*);

            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.userlist THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                    ItemSelected (hwnd, TRUE);
                    RETURN NIL;
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditUserButton), NIL);
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSIF msg = OS2.WM_CHAR THEN
            (* Check for Esc.  I probably should be using an accelerator  *)
            (* table to do this, but I'll sort that out later.            *)

            (*
            WriteHex (hwnd, 1, OS2.ULONGFROMMP(mp1));
            WriteHex (hwnd, 2, OS2.ULONGFROMMP(mp2));
            *)

            code := OS2.ULONGFROMMP(mp1);
            IF (IAND (code, OS2.KC_VIRTUALKEY) <> 0) THEN
                code := (OS2.ULONGFROMMP(mp2) DIV 65536) MOD 256;
                IF code = OS2.VK_ESC THEN
                    (* Deselect everything in the list box. *)
                    OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                                       OS2.MPFROMSHORT(OS2.LIT_NONE), NIL);
                    ItemSelected (hwnd, FALSE);
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the user page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.UserPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        Label := "~Users";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        RETURN pagehandle;
    END CreatePage;

(************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
END UserPage.

