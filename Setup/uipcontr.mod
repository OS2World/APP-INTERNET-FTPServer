(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
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

IMPLEMENTATION MODULE UIPControls;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*    Dialogue to edit the IP address controls for one user     *)
        (*                                                              *)
        (*        Started:        9 September 2008                      *)
        (*        Last edited:    26 May 2013                           *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, OS2RTL, DID, EditFilter, CommonSettings, Remote;

FROM SYSTEM IMPORT ADR;

FROM IPFilters IMPORT
    (* type *)  ListPtr,
    (* proc *)  AddrRecordToText;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

VAR
    (* List head for the allow/exclude list. *)

    ListHead: ListPtr;

    (* Current position in the list. *)

    CurrentItem: ListPtr;
    CurrentIndex: INTEGER;

    (* Flag to say that a window update is in progress. *)

    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*               LOADING THE INITIAL DIALOGUE ITEM CONTENTS             *)
(************************************************************************)

PROCEDURE InitialDisplay (hwnd: OS2.HWND);

    (* Fills the dialogue elements in the user IP filter dialogue.  *)

    VAR text: ARRAY [0..63] OF CHAR;
        current: ListPtr;

    BEGIN
        (* The master list has already been loaded from the INI file.   *)
        (* We still have to copy it into the listbox.                   *)

        current := ListHead;
        WHILE current <> NIL DO
            AddrRecordToText (current, text);
            OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), ADR(text));
            current := current^.next;
        END (*WHILE*);

        OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SELECTITEM,
                         OS2.MPFROMSHORT(0), OS2.MPFROMSHORT(1));
        IF (ListHead = NIL) OR (ListHead^.next = NIL) THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteFilterButton), FALSE);
        END (*IF*);

    END InitialDisplay;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;  p: ListPtr;
        listwindow: OS2.HWND;
        text: ARRAY [0..127] OF CHAR;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            InitialDisplay (hwnd);
            CurrentItem := ListHead;
            CurrentIndex := 0;
            RETURN NIL;
        END (*IF*);

        listwindow := OS2.WinWindowFromID(hwnd,DID.UFilterList);
        index := OS2.LONGFROMMR(
                  OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_QUERYSELECTION, NIL, NIL));
        IF index = OS2.LIT_NONE THEN
            index := 0;
            CurrentItem := ListHead;
            CurrentIndex := 0;
        ELSE
            IF CurrentItem = NIL THEN
                CurrentItem := ListHead;
                CurrentIndex := 0;
            END (*IF*);
            WHILE CurrentIndex < index DO
                CurrentItem := CurrentItem^.next;  INC(CurrentIndex);
            END (*WHILE*);
            WHILE CurrentIndex > index DO
                CurrentItem := CurrentItem^.previous;  DEC(CurrentIndex);
            END (*WHILE*);
        END (*IF*);

        IF msg = OS2.WM_COMMAND THEN

            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.UEditFilterButton:
                   IF EditFilter.Edit(listwindow, CurrentItem) THEN
                       AddrRecordToText (CurrentItem, text);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));
                   END (*IF*);

              | DID.UInsertFilterButton:
                   NEW (p);
                   WITH p^ DO
                       previous := CurrentItem^.previous;
                       next := CurrentItem;
                       allow := TRUE;  type := 3;
                       address := 0;  bitcount := 24;
                   END (*WITH*);
                   CurrentItem^.previous := p;
                   IF p^.previous = NIL THEN
                       ListHead := p;
                   ELSE
                       p^.previous^.next := p;
                   END (*IF*);
                   CurrentItem := p;
                   text := "";
                   OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(text));
                   OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   IF EditFilter.Edit(listwindow, CurrentItem) THEN
                       AddrRecordToText (CurrentItem, text);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));

                       (* Have we just done an insertion before the end of the list? *)
                       (* If so, might need to change "all" to "all others".         *)

                       p := CurrentItem^.next;
                       IF p^.type = 0 THEN
                           AddrRecordToText (p, text);
                           OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index+1), ADR(text));
                       END (*IF*);

                   ELSE
                       (* Editing was aborted, so delete the item we've just inserted. *)

                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                               OS2.MPFROMSHORT(DID.UDeleteFilterButton), NIL);
                   END (*IF*);
                   OS2.WinSetFocus (OS2.HWND_DESKTOP, listwindow);

              | DID.UPromoteFilterButton:

                   (*ConsistencyCheck (hwnd, "Before promote");*)
                   p := CurrentItem^.previous;
                   IF p <> NIL THEN
                       p^.next := CurrentItem^.next;
                       p^.next^.previous := p;
                       CurrentItem^.next := p;
                       CurrentItem^.previous := p^.previous;
                       IF p^.previous = NIL THEN
                           ListHead := CurrentItem;
                       ELSE
                           p^.previous^.next := CurrentItem;
                       END (*IF*);
                       p^.previous := CurrentItem;
                       AddrRecordToText (p, text);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(text));
                       DEC (CurrentIndex);
                       AddrRecordToText (CurrentItem, text);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(text));
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(CurrentIndex), OS2.MPFROMSHORT(ORD(TRUE)));
                       (*ConsistencyCheck (hwnd, "After promote");*)
                   END (*IF*);

              | DID.UDeleteFilterButton:

                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(text));
                   p := CurrentItem;
                   IF p^.previous = NIL THEN
                       ListHead := p^.next;
                   ELSE
                       p^.previous^.next := p^.next;
                   END (*IF*);
                   CurrentItem := p^.next;
                   CurrentItem^.previous := p^.previous;
                   DISPOSE (p);
                   OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));

                   (* Are we now down to just one list item?                *)
                   (* If so, might need to change "all others" to "all".    *)

                   IF (CurrentItem^.type = 0) AND (CurrentItem^.previous = NIL) THEN
                       AddrRecordToText (CurrentItem, text);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UFilterList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));
                       OS2.WinSetFocus (OS2.HWND_DESKTOP,
                              OS2.WinWindowFromID(hwnd, DID.UInsertFilterButton));
                   END (*IF*);

              | DID.UDone:

                   OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                   RETURN NIL;

              | ELSE
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

            END (*CASE*);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSIF msg = OS2.WM_CONTROL THEN

            (*ConsistencyCheck (hwnd, "WM_CONTROL received");*)
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.UFilterList THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.UEditFilterButton), NIL);
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);
        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        (* Enable or disable the buttons, as appropriate. *)

        IF CurrentItem = NIL THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UEditFilterButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteFilterButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteFilterButton), FALSE);
        ELSE

            (* If the following code looks kinky, it's because the more obvious *)
            (* code doesn't work.  Compiler bug, I suspect.                     *)

            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UEditFilterButton), TRUE);
            IF (CurrentItem^.previous = NIL) OR (CurrentItem^.next = NIL) THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteFilterButton), FALSE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteFilterButton), TRUE);
            END (*IF*);
            IF CurrentItem^.next = NIL THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteFilterButton), FALSE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteFilterButton), TRUE);
            END (*IF*);
        END (*IF*);
        RETURN NIL;

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  VAR (*INOUT*) L: ListPtr);

    (* Edit list L.  *)

    VAR hwnd: OS2.HWND;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        ListHead := L;
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,       (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.UserIPControls, (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "UIPControls");

        OS2.WinSetFocus (OS2.HWND_DESKTOP,
                          OS2.WinWindowFromID(hwnd, DID.UEditFilterButton));
        OS2.WinProcessDlg(hwnd);

        Remote.StoreWindowPosition (hwnd, "UIPControls", TRUE);
        OS2.WinDestroyWindow (hwnd);
        L := ListHead;
    END Edit;

(**************************************************************************)

BEGIN
    ListHead := NIL;
    CurrentItem := NIL;
    CurrentIndex := 0;
    ChangeInProgress := FALSE;
END UIPControls.

