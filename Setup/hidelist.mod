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

IMPLEMENTATION MODULE HideList;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*          Dialogue to edit the HideList for one user          *)
        (*                                                              *)
        (*        Started:        27 May 2013                           *)
        (*        Last edited:    1 June 2013                           *)
        (*        Status:         Working                               *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, OS2RTL, DID, EditFilter, CommonSettings, OneLine, Remote, Strings;

FROM SYSTEM IMPORT ADR;

FROM FSUINI IMPORT
    (* proc *)  OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  ItemSize, INIFetchBinary, INIPutBinary, INIDeleteKey;

FROM FileOps IMPORT FilenameString;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);

VAR
    (* List head for the HideList. *)

    ListHead: ListPtr;

    (* Current position in the list.  These are recalculated on each    *)
    (* entry to the dialogue procedure, because of the likelihood that  *)
    (* the user has moved the listbox cursor.                           *)

    (* So do they need to be global? Check this once other issues       *)
    (* have been resolved.                                              *)

    CurrentItem: ListPtr;
    CurrentIndex: INTEGER;

    (* Flag to say that a window update is in progress. *)

    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*               GETTING/PUTTING A HIDELIST FROM INI FILE               *)
(************************************************************************)

PROCEDURE LoadList (VAR (*IN*) username: ARRAY OF CHAR): ListPtr;

    (* Loads a HideList from the INI file.    *)

    VAR j, size: CARDINAL;
        bufptr: CharArrayPointer;
        ListHead, current: ListPtr;

    (********************************************************************)

    PROCEDURE GetString (VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

        (* Reads a string from bufptr^[j], updates j; but returns *)
        (* FALSE for an empty string.                             *)

        VAR k: CARDINAL;

        BEGIN
            IF bufptr^[j] = Nul THEN
                RETURN FALSE;
            END (*IF*);
            k := 0;
            LOOP
                IF k > HIGH(result) THEN
                    RETURN TRUE;
                ELSIF j >= size THEN
                    result[k] := Nul;
                    RETURN TRUE;
                ELSE
                    result[k] := bufptr^[j];  INC(j);  INC(k);
                    IF result[k-1] = CHR(0) THEN
                        RETURN TRUE;
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
        END GetString;

    (********************************************************************)

    BEGIN
        IF username[0] = Nul THEN
            NEW (ListHead);
            WITH ListHead^ DO
                previous := NIL;
                next := NIL;
                mask := "";
            END (*WITH*);
            RETURN ListHead;
        END (*IF*);

        OpenINIForUser (username);
        ListHead := NIL;
        IF NOT ItemSize (username, "HideList", size) THEN

            (* Make a list of one dummy entry. *)

            size := 1;
        END (*IF*);

        IF size > 0 THEN
            ALLOCATE (bufptr, size);
            IF NOT INIFetchBinary (username, "HideList", bufptr^, size) THEN
                bufptr^[0] := Nul;
            END (*IF*);
            j := 0;  current := NIL;
            REPEAT
                IF current = NIL THEN
                    NEW (ListHead);
                    ListHead^.previous := NIL;
                    current := ListHead;
                ELSE
                    NEW (current^.next);
                    current^.next^.previous := current;
                    current := current^.next;
                END (*IF*);
                current^.next := NIL;
            UNTIL NOT GetString (current^.mask);
            DEALLOCATE (bufptr, size);

            (* The final element was an empty string.  We retain it     *)
            (* because it's convenient to have a blank final entry in   *)
            (* the listbox.                                             *)

        END (*IF*);

        CloseINIFile;

        RETURN ListHead;

    END LoadList;

(************************************************************************)

PROCEDURE StoreList (VAR (*IN*) username: ARRAY OF CHAR;  head: ListPtr);

    (* Stores a HideList to the INI file.  We assume that the   *)
    (* caller already has the INI file open.                    *)

    VAR bufptr: CharArrayPointer;
        current: ListPtr;  j, size: CARDINAL;

    (********************************************************************)

    PROCEDURE Put (value: ARRAY OF CHAR);

        (* Stores a Nul-terminated string into bufptr^[j], updates j. *)

        VAR k: CARDINAL;  ch: CHAR;

        BEGIN
            k := 0;  ch := value[0];
            WHILE ch <> Nul DO
                bufptr^[j] := ch;  INC(j);
                INC (k);  ch := value[k];
            END (*WHILE*);
            bufptr^[j] := Nul;  INC(j);
        END Put;

    (********************************************************************)

    BEGIN
        (* Special case: if the list contains only the final dummy      *)
        (* entry, remove the entry from the INI file.                   *)

        IF (head = NIL) OR (head^.next = NIL) THEN
            INIDeleteKey (username, "HideList");
            RETURN;
        END (*IF*);

        (* The normal case: *)

        size := 0;  j := 0;  current := head;
        WHILE current <> NIL DO
            INC (size, Strings.Length(current^.mask) + 1);
            current := current^.next;
        END (*WHILE*);
        IF size = 0 THEN bufptr := NIL
        ELSE
            ALLOCATE (bufptr, size);
        END (*IF*);
        current := head;
        WHILE current <> NIL DO
            Put (current^.mask);
            current := current^.next;
        END (*WHILE*);
        IF bufptr <> NIL THEN
            INIPutBinary (username, "HideList", bufptr^, size);
            DEALLOCATE (bufptr, size);
        END (*IF*);
    END StoreList;

(************************************************************************)

PROCEDURE RemoveSlash (VAR (*INOUT*) str: ARRAY OF CHAR);

    (* Removes '/' and '\' characters from the string. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        pos := 0;
        REPEAT
            Strings.FindNext ('/', str, pos, found, pos);
            IF found THEN
                Strings.Delete (str, pos, 1);
            END (*IF*);
        UNTIL NOT found;
        pos := 0;
        REPEAT
            Strings.FindNext ('\', str, pos, found, pos);
            IF found THEN
                Strings.Delete (str, pos, 1);
            END (*IF*);
        UNTIL NOT found;
    END RemoveSlash;

(************************************************************************)
(*               LOADING THE INITIAL DIALOGUE ITEM CONTENTS             *)
(************************************************************************)

PROCEDURE InitialDisplay (hwnd: OS2.HWND);

    (* Fills the dialogue elements in the user HideList dialogue.  *)

    VAR current: ListPtr;

    BEGIN
        (* The master list has already been loaded from the INI file.   *)
        (* We still have to copy it into the listbox.                   *)

        current := ListHead;
        WHILE current <> NIL DO
            OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), ADR(current^.mask));
            current := current^.next;
        END (*WHILE*);

        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteMaskButton), FALSE);

        IF (ListHead = NIL) OR (ListHead^.next = NIL) THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UEditMaskButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteMaskButton), FALSE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SELECTITEM,
                         OS2.MPFROMSHORT(0), OS2.MPFROMSHORT(1));
            (*
            OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd,DID.UHideList));
            OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd,DID.UPromoteMaskButton));
            *)
        END (*IF*);

    END InitialDisplay;

(**************************************************************************)
(*                         THE DIALOGUE PROCEDURE                         *)
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

        listwindow := OS2.WinWindowFromID(hwnd,DID.UHideList);
        index := OS2.LONGFROMMR(
                  OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_QUERYSELECTION, NIL, NIL));
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

              | DID.UEditMaskButton:
                   IF CurrentItem^.mask[0] = Nul THEN
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                         OS2.MPFROMSHORT(DID.UInsertMaskButton), NIL);
                   ELSE
                       IF OneLine.Edit (listwindow, "Edit the mask", CurrentItem^.mask) THEN
                           OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index), ADR(CurrentItem^.mask));
                           RemoveSlash (CurrentItem^.mask);
                       END (*IF*);

                       (* If result is blank, delete the current item. *)

                       IF CurrentItem^.mask[0] = Nul THEN
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                         OS2.MPFROMSHORT(DID.UDeleteMaskButton), NIL);
                       END (*IF*);
                   END (*IF*);

              | DID.UInsertMaskButton:
                   NEW (p);
                   IF CurrentItem = NIL THEN
                       p^.previous := NIL;
                   ELSE
                       p^.previous := CurrentItem^.previous;
                       CurrentItem^.previous := p;
                   END (*IF*);
                   IF p^.previous = NIL THEN
                       ListHead := p;
                   ELSE
                       p^.previous^.next := p;
                   END (*IF*);
                   p^.next := CurrentItem;
                   CurrentItem := p;
                   p^.mask := "";
                   OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(p^.mask));
                   OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   IF OneLine.Edit (listwindow, "New mask", CurrentItem^.mask)
                                   AND (CurrentItem^.mask[0] <> Nul) THEN
                       RemoveSlash (CurrentItem^.mask);
                       IF CurrentItem^.mask[0] = Nul THEN
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                   OS2.MPFROMSHORT(DID.UDeleteMaskButton), NIL);
                       ELSE
                           OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index), ADR(CurrentItem^.mask));

                           (* Move to the next item, as an aid to someone who  *)
                           (* who is adding a series of entries.               *)

                           CurrentItem := CurrentItem^.next;
                           INC (CurrentIndex);   INC (index);
                           OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SELECTITEM,
                                  OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                       END (*IF*);
                   ELSE
                       (* Editing was aborted, so delete the item we've just inserted. *)

                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                               OS2.MPFROMSHORT(DID.UDeleteMaskButton), NIL);
                   END (*IF*);
                   OS2.WinSetFocus (OS2.HWND_DESKTOP, listwindow);

              | DID.UPromoteMaskButton:

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
                       OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(p^.mask));
                       DEC (CurrentIndex);
                       OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(CurrentItem^.mask));
                       OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(CurrentIndex), OS2.MPFROMSHORT(ORD(TRUE)));
                       (*ConsistencyCheck (hwnd, "After promote");*)
                   END (*IF*);

              | DID.UDeleteMaskButton:

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
                   OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinSendDlgItemMsg (hwnd, DID.UHideList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));

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
            IF ButtonID = DID.UHideList THEN
                IF NotificationCode = OS2.LN_ENTER THEN

                    (* A double-click will get us here.  Treat this one as if   *)
                    (* the edit button had been clicked.  If we are currently   *)
                    (* at the end of the list we should do an insert rather     *)
                    (* than an edit.                                            *)

                   IF CurrentItem^.mask[0] = Nul THEN
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                       OS2.MPFROMSHORT(DID.UInsertMaskButton), NIL);
                   ELSE
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                       OS2.MPFROMSHORT(DID.UEditMaskButton), NIL);
                   END (*IF*);

                ELSIF NotificationCode <> OS2.LN_SELECT THEN

                    (* For LN_SELECT we don't want to return just yet, because  *)
                    (* we still need to work out which buttons to disable.      *)

                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);
        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*IF*);

        (* Enable or disable the buttons, as appropriate. *)

        IF (CurrentItem = NIL) OR (CurrentItem^.next = NIL) THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UEditMaskButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteMaskButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteMaskButton), FALSE);
        ELSE

            (* If the following code looks kinky, it's because the more obvious *)
            (* code doesn't work.  Compiler bug, I suspect.                     *)

            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UEditMaskButton), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UDeleteMaskButton), TRUE);
            IF CurrentItem^.previous = NIL THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteMaskButton), FALSE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UPromoteMaskButton), TRUE);
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
                       DID.UserHideList,   (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "HideList");

        OS2.WinProcessDlg(hwnd);

        Remote.StoreWindowPosition (hwnd, "HideList", TRUE);
        OS2.WinDestroyWindow (hwnd);
        L := ListHead;
    END Edit;

(**************************************************************************)

BEGIN
    ListHead := NIL;
    CurrentItem := NIL;
    CurrentIndex := 0;
    ChangeInProgress := FALSE;
END HideList.

