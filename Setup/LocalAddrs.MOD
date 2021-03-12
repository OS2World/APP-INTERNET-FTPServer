(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE LocalAddrs;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*               Security page of the notebook                  *)
        (*                                                              *)
        (*        Started:        10 October 1999                       *)
        (*        Last edited:    23 October 2019                       *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, OS2RTL, DID, CommonSettings, EditFilter;

FROM SYSTEM IMPORT ADR, ADDRESS, CAST;

FROM FSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM IPFilters IMPORT
    (* type *)  ListPtr,
    (* proc *)  CardinalToDotted, StringToIPAddress,
                AddrRecordToText, LoadIPFilterList, StoreIPFilterList;

FROM RINIData IMPORT
    (* proc *)  INIGetCard, INIPut, INIDeleteKey, ServerIPAddress;

FROM Misc IMPORT
    (* proc *)  WinQueryDlgItemCard, WinSetDlgItemCard;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

VAR
    pagehandle: OS2.HWND;

    (* Initial value for the "report our IP address as".    *)

    OldFirewallIPAddr: CARDINAL;

    (* List head for the include/exclude list. *)

    MasterListHead: ListPtr;

    (* Current position in the list. *)

    CurrentItem: ListPtr;
    CurrentIndex: INTEGER;

    (* Flag: TRUE if list has been altered. *)

    ListChanged: BOOLEAN;

    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*               LOADING THE INITIAL DIALOGUE ITEM CONTENTS             *)
(************************************************************************)

PROCEDURE InitialDisplay (hwnd: OS2.HWND);

    (* Fills the dialogue elements on the user page with data from the INI file,*)
    (* or loads default values if they're not in the INI file.                  *)

    VAR text: ARRAY [0..63] OF CHAR;
        current: ListPtr;
        AddressString: ARRAY [0..15] OF CHAR;
        value, OurHostID: CARDINAL;

    BEGIN
        OurHostID := ServerIPAddress();

        OpenINIFile;

        IF INIGetCard ('$SYS', 'FirewallIPAddr', value) THEN
            OldFirewallIPAddr := value;
        ELSE
            value := OurHostID;
        END (*IF*);
        CardinalToDotted (value, AddressString);
        OS2.WinSetDlgItemText (hwnd, DID.FirewallIPAddr, AddressString);

        CloseINIFile;

        (* The master list has already been loaded from the INI file.   *)
        (* We still have to copy it into the listbox.                   *)

        current := MasterListHead;
        WHILE current <> NIL DO
            AddrRecordToText (current, text, TRUE);
            OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), ADR(text));
            current := current^.next;
        END (*WHILE*);

        OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SELECTITEM,
                         OS2.MPFROMSHORT(0), OS2.MPFROMSHORT(1));
        IF MasterListHead^.next = NIL THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteLAButton), FALSE);
        END (*IF*);

    END InitialDisplay;

(************************************************************************)

PROCEDURE ConvertFromOldFormat;

    (* If the master address list is empty, re-create it.   *)

    VAR next, p: ListPtr;
        MinAddr, MaxAddr: CARDINAL;

    BEGIN
        IF MasterListHead^.type = 0 THEN

            (* List starts with end record, so effectively empty. *)

            WHILE MasterListHead <> NIL DO
                next := MasterListHead^.next;
                DISPOSE (MasterListHead);
                MasterListHead := next;
            END (*WHILE*);

        END (*IF*);

        (* After that preliminary, we have either a genuine list (which *)
        (* we don't want to alter), or a totally empty list.            *)

        IF MasterListHead = NIL THEN

            (* Create a list that reflects the MinLocalAddr and         *)
            (* MaxLocalAddr values that are probably in the INI file.   *)

            OpenINIFile;

            IF NOT INIGetCard ('$SYS', 'MinLocalAddr', MinAddr) THEN
                MinAddr := ServerIPAddress();
            END (*IF*);

            IF NOT INIGetCard ('$SYS', 'MaxLocalAddr', MaxAddr) THEN
                MaxAddr := MinAddr;
            END (*IF*);

            NEW (MasterListHead);
            p := MasterListHead;
            p^.previous := NIL;  p^.next := NIL;
            p^.allow := TRUE;
            p^.type := 4;
            p^.firstaddr := MinAddr;
            p^.lastaddr := MaxAddr;

            NEW (p^.next);
            p := p^.next;
            p^.previous := MasterListHead;  p^.next := NIL;
            p^.allow := FALSE;
            p^.type := 0;
            p^.firstaddr := 0;
            p^.lastaddr := 0;

            ListChanged := TRUE;

            INIDeleteKey ('$SYS', 'MinLocalAddr');
            INIDeleteKey ('$SYS', 'MaxLocalAddr');

            CloseINIFile;

        END (*IF*);

    END ConvertFromOldFormat;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the security data back into the INI file. *)

    VAR TextBuffer: ARRAY [0..15] OF CHAR;
        value: CARDINAL;

    BEGIN
        OpenINIFile;

        (* Report our address as ... *)

        OS2.WinQueryDlgItemText (hwnd, DID.FirewallIPAddr, 16, TextBuffer);
        value := StringToIPAddress (TextBuffer);
        IF value <> OldFirewallIPAddr THEN
            INIPut ('$SYS', 'FirewallIPAddr', value);
        END (*IF*);

        (* The allow/deny list. *)

        IF ListChanged THEN
            StoreIPFilterList ("$SYS", "LocalAddrs", MasterListHead);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode, code: CARDINAL;
        index: INTEGER;  p: ListPtr;
        listwindow: OS2.HWND;
        text: ARRAY [0..127] OF CHAR;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            InitialDisplay (hwnd);
            CurrentItem := MasterListHead;
            CurrentIndex := 0;
            RETURN NIL;
        END (*IF*);

        listwindow := OS2.WinWindowFromID(hwnd,DID.LocalAddrList);
        index := OS2.LONGFROMMR(
                  OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_QUERYSELECTION, NIL, NIL));
        IF index = OS2.LIT_NONE THEN
            index := 0;
            CurrentItem := MasterListHead;
            CurrentIndex := 0;
        ELSE
            IF CurrentItem = NIL THEN
                CurrentItem := MasterListHead;
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

              | DID.EditLAButton:
                   IF EditFilter.Edit(listwindow, CurrentItem) THEN
                       ListChanged := TRUE;
                       AddrRecordToText (CurrentItem, text, TRUE);
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));
                   END (*IF*);

              | DID.InsertLAButton:
                   NEW (p);
                   WITH p^ DO
                       previous := CurrentItem^.previous;
                       next := CurrentItem;
                       allow := TRUE;  type := 3;
                       address := 0;  bitcount := 24;
                   END (*WITH*);
                   CurrentItem^.previous := p;
                   IF p^.previous = NIL THEN
                       MasterListHead := p;
                   ELSE
                       p^.previous^.next := p;
                   END (*IF*);
                   CurrentItem := p;
                   text := "";
                   OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(text));
                   OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   IF EditFilter.Edit(listwindow, CurrentItem) THEN
                       ListChanged := TRUE;
                       AddrRecordToText (CurrentItem, text, TRUE);
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));

                       (* Have we just done an insertion before the end of the list? *)
                       (* If so, might need to change "all" to "all others".         *)

                       p := CurrentItem^.next;
                       IF p^.type = 0 THEN
                           AddrRecordToText (p, text, TRUE);
                           OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index+1), ADR(text));
                       END (*IF*);

                   ELSE
                       (* Editing was aborted, so delete the item we've just inserted. *)

                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                               OS2.MPFROMSHORT(DID.LocalAddrList), NIL);
                   END (*IF*);
                   OS2.WinSetFocus (OS2.HWND_DESKTOP, listwindow);

              | DID.PromoteLAButton:

                   (*ConsistencyCheck (hwnd, "Before promote");*)
                   p := CurrentItem^.previous;
                   IF p <> NIL THEN
                       ListChanged := TRUE;
                       p^.next := CurrentItem^.next;
                       p^.next^.previous := p;
                       CurrentItem^.next := p;
                       CurrentItem^.previous := p^.previous;
                       IF p^.previous = NIL THEN
                           MasterListHead := CurrentItem;
                       ELSE
                           p^.previous^.next := CurrentItem;
                       END (*IF*);
                       p^.previous := CurrentItem;
                       AddrRecordToText (p, text, TRUE);
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(text));
                       DEC (CurrentIndex);
                       AddrRecordToText (CurrentItem, text, TRUE);
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(CurrentIndex), ADR(text));
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(CurrentIndex), OS2.MPFROMSHORT(ORD(TRUE)));
                       (*ConsistencyCheck (hwnd, "After promote");*)
                   END (*IF*);

              | DID.DeleteLAButton:

                   ListChanged := TRUE;
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(text));
                   p := CurrentItem;
                   IF p^.previous = NIL THEN
                       MasterListHead := p^.next;
                   ELSE
                       p^.previous^.next := p^.next;
                   END (*IF*);
                   CurrentItem := p^.next;
                   CurrentItem^.previous := p^.previous;
                   DISPOSE (p);
                   OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));

                   (* Are we now down to just one list item?                *)
                   (* If so, might need to change "all others" to "all".    *)

                   IF (CurrentItem^.type = 0) AND (CurrentItem^.previous = NIL) THEN
                       AddrRecordToText (CurrentItem, text, TRUE);
                       OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(text));
                       OS2.WinSetFocus (OS2.HWND_DESKTOP,
                              OS2.WinWindowFromID(hwnd, DID.InsertLAButton));
                   END (*IF*);

              | ELSE
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

            END (*CASE*);

        ELSIF msg = OS2.WM_CHAR THEN
            (* Check for Esc.  I probably should be using an accelerator  *)
            (* table to do this, but I'll sort that out later.            *)

            code := OS2.ULONGFROMMP(mp1);
            IF (IAND (code, OS2.KC_VIRTUALKEY) <> 0) THEN
                code := (OS2.ULONGFROMMP(mp2) DIV 65536) MOD 256;
                IF code = OS2.VK_ESC THEN
                    (* Deselect everything in the list box. *)
                    OS2.WinSendDlgItemMsg (hwnd, DID.LocalAddrList, OS2.LM_SELECTITEM,
                                       OS2.MPFROMSHORT(OS2.LIT_NONE), NIL);
                    CurrentItem := NIL;
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

        ELSIF msg = OS2.WM_CONTROL THEN

            (*ConsistencyCheck (hwnd, "WM_CONTROL received");*)
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.LocalAddrList THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditLAButton), NIL);
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
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditLAButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteLAButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteLAButton), FALSE);
        ELSE

            (* If the following code looks kinky, it's because the more obvious *)
            (* code doesn't work.  Compiler bug, I suspect.                     *)

            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditLAButton), TRUE);
            IF (CurrentItem^.previous = NIL) OR (CurrentItem^.next = NIL) THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteLAButton), FALSE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteLAButton), TRUE);
            END (*IF*);
            IF CurrentItem^.next = NIL THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteLAButton), FALSE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteLAButton), TRUE);
            END (*IF*);
        END (*IF*);
        RETURN NIL;

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  AfterPage: CARDINAL;
                        VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the localaddrs and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        ListChanged := FALSE;
        MasterListHead := LoadIPFilterList ("$SYS", "LocalAddrs");
        ConvertFromOldFormat;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.LocalAddrs,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         CAST(ADDRESS,AfterPage),
                          OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_NEXT)));
        Label := "~LocalAddrs";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        RETURN pagehandle;
    END CreatePage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(************************************************************************)

PROCEDURE Close (notebook, hwnd: OS2.HWND;  PageID: CARDINAL);

    (* Removes this page from the notebook. *)

    BEGIN
        StoreData (hwnd);
        OS2.WinSendMsg (notebook, OS2.BKM_DELETEPAGE,
                        CAST(ADDRESS, PageID),
                        OS2.MPFROMLONG (OS2.BKA_SINGLE));
        OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
        ListChanged := FALSE;
    END Close;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    OldFirewallIPAddr := 0;
END LocalAddrs.

