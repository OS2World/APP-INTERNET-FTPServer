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

IMPLEMENTATION MODULE UserPage;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*                 User page of the notebook                    *)
        (*                                                              *)
        (*        Started:        11 October 1999                       *)
        (*        Last edited:    10 July 2011                          *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, Strings, DID, BasicPage, EditUser, CommonSettings;

FROM FSUINI IMPORT
    (* proc *)  SetINIFileName, SetHashMax, OpenINIFile, OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  ItemSize, INIFetch, INIGetCard,
                GetStringList, NextString, CloseStringList, INIDeleteApp;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ConvertCard;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    pagehandle: OS2.HWND;
    UserCount: CARDINAL;
    UseTNI, ChangeInProgress: BOOLEAN;

(**************************************************************************)

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

(**************************************************************************)

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

(**************************************************************************)

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

(**************************************************************************)

PROCEDURE AddDetail (VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Appends the user category, in parentheses, to a username.  We    *)
    (* assume that the INI file is open.                                *)

    TYPE UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser,
                                NormalUser, Manager, UserTemplate);

    VAR size: CARDINAL;
        category: UserCategory;
        found, Active: BOOLEAN;
        label: ARRAY [0..15] OF CHAR;

    BEGIN
        (* First check whether user exists. *)

        IF (name[0] = Nul) OR (name[0] = '$') THEN
            found := FALSE;
        ELSE
            found := ItemSize(name, '', size) AND (size <> 0);
        END (*IF*);
        category := NormalUser;
        Active := TRUE;

        IF found THEN
            IF (NOT INIFetch (name, "Category", category))
                           OR (category = NoSuchUser) THEN
                category := NormalUser;
            END (*IF*);
            IF NOT INIFetch (name, "Active", Active) THEN
                Active := TRUE;
            END (*IF*);
        END (*IF*);

        CASE category OF
             NoSuchUser:        label := "invalid";
          |  NoPasswordNeeded:  label := "no password";
          |  GuestUser:         label := "guest";
          |  NormalUser:        label := "user";
          |  Manager:           label := "manager";
          |  UserTemplate:      label := "template";
           ELSE
                                label := "unknown";
        END (*CASE*);

        Strings.Append (" (", name);
        Strings.Append (label, name);
        IF NOT Active THEN
            Strings.Append (", inactive", name);
        END (*IF*);
        Strings.Append (")", name);

    END AddDetail;

(**************************************************************************)

PROCEDURE StripDetail (VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Removes the user category from name, by deleting everything      *)
    (* from " (" onwards.                                               *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext (" (", name, 0, found, pos);
        IF found THEN
            name[pos] := Nul;
        END (*IF*);
    END StripDetail;

(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the user list with data from the INI files.  Note that there *)
    (* can be multiple INI files depending on the 'HashMax' INI value.    *)

    VAR D: DirectoryEntry;
        state: StringReadState;
        appname:  ARRAY [0..NameLength+19] OF CHAR;
        mask: ARRAY [0..127] OF CHAR;
        N: CARDINAL;
        MultiFileMode, found: BOOLEAN;

    BEGIN
        OpenINIFile;
        MultiFileMode := INIGetCard ('$SYS', 'HashMax', N) AND (N > 0);
        SetHashMax (N);
        IF MultiFileMode THEN
            CloseINIFile;
            IF UseTNI THEN
                mask := "FTPD*.TNI";
            ELSE
                mask := "FTPD*.INI";
            END (*IF*);
            found := FirstDirEntry (mask, FALSE, TRUE, D);
        ELSE
            found := TRUE;
        END (*IF*);

        (* Each time around this loop we process one INI file. *)

        WHILE found DO

            IF MultiFileMode THEN
                SetINIFileName (D.name, UseTNI);
                OpenINIFile;
            END (*IF*);

            (* Process all the application names in this file. *)

            GetStringList ('', '', state);
            LOOP
                NextString (state, appname);
                IF appname[0] = Nul THEN

                    EXIT (*LOOP*);

                ELSIF appname[0] <> '$' THEN

                    (* Add name to the listbox. *)

                    INC (UserCount);
                    AddDetail (appname);
                    OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                             OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), ADR(appname));

                END (*IF*);

            END (*LOOP*);

            CloseStringList (state);
            CloseINIFile;
            found := MultiFileMode AND NextDirEntry (D);

        END (*WHILE*);

        IF MultiFileMode THEN
            DirSearchDone (D);
            IF UseTNI THEN
                mask := "FTPD.TNI";
            ELSE
                mask := "FTPD.INI";
            END (*IF*);
            SetINIFileName (mask, UseTNI);
        END (*IF*);

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* A do-nothing procedure - INI file updating is done as part of    *)
    (* the button processing.                                           *)

    BEGIN
    END StoreData;

(**************************************************************************)

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

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        code: CARDINAL;
        listwindow: OS2.HWND;
        name: ARRAY [0..NameLength+19] OF CHAR;

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
                       AddDetail (name);
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
                   StripDetail (name);
                   IF EditUser.Edit(listwindow, name, TRUE) THEN
                       OpenINIForUser (name);
                       AddDetail (name);
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
                   StripDetail (name);
                   EVAL (EditUser.Edit(listwindow, name, FALSE));
                   (* Name or category might have changed. *)
                   OpenINIForUser (name);
                   AddDetail (name);
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
                       StripDetail (name);
                       OpenINIForUser (name);
                       INIDeleteApp (name);
                       CloseINIFile;
                       DEC (UserCount);
                       DisplayUserCount (hwnd);
                   END (*IF*);

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

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  TNImode: BOOLEAN;
                                VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the user page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := TNImode;
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

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    UseTNI := FALSE;
    ChangeInProgress := FALSE;
END UserPage.

