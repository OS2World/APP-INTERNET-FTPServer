(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
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

IMPLEMENTATION MODULE BasicPage;

        (****************************************************************)
        (*                                                              *)
        (*                    PM Setup for FtpServer                    *)
        (*                    Page 1 of the notebook                    *)
        (*                                                              *)
        (*        Started:        8 October 1999                        *)
        (*        Last edited:    16 December 2018                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, INT16, CARD32, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings;

FROM FSUINI IMPORT
    (* proc *)  SetINIFileName, OpenINIFile, CloseINIFile,
                SetHashMax, OpenINIForUser;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  RemoteOperation, INIPut, INIPutString, INIFetchBinary,
                INIFetch, INIPutBinary, INIDeleteApp,
                INIGetCard, INIGetTwoShort, INIGetString, ItemSize,
                GetStringList, NextString, CloseStringList;

FROM Remote IMPORT
    (* proc *)  OurDirectory;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone,
                DeleteFile, MoveFile;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ToLower;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM Names IMPORT
    (* type *)  UserName, UserNameIndex;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NewHashMax = 97;

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;
    DirectoryString = ARRAY [0..511] OF CHAR;
    TextArray = ARRAY [0..31] OF CHAR;
    TextPointer = POINTER TO TextArray;
    LongTextPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

    AppPointer = POINTER TO AppRecord;
    KeyPointer = POINTER TO KeyRecord;

    AppRecord = RECORD
                    next: AppPointer;
                    firstkey: KeyPointer;
                    name: TextArray;
                END (*RECORD*);
    KeyRecord = RECORD
                    next: KeyPointer;
                    name: TextArray;
                    length: CARDINAL;
                    pval: LongTextPointer;
                END (*RECORD*);

VAR
    ChangeInProgress: BOOLEAN;
    pagehandle: OS2.HWND;

    (* Original values of the ini variables. *)

    OldServerPort, OldMaxUsers, OldGuestLimit, OldSpaceThreshold,
                                                    OldTimeout: CARDINAL;
    HashMax: CARDINAL;
    UseTNI: BOOLEAN;
    OldTelnetDisable: BOOLEAN;

(************************************************************************)
(*                    MOVING USERS BETWEEN INI FILES                    *)
(************************************************************************)

PROCEDURE LoadApp (p: AppPointer);

    (* Loads all data for one application from the currently opened     *)
    (* INI file.                                                        *)

    VAR state: StringReadState;
        appname, keyname: TextArray;
        head, current: KeyPointer;

    BEGIN
        appname := p^.name;

        (* Find all keys and the corresponding values. *)

        GetStringList (appname, '', state);
        head := NIL;  current := NIL;
        LOOP
            NextString (state, keyname);
            IF keyname[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF head = NIL THEN
                NEW (head);  current := head;
            ELSE
                NEW (current^.next);  current := current^.next;
            END (*IF*);
            current^.next := NIL;
            current^.name := keyname;
            IF NOT ItemSize (appname, keyname, current^.length) THEN
                current^.length := 0;
            END (*IF*);
            IF current^.length = 0 THEN
                current^.pval := NIL;
            ELSE
                ALLOCATE (current^.pval, current^.length);
                IF NOT INIFetchBinary (appname, keyname,
                               current^.pval^, current^.length) THEN
                    DEALLOCATE (current^.pval, current^.length);
                    current^.length := 0;
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
        CloseStringList (state);
        p^.firstkey := head;

    END LoadApp;

(************************************************************************)

PROCEDURE StoreApp (p: AppPointer);

    (* Stores all key/value pairs for one application to the currently  *)
    (* opened INI file.  As a side-effect, we deallocate the key list.  *)

    VAR appname: TextArray;
        current, temp: KeyPointer;

    BEGIN
        appname := p^.name;  current := p^.firstkey;
        p^.firstkey := NIL;
        WHILE current <> NIL DO
            IF current^.length = 0 THEN
                INIPutBinary (appname, current^.name, current, 0);
            ELSE
                INIPutBinary (appname, current^.name, current^.pval^,
                                                      current^.length);
                DEALLOCATE (current^.pval, current^.length);
            END (*IF*);
            temp := current;
            current := current^.next;
            DISPOSE (temp);
        END (*WHILE*);
    END StoreApp;

(************************************************************************)

PROCEDURE LoadAllINIData(): AppPointer;

    (* Loads all data from the currently opened INI file.     *)

    VAR state: StringReadState;
        appname: TextArray;
        head, current: AppPointer;

    BEGIN
        (* Start by getting a linear list of all applications. *)

        GetStringList ('', '', state);
        head := NIL;  current := NIL;
        LOOP
            NextString (state, appname);
            IF appname[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF head = NIL THEN
                NEW (head);  current := head;
            ELSE
                NEW (current^.next);  current := current^.next;
            END (*IF*);
            current^.next := NIL;
            current^.firstkey := NIL;
            current^.name := appname;
        END (*LOOP*);
        CloseStringList (state);

        (* Now pick up all values for each application. *)

        current := head;
        WHILE current <> NIL DO
            LoadApp (current);
            current := current^.next;
        END (*WHILE*);

        RETURN head;

    END LoadAllINIData;

(************************************************************************)

PROCEDURE LoadUserList(): AppPointer;

    (* Loads the names of all applications, excluding those whose name  *)
    (* starts with '$', from the currently opened INI file.             *)

    VAR state: StringReadState;
        appname: TextArray;
        head, current: AppPointer;

    BEGIN

        GetStringList ('', '', state);
        head := NIL;  current := NIL;
        LOOP
            NextString (state, appname);
            IF appname[0] = Nul THEN
                EXIT (*LOOP*);
            ELSIF appname[0] <> '$' THEN
                IF head = NIL THEN
                    NEW (head);  current := head;
                ELSE
                    NEW (current^.next);  current := current^.next;
                END (*IF*);
                current^.next := NIL;
                current^.firstkey := NIL;
                current^.name := appname;
            END (*IF*);
        END (*LOOP*);
        CloseStringList (state);

        RETURN head;

    END LoadUserList;

(************************************************************************)

PROCEDURE SwitchToSingleINI (status: OS2.HWND);

    (* Switches from multiple INI files to a single INI file.     *)

    CONST CountBufferSize = 16;

    VAR D: DirectoryEntry;
        list, temp: AppPointer;
        count: CARDINAL;
        found: BOOLEAN;
        CountBuffer: ARRAY [0..CountBufferSize-1] OF CHAR;
        mask: ARRAY [0..127] OF CHAR;

    BEGIN
        count := 0;
        HashMax := 0;
        SetHashMax (0);
        IF UseTNI THEN
            mask := "FTPD*.TNI";
        ELSE
            mask := "FTPD*.INI";
        END (*IF*);
        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found DO
            IF UseTNI THEN
                mask := "FTPD.TNI";
            ELSE
                mask := "FTPD.INI";
            END (*IF*);
            Strings.Capitalize (D.name);
            IF NOT Strings.Equal (D.name, mask) THEN
                SetINIFileName (D.name, UseTNI);
                OpenINIFile;
                list := LoadAllINIData();
                CloseINIFile;
                DeleteFile (D.name);

                SetINIFileName (mask, UseTNI);
                OpenINIFile;
                WHILE list <> NIL DO
                    ToLower (list^.name);
                    StoreApp (list);
                    INC (count);
                    temp := list;
                    list := list^.next;
                    DISPOSE (temp);
                END (*WHILE*);
                CloseINIFile;
                CardinalToString (count, CountBuffer, CountBufferSize-1);
                CountBuffer[CountBufferSize-1] := Nul;
                OS2.WinSetWindowText (status, CountBuffer);

            END (*IF*);
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);

        OpenINIFile;
        INIPut ('$SYS', 'HashMax', HashMax);
        CloseINIFile;

    END SwitchToSingleINI;

(************************************************************************)

PROCEDURE SwitchToMultipleINI (status: OS2.HWND);

    (* Switches from a single INI file to multiple INI files.     *)

    CONST GroupSize = 20;
          CountBufferSize = 16;

    VAR list, current, temp: AppPointer;
        count, subcount: CARDINAL;
        CountBuffer: ARRAY [0..CountBufferSize-1] OF CHAR;

    BEGIN
        count := 0;
        HashMax := NewHashMax;
        SetHashMax (HashMax);
        OpenINIFile;
        INIPut ('$SYS', 'HashMax', HashMax);
        list := LoadUserList();
        LOOP
            subcount := 0;
            IF list = NIL THEN
                EXIT (*LOOP*);
            END (*IF*);
            current := list;
            WHILE (subcount <= GroupSize) AND (current <> NIL) DO
                ToLower (current^.name);
                LoadApp (current);
                CloseINIFile;
                OpenINIForUser (current^.name);
                StoreApp (current);
                CloseINIFile;
                OpenINIFile;
                INC (subcount);
                current := current^.next;
            END (*WHILE*);

            (* We have now copied one batch of users to new INI files. *)
            (* Make a second pass through the list, deleting those     *)
            (* users from the main INI file.                           *)

            WHILE list <> current DO
                INIDeleteApp (list^.name);
                temp := list;
                list := list^.next;
                DISPOSE (temp);
            END (*WHILE*);
            INC (count, subcount);
            CardinalToString (count, CountBuffer, CountBufferSize-1);
            CountBuffer[CountBufferSize-1] := Nul;
            OS2.WinSetWindowText (status, CountBuffer);

        END (*LOOP*);

        CloseINIFile;

    END SwitchToMultipleINI;

(************************************************************************)

PROCEDURE MoveUsers (hwnd: OS2.HWND;  NewMulti: BOOLEAN);

    (* Switches between a single INI file and multiple INI files,       *)
    (* depending on the state of the MultiINI checkbox.                 *)

    VAR status: OS2.HWND;

    BEGIN
        IF NewMulti <> (HashMax > 0) THEN
            status := OS2.WinWindowFromID (hwnd, DID.Page1Status);
            OS2.WinSetWindowText (status,
                                     "Converting INI data, please wait");
            IF NewMulti THEN
                SwitchToMultipleINI (status);
            ELSE
                SwitchToSingleINI (status);
            END (*IF*);
            OS2.WinSetWindowText (status, "");
        END (*IF*);
    END MoveUsers;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR val0: CARDINAL;  status: OS2.HWND;

    BEGIN
        OpenINIFile;

        (* Server port. *)

        IF INIGetCard ('$SYS', 'ServerPort', val0) THEN
            OldServerPort := val0;
        ELSE
            val0 := 21;
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.ServerPortField, val0);

        (* Maximum number of users. *)

        IF INIGetCard ('$SYS', 'MaxUsers', val0) THEN
            OldMaxUsers := val0;
        ELSE
            val0 := 10;
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.MaxUsers, val0);

        (* Maximum number of guest users. *)

        IF INIGetCard ('$SYS', 'GuestLimit', val0) THEN
            OldGuestLimit := val0;
        ELSE
            DEC(val0);
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.MaxGuestUsers, val0);

        (* Free space threshold. *)

        IF INIGetCard ('$SYS', 'SpaceThreshold', val0) THEN
            OldSpaceThreshold := val0;
        ELSE
            val0 := 10;
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.FreeSpace, val0);

        (* Timeout values. *)

        IF INIGetCard ('$SYS', 'TimeOut', val0) THEN
            OldTimeout := val0;
        ELSE
            val0 := 900;
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.TimeOut, val0);

        (* HashMax controls the MultiINI checkbox. *)

        IF NOT INIGetCard ('$SYS', 'HashMax', HashMax) THEN
            HashMax := 0;
        END (*IF*);

        (* Disable Telnet compatibility. *)

        IF NOT INIFetch ('$SYS', 'TelnetDisable', OldTelnetDisable) THEN
            OldTelnetDisable := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.TelnetDisable, OS2.BM_SETCHECK,
                               OS2.MPFROMSHORT(ORD(OldTelnetDisable)), NIL);

        CloseINIFile;

        (* Allowance for the possibility that HashMax is changing from  *)
        (* one nonzero value to another nonzero value, for example      *)
        (* because of a version change.                                 *)

        IF HashMax <> NewHashMax THEN
            status := OS2.WinWindowFromID (hwnd, DID.Page1Status);
            OS2.WinSetWindowText (status,
                                     "Converting INI data, please wait");
            val0 := HashMax;
            SwitchToSingleINI (status);
            HashMax := val0;
            IF HashMax <> 0 THEN
                SwitchToMultipleINI (status);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, DID.Page1Status, "");
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.MultiINI, OS2.BM_SETCHECK,
                               OS2.MPFROMSHORT(ORD(HashMax<>0)), NIL);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on page 1 back into the INI file. *)

    CONST MaxTimeout = (MAX(CARD32) DIV 2) DIV 1000;

    VAR value: CARDINAL;  TelnetDisable: BOOLEAN;

    BEGIN
        OpenINIFile;

        (* Server port. *)

        WinQueryDlgItemCard (hwnd, DID.ServerPortField, value);
        IF value <> OldServerPort THEN
            INIPut ('$SYS', 'ServerPort', value);
        END (*IF*);

        (* Maximum number of users. *)

        WinQueryDlgItemCard (hwnd, DID.MaxUsers, value);
        IF value <> OldMaxUsers THEN
            INIPut ('$SYS', 'MaxUsers', value);
        END (*IF*);

        (* Maximum number of guest users. *)

        WinQueryDlgItemCard (hwnd, DID.MaxGuestUsers, value);
        IF value <> OldGuestLimit THEN
            INIPut ('$SYS', 'GuestLimit', value);
        END (*IF*);

        (* Free space threshold. *)

        WinQueryDlgItemCard (hwnd, DID.FreeSpace, value);
        IF value <> OldSpaceThreshold THEN
            INIPut ('$SYS', 'SpaceThreshold', value);
        END (*IF*);

        (* Timeout value. *)

        WinQueryDlgItemCard (hwnd, DID.TimeOut, value);
        IF value > MaxTimeout THEN
            value := MaxTimeout;
            WinSetDlgItemCard (hwnd, DID.TimeOut, value);
            Sleep (1000);
        END (*IF*);
        IF value <> OldTimeout THEN
            INIPut ('$SYS', 'TimeOut', value);
        END (*IF*);

        (* Check the MultiINI checkbox and update HashMax.  This check  *)
        (* is probably redundant, because the dialogue procedure has    *)
        (* been updating HashMax as we've gone along, but it doesn't    *)
        (* hurt to have a final check.                                  *)

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.MultiINI,
                                              OS2.BM_QUERYCHECK, NIL, NIL)) = 0 THEN
            value := 0;
        ELSE
            value := NewHashMax;
        END (*IF*);
        IF value <> HashMax THEN
            INIPut ('$SYS', 'HashMax', value);
        END (*IF*);

        (* Disabling Telnet compatibility. *)

        TelnetDisable := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.TelnetDisable,
                                              OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF TelnetDisable <> OldTelnetDisable THEN
            INIPut ('$SYS', 'TelnetDisable', TelnetDisable);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR NewMulti: BOOLEAN;
        code: CARDINAL;
        (*handle: OS2.HFILE;*)

    BEGIN
        IF msg = OS2.WM_INITDLG THEN

            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadValues (hwnd);
            RETURN NIL;

        (* Next section commented out.  My attempt to provide help      *)
        (* failed because WinOpenObject is not included in OS2.DEF.     *)

        (*
        ELSIF msg = OS2.WM_COMMAND THEN

            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.help1:
                    OS2.DosOpen("doc\FtpServer.inf", handle, action, size,
                           0, OS2.OPEN_ACTION_OPEN_IF_EXISTS, 0, NIL);
                    OS2.WinOpenObject(handle, OS2.OPEN_DEFAULT, FALSE);
                    RETURN NIL;

              | ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*CASE*);
        *)

        ELSIF msg = OS2.WM_CONTROL THEN

            IF OS2.ULONGFROMMP(mp1) DIV 65536  = OS2.BN_CLICKED THEN
                IF NOT ChangeInProgress THEN
                    ChangeInProgress := TRUE;
                    NewMulti := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.MultiINI,
                                                      OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                    IF NewMulti <> (HashMax <> 0)  THEN
                        IF OS2.WinMessageBox (OS2.HWND_DESKTOP, hwnd,
                                      "This is a slow operation. Are you sure?",
                                      "Warning", 0, OS2.MB_OKCANCEL) = OS2.MBID_OK THEN
                            MoveUsers (hwnd, NewMulti);
                            ChangeInProgress := FALSE;
                            RETURN NIL;
                        ELSE
                            NewMulti := NOT NewMulti;
                            OS2.WinSendDlgItemMsg (hwnd, DID.MultiINI, OS2.BM_SETCHECK,
                                                   OS2.MPFROMSHORT(ORD(NewMulti)), NIL);
                        END (*IF*);
                    END (*IF*);
                    ChangeInProgress := FALSE;
                END (*IF*);
            END (*IF*);

        ELSIF msg = OS2.WM_CHAR THEN

            (* Explicitly ignore Esc key, because the default processing  *)
            (* was causing problems.                                      *)

            code := OS2.ULONGFROMMP(mp1);
            IF (IAND (code, OS2.KC_VIRTUALKEY) <> 0) THEN
                code := (OS2.ULONGFROMMP(mp2) DIV 65536) MOD 256;
                IF code = OS2.VK_ESC THEN
                    (* Say we've already handled it. *)
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF NOT ChangeInProgress THEN
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*CASE*);

        (* Fall-through default for all cases not handled above. *)

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  TNImode: BOOLEAN;
                       VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates page 1 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.BasicPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        Label := "~Basic";
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
    OldServerPort := 0;
    OldMaxUsers := 0;
    OldGuestLimit := 0;
    OldSpaceThreshold := 0;
    OldTimeout := 0;
    UseTNI := FALSE;
    OldTelnetDisable := FALSE;
    ChangeInProgress := FALSE;
END BasicPage.

