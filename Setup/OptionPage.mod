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

IMPLEMENTATION MODULE OptionPage;

        (****************************************************************)
        (*                                                              *)
        (*                    PM Setup for FtpServer                    *)
        (*                    Page 2 of the notebook                    *)
        (*                                                              *)
        (*        Started:        10 October 1999                       *)
        (*        Last edited:    23 October 2019                       *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings;

FROM FSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIFetch, INIGetCard, INIPut, ServerIPAddress;

FROM IPFilters IMPORT
    (* proc *)  CardinalToDotted, StringToIPAddress;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4;

(**************************************************************************)

VAR
    pagehandle, hwndParent: OS2.HWND;
    OldBindAddr: CARDINAL;
    OldCheckTaggers: BOOLEAN;
    OldHidePasswords: BOOLEAN;
    OldBehindFirewall: BOOLEAN;
    OldLimitPASVPorts: BOOLEAN;
    PortLimitNeverSet: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    OldMinPort, OldMaxPort: CARD16;

(**************************************************************************)

PROCEDURE EnablePortRangeFields (hwnd: OS2.HWND;  enable: BOOLEAN);

    (* Enables or disables the entry fields associated with the         *)
    (* option of limiting the PASV port range.                          *)

    BEGIN
        IF enable THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.MinPort), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.MaxPort), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.MinPort), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.MaxPort), FALSE);
        END (*IF*);
    END EnablePortRangeFields;

(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR AddressString: ARRAY [0..15] OF CHAR;
        value, OurHostID: CARDINAL;  val16: CARD16;  boolval: BOOLEAN;

    BEGIN
        OpenINIFile;
        OurHostID := ServerIPAddress();

        (* Get Bind Address.  Zero means "bind to all interfaces". *)

        IF INIGetCard ('$SYS', 'BindAddr', value) THEN
            OldBindAddr := value;
        ELSE
            value := 0;
        END (*IF*);

        (* The coding below looks redundant, but I couldn't get it to           *)
        (* work when written in the more obvious way.  It's possibly due to     *)
        (* the differences in the way Booleans are encoded in C and Modula-2.   *)

        IF value = 0 THEN

            OS2.WinSendDlgItemMsg (hwnd, DID.BindAllButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(1), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.BindSpecificButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(0), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), FALSE);
            value := OurHostID;

        ELSE

            OS2.WinSendDlgItemMsg (hwnd, DID.BindAllButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(0), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.BindSpecificButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(1), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), TRUE);

        END (*IF*);

        CardinalToDotted (value, AddressString);
        OS2.WinSetDlgItemText (hwnd, DID.IPAddress, AddressString);

        (* Check taggers (Boolean value). *)

        IF INIFetch ('$SYS', 'CheckTaggers', boolval) THEN
            OldCheckTaggers := boolval;
        ELSE
            boolval := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTaggers, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(boolval)), NIL);

        (* Hide passwords (Boolean value). *)

        IF INIFetch ('$SYS', 'HidePasswords', boolval) THEN
            OldHidePasswords := boolval;
        ELSE
            boolval := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.HidePasswords, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(boolval)), NIL);

        (* BehindFirewall (Boolean value). *)

        IF INIFetch ('$SYS', 'BehindFirewall', boolval) THEN
            OldBehindFirewall := boolval;
        ELSE
            boolval := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.BehindFirewall, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(boolval)), NIL);
        OS2.WinPostMsg (hwndParent, CommonSettings.BEHINDFIREWALL,
                            OS2.MPFROMLONG(ORD(boolval)), OS2.MPFROMLONG(0));

        (* Option to limit the PASV port range (Boolean value). *)

        IF INIFetch ('$SYS', 'LimitPASVPorts', boolval) THEN
            OldLimitPASVPorts := boolval;
            PortLimitNeverSet := FALSE;
        ELSE
            boolval := OldBehindFirewall;
            IF boolval THEN
                INIPut ('$SYS', 'LimitPASVPorts', boolval);
                OldLimitPASVPorts := TRUE;
            END (*IF*);
            PortLimitNeverSet := NOT boolval;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LimitPASVPorts, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(boolval)), NIL);
        EnablePortRangeFields (hwnd, boolval);

        (* The numeric limits on the PASV port range. *)

        IF INIFetch ('$SYS', 'MinPort', val16) THEN
            OldMinPort := val16;
        ELSE
            val16 := Swap2(49152);
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.MinPort, Swap2(val16));

        IF INIFetch ('$SYS', 'MaxPort', val16) THEN
            OldMaxPort := val16;
        ELSE
            val16 := Swap2(65535);
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.MaxPort, Swap2(val16));

        (* Numeric values associated with BehindFirewall option. *)

        CloseINIFile;

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on page 2 back into the INI file. *)

    VAR TextBuffer: ARRAY [0..15] OF CHAR;
        value : CARDINAL;  val16: CARD16;  boolval: BOOLEAN;

    BEGIN
        OpenINIFile;

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.BindAllButton,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            value := 0;
        ELSE
            OS2.WinQueryDlgItemText (hwnd, DID.IPAddress, 16, TextBuffer);
            value := StringToIPAddress (TextBuffer);
        END (*IF*);

        IF value <> OldBindAddr THEN
            INIPut ('$SYS', 'BindAddr', value);
        END (*IF*);

        (* Check taggers (Boolean value). *)

        boolval := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.CheckTaggers,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF boolval <> OldCheckTaggers THEN
            INIPut ('$SYS', 'CheckTaggers', boolval);
        END (*IF*);

        (* Hide passwords (Boolean value). *)

        boolval := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.HidePasswords,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF boolval <> OldHidePasswords THEN
            INIPut ('$SYS', 'HidePasswords', boolval);
        END (*IF*);

        (* Option to limit the PASV port range (Boolean value). *)

        boolval := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.LimitPASVPorts,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF boolval <> OldLimitPASVPorts THEN
            INIPut ('$SYS', 'LimitPASVPorts', boolval);
        END (*IF*);

        (* Numeric values for the PASV port restriction. *)

        WinQueryDlgItemCard (hwnd, DID.MinPort, value);
        val16 := Swap2(VAL(CARD16, value));
        IF val16 <> OldMinPort THEN
            INIPut ('$SYS', 'MinPort', val16);
        END (*IF*);

        WinQueryDlgItemCard (hwnd, DID.MaxPort, value);
        val16 := Swap2(VAL(CARD16, value));
        IF val16 <> OldMaxPort THEN
            INIPut ('$SYS', 'MaxPort', val16);
        END (*IF*);

        (* BehindFirewall (Boolean value). *)

        boolval := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.BehindFirewall,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF boolval <> OldBehindFirewall THEN
            INIPut ('$SYS', 'BehindFirewall', boolval);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode, ButtonID, code: CARDINAL;
        ButtonValue: BOOLEAN;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                CASE ButtonID OF
                  | DID.BindSpecificButton:
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), TRUE);
                       RETURN NIL;
                  | DID.BindAllButton:
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), FALSE);
                       RETURN NIL;
                  | DID.LimitPASVPorts:
                       ButtonValue := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.LimitPASVPorts,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                       IF ButtonValue AND PortLimitNeverSet THEN
                           PortLimitNeverSet := FALSE;
                           OpenINIFile;
                           INIPut ('$SYS', 'LimitPASVPorts', ButtonValue);
                           OldLimitPASVPorts := TRUE;
                           CloseINIFile;
                       END (*IF*);
                       EnablePortRangeFields (hwnd, ButtonValue);
                       RETURN NIL;
                  | DID.BehindFirewall:
                       ButtonValue := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.BehindFirewall,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                       IF ButtonValue AND PortLimitNeverSet THEN
                           EnablePortRangeFields (hwnd, TRUE);
                           OS2.WinSendDlgItemMsg (hwnd, DID.LimitPASVPorts, OS2.BM_SETCHECK,
                                       OS2.MPFROMSHORT(ORD(TRUE)), NIL);
                       END (*IF*);
                       OS2.WinPostMsg (hwndParent, CommonSettings.BEHINDFIREWALL,
                                OS2.MPFROMLONG(ORD(ButtonValue)), OS2.MPFROMSHORT(1));
                       RETURN NIL;
                  | DID.HidePasswords:
                       ButtonValue := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.HidePasswords,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                       OpenINIFile;
                       INIPut ('$SYS', 'HidePasswords', ButtonValue);
                       CloseINIFile;
                       OldHidePasswords := ButtonValue;
                       RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*CASE*);

            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
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

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates page 2 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.page2,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        Label := "~Options";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        LoadValues (pagehandle);
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
    OldBindAddr := 0;
    OldCheckTaggers := FALSE;
    OldHidePasswords := FALSE;
    OldBehindFirewall := FALSE;
    OldLimitPASVPorts := FALSE;
    PortLimitNeverSet := TRUE;
    OldMinPort := Swap2(49152);
    OldMaxPort := Swap2(65535);
    ChangeInProgress := FALSE;
END OptionPage.

