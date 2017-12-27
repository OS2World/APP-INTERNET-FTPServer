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

IMPLEMENTATION MODULE LogPage;

        (****************************************************************)
        (*                                                              *)
        (*                    PM Setup for FtpServer                    *)
        (*                 Logging page of the notebook                 *)
        (*                                                              *)
        (*        Started:        10 June 2003                          *)
        (*        Last edited:    22 May 2017                           *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, INT16, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings;

FROM FSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  RemoteOperation, INIPut, INIPutString,
                INIFetch, INIGetCard, INIGetString;

FROM Remote IMPORT
    (* proc *)  OurDirectory;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM Names IMPORT
    (* type *)  UserName, UserNameIndex, FilenameString;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST Nul = CHR(0);

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;
    DirectoryString = ARRAY [0..511] OF CHAR;
    TextArray = ARRAY [0..31] OF CHAR;
    TextPointer = POINTER TO TextArray;

VAR
    ChangeInProgress: BOOLEAN;
    pagehandle: OS2.HWND;

    (* Original values of the ini variables. *)

    OldLogLevel, OldTransLevel: CARDINAL;
    OldUserLogName, OldCommonLogName,
                     OldTransLogName, OldSyslogHost: FilenameString;
    OldLogSITEMNGR: BOOLEAN;

    (* Text for a spinbutton control. *)

    LogDetailText: ARRAY [0..3] OF TextArray;
    LogDetailPtr: ARRAY [0..3] OF TextPointer;

(************************************************************************)
(*                    RESPONDING TO CHECKBOX VALUES                     *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE EnableFields (hwnd: OS2.HWND);

    (* Enables or disables the filename windows, depending on the       *)
    (* checkbox values.                                                 *)

    BEGIN
        IF QueryButton (hwnd, DID.DiskLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TransLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TransLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.UserLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UserLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UserLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.CommonLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CommonLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.CommonLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.SysLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SyslogHost), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SyslogHost), FALSE);
        END (*IF*);
    END EnableFields;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on the log page with data from the   *)
    (* INI file, or loads default values if they're not in the INI file.*)

    VAR val0, temp: CARDINAL;
        name: FilenameString;

    BEGIN
        OpenINIFile;

        (* Transfer log level. *)

        IF INIGetCard ('$SYS', 'LogLevel', val0) THEN
            OldLogLevel := val0;
        ELSE
            val0 := 1;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogDetail, OS2.SPBM_SETCURRENTVALUE,
                                    OS2.MPFROMSHORT(val0), NIL);

        (* "Enable logging" flags.  The value in the INI file is        *)
        (* effectively an array of eight Booleans:                      *)
        (*    bit 0          transaction log to disk   (obsolescent)    *)
        (*    bit 1          transaction log to screen (obsolescent)    *)
        (*    bit 2          enable user transfer logging               *)
        (*    bit 3          enable common.log transfer logging         *)
        (*    bit 4          transaction log to disk                    *)
        (*    bit 5          transaction log to screen                  *)
        (*    bit 6          transaction log to pipe                    *)
        (*    bit 7          transaction log to syslog                  *)
        (* That is, TransLevel DIV 16 controls the transaction logging, *)
        (* while TransLevel MOD 16 DIV 4 controls the transfer logging. *)
        (* The redundant bits 0 and 1 are a transition arrangement;     *)
        (* originally bits 0 and 1 controlled transaction logging, so   *)
        (* we have to take the OR of two ways to specify the disk and   *)
        (* screen options.                                              *)

        IF INIGetCard ('$SYS', 'TransLevel', val0) THEN
            OldTransLevel := val0;
        ELSE
            val0 := 52;
        END (*IF*);
        temp := val0 MOD 4;
        val0 := IOR (val0 DIV 4, 4*temp);
        OS2.WinSendDlgItemMsg (hwnd, DID.UserLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.CommonLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.DiskLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.ScreenLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.PipeLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.SysLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);

        (* Log SITE MNGR commands. *)

        IF NOT INIFetch ('$SYS', 'LogSITEMNGR', OldLogSITEMNGR) THEN
            OldLogSITEMNGR := TRUE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogSITEMNGR, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(OldLogSITEMNGR)), NIL);

        (* Log file names. *)

        IF NOT INIGetString ('$SYS', 'TransLogName', OldTransLogName) THEN
            OldTransLogName := 'FTPTRANS.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.TransLogName, OldTransLogName);

        IF NOT INIGetString ('$SYS', 'CommonLogName', OldCommonLogName) THEN
            OldCommonLogName := 'COMMON.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.CommonLogName, OldCommonLogName);

        IF NOT INIGetString ('$SYS', 'UserLogName', OldUserLogName) THEN
            OldUserLogName := 'FTPUSERS.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.UserLogName, OldUserLogName);

        IF INIGetString ('$SYS', 'SyslogHost', name) THEN
            OldSyslogHost := name;
        ELSE
            OldSyslogHost := '';
            name := '127.0.0.1';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.SyslogHost, name);

        CloseINIFile;
        EnableFields (hwnd);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on the log page back into the INI file. *)

    VAR value: CARDINAL;
        filename: FilenameString;
        bool: BOOLEAN;

    BEGIN
        OpenINIFile;

        (* Transfer log level. *)

        value := 0;
        OS2.WinSendDlgItemMsg (hwnd, DID.LogDetail, OS2.SPBM_QUERYVALUE,
                        ADR(value), OS2.MPFROM2SHORT(0,0));
        IF value <> OldLogLevel THEN
            INIPut ('$SYS', 'LogLevel', value);
        END (*IF*);

        (* "Enable logging" flags.  The value in the INI file is *)
        (* effectively an array of six Booleans.                 *)

        value := 4*( QueryButton (hwnd, DID.UserLog)
                   + 2 * QueryButton (hwnd, DID.CommonLog)
                   + 4 * QueryButton (hwnd, DID.DiskLog)
                   + 8 * QueryButton (hwnd, DID.ScreenLog)
                   + 16 * QueryButton (hwnd, DID.PipeLog)
                   + 32 * QueryButton (hwnd, DID.SysLog) );
        IF value <> OldTransLevel THEN
            INIPut ('$SYS', 'TransLevel', value);
        END (*IF*);

        (* Log SITE MNGR commands. *)

        bool := QueryButton (hwnd, DID.LogSITEMNGR) > 0;
        IF bool <> OldLogSITEMNGR THEN
            INIPut ('$SYS', 'LogSITEMNGR', bool);
        END (*IF*);

        (* Log file names. *)

        OS2.WinQueryDlgItemText (hwnd, DID.TransLogName, SIZE(filename),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldTransLogName) THEN
            INIPutString ('$SYS', 'TransLogName', filename);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.CommonLogName, SIZE(filename),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldCommonLogName) THEN
            INIPutString ('$SYS', 'CommonLogName', filename);
        END (*IF*);
        OS2.WinQueryDlgItemText (hwnd, DID.UserLogName, SIZE(filename),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldUserLogName) THEN
            INIPutString ('$SYS', 'UserLogName', filename);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.SyslogHost, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldSyslogHost) THEN
            INIPutString ('$SYS', 'SyslogHost', filename);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode, code: CARDINAL;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN

            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinSendDlgItemMsg (hwnd, DID.LogDetail, OS2.SPBM_SETARRAY,
                        ADR(LogDetailPtr), OS2.MPFROMSHORT(4));
            LoadValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1) DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                EnableFields (hwnd);
                RETURN NIL;
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

        END (*IF*);

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the log page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.LogPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        Label := "~Logging";
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

VAR j: CARDINAL;

BEGIN
    OldLogLevel := 0;
    OldTransLevel := 0;
    OldLogSITEMNGR := FALSE;
    LogDetailText[0] := "No transfer logging";
    LogDetailText[1] := "Log successful transfers";
    LogDetailText[2] := "Log all transfers";
    LogDetailText[3] := "Log all clients";
    FOR j := 0 TO 3 DO
        LogDetailPtr[j] := ADR(LogDetailText[j]);
    END (*FOR*);
    ChangeInProgress := FALSE;
END LogPage.

