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

IMPLEMENTATION MODULE BigFrame;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for FtpServer                *)
        (*             The settings notebook and its frame          *)
        (*                                                          *)
        (*    Started:        8 October 1999                        *)
        (*    Last edited:    22 October 2019                       *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, OS2RTL;

IMPORT DID, BasicPage, LogPage, OptionPage, LocalAddrs, Security, UserPage,
        Strings, CommonSettings, INIData;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST, ADR;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM RINIData IMPORT
    (* proc *)  RemoteOperation;

FROM Remote IMPORT
    (* proc *)  SelectRemoteFile;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

FROM FSUINI IMPORT
    (* proc *)  TNImode;

(**************************************************************************)

CONST
    NewStyle = TRUE;
    Nul = CHR(0);
    SetupINIname0 = "Setup";

TYPE
    Page = (pbase, plog, poption, plocal, psecurity, puser);

VAR
    SetupINIname: ARRAY [0..127] OF CHAR;
    pagehandle: ARRAY Page OF OS2.HWND;
    IDofPage: ARRAY Page OF CARDINAL;
    StartingPage: Page;
    ChangeInProgress, LocalPagePresent: BOOLEAN;
    PageFont, TabFontName: CommonSettings.FontName;

(**************************************************************************)

PROCEDURE SetPageFonts;

    (* Changes the font of the notebook pages to the font recorded in the *)
    (* INI file as belonging to this notebook.                            *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;
            BasicPage.SetFont (PageFont);
            LogPage.SetFont (PageFont);
            OptionPage.SetFont (PageFont);
            LocalAddrs.SetFont (PageFont);
            Security.SetFont (PageFont);
            UserPage.SetFont (PageFont);
        END (*IF*);
    END SetPageFonts;

(**************************************************************************)

PROCEDURE MakeNotebookNewStyle (hwnd: OS2.HWND;  NewStyle: BOOLEAN);

    (* Change to Warp 3 or Warp 4 notebook style. *)

    CONST
        OldStyleFlags = OS2.BKS_BACKPAGESBR + OS2.BKS_MAJORTABBOTTOM
                + OS2.BKS_ROUNDEDTABS + OS2.BKS_TABTEXTCENTER
                + OS2.BKS_STATUSTEXTCENTER + OS2.BKS_SPIRALBIND;
        NewStyleFlags = OS2.BKS_TABBEDDIALOG + OS2.BKS_MAJORTABTOP + OS2.BKS_BACKPAGESTR;

    VAR style: CARDINAL;

    BEGIN
        style := OS2.WinQueryWindowULong (hwnd, OS2.QWL_STYLE);
        style := IAND (style, 0FFFF0000H);
        IF NewStyle THEN
            INC (style, NewStyleFlags);
        ELSE
            INC (style, OldStyleFlags);
        END (*IF*);
        OS2.WinSetWindowULong (hwnd, OS2.QWL_STYLE, style);
    END MakeNotebookNewStyle;

(**************************************************************************)

PROCEDURE InitialiseNotebook (hwnd: OS2.HWND);

    (* hwnd is the handle of the notebook control. *)

    VAR swp: OS2.SWP;  scale: CARDINAL;
        owner: OS2.HWND;  hini: INIData.HINI;
        NewStyle: BOOLEAN;
        app: ARRAY [0..12] OF CHAR;
        colour, background: CARDINAL;

    BEGIN
        (* Find OS version to decide what notebook style to use. *)

        scale := 30;
        OS2.DosQuerySysInfo(12, 12, ADR(scale), SIZE(CARDINAL));
        NewStyle := scale >= 40;
        MakeNotebookNewStyle (hwnd, NewStyle);

        hini := INIData.OpenINIFile (SetupINIname);
        app := "StartingPage";
        IF NOT INIData.INIGet (hini, app, "MainNotebook", StartingPage) THEN
            StartingPage := MIN(Page);
        END (*IF*);
        app := "Font";
        IF NOT INIData.INIGetString (hini, app, "MainNotebookTabs", TabFontName) THEN
            TabFontName := "8.Helv";
        END (*IF*);
        INIData.CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);

        (* If the new style is enabled, the following code will have no effect *)
        (* because the messages to set tab size and colours will be ignored.   *)

        OS2.WinQueryWindowPos (hwnd, swp);
        scale := 2*swp.cx DIV 19;
        OS2.WinSendMsg (hwnd, OS2.BKM_SETDIMENSIONS,
             OS2.MPFROM2SHORT(scale,scale DIV 2), OS2.MPFROMSHORT(OS2.BKA_MAJORTAB));
        colour := 00FFFFAAH(*0055DBFFH*);
        background := OS2.BKA_BACKGROUNDPAGECOLOR;
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        CAST (ADDRESS, colour), CAST(ADDRESS,background));
        colour := 0080DBAAH;
        background := OS2.BKA_BACKGROUNDPAGECOLOR;
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        CAST(ADDRESS,colour), CAST(ADDRESS,background));

        (* Begin with the local page absent, and let the appropriate    *)
        (* lower-level module request its insertion if needed.          *)

        pagehandle[pbase] := BasicPage.CreatePage(hwnd, IDofPage[pbase]);
        pagehandle[plog] := LogPage.CreatePage(hwnd, IDofPage[plog]);
        pagehandle[poption] := OptionPage.CreatePage(hwnd, IDofPage[poption]);
        LocalPagePresent := FALSE;
        pagehandle[psecurity] := Security.CreatePage(hwnd, IDofPage[psecurity]);
        pagehandle[puser] := UserPage.CreatePage(hwnd, IDofPage[puser]);
        SetPageFonts;
        OS2.WinSendMsg (hwnd, OS2.BKM_TURNTOPAGE,
                           OS2.MPFROMULONG(IDofPage[StartingPage]), NIL);
        OS2.WinShowWindow (hwnd, TRUE);

        (* The parent of this window is the frame.  The owner of that   *)
        (* frame is the window we want to hide.                         *)

        owner := OS2.WinQueryWindow (hwnd, OS2.QW_PARENT);
        owner := OS2.WinQueryWindow (owner, OS2.QW_OWNER);
        OS2.WinShowWindow (owner, FALSE);

    END InitialiseNotebook;

(************************************************************************)

PROCEDURE InsertLocalAddrsPage (hwnd: OS2.HWND;  Insert, new: BOOLEAN);

    (* Adds or removes the LocalAddrs notebook page.  The "new"         *)
    (* parameter indicates that "behind firewall" rules have just been  *)
    (* activated, as distinct from an activation at program startup.    *)

    VAR notebook: OS2.HWND;

    BEGIN
        notebook := OS2.WinWindowFromID(hwnd, DID.notebook);
        IF LocalPagePresent <> Insert THEN

            IF Insert THEN

                (* Insert the LocalAddrs page. *)

                pagehandle[plocal] := LocalAddrs.CreatePage(notebook,
                                      IDofPage[poption], IDofPage[plocal]);
                IF new THEN
                    OS2.WinPostMsg (notebook, OS2.BKM_TURNTOPAGE,
                         OS2.MPFROMULONG(IDofPage[plocal]), NIL);
                END (*IF*);

            ELSE

                (* Close the LocalAddrs page. *)

                LocalAddrs.Close(notebook, pagehandle[plocal], IDofPage[plocal]);
                IDofPage[plocal] := 0;

            END (*IF*);

            LocalPagePresent := Insert;

        END (*IF*);

    END InsertLocalAddrsPage;

(**************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the notebook subwindow.  We want this here mainly so that we can *)
    (* detect a new font dropped on the notebook tabs.  If the message  *)
    (* is something we don't want to deal with here, we pass it         *)
    (* to the parent window procedure.                                  *)

    VAR OldWndProc: OS2.PFNWP;
        owner: OS2.HWND;  hini: INIData.HINI;
        length, AttrFound: CARDINAL;
        NewFontName: CommonSettings.FontName;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        OldWndProc := CAST (OS2.PFNWP, OS2.WinQueryWindowPtr (hwnd, OS2.QWL_USER));
        owner := OS2.WinQueryWindow(hwnd,OS2.QW_OWNER);

        (* Because of the interaction between subclassing and DragText, *)
        (* some messages will go lost if we use the obvious strategy of *)
        (* sending them through to OldWndProc.  To get around this, we  *)
        (* have to send those messages directly to the target window.   *)

        IF (msg = OS2.WM_BUTTON2DOWN) OR (msg = OS2.DM_DRAGOVER)
                   OR (msg = OS2.DM_DRAGLEAVE) OR (msg = OS2.DM_DROP) THEN

            RETURN OS2.WinSendMsg (owner, msg, mp1, mp2);

        (* Check for font or colour change. *)

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound, CommonSettings.FontNameSize, NewFontName,
                                              0(*OS2.QPF_NOINHERIT*));
                IF length < CommonSettings.FontNameSize THEN
                    NewFontName[length] := Nul;
                END (*IF*);

                IF NOT Strings.Equal (NewFontName, TabFontName) THEN
                    TabFontName := NewFontName;
                    hini := INIData.OpenINIFile (SetupINIname);
                    app := "Font";
                    INIData.INIPutString (hini, app, "MainNotebookTabs", TabFontName);
                    INIData.CloseINIFile (hini);
                    OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
                END (*IF*);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(**************************************************************************)
(*                 WINDOW PROCEDURE FOR THE MAIN FRAME                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR bookwin: OS2.HWND;
        pageID: CARDINAL;  pg: Page;
        hini: INIData.HINI;
        stringval, filename: ARRAY [0..255] OF CHAR;
        app: ARRAY [0..12] OF CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   SetInitialWindowPosition (hwnd, SetupINIname, "BigFrame");
                   IF TNImode() THEN
                       filename := "FTPD.TNI";
                   ELSE
                       filename := "FTPD.INI";
                   END (*IF*);
                   IF RemoteOperation() THEN
                       IF SelectRemoteFile('FTPD.INI') THEN
                           Strings.Assign ("Remote FtpServer Setup", stringval);
                       ELSE
                           Strings.Assign ("Can't open remote INI file", stringval);
                       END (*IF*);
                   ELSE
                       Strings.Assign ("Local FtpServer Setup", stringval);
                   END (*IF*);
                   Strings.Append ("      ", stringval);
                   Strings.Append (filename, stringval);
                   OS2.WinSetWindowText (hwnd, stringval);
                   bookwin := OS2.WinWindowFromID (hwnd, DID.notebook);
                   InitialiseNotebook (bookwin);
                   OS2.WinSetWindowPtr (bookwin, OS2.QWL_USER,
                               CAST(ADDRESS,OS2.WinSubclassWindow (bookwin,
                                                           SubWindowProc)));

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  CommonSettings.BEHINDFIREWALL:
                   InsertLocalAddrsPage (hwnd, OS2.LONGFROMMR(mp1) > 0,
                                                OS2.LONGFROMMR(mp2) > 0);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_CLOSE:
                   bookwin := OS2.WinWindowFromID(hwnd, DID.notebook);
                   pageID := OS2.ULONGFROMMR(OS2.WinSendMsg (bookwin, OS2.BKM_QUERYPAGEID,
                                 OS2.MPFROMULONG(0),
                                  OS2.MPFROMSHORT(OS2.BKA_TOP)));
                   pg := MAX(Page);
                   WHILE (IDofPage[pg] <> pageID) AND (pg > MIN(Page)) DO
                       DEC (pg);
                   END (*WHILE*);
                   hini := INIData.OpenINIFile (SetupINIname);
                   app := "StartingPage";
                   INIData.INIPut (hini, app, "MainNotebook", pg);
                   INIData.CloseINIFile (hini);
                   StoreWindowPosition (hwnd, SetupINIname, "BigFrame");
                   BasicPage.StoreData (pagehandle[pbase]);
                   LogPage.StoreData (pagehandle[plog]);
                   OptionPage.StoreData (pagehandle[poption]);
                   LocalAddrs.StoreData (pagehandle[plocal]);
                   Security.StoreData (pagehandle[psecurity]);
                   UserPage.StoreData (pagehandle[puser]);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
        RETURN NIL;
    END MainDialogueProc;

(**************************************************************************)

PROCEDURE OpenBigFrame (owner: OS2.HWND);

    (* Creates the main dialogue box. *)

    BEGIN
        SetupINIname := SetupINIname0;
        IF TNImode() THEN
            Strings.Append (".TNI", SetupINIname);
        ELSE
            Strings.Append (".INI", SetupINIname);
        END (*IF*);
        OS2.WinDlgBox(OS2.HWND_DESKTOP, owner,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.BigFrame,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
    END OpenBigFrame;

(**************************************************************************)

VAR pg: Page;

BEGIN
    ChangeInProgress := FALSE;
    PageFont := "";
    StartingPage := MIN(Page);
    FOR pg := MIN(Page) TO MAX(Page) DO
        IDofPage[pg] := 0;
    END (*FOR*);
END BigFrame.

