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

IMPLEMENTATION MODULE OpeningDialogue;

        (****************************************************************)
        (*                   PM Setup for FtpServer                     *)
        (*                    Initial dialogue box                      *)
        (*                                                              *)
        (*    Started:        7 October 1999                            *)
        (*    Last edited:    7 January 2013                            *)
        (*    Status:         Working                                   *)
        (*                                                              *)
        (****************************************************************)

IMPORT SYSTEM, OS2, OS2RTL, DID, Remote, BigFrame, PMInit;

FROM RINIData IMPORT
    (* proc *)  SetRemote;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL;

(************************************************************************)

VAR RemoteFlag, UseTNI: BOOLEAN;
    SwitchData : OS2.SWCNTRL;     (* switch entry data *)

(************************************************************************)

PROCEDURE PostUpdated (semName: ARRAY OF CHAR);

    (* Posts on a public event semaphore. *)

    VAR changehev: OS2.HEV;  count: CARDINAL;

    BEGIN
        changehev := 0;
        IF OS2.DosOpenEventSem (semName, changehev) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, changehev, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (changehev);
        OS2.DosResetEventSem (changehev, count);
        OS2.DosCloseEventSem(changehev);
    END PostUpdated;

(************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    CONST UpdateSemName = "\SEM32\FTPSERVER\UPDATED";

    VAR ButtonID, NotificationCode: CARDINAL;

    BEGIN
        CASE msg OF
           |  OS2.WM_CLOSE:
                   OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.SetupButton:
                          Remote.OpenSetupDialogue (hwnd);

                     | DID.GoButton:
                          IF NOT RemoteFlag OR Remote.ConnectToServer (hwnd, DID.Status) THEN
                              OS2.WinSetDlgItemText (hwnd, DID.Status, "Loading, please wait");
                              SetRemote (RemoteFlag);
                              BigFrame.OpenBigFrame (hwnd, UseTNI);
                              IF RemoteFlag THEN
                                  EVAL(Remote.PostSemaphore (UpdateSemName));
                                  EVAL(Remote.ExecCommand ('Q'));
                              ELSE
                                  PostUpdated (UpdateSemName);
                              END (*IF*);
                              Remote.SaveRemoteFlag (RemoteFlag);
                              OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);
                          END (*IF*);

                     | DID.ExitButton:
                          Remote.SaveRemoteFlag (RemoteFlag);
                          OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

                   ELSE
                          RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

           |  OS2.WM_CONTROL:
                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF NotificationCode = OS2.BN_CLICKED THEN
                       CASE ButtonID OF
                         | DID.RemoteButton:
                              RemoteFlag := TRUE;
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SetupButton), TRUE);
                         | DID.LocalButton:
                              RemoteFlag := FALSE;
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SetupButton), FALSE);
                       END (*CASE*);

                   END (*IF*);

        ELSE    (* default must call WinDefWindowProc() *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        RETURN NIL;

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE CreateMainDialogue (LocalRemote: CARDINAL;  TNImode: BOOLEAN);

    (* Creates the main dialogue box.  The meaning of LocalRemote is:    *)
    (*         0   let user specify local or remote                      *)
    (*         1   force local setup                                     *)
    (*         2   force remote setup                                    *)
    (*         3   force whichever was used last time                    *)

    VAR hwnd, SetupButtonWindow: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;

    BEGIN
        UseTNI := TNImode;
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP,    (* parent *)
                       OS2.HWND_DESKTOP,   (* owner *)
                       MainDialogueProc,   (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.FirstWindow,    (* dialogue ID *)
                       NIL);               (* creation parameters *)

        (* Put us on the visible task list. *)

        OS2.WinQueryWindowProcess (hwnd, pid, tid);
        SwitchData.hwnd := hwnd;
        WITH SwitchData DO
            hwndIcon      := 0;
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "FtpServer Setup";
            bProgType     := 0;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (PMInit.OurHab(), SwitchData);

        RemoteFlag := Remote.InitialSetup (NIL, "FtpServer",
                                 "Setup", "C:\Servers\FtpServer", UseTNI);
        Remote.SetInitialWindowPosition (hwnd, "Opening");

        (* Set the local/remote check buttons correctly. *)

        OS2.WinSendDlgItemMsg (hwnd, DID.LocalButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(1-ORD(RemoteFlag)), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.RemoteButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(ORD(RemoteFlag)), NIL);
        SetupButtonWindow := OS2.WinWindowFromID(hwnd, DID.SetupButton);

        (* The following coding is admittedly a little weird, but it    *)
        (* was the only way I could get it to work correctly.  For some *)
        (* reason making the second parameter a variable gives          *)
        (* unreliable results.                                          *)

        IF RemoteFlag THEN
            OS2.WinEnableWindow (SetupButtonWindow, TRUE);
        ELSE
            OS2.WinEnableWindow (SetupButtonWindow, FALSE);
        END (*IF*);

        IF LocalRemote = 1 THEN
            OS2.WinSendMsg (hwnd, OS2.WM_CONTROL,
                   OS2.MPFROM2SHORT(DID.LocalButton, OS2.BN_CLICKED), NIL);
        ELSIF LocalRemote = 2 THEN
            OS2.WinSendMsg (hwnd, OS2.WM_CONTROL,
                   OS2.MPFROM2SHORT(DID.RemoteButton, OS2.BN_CLICKED), NIL);
        END (*IF*);

        IF LocalRemote > 0 THEN
            OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.GoButton), NIL);
        END (*IF*);

        OS2.WinProcessDlg(hwnd);
        Remote.StoreWindowPosition (hwnd, "Opening", TRUE);
        OS2.WinDestroyWindow (hwnd);

    END CreateMainDialogue;

(**************************************************************************)

BEGIN
    UseTNI := FALSE;
END OpeningDialogue.

