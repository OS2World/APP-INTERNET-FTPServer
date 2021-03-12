(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
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

IMPLEMENTATION MODULE KillServer;

        (****************************************************************)
        (*                                                              *)
        (*                    Monitor for FtpServer                     *)
        (*                    "Kill server" dialogue                    *)
        (*                                                              *)
        (*    Started:        3 April 2000                              *)
        (*    Last edited:    20 November 2020                          *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)

IMPORT OS2, DID, INIData;

FROM MonINI IMPORT
    (* proc *)  GetINIFileName;

(************************************************************************)

VAR ResultCode: CARDINAL;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;

    BEGIN
        CASE msg OF

           |  OS2.WM_CLOSE:
                   ResultCode := 0;
                   OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.ShutdownOK:
                          OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

                     | DID.ShutdownCancel, OS2.DID_CANCEL:
                          ResultCode := 0;
                          OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

                   ELSE    (* default must call WinDefWindowProc() *)
                      RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

           |  OS2.WM_CONTROL:
                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF NotificationCode = OS2.BN_CLICKED THEN
                       CASE ButtonID OF
                         | DID.GradualShutdown:
                              ResultCode := 1;
                         | DID.QuickShutdown:
                              ResultCode := 2;
                       END (*CASE*);

                   END (*IF*);

        ELSE    (* default must call WinDefWindowProc() *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        RETURN NIL;

    END DialogueProc;

(**************************************************************************)

PROCEDURE RunDialogue (parent: OS2.HWND): CARDINAL;

    (* Runs the "kill server" dialogue.  The result code is:    *)
    (*     0    cancel                                          *)
    (*     1    gradual shutdown                                *)
    (*     2    quick shutdown                                  *)

    VAR hwnd: OS2.HWND;
        ININame: ARRAY [0..511] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg (OS2.HWND_DESKTOP, parent,
                       DialogueProc,   (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.KillServerDialogue,    (* dialogue ID *)
                       NIL);               (* creation parameters *)

        GetINIFileName (ININame);
        INIData.SetInitialWindowPosition (hwnd, ININame, "KillServer");

        (* Set the check buttons correctly. *)

        OS2.WinSendDlgItemMsg (hwnd, DID.GradualShutdown, OS2.BM_SETCHECK,
                                  OS2.MPFROMSHORT(1), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.QuickShutdown, OS2.BM_SETCHECK,
                                  OS2.MPFROMSHORT(0), NIL);

        ResultCode := 1;
        OS2.WinProcessDlg(hwnd);
        INIData.StoreWindowPosition (hwnd, ININame, "KillServer");
        OS2.WinDestroyWindow (hwnd);
        RETURN ResultCode;

    END RunDialogue;

(**************************************************************************)

END KillServer.

