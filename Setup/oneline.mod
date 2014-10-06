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

IMPLEMENTATION MODULE OneLine;

        (************************************************************)
        (*                                                          *)
        (*                   Setup for FtpServer                    *)
        (*            Dialogue to edit a one-line string            *)
        (*                                                          *)
        (*    Started:        28 May 2013                           *)
        (*    Last edited:    29 May 2013                           *)
        (*    Status:         Working                               *)
        (*                                                          *)
        (************************************************************)

IMPORT OS2, DID, Remote;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    Confirmed: BOOLEAN;

(************************************************************************)
(*                   THE EDIT-ONE-STRING DIALOGUE                       *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    BEGIN
        IF (msg = OS2.WM_COMMAND) AND (OS2.LONGFROMMP(mp1) = OS2.DID_CANCEL) THEN
            Confirmed := FALSE;
        END (*IF*);
        RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
    END DialogueProc;

(**************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  title: ARRAY OF CHAR;
                       VAR (*INOUT*) item: ARRAY OF CHAR): BOOLEAN;

    (* Edits the one-line string "item".  Returns FALSE if the edit     *)
    (* was cancelled.                                                   *)

    VAR hwnd: OS2.HWND;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, (* parent *)
                       owner,                    (* owner *)
                       DialogueProc,       (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.GetOneLine,     (* dialogue ID *)
                       NIL);               (* creation parameters *)

        Remote.SetInitialWindowPosition (hwnd, "OneLine");
        OS2.WinSetWindowText (hwnd, title);
        OS2.WinSetDlgItemText (hwnd, DID.OneLineEntry, item);

        Confirmed := TRUE;
        OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd,DID.OneLineEntry));
        OS2.WinProcessDlg(hwnd);
        IF Confirmed THEN
            OS2.WinQueryDlgItemText (hwnd, DID.OneLineEntry, SIZE(item), item);
        END (*IF*);
        Remote.StoreWindowPosition (hwnd, "OneLine", TRUE);
        OS2.WinDestroyWindow (hwnd);
        RETURN Confirmed;
    END Edit;

(************************************************************************)

END OneLine.

