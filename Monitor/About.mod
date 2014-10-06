(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
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

IMPLEMENTATION MODULE About;

        (****************************************************************)
        (*                    Monitor for FtpServer                     *)
        (*                     The 'about' dialogue                     *)
        (*                                                              *)
        (*    Started:        25 May 2000                               *)
        (*    Last edited:    19 November 2009                          *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, DID;

FROM MonINI IMPORT
    (* proc *)  GetINIFileName;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

(************************************************************************)

PROCEDURE Create (owner: OS2.HWND);

    (* Creates the dialogue box. *)

    VAR hwnd: OS2.HWND;  TNImode: BOOLEAN;
        INIFileName: ARRAY [0..511] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       OS2.WinDefDlgProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.AboutDialogue,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)

        GetINIFileName (INIFileName, TNImode);
        SetInitialWindowPosition (hwnd, INIFileName, "About", TNImode);
        OS2.WinShowWindow (hwnd, TRUE);

        OS2.WinProcessDlg(hwnd);
        StoreWindowPosition (hwnd, INIFileName, "About", TNImode);
        OS2.WinDestroyWindow (hwnd);
    END Create;

(**************************************************************************)

END About.

