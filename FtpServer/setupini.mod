(**************************************************************************)
(*                                                                        *)
(*  Text-mode setup for FtpServer                                         *)
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

IMPLEMENTATION MODULE SetupINI;

        (********************************************************)
        (*                                                      *)
        (*           Opens/closes FtpServer INI file            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 February 1998                 *)
        (*  Last edited:        2 October 2019                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CloseINIFile;

(********************************************************************************)

VAR hini: HINI;

(********************************************************************************)

PROCEDURE OpenOurINIFile (UseTNI: BOOLEAN);

    (* Opens the INI file. We are using a system of keeping the INI file open   *)
    (* for the lifetime of the program.                                         *)

    VAR name: ARRAY [0..8] OF CHAR;

    BEGIN
        IF UseTNI THEN
            name := "ftpd.tni";
        ELSE
            name := "ftpd.ini";
        END (*IF*);
        hini := OpenINIFile (name);
    END OpenOurINIFile;

(********************************************************************************)

PROCEDURE OurINIHandle(): HINI;

    (* Returns the handle of the INI file, which is already open. *)

    BEGIN
        RETURN hini;
    END OurINIHandle;

(********************************************************************************)

BEGIN
FINALLY
    CloseINIFile (hini);
END SetupINI.

