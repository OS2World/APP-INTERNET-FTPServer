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

IMPLEMENTATION MODULE MonINI;

        (************************************************************)
        (*                                                          *)
        (*             Ftp Monitor INI file operations              *)
        (*   This module opens and closes the Monitor INI file      *)
        (*   The actual INI file operations are done by INIData     *)
        (*                                                          *)
        (*      Started:        18 November 2009                    *)
        (*      Last edited:    20 November 2020                    *)
        (*      Status:         OK                                  *)
        (*                                                          *)
        (************************************************************)


IMPORT INIData, FileOps, Strings, Names;

(************************************************************************)

CONST Nul = CHR(0);

VAR
    INIFilename: Names.FilenameString;

(************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR);

    (* Specifies the name of the file to be opened by OpenINIFile. *)

    BEGIN
        Strings.Assign (name, INIFilename);
    END SetINIFileName;

(************************************************************************)

PROCEDURE GetINIFileName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the current INI file name and mode. *)

    BEGIN
        Strings.Assign (INIFilename, name);
    END GetINIFileName;

(************************************************************************)

PROCEDURE OpenINIFile(): INIData.HINI;

    (* Opens our INI file, returns the handle. *)

    VAR hini: INIData.HINI;

    BEGIN
        hini := INIData.OpenINIFile (INIFilename);
        IF NOT INIData.INIValid (hini) THEN
            hini := INIData.CreateINIFile (INIFilename);
        END (*IF*);
        RETURN hini;
    END OpenINIFile;

(************************************************************************)

PROCEDURE CloseINIFile (hini: INIData.HINI);

    (* Closes an INI file. *)

    BEGIN
        INIData.CloseINIFile(hini);
    END CloseINIFile;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

BEGIN
    SetINIFileName ("MONITOR.INI");
END MonINI.

