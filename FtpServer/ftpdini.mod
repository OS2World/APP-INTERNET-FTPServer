(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
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

IMPLEMENTATION MODULE FtpdINI;

        (************************************************************)
        (*                                                          *)
        (*               FtpServer INI file operations              *)
        (*   This module opens and closes the FtpServer INI file    *)
        (*   The actual INI file operations are done by INIData     *)
        (*                                                          *)
        (*      Started:        28 January 2002                     *)
        (*      Last edited:    5 February 2015                     *)
        (*      Status:         OK                                  *)
        (*                                                          *)
        (************************************************************)


IMPORT INIData, FileOps, Strings, Names;

(************************************************************************)

CONST Nul = CHR(0);

VAR
    INIFilename, Basename: Names.FilenameString;
    HashMax: CARDINAL;
    UseTNI: BOOLEAN;

(************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Specifies the name of the file to be opened by OpenINIFile. *)

    BEGIN
        UseTNI := TNImode;
        Strings.Assign (name, INIFilename);
        Strings.Assign (name, Basename);
        Basename[4] := Nul;
        Strings.Append ("    .INI", Basename);
    END SetINIFileName;

(************************************************************************)

PROCEDURE GetINIFileName (VAR (*OUT*) name: ARRAY OF CHAR;
                               VAR (*OUT*) TNImode: BOOLEAN);

    (* Returns the name of the INI or TNI file. *)

    BEGIN
        TNImode := UseTNI;
        Strings.Assign (INIFilename, name);
    END GetINIFileName;

(************************************************************************)

PROCEDURE INIFileExists(): BOOLEAN;

    (* Returns TRUE iff the INI (or TNI, if appropriate) file exists. *)

    BEGIN
        RETURN FileOps.Exists(INIFilename);
    END INIFileExists;

(************************************************************************)

PROCEDURE OpenINIFile(): INIData.HINI;

    (* Opens our INI file, returns the handle. *)

    VAR hini: INIData.HINI;

    BEGIN
        hini := INIData.OpenINIFile (INIFilename, UseTNI);
        (*
        IF NOT INIData.INIValid(hini) THEN
            hini := INIData.CreateINIFile (INIFilename, UseTNI);
        END (*IF*);
        *)
        RETURN hini;
    END OpenINIFile;

(************************************************************************)

PROCEDURE CloseINIFile (hini: INIData.HINI);

    (* Closes an INI file. *)

    BEGIN
        INIData.CloseINIFile(hini);
    END CloseINIFile;

(************************************************************************)
(*                    PROVISION FOR MULTIPLE INI FILES                  *)
(************************************************************************)

PROCEDURE SetHashMax (value: CARDINAL);

    (* Sets the number of extra INI files to be used for the "large     *)
    (* number of users" case.  If value=0 then we revert to the default *)
    (* of using a single INI file.  Otherwise, it is recommended that   *)
    (* value be a prime number.                                         *)

    BEGIN
        HashMax := value;
    END SetHashMax;

(************************************************************************)

PROCEDURE OpenINIForUser (name: ARRAY OF CHAR;
                                CreateIfNotExists: BOOLEAN): INIData.HINI;

    (* Opens the INI file that contains the data for the named user. *)

    VAR INIname: Names.FilenameString;
        code, k, length: CARDINAL;
        hini: INIData.HINI;

    BEGIN
        IF HashMax = 0 THEN
            INIname := INIFilename;
        ELSE
            Strings.Capitalize (name);

            (* Compute a hash code from the name. *)

            code := 0;
            length := Strings.Length(name);
            IF length > 0 THEN
                FOR k := 0 TO length-1 DO
                    code := (16*code + ORD(name[k])) MOD HashMax;
                END (*FOR*);
            END (*IF*);

            (* Turn the code into a file name. *)

            INIname := Basename;
            FOR k := 7 TO 4 BY -1 DO
                INIname[k] := CHR(code MOD 10 + ORD('0'));
                code := code DIV 10;
            END (*FOR*);

        END (*IF*);
        hini := INIData.OpenINIFile (INIname, UseTNI);
        IF (NOT INIData.INIValid(hini)) AND CreateIfNotExists THEN
            hini := INIData.CreateINIFile (INIname, UseTNI);
        END (*IF*);

        RETURN hini;

    END OpenINIForUser;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

BEGIN
    UseTNI := FALSE;
    SetINIFileName ("FTPD.INI", UseTNI);
    HashMax := 0;
END FtpdINI.

