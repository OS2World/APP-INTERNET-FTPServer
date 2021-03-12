(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
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

IMPLEMENTATION MODULE FSUINI;

        (************************************************************)
        (*                                                          *)
        (*                  PM Setup for FtpServer                  *)
        (*   This module opens and closes the FtpServer INI file    *)
        (*   The actual INI file operations are done by RINIData    *)
        (*                                                          *)
        (*      Started:        21 January 2002                     *)
        (*      Last edited:    16 December 2020                    *)
        (*      Status:         OK                                  *)
        (*                                                          *)
        (************************************************************)


IMPORT RINIData, Names, Strings;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST Nul = CHR(0);

VAR
    OriginalINIFilename, INIFilename, Basename: Names.FilenameString;
    HashMax: CARDINAL;
    UseTNI: BOOLEAN;

(************************************************************************)

PROCEDURE SetTNIMode (TNImode: BOOLEAN);

    (* Specifies whether the INI file name is FTPD.INI or FTPD.TNI.  *)

    BEGIN
        UseTNI := TNImode;
        Strings.Assign ("FTPD", INIFilename);
        Strings.Assign (INIFilename, Basename);
        IF TNImode THEN
            Strings.Append (".TNI", INIFilename);
            Strings.Append ("    .TNI", Basename);
        ELSE
            Strings.Append (".INI", INIFilename);
            Strings.Append ("    .INI", Basename);
        END (*IF*);
        Strings.Assign (INIFilename, OriginalINIFilename);
    END SetTNIMode;

(************************************************************************)

PROCEDURE TNImode(): BOOLEAN;

    (* Returns TRUE iff we are using TNI files. *)

    BEGIN
        RETURN UseTNI;
    END TNImode;

(************************************************************************)

PROCEDURE ChangeINIFilename (name: ARRAY OF CHAR);

    (* Temporarily changes the specification of the INI file name. *)

    BEGIN
        Strings.Assign (name, INIFilename);
    END ChangeINIFilename;

(************************************************************************)

PROCEDURE RestoreINIFilename;

    (* Sets the INI file name back to its original value. *)

    BEGIN
        Strings.Assign (OriginalINIFilename, INIFilename);
    END RestoreINIFilename;

(************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Specifies the name of the file to be opened by OpenINIFile. *)

    BEGIN
        UseTNI := TNImode;
        Strings.Assign (name, INIFilename);
        Strings.Assign (name, Basename);
        Basename[4] := Nul;
        IF TNImode THEN
            Strings.Append ("    .TNI", Basename);
        ELSE
            Strings.Append ("    .INI", Basename);
        END (*IF*);
    END SetINIFileName;

(************************************************************************)

PROCEDURE OpenINIFile;

    BEGIN
        EVAL(RINIData.OpenINIFile (INIFilename));
    END OpenINIFile;

(************************************************************************)

PROCEDURE CloseINIFile;

    BEGIN
        RINIData.CloseINIFile;
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

PROCEDURE IsMultiFileMode(): BOOLEAN;

    (* Returns TRUE iff we are currently using multiple INI files. *)

    BEGIN
        RETURN HashMax <> 0;
    END IsMultiFileMode;

(************************************************************************)

PROCEDURE OpenINIForUser (name: ARRAY OF CHAR);

    (* Opens the INI file that contains the data for the named user. *)

    VAR INIname: Names.FilenameString;
        code, k, length: CARDINAL;

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

        EVAL(RINIData.OpenINIFile (INIname));

    END OpenINIForUser;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

BEGIN
    SetTNIMode (FALSE);
    HashMax := 0;
END FSUINI.

