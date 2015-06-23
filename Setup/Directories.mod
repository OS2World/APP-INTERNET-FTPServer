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

IMPLEMENTATION MODULE Directories;

        (********************************************************)
        (*                                                      *)
        (*            Looking up file directories               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 November 1999                *)
        (*  Last edited:        21 January 2002                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2, Strings, RINIData, Remote;

FROM SYSTEM IMPORT
    (* type *)  CARD32,
    (* proc *)  ADR;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    ResultBufLen = SIZE(OS2.FILEFINDBUF3);

TYPE
    FileAttribute = (readonly,hidden,system,b3,directory,archive);
    FileAttr = SET OF FileAttribute;

    DirectorySearchHandle = POINTER TO SearchData;
    SearchData = RECORD
                     RunRemote: BOOLEAN;
                     handle: CARD32;
                 END (*RECORD*);

(************************************************************************)
(*                   CHECK FOR EXISTENCE OF A FILE                      *)
(************************************************************************)

PROCEDURE FileCheck (FullName: FileName;
                     VAR (*OUT*) exists, IsAFile: BOOLEAN);

    (* Returns with exists=TRUE if the file exists, and IsAFile=TRUE    *)
    (* if this is a non-directory file.                                 *)

    CONST attrib = 37H;

    VAR Handle: CARD32;  k, FindCount: CARDINAL;
        InfoBuf: OS2.FSALLOCATE;
        FindBuffer: OS2.FILEFINDBUF3;
        rc: OS2.APIRET;
        Name: FileName;

    BEGIN
        IF RINIData.RemoteOperation() THEN

            (* Remote file or directory. *)

            exists := Remote.StartDirectoryListing (FullName)
                   AND Remote.NextDirectoryEntry (Name);
            Remote.FinishDirectoryListing;
            IsAFile := exists;
            IF exists THEN
                k := Strings.Length (Name);
                IF (k > 0) AND (Name[k-1] = '/') THEN
                    IsAFile := FALSE;
                END (*IF*);
            END (*IF*);

        ELSIF (FullName[2] = Nul) AND (FullName[1] = ':') THEN

            (* Root of a local filesystem volume. *)

            exists := OS2.DosQueryFSInfo (ORD(CAP(FullName[0])) - ORD('A') + 1,
                           1, ADR(InfoBuf), SIZE(InfoBuf)) = 0;
            IsAFile := NOT exists;

        ELSE

            Handle := OS2.HDIR_CREATE;
            FindCount := 1;
            rc := OS2.DosFindFirst (FullName, Handle, attrib,
                                    ADR(FindBuffer), ResultBufLen,
                                    FindCount, OS2.FIL_STANDARD);
            OS2.DosFindClose (Handle);
            exists := rc = OS2.NO_ERROR;
            IsAFile := exists AND (IAND (FindBuffer.attrFile, 10H) = 0);
        END (*IF*);

    END FileCheck;

(************************************************************************)
(*                        SUBDIRECTORY LOOKUP                           *)
(************************************************************************)

PROCEDURE FirstSubdirectory (parent: FileName;
                           VAR (*OUT*) Name: FileName;
                           VAR (*OUT*) D: DirectorySearchHandle): BOOLEAN;

    (* Sets Name to the name of the first subdirectory in parent, and   *)
    (* returns TRUE if such a subdirectory exists.  D is the handle for *)
    (* further searches in the same directory.                          *)

    CONST attrib = 1037H;

    VAR mask: FileName;
        FindBuffer: OS2.FILEFINDBUF3;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        NEW (D);
        D^.RunRemote := RINIData.RemoteOperation();
        IF D^.RunRemote THEN
            Name[0] := Nul;
            Strings.Append ('\*', parent);
            RETURN Remote.StartDirectoryListing (parent)
                   AND NextSubdirectory (D, Name);
        ELSE
            D^.handle := OS2.HDIR_CREATE;
            FindCount := 1;
            mask := parent;
            Strings.Append ("\*", mask);
            rc := OS2.DosFindFirst (mask, D^.handle, attrib,
                                    ADR(FindBuffer), ResultBufLen,
                                    FindCount, OS2.FIL_STANDARD);
            Strings.Assign (FindBuffer.achName, Name);
            RETURN rc = OS2.NO_ERROR;
        END (*IF*);
    END FirstSubdirectory;

(************************************************************************)

PROCEDURE NextSubdirectory (VAR (*INOUT*) D: DirectorySearchHandle;
                            VAR (*OUT*) Name: FileName): BOOLEAN;

    (* Read the next directory entry satisfying the search criteria     *)
    (* specified by the FirstSubdirectory call.                         *)

    VAR FindBuffer: OS2.FILEFINDBUF3;
        FindCount, pos: CARDINAL;
        rc: OS2.APIRET;  success: BOOLEAN;

    BEGIN
        IF D^.RunRemote THEN
            LOOP
                success := Remote.NextDirectoryEntry (Name);
                IF NOT success THEN EXIT(*LOOP*) END(*IF*);
                pos := Strings.Length (Name);
                success := (pos > 0) AND (Name[pos-1] = '/');
                IF success THEN
                    Name[pos-1] := Nul;
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
            RETURN success;
        ELSE
            FindCount := 1;
            rc := OS2.DosFindNext(D^.handle, ADR(FindBuffer),
                                  ResultBufLen, FindCount);
            Strings.Assign (FindBuffer.achName, Name);
            RETURN rc = OS2.NO_ERROR;
        END (*IF*);
    END NextSubdirectory;

(************************************************************************)

PROCEDURE SubdirSearchDone (VAR (*INOUT*) D: DirectorySearchHandle);

    (* Discard the search handle. *)

    BEGIN
        IF D^.RunRemote THEN
            Remote.FinishDirectoryListing;
        ELSE
            OS2.DosFindClose (D^.handle);
        END (*IF*);
        DISPOSE (D);
    END SubdirSearchDone;

(************************************************************************)

END Directories.

