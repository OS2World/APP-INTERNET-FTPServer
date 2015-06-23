(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE VStrings;

        (********************************************************)
        (*                                                      *)
        (*  Storage manager for dynamically allocated strings.  *)
        (*                                                      *)
        (*  Using VStrings instead of normal character strings  *)
        (*  can produce a significant space saving, but it does *)
        (*  place an obligation on the caller to deallocate     *)
        (*  the strings when they are no longer needed, to      *)
        (*  avoid leaving orphans in the heap.                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        28 November 2014                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM STextIO IMPORT
    (* proc *)  WriteChar;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    CharArray = ARRAY [0..65535] OF CHAR;

    VString = POINTER TO StringDescriptor;
    StringDescriptor =  RECORD
                            length: CARDINAL;
                            loc: POINTER TO CharArray;
                        END (*RECORD*);

    (* Global convention: a valid VString can never be NIL.  (This      *)
    (* makes error detection easier.)  A string of zero length is       *)
    (* represented by a descriptor with length=0, loc=NIL.              *)

    (* We do not use any string terminators.  Although this has the     *)
    (* potential to complicate some operations, it creates uniformity   *)
    (* when compared to the Modula-2 standard of sometimes having a     *)
    (* Nul terminator and sometimes not.                                *)

(************************************************************************)
(*              CONVERTING BETWEEN STRING AND VSTRING FORMAT            *)
(************************************************************************)

PROCEDURE MakeVString (VAR (*IN*) source: ARRAY OF CHAR): VString;

    (* Converts an array to a VString.  The source array is not altered.*)

    VAR result: VString;

    BEGIN
        NEW (result);
        WITH result^ DO
            length := LENGTH(source);
            IF length > 0 THEN
                ALLOCATE (loc, length);
                Copy (ADR(source), loc, length);
            ELSE
                loc := NIL;
            END (*IF*);
        END (*WITH*);
        RETURN result;
    END MakeVString;

(************************************************************************)
(*                         DISCARDING A VSTRING                         *)
(************************************************************************)

PROCEDURE DiscardVString (VAR (*INOUT*) VS: VString);

    (* Deallocates the storage for VS, returns with VS = NIL. *)

    BEGIN
        WITH VS^ DO
            IF length > 0 THEN
                DEALLOCATE (loc, length);
            END (*IF*);
        END (*WITH*);
        DISPOSE (VS);
    END DiscardVString;

(************************************************************************)
(*                             COMPARISONS                              *)
(************************************************************************)

PROCEDURE VSCompare (VAR (*IN*) first, second: VString): INTEGER;

    (* Compares two strings, ignoring alphabetic case.   Returns        *)
    (* -1 if first < second, 0 if first = second, +1 if first > second. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF j >= first^.length THEN
                IF j >= second^.length THEN
                    RETURN 0;
                END (*IF*);
            ELSIF j >= second^.length THEN
                RETURN +1;
            ELSIF CAP(first^.loc^[j]) < CAP(second^.loc^[j]) THEN
                RETURN -1;
            ELSIF CAP(first^.loc^[j]) > CAP(second^.loc^[j]) THEN
                RETURN +1;
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
    END VSCompare;

(************************************************************************)
(*                       WRITE TO STANDARD OUTPUT                       *)
(************************************************************************)

PROCEDURE WriteVString (VS: VString);

    (* Writes a string to standard output. *)

    VAR j, L: CARDINAL;

    BEGIN
        L := VS^.length;
        IF L > 0 THEN
            FOR j := 0 TO L-1 DO
                WriteChar (VS^.loc^[j]);
            END (*FOR*);
        END (*IF*);
    END WriteVString;

(************************************************************************)

END VStrings.

