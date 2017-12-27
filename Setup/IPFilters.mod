(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE IPFilters;

        (****************************************************************)
        (*                                                              *)
        (*                   PM Setup for FtpServer                     *)
        (*             Manipulation of IP filter elements               *)
        (*                                                              *)
        (*        Started:        11 September 2008                     *)
        (*        Last edited:    22 May 2017                           *)
        (*        Status:         Updated to more record types          *)
        (*                                                              *)
        (****************************************************************)


IMPORT Strings;

FROM SYSTEM IMPORT LOC, CARD8, CAST;

FROM FSUINI IMPORT
    (* proc *)  OpenINIFile, OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIPutBinary, ItemSize, INIFetchBinary;

FROM MiscFuncs IMPORT
    (* type *)  LocArrayPointer,
    (* proc *)  ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)
(*          CONVERTING AN ADDRESS RECORD TO HUMAN-READABLE FORM         *)
(************************************************************************)

PROCEDURE CardToDotted (value: ARRAY OF LOC;  VAR (*OUT*) text: ARRAY OF CHAR;
                                              VAR (*INOUT*) place: CARDINAL);

    (* Converts the four-byte value to a string of the form a.b.c.d,    *)
    (* starting at text[place].  On return, place is the first location *)
    (* in the text array that hasn't been used.                         *)

    VAR j: [0..2];

    BEGIN
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,value[j]), text, place);
            text[place] := '.';  INC(place);
        END (*FOR*);
        ConvertCard (CAST(CARD8,value[3]), text, place);
        IF place <= HIGH(text) THEN
            text[place] := Nul;
        END (*IF*);
    END CardToDotted;

(************************************************************************)

PROCEDURE CardinalToDotted (value: CARDINAL;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* Converts the value to a string of the form a.b.c.d    *)

    VAR place: CARDINAL;

    BEGIN
        place := 0;
        CardToDotted (value, text, place);
    END CardinalToDotted;

(************************************************************************)

PROCEDURE AddrRecordToText (p: ListPtr;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* Converts p^ to a text string. *)

    CONST MaskPos = 25;

    VAR place, j: CARDINAL;

    BEGIN
        IF p^.allow THEN Strings.Assign ("Allow   ", text)
        ELSE Strings.Assign ("Refuse  ", text)
        END (*IF*);
        IF p^.type = 0 THEN
            Strings.Append ("all", text);
            IF p^.previous <> NIL THEN
                Strings.Append (" others", text);
            END (*IF*);
        ELSE
            place := 8;
            CardToDotted (p^.address, text, place);
            IF p^.type = 1 THEN
                WHILE place < MaskPos DO
                    text[place] := ' ';  INC (place);
                END (*WHILE*);
                CardToDotted (p^.mask, text, place);
            ELSIF p^.type = 2 THEN
                (* single address, nothing to add. *)
            ELSIF p^.type = 3 THEN
                text[place] := ' ';  INC (place);
                text[place] := '/';  INC(place);
                text[place] := ' ';  INC (place);
                ConvertCard (p^.bitcount, text, place);
                text[place] := Nul;
            ELSIF p^.type = 4 THEN
                FOR j := 1 TO 3 DO
                    text[place] := ' ';  INC (place);
                END (*FOR*);
                text[place] := '-';  INC (place);
                FOR j := 1 TO 3 DO
                    text[place] := ' ';  INC (place);
                END (*FOR*);
                CardToDotted (p^.lastaddr, text, place);
            END (*IF*);
        END (*IF*);
    END AddrRecordToText;

(************************************************************************)
(*             CONVERTING A STRING TO AN ADDRESS RECORD                 *)
(************************************************************************)

PROCEDURE SkipBlanks (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL);

    (* Increases j until text[j] is not a space character. *)

    BEGIN
        WHILE text[j] = ' ' DO INC(j) END(*WHILE*);
    END SkipBlanks;

(************************************************************************)

PROCEDURE DottedToCard (text: ARRAY OF CHAR;
                                 VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts a dotted-quad text string to a cardinal value. *)

    VAR result: CARDINAL;  j: [0..3];

    (********************************************************************)

    PROCEDURE ConvertOneComponent(): CARDINAL;

        TYPE CharSet = SET OF CHAR;
        CONST Digits = CharSet {'0'..'9'};

        VAR answer: CARDINAL;

        BEGIN
            answer := 0;
            WHILE text[pos] IN Digits DO
                answer := 10*answer + (ORD(text[pos]) - ORD('0'));
                INC (pos);
            END (*WHILE*);
            RETURN answer;
        END ConvertOneComponent;

    (********************************************************************)

    BEGIN
        result := 0;
        FOR j := 0 TO 3 DO
            result := 256*result + ConvertOneComponent();
            IF text[pos] = '.' THEN INC(pos) END(*IF*);
        END (*FOR*);
        RETURN Swap4(result);
    END DottedToCard;

(************************************************************************)

PROCEDURE StringToCard (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL): CARDINAL;

    (* Picks up an IP address, starting at text[j], updates j. *)

    VAR result: CARDINAL;

    BEGIN
        SkipBlanks (text, j);
        IF text[j] = '[' THEN INC(j) END(*IF*);
        result := DottedToCard (text, j);
        IF text[j] = ']' THEN INC(j) END(*IF*);
        RETURN result;
    END StringToCard;

(************************************************************************)

PROCEDURE StringToIPAddress (text: ARRAY OF CHAR): CARDINAL;

    (* Converts an IP address. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        RETURN StringToCard (text, j);
    END StringToIPAddress;

(************************************************************************)
(*          GETTING/PUTTING IP ADDRESS CONTROLS FROM INI FILE           *)
(************************************************************************)

PROCEDURE LoadIPFilterList (appname: ARRAY OF CHAR): ListPtr;

    (* Loads an allow/deny list from the INI file.    *)

    VAR j, size: CARDINAL;
        bufptr: LocArrayPointer;
        ListHead, current: ListPtr;

    (********************************************************************)

    PROCEDURE Get (VAR (*OUT*) result: ARRAY OF LOC);

        (* Reads a value from bufptr^[j], updates j. *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(result) DO
                IF j >= size THEN
                    result[k] := 0;
                ELSE
                    result[k] := bufptr^[j];  INC(j);
                END (*IF*);
            END (*FOR*);
        END Get;

    (********************************************************************)

    BEGIN
        IF appname[0] = Nul THEN
            NEW (ListHead);
            WITH ListHead^ DO
                previous := NIL;
                next := NIL;
                allow := TRUE;
                type := 0;
            END (*WITH*);
            RETURN ListHead;
        END (*IF*);

        IF Strings.Equal (appname, "$SYS") THEN
            OpenINIFile;
        ELSE
            OpenINIForUser (appname);
        END (*IF*);
        ListHead := NIL;
        IF NOT ItemSize (appname, "IPfilter", size) THEN
            size := 2;
        END (*IF*);

        (*IF size <> 0 THEN*)
            IF size < 2 THEN
                size := 2;
            END (*IF*);
            ALLOCATE (bufptr, size);
            IF NOT INIFetchBinary (appname, "IPfilter", bufptr^, size) THEN
                bufptr^[0] := 1;  bufptr^[1] := 0;
            END (*IF*);
            j := 0;  current := NIL;
            LOOP
                IF current = NIL THEN
                    NEW (ListHead);
                    ListHead^.previous := NIL;
                    current := ListHead;
                ELSE
                    NEW (current^.next);
                    current^.next^.previous := current;
                    current := current^.next;
                END (*IF*);
                current^.next := NIL;
                Get (current^.allow);
                Get (current^.type);
                IF current^.type > 4 THEN
                    current^.type := 1;
                END (*IF*);
                CASE current^.type OF
                  | 0:  EXIT (*LOOP*);
                  | 1:  Get (current^.address);
                        Get (current^.mask);
                        current^.address := IAND (current^.address, current^.mask);
                  | 2:  Get (current^.address2);
                        Get (current^.dummy);
                  | 3:  Get (current^.address3);
                        Get (current^.bitcount);
                  | 4:  Get (current^.firstaddr);
                        Get (current^.lastaddr);
                END (*CASE*);
            END (*LOOP*);
            DEALLOCATE (bufptr, size);
        (*END (*IF*);*)

        (* Ensure that the list is terminated with a catch-all entry. *)

        current := ListHead;
        IF current = NIL THEN
            NEW (current);  current^.previous := NIL;
            ListHead := current;
        ELSE
            WHILE current^.next <> NIL DO
                current := current^.next;
            END (*WHILE*);
            IF current^.type = 0 THEN
                current := NIL;
            ELSE
                NEW (current^.next);
                current^.next^.previous := current;
                current := current^.next;
            END (*IF*);
        END (*IF*);
        IF current <> NIL THEN
            current^.next := NIL;
            current^.allow := TRUE;
            current^.type := 0;
        END (*IF*);
        CloseINIFile;

        RETURN ListHead;

    END LoadIPFilterList;

(************************************************************************)

PROCEDURE StoreIPFilterList (appname: ARRAY OF CHAR;  head: ListPtr);

    (* Stores an allow/deny list to the INI file.  We assume that the   *)
    (* caller already has the INI file open.                            *)

    VAR bufptr: LocArrayPointer;
        current: ListPtr;  j, size: CARDINAL;

    (********************************************************************)

    PROCEDURE Put (value: ARRAY OF LOC);

        (* Stores a value into bufptr^[j], updates j. *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(value) DO
                bufptr^[j] := value[k];  INC(j);
            END (*FOR*);
        END Put;

    (********************************************************************)

    BEGIN
        size := 0;  current := head;
        WHILE current <> NIL DO
            INC (size, SIZE(BOOLEAN) + SIZE(RecordKind));
            IF current^.type > 0 THEN
                INC (size, 2*SIZE(CARDINAL));
            END (*IF*);
            current := current^.next;
        END (*WHILE*);
        IF size = 0 THEN bufptr := NIL
        ELSE ALLOCATE (bufptr, size);
        END (*IF*);
        current := head;  j := 0;
        WHILE current <> NIL DO
            Put (current^.allow);  Put (current^.type);
            IF current^.type > 0 THEN
                 Put (current^.address);
                 Put (current^.mask);
            END (*IF*);
            current := current^.next;
        END (*WHILE*);
        INIPutBinary (appname, "IPfilter", bufptr^, size);
        IF bufptr <> NIL THEN
            DEALLOCATE (bufptr, size);
        END (*IF*);
    END StoreIPFilterList;

(************************************************************************)

END IPFilters.

