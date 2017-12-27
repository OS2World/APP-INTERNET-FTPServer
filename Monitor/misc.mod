(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
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

IMPLEMENTATION MODULE Misc;

        (********************************************************)
        (*                                                      *)
        (*                   Monitor for FtpServer              *)
        (*                 Miscellaneous procedures             *)
        (*                                                      *)
        (*    Started:        29 March 2000                     *)
        (*    Last edited:    8 October 2017                    *)
        (*    Status:         adding some 64-bit functions      *)
        (*                                                      *)
        (********************************************************)

(*
FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;              (* while debugging only *)
*)

FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, CARD16, CARD32,
    (* proc *)  CAST;

FROM LONGLONG IMPORT
    (* const*)  Zero64, Max64,
    (* type *)  CARD64,
    (* proc *)  Div10, Compare64, Add64, Sub64, ShortMul64;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

IMPORT Strings;

(********************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    DecimalDigits = CharSet{'0'..'9'};
    HexDigits = CharSet{'0'..'9', 'A'..'F', 'a'..'f'};

VAR
    Max64DIV10: CARD64;

(************************************************************************)

PROCEDURE SwapIt (VAR (*INOUT*) arg: ARRAY OF LOC);

    (* Reverses the byte order of its argument. *)

    VAR j, top: CARDINAL;  temp: LOC;

    BEGIN
        top := HIGH(arg);
        FOR j := 0 TO top DIV 2 DO
            temp := arg[j];  arg[j] := arg[top-j];  arg[top-j] := temp;
        END (*FOR*);
    END SwapIt;

(************************************************************************)

PROCEDURE Swap2 (val: CARD16): CARD16;

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD16;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap2;

(************************************************************************)

PROCEDURE Swap4 (val: CARD32): CARD32;

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD32;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap4;

(************************************************************************)

PROCEDURE EVAL (f: ARRAY OF LOC);

    (* A do-nothing procedure - we use it for evaluating a function and *)
    (* ignoring the result.                                             *)

    BEGIN
    END EVAL;

(********************************************************************************)

PROCEDURE NameIsNumeric (VAR (*INOUT*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name has the form N.N.N.N or [N.N.N.N] where each N is  *)
    (* a decimal number.  (The present version actually accepts an arbitrary    *)
    (* number of numbers separated by dots.)  As a side-effect, we also strip   *)
    (* the square brackets if they are present.                                 *)

    VAR j: CARDINAL;  result: BOOLEAN;

    (****************************************************************************)

    PROCEDURE ScanNumber(): BOOLEAN;

        (* At least one digit. *)

        BEGIN
            IF name[j] IN DecimalDigits THEN
                REPEAT
                    INC(j);
                UNTIL NOT(name[j] IN DecimalDigits);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        END ScanNumber;

    (****************************************************************************)

    PROCEDURE ScanNumberString(): BOOLEAN;

        (* Numbers separated by '.' *)

        BEGIN
            LOOP
                IF NOT ScanNumber() THEN RETURN FALSE END(*IF*);
                IF name[j] = '.' THEN INC(j)
                ELSE RETURN TRUE
                END (*IF*);
            END (*LOOP*);
        END ScanNumberString;

    (****************************************************************************)

    BEGIN
        j := 0;
        IF name[0] = '[' THEN j := 1 END(*IF*);
        result := ScanNumberString();
        IF result THEN
            IF name[0] = '[' THEN
                result := name[j] = ']';
                IF result THEN INC(j) END(*IF*);
            END (*IF*);
            result := result AND (name[j] = Nul);
        END (*IF*);
        IF result AND (name[0] = '[') THEN
            name[j-1] := Nul;
            Strings.Delete (name, 0, 1);
        END (*IF*);
        RETURN result;
    END NameIsNumeric;

(********************************************************************************)

PROCEDURE ConvertCard (number: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL);

    (* Converts number to decimal, left justified starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR j: CARDINAL;  buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        CardinalToString (number, buffer, SIZE(buffer));
        j := 0;
        WHILE buffer[j] = ' ' DO INC(j);  END(*WHILE*);
        WHILE (pos <= HIGH(result)) AND (j < SIZE(buffer)) DO
            result[pos] := buffer[j];  INC(pos);  INC(j);
        END (*WHILE*);
    END ConvertCard;

(********************************************************************************)

PROCEDURE Card64ToString (number: CARD64;  VAR (*OUT*) result: ARRAY OF CHAR;
                                           VAR (*INOUT*) pos: CARDINAL);

    (* Converts number to decimal, left justified starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR Q: CARD64;  R: CARDINAL;

    BEGIN
        R := number.low;
        IF (number.high <> 0) OR (R > 9) THEN
            Div10 (number, Q, R);
            Card64ToString (Q, result, pos);
        END (*IF*);
        result[pos] := CHR(ORD('0') + R);
        INC (pos);
    END Card64ToString;

(********************************************************************************)

PROCEDURE IPToString (IP: ARRAY OF LOC;  EncloseInBrackets: BOOLEAN;
                                VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts a four-byte IP address (in network byte order) to a             *)
    (* human-readable form.  There must be at least 15 character positions      *)
    (* available in the result array, or 17 if EncloseInBrackets is TRUE.       *)

    VAR j, position: CARDINAL;

    BEGIN
        IF EncloseInBrackets THEN
            result[0] := '[';  position := 1;
        ELSE
            position := 0;
        END (*IF*);
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,IP[j]), result, position);
            result[position] := '.';  INC(position);
        END (*FOR*);
        ConvertCard (CAST(CARD8,IP[3]), result, position);
        IF EncloseInBrackets THEN
            result[position] := ']';  INC(position);
        END (*IF*);
        IF position <= HIGH(result) THEN
            result[position] := Nul;
        END (*IF*);
    END IPToString;

(********************************************************************************)

(*
PROCEDURE WriteCard64 (value: CARD64);       (* debugging *)

    (* For debugging: writes out value *)

    VAR buffer: ARRAY [0..31] OF CHAR;
        pos: CARDINAL;

    BEGIN
        pos := 0;
        Card64ToString (value, buffer, pos);
        buffer[pos] := Nul;
        WriteString (buffer);
    END WriteCard64;

(********************************************************************************)

PROCEDURE DumpVal (value: CARD64);       (* debugging *)

    (* For debugging: writes out value *)

    BEGIN
        WriteString ("Partial result is ");
        WriteCard64 (value);
        WriteLn;
    END DumpVal;
*)

(********************************************************************************)

PROCEDURE StringToCard64 (VAR (*IN*) string: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARD64;

    (* Reads a decimal number starting at string[pos], updates pos.     *)
    (* This is the same as StringToCard, below, except that we do the   *)
    (* calculation in 64-bit arithmetic.                                *)

    VAR value, test: CARD64;  increment: CARD32;

    BEGIN
        value := Zero64;
        WHILE (pos <= HIGH(string)) AND (string[pos] IN DecimalDigits) DO
            IF Compare64 (value, Max64DIV10) > 0 THEN
                value := Max64;
            ELSE
                value := ShortMul64 (value, 10);
            END (*IF*);
            increment := VAL(CARD32, ORD(string[pos]) - ORD('0'));
            test := Max64;
            Sub64 (test, increment);
            IF Compare64 (value, test) > 0 THEN
                value := Max64;
            ELSE
                Add64 (value, increment);
            END (*IF*);
            INC (pos);
        END (*WHILE*);
        RETURN value;
    END StringToCard64;

(********************************************************************************)

PROCEDURE StringToCard (VAR (*IN*) string: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Reads a decimal number starting at string[pos], updates pos. *)

    VAR value, increment: CARDINAL;

    BEGIN
        value := 0;
        WHILE (pos <= HIGH(string)) AND (string[pos] IN DecimalDigits) DO
            IF value > MAX(CARD32) DIV 10 THEN
                value := MAX(CARD32);
            ELSE
                value := 10*value;
            END (*IF*);
            increment := VAL(CARD32, ORD(string[pos]) - ORD('0'));
            IF value > MAX(CARD32) - increment THEN
                value := MAX(CARD32);
            ELSE
                value := value + increment;
            END (*IF*);
            INC (pos);
        END (*WHILE*);
        RETURN value;
    END StringToCard;

(********************************************************************************)

PROCEDURE StringToShortCard (VAR (*IN*) string: ARRAY OF CHAR;
                                      VAR (*INOUT*) pos: CARDINAL): CARD8;

    (* Like StringToCard, but truncates result. *)

    VAR val: CARDINAL;  result: CARD8;

    BEGIN
        val := StringToCard (string, pos);
        IF val > MAX(CARD8) THEN
            result := MAX(CARD8);
        ELSE
            result := val;
        END (*IF*);
        RETURN result;
    END StringToShortCard;

(********************************************************************************)

PROCEDURE StringToIPAddress (VAR (*IN*) name: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts a string of the form N.N.N.N or [N.N.N.N], where each N is a    *)
    (* decimal number, starting at name[pos] and updating pos.  We assume that  *)
    (* the caller has already checked for syntactic correctness.                *)

    (****************************************************************************)

    PROCEDURE Scan4 (VAR (*OUT*) result: ARRAY OF LOC);

        (* Numbers separated by '.' *)

        VAR j: [0..3];

        BEGIN
            FOR j := 0 TO 2 DO
                result[j] := StringToShortCard (name, pos);
                IF (pos <= HIGH(name)) AND (name[pos] = '.') THEN
                    INC (pos);
                END (*IF*);
            END (*FOR*);
            result[3] := StringToShortCard (name, pos);
        END Scan4;

    (****************************************************************************)

    VAR result: CARDINAL;  bracketed: BOOLEAN;

    BEGIN
        bracketed := (pos <= HIGH(name)) AND (name[pos] = '[');
        IF bracketed THEN INC(pos) END(*IF*);
        Scan4 (result);
        IF bracketed AND (pos <= HIGH(name)) AND (name[pos] = ']') THEN
            INC(pos);
        END (*IF*);
        RETURN result;
    END StringToIPAddress;

(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    Div10 (Max64, Max64DIV10, dummy);
END Misc.

