(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE CodePage;

        (********************************************************)
        (*                                                      *)
        (*     Translation between code pages and Unicode       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        7 April 2013                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM OS2 IMPORT
    (* proc *)  DosQueryCp, DosSetProcessCp;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(********************************************************************************)

CONST Nul = CHR(0);  CR = CHR(13);

TYPE
    UpperCodeRange = [CHR(128)..CHR(255)];
    UpperPage = ARRAY UpperCodeRange OF CARDINAL;

CONST
    (* Code page 437 from ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT *)

    CP437 = UpperPage {00C7H, 00FCH, 00E9H, 00E2H, 00E4H, 00E0H, 00E5H, 00E7H,
                       00EAH, 00EBH, 00E8H, 00EFH, 00EEH, 00ECH, 00C4H, 00C5H,
                       00C9H, 00E6H, 00C6H, 00F4H, 00F6H, 00F2H, 00FBH, 00F9H,
                       00FFH, 00D6H, 00DCH, 00A2H, 00A3H, 00A5H, 20A7H, 0192H,
                       00E1H, 00EDH, 00F3H, 00FAH, 00F1H, 00D1H, 00AAH, 00BAH,
                       00BFH, 2310H, 00ACH, 00BDH, 00BCH, 00A1H, 00ABH, 00BBH,
                       2591H, 2592H, 2593H, 2502H, 2524H, 2561H, 2562H, 2556H,
                       2555H, 2563H, 2551H, 2557H, 255DH, 255CH, 255BH, 2510H,
                       2514H, 2534H, 252CH, 251CH, 2500H, 253CH, 255EH, 255FH,
                       255AH, 2554H, 2569H, 2566H, 2560H, 2550H, 256CH, 2567H,
                       2568H, 2564H, 2565H, 2559H, 2558H, 2552H, 2553H, 256BH,
                       256AH, 2518H, 250CH, 2588H, 2584H, 258CH, 2590H, 2580H,
                       03B1H, 00DFH, 0393H, 03C0H, 03A3H, 03C3H, 00B5H, 03C4H,
                       03A6H, 0398H, 03A9H, 03B4H, 221EH, 03C6H, 03B5H, 2229H,
                       2261H, 00B1H, 2265H, 2264H, 2320H, 2321H, 00F7H, 2248H,
                       00B0H, 2219H, 00B7H, 221AH, 207FH, 00B2H, 25A0H, 00A0H
                       };

    (* Code page 850 from ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP850.TXT *)

    CP850 = UpperPage {00C7H, 00FCH, 00E9H, 00E2H, 00E4H, 00E0H, 00E5H, 00E7H,
                       00EAH, 00EBH, 00E8H, 00EFH, 00EEH, 00ECH, 00C4H, 00C5H,
                       00C9H, 00E6H, 00C6H, 00F4H, 00F6H, 00F2H, 00FBH, 00F9H,
                       00FFH, 00D6H, 00DCH, 00F8H, 00A3H, 00D8H, 00D7H, 0192H,

                       00E1H, 00EDH, 00F3H, 00FAH, 00F1H, 00D1H, 00AAH, 00BAH,
                       00BFH, 00AEH, 00ACH, 00BDH, 00BCH, 00A1H, 00ABH, 00BBH,
                       2591H, 2592H, 2593H, 2502H, 2524H, 00C1H, 00C2H, 00C0H,
                       00A9H, 2563H, 2551H, 2557H, 255DH, 00A2H, 00A5H, 2510H,

                       2514H, 2534H, 252CH, 251CH, 2500H, 253CH, 00E3H, 00C3H,
                       255AH, 2554H, 2569H, 2566H, 2560H, 2550H, 256CH, 00A4H,
                       00F0H, 00D0H, 00CAH, 00CBH, 00C8H, 0131H, 00CDH, 00CEH,
                       00CFH, 2518H, 250CH, 2588H, 2584H, 00A6H, 00CCH, 2580H,

                       00D3H, 00DFH, 00D4H, 00D2H, 00F5H, 00D5H, 00B5H, 00FEH,
                       00DEH, 00DAH, 00DBH, 00D9H, 00FDH, 00DDH, 00AFH, 00B4H,
                       00ADH, 00B1H, 2017H, 00BEH, 00B6H, 00A7H, 00F7H, 00B8H,
                       00B0H, 00A8H, 00B7H, 00B9H, 00B3H, 00B2H, 25A0H, 00A0H
                       };

    (* Code page 866 from ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP866.TXT *)

    CP866 = UpperPage {0410H, 0411H, 0412H, 0413H, 0414H, 0415H, 0416H, 0417H,
                       0418H, 0419H, 041AH, 041BH, 041CH, 041DH, 041EH, 041FH,
                       0420H, 0421H, 0422H, 0423H, 0424H, 0425H, 0426H, 0427H,
                       0428H, 0429H, 042AH, 042BH, 042CH, 042DH, 042EH, 042FH,

                       0430H, 0431H, 0432H, 0433H, 0434H, 0435H, 0436H, 0437H,
                       0438H, 0439H, 043AH, 043BH, 043CH, 043DH, 043EH, 043FH,
                       2591H, 2592H, 2593H, 2502H, 2524H, 2561H, 2562H, 2556H,
                       2555H, 2563H, 2551H, 2557H, 255DH, 255CH, 255BH, 2510H,

                       2514H, 2534H, 252CH, 251CH, 2500H, 253CH, 255EH, 255FH,
                       255AH, 2554H, 2569H, 2566H, 2560H, 2550H, 256CH, 2567H,
                       2568H, 2564H, 2565H, 2559H, 2558H, 2552H, 2553H, 256BH,
                       256AH, 2518H, 250CH, 2588H, 2584H, 258CH, 2590H, 2580H,

                       0440H, 0441H, 0442H, 0443H, 0444H, 0445H, 0446H, 0447H,
                       0448H, 0449H, 044AH, 044BH, 044CH, 044DH, 044EH, 044FH,
                       0401H, 0451H, 0404H, 0454H, 0407H, 0457H, 040EH, 045EH,
                       00B0H, 2219H, 00B7H, 221AH, 2116H, 00A4H, 25A0H, 00A0H
                       };


VAR
    (* Translation from upper-half character to UTF-8. *)

    CodeTable: UpperPage;

(********************************************************************************)
(*                           TRANSLATION TO UTF-8                               *)
(********************************************************************************)

PROCEDURE UTF (ch: CHAR): CARDINAL;

    (* Translates one character from current code page to its 32-bit UTF code. *)

    BEGIN
        IF ch < MIN(UpperCodeRange) THEN
            (* 7-bit ASCII, for which Unicode = ASCII. *)

            RETURN ORD(ch);
        ELSE
            RETURN CodeTable[ch];
        END (*IF*);
    END UTF;

(********************************************************************************)

PROCEDURE TranslateToUTF8 (string: ARRAY OF CHAR;
                           VAR (*OUT*) result: ARRAY OF CHAR);

    (* Translates string, in the encoding used by the current code page, to a   *)
    (* UTF-8 representation of the same string.                                 *)

    VAR k: CARDINAL;

    (****************************************************************************)

    PROCEDURE PutChar (ch: CHAR);

        (* Appends ch to the result string. *)

        BEGIN
            IF k <= HIGH(result) THEN
                result[k] := ch;  INC(k);
            END (*IF*);
        END PutChar;

    (****************************************************************************)

    PROCEDURE PutUTF8tail (N, val: CARDINAL);

        (* Decomposes val into N 6-bit characters, sets the high bit of each    *)
        (* of those characters, and appends them to the result string.          *)

        BEGIN
            IF N > 1 THEN
                PutUTF8tail (N-1, val DIV 64);
                val := val MOD 64;
            END (*IF*);
            PutChar (CHR(080H + ORD(val)));
        END PutUTF8tail;

    (****************************************************************************)

    VAR j, val: CARDINAL;

    BEGIN
        j := 0;  k := 0;
        WHILE (j <= HIGH(string)) AND (k <= HIGH(result)) AND (string[j] <> Nul) DO
            val := UTF(string[j]);  INC(j);
            IF val < 128 THEN

                (* One-byte result *)

                PutChar (CHR(val));

                (* But carriage return must be padded. *)

                IF val = ORD(CR) THEN
                    PutChar (Nul);
                END (*IF*);

            ELSIF val < 800H THEN
                (* Two-byte result *)
                PutChar (CHR(0C0H + val DIV 40H));
                PutUTF8tail (1, val MOD 40H);
            ELSIF val < 10000H THEN
                (* Three-byte result *)
                PutChar (CHR(0E0H + val DIV 1000H));
                PutUTF8tail (2, val MOD 1000H);
            ELSIF val < 200000H THEN
                (* Four-byte result *)
                PutChar (CHR(0F0H + val DIV 40000H));
                PutUTF8tail (3, val MOD 40000H);
            ELSIF val < 4000000H THEN
                (* Five-byte result *)
                PutChar (CHR(0F8H + val DIV 1000000H));
                PutUTF8tail (4, val MOD 1000000H);
            ELSE
                (* Six-byte result *)
                PutChar (CHR(0FCH + val DIV 40000000H));
                PutUTF8tail (5, val MOD 40000000H);
            END (*IF*);

        END (*WHILE*);

        (* Terminate the result string with a Nul. *)

        PutChar (Nul);

    END TranslateToUTF8;

(********************************************************************************)
(*                          TRANSLATION FROM UTF-8                              *)
(********************************************************************************)

PROCEDURE SearchTable (val: CARDINAL): CHAR;

    (* Looks for val in the current code table.  If not found, we just return   *)
    (* the low-order 8 bits of val.                                             *)

    VAR ch: CHAR;  found: BOOLEAN;

    BEGIN
        ch := MIN(UpperCodeRange);  found := FALSE;
        WHILE NOT found AND (ch < MAX(UpperCodeRange)) DO
            IF CodeTable[ch] = val THEN
                found := TRUE;
            ELSE
                INC (ch);
            END (*IF*);
        END (*WHILE*);

        IF found OR ((ch = MAX(UpperCodeRange)) AND (CodeTable[ch] = val)) THEN
            RETURN ch;
        ELSE
            RETURN CHR(IAND(val, 0FFH));
            (* RETURN '?'; *)             (* would this be a better choice? *)
        END (*IF*);

    END SearchTable;

(********************************************************************************)

PROCEDURE ExtASCII (val: CARDINAL): CHAR;

    (* Translates a 32-bit UTF code to a character in the current code page. *)

    (* In the present implementation I am willing to accept the inefficiency *)
    (* of a table search for each non-ASCII character, simply because such   *)
    (* characters are a minority in the typical filename string.             *)

    BEGIN
        IF val < 128 THEN
            (* 7-bit ASCII, for which Unicode = ASCII. *)

            RETURN CHR(val);
        ELSE
            RETURN SearchTable (val);
        END (*IF*);
    END ExtASCII;

(********************************************************************************)

PROCEDURE TranslateFromUTF8 (string: ARRAY OF CHAR;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Translates string from UTF-8 to the extended ASCII code for the current  *)
    (* code page.  If string is not valid UTF-8, result is an unmodified copy   *)
    (* of string.  If it is valid UTF-8 but contains characters that cannot     *)
    (* be represented in the current code page, we translate each such          *)
    (* code to its low-order byte.  (This is probably as good as any other      *)
    (* arbitrary decision.)                                                     *)

    VAR k: CARDINAL;

    (****************************************************************************)

    PROCEDURE PutChar (ch: CHAR);

        (* Appends ch to the result string. *)

        BEGIN
            IF k <= HIGH(result) THEN
                result[k] := ch;  INC(k);
            END (*IF*);
        END PutChar;

    (****************************************************************************)

    VAR j, val, N, m, bitmask: CARDINAL;
        ch: CHAR;
        validcode: BOOLEAN;

    BEGIN
        j := 0;  k := 0;
        validcode := TRUE;
        WHILE validcode AND (j <= HIGH(string))
                        AND (k <= HIGH(result)) AND (string[j] <> Nul) DO

            ch := string[j];  INC(j);
            IF ORD(ch) < 80H THEN

                (* One-byte result *)

                PutChar (ch);

                (* Compensate for padding code after carriage return. *)

                IF ch = CR THEN
                    validcode := string[j] = Nul;
                    IF validcode THEN
                        INC (j);
                    END (*IF*);
                END (*IF*);

            ELSE
                (* Find the first 0 bit, counting from the high-order bit. *)

                bitmask := 40H;  N := 0;
                WHILE IAND (ORD(ch), bitmask) <> 0 DO
                    INC (N);  bitmask := bitmask DIV 2;
                END (*WHILE*);

                (* On exit from this loop, N is equal to the number of  *)
                (* bytes to follow for the current character.           *)

                validcode := (N > 0) AND (N < 6);
                IF validcode THEN

                    val := IAND (ORD(ch), bitmask-1);

                    (* We have the first byte of a valid UTF-8 code, although   *)
                    (* we still have to verify that the remaining N bytes have  *)
                    (* binary 10 in their top two bits.                         *)

                    FOR m := 1 TO N DO
                        IF validcode THEN
                            ch := string[j];  INC(j);
                            validcode := IAND(ORD(ch), 0C0H) = 80H;
                            val := 40H*val + ORD(ch) - 80H;
                        END (*IF*);
                    END (*FOR*);

                    IF validcode THEN

                        (* We have passed all validity tests, so store result.  *)

                        PutChar(ExtASCII(val));

                    END (*IF*);

                END (*IF*);

            END (*IF*);

        END (*WHILE*);

        IF validcode THEN

            (* Terminate the result string with a Nul. *)

            IF k <= HIGH(result) THEN
                result[k] := Nul;
            END (*IF*);
        ELSE

            (* The above translation was aborted because of an invalid  *)
            (* UTF-8 string, so just copy the input directly.           *)

            Strings.Assign (string, result);

        END (*IF*);

    END TranslateFromUTF8;

(********************************************************************************)
(*                       SETTING THE CURRENT CODE PAGE DATA                     *)
(********************************************************************************)

PROCEDURE SetCodePageData (P: CARDINAL);

     (* Sets our current code page to page P.  *)

    VAR ch: CHAR;

    BEGIN
        IF P = 437 THEN
            FOR ch := MIN(UpperCodeRange) TO MAX(UpperCodeRange) DO
                CodeTable[ch] := CP437[ch];
            END (*FOR*);
        ELSIF P = 850 THEN
            FOR ch := MIN(UpperCodeRange) TO MAX(UpperCodeRange) DO
                CodeTable[ch] := CP850[ch];
            END (*FOR*);
        ELSIF P = 866 THEN
            FOR ch := MIN(UpperCodeRange) TO MAX(UpperCodeRange) DO
                CodeTable[ch] := CP866[ch];
            END (*FOR*);
        ELSE
            (* Unsupported code page, set the translation table to be  *)
            (* the identity transformation.                            *)

            FOR ch := MIN(UpperCodeRange) TO MAX(UpperCodeRange) DO
                CodeTable[ch] := ORD(ch);
            END (*FOR*);
        END (*IF*);

    END SetCodePageData;

(********************************************************************************)

PROCEDURE GetCurrentCodePage;

    (* Finds out our current code page, and initialises this module's data      *)
    (* to reflect that.                                                         *)

    CONST len = 8;

    VAR pagelist: ARRAY [0..len-1] OF CARDINAL;
        count: CARDINAL;

    BEGIN
        DosQueryCp (len, pagelist, count);
        SetCodePageData (pagelist[0]);
    END GetCurrentCodePage;

(********************************************************************************)

PROCEDURE ChangeCurrentCodePage (P: CARDINAL);

    (* Change the current process's code page to page P.  It is almost never    *)
    (* necessary to call this procedure, because most programs will want to     *)
    (* keep the code page they were started with.                               *)

    (* Warning: this will affect every thread in this process, including those  *)
    (* that are relying on previously scanned directory listings.  That can     *)
    (* lead to files becoming misplaced because their names have changed.  In   *)
    (* general it is unwise to call this procedure at any point other than      *)
    (* program initialisation.                                                  *)

    BEGIN
        DosSetProcessCp (P);
        SetCodePageData (P);
    END ChangeCurrentCodePage;

(********************************************************************************)
(*                             MODULE INITIALISATION                            *)
(********************************************************************************)

BEGIN
    GetCurrentCodePage;
END CodePage.

