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

IMPLEMENTATION MODULE EditFilter;

        (************************************************************)
        (*                                                          *)
        (*                 PM Setup for FtpServer                   *)
        (*      Dialogue to edit one allow/refuse filter line       *)
        (*                                                          *)
        (*    Started:        14 October 1999                       *)
        (*    Last edited:    7 January 2013                        *)
        (*    Status:         Testing new options                   *)
        (*                                                          *)
        (************************************************************)

IMPORT OS2, DID, Remote, Strings;

FROM IPFilters IMPORT
    (* type *)  ListPtr, RecordKind,
    (* proc *)  CardinalToDotted, StringToIPAddress;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4;

FROM LowLevel IMPORT
    (* proc *)  INOT, IXOR, IAND;

(**************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    Digits = CharSet{'0'..'9'};

VAR
    OKPressed: BOOLEAN;

    (* The sort of record we are dealing with, range [0..4].  *)

    CurrentKind: RecordKind;

(************************************************************************)
(*                          TYPE CONVERSIONS                            *)
(************************************************************************)

PROCEDURE StringToCardinal (str: ARRAY OF CHAR): CARDINAL;

    (* Convert string to numeric. *)

    VAR result, pos: CARDINAL;

    BEGIN
        result := 0;
        pos := 0;
        WHILE (pos <= HIGH(str)) AND (str[pos] = ' ') DO
            INC (pos);
        END (*WHILE*);
        WHILE (pos <= HIGH(str)) AND (str[pos] IN Digits) DO
            result := 10*result + ORD(str[pos]) - ORD('0');
            INC (pos);
        END (*WHILE*);
        RETURN result;
    END StringToCardinal;

(************************************************************************)

PROCEDURE CardinalToString (val: CARDINAL;  VAR (*OUT*) str: ARRAY OF CHAR);

    (* Convert numeric to string. *)

    VAR pos: CARDINAL;

    (********************************************************************)

    PROCEDURE CtoS (N: CARDINAL);

        (* Converts N starting at str[pos]. *)

        BEGIN
            IF N > 9 THEN
                CtoS (N DIV 10);
                N := N MOD 10;
            END (*IF*);
            IF pos <= HIGH(str) THEN
                str[pos] := CHR(ORD('0')+N);
                INC (pos);
            END (*IF*);
        END CtoS;

    (********************************************************************)

    BEGIN
        pos := 0;
        CtoS (val);
        IF pos <= HIGH(str) THEN
            str[pos] := Nul;
        END (*IF*);
    END CardinalToString;

(************************************************************************)

PROCEDURE BitsToMask (numbits: CARDINAL): CARDINAL;

    (* Convert a CIDR bit count to an IP address mask. *)

    CONST TopBit = 080000000H;

    VAR result: CARDINAL;

    BEGIN
        result := 0;
        WHILE numbits > 0 DO
            result := result DIV 2 + TopBit;
            DEC (numbits);
        END (*WHILE*);
        RETURN Swap4(result);
    END BitsToMask;

(************************************************************************)

PROCEDURE MaskToBits (mask: CARDINAL): CARDINAL;

    (* Convert an IP address mask to a CIDR bit count. *)

    CONST TopBit = 080000000H;

    VAR result: CARDINAL;

    BEGIN
        result := 32;
        mask := Swap4(mask);
        WHILE (result > 0) AND NOT ODD(mask) DO
            DEC (result);
            mask := mask DIV 2;
        END (*WHILE*);
        RETURN result;
    END MaskToBits;

(************************************************************************)

PROCEDURE RangeToMask (lower, upper: CARDINAL): CARDINAL;

    (* Convert an IP address range to an IP address mask. *)

    CONST TopBit = 080000000H;

    VAR result: CARDINAL;

    BEGIN
        result := INOT(IXOR (lower, upper));
        RETURN result;
    END RangeToMask;

(************************************************************************)

PROCEDURE MaskToRange (lower, mask: CARDINAL): CARDINAL;

    (* Given the lower IP address and a mask, returns the upper address. *)

    CONST TopBit = 080000000H;

    VAR result: CARDINAL;

    BEGIN
        result := lower + INOT(mask);
        RETURN result;
    END MaskToRange;

(************************************************************************)
(*              OPERATIONS THAT DEPEND ON THE RECORD TYPE               *)
(************************************************************************)

PROCEDURE ShowFields (hwnd: OS2.HWND);

    (* Sets the visibility of the various elements of the dialogue       *)
    (* according to the value of CurrentKind.                            *)

    BEGIN
        (* Type buttons. *)

        IF CurrentKind = 0 THEN
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.SingleBtn), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskedBtn), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.RangeBtn), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.CIDRBtn), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), FALSE);
        ELSE
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.SingleBtn), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskedBtn), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.RangeBtn), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.CIDRBtn), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
        END (*IF*);

        (* Address/mask labels. *)

        IF CurrentKind = 4 THEN
            OS2.WinSetDlgItemText (hwnd, DID.IPAddressLabel, "From");
            OS2.WinSetDlgItemText (hwnd, DID.MaskLabel, "To");
        ELSE
            OS2.WinSetDlgItemText (hwnd, DID.IPAddressLabel, "IP Address");
            OS2.WinSetDlgItemText (hwnd, DID.MaskLabel, "Mask");
        END (*IF*);

        (* Fields unique to the CIDR type. *)

        IF CurrentKind = 3 THEN
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Slash), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.NumBits), TRUE);
        ELSE
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Slash), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.NumBits), FALSE);
        END (*IF*);

        (* All other elements. *)

        IF CurrentKind = 0 THEN
            (* final catchall *)

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Mask), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskLabel), FALSE);

        ELSIF CurrentKind = 1 THEN
            (* masked *)

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Mask), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskLabel), TRUE);

        ELSIF CurrentKind = 2 THEN
            (* single *)

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Mask), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskLabel), FALSE);

        ELSIF CurrentKind = 3 THEN
            (* CIDR *);

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Mask), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskLabel), FALSE);

        ELSIF CurrentKind = 4 THEN
            (* range *)

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.IPAddressField), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.Mask), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd,DID.MaskLabel), TRUE);

        END (*IF*);

   END ShowFields;

(************************************************************************)

PROCEDURE ConvertValues (hwnd: OS2.HWND;  newkind: RecordKind);

    (* Updates some of the fields to reflect a change in record kind. *)

    VAR TextBuffer: ARRAY [0..15] OF CHAR;

    (********************************************************************)

    PROCEDURE GetFirstAddress(): CARDINAL;

        (* Returns the value in the IPAddressField dialogue element. *)

        BEGIN
            OS2.WinQueryDlgItemText (hwnd, DID.IPAddressField, 16, TextBuffer);
            RETURN StringToIPAddress (TextBuffer);
        END GetFirstAddress;

    (********************************************************************)

    PROCEDURE GetMask(): CARDINAL;

        (* Returns the value in the Mask dialogue element. *)

        BEGIN
            OS2.WinQueryDlgItemText (hwnd, DID.Mask, 16, TextBuffer);
            RETURN StringToIPAddress (TextBuffer);
        END GetMask;

    (********************************************************************)

    PROCEDURE GetNumbits(): CARDINAL;

        (* Returns the value in the NumBits dialogue element. *)

        BEGIN
            OS2.WinQueryDlgItemText (hwnd, DID.NumBits, 16, TextBuffer);
            RETURN StringToCardinal (TextBuffer);
        END GetNumbits;

    (********************************************************************)

    VAR val1, val2: CARDINAL;
        newbits, newmask: BOOLEAN;

    BEGIN
        newbits := FALSE;  newmask := FALSE;  val2 := 0;

        IF newkind = 1 THEN
            (* to masked *)

            IF CurrentKind = 2 THEN
                (* from single *)
                val2 := MAX(CARDINAL);
                newmask := TRUE;

            ELSIF CurrentKind = 3 THEN
                (* from CIDR *);
                val2 := GetNumbits();
                val2 := BitsToMask(val2);
                newmask := TRUE;

            ELSIF CurrentKind = 4 THEN
                (* from range *)
                val1 := GetFirstAddress();
                val2 := GetMask();
                val2 := RangeToMask (val1, val2);
                newmask := TRUE;

            END (*IF*);

        ELSIF newkind = 2 THEN
            (* to single - nothing to do *)

        ELSIF newkind = 3 THEN
            (* to CIDR *);

            IF CurrentKind = 1 THEN
                (* from masked *)
                val2 := GetMask();
                val2 := MaskToBits(val2);
                newbits := TRUE;

            ELSIF CurrentKind = 2 THEN
                (* from single *)
                val2 := 32;
                newbits := TRUE;

            ELSIF CurrentKind = 4 THEN
                (* from range *)
                val1 := GetFirstAddress();
                val2 := GetMask();
                val2 := MaskToBits(RangeToMask (val1, val2));
                newbits := TRUE;

            END (*IF*);

        ELSIF newkind = 4 THEN
            (* to range *)

            IF CurrentKind = 1 THEN
                (* from masked *)
                val1 := GetFirstAddress();
                val2 := GetMask();
                val2 := MaskToRange (val1, val2);
                newmask := TRUE;

            ELSIF CurrentKind = 2 THEN
                (* from single *)
                val2 := GetFirstAddress();
                newmask := TRUE;

            ELSIF CurrentKind = 3 THEN
                (* from CIDR *);
                val1 := GetFirstAddress();
                val2 := GetNumbits();
                val2 := BitsToMask(val2);
                val2 := MaskToRange (val1, val2);
                newmask := TRUE;

            END (*IF*);

        END (*IF*);

        IF newbits THEN
            CardinalToString (val2, TextBuffer);
            OS2.WinSetDlgItemText (hwnd, DID.NumBits, TextBuffer);
        END (*IF*);
        IF newmask THEN
            CardinalToDotted (val2, TextBuffer);
            OS2.WinSetDlgItemText (hwnd, DID.Mask, TextBuffer);
        END (*IF*);

        CurrentKind := newkind;
        ShowFields (hwnd);

    END ConvertValues;

(************************************************************************)
(*                   THE "EDIT FILTER ITEM" DIALOGUE                    *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR NotificationCode, ButtonID: CARDINAL;
        ButtonValue: BOOLEAN;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   RETURN NIL;

           |  OS2.WM_COMMAND:
                   IF OS2.SHORT1FROMMP(mp1) = DID.FIOK THEN
                       OKPressed := TRUE;
                       OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;
                   ELSIF OS2.SHORT1FROMMP(mp1) = DID.FICancel THEN
                       OKPressed := FALSE;
                       OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

          |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   ButtonValue := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, ButtonID,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;

                   IF (NotificationCode = OS2.BN_CLICKED) AND ButtonValue THEN

                       CASE ButtonID OF

                         | DID.MaskedBtn:
                           ConvertValues(hwnd, 1);
                           RETURN NIL;

                         | DID.SingleBtn:
                           ConvertValues(hwnd, 2);
                           RETURN NIL;

                         | DID.CIDRBtn:
                           ConvertValues(hwnd, 3);
                           RETURN NIL;

                         | DID.RangeBtn:
                           ConvertValues(hwnd, 4);
                           RETURN NIL;

                         ELSE
                             RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*CASE*);

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  p: ListPtr): BOOLEAN;

    (* Edit the filter line p^, which is the current item in the listbox whose  *)
    (* handle is "owner".                                                       *)

    VAR hwnd: OS2.HWND;
        address, numbits, mask: CARDINAL;
        TextBuffer: ARRAY [0..15] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP,    (* parent *)
                       owner,              (* owner *)
                       DialogueProc,       (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.FilterItem,     (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "EditFilter");

        (* Set the allow/refuse check buttons correctly.  The strange   *)
        (* code below is because of a bug report that seemed to imply   *)
        (* that p^.allow had a value other than 0 or 1.                 *)

        IF p^.allow THEN mask := 1
        ELSE mask := 0;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.AllowButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(mask), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.RefuseButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(1-mask), NIL);

        (* Fill in the other dialogue fields. *)

        CurrentKind := p^.type;
        IF p^.next = NIL THEN
            OS2.WinSetDlgItemText (hwnd, DID.IPAddressField, "ALL");
        ELSE
            IF CurrentKind = 1 THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.MaskedBtn,
                             OS2.BM_SETCHECK, OS2.MPFROMSHORT(1), NIL);
            ELSIF CurrentKind = 2 THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.SingleBtn,
                             OS2.BM_SETCHECK, OS2.MPFROMSHORT(1), NIL);
            ELSIF CurrentKind = 3 THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.CIDRBtn,
                             OS2.BM_SETCHECK, OS2.MPFROMSHORT(1), NIL);
            ELSIF CurrentKind = 4 THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.RangeBtn,
                             OS2.BM_SETCHECK, OS2.MPFROMSHORT(1), NIL);
            END (*IF*);

            CardinalToDotted (p^.address, TextBuffer);
            OS2.WinSetDlgItemText (hwnd, DID.IPAddressField, TextBuffer);
            IF CurrentKind = 3 THEN
                CardinalToString (p^.mask, TextBuffer);
                OS2.WinSetDlgItemText (hwnd, DID.NumBits, TextBuffer);
            ELSE
                CardinalToDotted (p^.mask, TextBuffer);
                OS2.WinSetDlgItemText (hwnd, DID.Mask, TextBuffer);
            END (*IF*);
        END (*IF*);
        ShowFields (hwnd);
        OKPressed := FALSE;

        OS2.WinProcessDlg(hwnd);

        (* On dialogue exit, read the updated values. *)

        IF OKPressed THEN
            p^.allow := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.AllowButton,
                                                  OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;

            p^.type := CurrentKind;
            IF CurrentKind <> 0 THEN
                OS2.WinQueryDlgItemText (hwnd, DID.IPAddressField, 16, TextBuffer);
                address := StringToIPAddress (TextBuffer);
                OS2.WinQueryDlgItemText (hwnd, DID.NumBits, 16, TextBuffer);
                numbits := StringToCardinal (TextBuffer);
                OS2.WinQueryDlgItemText (hwnd, DID.Mask, 16, TextBuffer);
                mask := StringToIPAddress (TextBuffer);
                CASE CurrentKind OF
                    | 1:       (* masked *)
                         p^.address := IAND (address, mask);
                         p^.mask := mask;
                    | 2:       (* single *)
                         p^.address2 := address;
                         p^.dummy := MAX(CARDINAL);
                    | 3:       (* CIDR *)
                         p^.address3 := IAND (address, BitsToMask(numbits));
                         p^.bitcount := numbits;
                    | 4:       (* range *)
                         p^.firstaddr := address;
                         p^.lastaddr := mask;
                    ELSE
                         (* do nothing *)
                END (*CASE*);
            END (*IF*);
        END (*IF*);

        Remote.StoreWindowPosition (hwnd, "EditFilter", TRUE);
        OS2.WinDestroyWindow (hwnd);

        RETURN OKPressed;

    END Edit;

(************************************************************************)

BEGIN
    OKPressed := FALSE;
    CurrentKind := 0;
END EditFilter.

