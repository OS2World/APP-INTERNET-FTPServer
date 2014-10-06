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

IMPLEMENTATION MODULE IPFilter;

        (********************************************************)
        (*                                                      *)
        (*   Checks whether a client IP address is acceptable.  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 September 2008                *)
        (*  Last edited:        22 May 2012                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT LOC, CARD8;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, ItemSize, INIGetTrusted;

FROM Names IMPORT
    (* type *)  UserName;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

(********************************************************************************)

TYPE
    (* Records used in the checking of which IP addresses to allow and  *)
    (* which to exclude.                                                *)

    RecordKind = CARD8[0..4];

    AddressListPointer = POINTER TO
                             RECORD
                                 allow: BOOLEAN;
                                 type: RecordKind;
                                 IPAddress, mask: CARDINAL;
                                 next: AddressListPointer;
                             END (*RECORD*);

(********************************************************************************)

VAR
    (* Lists of IP addresses against we check each new client,  *)
    (* and a lock to protect them.                              *)

    MasterHostFilter: AddressListPointer;
    MasterHostFilterLock: Lock;

(************************************************************************)
(*                    LOADING AND UNLOADING FILTERS                     *)
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

PROCEDURE LoadFilter (hini: HINI;  app: ARRAY OF CHAR): AddressListPointer;

    (* Loads a filter list from the INI file into memory.  We process   *)
    (* the records such that, after loading, only types 0, 1, and 4     *)
    (* can occur.                                                       *)

    VAR j, size: CARDINAL;
        bufptr: POINTER TO ARRAY [0..65535] OF LOC;
        result, current: AddressListPointer;
        key: ARRAY [0..8] OF CHAR;

    (********************************************************************)

    PROCEDURE Get (VAR (*OUT*) x: ARRAY OF LOC);

        (* Copies the right number of bytes from bufptr[j] to x, and    *)
        (* updates j.                                                   *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(x) DO
                x[k] := bufptr^[j];  INC(j);
            END (*FOR*);
        END Get;

    (********************************************************************)

    BEGIN
        result := NIL;
        IF INIValid (hini)
                 AND (ItemSize (hini, app, "IPfilter", size)) THEN
            IF size <> 0 THEN
                ALLOCATE (bufptr, size);
                key := "IPfilter";
                EVAL(INIGetTrusted (hini, app, key, bufptr^, size));
                j := 0;  current := NIL;
                WHILE j < size DO
                    IF current = NIL THEN
                        NEW (current);
                        result := current;
                    ELSE
                        NEW (current^.next);
                        current := current^.next;
                    END (*IF*);
                    current^.next := NIL;
                    Get (current^.allow);
                    Get (current^.type);
                    IF current^.type > 4 THEN
                        current^.type := 2;
                    END (*IF*);
                    IF current^.type = 0 THEN
                        current^.IPAddress := 0;
                        current^.mask := 0;
                        j := size;
                    ELSE
                        Get (current^.IPAddress);
                        Get (current^.mask);
                    END (*IF*);

                    (* Types 2 and 3 can be treated as special *)
                    (* cases of type 1.                        *)

                    IF current^.type = 2 THEN
                        current^.mask := MAX(CARDINAL);
                    ELSIF current^.type = 3 THEN
                        current^.mask := BitsToMask (current^.mask);
                    END (*IF*);

                END (*WHILE*);
                DEALLOCATE (bufptr, size);
            END (*IF*);
        END (*IF*);
        RETURN result;
    END LoadFilter;

(************************************************************************)

PROCEDURE UnloadFilter (VAR (*INOUT*) FL: AddressListPointer);

    (* Disposes of a filter list.  *)

    VAR current: AddressListPointer;

    BEGIN
        WHILE FL <> NIL DO
            current := FL;
            FL := current^.next;
            DISPOSE (current);
        END (*WHILE*);
    END UnloadFilter;

(********************************************************************************)
(*                          SCANNING A FILTER LIST                              *)
(********************************************************************************)

PROCEDURE ScanList (L: AddressListPointer;  IPAddress: CARDINAL): BOOLEAN;

    (* Scan the list L to see whether this is an acceptable client. *)

    VAR current: AddressListPointer;
        addr: CARDINAL;
        match: BOOLEAN;

    BEGIN
        current := L;
        match := FALSE;
        WHILE NOT match DO
            IF (current = NIL) OR (current^.type = 0) THEN
                match := TRUE;
            ELSIF current^.type = 4 THEN
                addr := Swap4(IPAddress);
                match := (addr >= Swap4(current^.IPAddress))
                           AND (addr <= Swap4(current^.mask))
            ELSE
                match := (IAND (IPAddress, current^.mask) = current^.IPAddress);
            END (*IF*);
            IF NOT match THEN
                current := current^.next;
            END (*IF*);
        END (*WHILE*);
        RETURN (current = NIL) OR current^.allow;
    END ScanList;

(************************************************************************)

PROCEDURE CheckMasterIPFilter (IPAddress: CARDINAL): BOOLEAN;

    (* Scan the master HostFilter to see whether this is an acceptable client. *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (MasterHostFilterLock);
        result := ScanList (MasterHostFilter, IPAddress);
        Release (MasterHostFilterLock);
        RETURN result;
    END CheckMasterIPFilter;

(************************************************************************)

PROCEDURE CheckUserIPFilter (hini: HINI;  user: ARRAY OF CHAR;
                                IPAddress: CARDINAL): BOOLEAN;

    (* Scan the filter for "user" to see whether this is an acceptable client. *)

    VAR L: AddressListPointer;
        result: BOOLEAN;

    BEGIN
        L := LoadFilter (hini, user);
        result := ScanList (L, IPAddress);
        UnloadFilter (L);
        RETURN result;
    END CheckUserIPFilter;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE UpdateMasterHostFilter (hini: HINI);

    (* Constructs the master host filter list from the INI file data.  *)

    BEGIN
        Obtain (MasterHostFilterLock);
        UnloadFilter (MasterHostFilter);
        MasterHostFilter := LoadFilter (hini, "$SYS");
        Release (MasterHostFilterLock);
    END UpdateMasterHostFilter;

(************************************************************************)

BEGIN
    CreateLock (MasterHostFilterLock);
    MasterHostFilter := NIL;
END IPFilter.

