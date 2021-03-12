(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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
        (*  Last edited:        24 October 2019                 *)
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
    (* proc *)  INIValid, ItemSize, INIGetTrusted, INIGet,
                INIDeleteKey, INIPutBinary;

FROM FtpdINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

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

    (* The record types are:                *)
    (*              0       end of list                                 *)
    (*              1       address and mask                            *)
    (*              2       single address                              *)
    (*              3       address and bit count                       *)
    (*              4       range                                       *)
    (* When loading a list, types 2 and 3 are treated as special cases  *)
    (* of type 1, so 2 and 3 do not appear in the loaded list.  For     *)
    (* type 4, the "mask" field holds the upper address.                *)

(************************************************************************)

VAR
    (* Lists of IP addresses against we check each new client,  *)
    (* and a lock to protect them.                              *)

    MasterHostFilter: AddressListPointer;
    MasterHostFilterLock: Lock;

    (* List of local addresses (only relevant if the "behind firewall"  *)
    (* rules are in use).  We use the same master lock for critical     *)
    (* section protection.                                              *)

    LocalAddrs: AddressListPointer;

    (* A flag to say that we've already dealt with the case of  *)
    (* old-format data.                                         *)

    ConversionDone: BOOLEAN;

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

PROCEDURE LoadFilter (hini: HINI;  app, key: ARRAY OF CHAR): AddressListPointer;

    (* Loads a filter list from the INI file into memory.  We process   *)
    (* the records such that, after loading, only types 0, 1, and 4     *)
    (* can occur.                                                       *)

    VAR j, size: CARDINAL;
        bufptr: POINTER TO ARRAY [0..65535] OF LOC;
        result, current: AddressListPointer;

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
                 AND (ItemSize (hini, app, key, size)) THEN
            IF size <> 0 THEN
                ALLOCATE (bufptr, size);
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

PROCEDURE AddressIsLocal (IPAddress: CARDINAL): BOOLEAN;

    (* Checks whether IPAddress is in the list of local addresses. *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (MasterHostFilterLock);
        result := ScanList (LocalAddrs, IPAddress);
        Release (MasterHostFilterLock);
        RETURN result;
    END AddressIsLocal;

(************************************************************************)

PROCEDURE CheckUserIPFilter (hini: HINI;  user: ARRAY OF CHAR;
                                IPAddress: CARDINAL): BOOLEAN;

    (* Scan the filter for "user" to see whether this is an acceptable client. *)

    VAR L: AddressListPointer;
        result: BOOLEAN;

    BEGIN
        L := LoadFilter (hini, user, "IPfilter");
        result := ScanList (L, IPAddress);
        UnloadFilter (L);
        RETURN result;
    END CheckUserIPFilter;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE StoreFilter (hini: HINI;  app, key: ARRAY OF CHAR;
                                        p: AddressListPointer);

    (* Stores the p^ list to the INI file. *)

    VAR j: CARDINAL;
        bufptr: POINTER TO ARRAY [0..65535] OF LOC;

    (********************************************************************)

    PROCEDURE Put (x: ARRAY OF LOC);

        (* Copies the right number of bytes from x to bufptr[j], and    *)
        (* updates j.                                                   *)

        VAR k: CARDINAL;

        BEGIN
            FOR k := 0 TO HIGH(x) DO
                bufptr^[j] := x[k];  INC(j);
            END (*FOR*);
        END Put;

    (********************************************************************)

    CONST
        R0size = SIZE(BOOLEAN) + SIZE(RecordKind);
        Rsize = R0size + 2*SIZE(CARDINAL);

    VAR size: CARDINAL;
        p0: AddressListPointer;

    BEGIN
        INIDeleteKey (hini, app, key);
        IF p <> NIL THEN

            (* Begin by calculating how much space we need. *)

            p0 := p;  size := R0size;
            WHILE p^.type <> 0 DO
                INC (size, Rsize);
                p := p^.next;
            END (*WHILE*);
            ALLOCATE (bufptr, size);

            (* Copy the data into bufptr^. *)

            p := p0;  j := 0;
            WHILE p^.type <> 0 DO
                Put (p^.allow);
                Put (p^.type);
                Put (p^.IPAddress);
                Put (p^.mask);
                p := p^.next;
            END (*WHILE*);

            (* Next, the end-of-data record. *)

            Put (p^.allow);
            Put (p^.type);

            (* Store the result in the INI file. *)

            INIPutBinary (hini, app, key, bufptr^, size);
            DEALLOCATE (bufptr, size);

        END (*IF*);

    END StoreFilter;

(************************************************************************)

PROCEDURE ConvertLocalFromOldFormat;

    (* If the master address list is empty, re-create it.   *)

    VAR next, p: AddressListPointer;
        MinAddr, MaxAddr: CARDINAL;
        app: ARRAY [0..4] OF CHAR;
        hini: HINI;

    BEGIN
        hini := OpenINIFile();
        app := '$SYS';
        IF INIGet (hini, app, 'MinLocalAddr', MinAddr) THEN

            (* Since that entry exists in the INI file, we have         *)
            (* old-format information.  Delete that, and construct a    *)
            (* list that contains the same information.                 *)

            IF (LocalAddrs <> NIL) AND (LocalAddrs^.type = 0) THEN
    
                (* List starts with end record, so effectively empty. *)
    
                WHILE LocalAddrs <> NIL DO
                    next := LocalAddrs^.next;
                    DISPOSE (LocalAddrs);
                    LocalAddrs := next;
                END (*WHILE*);
    
            END (*IF*);
    
            (* After that preliminary, we have either a genuine list (which *)
            (* we don't want to alter), or a totally empty list.            *)
    
            IF LocalAddrs = NIL THEN

                IF NOT INIGet (hini, app, 'MaxLocalAddr', MaxAddr) THEN
                    MaxAddr := MinAddr;
                END (*IF*);

                (* Create a list that represents the address range. *)

                NEW (LocalAddrs);
                p := LocalAddrs;
                p^.next := NIL;
                p^.allow := TRUE;
                p^.type := 4;
                p^.IPAddress := MinAddr;
                p^.mask := MaxAddr;
    
                NEW (p^.next);
                p := p^.next;
                p^.next := NIL;
                p^.allow := FALSE;
                p^.type := 0;
                p^.IPAddress := 0;
                p^.mask := 0;

                (* Put the list into the INI file to replace the old data. *)

                INIDeleteKey (hini, app, 'MinLocalAddr');
                INIDeleteKey (hini, app, 'MaxLocalAddr');
                StoreFilter (hini, app, "LocalAddrs", LocalAddrs);

            END (*IF*);

        END (*IF*);

        CloseINIFile(hini);
        ConversionDone := TRUE;

    END ConvertLocalFromOldFormat;

(************************************************************************)

PROCEDURE UpdateMasterHostFilter (hini: HINI);

    (* Constructs the master host filter and the local address list     *)
    (* from the INI file data.                                          *)

    BEGIN
        Obtain (MasterHostFilterLock);
        UnloadFilter (MasterHostFilter);
        MasterHostFilter := LoadFilter (hini, "$SYS", "IPfilter");
        UnloadFilter (LocalAddrs);
        LocalAddrs := LoadFilter (hini, "$SYS", "LocalAddrs");
        IF NOT ConversionDone THEN
            ConvertLocalFromOldFormat;
        END (*IF*);
        Release (MasterHostFilterLock);
    END UpdateMasterHostFilter;

(************************************************************************)

BEGIN
    CreateLock (MasterHostFilterLock);
    MasterHostFilter := NIL;
    LocalAddrs := NIL;
    ConversionDone := FALSE;
END IPFilter.

