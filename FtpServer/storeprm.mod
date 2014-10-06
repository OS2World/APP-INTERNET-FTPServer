(**************************************************************************)
(*                                                                        *)
(*  StorePRM utility for FtpServer                                        *)
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

MODULE StorePRM;

        (********************************************************)
        (*                                                      *)
        (*  Program to store data from ftpd.ini to PRM files    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 March 1998                    *)
        (*  Last edited:        9 June 2013                     *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  LOC,
    (* proc *)  ADR;

IMPORT OS2, IOChan, ChanConsts, Strings, STextIO, TextIO, SeqFile, FileSys;

FROM INIData IMPORT
    (* type *)  HINI, StringReadState,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile, ItemSize,
                INIGet, INIGetString, INIGetTrusted,
                GetStringList, NextString, CloseStringList;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    NameString = ARRAY [0..31] OF CHAR;
    RealNameString = ARRAY [0..63] OF CHAR;
    NoteString = ARRAY [0..2047] OF CHAR;
    FileNameString = ARRAY [0..255] OF CHAR;
    CharSet = SET OF CHAR;
    UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser, NormalUser,
                    Manager, UserTemplate);

(************************************************************************)

VAR
    (* Anchor block handle for this application.  *)

    hab: OS2.HAB;

(********************************************************************************)
(*                          OUTPUT TO PRM FILE                                  *)
(********************************************************************************)

PROCEDURE WriteRaw (cid: IOChan.ChanId;  data: ARRAY OF LOC;  amount: CARDINAL);

    (* Writes a string to a file. *)

    BEGIN
        IOChan.RawWrite (cid, ADR(data), amount);
    END WriteRaw;

(************************************************************************)

PROCEDURE FWriteChar (cid: IOChan.ChanId;  character: CHAR);

    (* Writes a single character to a file. *)

    BEGIN
        IOChan.RawWrite (cid, ADR(character), 1);
    END FWriteChar;

(************************************************************************)

PROCEDURE FWriteString (cid: IOChan.ChanId;  string: ARRAY OF CHAR);

    (* Writes a string to a file. *)

    BEGIN
        IOChan.RawWrite (cid, ADR(string), LENGTH(string));
    END FWriteString;

(************************************************************************)

PROCEDURE FWriteQuotedString (cid: IOChan.ChanId;  string: ARRAY OF CHAR);

    (* Writes a string to a file, surrounded by quotation marks. *)

    BEGIN
        FWriteChar (cid, '"');
        IOChan.RawWrite (cid, ADR(string), LENGTH(string));
        FWriteChar (cid, '"');
    END FWriteQuotedString;

(************************************************************************)

PROCEDURE FWriteCard (cid: IOChan.ChanId;  number: CARDINAL);

    (* Writes a cardinal number to a file.  *)

    BEGIN
        IF number > 9 THEN
            FWriteCard (cid, number DIV 10);
        END (*IF*);
        FWriteChar (cid, CHR(ORD('0') + number MOD 10));
    END FWriteCard;

(************************************************************************)

PROCEDURE FWriteLn (cid: IOChan.ChanId);

    (* Writes end-of-line to the file. *)

    TYPE TwoChar = ARRAY [0..1] OF CHAR;
    CONST CRLF = TwoChar {CHR(13), CHR(10)};

    BEGIN
        WriteRaw (cid, CRLF, 2);
    END FWriteLn;

(************************************************************************)

PROCEDURE OpenPRMFile (VAR (*OUT*) cid: IOChan.ChanId;
                       username: ARRAY OF CHAR): BOOLEAN;

    VAR filename, BAKname: FileNameString;
        result: ChanConsts.OpenResults;  dummy: BOOLEAN;

    BEGIN
        Strings.Assign (username, filename);
        Strings.Append (".PRM", filename);
        SeqFile.OpenWrite (cid, filename, ChanConsts.write+ChanConsts.raw, result);
        IF result = ChanConsts.fileExists THEN
            Strings.Assign (username, BAKname);
            Strings.Append (".BAK", BAKname);
            FileSys.Remove (BAKname, dummy);
            FileSys.Rename (filename, BAKname, dummy);
            SeqFile.OpenWrite (cid, filename, ChanConsts.write+ChanConsts.raw, result);
        END (*IF*);
        RETURN result = ChanConsts.opened;
    END OpenPRMFile;

(************************************************************************)
(*                PRETTYPRINTING OF A USER'S DIRECTORY DATA             *)
(************************************************************************)

PROCEDURE WriteOneChar (cid: IOChan.ChanId;  ch: CHAR);

    (* Writes a single character to the file. *)

    BEGIN
        IOChan.RawWrite (cid, ADR(ch), 1);
    END WriteOneChar;

(************************************************************************)

PROCEDURE NewLine (cid: IOChan.ChanId;  indent: CARDINAL);

    (* Writes a CRLF to the file, followed by indent space characters. *)

    VAR j: CARDINAL;

    BEGIN
        FWriteLn (cid);
        FOR j := 1 TO indent DO
            WriteOneChar (cid, ' ');
        END (*FOR*);
    END NewLine;

(************************************************************************)

PROCEDURE CopyString (cid: IOChan.ChanId;  LookForQuotes: BOOLEAN;
                      Stoppers: CharSet;  VAR (*IN*) Buffer: ARRAY OF CHAR;
                      VAR (*INOUT*) pos: CARDINAL;  BufferSize: CARDINAL);

    (* Writes from Buffer to file, starting at Buffer[pos], and         *)
    (* stopping when we run off the end of the buffer or when           *)
    (* Buffer[pos] contains a character in Stoppers.  If LookForQuotes  *)
    (* is TRUE then we don't look for stopper characters when we're     *)
    (* inside a quoted string.                                          *)

    CONST QuoteChars = CharSet {'"', "'"};

    VAR ch, delimiter: CHAR;

    BEGIN
        WHILE (pos < BufferSize) AND NOT (Buffer[pos] IN Stoppers) DO
            ch := Buffer[pos];  INC (pos);
            WriteOneChar (cid, ch);
            IF LookForQuotes AND (ch IN QuoteChars) THEN
                delimiter := ch;
                CopyString (cid, FALSE, CharSet{delimiter},
                            Buffer, pos, BufferSize);
                IF (pos < BufferSize) AND (Buffer[pos] = delimiter) THEN
                    WriteOneChar (cid, delimiter);
                    INC (pos);
                END (*IF*);
            END (*IF*);
        END (*WHILE*);
    END CopyString;

(************************************************************************)

PROCEDURE WriteDirectoryData (cid: IOChan.ChanId;  indent: CARDINAL;
                      VAR (*IN*) Buffer: ARRAY OF CHAR;
                      VAR (*INOUT*) pos: CARDINAL;  BufferSize: CARDINAL);
                                                                  FORWARD;

(************************************************************************)

PROCEDURE WriteDirectoryList (cid: IOChan.ChanId;  indent: CARDINAL;
                      VAR (*IN*) Buffer: ARRAY OF CHAR;
                      VAR (*INOUT*) pos: CARDINAL;  BufferSize: CARDINAL);

    BEGIN
        WHILE (pos < BufferSize) AND (Buffer[pos] <> ')') DO
            WriteDirectoryData (cid, indent, Buffer, pos, BufferSize);
            IF (pos < BufferSize) AND (Buffer[pos] = ',') THEN
                WriteOneChar (cid, ',');  INC(pos);
                NewLine (cid, indent);
            END (*IF*);
        END (*WHILE*);
    END WriteDirectoryList;

(************************************************************************)

PROCEDURE WriteDirectoryData (cid: IOChan.ChanId;  indent: CARDINAL;
                      VAR (*IN*) Buffer: ARRAY OF CHAR;
                      VAR (*INOUT*) pos: CARDINAL;  BufferSize: CARDINAL);

    (* Writes the data for one directory to the file, starting at       *)
    (* Buffer[pos].                                                     *)

    BEGIN
        CopyString (cid, TRUE, CharSet{',', ')', '('}, Buffer, pos, BufferSize);
        IF (pos < BufferSize) AND (Buffer[pos] = '(') THEN
            NewLine (cid, indent+3);
            WriteOneChar (cid, '(');  INC(pos);
            WriteDirectoryList (cid, indent+4, Buffer, pos, BufferSize);
            IF (pos < BufferSize) AND (Buffer[pos] = ')') THEN
                INC(pos);
            END (*IF*);
            NewLine (cid, indent+3);
            WriteOneChar (cid, ')');
        END (*IF*);
    END WriteDirectoryData;

(************************************************************************)
(*                        PERFORMING THE CONVERSION                     *)
(************************************************************************)

PROCEDURE WriteVolumeData (cid: IOChan.ChanId;
                 VAR (*IN*) Buffer: ARRAY OF CHAR;  BufferSize: CARDINAL);

    (* This is a formatted dump of what's in Buffer.  *)

    VAR pos: CARDINAL;

    BEGIN
        pos := 0;
        WriteDirectoryData (cid, 0, Buffer, pos, BufferSize);
        NewLine (cid, 0);
    END WriteVolumeData;

(************************************************************************)

PROCEDURE INIGetBoolean (hini: HINI;  name1, name2: ARRAY OF CHAR)
                                                                : BOOLEAN;

    (* Load a Boolean value from the INI file. *)

    VAR result: BOOLEAN;

    BEGIN
        IF NOT INIGet (hini, name1, name2, result) THEN
            result := FALSE;
        END (*IF*);
        RETURN result;
    END INIGetBoolean;

(************************************************************************)

PROCEDURE INIGetCard (hini: HINI;  name1, name2: ARRAY OF CHAR): CARDINAL;

    (* Load a Cardinal value from the INI file. *)

    VAR result: CARDINAL;

    BEGIN
        IF NOT INIGet (hini, name1, name2, result) THEN
            result := 0;
        END (*IF*);
        RETURN result;
    END INIGetCard;

(********************************************************************************)

PROCEDURE WriteHideList (cid: IOChan.ChanId;  hini: HINI;  username: ARRAY OF CHAR);

    (* Writes out the HideList for this user. *)

    VAR state: StringReadState;
        key: ARRAY [0..8] OF CHAR;
        str: FileNameString;
        started: BOOLEAN;

    BEGIN
        started := FALSE;
        key := "HideList";
        GetStringList (hini, username, key, state);
        REPEAT
            NextString (state, str);
            IF str[0] <> Nul THEN
                IF NOT started THEN
                    FWriteString (cid, "HideList={");
                    FWriteLn (cid);
                    started := TRUE;
                END (*IF*);
                FWriteString (cid, str);
                FWriteLn (cid);
            END (*IF*);
        UNTIL str[0] = Nul;
        CloseStringList (state);

        IF started THEN
            FWriteString (cid, "}");
            FWriteLn (cid);
        END (*IF*);

    END WriteHideList;

(****************************************************************************)

PROCEDURE ConvertOneUser (hini: HINI;  username: ARRAY OF CHAR);

    (* Converts one INI file entry to a PRM file. *)

    TYPE BufferIndex = [0..65535];
        CategoryMap = ARRAY UserCategory OF CHAR;

    CONST CategoryCode = CategoryMap {'?', 'N', 'G', 'U', 'M', 'T'};

    VAR cid: IOChan.ChanId;
        category: UserCategory;
        password, Template: NameString;
        BufferSize, UserLimit, SpeedLimit, val: CARDINAL;
        realname: RealNameString;
        notes: NoteString;
        bufptr: POINTER TO ARRAY BufferIndex OF CHAR;
        ch: CHAR;

    BEGIN
        STextIO.WriteString ("Converting ");
        STextIO.WriteString (username);
        STextIO.WriteLn;
        IF OpenPRMFile (cid, username) THEN

            UserLimit := MAX(CARDINAL);
            SpeedLimit := MAX(CARDINAL);
            realname := "";
            notes := "";
            category := NormalUser;

            BufferSize := SIZE(UserCategory);
            IF NOT INIGet (hini, username, "Category", category) THEN
                category := NoSuchUser;
            END (*IF*);
            IF category = UserTemplate THEN
                UserLimit := 1;  SpeedLimit := 1;
            END (*IF*);

            ch := CategoryCode[category];
            FWriteChar (cid, ch);
            val := INIGetCard (hini, username, "LoginLimit");
            IF (val>0) OR INIGetBoolean (hini, username, "SingleUse") THEN
                FWriteChar (cid, 'S');
            END (*IF*);
            FWriteLn (cid);

            IF NOT INIGetString (hini, username, "Password", password) THEN
                password := "";
            END (*IF*);
            FWriteQuotedString (cid, password);
            FWriteLn (cid);

            IF NOT INIGetString (hini, username, "Notes", notes) THEN
                notes := "";
            END (*IF*);
            IF (category < UserTemplate) OR (notes[0] <> Nul) THEN

                IF NOT INIGet (hini, username, "UserLimit", UserLimit) THEN
                    UserLimit := MAX(CARDINAL);
                END (*IF*);
                FWriteCard (cid, UserLimit);
                FWriteString (cid, "  ");

                BufferSize := SIZE(CARDINAL);
                IF NOT INIGet (hini, username, "SpeedLimit", SpeedLimit) THEN
                    SpeedLimit := MAX(CARDINAL);
                END (*IF*);
                FWriteCard (cid, SpeedLimit);
                FWriteLn (cid);

                IF NOT INIGetString (hini, username, "RealName", realname) THEN
                    realname := "";
                END (*IF*);
                FWriteQuotedString (cid, realname);
                FWriteLn (cid);

                FWriteString (cid, "(*");
                FWriteString (cid, notes);
                FWriteString (cid, "*)");
                FWriteLn (cid);

            END (*IF*);

            IF INIGetBoolean (hini, username, "UseTemplate") THEN
                IF NOT INIGetString (hini, username, "TemplateName", Template) THEN
                    Template := "";
                END (*IF*);
                FWriteChar (cid, '@');
                FWriteQuotedString (cid, Template);
                FWriteLn (cid);
            ELSIF ItemSize (hini, username, "Volume", BufferSize)
                      AND (BufferSize > 0) THEN
                ALLOCATE (bufptr, BufferSize);
                IF INIGetTrusted (hini, username, "Volume", bufptr^, BufferSize) THEN
                     WriteVolumeData (cid, bufptr^, BufferSize);
                END (*IF*);
                DEALLOCATE (bufptr, BufferSize);
            END (*IF*);

            WriteHideList(cid, hini, username);

            SeqFile.Close (cid);
        ELSE
            STextIO.WriteString ("Can't create ");
            STextIO.WriteString (username);
            STextIO.WriteString (".PRM");
            STextIO.WriteLn;
        END (*IF*);
    END ConvertOneUser;

(********************************************************************************)

PROCEDURE WildcardMatch (string, mask: ARRAY OF CHAR): BOOLEAN;

    (* Checks for string=mask, except that mask is allowed to contain '*' and   *)
    (* '?' wildcard characters.  In this version, '*' is allowed only at the end.*)

    VAR k: CARDINAL;

    BEGIN
        k := 0;
        LOOP
            IF (k > HIGH(mask)) OR (mask[k] = Nul) THEN
                RETURN (k > HIGH(string)) OR (string[k] = Nul);
            ELSIF mask[k] = '*' THEN
                RETURN TRUE;
            ELSIF (k > HIGH(string)) OR (string[k] = Nul) THEN
                RETURN FALSE;
            ELSIF mask[k] = '?' THEN
                INC (k);
            ELSIF CAP(string[k]) <> CAP(mask[k]) THEN
                RETURN FALSE;
            ELSE
                INC (k);
            END (*IF*);
        END (*LOOP*);
    END WildcardMatch;

(********************************************************************************)

PROCEDURE GetParameter (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Picks up program argument from the command line. *)

    VAR args: IOChan.ChanId;  j: CARDINAL;

    BEGIN
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, result);
            j := LENGTH (result);
        ELSE
            j := 0;
        END (*IF*);

        (* Strip trailing spaces. *)

        WHILE (j > 0) AND (result[j-1] = ' ') DO
            DEC (j);
        END (*WHILE*);
        result[j] := CHR(0);
    END GetParameter;

(************************************************************************)

PROCEDURE PerformTheConversions;

    (* Reads command-line argument, converts all the users that match. *)

    TYPE BufferIndex = [0..65535];

    VAR mask: ARRAY [0..127] OF CHAR;
        state: StringReadState;
        hini: HINI;
        BufferSize: CARDINAL;
        Name: ARRAY [0..31] OF CHAR;
        Blank: ARRAY [0..1] OF CHAR;

    BEGIN
        Blank := "";
        GetParameter (mask);
        Name := "ftpd.ini";
        hini := OpenINIFile (Name, FALSE);
        IF (NOT INIValid(hini))
                  OR NOT ItemSize (hini, Blank, "", BufferSize)
                  OR (BufferSize = 0) THEN
            STextIO.WriteString ("Nothing to convert");
            STextIO.WriteLn;
            RETURN;
        END (*IF*);

        GetStringList (hini, Blank, Blank, state);
        LOOP
            NextString (state, Name);
            IF Name[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF WildcardMatch (Name, mask) AND NOT WildcardMatch (Name, '$SYS') THEN
                ConvertOneUser (hini, Name);
            END (*IF*);
        END (*LOOP*);
        CloseStringList (state);
        CloseINIFile (hini);

    END PerformTheConversions;

(********************************************************************************)
(*                               MAIN PROGRAM                                   *)
(********************************************************************************)

BEGIN
    hab := OS2.WinInitialize (0);
    PerformTheConversions;
FINALLY
    IF hab <> OS2.NULLHANDLE THEN
        OS2.WinTerminate (hab);
    END (*IF*);
END StorePRM.

