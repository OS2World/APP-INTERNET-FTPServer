(**************************************************************************)
(*                                                                        *)
(*  Log file analysis for FtpServer                                       *)
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

MODULE LogAnalysis;

        (*****************************************************)
        (*        Analysis of FtpServer user log file        *)
        (*                                                   *)
        (*     Programmer:    P. Moylan                      *)
        (*     Started:       25 August 1999                 *)
        (*     Last edited:   4 December 2014                *)
        (*     Status:        Working                        *)
        (*                                                   *)
        (*****************************************************)


IMPORT Strings, STextIO, TextIO, IOChan, IOConsts, ChanConsts, SeqFile, ProgramArgs;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM VStrings IMPORT
    (* type *)  VString,
    (* proc *)  MakeVString, DiscardVString, VSCompare, WriteVString;

(********************************************************************************)

CONST
    Nul = CHR(0);  CtrlZ = CHR(26);

TYPE
    LogLine = ARRAY [0..511] OF CHAR;
    FilenameString = ARRAY [0..511] OF CHAR;
    TimeString = ARRAY [0..18] OF CHAR;
    LineType = (eof, blank, starttime, endtime, upload, partupload, download,
                partdownload, delete, other);

    PhraseType = ARRAY LineType OF ARRAY [0..15] OF CHAR;
    FileListPtr = POINTER TO FilenameRecord;

    FilenameRecord = RECORD
                         next: FileListPtr;
                         name: VString;
                         count: CARDINAL;
                     END (*RECORD*);

(********************************************************************************)

CONST
    keyphrase = PhraseType {"", "", "Logged in", "Finished", "Uploaded",
                            "Tried to put", "Downloaded", "Tried to get",
                            "Deleted", ""};

    NullTime = "0000-00-00 00:00:00";
    MaxTime = "9999-12-31 23:59:59";

(********************************************************************************)

VAR
    LogfileName: FilenameString;
    StartTime, EndTime: TimeString;

    Files: ARRAY [upload..delete] OF FileListPtr;

(********************************************************************************)
(*                                 MISCELLANEOUS                                *)
(********************************************************************************)

PROCEDURE SWriteCard (val: CARDINAL);

    (* Screen output of a cardinal number. *)

    BEGIN
        IF val > 9 THEN
            SWriteCard (val DIV 10);  val := val MOD 10;
        END (*IF*);
        STextIO.WriteChar (CHR(ORD('0')+val));
    END SWriteCard;

(********************************************************************************)

PROCEDURE SWriteRJCard (val, places: CARDINAL);

    (* Screen output of a cardinal number, right justified so as to take up     *)
    (* 'places' character positions.                                            *)

    BEGIN
        IF places = 0 THEN
            (* Can't do anything sensible. *)
        ELSIF val < 10 THEN
            WHILE places > 1 DO
                STextIO.WriteChar (' ');  DEC(places);
            END (*WHILE*);
            STextIO.WriteChar (CHR(ORD('0')+val MOD 10));
        ELSIF places = 1 THEN
            STextIO.WriteChar ('*');
        ELSE
            SWriteRJCard (val DIV 10, places-1);
            STextIO.WriteChar (CHR(ORD('0')+val MOD 10));
        END (*IF*);
    END SWriteRJCard;

(********************************************************************************)
(*                          OPERATIONS ON A FILE LIST                           *)
(********************************************************************************)

PROCEDURE MergeIntoFileList (N: LineType;  VName: VString);

    (* Adds "name" into Files[N], either by inserting a new list entry   *)
    (* or incrementing the count field of an existing entry.             *)

    VAR previous, current, p: FileListPtr;  comparison: INTEGER;

    BEGIN
        (* Find a suitable insertion point. *)

        previous := NIL;  current := Files[N];  comparison := +1;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            comparison := VSCompare (VName, current^.name);
            IF comparison <= 0 THEN EXIT(*LOOP*) END(*IF*);
            previous := current;  current := current^.next;
        END (*LOOP*);

        (* Now do the insertion. *)

        IF comparison = 0 THEN
            INC (current^.count);
        ELSE
            NEW (p);
            p^.count := 1;
            p^.name := VName;

            (* Insert between previous and current. *)

            IF previous = NIL THEN
                Files[N] := p;
            ELSE
                previous^.next := p;
            END (*IF*);
            p^.next := current;

        END (*IF*);

    END MergeIntoFileList;

(********************************************************************************)

PROCEDURE WriteFileList (p: FileListPtr);

    (* Puts a summary to standard output. *)

    VAR total: CARDINAL;

    BEGIN
        IF p = NIL THEN
            STextIO.WriteString ("  (None)");
            STextIO.WriteLn;
            RETURN;
        END (*IF*);
        total := 0;
        REPEAT
            INC (total, p^.count);
            SWriteRJCard (p^.count, 8);  STextIO.WriteString ("  ");
            WriteVString (p^.name);  STextIO.WriteLn;
            p := p^.next;
        UNTIL p = NIL;
        STextIO.WriteString ("A total of ");  SWriteCard (total);
        STextIO.WriteString (" file transfers.");  STextIO.WriteLn;
    END WriteFileList;

(********************************************************************************)

PROCEDURE DiscardFileList (VAR (*INOUT*) p: FileListPtr);

    (* Disposes of the linked list. *)

    VAR q: FileListPtr;

    BEGIN
        WHILE p <> NIL DO
            q := p^.next;
            DiscardVString (p^.name);
            DISPOSE (p);
            p := q;
        END (*WHILE*);
    END DiscardFileList;

(********************************************************************************)
(*                        OPERATIONS ON THE LOGFILE LINES                       *)
(********************************************************************************)

PROCEDURE Classify (VAR (*INOUT*) buffer: ARRAY OF CHAR): LineType;

    (* Takes one log line, works out what sort of record it is.  As a           *)
    (* side-effect, we also strip the header keyword from the line.             *)

    VAR pos: CARDINAL;  found: BOOLEAN;  result: LineType;
        key: ARRAY [0..15] OF CHAR;

    BEGIN
        result := other;
        IF buffer[0] = CtrlZ THEN result := eof
        ELSIF buffer[0] = Nul THEN result := blank
        ELSE
            Strings.FindNext (':', buffer, 0, found, pos);
            IF found THEN
                Strings.Extract (buffer, 0, pos, key);
                result := MIN(LineType);
                WHILE NOT Strings.Equal (key, keyphrase[result])
                                 AND (result <> other) DO
                    INC (result);
                END (*WHILE*);
                IF result <> other THEN
                    REPEAT
                        INC (pos);
                    UNTIL buffer[pos] <> ' ';
                    Strings.Delete (buffer, 0, pos);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        RETURN result;
    END Classify;

(********************************************************************************)
(*                       READING THE DATA FROM THE LOG FILE                     *)
(********************************************************************************)

PROCEDURE ReadOneLine (cid: IOChan.ChanId;  VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Input of a single line of data. *)

    BEGIN
        TextIO.ReadString (cid, buffer);
        IF IOChan.ReadResult(cid) = IOConsts.endOfInput THEN
            buffer[0] := CtrlZ;  buffer[1] := Nul;
        ELSE
            TextIO.SkipLine (cid);
        END (*IF*);
    END ReadOneLine;

(********************************************************************************)

PROCEDURE GetFileName (StripTrailing: BOOLEAN;  VAR (*IN*) buffer: LogLine;
                           VAR (*INOUT*) result: FilenameString);

    (* Picks up a file name from the buffer.  If StripTrailing is TRUE, we      *)
    (* must remove a parenthesised comment at the end of the line.              *)

    VAR pos, length: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.Assign (buffer, result);
        length := Strings.Length (result);
        IF StripTrailing AND (length >= 2) AND (result[length-1] = ')') THEN
            Strings.FindPrev ('(', result, length-2, found, pos);
            IF found THEN
                IF (pos > 0) AND (result[pos-1] = ' ') THEN
                    DEC (pos);
                END (*IF*);
                result[pos] := Nul;
            END (*IF*);
        END (*IF*);

        (* Change all '/' to '\'. *)

        REPEAT
            Strings.FindNext ('/', result, 0, found, pos);
            IF found THEN
                result[pos] := '\';
            END (*IF*);
        UNTIL NOT found;

    END GetFileName;

(********************************************************************************)

PROCEDURE ReadOneGroup (cid: IOChan.ChanId): BOOLEAN;

    (* Handles the data from one client session.  The function result is TRUE   *)
    (* for valid data, and FALSE if we've run off the end of the file.          *)

    VAR buffer: LogLine;
        AtEOF: BOOLEAN;  kind: LineType;
        NameBuffer: FilenameString;
        VName: VString;

    BEGIN
        AtEOF := FALSE;
        REPEAT
            ReadOneLine (cid, buffer);
        UNTIL buffer[0] <> Nul;
        LOOP
            kind := Classify(buffer);
            CASE kind OF
              | eof:
                    AtEOF := TRUE;
                    EXIT (*LOOP*);
              | blank:
                    EXIT (*LOOP*);
              | starttime:
                    IF Strings.Compare (buffer, StartTime) = Strings.less THEN
                        Strings.Assign (buffer, StartTime);
                    END (*IF*);
              | endtime:
                    IF Strings.Compare (buffer, EndTime) = Strings.greater THEN
                        Strings.Assign (buffer, EndTime);
                    END (*IF*);
              | upload..delete:
                    GetFileName (kind < delete, buffer, NameBuffer);
                    VName := MakeVString (NameBuffer);
                    MergeIntoFileList (kind, VName);
            ELSE
                (* No special processing needed. *)
            END (*CASE*);
            ReadOneLine (cid, buffer);
        END (*LOOP*);

        RETURN NOT AtEOF;

    END ReadOneGroup;

(********************************************************************************)

PROCEDURE ReadTheData;

    (* Returns a linked list of all client data. *)

    VAR cid: IOChan.ChanId;  res: ChanConsts.OpenResults;
        j: LineType;


    BEGIN
        StartTime := MaxTime;  EndTime := NullTime;
        FOR j := upload TO delete DO
            Files[j] := NIL;
        END (*FOR*);
        SeqFile.OpenRead (cid, LogfileName, SeqFile.read+SeqFile.text, res);
        IF res = ChanConsts.opened THEN
            WHILE ReadOneGroup (cid) DO
            END (*WHILE*);
            SeqFile.Close (cid);
        END (*IF*);
    END ReadTheData;

(********************************************************************************)
(*                      WRITING A REPORT TO STANDARD OUTPUT                     *)
(********************************************************************************)

PROCEDURE WriteHeading;

    BEGIN
        STextIO.WriteString ("FTP summary for period ");
        STextIO.WriteString (StartTime);
        STextIO.WriteString (" to ");
        STextIO.WriteString (EndTime);
        STextIO.WriteLn;
        STextIO.WriteString ("-----------------------------------------------------------------");
        STextIO.WriteLn;
    END WriteHeading;

(********************************************************************************)

PROCEDURE WriteSummary;

    TYPE GroupHead = ARRAY [upload..delete] OF ARRAY [0..31] OF CHAR;
    CONST Heading = GroupHead {"UPLOADS", "PARTIAL UPLOADS", "DOWNLOADS",
                               "PARTIAL DOWNLOADS", "DELETIONS"};

    VAR j: LineType;

    BEGIN
        WriteHeading;

        FOR j := upload TO delete DO
            STextIO.WriteLn;  STextIO.WriteString (Heading[j]);  STextIO.WriteLn;
            WriteFileList (Files[j]);
            DiscardFileList (Files[j]);
        END (*FOR*);

    END WriteSummary;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

PROCEDURE GetLogfileName;

    (* Picks up program argument from the command line. *)

    CONST DefaultLogfileName = "FTPUSERS.LOG";

    VAR j: CARDINAL;
        args: IOChan.ChanId;

    (****************************************************************************)

    PROCEDURE SkipBlanks;
        BEGIN
            WHILE LogfileName[j] = ' ' DO
                INC(j);
            END (*WHILE*);
        END SkipBlanks;

    (****************************************************************************)

    BEGIN
        args := ProgramArgs.ArgChan();
        IF ProgramArgs.IsArgPresent() THEN

            TextIO.ReadString (args, LogfileName);

            j := 0;  SkipBlanks;

            (* Remove leading and trailing rubbish from the result. *)

            IF j > 0 THEN
                Strings.Delete (LogfileName, 0, j);
            END (*IF*);
            j := LENGTH(LogfileName);
            LOOP
                IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
                DEC (j);
                IF LogfileName[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                LogfileName[j] := Nul;
            END (*LOOP*);

        END (*IF*);

        (* If no file name supplied, use the default name. *)

        IF LogfileName[0] = Nul THEN
            LogfileName := DefaultLogfileName;
        END (*IF*);

    END GetLogfileName;

(********************************************************************************)

BEGIN
    GetLogfileName;
    ReadTheData;
    WriteSummary;
END LogAnalysis.

