(**************************************************************************)
(*                                                                        *)
(*  LoadPRM utility for FtpServer                                         *)
(*  Copyright (C) 2015   Peter Moylan                                     *)
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

MODULE LoadPRM;

        (********************************************************)
        (*                                                      *)
        (*  Program to load data from PRM files into ftpd.ini   *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 March 1998                    *)
        (*  Last edited:        5 February 2015                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(*     Syntax for the directory permissions in the PRM file:            *)
(*                                                                      *)
(*     <result>     ->  <dirlist>                                       *)
(*     <dirlist>    ->  <diritem> { , <diritem> }*                      *)
(*     <diritem>    ->  <dirname> <dirrule>                             *)
(*     <dirname>    ->  <namestring>  |  <namestring> = <namestring>    *)
(*     <dirrule>   -> { <permission> }* { ( <dirlist> ) }               *)
(*     <dirlist>   ->  <diritem> { , <diritem> }*                       *)
(*     <dirlist>   -> { <diritem> }+                                    *)
(*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+ | N- | N+ *)
(*     <namestring> ->  <string1> | " <string2> " | ' <string3> '       *)
(*     <string1>   -> any string not including space char               *)
(*     <string2>   -> any string not including double quote             *)
(*     <string3>   -> any string not including single quote             *)
(*                                                                      *)
(*  Notation: {} means optional, {}* means zero or more, {}+ means      *)
(*            one or more.                                              *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT
    (* proc *)  ADR;

IMPORT IOChan, TextIO, STextIO, Strings, OS2, FileOps;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM FtpdINI IMPORT
    (* proc *)  SetINIFileName, SetHashMax, OpenINIFile, CloseINIFile,
                OpenINIForUser;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIPut, INIPutString, INIPutBinary;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    RealNameSize = 64;
    PasswordSize = 32;
    NoteFieldSize = 2048;

TYPE
    CharSet = SET OF CHAR;
    PassString = ARRAY [0..PasswordSize-1] OF CHAR;
    NoteField = ARRAY [0..NoteFieldSize-1] OF CHAR;
    FileNameString = ARRAY [0..255] OF CHAR;

    (* The following declarations relate to the data structure that we keep,    *)
    (* for a logged-in user, to show which directories and files are accessible.*)
    (* For each directory, we have a linked list of subdirectories.             *)

    Permission = (Visible, AllowRead, AllowWrite, AllowDelete, AllowRename);
    PermissionSet = SET OF Permission;

    DirEntryPtr = POINTER TO DirEntry;

    DirEntry = RECORD
                   flags: PermissionSet;
                   parent, FirstChild, next: DirEntryPtr;
                   name: FileNameString;
                   link: POINTER TO FileNameString;
               END (*RECORD*);

    UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser, NormalUser,
                    Manager, UserTemplate);

    HideListPtr = POINTER TO HideListEntry;

    HideListEntry = RECORD
                        next: HideListPtr;
                        mask: FileNameString;
                    END (*RECORD*);

    (* The fields in a UserPermission record have the following meanings.       *)
    (*      username     The user's login name                                  *)
    (*      Password     The user's password                                    *)
    (*      category     The user's category                                    *)
    (*      TreeRoot     Root of the user's permission tree.                    *)

    User = POINTER TO UserPermission;

    UserPermission = RECORD
                         username, Password, TemplateName: PassString;
                         category: UserCategory;
                         UseTemplate: BOOLEAN;
                         SingleUse: BOOLEAN;
                         UserLimitSpecified: BOOLEAN;
                         UserLimit: CARDINAL;
                         SpeedLimitSpecified: BOOLEAN;
                         SpeedLimit: CARDINAL;
                         RealName: ARRAY [0..RealNameSize-1] OF CHAR;
                         Notes: NoteField;
                         TreeRoot: DirEntryPtr;
                         HideList: HideListPtr;
                     END (*RECORD*);

(************************************************************************)

VAR
    (* Anchor block handle for this application.  *)

    hab: OS2.HAB;

(********************************************************************************)
(*                          TREE OUTPUT, FOR TESTING                            *)
(********************************************************************************)

(*
PROCEDURE Indent (level: CARDINAL);

    (* Writes some spaces. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 1 TO 3*level DO
            STextIO.WriteChar (" ");
        END (*FOR*);
    END Indent;

(********************************************************************************)

PROCEDURE DumpList (p: DirEntryPtr;  level: CARDINAL);

    (* Writes out a visual representation of a linked list of trees.  The       *)
    (* level parameter controls the indentation.                                *)

    BEGIN
        REPEAT
            WITH p^ DO
                Indent (level);  WriteString (name);
                IF link <> NIL THEN
                    WriteString (" = ");  WriteString (link^);
                END (*IF*);
                WriteString ("  ");
                IF Visible IN flags THEN STextIO.WriteChar ('V') ELSE STextIO.WriteChar (' ')  END(*IF*);
                IF AllowRead IN flags THEN STextIO.WriteChar ('R') ELSE STextIO.WriteChar (' ')  END(*IF*);
                IF AllowWrite IN flags THEN STextIO.WriteChar ('W') ELSE STextIO.WriteChar (' ')  END(*IF*);
                IF AllowDelete IN flags THEN STextIO.WriteChar ('D') ELSE STextIO.WriteChar (' ')  END(*IF*);
                IF AllowRenamee IN flags THEN STextIO.WriteChar ('N') ELSE STextIO.WriteChar (' ')  END(*IF*);
                STextIO.WriteLn;
                IF FirstChild <> NIL THEN
                    DumpList (FirstChild, level+1);
                END (*IF*);
            END (*WITH*);
            p := p^.next;
        UNTIL p = NIL;
    END DumpList;

(********************************************************************************)
(*                      DUMP TO SCREEN OF ALL USER DATA                         *)
(********************************************************************************)

PROCEDURE DumpUserData (U: User);

    (* Writes out the permissions of this user.  *)

    BEGIN
        WriteString ("Password is ");  WriteString (U^.Password);  STextIO.WriteLn;
        DumpList (U^.TreeRoot, 0);
    END DumpUserData;
*)

(********************************************************************************)
(*                    PARSER - READS USER DATA FROM PRM FILE                    *)
(********************************************************************************)

PROCEDURE ToLower (VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts all letters in string to lower case. *)

    TYPE CharSet = SET OF CHAR;

    CONST shift = ORD('a') - ORD('A');

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO LENGTH(string) DO
            IF string[j] IN CharSet {'A'..'Z'} THEN
                INC (string[j], shift);
            END (*IF*);
        END (*FOR*);
    END ToLower;

(********************************************************************************)

PROCEDURE ReadUserData (filename, username: ARRAY OF CHAR): User;

    (* Fetches the password, etc., from the specified PRM file.  Returns with   *)
    (* category = NoSuchUser and result = NIL if the user's data could not be   *)
    (* found.                                                                   *)

    CONST Space = " ";  CR = CHR(13);  LF = CHR(10);
          UsingINIdata = FALSE;

    CONST Digits = CharSet {'0'..'9'};

    VAR NextChar: CHAR;  InComment: BOOLEAN;  cid: FileOps.ChanId;

    (****************************************************************************)

    PROCEDURE Scan;

        (* Puts the next input character into variable NextChar. *)

        CONST CtrlZ = CHR(26);

        VAR charsRead: CARDINAL;

        BEGIN
            FileOps.ReadRaw (cid, NextChar, 1, charsRead);

            IF (charsRead = 0) OR (NextChar = CtrlZ) THEN
                NextChar := Nul;
            ELSIF (NextChar = CR) OR (NextChar = LF) THEN
                NextChar := Space;  InComment := FALSE;
            ELSIF NextChar = '%' THEN
                InComment := TRUE;
            END (*IF*);
            IF InComment THEN NextChar := Space
            END (*IF*);
        END Scan;

    (****************************************************************************)

    PROCEDURE Scan1;

        (* Puts the next input character into variable NextChar.  Like Scan,    *)
        (* except that carriage return, line feed, and '%' are all treated as   *)
        (* ordinary characters rather than format effectors.                    *)

        CONST CR = CHR(13);  LF = CHR(10);  CtrlZ = CHR(26);

        VAR charsRead: CARDINAL;

        BEGIN
            FileOps.ReadRaw (cid, NextChar, 1, charsRead);

            IF (charsRead = 0) OR (NextChar = CtrlZ) THEN
                NextChar := Nul;
            END (*IF*);

        END Scan1;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            WHILE NextChar = Space DO Scan END(*WHILE*);
        END SkipBlanks;

    (****************************************************************************)

    PROCEDURE LoadString (VAR (*OUT*) result: ARRAY OF CHAR;  Stoppers: CharSet);

        (* Reads a string up to but not including a character in Stoppers.      *)

        VAR j: CARDINAL;

        BEGIN
            j := 0;
            WHILE (j <= HIGH(result)) AND NOT (NextChar IN Stoppers) DO
                result[j] := NextChar;  INC(j);
                Scan;
            END (*WHILE*);
            IF j <= HIGH(result) THEN result[j] := Nul END(*IF*);
        END LoadString;

    (****************************************************************************)

    PROCEDURE LoadLine (VAR (*OUT*) line: ARRAY OF CHAR): BOOLEAN;

        (* Reads a whole line, consuming but not storing the line terminator. *)
        (* Returns FALSE on end of file.                                      *)

        CONST Stoppers = CharSet {Nul, CR, LF};

        VAR j: CARDINAL;

        BEGIN
            IF (NextChar = Nul) THEN
                RETURN FALSE;
            END (*IF*);
            j := 0;
            WHILE (j <= HIGH(line)) AND NOT (NextChar IN Stoppers) DO
                line[j] := NextChar;  INC(j);
                Scan1;
            END (*WHILE*);
            IF j <= HIGH(line) THEN line[j] := Nul END(*IF*);

            (* Consume the final CRLF, if present. *)

            IF NextChar = CR THEN
                Scan1;
                IF NextChar = LF THEN
                    Scan1;
                END (*IF*);
            END (*IF*);
            RETURN TRUE;
        END LoadLine;

    (****************************************************************************)

    PROCEDURE NameString (VAR (*OUT*) result: ARRAY OF CHAR;  Stoppers: CharSet);

        (*     <namestring> ->  <string1> | " <string2> " | ' <string3> '       *)
        (*     <string1>   -> any string not including space char               *)
        (*     <string2>   -> any string not including double quote             *)
        (*     <string3>   -> any string not including single quote             *)
        (* The strings in <string1> also may not contain characters in Stoppers.*)

        CONST SingleQuote = "'";  DoubleQuote = '"';

        VAR Delimiter: CHAR;

        BEGIN
            INCL (Stoppers, Nul);
            IF (NextChar = SingleQuote) OR (NextChar = DoubleQuote) THEN
                Delimiter := NextChar;
                Stoppers := CharSet {Nul, Delimiter};
                Scan;
                LoadString (result, Stoppers);
                IF NextChar = Delimiter THEN
                    Scan;
                ELSE
                    STextIO.WriteString ("Mismatched quotation marks.");
                    STextIO.WriteLn;
                END (*IF*);
            ELSE
                INCL (Stoppers, Space);
                LoadString (result, Stoppers);
            END (*IF*);
        END NameString;

    (****************************************************************************)

    PROCEDURE ReadNoteField (VAR (*OUT*) Notes: NoteField);

        (* Reads a character string delimited by '(*' and '*)'.  On entry the   *)
        (* initial '(' has already been seen.                                   *)

        VAR j, nested: CARDINAL;

        (************************************************************************)

        PROCEDURE Store (ch: CHAR);

            (* Stores ch at Notes[j]. *)

            BEGIN
                IF j < NoteFieldSize THEN
                    Notes[j] := ch;  INC(j);
                END (*IF*);
            END Store;

        (************************************************************************)

        BEGIN
            j := 0;  nested := 0;
            Scan1;
            IF NextChar = '*' THEN
                Scan1;
                LOOP
                    IF NextChar = Nul THEN
                        STextIO.WriteString ("End of file reached while processing Note field");
                        STextIO.WriteLn;
                        EXIT (*LOOP*);
                    ELSIF NextChar = '*' THEN
                        Scan1;
                        IF NextChar = ')' THEN
                            Scan1;
                            IF nested = 0 THEN
                                EXIT (*LOOP*);
                            END (*IF*);
                            DEC (nested);
                            Store ('*');  Store (NextChar);
                        END (*IF*);
                    ELSIF NextChar = '(' THEN
                        Scan1;
                        IF NextChar = '*' THEN
                            INC (nested);
                        END (*IF*);
                        Store ('(');  Store (NextChar);  Scan1;
                    ELSE
                        Store (NextChar);  Scan1;
                    END (*IF*);
                END (*LOOP*);
            ELSE
                STextIO.WriteString ("Note field must start with (*");
                STextIO.WriteLn;
            END (*IF*);

            (* Terminate the string. *)

            Store (Nul);

            (* Go back to normal scanning mode. *)

            IF (NextChar = CR) OR (NextChar = LF) OR (NextChar = '%') THEN
                InComment := NextChar = '%';
                NextChar := Space;
            END (*IF*);

        END ReadNoteField;

    (****************************************************************************)

    PROCEDURE DirItem (mother: DirEntryPtr): DirEntryPtr;  FORWARD;

    (****************************************************************************)

    PROCEDURE DirList (mother: DirEntryPtr): DirEntryPtr;

        (*     <dirlist>   ->  <diritem> { , <diritem> }*       *)
        (* Result returned: a linked list of directory nodes.   *)

        CONST Comma = ",";

        VAR result, lastnode: DirEntryPtr;

        BEGIN
            result := DirItem (mother);  lastnode := result;
            SkipBlanks;
            WHILE (NextChar = Comma) OR (NextChar = ';') DO
                Scan;  SkipBlanks;
                lastnode^.next := DirItem (mother);
                lastnode := lastnode^.next;
                SkipBlanks;
            END (*WHILE*);
            RETURN result;
        END DirList;

    (****************************************************************************)

    PROCEDURE DirRule (pnode: DirEntryPtr);

        (* Fills in the permissions and subdirectory info for pnode.            *)
        (*     <dirrule>   -> { <permission> }* { ( <dirlist> ) }               *)
        (*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+ | N- | N+ *)

        VAR option: Permission;

        BEGIN
            (* Default flags are inherited from parent. *)

            IF pnode^.parent = NIL THEN
                pnode^.flags := PermissionSet {Visible, AllowRead}
            ELSE pnode^.flags := pnode^.parent^.flags
            END (*IF*);

            (* Look for optional permission codes. *)

            WHILE CAP(NextChar) IN CharSet {'V', 'R', 'W', 'D', 'N'} DO
                CASE CAP(NextChar) OF
                  | 'V':  option := Visible;
                  | 'R':  option := AllowRead;
                  | 'W':  option := AllowWrite;
                  | 'D':  option := AllowDelete;
                  | 'N':  option := AllowRename;
                END (*CASE*);
                Scan;
                IF NextChar = '-' THEN
                    Scan;
                    EXCL (pnode^.flags, option);
                ELSIF NextChar = '+' THEN
                    Scan;
                    INCL (pnode^.flags, option);
                ELSE
                    STextIO.WriteString ("Option modifier must be + or -");
                    STextIO.WriteLn;
                END (*IF*);

                SkipBlanks;
            END (*WHILE*);

            (* Look for optional list of subdirectories. *)

            IF NextChar = '(' THEN
                Scan;  SkipBlanks;
                IF NextChar <> ')' THEN
                    pnode^.FirstChild := DirList(pnode);
                END (*IF*);
                IF NextChar = ')' THEN
                    Scan;
                ELSE
                    STextIO.WriteString ("Missing closing parenthesis.");
                    STextIO.WriteLn;
                END (*IF*);
            END (*IF*);

        END DirRule;

    (****************************************************************************)

    PROCEDURE DirName (mother: DirEntryPtr): DirEntryPtr;

        (*     <dirname>    ->  <namestring>  |  <namestring> = <namestring>    *)
        (* The alternative with the '=' sign means that we have to create       *)
        (* a symbolic link.                                                     *)

        VAR result: DirEntryPtr;  k: CARDINAL;

        BEGIN
            NEW (result);
            WITH result^ DO
                flags := PermissionSet {Visible, AllowRead};
                parent := mother;  FirstChild := NIL;  next := NIL;
                NameString (name, CharSet{'=', ',', '('} );
                link := NIL;
                SkipBlanks;
                IF NextChar = '=' THEN
                    Scan;  SkipBlanks;
                    NEW (link);
                    NameString (link^, CharSet{});

                    (* Transition arrangement: get rid of the trailing '/'.  *)

                    k := LENGTH (link^);
                    IF k > 0 THEN
                        DEC (k);
                        IF (link^[k] = '/') OR (link^[k] = '\') THEN
                            link^[k] := Nul;
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*WITH*);
            RETURN result;
        END DirName;

    (****************************************************************************)

    PROCEDURE DirItem (mother: DirEntryPtr): DirEntryPtr;

        (*     <diritem>   -> <dirname> <dirrule>                            *)

        VAR result: DirEntryPtr;

        BEGIN
            result := DirName(mother);
            SkipBlanks;
            DirRule (result);
            RETURN result;
        END DirItem;

    (************************************************************************)

    PROCEDURE GetCard(): CARDINAL;

        (* Reads a cardinal number from the input stream. *)

        VAR result: CARDINAL;

        BEGIN
            result := 0;
            WHILE NextChar IN Digits DO
                result := 10*result;
                INC (result, ORD(NextChar) - ORD('0'));
                Scan;
            END (*WHILE*);
            RETURN result;
        EXCEPT
            WHILE NextChar IN Digits DO
                Scan;
            END (*WHILE*);
            RETURN MAX(CARDINAL);
        END GetCard;

    (************************************************************************)

    PROCEDURE ReadHideList (U: User);

        VAR str: FileNameString;  finished: BOOLEAN;
            previous, current: HideListPtr;

        BEGIN
            LoadString (str, CharSet{'=', Nul});
            IF Strings.Equal (str, "HideList") AND (NextChar = '=') THEN
                Scan1;
                IF NextChar = '{' THEN
                    Scan1;
                END (*IF*);
                previous := NIL;
                REPEAT
                    finished := (NOT LoadLine (str)) OR (str[0] = '}');
                    IF NOT finished AND (str[0] <> Nul) THEN
                        NEW (current);  current^.next := NIL;
                        current^.mask := str;
                        IF previous = NIL THEN
                            U^.HideList := current;
                        ELSE
                            previous^.next := current;
                        END (*IF*);
                        previous := current;
                    END (*IF*);
                UNTIL finished;
            END (*IF*);
        END ReadHideList;

    (************************************************************************)
    (*                     BODY OF READUSERDATA                             *)
    (*                                                                      *)
    (*     <result>  ->  <userclass> <password> <userlimit>                 *)
    (*                       <speedlimit> <realname> <notes>                *)
    (*                                           { <volumeinfo> }*          *)
    (*     <userclass> -> G | U | N | M | T                                 *)
    (*     <password>  -> <namestring>                                      *)
    (*                                                                      *)
    (* Note: for simplicity, a password must always be specified, even if   *)
    (* it is not going to be used.                                          *)
    (*                                                                      *)
    (************************************************************************)

    VAR result: User;  root: DirEntryPtr;

    BEGIN       (* Body of ReadUserData *)

        InComment := FALSE;

        cid := FileOps.OpenOldFile (filename, FALSE, FALSE);
        IF cid = FileOps.NoSuchChannel THEN
            RETURN NIL;
        END (*IF*);

        NEW (result);  result^.TreeRoot := NIL;  result^.HideList := NIL;
        Strings.Assign (username, result^.username);
        result^.category := NoSuchUser;

        Scan;  SkipBlanks;

        (* Decode the user class code. *)

        IF NextChar = 'G' THEN
            Scan;  result^.category := GuestUser;
        ELSIF NextChar = 'U' THEN
            Scan;  result^.category := NormalUser;
        ELSIF NextChar = 'N' THEN
            Scan;  result^.category := NoPasswordNeeded;
        ELSIF NextChar = 'M' THEN
            Scan;  result^.category := Manager;
        ELSIF NextChar = 'T' THEN
            Scan;  result^.category := UserTemplate;
        ELSE
            STextIO.WriteString ("Unknown user category ");  STextIO.WriteChar (NextChar);
            STextIO.WriteLn;  Scan;
        END (*IF*);

        (* If the user class code is followed by an 'S', this is        *)
        (* a single-use account.                                        *)

        IF NextChar = 'S' THEN
            Scan;  result^.SingleUse := TRUE;
        ELSE
            result^.SingleUse := FALSE;
        END (*IF*);

        (* Read the password. *);

        SkipBlanks;  NameString (result^.Password, CharSet{});

        (* Read the user limit, if present. *)

        SkipBlanks;
        result^.UserLimitSpecified := NextChar IN Digits;
        result^.UserLimit := GetCard();

        (* If the next field is also numeric, then we expect to find    *)
        (* the speed limit, realname, and notes.                        *)

        SkipBlanks;
        result^.SpeedLimitSpecified := NextChar IN Digits;
        result^.SpeedLimit := GetCard();

        IF result^.SpeedLimitSpecified THEN
            SkipBlanks;  NameString (result^.RealName, CharSet{});
            SkipBlanks;
            IF NextChar = '(' THEN
                ReadNoteField (result^.Notes);
            ELSE
                result^.Notes := "";
            END (*IF*);
        END (*IF*);

        (* Now we're up to the volume information. *)

        SkipBlanks;
        result^.TemplateName := "";
        result^.UseTemplate := NextChar = '@';

        IF result^.UseTemplate THEN

            (* This user uses a template. *)

            result^.TreeRoot := NIL;
            Scan;  NameString (result^.TemplateName, CharSet{});

        ELSE

            (* Give the user a root directory. *)

            NEW (root);
            result^.TreeRoot := root;
            WITH root^ DO
                flags := PermissionSet {Visible, AllowRead};
                name := "";
                parent := NIL;  FirstChild := NIL;  next := NIL;
                NEW (link);  link^ := "";
            END (*WITH*);

            (* Load the volume information. *)

            root^.FirstChild := DirList (root);

            (* Remove redundant top-level pseudo-directories. *)

            WHILE (root <> NIL) AND (root^.link <> NIL) AND (root^.link^[0] = Nul)
                                      AND (root^.FirstChild <> NIL)
                                      AND (root^.FirstChild^.next = NIL) DO
                DISPOSE (root^.link);
                root := root^.FirstChild;
                DISPOSE (result^.TreeRoot);
                result^.TreeRoot := root;
                root^.parent := NIL;
            END (*WHILE*);

        END (*IF*);

        (* Read the HideList, if present. *)

        ReadHideList(result);

        (* Close the data file. *)

        FileOps.CloseFile (cid);
        RETURN result;

    END ReadUserData;

(********************************************************************************)
(*                        DISCARDING A USER RECORD                              *)
(********************************************************************************)

PROCEDURE KillList (VAR (*INOUT*) p: DirEntryPtr);

    (* Discards a directory tree.  *)

    VAR q: DirEntryPtr;

    BEGIN
        WHILE p <> NIL DO
            KillList (p^.FirstChild);
            q := p^.next;
            IF p^.link <> NIL THEN
                DISPOSE (p^.link);
            END (*IF*);
            DISPOSE (p);
            p := q;
        END (*IF*);
    END KillList;

(********************************************************************************)

PROCEDURE DestroyUserData (VAR (*INOUT*) U: User);

    (* Discards the data structure.  *)

    VAR p, next: HideListPtr;

    BEGIN
        KillList (U^.TreeRoot);
        p := U^.HideList;
        WHILE p <> NIL DO
            next := p^.next;
            DISPOSE (p);
            p := next;
        END (*WHILE*);
        DISPOSE (U);
    END DestroyUserData;

(************************************************************************)
(*         ENCODING THE VOLUME INFORMATION AS AN ASCII STRING           *)
(************************************************************************)

PROCEDURE AppendString (string: ARRAY OF CHAR;
                                       VAR (*INOUT*) result: ARRAY OF CHAR;
                                       VAR (*INOUT*) index: CARDINAL);

    (* Puts string into the result array, starting at result[index].    *)
    (* On return index has been updated to the next unused array slot.  *)

    VAR j, length: CARDINAL;

    BEGIN
        length := LENGTH(string);
        IF length > 0 THEN
            FOR j := 0 TO length-1 DO
                result[index] := string[j];  INC(index);
            END (*FOR*);
        END (*IF*);
    END AppendString;

(************************************************************************)

PROCEDURE AppendQuotedString (string: ARRAY OF CHAR;
                                       VAR (*INOUT*) result: ARRAY OF CHAR;
                                       VAR (*INOUT*) index: CARDINAL);

    (* Puts quotation marks around string, and puts it into the result  *)
    (* array, starting at result[index].  On return index has been      *)
    (* updated to the next unused array slot.                           *)

    BEGIN
        result[index] := '"';  INC(index);
        AppendString (string, result, index);
        result[index] := '"';  INC(index);
    END AppendQuotedString;

(************************************************************************)

PROCEDURE StoreTree (D: DirEntryPtr;  DefaultFlags: PermissionSet;
                                      VAR (*INOUT*) result: ARRAY OF CHAR;
                                      VAR (*INOUT*) index: CARDINAL);
                                                                 FORWARD;

(************************************************************************)

PROCEDURE SizeOfTreeData (D: DirEntryPtr;
                          DefaultFlags: PermissionSet): CARDINAL;
                                                                 FORWARD;

(************************************************************************)

PROCEDURE StorePermAndSubdir (D: DirEntryPtr;  DefaultFlags: PermissionSet;
                              VAR (*INOUT*) result: ARRAY OF CHAR;
                              VAR (*INOUT*) index: CARDINAL);

    (* Puts the flags for D, then information for all subdirectories    *)
    (* of D, into the result array, starting at result[index].          *)
    (* On return index has been updated to the next unused array slot.  *)

    TYPE CodeArray = ARRAY Permission OF CHAR;

    CONST PermissionCode = CodeArray {'V','R','W','D','N'};

    VAR child: DirEntryPtr;  perm: Permission;  difference: PermissionSet;

    BEGIN
        difference := DefaultFlags - D^.flags;
        FOR perm := MIN(Permission) TO MAX(Permission) DO
            IF perm IN difference THEN
                result[index] := PermissionCode[perm];  INC (index);
                result[index] := '-';  INC (index);
            END (*IF*);
        END (*FOR*);
        difference := D^.flags - DefaultFlags;
        FOR perm := MIN(Permission) TO MAX(Permission) DO
            IF perm IN difference THEN
                result[index] := PermissionCode[perm];  INC (index);
                result[index] := '+';  INC (index);
            END (*IF*);
        END (*FOR*);

        child := D^.FirstChild;
        IF child <> NIL THEN
            result[index] := '(';  INC(index);
            REPEAT
                StoreTree (child, D^.flags, result, index);
                child := child^.next;
                IF child <> NIL THEN
                    result[index] := ',';  INC(index);
                END (*IF*);
            UNTIL child = NIL;
            result[index] := ')';  INC(index);
        END (*IF*);

    END StorePermAndSubdir;

(************************************************************************)

PROCEDURE SizeOfPermAndSubdir (D: DirEntryPtr;
                                   DefaultFlags: PermissionSet): CARDINAL;

    (* Works out how much space to allocate for the flags for D, plus   *)
    (* information for all subdirectories of D.                         *)

    VAR size: CARDINAL;  child: DirEntryPtr;
        perm: Permission;  difference: PermissionSet;

    BEGIN
        size := 0;
        difference := DefaultFlags / D^.flags;
        FOR perm := MIN(Permission) TO MAX(Permission) DO
            IF perm IN difference THEN
                INC (size, 2);
            END (*IF*);
        END (*FOR*);

        child := D^.FirstChild;
        IF child <> NIL THEN
            INC (size);
            REPEAT
                INC (size, 1 + SizeOfTreeData (child, D^.flags));
                child := child^.next;
            UNTIL child = NIL;
        END (*IF*);

        RETURN size;

    END SizeOfPermAndSubdir;

(************************************************************************)

PROCEDURE StoreTree (D: DirEntryPtr;  DefaultFlags: PermissionSet;
                                      VAR (*INOUT*) result: ARRAY OF CHAR;
                                      VAR (*INOUT*) index: CARDINAL);

    (* Puts the data for the tree whose root is D into the result       *)
    (* array, starting at result[index].  On return index has been      *)
    (* updated to the next unused array slot.                           *)

    BEGIN
        AppendQuotedString (D^.name, result, index);
        IF D^.link <> NIL THEN
            result[index] := '=';  INC(index);
            AppendQuotedString (D^.link^, result, index);
        END (*IF*);
        StorePermAndSubdir (D, DefaultFlags, result, index);
    END StoreTree;

(************************************************************************)

PROCEDURE SizeOfTreeData (D: DirEntryPtr;
                          DefaultFlags: PermissionSet): CARDINAL;

    (* Works out how much space to allocate to hold all the data for    *)
    (* the tree whose root is D.                                        *)

    VAR result: CARDINAL;

    BEGIN
        result := LENGTH(D^.name) + 2 + SizeOfPermAndSubdir (D, DefaultFlags);
        IF D^.link <> NIL THEN
            INC (result, LENGTH(D^.link^) + 3);
        END (*IF*);
        RETURN result;
    END SizeOfTreeData;

(************************************************************************)
(*                          OUTPUT TO INI FILE                          *)
(************************************************************************)

PROCEDURE WriteHideList (hini: HINI;  U: User);

    TYPE Big = [0..65535];

    VAR bufptr: POINTER TO ARRAY Big OF CHAR;
        j: CARDINAL;

    (********************************************************************)

    PROCEDURE Put (value: ARRAY OF CHAR);

        (* Stores a Nul-terminated string into bufptr^[j], updates j. *)

        VAR k: CARDINAL;  ch: CHAR;

        BEGIN
            k := 0;  ch := value[0];
            WHILE ch <> Nul DO
                bufptr^[j] := ch;  INC(j);
                INC (k);  ch := value[k];
            END (*WHILE*);
            bufptr^[j] := Nul;  INC(j);
        END Put;

    (********************************************************************)

    VAR current: HideListPtr;
        size: CARDINAL;

    BEGIN
        current := U^.HideList;
        size := 0;  j := 0;
        WHILE current <> NIL DO
            INC (size, Strings.Length(current^.mask) + 1);
            current := current^.next;
        END (*WHILE*);
        IF size = 0 THEN bufptr := NIL
        ELSE
            INC (size);
            ALLOCATE (bufptr, size);
        END (*IF*);
        current := U^.HideList;
        WHILE current <> NIL DO
            Put (current^.mask);
            current := current^.next;
        END (*WHILE*);
        IF bufptr <> NIL THEN
            bufptr^[j] := Nul;
            INIPutBinary (hini, U^.username, "HideList", bufptr^, size);
            DEALLOCATE (bufptr, size);
        END (*IF*);
    END WriteHideList;

(************************************************************************)

PROCEDURE WriteUserData (U: User): BOOLEAN;

    TYPE Big = [0..65535];

    VAR hini: HINI;  size, index: CARDINAL;
        bufptr: POINTER TO ARRAY Big OF CHAR;

    BEGIN
        hini := OpenINIForUser (U^.username, TRUE);
        IF NOT INIValid(hini) THEN
            RETURN FALSE;
        END (*IF*);
        INIPut (hini, U^.username, "Category", U^.category);
        INIPutString (hini, U^.username, "Password", U^.Password);
        index := ORD (U^.SingleUse);
        INIPut (hini, U^.username, "LoginLimit", index);
        INIPut (hini, U^.username, "UseTemplate", U^.UseTemplate);
        IF U^.UserLimitSpecified THEN
            INIPut (hini, U^.username, "UserLimit", U^.UserLimit);
        END (*IF*);
        IF U^.SpeedLimitSpecified THEN
            INIPut (hini, U^.username, "SpeedLimit", U^.SpeedLimit);
            INIPutString (hini, U^.username, "RealName", U^.RealName);
            INIPutString (hini, U^.username, "Notes", U^.Notes);
        END (*IF*);

        IF U^.UseTemplate THEN
            INIPut (hini, U^.username, "TemplateName", U^.TemplateName);
            size := 0;
        ELSE
            size := SizeOfTreeData (U^.TreeRoot,
                                PermissionSet {Visible, AllowRead});
        END (*IF*);

        IF size = 0 THEN
            bufptr := NIL;
        ELSE
            ALLOCATE (bufptr, size);

            (* Store all volume information in the bufptr^ array. *)

            index := 0;
            StoreTree (U^.TreeRoot, PermissionSet {Visible, AllowRead},
                                                 bufptr^, index);
        END (*IF*);

        (* Copy the result from bufptr^ to the INI file. *)

        INIPutBinary (hini, U^.username, "Volume", bufptr^, size);
        IF size > 0 THEN
            DEALLOCATE (bufptr, size);
        END (*IF*);

        (* Write out the HideList. *)

        WriteHideList (hini, U);
        CloseINIFile (hini);

        RETURN TRUE;

    END WriteUserData;

(************************************************************************)
(*                      MAIN CONVERSION PROCEDURES                      *)
(************************************************************************)

PROCEDURE ConvertOneUser (filename, username: ARRAY OF CHAR);

    (* Converts one PRM file to an INI file entry. *)

    VAR U: User;

    BEGIN
        U := ReadUserData (filename, username);
        IF U <> NIL THEN
            IF WriteUserData (U) THEN
                STextIO.WriteString ("Done");
            ELSE
                STextIO.WriteString ("Failed");
            END (*IF*);
            STextIO.WriteLn;
            DestroyUserData (U);
        ELSE
            STextIO.WriteString ("Cannot open data file");
            STextIO.WriteLn;
        END (*IF*);
    END ConvertOneUser;

(********************************************************************************)

PROCEDURE GetParameter (VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Picks up program argument from the command line.  The function   *)
    (* result is TRUE iff a -t parameter is also present.               *)

    CONST Testing = FALSE;

    VAR args: IOChan.ChanId;  k: CARDINAL;
        UseTNI: BOOLEAN;

    BEGIN
        IF Testing THEN
            Strings.Assign ("-t test/t2/*", result);
        ELSE
            args := ArgChan();
            IF IsArgPresent() THEN
                TextIO.ReadString (args, result);
            ELSE
                result[0] := Nul;
            END (*IF*);
        END (*IF*);

        (* Check for -t option. *)

        UseTNI := FALSE;
        k := 0;
        WHILE result[k] = ' ' DO INC (k) END (*WHILE*);
        IF (result[k] = '-') AND (CAP(result[k+1]) = 'T') THEN
            UseTNI := TRUE;
            INC (k, 2);
            WHILE result[k] = ' ' DO INC (k) END (*WHILE*);
            Strings.Delete (result, 0, k);
        END (*IF*);

        (* Strip trailing spaces. *)

        k := LENGTH (result);
        WHILE (k > 0) AND (result[k-1] = ' ') DO
            DEC (k);
        END (*WHILE*);
        result[k] := CHR(0);

        RETURN UseTNI;

    END GetParameter;

(************************************************************************)

PROCEDURE PerformTheConversions;

    (* Reads command-line argument, converts all the PRM files that match. *)

    VAR mask, filename, dirname, username: ARRAY [0..511] OF CHAR;
        D: FileOps.DirectoryEntry;
        pos, pos2, HashMax: CARDINAL;  found, found2, UseTNI: BOOLEAN;
        hini: HINI;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        UseTNI := GetParameter (mask);
        SetINIFileName ("ftpd.ini", UseTNI);
        hini := OpenINIFile();
        app := "$SYS";
        HashMax := 0;
        IF INIValid(hini) THEN
            username := "$SYS";
            found := INIGet (hini, username, "HashMax", HashMax);
        END (*IF*);
        CloseINIFile (hini);
        SetHashMax (HashMax);

        (* Extract the directory name, if present. *)

        ToLower (mask);
        Strings.FindPrev ('\', mask, Strings.Length(mask), found, pos);
        Strings.FindPrev ('/', mask, Strings.Length(mask), found2, pos2);
        IF found2 THEN
            IF NOT(found) OR (pos2 > pos) THEN
                pos := pos2;  found := TRUE;
            END (*IF*);
        END (*IF*);
        Strings.FindPrev (':', mask, Strings.Length(mask), found2, pos2);
        IF found2 THEN
            IF NOT(found) OR (pos2 > pos) THEN
                pos := pos2;  found := TRUE;
            END (*IF*);
        END (*IF*);

        (* We want mask to be the full file specification, including    *)
        (* directory and wildcards if present, and dirname to be        *)
        (* just the directory part of mask.                             *)

        IF found THEN
            Strings.Assign (mask, dirname);
            IF NOT found2 THEN
                dirname[pos] := '\';
            END (*IF*);
            INC(pos);
            dirname[pos] := Nul;
        ELSE
            dirname[0] := Nul;
        END (*IF*);

        (* Append a ".prm" extension if it is missing. *)

        Strings.FindPrev ('.prm', mask, Strings.Length(mask), found, pos);
        IF NOT found THEN
            Strings.Append (".prm", mask);
        END (*IF*);

        (* We have a loop below in case of wildcards in the mask. *)

        IF FileOps.FirstDirEntry (mask, FALSE, FALSE, D) THEN
            REPEAT
                Strings.Assign (dirname, filename);
                Strings.Append (D.name, filename);
                Strings.Assign (D.name, username);
                ToLower (username);
                Strings.FindPrev (".prm", username, LENGTH(username),
                                                           found, pos);
                IF found THEN
                    username[pos] := Nul;
                END (*IF*);
                STextIO.WriteString ("Converting file ");
                STextIO.WriteString (filename);
                STextIO.WriteString (", user ");
                STextIO.WriteString (username);
                STextIO.WriteLn;
                ConvertOneUser (filename, username);
            UNTIL NOT FileOps.NextDirEntry (D);
        ELSE
            STextIO.WriteString ("Nothing to convert");
            STextIO.WriteLn;
        END (*IF*);
        FileOps.DirSearchDone (D);

    END PerformTheConversions;

(********************************************************************************)
(*                               MAIN PROGRAM                                   *)
(********************************************************************************)

BEGIN
    hab := OS2.WinInitialize (0);
    PerformTheConversions;
    IF hab <> OS2.NULLHANDLE THEN
        OS2.WinTerminate (hab);
    END (*IF*);
END LoadPRM.

