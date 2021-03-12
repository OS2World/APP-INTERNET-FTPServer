(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE FDUsers;

        (********************************************************)
        (*                                                      *)
        (*  FTP server: module to deal with user access rights  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            30 August 1997                  *)
        (*  Last edited:        26 November 2020                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(*     Syntax for the directory permissions in the INI file:            *)
(*                                                                      *)
(*     <result>     ->  <dirlist>                                       *)
(*     <dirlist>    ->  <diritem> { , <diritem> }*                      *)
(*     <diritem>    ->  <dirname> <dirrule>                             *)
(*     <dirname>    ->  <namestring>  |  <namestring> = <namestring>    *)
(*     <dirrule>    -> { <permission> }* { ( <dirlist> ) }              *)
(*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+ | N+ | N- *)
(*     <namestring> ->  <string1> | " <string2> " | ' <string3> '       *)
(*     <string1>    -> any string not including space char              *)
(*     <string2>    -> any string not including double quote            *)
(*     <string3>    -> any string not including single quote            *)
(*                                                                      *)
(*  Notation: {} means optional, {}* means zero or more, {}+ means      *)
(*            one or more.                                              *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT ADR, CARD16;

IMPORT FileSys, Strings, SysClock, OS2;

FROM Arith64 IMPORT
    (* const*)  Max64,
    (* type *)  CARD64;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FileAttr, FileAttribute, DirectoryEntry,
    (* proc *)  FileExists, FirstDirEntry, NextDirEntry, DirSearchDone,
                FreeSpace, OpenOldFile, OpenNewFile, CloseFile,
                SetPosition, EndPosition;

FROM Volume IMPORT
    (* proc *)  CreateDir, RmvDir;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FtpdINI IMPORT
    (* proc *)  OpenINIForUser, CloseINIFile;

FROM INIData IMPORT
    (* type *)  HINI, StringReadState,
    (* proc *)  INIValid, ItemSize, INIPut, INIGet, INIGetTrusted, INIGetString,
                INIDeleteApp, INIDeleteKey, GetStringList, NextString,
                CloseStringList;

FROM MyClock IMPORT
    (* proc *)  CorrectToGMT, CurrentTimeToString;

FROM MiscFuncs IMPORT
    (* proc *)  AppendString, ConvertCardRJ,
                ConvertCard64RJ, ConvertCardZ, AddEOL;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

FROM CodePage IMPORT
    (* proc *)  TranslateToUTF8;

FROM Sockets IMPORT
    (* type *)  Socket,
    (* proc *)  send;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM Names IMPORT
    (* type *)  PassString, HostName;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    CharSet = SET OF CHAR;
    FileNameString = ARRAY [0..255] OF CHAR;
    UserName = FileNameString;

    (* The following declarations relate to the data structure that we keep,    *)
    (* for a logged-in user, to show which directories and files are accessible.*)
    (* For each directory, we have a linked list of subdirectories.             *)

    Permission = (Visible, AllowRead, AllowWrite, AllowDelete, AllowRename);
    PermissionSet = SET OF Permission;

    (* A HideList is a linear list of filename masks, to indicate filenames     *)
    (* that should always be invisible regardless of what directory they're in. *)

    HideListPtr = POINTER TO HideListRecord;
    HideListRecord =
               RECORD
                   next: HideListPtr;
                   mask: FileNameString;
               END (*RECORD*);

    LinkData = RECORD
                   parent, this: FileNameString;
               END (*RECORD*);

    DirEntryPtr = POINTER TO DirEntry;

    (* A DirEntry describes one directory.  If IsADir is FALSE then it          *)
    (* describes a file rather than a directory.                                *)
    (*      flags       The user's permissions for this directory               *)
    (*      parent      Parent directory                                        *)
    (*      FirstChild  First subdirectory of this directory                    *)
    (*      next        Next subdirectory of the parent                         *)
    (*      BeenHereBefore   Initially FALSE, but set to TRUE the first time    *)
    (*                       the user reaches this directory                    *)
    (*      name        Name of this directory or file (not the full path)      *)
    (*      exists      TRUE if this is either a pseudo-directory or a file     *)
    (*                    or directory that exists in our file system           *)
    (*      IsADir      TRUE if this is a directory, not a non-directory file   *)
    (*      categorised TRUE if the values of the "exists" and "IsADir" field   *)
    (*                    are valid.                                            *)
    (*      virtual     TRUE iff this is a symbolic link                        *)
    (*      link        Pointer to full path name for the parent directory and  *)
    (*                    the current directory, in the case of a link.         *)
    (* NOTE: For a symbolic link, the parent link always ends with a '/', but   *)
    (* the "this" link never ends with a '/'.                                   *)

    DirEntry = RECORD
                   flags: PermissionSet;
                   parent, FirstChild, next: DirEntryPtr;
                   BeenHereBefore: BOOLEAN;
                   name: FileNameString;
                   exists, IsADir, categorised, virtual: BOOLEAN;
                   link: POINTER TO LinkData;
               END (*RECORD*);

    (* An FName structure is our internal representation of a file name.        *)
    (*     dir          Directory string, a complete path name.  Always ends    *)
    (*                    with a '/', unless it's an empty string.              *)
    (*     vdir         like dir, except that it's the path name as seen by     *)
    (*                    the client, which can be different from the path      *)
    (*                    name as seen by the file system.  The component       *)
    (*                    separator is always a '/', never a '\'.               *)
    (*     fname        file name within the directory.  This is an empty       *)
    (*                    string if the file in question is a directory.        *)
    (*     EntryPtr     points to the DirEntry record for the "dir" field, or   *)
    (*                    NIL if we've detected an invalid file name.           *)

    FName = POINTER TO FNameRecord;
    FNameRecord = RECORD
                      dir, vdir, fname: FileNameString;
                      EntryPtr: DirEntryPtr;
                  END (*RECORD*);

    (* The fields in a UserPermission record have the following meanings.       *)
    (*      Name         The user's login name, which can be a plain username   *)
    (*                       or possibly a name of the form user@domain         *)
    (*      Password     The user's password                                    *)
    (*      EncryptPassword   TRUE iff the password is stored in encrypted form *)
    (*      UserNumber   A serial number to use in welcome messages.            *)
    (*      UserLimit    Maximum number of instances of this user.  Storing     *)
    (*                   a copy of this for each instance might seem to be      *)
    (*                   redundant; but this allows us to cover the possibility *)
    (*                   that the limit has changed between instances.          *)
    (*      CurrentDir   Current directory, an absolute path.  This always      *)
    (*                      ends with a '/', except in the special case where   *)
    (*                      it's a null string.                                 *)
    (*      CurrentVDir  Same as CurrentDir, except that this is the full       *)
    (*                      pathname as seen by the client, not the pathname    *)
    (*                      as seen by the file system.                         *)
    (*      TreeRoot     Root of the permissions tree for this user.            *)
    (*      PosInTree    Pointer to current position within tree                *)
    (*      HideList     A list of file masks, for files that should be         *)
    (*                      invisible no matter where they are in the tree.     *)
    (*                                                                          *)
    (* The user data in the INI file can also include a LoginLimit field which, *)
    (* if nonzero, specifies how many logins will be allowed before this user   *)
    (* account is destroyed or deactivated.  We don't have to include that in   *)
    (* the permission record because we deal with it immediately on reading     *)
    (* the INI data.                                                            *)

    User = POINTER TO UserPermission;

    UserPermission = RECORD
                         Name: UserName;
                         Password: PassString;
                         TreeRoot, PosInTree: DirEntryPtr;
                         HideList: HideListPtr;
                         CurrentDir: FileNameString;
                         CurrentVDir: FileNameString;
                         UserNumber, UserLimit: CARDINAL;
                         EncryptPassword: BOOLEAN;
                     END (*RECORD*);

    (* A structure for maintaining a linked list of user counts. *)

    CountList = POINTER TO CountListRecord;
    CountListRecord = RECORD
                          next: CountList;
                          count: CARDINAL;
                          name: UserName;
                      END (*RECORD*);

(********************************************************************************)

VAR
    (* A linked list showing, for each username, how many users are currently   *)
    (* logged in under that name.                                               *)

    UserCount: CountList;

    (* Critical section protection for this list. *)

    UserCountLock: Lock;

(********************************************************************************)
(*                         CHECK FOR TOO MANY USERS                             *)
(********************************************************************************)

PROCEDURE AllocateUserNumber (username: UserName;  limit: CARDINAL): CARDINAL;

    (* Works out which instance we are of this username.  If the result goes    *)
    (* over the limit, the returned result is 0.                                *)

    VAR current, newelement: CountList;
        result: CARDINAL;

    BEGIN
        Obtain (UserCountLock);

        (* Find the list entry for this username. *)

        current := UserCount;
        WHILE (current <> NIL) AND NOT Strings.Equal(current^.name, username) DO
            current := current^.next;
        END (*WHILE*);

        (* If this username is not already in the list, add it. *)

        IF current = NIL THEN
            NEW (newelement);
            WITH newelement^ DO
                next := UserCount;
                count := 0;
                name := username;
            END (*WITH*);
            UserCount := newelement;
            current := newelement;
        END (*IF*);

        (* Increment the count, unless it's already at the limit. *)

        IF current^.count < limit THEN
            INC (current^.count);
            result := current^.count;
        ELSE
            result := 0;
        END (*IF*);

        Release (UserCountLock);
        RETURN result;

    END AllocateUserNumber;

(********************************************************************************)
(*                               THE HIDE LIST                                  *)
(********************************************************************************)

PROCEDURE DiscardHideList (U: User);

    (* Disposes of this user's HideList. *)

    VAR p, next: HideListPtr;

    BEGIN
        p := U^.HideList;
        WHILE p <> NIL DO
            next := p^.next;
            DISPOSE (p);
            p := next;
        END (*WHILE*);
    END DiscardHideList;

(********************************************************************************)
(*                          CATEGORISING A TREE NODE                            *)
(********************************************************************************)

PROCEDURE CategoriseNode (p: DirEntryPtr;  parentpath: ARRAY OF CHAR);

    (* Checks whether p^ describes a file, a directory, or something that       *)
    (* doesn't exist on the disk.  (A pseudo-directory is considered to be a    *)
    (* directory that exists.)  The parentpath argument, which should end with  *)
    (* a '/' unless it is empty, is the physical path of this node's parent     *)
    (* directory.  On exit values have been assigned to p^.exists and p^.IsADir.*)

    VAR fullname: FileNameString;
        InfoBuf: OS2.FSALLOCATE;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        IF p^.virtual THEN
            fullname := p^.link^.this;
        ELSE
            Strings.Assign (parentpath, fullname);
            Strings.Append (p^.name, fullname);
        END (*IF*);
        IF fullname[0] = Nul THEN

            (* This is a pseudo-directory. *)

            p^.exists := TRUE;  p^.IsADir := TRUE;

        ELSIF (fullname[2] = Nul) AND (fullname[1] = ':') THEN

            (* Root of a filesystem volume. *)

            p^.exists := OS2.DosQueryFSInfo (ORD(CAP(fullname[0])) - ORD('A') + 1,
                           1, ADR(InfoBuf), SIZE(InfoBuf)) = 0;
            p^.IsADir := p^.exists;

        ELSE
            (* We have a real name, but we still have to check three    *)
            (* possibilities: this is a file, this is a directory, this *)
            (* is something that doesn't exist on our disk.             *)

            p^.exists := FileExists (fullname, p^.IsADir);

        END (*IF*);
        p^.categorised := TRUE;

    END CategoriseNode;

(********************************************************************************)
(*                                PARSER                                        *)
(********************************************************************************)

PROCEDURE LoadUserData (name: ARRAY OF CHAR;
                          VAR (*OUT*) SpeedLimit: CARDINAL;
                          VAR (*OUT*) UpSpeedLimit: CARDINAL;
                          VAR (*OUT*) UpSizeLimit: CARD64;
                          VAR (*OUT*) category: UserCategory;
                          VAR (*OUT*) LogSession: BOOLEAN): User;

    (* Fetches the password, etc., for the user whose username is specified     *)
    (* as the argument.  Returns with category = NoSuchUser if the user's data  *)
    (* could not be found or if this is a user category that does not permit    *)
    (* logging in.  The result is NIL in this case, and also in the             *)
    (* case of an overflow user.                                                *)

    CONST Space = " ";

    VAR NextChar: CHAR;
        hini: HINI;
        bufptr: POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;
        BufferPosition, BufferSize: CARDINAL;

    (****************************************************************************)

    PROCEDURE Scan;

        (* Puts the next input character into variable NextChar. *)

        BEGIN
            IF BufferPosition >= BufferSize THEN
                NextChar := Nul;
            ELSE
                NextChar := bufptr^[BufferPosition];
                INC (BufferPosition);
            END (*IF*);
        END Scan;

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

    PROCEDURE GetNameString (VAR (*OUT*) result: ARRAY OF CHAR;  Stoppers: CharSet);

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
                END (*IF*);
            ELSE
                INCL (Stoppers, Space);
                LoadString (result, Stoppers);
            END (*IF*);
        END GetNameString;

    (****************************************************************************)

    PROCEDURE DirItem (mother: DirEntryPtr;
                       MotherPath: ARRAY OF CHAR): DirEntryPtr;  FORWARD;

    (****************************************************************************)

    PROCEDURE DirList (mother: DirEntryPtr;
                            MotherPath: ARRAY OF CHAR): DirEntryPtr;

        (*     <dirlist>   ->  <diritem> { , <diritem> }*       *)
        (* Result returned: a linked list of directory nodes.   *)

        CONST Comma = ",";

        VAR result, lastnode: DirEntryPtr;

        BEGIN
            result := DirItem (mother, MotherPath);  lastnode := result;
            WHILE (NextChar = Comma) OR (NextChar = ';') DO
                Scan;
                lastnode^.next := DirItem (mother, MotherPath);
                lastnode := lastnode^.next;
            END (*WHILE*);
            RETURN result;
        END DirList;

    (****************************************************************************)

    PROCEDURE DirRule (pnode: DirEntryPtr;  OurPath: ARRAY OF CHAR);

        (* Fills in the permissions and subdirectory info for pnode.            *)
        (*     <dirrule>   -> { <permission> }* { ( <dirlist> ) }               *)
        (*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+ | N- | N+ *)

        VAR option: Permission;

        BEGIN
            (* Default flags are inherited from parent. *)

            IF pnode^.parent = NIL THEN
                pnode^.flags := PermissionSet {Visible, AllowRead}
            ELSE
                pnode^.flags := pnode^.parent^.flags
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
                END (*IF*);

            END (*WHILE*);

            (* Look for optional list of subdirectories. *)

            IF NextChar = '(' THEN
                Scan;
                IF NextChar <> ')' THEN
                    pnode^.FirstChild := DirList(pnode, OurPath);
                END (*IF*);
                IF NextChar = ')' THEN
                    Scan;
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
                BeenHereBefore := FALSE;
                GetNameString (name, CharSet{});
                virtual := FALSE;  categorised := FALSE;
                link := NIL;
                IF NextChar = '=' THEN
                    Scan;
                    virtual := TRUE;
                    NEW (link);
                    link^.parent := "";      (* fill this in later *)
                    GetNameString (link^.this, CharSet{});

                    (* Transition arrangement: get rid of the trailing '/'.  *)

                    k := LENGTH (link^.this);
                    IF k > 0 THEN
                        DEC (k);
                        IF (link^.this[k] = '/') OR (link^.this[k] = '\') THEN
                            link^.this[k] := Nul;
                        END (*IF*);
                    END (*IF*);

                ELSIF name[0] = Nul THEN

                    (* Assume bare "" means ""="". *)

                    virtual := TRUE;
                    NEW (link);
                    link^.parent := "";  link^.this := "";

                END (*IF*);
            END (*WITH*);
            RETURN result;
        END DirName;

    (****************************************************************************)

    PROCEDURE DirItem (mother: DirEntryPtr;
                       MotherPath: ARRAY OF CHAR): DirEntryPtr;

        (*     <diritem>    ->  <dirname> <dirrule>                             *)

        VAR result: DirEntryPtr;  OurPath: FileNameString;

        BEGIN
            result := DirName(mother);
            IF result^.virtual THEN
                Strings.Assign (MotherPath, result^.link^.parent);
                OurPath := result^.link^.this;
            ELSE
                Strings.Assign (MotherPath, OurPath);
                Strings.Append (result^.name, OurPath);
            END (*IF*);
            IF OurPath[0] <> Nul THEN
                Strings.Append ('/', OurPath);
            END (*IF*);
            DirRule (result, OurPath);
            RETURN result;
        END DirItem;

    (****************************************************************************)

    VAR Active: BOOLEAN;

    (****************************************************************************)

    PROCEDURE IsActiveUser (hini: HINI;  username: ARRAY OF CHAR): BOOLEAN;

        (* Returns TRUE iff username is in INI file with "Active" not FALSE. *)
        (* It's acceptable for Active to be absent from INI file.            *)

        VAR size: CARDINAL;  result: BOOLEAN;

        BEGIN
            IF (username[0] = Nul) OR (username[0] = '$') OR NOT INIValid(hini) THEN
                result := FALSE;
            ELSE
                result := ItemSize (hini, username, '', size)
                                       AND (size <> 0);
                IF result AND INIGet (hini, username, "Active", Active) THEN
                    result := Active;
                END (*IF*);
            END (*IF*);
            RETURN result;
        END IsActiveUser;

    (****************************************************************************)

    PROCEDURE LoadHideList (hini: HINI;  U: User);

        (* Loads the HideList for this user. *)

        VAR state: StringReadState;
            previous, p: HideListPtr;
            key: ARRAY [0..8] OF CHAR;

        BEGIN
            key := "HideList";
            GetStringList (hini, U^.Name, key, state);
            p := NIL;
            REPEAT
                previous := p;
                NEW (p);
                IF previous = NIL THEN U^.HideList := p;
                ELSE previous^.next := p;
                END (*IF*);
                NextString (state, p^.mask);
            UNTIL p^.mask[0] = Nul;
            CloseStringList (state);

            (* Correct for the overshoot. *)

            IF previous = NIL THEN U^.HideList := NIL;
            ELSE previous^.next := NIL;
            END (*IF*);
            DISPOSE (p);

        END LoadHideList;

    (************************************************************************)
    (*                     BODY OF LOADUSERDATA                             *)
    (*                                                                      *)
    (*     <result>  ->  <userclass> <password> { <volumeinfo> }*           *)
    (*     <userclass> -> G | U | N | M | T                                 *)
    (*     <password>  -> <namestring>                                      *)
    (*                                                                      *)
    (* Note: for simplicity, a password must always be specified, even if   *)
    (* it is not going to be used.                                          *)
    (*                                                                      *)
    (************************************************************************)

    VAR result: User;  root: DirEntryPtr;
        UseTemplate, SuppressLog: BOOLEAN;
        App: UserName;   key: ARRAY [0..6] OF CHAR;

    BEGIN       (* Body of LoadUserData *)

        SuppressLog := FALSE;
        NEW (result);
        Strings.Assign (name, result^.Name);
        result^.TreeRoot := NIL;  result^.PosInTree := NIL;  result^.HideList := NIL;

        (* Read the user category and password from the INI file. *)

        hini := OpenINIForUser(name, FALSE);
        IF IsActiveUser (hini, result^.Name)
                AND INIGet (hini, result^.Name, "Category", category)
                AND (category <= Manager) THEN

            (* Password. *)

            IF category = GuestUser THEN
                result^.Password := '@';
            ELSE
                EVAL (INIGetString (hini, result^.Name, "Password", result^.Password));
            END (*IF*);
            IF (category < NormalUser) OR NOT INIGet (hini, result^.Name, "EncryptPassword", result^.EncryptPassword) THEN
                result^.EncryptPassword := FALSE;
            END (*IF*);

            (* Suppress logging for this user? *)

            IF NOT INIGet (hini, result^.Name, "SuppressLog", SuppressLog) THEN
                SuppressLog := FALSE;
            END (*IF*);

            (* Hide list. *)

            LoadHideList (hini, result);

            (* Limits. *)

            IF NOT INIGet (hini, result^.Name, "UserLimit", result^.UserLimit) THEN
                result^.UserLimit := MAX(CARDINAL);
            END (*IF*);
            IF NOT INIGet (hini, result^.Name, "SpeedLimit", SpeedLimit) THEN
                SpeedLimit := MAX(CARDINAL);
            ELSIF SpeedLimit = 0 THEN
                SpeedLimit := 1;
            END (*IF*);
            IF NOT INIGet (hini, result^.Name, "UpSpeedLimit", UpSpeedLimit) THEN
                UpSpeedLimit := SpeedLimit;
            ELSIF UpSpeedLimit = 0 THEN
                UpSpeedLimit := 1;
            END (*IF*);
            IF NOT INIGet (hini, result^.Name, "UpSizeLimit", UpSizeLimit) THEN
                UpSizeLimit := Max64;
            END (*IF*);

            (* Should we read volume information from a template? *)

            IF NOT INIGet (hini, result^.Name, "UseTemplate", UseTemplate)
                     OR NOT UseTemplate
                     OR NOT INIGetString (hini, result^.Name, "TemplateName", App)
                     OR (App[0] = Nul) THEN
                Strings.Assign (result^.Name, App);
            END (*IF*);

            (* Check for too many users. *)

            result^.UserNumber := AllocateUserNumber (result^.Name, result^.UserLimit);
            IF result^.UserNumber = 0 THEN
                DiscardHideList (result);
                DISPOSE (result);
                category := OverflowUser;
            END (*IF*);

        ELSE

            DiscardHideList (result);
            DISPOSE (result);
            category := NoSuchUser;

        END (*IF*);

        (* If the volume information exists, allocate a buffer for it. *)

        IF (result <> NIL)
                AND ItemSize (hini, App, "Volume", BufferSize)
                AND (BufferSize <> 0) THEN
            ALLOCATE (bufptr, BufferSize);
            key := "Volume";
            EVAL (INIGetTrusted (hini, App, key, bufptr^, BufferSize));
        ELSE
            bufptr := NIL;
            BufferSize := 0;
        END (*IF*);
        BufferPosition := 0;
        Scan;

        IF result <> NIL THEN
            (* Give the user a root directory. *)

            NEW (root);
            result^.TreeRoot := root;
            WITH root^ DO
                flags := PermissionSet {Visible, AllowRead};
                name := "";
                parent := NIL;  FirstChild := NIL;  next := NIL;
                BeenHereBefore := FALSE;
                exists := TRUE;  IsADir := TRUE;
                virtual := TRUE;  categorised := TRUE;
                NEW (link);
                link^.parent := "";
                link^.this := "";
            END (*WITH*);

            (* Load the volume information. *)

            root^.FirstChild := DirList (root, "");
            result^.CurrentVDir := '/';

            (* Discard the INI file raw data. *)

            IF bufptr <> NIL THEN
                DEALLOCATE (bufptr, BufferSize);
            END (*IF*);

            (* Decide whether we really need a virtual directory as     *)
            (* the root directory.                                      *)

            IF root^.FirstChild <> NIL THEN
                IF root^.FirstChild^.next <> NIL THEN
                    result^.CurrentDir := "";
                ELSE
                    DISPOSE (root^.link);
                    root := root^.FirstChild;
                    DISPOSE (result^.TreeRoot);
                    result^.TreeRoot := root;
                    root^.parent := NIL;
                    IF root^.virtual THEN
                        result^.CurrentDir := root^.link^.this;
                        IF result^.CurrentDir[0] <> Nul THEN
                            Strings.Append ('/', result^.CurrentDir);
                        END (*IF*);
                    ELSE
                        result^.CurrentDir := "";
                    END (*IF*);
                    CategoriseNode (root, "");
                END (*IF*);
            END (*IF*);
            result^.PosInTree := root;

        END (*IF*);

        CloseINIFile (hini);
        LogSession := NOT SuppressLog;

        RETURN result;

    END LoadUserData;

(********************************************************************************)

PROCEDURE ReadUserData (name: ARRAY OF CHAR;
                          VAR (*IN*)  host: ARRAY OF CHAR;
                          VAR (*OUT*) SpeedLimit: CARDINAL;
                          VAR (*OUT*) UpSpeedLimit: CARDINAL;
                          VAR (*OUT*) UpSizeLimit: CARD64;
                          VAR (*OUT*) category: UserCategory;
                          VAR (*OUT*) LogSession: BOOLEAN): User;

    (* Fetches the password, etc., for the user whose username is specified     *)
    (* as the argument.  Returns with category = NoSuchUser if the user's data  *)
    (* could not be found or if this is a user category that does not permit    *)
    (* logging in.  The result is NIL in this case, and also in the             *)
    (* case of an overflow user.                                                *)

    VAR result: User;

    (****************************************************************************)

    PROCEDURE Try (thisname: ARRAY OF CHAR);

        BEGIN
            result := LoadUserData (thisname, SpeedLimit, UpSpeedLimit,
                                         UpSizeLimit, category, LogSession);
        END Try;

    (****************************************************************************)

    VAR username, guess: UserName;
        domain: HostName;
        pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.Assign (name, username);
        Strings.Assign (host, domain);
        result := NIL;

        (* If name contains an '@', the part after the '@' overrides the host.  *)

        Strings.FindNext ('@', username, 0, found, pos);
        IF found THEN
            Strings.Assign (username, domain);
            Strings.Delete (domain, 0, pos+1);
            username[pos] := Nul;
        END (*IF*);

        IF domain[0] <> Nul THEN

            (* First look for the user@host form, and if that fails then        *)
            (* revert back to the name as supplied.                             *)

            Strings.Assign (username, guess);
            Strings.Append ("@", guess);
            Strings.Append (domain, guess);
            Try (guess);
        END (*IF*);

        IF result = NIL THEN
            Try (username);
        END (*IF*);

        RETURN result;

    END ReadUserData;

(********************************************************************************)

PROCEDURE PasswordAcceptable (U: User;  VAR (*IN*) pass: ARRAY OF CHAR): BOOLEAN;

    (* Tests for a password match. *)

    VAR dummy: CARDINAL;  found: BOOLEAN;
        ctx: MD5_CTX;  digest: MD5_DigestType;
        buffer: PassString;

    BEGIN
        IF U = NIL THEN
            found := FALSE;
        ELSIF (U^.Password[0] = '@') AND (U^.Password[1] = Nul) THEN
            Strings.FindNext ('@', pass, 0, found, dummy);
        ELSE
            IF U^.EncryptPassword THEN
                ctx := MD5Init();
                MD5Update (ctx, pass, LENGTH(pass));
                MD5Final (ctx, digest);
                MD5DigestToString (digest, buffer);
                found := Strings.Equal (buffer, U^.Password);
            ELSE
                found := Strings.Equal (pass, U^.Password);
            END (*IF*);
        END (*IF*);
        RETURN found;
    END PasswordAcceptable;

(********************************************************************************)

PROCEDURE CheckLimitedUse (hini: HINI;  VAR (*IN*) name: UserName);

    (* Checks whether this is a limited-use account, and takes appropriate      *)
    (* action if so. This procedure should be called after the user has been    *)
    (* authenticated, i.e. once we are certain that this is a genuine login.    *)

    VAR LoginLimit: CARDINAL;
        SingleUse, DeleteExpired: BOOLEAN;

    BEGIN
        LoginLimit := 0;

        (* Limited-use account? *)

        IF INIGet (hini, name, "SingleUse", SingleUse) THEN
            (* Obsolete option *)
            IF SingleUse THEN
                LoginLimit := 1;
            END (*IF*);
            INIDeleteKey (hini, name, "SingleUse");
        ELSE EVAL (INIGet(hini, name, "LoginLimit", LoginLimit));
        END (*IF*);
        IF NOT INIGet (hini, name, "DeleteExpired", DeleteExpired) THEN
            DeleteExpired := FALSE;
        END (*IF*);

        (* If this is a 'limited logins' account, decrement the     *)
        (* number of logins allowed.  If the count reaches zero,    *)
        (* keep the user data in memory but either delete it from   *)
        (* the INI data or make the account inactive, depending on  *)
        (* the DeletedExpired flag in the INI file.                 *)
        (*       Note: LoginLimit=0 means no limit.                 *)

        IF LoginLimit > 0 THEN
            DEC (LoginLimit);
            IF LoginLimit = 0 THEN
                IF DeleteExpired THEN
                    INIDeleteApp (hini, name);
                ELSE
                    INIPut (hini, name, "Active", FALSE);
                END (*IF*);
            ELSE
                INIPut (hini, name, "LoginLimit", LoginLimit);
            END (*IF*);
        END (*IF*);

    END CheckLimitedUse;

(********************************************************************************)

PROCEDURE GetUserNumber (U: User;  VAR (*OUT*) UserNumber, Limit: CARDINAL);

    (* Returns the user number and user limit, within the category defined by   *)
    (* this user's username.                                                    *)

    BEGIN
        IF U = NIL THEN
            UserNumber := 0;  Limit := 0;
        ELSE
            UserNumber := U^.UserNumber;
            Limit := U^.UserLimit;
        END (*IF*);
    END GetUserNumber;

(********************************************************************************)

PROCEDURE NoteLoginTime (U: User);

    (* Puts the current date/time as the "last login time" for this user.  This *)
    (* is also the point at which we check whether this is a limited-use        *)
    (* account.                                                                 *)

    VAR hini: HINI;  timestamp: ARRAY [0..18] OF CHAR;

    BEGIN
        CurrentTimeToString (timestamp);
        hini := OpenINIForUser (U^.Name, FALSE);
        IF INIValid(hini) THEN
            INIPut (hini, U^.Name, "LastLogin", timestamp);
            CheckLimitedUse (hini, U^.Name);
            CloseINIFile (hini);
        END (*IF*);
    END NoteLoginTime;

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
            IF p^.virtual THEN
                DISPOSE (p^.link);
            END (*IF*);
            DISPOSE (p);
            p := q;
        END (*IF*);
    END KillList;

(********************************************************************************)

PROCEDURE DestroyUserData (VAR (*INOUT*) U: User);

    (* Discards the data structure.  *)

    VAR current: CountList;

    BEGIN
        IF U <> NIL THEN

            (* Decrement the number of instances of this username. *)

            Obtain (UserCountLock);
            current := UserCount;
            WHILE (current <> NIL) AND NOT Strings.Equal(current^.name, U^.Name) DO
                current := current^.next;
            END (*WHILE*);
            IF current <> NIL THEN
                DEC (current^.count);
            END (*IF*);
            Release (UserCountLock);
            KillList (U^.TreeRoot);
            DiscardHideList (U);
            DISPOSE (U);
        END (*IF*);
    END DestroyUserData;

(********************************************************************************)
(*                           STRING COMPARISONS                                 *)
(********************************************************************************)

PROCEDURE CompareStrings (first, second: ARRAY OF CHAR): INTEGER;

    (* Returns +1 if first>second, 0 if first=second, and -1 if first<second.   *)
    (* Character case is ignored when making the comparison.                    *)

    VAR k: CARDINAL;

    BEGIN
        k := 0;
        LOOP
            IF k > HIGH(second) THEN
                IF k > HIGH(first) THEN RETURN 0
                ELSE RETURN +1
                END (*IF*);
            ELSIF k > HIGH(first) THEN RETURN -1
            ELSIF first[k] = Nul THEN
                IF second[k] = Nul THEN RETURN 0
                ELSE RETURN -1
                END (*IF*);
            ELSIF CAP(first[k]) > CAP(second[k]) THEN RETURN +1
            ELSIF CAP(first[k]) < CAP(second[k]) THEN RETURN -1
            ELSE INC(k);
            END (*IF*);
        END (*LOOP*);
    END CompareStrings;

(********************************************************************************)

PROCEDURE NameMatch (VAR (*IN*) first, second: ARRAY OF CHAR): BOOLEAN;

    (* Equality test, modulo upper/lower case differences.  That is, the result *)
    (* is TRUE even if the letters in first and second don't have the same      *)
    (* case, as long as the strings are otherwise equal.                        *)

    BEGIN
        RETURN CompareStrings(first, second) = 0;
    END NameMatch;

(********************************************************************************)

(*
PROCEDURE WildMatch (first, second: ARRAY OF CHAR): BOOLEAN;

    (* Same as NameMatch, except that second is allowed to contain the          *)
    (* wildcard characters '*' and '?'.  This is not a completely general       *)
    (* wild-match routine, but it is good enough for the limited use we have    *)
    (* for it.  The wildcard matching that is done for directory listing is     *)
    (* handled by FirstDirEntry, outside this module.                           *)

    VAR k1, k2: CARDINAL;

    BEGIN
        k1 := 0;  k2 := 0;
        LOOP
            IF k2 > HIGH(second) THEN RETURN (k1 > HIGH(first));
            ELSIF second[k2] = '*' THEN
                INC(k2);
                WHILE (k1 <= HIGH(first)) AND (first[k1] <> Nul)
                                AND (CAP(first[k1]) <> CAP(second[k2])) DO
                    INC (k1);
                END (*WHILE*);
            ELSIF k1 > HIGH(first) THEN RETURN FALSE;
            ELSIF first[k1] = Nul THEN RETURN (second[k2] = Nul);
            ELSIF second[k2] = '?' THEN INC(k1);  INC (k2);
            ELSIF CAP(first[k1]) <> CAP(second[k2]) THEN RETURN FALSE;
            ELSE INC(k1);  INC(k2);
            END (*IF*);
        END (*LOOP*);
    END WildMatch;
*)

(********************************************************************************)

PROCEDURE InHideList (U: User;  name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if this name is in the user's hide list.   *)
    (* Special case: ".." is never hidden.                     *)

    VAR p: HideListPtr;
        dotdot: ARRAY [0..1] OF CHAR;

    BEGIN
        dotdot := "..";
        IF WildMatch (name, dotdot) THEN
            RETURN FALSE;
        ELSE
            p := U^.HideList;
            WHILE (p <> NIL) AND NOT WildMatch (name, p^.mask) DO
                p := p^.next;
            END (*WHILE*);
            RETURN p <> NIL;
        END (*IF*);
    END InHideList;

(********************************************************************************)
(*                             VOLUME OPERATIONS                                *)
(********************************************************************************)

PROCEDURE SpaceAvailable (name: FName): CARDINAL;

    (* Returns the free space, in kilobytes, on the drive that would be used    *)
    (* to store this file if we accepted it.                                    *)

    BEGIN
        IF (name = NIL) OR (name^.dir[0] = Nul) THEN
            RETURN 0;
        ELSIF name^.dir[1] = ':' THEN
            RETURN FreeSpace (name^.dir[0]);
        ELSE
            (* Temporary workaround for the fact that I don't know how to       *)
            (* calculate free space on a network drive.                         *)
            RETURN MAX(CARDINAL);
        END (*IF*);
    END SpaceAvailable;

(********************************************************************************)
(*                          FILE NAME PROCESSING                                *)
(********************************************************************************)

PROCEDURE MakeFullName (Name: FName;  VAR (*OUT*) FullName: ARRAY OF CHAR);

    (* Converts a relative file name to a full file name.  *)

    VAR k: CARDINAL;

    BEGIN
        IF (Name = NIL) OR (Name^.EntryPtr = NIL) OR (Name^.dir[0] = Nul) THEN
            FullName[0] := Nul;
            RETURN;
        ELSE
            Strings.Assign (Name^.dir, FullName);
        END (*IF*);
        IF Name^.fname[0] = Nul THEN

            (* Strip trailing '/'. *)

            k := LENGTH (FullName);
            IF k > 0 THEN
                DEC (k);
                IF FullName[k] = '/' THEN
                    FullName[k] := Nul;
                END (*IF*);
            END (*IF*);
        ELSE
            Strings.Append (Name^.fname, FullName);
        END (*IF*);

    END MakeFullName;

(********************************************************************************)

PROCEDURE MakeShortName (Name: FName;  VAR (*OUT*) ShortName: ARRAY OF CHAR);

    (* Converts an FName to a file name relative to its own directory.  *)

    BEGIN
        IF Name = NIL THEN
            ShortName[0] := Nul;
        ELSE
            Strings.Assign (Name^.fname, ShortName);
        END (*IF*);
    END MakeShortName;

(********************************************************************************)

PROCEDURE SplitHead (VAR (*INOUT*) src: ARRAY OF CHAR;  VAR (*OUT*) head: ARRAY OF CHAR);

    (* Finds the first '/' character in src.  Everything up to the separator is *)
    (* assigned to head, the separator itself is discarded, the remainder is    *)
    (* assigned to src.  If there is no '/', src becomes the null string.       *)

    (* (Any '\' found is treated as a '/'.)                                     *)

    VAR k: CARDINAL;  ch: CHAR;

    BEGIN
        k := 0;  ch := Nul;
        LOOP
            IF k > HIGH(src) THEN
                EXIT (*LOOP*);
            END (*IF*);
            ch := src[k];
            IF (ch = Nul) OR (ch = '/') OR (ch = '\')
                       OR (k > HIGH(head)) THEN
                EXIT (*LOOP*);
            ELSE
                head[k] := ch;  INC(k);
            END (*IF*);
        END (*LOOP*);
        IF k <= HIGH(head) THEN
            head[k] := Nul;
        END (*IF*);
        IF ch <> Nul THEN
            INC (k);
        END (*IF*);
        IF k > HIGH(src) THEN
            Strings.Assign ("", src);
        ELSE
            Strings.Delete (src, 0, k);
        END (*IF*);
    END SplitHead;

(********************************************************************************)

PROCEDURE SplitTail (VAR (*INOUT*) src: ARRAY OF CHAR;
                              VAR (*OUT*) head: ARRAY OF CHAR);

    (* Finds the last '/' character in src.  Everything up to the separator is  *)
    (* assigned to head, the separator is discarded, and src is set to whatever *)
    (* is left after the separator.  If there is no '/', head becomes the null  *)
    (* string.  Special case: if the last '/' is actually the first character   *)
    (* of src, then we return with head = "/".                                  *)

    (* (Any '\' found is treated as a '/'.)                                     *)

    VAR k: CARDINAL;  ch: CHAR;

    BEGIN
        k := Strings.Length (src);  ch := Nul;
        LOOP
            IF k = 0 THEN EXIT(*LOOP*) END(*IF*);
            DEC (k);
            ch := src[k];
            IF ch = '\' THEN ch := '/' END(*IF*);
            IF ch = '/' THEN EXIT (*LOOP*) END(*IF*);
        END (*LOOP*);
        IF ch = '/' THEN
            Strings.Assign (src, head);
            Strings.Delete (src, 0, k+1);
            IF k = 0 THEN INC(k) END(*IF*);
        END (*IF*);
        head[k] := Nul;
    END SplitTail;

(********************************************************************************)

PROCEDURE SplitTail1 (VAR (*INOUT*) src: ARRAY OF CHAR;
                              VAR (*OUT*) tail: ARRAY OF CHAR);

    (* Finds the last '/' character in src.  Everything up to the separator is  *)
    (* assigned to src, the separator is discarded, and tail is set to whatever *)
    (* is left after the separator.  If there is no '/', src becomes the null   *)
    (* string.  Special case: if the last '/' is actually the first character   *)
    (* of src, then we return with src = "/".                                   *)

    (* (Any '\' found is treated as a '/'.)                                     *)

    VAR k: CARDINAL;  ch: CHAR;

    BEGIN
        k := Strings.Length (src);  ch := Nul;
        LOOP
            IF k = 0 THEN EXIT(*LOOP*) END(*IF*);
            DEC (k);
            ch := src[k];
            IF ch = '\' THEN ch := '/' END(*IF*);
            IF ch = '/' THEN EXIT (*LOOP*) END(*IF*);
        END (*LOOP*);
        IF ch = '/' THEN
            Strings.Assign (src, tail);
            Strings.Delete (tail, 0, k+1);
            IF k = 0 THEN INC(k) END(*IF*);
        ELSE
            Strings.Assign (src, tail);
        END (*IF*);
        src[k] := Nul;
    END SplitTail1;

(********************************************************************************)

PROCEDURE StripTail (VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Deletes the last subdirectory specification from name, for example       *)
    (* we change A/B/C/D/ to A/B/C/.                                            *)

    VAR k: CARDINAL;

    BEGIN
        k := Strings.Length (name);
        IF k > 0 THEN
            REPEAT
                DEC (k);
            UNTIL (k = 0) OR (name[k-1] = '/') OR (name[k-1] = '\');
        END (*IF*);
        name[k] := Nul;
    END StripTail;

(********************************************************************************)

PROCEDURE TailMatch (first, second: ARRAY OF CHAR): BOOLEAN;

    (* Tests whether the tail of first matches second. *)

    VAR tail: FileNameString;

    BEGIN
        SplitTail1 (first, tail);
        RETURN NameMatch (tail, second);
    END TailMatch;

(********************************************************************************)
(*                              DIRECTORIES                                     *)
(********************************************************************************)

PROCEDURE NameIsReserved (name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name is a device name such as CLOCK$. *)

    CONST
        List3 = "CONNULPRN";
        List4 = "COM1COM2COM3COM4KBD$LPT1LPT2LPT3";
        List6 = "CLOCK$MOUSE$";
        List7 = "SCREEN$";
        List8 = "POINTER$";

    (********************************************************************)

    PROCEDURE Match (length: CARDINAL;  list: ARRAY OF CHAR): BOOLEAN;

        (* List is the concatenation of strings of 'length' characters. *)
        (* We return TRUE iff name matches one of them.                 *)

        VAR j, k, limit: CARDINAL;

        BEGIN
            limit := LENGTH(list);
            k := 0;
            LOOP
                IF k >= limit THEN RETURN FALSE END(*IF*);
                j := 0;
                LOOP
                    IF j = length THEN RETURN TRUE END(*IF*);
                    IF CAP(name[j]) <> list[k] THEN EXIT(*LOOP*) END(*IF*);
                    INC(j);  INC(k);
                END (*LOOP*);
                INC (k, length-j);
            END (*LOOP*);
        END Match;

    (********************************************************************)

    BEGIN
        CASE LENGTH(name) OF
          |  3:  RETURN Match(3, List3);
          |  4:  RETURN Match(4, List4);
          |  6:  RETURN Match(6, List6);
          |  7:  RETURN Match(7, List7);
          |  8:  RETURN Match(8, List8);
          |  ELSE
                 RETURN FALSE;
        END (*CASE*);
    END NameIsReserved;

(********************************************************************************)

PROCEDURE FileIsADirectory (parent: FName;  name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the file exists and is a directory.   *)

    VAR FullName: FileNameString;  D: DirectoryEntry;  result: BOOLEAN;

    BEGIN
        IF parent^.EntryPtr = NIL THEN result := FALSE
        ELSIF parent^.dir[0] = Nul THEN result := FALSE
        ELSIF name[0] = Nul THEN result := TRUE;
        ELSE
            FullName := parent^.dir;
            Strings.Append (name, FullName);

            (* Watch out for wildcard matches! *)

            result := FirstDirEntry (FullName, FALSE, TRUE, TRUE, D)
                         AND (directory IN D.attr)
                         AND TailMatch (name, D.name);
            DirSearchDone (D);
        END (*IF*);

        RETURN result;

    END FileIsADirectory;

(********************************************************************************)

PROCEDURE FindDirEntry (parent: FName;  name: ARRAY OF CHAR;
                                                  NoCheck: BOOLEAN): DirEntryPtr;

    (* Looks for name in the directory described by the directory part of       *)
    (* parent.  (The fname component of parent is not used.)  If subdirectory   *)
    (* name is not already in the directory tree, but name exists and is a      *)
    (* directory, an entry is added to the tree.  If the NoCheck flag is set,   *)
    (* we trust the caller's assertion that name is indeed a directory,         *)
    (* so we don't do the check again.                                          *)
    (* Returns NIL if the file doesn't exist.                                   *)

    (* Note: new entries are added at the tail of the appropriate list, to      *)
    (* ensure that already-existing entries have priority over those added      *)
    (* during a directory scan.                                                 *)

    (* Special case: we don't want ".." records wasting memory, so we deal      *)
    (* with them in such a way that they don't get added to the tree.           *)

    VAR father, previous, current: DirEntryPtr;

    BEGIN
        father := parent^.EntryPtr;
        IF Strings.Equal (name, "..") THEN
            RETURN father^.parent;
        END (*IF*);

        previous := NIL;  current := father^.FirstChild;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            IF NameMatch (name, current^.name) THEN EXIT(*LOOP*) END(*IF*);
            previous := current;  current := current^.next;
        END (*LOOP*);

        IF current = NIL THEN
            IF NoCheck OR FileIsADirectory (parent, name) THEN
                NEW (current);
                Strings.Assign (name, current^.name);
                current^.parent := father;
                current^.FirstChild := NIL;  current^.next := NIL;
                current^.flags := father^.flags;
                current^.BeenHereBefore := FALSE;
                IF previous = NIL THEN
                    father^.FirstChild := current;
                ELSE
                    previous^.next := current;
                END (*IF*);
                current^.exists := TRUE;  current^.IsADir := TRUE;
                current^.categorised := TRUE;  current^.virtual := FALSE;
                current^.link := NIL;
            END (*IF*);
        END (*IF*);

        IF (current <> NIL) AND NOT current^.categorised THEN
            CategoriseNode (current, parent^.dir);
        END (*IF*);

        RETURN current;

    END FindDirEntry;

(********************************************************************************)
(*                         THE INTERNAL DATA TYPE FName                         *)
(********************************************************************************)

PROCEDURE MakeFName (U: User;  filename: ARRAY OF CHAR): FName;

    (* Creates an FName descriptor from the filename.  The result is NIL for    *)
    (* a name that implies a nonexistent directory.                             *)

    VAR result: FName;
        head: FileNameString;
        NewEntryPtr: DirEntryPtr;  FinalSlash, finished: BOOLEAN;
        k: CARDINAL;

    BEGIN
        (* For later reference, we want to know whether the name ends with '/'. *)

        k := LENGTH (filename);
        FinalSlash := (k > 0) AND ((filename[k-1] = '/') OR (filename[k-1] = '\'));

        NEW (result);
        WITH result^ DO
            Strings.Assign (filename, fname);

            (* Start at the user's root directory or the user's current     *)
            (* directory, depending on whether the name starts with a '/'.  *)

            IF (fname[0] = '/') OR (fname[0] = '\') THEN
                EntryPtr := U^.TreeRoot;
                IF EntryPtr^.virtual THEN
                    dir := EntryPtr^.link^.this;
                    IF dir[0] <> Nul THEN
                        Strings.Append ('/', dir);
                    END (*IF*);
                ELSE
                    dir := "";
                END (*IF*);
                vdir := '/';
                Strings.Delete (fname, 0, 1);
            ELSE
                dir := U^.CurrentDir;
                vdir := U^.CurrentVDir;
                EntryPtr := U^.PosInTree;
            END (*IF*);

            (* Now strip all leading directory information from fname, and  *)
            (* move it to dir, updating EntryPtr as we go.                  *)

            finished := (EntryPtr = NIL) OR (fname[0] = Nul);
            WHILE NOT finished DO

                SplitHead (fname, head);
                IF Strings.Equal (head, "..") THEN

                    (* Change to parent directory. *)

                    IF EntryPtr^.virtual THEN
                        dir := EntryPtr^.link^.parent;
                        StripTail (vdir);
                        EntryPtr := EntryPtr^.parent;
                    ELSE
                        EntryPtr := EntryPtr^.parent;
                        IF EntryPtr <> NIL THEN
                            StripTail (dir);
                            StripTail (vdir);
                        END (*IF*);
                    END (*IF*);

                ELSIF (head[0] = '.') AND (head[1] = Nul) THEN

                    (* Change to current directory, i.e. do nothing. *)

                ELSIF NameIsReserved (head) THEN

                    EntryPtr := NIL;

                ELSE
                    (* Try to move to a subdirectory of the current directory. *)

                    NewEntryPtr := FindDirEntry (result, head, FALSE);
                    IF (NewEntryPtr = NIL) OR NOT NewEntryPtr^.exists THEN

                        (* 'head' is not the name of a subdirectory. *)

                        finished := TRUE;
                        IF (fname[0] = Nul) AND NOT FinalSlash THEN

                            (* Still OK, we must have arrived at a file name or *)
                            (* mask instead of a subdirectory name.             *)

                            fname := head;

                        ELSE

                            (* Illegal file name, clear EntryPtr to show error. *)

                            EntryPtr := NIL;

                        END (*IF*);

                    ELSIF NewEntryPtr^.virtual THEN

                        (* We've reached a symbolic link.  Is it a link to an   *)
                        (* ordinary file or to a directory?                     *)

                        EntryPtr := NewEntryPtr;
                        dir := EntryPtr^.link^.this;
                        IF EntryPtr^.IsADir THEN
                            Strings.Append (head, vdir);
                            Strings.Append ('/', vdir);
                        ELSIF (fname[0] <> Nul) OR FinalSlash THEN
                            EntryPtr := NIL;
                        ELSE
                            fname := dir;
                            SplitTail (fname, dir);
                            finished := TRUE;
                        END (*IF*);
                        IF dir[0] <> Nul THEN
                            Strings.Append ('/', dir);
                        END (*IF*);

                    ELSIF NewEntryPtr^.IsADir THEN

                        (* Move to subdirectory. *)

                        EntryPtr := NewEntryPtr;
                        Strings.Append (head, dir);
                        Strings.Append ('/', dir);
                        Strings.Append (head, vdir);
                        Strings.Append ('/', vdir);

                    ELSIF (fname[0] <> Nul) OR FinalSlash THEN

                        (* Attempt to treat a file as a directory. *)

                        EntryPtr := NIL;

                    ELSE

                        (* We've reached a file rather than a directory. *)

                        EntryPtr := NewEntryPtr;
                        fname := head;
                        finished := TRUE;

                    END (*IF*);

                END (*IF*);

                finished := finished OR (EntryPtr = NIL) OR (fname[0] = Nul);

            END (*WHILE*);

        END (*WITH*);

        IF result^.EntryPtr = NIL THEN
            DISPOSE (result);
        END (*IF*);

        (*
        WriteString ("MakeFName: ");
        IF result = NIL THEN
            WriteString ("NIL result");  WriteLn;
        ELSE
            WriteString ("dir = ");  WriteString (result^.dir);
            WriteString (", vdir = ");  WriteString (result^.vdir);
            WriteString (", fname = ");  WriteString (result^.fname);
            WriteLn;
            IF result^.EntryPtr^.virtual THEN
                WriteString ("  (virtual) parent = ");  WriteString (result^.EntryPtr^.link^.parent);
                WriteString (", this = ");  WriteString (result^.EntryPtr^.link^.this);
                WriteLn;
            END (*IF*);
        END (*IF*);
        *)

        RETURN result;

    END MakeFName;

(********************************************************************************)

PROCEDURE DiscardFName (VAR (*INOUT*) name: FName);

    (* Disposes of the storage used by name. *)

    BEGIN
        IF name <> NIL THEN
            DISPOSE (name);
        END (*IF*);
    END DiscardFName;

(********************************************************************************)

PROCEDURE SameParent (name1, name2: FName): BOOLEAN;

    (* Checks whether two file names are in the same directory, as seen by the  *)
    (* user.                                                                    *)

    VAR parent1, parent2: FileNameString;

    BEGIN
        IF name1 = NIL THEN
            RETURN name2 = NIL;
        ELSE
            parent1 := name1^.vdir;
            IF name1^.fname[0] = Nul THEN
                StripTail (parent1);
            END (*IF*);
            parent2 := name2^.vdir;
            IF name2^.fname[0] = Nul THEN
                StripTail (parent2);
            END (*IF*);
            RETURN CompareStrings(parent1, parent2) = 0;
        END (*IF*);
    END SameParent;

(********************************************************************************)

PROCEDURE SameDrive (name1, name2: FName): BOOLEAN;

    (* Checks whether two file names are in the same physical drive. *)

    BEGIN
        IF name1 = NIL THEN
            RETURN name2 = NIL;
        ELSIF name2 = NIL THEN
            RETURN FALSE;
        ELSIF (name1^.dir[1] <> ':') OR (name2^.dir[1] <> ':') THEN
            RETURN FALSE;
        ELSE
            RETURN CAP(name1^.dir[0]) = CAP(name2^.dir[0]);
        END (*IF*);
    END SameDrive;

(********************************************************************************)
(*                                FILE OPERATIONS                               *)
(********************************************************************************)

PROCEDURE OpenForReading (VAR (*OUT*) cid: ChanId;  name: FName): BOOLEAN;

    (* Opens the file, returns TRUE iff successful. *)

    VAR FullName: FileNameString;

    BEGIN
        MakeFullName (name, FullName);
        cid := OpenOldFile (FullName, FALSE, TRUE);
        RETURN cid <> NoSuchChannel;
    END OpenForReading;

(********************************************************************************)

PROCEDURE OpenForWriting (VAR (*OUT*) cid: ChanId;  name: FName): BOOLEAN;

    (* Opens the file - which might or might not already exist - and returns    *)
    (* TRUE iff successful.                                                     *)

    VAR FullName: FileNameString;

    BEGIN
        MakeFullName (name, FullName);
        IF FileSys.Exists (FullName) THEN
            cid := OpenOldFile (FullName, TRUE, TRUE);
        ELSE
            cid := OpenNewFile (FullName, TRUE);
        END (*IF*);
        RETURN cid <> NoSuchChannel;
    END OpenForWriting;

(********************************************************************************)

PROCEDURE OpenForAppend (VAR (*OUT*) cid: ChanId;  name: FName): BOOLEAN;

    (* Opens the file, returns TRUE iff successful. *)

    VAR FullName: FileNameString;

    BEGIN
        MakeFullName (name, FullName);
        IF FileSys.Exists (FullName) THEN
            cid := OpenOldFile (FullName, TRUE, TRUE);
            SetPosition (cid, EndPosition(cid));
        ELSE
            cid := OpenNewFile (FullName, TRUE);
        END (*IF*);
        RETURN cid <> NoSuchChannel;
    END OpenForAppend;

(********************************************************************************)

PROCEDURE RemoveFile (name: FName): BOOLEAN;

    (* Deletes the file, returns TRUE iff successful. *)

    VAR result: BOOLEAN;
        FullName: FileNameString;

    BEGIN
        MakeFullName (name, FullName);
        FileSys.Remove (FullName, result);
        RETURN result;
    END RemoveFile;

(********************************************************************************)

PROCEDURE Rename (OldName, NewName: FName): BOOLEAN;

    (* Renames OldName to NewName.  Returns TRUE for success.  *)

    VAR result: BOOLEAN;
        FullName1, FullName2, tail, dummy: FileNameString;

    BEGIN
        MakeFullName (OldName, FullName1);
        MakeFullName (NewName, FullName2);
        tail := FullName2;
        SplitTail (tail, dummy);
        IF CompareStrings(FullName1, FullName2) = 0 THEN
            result := TRUE;
        ELSIF (OldName = NIL) OR (OldName^.EntryPtr = NIL) THEN
            result := FALSE;
        ELSE
            FileSys.Rename (FullName1, FullName2, result);
            IF result THEN
                OldName^.EntryPtr^.name := tail;
            END (*IF*);
        END (*IF*);
        RETURN result;
    END Rename;

(********************************************************************************)
(*                            DIRECTORY OPERATIONS                              *)
(********************************************************************************)

PROCEDURE Encode (flags: PermissionSet;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Translates flags into a four-character string.  The Visible attribute    *)
    (* is not encoded, since we're never going to use this encoding for an      *)
    (* invisible directory.                                                     *)

    BEGIN
        Strings.Assign ("----", result);
        IF AllowRead IN flags THEN
            result[0] := 'r';
        END (*IF*);
        IF AllowWrite IN flags THEN
            result[1] := 'w';
        END (*IF*);
        IF AllowDelete IN flags THEN
            result[2] := 'x';
        END (*IF*);
        IF AllowRename IN flags THEN
            result[3] := 'n';
        END (*IF*);
    END Encode;

(********************************************************************************)

PROCEDURE PermissionString (U: User;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Returns a string indicating read/write/delete permissions for the        *)
    (* user's current directory.                                                *)

    VAR p: DirEntryPtr;

    BEGIN
        Strings.Assign ("----", result);
        p := U^.PosInTree;
        IF p <> NIL THEN
            Encode (p^.flags, result);
        END (*IF*);
    END PermissionString;

(********************************************************************************)

PROCEDURE FileOrDirExists (name: FName;  ShowHidden: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the file or directory exists.  System and hidden files  *)
    (* will appear to be nonexistent unless ShowHidden is TRUE.                 *)

    VAR FullName: FileNameString;  D: DirectoryEntry;  result: BOOLEAN;

    BEGIN
        IF (name = NIL) OR (name^.EntryPtr = NIL) THEN
            RETURN FALSE;
        ELSE
            MakeFullName (name, FullName);
            result := FirstDirEntry (FullName, TRUE, FALSE, ShowHidden, D)
                         AND TailMatch (FullName, D.name)
                         AND (ShowHidden
                             OR NOT ((hidden IN D.attr) OR (system IN D.attr)));
            DirSearchDone (D);
            RETURN result;
        END (*IF*);
    END FileOrDirExists;

(********************************************************************************)

PROCEDURE HaveSeenMessage (U: User): BOOLEAN;

    (* Returns a flag that says whether this is the first call of this          *)
    (* procedure for this user and this user's current directory.               *)

    VAR result: BOOLEAN;

    BEGIN
        IF (U=NIL) OR (U^.PosInTree = NIL) THEN
            result := TRUE;
        ELSE
            result := U^.PosInTree^.BeenHereBefore;
            U^.PosInTree^.BeenHereBefore := TRUE;
        END (*IF*);
        RETURN result;
    END HaveSeenMessage;

(********************************************************************************)

PROCEDURE NameOfCurrentDirectory (U: User;  VAR (*OUT*) DirString: ARRAY OF CHAR);

    (* Gives back the name of the current directory for this user.  *)

    BEGIN
        Strings.Assign (U^.CurrentVDir, DirString);
    END NameOfCurrentDirectory;

(********************************************************************************)

PROCEDURE RealNameOfCurrentDirectory (U: User;  VAR (*OUT*) DirString: ARRAY OF CHAR);

    (* Gives back the name of the current directory for this user.  *)

    VAR pos: CARDINAL;  found: BOOLEAN;
    BEGIN
        Strings.Assign (U^.CurrentDir, DirString);

        (* Change all '/' to '\', and remove the final '\' unless it is the     *)
        (* only character.  This makes the result useable in a "CD" command.    *)

        pos := 0;
        LOOP
            Strings.FindNext ('/', DirString, pos, found, pos);
            IF NOT found THEN EXIT(*LOOP*) END(*IF*);
            DirString[pos] := '\';
        END (*LOOP*);

        pos := Strings.Length (DirString);
        IF pos > 1 THEN
            DEC (pos);
            IF DirString[pos] = '\' THEN
                DirString[pos] := Nul;
            END (*IF*);
        END (*IF*);

    END RealNameOfCurrentDirectory;

(********************************************************************************)

PROCEDURE SetWorkingDirectory (U: User;  newdir: FName): BOOLEAN;

    (* Changes user to the specified directory.  Returns FALSE if the requested    *)
    (* directory does not exist, or if the user does not have the right to see it. *)

    (* Special case: it is legal to go to the root directory even if it is invisible. *)

    VAR k: CARDINAL;  success: BOOLEAN;
        head, vdir: FileNameString;

    BEGIN
        success := (newdir <> NIL) AND (newdir^.fname[0] = Nul)
                           AND (newdir^.EntryPtr <> NIL)
                           AND newdir^.EntryPtr^.IsADir
                           AND ((Visible IN newdir^.EntryPtr^.flags)
                                OR (newdir^.EntryPtr^.parent = NIL));

        IF success THEN
            (* Check the HideList. *)

            vdir := newdir^.vdir;

            (* vdir usually ends with a '/', which we want to remove    *)
            (* for this test.                                           *)

            k := LENGTH(vdir);
            IF k > 0 THEN
                DEC(k);
                IF vdir[k] = '/' THEN
                    vdir[k] := Nul;
                END (*IF*);
            END (*IF*);
            SplitTail (vdir, head);
            success := NOT InHideList(U, vdir);
        END (*IF*);

        IF success THEN
            WITH U^ DO
                PosInTree := newdir^.EntryPtr;
                CurrentDir := newdir^.dir;
                CurrentVDir := newdir^.vdir;
            END (*WITH*);
        END (*IF*);
        RETURN success;
    END SetWorkingDirectory;

(********************************************************************************)
(*                     PUTTING OUT A DIRECTORY LISTING                          *)
(********************************************************************************)

PROCEDURE EncodeDirDetails (D: DirectoryEntry;  permissions: ARRAY OF CHAR;
                                       VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                       VAR (*INOUT*) pos: CARDINAL);

    (* Encoding of date, time, attributes, etc of a directory entry. *)

    (****************************************************************************)

    PROCEDURE EncodeDateTime (datecode, timecode: CARD16);

        (* This version sends American-style dates, for compatibility with      *)
        (* ftp clients that are fussy about the format.                         *)

        VAR day, month, year: CARDINAL;

        (************************************************************************)

        PROCEDURE AgeInMonths(): INTEGER;

            VAR Now: SysClock.DateTime;
            BEGIN
                SysClock.GetClock (Now);
                RETURN 12*(VAL(INTEGER,Now.year) - VAL(INTEGER,year))
                       + VAL(INTEGER,Now.month) - VAL(INTEGER,month);
            END AgeInMonths;

        (************************************************************************)

        TYPE MonthNameType = ARRAY [0..15] OF ARRAY [0..2] OF CHAR;

        CONST MonthName = MonthNameType {'M00', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
                                         'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
                                         'Dec', 'M13', 'M14', 'M15'};

        VAR minute, hour: CARDINAL;  age: INTEGER;

        BEGIN
            day := datecode MOD 32;  datecode := datecode DIV 32;
            month := datecode MOD 16;  year := 1980 + datecode DIV 16;
            AppendString (MonthName[month], buffer, pos);
            buffer[pos] := ' ';  INC(pos);
            ConvertCardRJ (day, buffer, 2, pos);
            buffer[pos] := ' ';  INC(pos);
            age := AgeInMonths();
            IF (age >= 0) AND (age <= 6) THEN
                timecode := timecode DIV 32;
                minute := timecode MOD 64;  hour := timecode DIV 64;
                ConvertCardZ (hour, buffer, 2, pos);
                buffer[pos] := ':';  INC(pos);
                ConvertCardZ (minute, buffer, 2, pos);
            ELSE
                buffer[pos] := ' ';  INC(pos);
                ConvertCardZ (year, buffer, 4, pos);
            END (*IF*);
        END EncodeDateTime;

    (****************************************************************************)

    PROCEDURE EncodeAttributes (attr: FileAttr);

        (************************************************************************)

        PROCEDURE PutAttr (totest: FileAttribute;  code, filler: CHAR): CARDINAL;

            BEGIN
                IF NOT (totest IN attr) THEN
                    code := filler;
                END (*IF*);
                IF code = ' ' THEN RETURN 0
                ELSE
                    buffer[pos] := code;  INC(pos);
                    RETURN 1;
                END (*IF*);
            END PutAttr;

        (************************************************************************)

        CONST attrfiller = '-';
        VAR count: CARDINAL;  perm3: ARRAY [0..3] OF CHAR;

        BEGIN
            EVAL (PutAttr (directory, 'd', '-'));
            Strings.Assign (permissions, perm3);  perm3[3] := Nul;
            AppendString (perm3, buffer, pos);
            AppendString (perm3, buffer, pos);
            AppendString (perm3, buffer, pos);
            AppendString ("  1 ftp      ", buffer, pos);
            count := PutAttr (readonly, 'R', attrfiller)
                     + PutAttr (archive, 'A', attrfiller)
                     + PutAttr (system, 'S', attrfiller)
                     + PutAttr (hidden, 'H', attrfiller);
            buffer[pos] := CAP(permissions[3]);  INC(pos);  INC(count);
            IF count = 0 THEN
                buffer[pos] := '0';  INC(pos);  INC(count);
            END (*IF*);
            WHILE count < 4 DO
                buffer[pos] := ' ';  INC(pos);  INC(count);
            END (*WHILE*)
        END EncodeAttributes;

    (****************************************************************************)

    BEGIN
        EncodeAttributes (D.attr);
        ConvertCard64RJ (D.size, buffer, 12, pos);
        buffer[pos] := ' ';  INC(pos);
        EncodeDateTime (D.datePkd, D.timePkd);
        buffer[pos] := ' ';  INC(pos);
    END EncodeDirDetails;

(********************************************************************************)

TYPE
    LODptr = POINTER TO LODentry;
    LODentry = RECORD
                   next: LODptr;
                   this: FName;
               END (*RECORD*);

    (* A queue type, for holding a list of the directories that still have to   *)
    (* be listed.  We need this in case we're recursing over subdirectories.    *)

    ListOfDirectories = RECORD
                            head, tail: LODptr;
                        END (*RECORD*);

    (* Another queue type, this time for a list of items in a single directory. *)
    (* Instead of sending off the listing line as we reach each item, we can    *)
    (* collect the data for an entire directory listing, and then send it all   *)
    (* off after the list has been built.  This allows us to sort the items if  *)
    (* desired, and more importantly it lets us check for duplicates.           *)

    ListOfItems = POINTER TO DirectoryItem;
    DirectoryItem = RECORD
                        next: ListOfItems;
                        flags: PermissionSet;
                        DE: DirectoryEntry;
                    END (*RECORD*);

(********************************************************************************)

PROCEDURE SendTheListing (S: Socket;  U: User;  KeepAlive: Semaphore;
                          VAR (*INOUT*) p: ListOfItems;  options: ListingOptions);

    (* Sends a directory listing to socket S, where p points to a list of the   *)
    (* entries we want to send.  On return the list has been discarded.         *)
    (* File names in the listing are UTF-8 encoded.                             *)

    CONST BufferSize = 1023;

    VAR temp: ListOfItems;  pstring: ARRAY [0..3] OF CHAR;
        buffer, buffer1: ARRAY [0..BufferSize-1] OF CHAR;
        pos, needed: CARDINAL;
        SlashWanted: BOOLEAN;

    BEGIN
        WHILE p <> NIL DO
            IF (Visible IN p^.flags) AND NOT InHideList(U, p^.DE.name) THEN
                pos := 0;
                IF ShowDetails IN options THEN
                    Encode (p^.flags, pstring);
                    EncodeDirDetails (p^.DE, pstring, buffer, pos);
                END (*IF*);
                AppendString (p^.DE.name, buffer, pos);
                SlashWanted := (AddSlash IN options) AND (directory IN p^.DE.attr);

                (* If we're going to overflow the line, truncate the file name. *)

                IF SlashWanted THEN
                    needed := 3;
                ELSE
                    needed := 2;
                END (*IF*);
                IF pos + needed > BufferSize THEN
                    pos := BufferSize - needed;
                END (*IF*);

                IF SlashWanted THEN
                    buffer[pos] := '/';  INC(pos);
                END (*IF*);
                buffer[pos] := Nul;
                TranslateToUTF8 (buffer, buffer1);
                pos := AddEOL (buffer1);
                EVAL (send (S, buffer1, pos, 0));
            END (*IF*);
            temp := p;  p := p^.next;
            DISPOSE(temp);
        END (*WHILE*);
        Signal (KeepAlive);

    END SendTheListing;

(********************************************************************************)

PROCEDURE AddItemToList (VAR (*INOUT*) head: ListOfItems;  D: DirectoryEntry;
                                             permissions: PermissionSet): BOOLEAN;

    (* Adds one directory entry to the list of items waiting to be listed.      *)
    (* Returns FALSE if this duplicates an item already present in the list.    *)
    (* We keep the list sorted by name.                                         *)

    VAR previous, current, this: ListOfItems;  test: INTEGER;

    BEGIN
        previous := NIL;  current := head;  test := +1;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            test := CompareStrings (D.name, current^.DE.name);
            IF test <= 0 THEN
                EXIT (*LOOP*);
            ELSE
                previous := current;  current := current^.next;
            END (*IF*);
        END (*LOOP*);

        IF test <> 0 THEN

            (* Insert new item between previous and current. *);

            NEW (this);
            WITH this^ DO
                next := current;
                flags := permissions;
                DE := D;
            END (*WITH*);
            IF previous = NIL THEN head := this
            ELSE previous^.next := this;
            END (*IF*);

        END (*IF*);

        RETURN test <> 0;

    END AddItemToList;

(********************************************************************************)

PROCEDURE ListRealDirectory (S: Socket;  U: User;  KeepAlive: Semaphore;
                                arg: FName;  options: ListingOptions;
                                VAR (*INOUT*) sublist: ListOfDirectories);

    (* Sends a directory listing, as specified by arg.  If the options include  *)
    (* recursing over subdirectories then sublist will be updated.              *)
    (* The listed file names, as sent to S, are in UTF-8 encoding, but sublist  *)
    (* and arg use file names in the native character set; translation to       *)
    (* UTF-8 does not occur until the actual sending.                           *)

    CONST ListSizeLimit = 200;

    VAR head: ListOfItems;
        D: DirectoryEntry;
        ListSize: CARDINAL;
        success: BOOLEAN;

    (****************************************************************************)

    PROCEDURE HandleOneEntry (updatesublist: BOOLEAN;  flags: PermissionSet);

        (* Sends one line of a directory listing.  If updatesublist is TRUE,    *)
        (* and the entry is a subdirectory, then the subdirectory is added to   *)
        (* sublist.  The node we are handling is described by 'arg', and the    *)
        (* caller has put its directory details into D.                         *)

        VAR SendIt: BOOLEAN;  p: DirEntryPtr;
            subarg: FName;  subentry: LODptr;

        BEGIN
            SendIt := TRUE;  p := arg^.EntryPtr;

            (* Skip over the "." entry, and list the ".." entry only    *)
            (* if options includes ListDotDot.                          *)

            IF D.name[0] = '.' THEN
                SendIt := (D.name[1] <> Nul) AND
                                 ((ListDotDot IN options) OR
                                  (D.name[1] <> '.') OR (D.name[2] <> Nul));
            END (*IF*);

            (* Work out whether we want to send this entry. *)

            IF SendIt THEN
                SendIt := (SystemAndHidden IN options)
                          OR NOT ((hidden IN D.attr) OR (system IN D.attr));
            END (*IF*);

            (* Don't send invisible directories.  Non-directory files are       *)
            (* different: we might encounter this file more than once in our    *)
            (* processing, and so we want to keep the first instance in our     *)
            (* "to send" list -- even though we finally won't send it if it's   *)
            (* invisible -- so that it will take precedence over any future     *)
            (* attempts to put it in the list.                                  *)

            IF SendIt AND (directory IN D.attr) THEN

                (* I'm not sure whether this call to FindDirEntry is redundant  *)
                (* by now, but this works so I'll leave it alone.               *)

                p := FindDirEntry (arg, D.name, TRUE);
                SendIt := (p <> NIL) AND (Visible IN p^.flags);
            END (*IF*);

            (* Queue this item on the list of things to be listed. *)

            SendIt := SendIt AND AddItemToList (head, D, flags);

            (* If we're recursing over subdirectories, and this item is a   *)
            (* directory, add the directory to the list of jobs to be done. *)

            IF SendIt AND updatesublist AND (directory IN D.attr) THEN
                NEW (subarg);
                subarg^ := arg^;
                WITH subarg^ DO
                    Strings.Append (D.name, vdir);
                    Strings.Append ('/', vdir);
                    EntryPtr := p;
                    IF p^.virtual THEN
                        dir := p^.link^.this;
                    ELSE
                        Strings.Append (D.name, dir);
                    END (*IF*);
                    Strings.Append ('/', dir);
                END (*WITH*);
                NEW (subentry);
                subentry^.this := subarg;
                subentry^.next := NIL;
                IF sublist.head = NIL THEN
                    sublist.head := subentry;
                ELSE
                    sublist.tail^.next := subentry;
                END (*IF*);
                sublist.tail := subentry;
            END (*IF*);

            (* If our list has grown too large, send it now rather      *)
            (* than waiting until it's completed.  Anything left over   *)
            (* will start a new list.                                   *)

            IF SendIt THEN
                INC (ListSize);
                IF ListSize >= ListSizeLimit THEN
                    SendTheListing (S, U, KeepAlive, head, options);
                    ListSize := 0;
                END (*IF*);
            END (*IF*);

        END HandleOneEntry;

    (****************************************************************************)

    CONST DummyTime = 0;  DummyDate = 34;  (* 02 Jan 1980 *)

    VAR Name, temp, mask: FileNameString;  q: DirEntryPtr;
        dotdot: ARRAY [0..1] OF CHAR;
        OK: BOOLEAN;  j: CARDINAL;

    BEGIN                       (* body of ListRealDirectory *)
        dotdot := "..";
        ListSize := 0;
        head := NIL;

        (* An empty filename mask means "entire directory", unless the -d       *)
        (* option suppresses this interpretation.                               *)

        IF (arg^.fname[0] = Nul) AND (MayExpand IN options) THEN
            arg^.fname[0] := '*';  arg^.fname[1] := Nul;
        END (*IF*);

        (* Treat the ".." entry separately, to circumvent problems related to   *)
        (* the status of ".." in the root directory of a volume.                *)

        IF (ListDotDot IN options) AND WildMatch (dotdot, arg^.fname) THEN

            q := arg^.EntryPtr^.parent;

            IF (q <> NIL) AND (Visible IN q^.flags) THEN

                WITH D DO
                    dirHandle := 0;
                    attr      := FileAttr{directory};
                    timePkd   := DummyTime;
                    datePkd   := DummyDate;
                    size      := CARD64{0,0};
                    Strings.Assign ("..", name);
                END (*WITH*);
                HandleOneEntry (FALSE, q^.flags);
            END (*IF*);

        END (*IF*);
        EXCL (options, ListDotDot);

        (* Update our directory tree to ensure that it contains all     *)
        (* subdirectories of the current directory that match our mask. *)

        q := arg^.EntryPtr;
        IF (NOT q^.virtual) OR (q^.link^.this[0] <> Nul) THEN
            MakeFullName (arg, Name);
            success := FirstDirEntry (Name, FALSE, TRUE, TRUE, D);
            WHILE success DO
                OK := directory IN D.attr;
                IF OK AND (D.name[0] = '.') THEN
                    OK := (D.name[1] <> Nul) AND
                               ((D.name[1] <> '.') OR (D.name[2] <> Nul));
                END (*IF*);

                IF OK THEN
                    EVAL (FindDirEntry (arg, D.name, TRUE));
                END (*IF*);
                success := NextDirEntry (D);

            END (*WHILE*);
            DirSearchDone (D);
        END (*IF*);

        (* First handle the items that are explicitly listed in our     *)
        (* directory tree, including the directories we've just added.  *)

        q := arg^.EntryPtr;
        IF arg^.fname[0] = Nul THEN
            mask := arg^.vdir;
            j := LENGTH(mask);
            IF (j > 0) AND (mask[j-1] = '/') THEN
                mask[j-1] := Nul;
            END (*IF*);
            SplitTail (mask, temp);
        ELSE
            q := q^.FirstChild;
            mask := arg^.fname;
        END (*IF*);

        WHILE q <> NIL DO

            IF NOT q^.categorised THEN
                CategoriseNode (q, arg^.dir);
            END (*IF*);

            IF q^.exists AND WildMatch(q^.name, mask) THEN

                IF q^.virtual THEN

                    (* This entry describes a link. *)

                    IF (q^.link^.this[0] = Nul) OR
                      ((q^.link^.this[1] = ':') AND (q^.link^.this[2] = Nul)) THEN

                        (* We have a disk root directory or a pseudo-directory.     *)
                        (* Construct a dummy DirectoryEntry record for it.          *)

                        WITH D DO
                            dirHandle := 0;
                            attr      := FileAttr{directory};
                            timePkd   := DummyTime;
                            datePkd   := DummyDate;
                            size      := CARD64{0,0};
                        END (*WITH*);
                        Strings.Assign (q^.name, D.name);
                        HandleOneEntry (Recurse IN options, q^.flags);

                    ELSE

                        IF FirstDirEntry (q^.link^.this, TRUE, FALSE, TRUE, D) THEN
                            Strings.Assign (q^.name, D.name);
                            HandleOneEntry (Recurse IN options, q^.flags);
                        END (*IF*);
                        DirSearchDone (D);

                    END (*IF*);

                ELSE

                    (* Not a link, this is an ordinary directory or file. *)

                    temp := arg^.fname;
                    IF temp[0] <> Nul THEN
                        arg^.fname := q^.name;
                    END (*IF*);
                    MakeFullName (arg, Name);
                    IF temp[0] <> Nul THEN
                        arg^.fname := temp;
                    END (*IF*);

                    IF FirstDirEntry (Name, TRUE, FALSE, TRUE, D) THEN
                        HandleOneEntry (Recurse IN options, q^.flags);
                    END (*IF*);
                    DirSearchDone (D);

                END (*IF*);

            END (*IF*);

            IF arg^.fname[0] = Nul THEN
                q := NIL;
            ELSE
                q := q^.next;
            END (*IF*);

        END (*WHILE*);

        (* Now the main part of the listing.  (Omit this part if we're in a     *)
        (* pseudo-directory.)  We can ignore directories at this stage, because *)
        (* these have already been listed above.                                *)

        q := arg^.EntryPtr;
        IF (NOT q^.virtual) OR (q^.link^.this[0] <> Nul) THEN
            MakeFullName (arg, Name);
            success := FirstDirEntry (Name, FALSE, FALSE, TRUE, D);
            WHILE success DO

                IF NOT (directory IN D.attr) THEN
                    HandleOneEntry (FALSE, q^.flags);
                END (*IF*);
                success := NextDirEntry (D);

            END (*WHILE*);
            DirSearchDone (D);
        END (*IF*);

        (* We've put together the list, now send it to the client. *)

        SendTheListing (S, U, KeepAlive, head, options);

    END ListRealDirectory;

(********************************************************************************)

PROCEDURE ListDirectory (S: Socket;  U: User;  KeepAlive: Semaphore;
                            VAR (*INOUT*) arg: FName;  options: ListingOptions);

    (* Sends a directory listing, as specified by arg and options.  On return   *)
    (* arg has been disposed of.  Parameter arg uses the native character set,  *)
    (* but the file names sent to socket S are in UTF-8 encoding.               *)

    VAR ToDo: ListOfDirectories;  temp: LODptr;
        buffer: ARRAY [0..127] OF CHAR;  pos: CARDINAL;

    BEGIN
        (* Special case: if the root node is invisible, we allow an empty   *)
        (* listing to be returned.                                          *)

        IF (arg^.EntryPtr^.parent = NIL) AND NOT (Visible IN arg^.EntryPtr^.flags) THEN
            EXCL (options, MayExpand);
        END (*IF*);

        NEW (ToDo.head);
        ToDo.tail := ToDo.head;
        ToDo.head^.next := NIL;
        ToDo.head^.this := arg;
        WHILE ToDo.head <> NIL DO
            temp := ToDo.head;
            ToDo.head := temp^.next;
            IF ToDo.head = NIL THEN
                ToDo.tail := NIL;
            END (*IF*);
            arg := temp^.this;
            DISPOSE (temp);
            IF arg^.EntryPtr <> NIL THEN
                IF Recurse IN options THEN

                    (* Send a blank line, followed by a heading for     *)
                    (* this directory.                                  *)

                    buffer := "";  pos := AddEOL(buffer);
                    EVAL (send (S, buffer, pos, 0));
                    TranslateToUTF8 (arg^.vdir, buffer);
                    Strings.Append (':', buffer);
                    pos := AddEOL(buffer);
                    EVAL (send (S, buffer, pos, 0));
                END (*IF*);

                (* Now the actual listing.  This might include adding new       *)
                (* entries to ToDo as a side-effect.                            *)

                ListRealDirectory (S, U, KeepAlive, arg, options, ToDo);

            END (*IF*);
            DISPOSE (arg);

        END (*WHILE*);

    END ListDirectory;

(********************************************************************************)
(*                       CREATING AND DELETING DIRECTORIES                      *)
(********************************************************************************)

PROCEDURE CreateDirectory (Name: FName): BOOLEAN;

    (* Creates a new directory. *)

    VAR FullName: FileNameString;  result: CARDINAL;

    BEGIN
        IF (Name <> NIL) AND Name^.EntryPtr^.IsADir
                              AND (AllowWrite IN Name^.EntryPtr^.flags) THEN
            MakeFullName (Name, FullName);
            IF FullName[0] = Nul THEN
                result := 1;
            ELSE
                CreateDir (FullName, result);
            END (*IF*);
        ELSE
            result := 1;
        END (*IF*);
        RETURN result = 0;
    END CreateDirectory;

(********************************************************************************)

PROCEDURE RemoveDirectory (Name: FName): BOOLEAN;

    (* Deletes a directory. *)

    VAR FullName: FileNameString;  result: CARDINAL;

    BEGIN
        MakeFullName (Name, FullName);
        RmvDir (FullName, result);
        RETURN result = 0;
    END RemoveDirectory;

(********************************************************************************)
(*                             LEGALITY CHECKS                                  *)
(********************************************************************************)

PROCEDURE MayListFiles (iname: FName): BOOLEAN;

    (* Returns TRUE iff the user has permission to see a directory listing of   *)
    (* the file(s) implied by iname.                                            *)

    (* Special case: if iname represents the root node, and it is invisible,    *)
    (* then we allow the listing but arrange that the directory contents will   *)
    (* not be included in the listing.  Note that this also requires a special  *)
    (* check in procedure ListDirectory.                                        *)

    BEGIN
        IF iname = NIL THEN
            RETURN FALSE;
        ELSE
            RETURN (Visible IN iname^.EntryPtr^.flags)
                        OR (iname^.EntryPtr^.parent = NIL);
        END (*IF*);
    END MayListFiles;

(********************************************************************************)

PROCEDURE IsAFile (name: FName;  ShowHidden: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff this is a file (not a directory) that exists.  System   *)
    (* and hidden files will appear to be nonexistent unless ShowHidden is TRUE.*)

    BEGIN
        RETURN (name <> NIL) AND (name^.fname[0] <> Nul)
               AND FileOrDirExists (name, ShowHidden);
    END IsAFile;

(********************************************************************************)

PROCEDURE IsADirectory (name: FName;  ShowHidden: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff this is a directory that exists.  System and hidden     *)
    (* files will appear to be nonexistent unless ShowHidden is TRUE.           *)

    BEGIN
        RETURN (name <> NIL) AND (name^.fname[0] = Nul)
               AND FileOrDirExists (name, ShowHidden);
    END IsADirectory;

(********************************************************************************)

PROCEDURE CanReadFile (U: User;  Name: FName;  Manager: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the file exists and is readable by this user.   *)

    BEGIN
        RETURN (Name <> NIL) AND (Name^.fname[0] <> Nul)
               AND FileOrDirExists (Name, Manager)
               AND (AllowRead IN Name^.EntryPtr^.flags)
               AND NOT InHideList(U, Name^.fname);
    END CanReadFile;

(********************************************************************************)

PROCEDURE CanWriteFile (U: User;  Name: FName;  Manager, CheckExists: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff we can create a file of this name.  If CheckExists is   *)
    (* TRUE and the file already exists, we must also have delete permission    *)
    (* for it.                                                                  *)

    VAR perm: PermissionSet;  result: BOOLEAN;

    BEGIN
        IF (Name = NIL) OR (Name^.EntryPtr = NIL)
                        OR InHideList(U, Name^.fname) THEN
            RETURN FALSE;
        END (*IF*);
        perm := Name^.EntryPtr^.flags;
        result := AllowWrite IN perm;
        IF result AND CheckExists AND FileOrDirExists (Name, Manager) THEN
            result := AllowDelete IN perm;
        END (*IF*);
        RETURN result;
    END CanWriteFile;

(********************************************************************************)

PROCEDURE CanCreateDirectory (U: User;  Name: FName): BOOLEAN;

    (* Returns TRUE iff user can create a directory with this name.  *)

    BEGIN
        RETURN CanWriteFile (U, Name, TRUE, TRUE);
    END CanCreateDirectory;

(********************************************************************************)

PROCEDURE CanDeleteFile (U: User;  Name: FName;  Manager: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the file exists and is readable by this user.   *)

    BEGIN
        RETURN FileOrDirExists (Name, Manager)
                    AND (Name <> NIL) AND (Name^.EntryPtr <> NIL)
                       AND (AllowDelete IN Name^.EntryPtr^.flags)
                          AND NOT InHideList(U, Name^.fname);
    END CanDeleteFile;

(********************************************************************************)

PROCEDURE DirectoryHasPermission (Name: FName;  perm: Permission): BOOLEAN;

    (* Returns TRUE iff Name describes a directory, and the parent of that      *)
    (* directory has the permission 'perm'.                                     *)

    VAR pos: DirEntryPtr;

    BEGIN
        IF (Name <> NIL) AND (Name^.fname[0] = Nul) THEN
            pos := Name^.EntryPtr;
        ELSE
            pos := NIL;
        END (*IF*);

        (* The permission we're looking for is actually in the directory        *)
        (* record of the parent of this directory.                              *)

        IF pos <> NIL THEN
            pos := pos^.parent;
        END (*IF*);

        RETURN (pos <> NIL) AND (perm IN pos^.flags);

    END DirectoryHasPermission;

(********************************************************************************)

PROCEDURE CanDeleteDirectory (U: User;  Name: FName): BOOLEAN;

    (* Returns TRUE iff user can delete directory DirName.  *)

    BEGIN
        RETURN DirectoryHasPermission (Name, AllowDelete)
                         AND NOT InHideList(U, Name^.fname);
    END CanDeleteDirectory;

(********************************************************************************)

PROCEDURE CanSeeFileOrDir (U: User;  Name: FName;  Manager: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the file or directory exists and is visible to this user.*)

    BEGIN
        IF (Name = NIL) OR (Name^.EntryPtr = NIL)
                    OR NOT FileOrDirExists (Name, Manager) THEN
            RETURN FALSE;
        ELSIF Name^.fname[0] = Nul THEN
            (* Directory *)
            IF Name^.EntryPtr^.parent = NIL THEN
                RETURN FALSE;
            ELSE
                RETURN (Visible IN Name^.EntryPtr^.parent^.flags)
                              AND NOT InHideList(U, Name^.EntryPtr^.name);
            END (*IF*);
        ELSE
            RETURN (Visible IN Name^.EntryPtr^.flags)
                              AND NOT InHideList(U, Name^.fname);
        END (*IF*);
    END CanSeeFileOrDir;

(********************************************************************************)

PROCEDURE RenameIsLegal (U: User;  Name: FName;  Manager: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff we may rename this file or directory.  *)

    VAR pos: DirEntryPtr;

    BEGIN
        IF Name = NIL THEN
            pos := NIL;
        ELSE
            pos := Name^.EntryPtr;

            IF InHideList(U, Name^.fname) THEN pos := NIL;

            (* If this is a directory, we want the permissions of the   *)
            (* parent node.                                             *)

            ELSIF (Name^.fname[0] = Nul) AND (pos <> NIL) THEN
                pos := pos^.parent;
            END (*IF*);

        END (*IF*);

        RETURN (pos <> NIL) AND FileOrDirExists (Name, Manager)
                            AND (AllowRename IN pos^.flags);

    END RenameIsLegal;

(********************************************************************************)
(*                          FILE DATE, TIME, AND SIZE                           *)
(********************************************************************************)

PROCEDURE GetFileSize (Name: FName;  trusted: BOOLEAN): CARD64;

    (* Returns the size in bytes of file "Name".  If the file is not    *)
    (* accessible, the result is returned as MAX(CARD64).  Exception:   *)
    (* if trusted=TRUE then we return the correct size even if it's an  *)
    (* invisible file.                                                  *)

    VAR D: DirectoryEntry;  FullName: FileNameString;  result: CARD64;

    BEGIN
        IF trusted OR MayListFiles(Name) THEN
            MakeFullName (Name, FullName);
            IF FirstDirEntry (FullName, FALSE, FALSE, TRUE, D) THEN
                result := D.size;
            ELSE
                result.low  := MAX(CARDINAL);
                result.high := MAX(CARDINAL);
            END (*IF*);
            DirSearchDone (D);
        ELSE
            result.low  := MAX(CARDINAL);
            result.high := MAX(CARDINAL);
        END (*IF*);
        RETURN result;
    END GetFileSize;

(********************************************************************************)

PROCEDURE SetDateTime (iname: FName;  datetime: ARRAY OF CARDINAL);

    (* Sets the modified/accessed/created timestamps for the file,      *)
    (* from an array of 6 numbers already encoded into the format the   *)
    (* filesystem uses for timestamps.                                  *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        OpenMode = OS2.OPEN_FLAGS_NOINHERIT + OS2.OPEN_ACCESS_READWRITE
                              + OS2.OPEN_SHARE_DENYNONE;

    VAR p: OS2.PFILESTATUS3;  hf: OS2.HFILE;  result: CARDINAL;
        FullName: FileNameString;

    BEGIN
        MakeFullName (iname, FullName);
        OS2.DosOpen (FullName, hf, result, 0, 0, OpenFlags, OpenMode, NIL);
        NEW (p);
        OS2.DosQueryFileInfo (hf, OS2.FIL_STANDARD,
                              p, SIZE(OS2.FILESTATUS3));
        WITH p^ DO
            fdateCreation   := datetime[4];
            ftimeCreation   := datetime[5];
            fdateLastAccess := datetime[2];
            ftimeLastAccess := datetime[3];
            fdateLastWrite  := datetime[0];
            ftimeLastWrite  := datetime[1];
        END (*WITH*);
        result := OS2.DosSetFileInfo (hf, OS2.FIL_STANDARD,
                              p, SIZE(OS2.FILESTATUS3));
        DISPOSE (p);
        OS2.DosClose (hf);
    END SetDateTime;

(********************************************************************************)

PROCEDURE GetDateTime (iname: FName;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Returns the date/time of the file's directory entry, in a string of the  *)
    (* form "yyyymmddhhmmss" (exactly 14 characters).  If the file is not       *)
    (* accessible, the result is the null string.                               *)

    VAR place: CARDINAL;

    (****************************************************************************)

    PROCEDURE Encode1 (value: CARDINAL);

        BEGIN
            result[place] := CHR(ORD('0') + value);  INC (place);
        END Encode1;

    (****************************************************************************)

    PROCEDURE Encode (value: CARDINAL);

        BEGIN
            Encode1 (value DIV 10);  Encode1 (value MOD 10);
        END Encode;

    (****************************************************************************)

    VAR D: DirectoryEntry;  FullName: FileNameString;
        date: SysClock.DateTime;  val: CARDINAL;

    BEGIN
        place := 0;
        IF MayListFiles(iname) THEN
            MakeFullName (iname, FullName);
            IF FirstDirEntry (FullName, FALSE, FALSE, TRUE, D) THEN
                WITH date DO
                    year := D.datePkd;
                    day := year MOD 32;  year := year DIV 32;
                    month := year MOD 16;
                    year := 1980 + year DIV 16;
                    val := D.timePkd;
                    second := 2*(val MOD 32);  val := val DIV 32;
                    minute := val MOD 64;  hour := val DIV 64;
                    fractions := 0;
                    zone := 0;
                    SummerTimeFlag := FALSE;
                END (*WITH*);
                CorrectToGMT (date);
                WITH date DO
                    Encode (year DIV 100);  Encode (year MOD 100);
                    Encode (month);  Encode (day);
                    Encode (hour);  Encode (minute);  Encode (second);
                END (*WITH*);
            END (*IF*);
            DirSearchDone (D);
        END (*IF*);
        IF place <= HIGH(result) THEN
            result[place] := Nul;
        END (*IF*);
    END GetDateTime;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

BEGIN
    UserCount := NIL;
    CreateLock (UserCountLock);
END FDUsers.

