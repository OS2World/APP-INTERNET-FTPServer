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

IMPLEMENTATION MODULE Tree;

        (************************************************************)
        (*                                                          *)
        (*                  PM Setup for FtpServer                  *)
        (*          Dialogue to edit a file permission tree         *)
        (*                                                          *)
        (*    Started:        4 November 1999                       *)
        (*    Last edited:    22 May 2017                           *)
        (*    Status:         Working                               *)
        (*                                                          *)
        (*   Faults:                                                *)
        (*     - should overview be                                 *)
        (*       a different format?  I'm not sure.                 *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM, OS2, Remote, DID, Nodes, Strings;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer;

FROM RINIData IMPORT
    (* proc *)  INIFetchBinary, ItemSize, INIPutBinary, INIDeleteKey;

FROM Directories IMPORT
    (* type *)  FileNameSubscript, FileName, DirectorySearchHandle,
    (* proc *)  FileCheck, FirstSubdirectory, NextSubdirectory,
                SubdirSearchDone;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    IndentIncrement = 2;

TYPE
    CharSet = SET OF CHAR;

    Permission = (Visible, AllowRead, AllowWrite, AllowDelete, AllowRename);
    PermissionSet = SET OF Permission;

    DirEntryPtr = POINTER TO DirEntry;

    (* Explanation of some of the fields in a DirEntry record:                  *)
    (*                                                                          *)
    (*   collapsed     the subtrees of this entry are not displayed             *)
    (*   expanded      all subdirectories of this entry are already present as  *)
    (*                 children of this node (i.e. we've checked the disk)      *)
    (*   exists        this entry corresponds to something that exists in the   *)
    (*                 file system                                              *)
    (*   IsAFile       this file is not a directory                             *)
    (*   vpath         virtual path, i.e. path as seen by the ftp user          *)

    DirEntry = RECORD
                   indent: CARDINAL;
                   parent, FirstChild, LastChild, next: DirEntryPtr;
                   collapsed, expanded: BOOLEAN;
                   flags: PermissionSet;
                   exists, IsAFile: BOOLEAN;
                   vpath: FileName;
                   nodeinfo: Nodes.NodePtr;
               END (*RECORD*);

VAR hwndtop: OS2.HWND;

(**************************************************************************)
(*                          MANIPULATING THE TREE                         *)
(**************************************************************************)

PROCEDURE DeleteLeafNode (VAR (*INOUT*) p: DirEntryPtr);

    (* Removes a node from the tree, adjusts links in remaining nodes   *)
    (* as needed.  Assumption: this node has no children.               *)

    VAR father, brother, current: DirEntryPtr;

    BEGIN
        father := p^.parent;

        IF father <> NIL THEN

            (* Find the left brother of this node. *)

            brother := NIL;
            current := father^.FirstChild;
            WHILE current <> p DO
                brother := current;  current := current^.next;
            END (*WHILE*);

            (* Remove this node from the tree. *)

            IF brother = NIL THEN
                father^.FirstChild := p^.next;
            ELSE
                brother^.next := p^.next;
            END (*IF*);
            IF father^.LastChild = p THEN
                father^.LastChild := brother;
            END (*IF*);

        END (*IF*);

        (* Discard this node. *)

        Nodes.DiscardNode (p^.nodeinfo);
        DISPOSE (p);

    END DeleteLeafNode;

(********************************************************************************)

PROCEDURE RemoveChildren (p: DirEntryPtr);

    (* Deletes all children of p from the tree. *)

    VAR child, temp: DirEntryPtr;

    BEGIN
        child := p^.FirstChild;
        WHILE child <> NIL DO
            RemoveChildren (child);
            temp := child;  child := child^.next;
            Nodes.DiscardNode (temp^.nodeinfo);
            DISPOSE (temp);
        END (*WHILE*);
        p^.FirstChild := NIL;  p^.LastChild := NIL;
        p^.expanded := FALSE;
    END RemoveChildren;

(********************************************************************************)

PROCEDURE DeleteTree (VAR (*INOUT*) p: DirEntryPtr);

    (* Removes an entire tree, including cleaning up the containing tree if     *)
    (* this happens to be a subtree of another tree.                            *)

    BEGIN
        IF p <> NIL THEN
            RemoveChildren (p);
            DeleteLeafNode (p);
        END (*IF*);
    END DeleteTree;

(********************************************************************************)

PROCEDURE PruneTree (p, root: DirEntryPtr;  defaults: PermissionSet);

    (* Removes the tree nodes that aren't necessary because their permissions   *)
    (* are directly inherited from their parents.  Note: virtual nodes must     *)
    (* never be pruned, or we would lose the link information.                  *)

    VAR current, next: DirEntryPtr;

    BEGIN
        current := p;
        WHILE current <> NIL DO
            next := current^.next;
            current^.collapsed := FALSE;
            PruneTree (current^.FirstChild, root, current^.flags);
            current^.expanded := FALSE;
            IF (current^.FirstChild = NIL) AND (current^.flags = defaults)
                      AND (current^.parent <> NIL)
                      AND NOT Nodes.IsVirtual(current^.nodeinfo) THEN
                DeleteLeafNode (current);
            END (*IF*);
            current := next;
        END (*WHILE*);
    END PruneTree;

(********************************************************************************)

PROCEDURE CopyDownPermissions (thisnode: DirEntryPtr);

    (* All the children of thisnode inherit its permissions. *)

    VAR current: DirEntryPtr;

    BEGIN
        current := thisnode^.FirstChild;
        WHILE current <> NIL DO
            current^.flags := thisnode^.flags;
            CopyDownPermissions (current);
            current := current^.next;
        END (*WHILE*);
    END CopyDownPermissions;

(********************************************************************************)

PROCEDURE CategoriseNode (VAR (*INOUT*) p: DirEntryPtr);

    (* Checks whether p^ describes a file, a directory, or something that       *)
    (* doesn't exist on the disk.  (A pseudo-directory is considered to be a    *)
    (* directory that exists.)                                                  *)

    VAR fullname: FileName;

    BEGIN
        Nodes.QueryPhysicalPath (p^.nodeinfo, fullname);
        IF fullname[0] = Nul THEN

            (* This is a pseudo-directory. *)

            p^.exists := TRUE;  p^.IsAFile := FALSE;

        ELSE
            (* We have a real name, but we still have to check three    *)
            (* possibilities: this is a file, this is a directory, this *)
            (* is something that doesn't exist on our disk.             *)

            FileCheck (fullname, p^.exists, p^.IsAFile);

        END (*IF*);

    END CategoriseNode;

(********************************************************************************)

PROCEDURE RemoveObsoleteNodes (p: DirEntryPtr);

    (* Called when p^.ppath might have changed.  We delete non-virtual children *)
    (* and their subtrees, but leave the virtual children intact.               *)

    VAR child, next: DirEntryPtr;

    BEGIN
        CategoriseNode (p);
        next := p^.FirstChild;
        WHILE next <> NIL DO
            child := next;  next := child^.next;
            IF NOT Nodes.IsVirtual(child^.nodeinfo) THEN
                DeleteTree (child);
            END (*IF*);
        END (*WHILE*);
        p^.expanded := FALSE;
        p^.collapsed := TRUE;
    END RemoveObsoleteNodes;

(********************************************************************************)

PROCEDURE CleanUpTree (VAR (*INOUT*) root: DirEntryPtr);

    (* Removes the tree nodes that aren't necessary because their permissions   *)
    (* are directly inherited from their parents.                               *)

    BEGIN
        PruneTree (root, root, PermissionSet {Visible, AllowRead} );
    END CleanUpTree;

(********************************************************************************)

PROCEDURE AppendChild (father: DirEntryPtr;  newname: ARRAY OF CHAR;
                                         FileExists, IsADirectory: BOOLEAN);

    (* Creates a new node and adds it the the children of father.        *)

    VAR q, brother: DirEntryPtr;

    BEGIN
        NEW (q);
        brother := father^.LastChild;  father^.LastChild := q;
        IF brother = NIL THEN
            father^.FirstChild := q;
        ELSE
            brother^.next := q;
        END (*IF*);

        WITH q^ DO
            indent := father^.indent + IndentIncrement;
            parent := father;
            FirstChild := NIL;  LastChild := NIL;
            next := NIL;
            flags := father^.flags;
            Strings.Assign (father^.vpath, vpath);
            Strings.Append ('/', vpath);
            Strings.Append (newname, vpath);
            exists := FileExists;
            IsAFile := NOT IsADirectory;
            nodeinfo := Nodes.CreateNode (newname, father^.nodeinfo);
            collapsed := TRUE;
            expanded := FALSE;
        END (*WITH*);

    END AppendChild;

(********************************************************************************)

PROCEDURE CreateDefaultRoot(): DirEntryPtr;

    (* Creates a node with default values for the root. *)

    VAR result: DirEntryPtr;

    BEGIN
        NEW (result);
        WITH result^ DO
            indent := 0;
            parent := NIL;  FirstChild := NIL;  LastChild := NIL;  next := NIL;
            collapsed := FALSE;  expanded := FALSE;
            flags := PermissionSet {Visible, AllowRead};
            exists := TRUE;  IsAFile := FALSE;
            vpath := "";
            nodeinfo := Nodes.CreateNode ("", NIL);
            Nodes.MakeVirtual (nodeinfo, "");
        END (*WITH*);
        RETURN result;
    END CreateDefaultRoot;

(************************************************************************)
(*         ENCODING THE VOLUME INFORMATION AS AN ASCII STRING           *)
(************************************************************************)

PROCEDURE AppendString (string: ARRAY OF CHAR;
                                       VAR (*INOUT*) result: ARRAY OF CHAR;
                                       VAR (*INOUT*) index: CARDINAL);

    (* Puts string into the result array, starting at result[index].    *)
    (* On return index has been updated to the next unused array slot.  *)

    VAR j: CARDINAL;

    BEGIN
        IF LENGTH(string) > 0 THEN
            FOR j := 0 TO LENGTH(string)-1 DO
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

PROCEDURE StoreTreeData (D: DirEntryPtr;  DefaultFlags: PermissionSet;
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
                StoreTreeData (child, D^.flags, result, index);
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

PROCEDURE StoreTreeData (D: DirEntryPtr;  DefaultFlags: PermissionSet;
                              VAR (*INOUT*) result: ARRAY OF CHAR;
                              VAR (*INOUT*) index: CARDINAL);

    (* Stores the information, for the directory tree whose root is D,  *)
    (* into the result array starting at result[index].                 *)
    (* On return index has been updated to the next unused array slot.  *)

    VAR buffer: FileName;

    BEGIN
        Nodes.NameOf (D^.nodeinfo, buffer);
        AppendQuotedString (buffer, result, index);
        IF Nodes.IsVirtual(D^.nodeinfo) THEN
            result[index] := '=';  INC (index);
            Nodes.QueryPhysicalPath (D^.nodeinfo, buffer);
            AppendQuotedString (buffer, result, index);
        END (*IF*);
        StorePermAndSubdir (D, DefaultFlags, result, index);
    END StoreTreeData;

(************************************************************************)

PROCEDURE SizeOfTreeData (D: DirEntryPtr;
                               DefaultFlags: PermissionSet): CARDINAL;

    (* Returns the number of characters needed to store the INI file    *)
    (* data for this directory.                                         *)

    VAR result: CARDINAL;
        buffer: FileName;

    BEGIN
        Nodes.NameOf (D^.nodeinfo, buffer);
        result := LENGTH(buffer) + 2 + SizeOfPermAndSubdir (D, DefaultFlags);
        IF Nodes.IsVirtual(D^.nodeinfo) THEN
            Nodes.QueryPhysicalPath (D^.nodeinfo, buffer);
            INC (result, 3 + LENGTH(buffer));
        END (*IF*);
        RETURN result;
    END SizeOfTreeData;

(************************************************************************)

PROCEDURE SizeOfDirectoryData (D: DirEntryPtr): CARDINAL;

    (* Returns the number of characters needed to store the INI file    *)
    (* data for this directory.                                         *)

    BEGIN
        IF D = NIL THEN
            RETURN 0;
        ELSE
            RETURN SizeOfTreeData (D, PermissionSet {Visible, AllowRead});
        END (*IF*);
    END SizeOfDirectoryData;

(************************************************************************)

PROCEDURE StoreDirectoryData (D: DirEntryPtr;
                                  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Stores the information, for the directory tree whose root is D,  *)
    (* into the result array starting at result[0].                     *)

    VAR index: CARDINAL;

    BEGIN
        index := 0;
        IF D <> NIL THEN
            StoreTreeData (D, PermissionSet {Visible, AllowRead},
                                                 result, index);
        END (*IF*);
    END StoreDirectoryData;

(************************************************************************)
(*               SAVING THE PERMISSIONS TO THE INI FILE                 *)
(************************************************************************)

PROCEDURE SavePermissions (username: ARRAY OF CHAR;  root: DirEntryPtr);

    (* Writes the tree back to the INI file.  We assume that the caller *)
    (* has already opened the INI file.                                 *)

    VAR size: CARDINAL;
        bufptr: CharArrayPointer;

    BEGIN
        CleanUpTree (root);
        size := SizeOfDirectoryData (root);
        IF size = 0 THEN
            INIDeleteKey (username, "Volume");
        ELSE
            ALLOCATE (bufptr, size);
            StoreDirectoryData (root, bufptr^);
            INIPutBinary (username, "Volume", bufptr^, size);
            DEALLOCATE (bufptr, size);
        END (*IF*);
    END SavePermissions;

(**************************************************************************)
(*         LOADING AND PARSING THE PERMISSIONS FROM THE INI FILE          *)
(**************************************************************************)

PROCEDURE Scan (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                                            BufferSize: CARDINAL): CHAR;

    (* Returns the character at bufptr^[position], and updates position. *)

    VAR NextChar: CHAR;

    BEGIN
        IF position >= BufferSize THEN
            NextChar := Nul;
        ELSE
            NextChar := bufptr^[position];  INC(position);
        END (*IF*);
        RETURN NextChar;
    END Scan;

(****************************************************************************)

PROCEDURE LoadString (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                           VAR (*INOUT*) NextChar: CHAR;  Stoppers: CharSet);

    (* Reads a string up to but not including a character in Stoppers.      *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE NOT (NextChar IN Stoppers) AND (j <= HIGH(result)) DO
            result[j] := NextChar;  INC(j);
            NextChar := Scan (bufptr, position, BufferSize);
        END (*WHILE*);
        IF j <= HIGH(result) THEN result[j] := Nul END(*IF*);
    END LoadString;

(****************************************************************************)

PROCEDURE NameString (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                            VAR (*INOUT*) NextChar: CHAR;  Stoppers: CharSet);

    (*     <namestring> ->  <string1> | " <string2> " | ' <string3> '       *)
    (*     <string1>   -> any string not including space char               *)
    (*     <string2>   -> any string not including double quote             *)
    (*     <string3>   -> any string not including single quote             *)
    (* The strings in <string1> also may not contain characters in Stoppers.*)

    CONST Space = ' ';  SingleQuote = "'";  DoubleQuote = '"';

    VAR Delimiter: CHAR;

    BEGIN
        IF (NextChar = SingleQuote) OR (NextChar = DoubleQuote) THEN
            Delimiter := NextChar;
            Stoppers := CharSet {Nul, Delimiter};
            NextChar := Scan (bufptr, position, BufferSize);
            LoadString (bufptr, position, BufferSize, result, NextChar, Stoppers);
            IF NextChar = Delimiter THEN
                NextChar := Scan (bufptr, position, BufferSize);
            END (*IF*);
        ELSE
            INCL (Stoppers, Space);
            LoadString (bufptr, position, BufferSize, result, NextChar, Stoppers);
        END (*IF*);
    END NameString;

(****************************************************************************)

PROCEDURE DirItem (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                      VAR (*INOUT*) NextChar: CHAR);  FORWARD;

(****************************************************************************)

PROCEDURE DirList (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                   VAR (*INOUT*) NextChar: CHAR);

    (*     <dirlist>   ->  <diritem> { , <diritem> }*       *)
    (* Result returned: a linked list of directory nodes.   *)

    CONST Comma = ",";

    BEGIN
        DirItem (bufptr, position, BufferSize, mother, NextChar);
        WHILE (NextChar = Comma) OR (NextChar = ';') DO
            NextChar := Scan (bufptr, position, BufferSize);
            DirItem (bufptr, position, BufferSize, mother, NextChar);
        END (*WHILE*);
    END DirList;

(****************************************************************************)

PROCEDURE DirRule (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  pnode: DirEntryPtr;
                            VAR (*INOUT*) NextChar: CHAR);

    (* Fills in the permissions and subdirectory info for pnode.  *)
    (*     <dirrule>   -> { <permission> }* { ( <dirlist> ) }     *)
    (*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+ *)

    VAR option: Permission;

    BEGIN
        (* Check for an anomalous case (should not happen, but could *)
        (* occur during debugging).                                  *)

        IF pnode = NIL THEN
            RETURN;
        END (*IF*);

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
            NextChar := Scan (bufptr, position, BufferSize);
            IF NextChar = '-' THEN
                NextChar := Scan (bufptr, position, BufferSize);
                EXCL (pnode^.flags, option);
            ELSIF NextChar = '+' THEN
                NextChar := Scan (bufptr, position, BufferSize);
                INCL (pnode^.flags, option);
            END (*IF*);
        END (*WHILE*);

        (* Look for optional list of subdirectories. *)

        IF NextChar = '(' THEN
            pnode^.collapsed := FALSE;
            NextChar := Scan (bufptr, position, BufferSize);
            DirList (bufptr, position, BufferSize, pnode, NextChar);
            IF NextChar = ')' THEN
                NextChar := Scan (bufptr, position, BufferSize);
            END (*IF*);
        END (*IF*);

    END DirRule;

(****************************************************************************)

PROCEDURE DirName (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                VAR (*INOUT*) NextChar: CHAR): DirEntryPtr;

    (*     <dirname>    ->  <namestring>  |  <namestring> = <namestring>    *)
    (* The alternative with the '=' sign means that we have to create       *)
    (* a symbolic link.                                                     *)

    VAR result: DirEntryPtr;  k: CARDINAL;  NewName: FileName;

    BEGIN
        NameString (bufptr, position, BufferSize, NewName, NextChar,
                                                   CharSet{'=', '(', ','});
        AppendChild (mother, NewName, FALSE, FALSE);
        result := mother^.LastChild;

        IF NextChar = '=' THEN
            NextChar := Scan (bufptr, position, BufferSize);
            NameString (bufptr, position, BufferSize, NewName, NextChar,
                                                       CharSet{'(', ','});

            (* Transition arrangement: get rid of the trailing '/'.  *)

            k := LENGTH (NewName);
            IF k > 0 THEN
                DEC (k);
                IF (NewName[k] = '/') OR (NewName[k] = '\') THEN
                    NewName[k] := Nul;
                END (*IF*);
            END (*IF*);

            Nodes.MakeVirtual (result^.nodeinfo, NewName);

        END (*IF*);

        RETURN result;

    END DirName;

(****************************************************************************)

PROCEDURE DirItem (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                                VAR (*INOUT*) NextChar: CHAR);

    (*     <diritem>   -> <dirname> <dirrule>                            *)

    VAR result: DirEntryPtr;

    BEGIN
        result := DirName (bufptr, position, BufferSize, mother, NextChar);
        DirRule (bufptr, position, BufferSize, result, NextChar);
    END DirItem;

(****************************************************************************)

PROCEDURE RecomputeVPath (root: DirEntryPtr);

    (* The caller must have set root^.vpath and root^.indent correctly.         *)
    (* This procedure sets the vpath and indent fields for all descendant       *)
    (* nodes.  (We need to do this only in the case where root^.nodeinfo^.vpath *)
    (* has changed for some reason.)                                            *)

    VAR current: DirEntryPtr;  buffer: FileName;

    BEGIN
        current := root^.FirstChild;
        WHILE current <> NIL DO
            current^.indent := root^.indent + IndentIncrement;
            current^.vpath := root^.vpath;
            Strings.Append ('/', current^.vpath);
            Nodes.NameOf (current^.nodeinfo, buffer);
            Strings.Append (buffer, current^.vpath);
            RecomputeVPath (current);
            current := current^.next;
        END (*WHILE*);
    END RecomputeVPath;

(********************************************************************************)

PROCEDURE LoadTreeData (bufptr: CharArrayPointer; BufferSize: CARDINAL): DirEntryPtr;

    (* Reads a representation of the directory tree from bufptr^, starting *)
    (* at bufptr^[0], and constructs the permissions tree.                 *)

    VAR root, temp: DirEntryPtr;  position: CARDINAL;  NextChar: CHAR;

    BEGIN
        (* Give the user a root directory. *)

        root := CreateDefaultRoot();

        (* Load the volume information. *)

        position := 0;
        NextChar := Scan (bufptr, position, BufferSize);
        IF NextChar <> Nul THEN
            DirList (bufptr, position, BufferSize,
                                         root, NextChar);
        END (*IF*);

        (* Decide whether we really need a pseudo-directory as     *)
        (* the root directory.                                     *)

        IF root^.FirstChild <> NIL THEN
            IF root^.FirstChild^.next = NIL THEN
                temp := root;
                root := root^.FirstChild;
                Nodes.DiscardNode (temp^.nodeinfo);
                DISPOSE (temp);
                Nodes.DetachFromParent (root^.nodeinfo);
                root^.indent := 0;
                root^.parent := NIL;
                root^.vpath := "";
                RecomputeVPath (root);
            END (*IF*);
        END (*IF*);

        RETURN root;

    END LoadTreeData;

(********************************************************************************)
(*                           STRING COMPARISON                                  *)
(********************************************************************************)

PROCEDURE NameMatch (first, second: ARRAY OF CHAR): BOOLEAN;

    (* Equality test, modulo upper/lower case differences.  That is, the result *)
    (* is TRUE even if the letters in first and second don't have the same      *)
    (* case, as long as the strings are otherwise equal.                        *)
    (* In addition, we treat '/' and '\' as matching characters.                *)

    VAR k: CARDINAL;

    BEGIN
        k := 0;
        LOOP
            IF k > HIGH(first) THEN RETURN (k > HIGH(second));
            ELSIF first[k] = Nul THEN RETURN (second[k] = Nul);
            ELSIF (first[k] = '/') AND (second[k] = '\') THEN INC(k);
            ELSIF (first[k] = '\') AND (second[k] = '/') THEN INC(k);
            ELSIF CAP(first[k]) <> CAP(second[k]) THEN RETURN FALSE;
            ELSE INC(k);
            END (*IF*);
        END (*LOOP*);
    END NameMatch;

(************************************************************************)
(*                        DISPLAYING THE TREE                           *)
(************************************************************************)

PROCEDURE PermissionsToString (flags: PermissionSet;
                                 VAR (*OUT*) result: ARRAY OF CHAR);

    (* Turns the flags into a five-character string.  We assume that result is  *)
    (* at least six characters long, and we don't add a string terminator.      *)

    BEGIN
        result[0] := ' ';
        IF Visible IN flags THEN result[1] := 'V' ELSE result[1] := ' ' END(*IF*);
        IF AllowRead IN flags THEN result[2] := 'R' ELSE result[2] := ' ' END(*IF*);
        IF AllowWrite IN flags THEN result[3] := 'W' ELSE result[3] := ' ' END(*IF*);
        IF AllowDelete IN flags THEN result[4] := 'D' ELSE result[4] := ' ' END(*IF*);
        IF AllowRename IN flags THEN result[5] := 'N' ELSE result[5] := ' ' END(*IF*);
    END PermissionsToString;

(********************************************************************************)

PROCEDURE DirNameToText (p: DirEntryPtr;  VAR (*OUT*) Entry: ARRAY OF CHAR);

    (* Creates a text string containing the permissions, collapsed marker, and  *)
    (* directory name for this node.                                            *)

    VAR j: CARDINAL;  buffer: FileName;

    BEGIN
        PermissionsToString (p^.flags, Entry);
        FOR j := 6 TO p^.indent+8 DO
            IF j <= HIGH(Entry) THEN
                Entry[j] := ' ';
            END (*IF*);
        END (*FOR*);
        IF p^.IsAFile THEN
            Entry[7] := '#';
        ELSIF NOT p^.exists THEN
            Entry[7] := '?';
        ELSIF p^.collapsed THEN
            Entry[7] := '+';
        END (*IF*);
        IF Nodes.IsVirtual(p^.nodeinfo) THEN
            Entry[8] := '*';
        END (*IF*);
        IF p^.indent+9 <= HIGH(Entry) THEN
            Entry[p^.indent+9] := Nul;
        END (*IF*);
        IF p^.parent = NIL THEN
            buffer := "/";
        ELSE
            Nodes.NameOf (p^.nodeinfo, buffer);
        END (*IF*);
        Strings.Append (buffer, Entry);

    END DirNameToText;

(************************************************************************)

PROCEDURE DisplayTree (hwnd: OS2.HWND;  VAR (*INOUT*) index: CARDINAL;
                                     node: DirEntryPtr);         FORWARD;

(************************************************************************)

PROCEDURE ShowChildren (hwnd: OS2.HWND;  VAR (*INOUT*)  index: CARDINAL;
                                     node: DirEntryPtr);

    VAR p: DirEntryPtr;

    BEGIN
        p := node^.FirstChild;
        WHILE p <> NIL DO
            DisplayTree (hwnd, index, p);
            p := p^.next;
        END (*WHILE*);
    END ShowChildren;

(************************************************************************)

PROCEDURE DisplayNode (hwnd: OS2.HWND;  index: CARDINAL;  node: DirEntryPtr);

    VAR buffer: ARRAY [0..511] OF CHAR;

    (* Inserts a line for this node into the listbox window hwnd. *)

    BEGIN
        DirNameToText (node, buffer);
        OS2.WinSendMsg (hwnd, OS2.LM_INSERTITEM,
                              OS2.MPFROMSHORT(index), SYSTEM.ADR(buffer));
        OS2.WinSendMsg (hwnd, OS2.LM_SETITEMHANDLE,
                              OS2.MPFROMSHORT(index), node);
    END DisplayNode;

(************************************************************************)

PROCEDURE DisplayTree (hwnd: OS2.HWND;  VAR (*INOUT*) index: CARDINAL;
                                     node: DirEntryPtr);

    (* Inserts listbox entries for the tree whose root is 'node'.  On   *)
    (* return, index is one step beyond the last node we displayed.     *)

    BEGIN
        DisplayNode (hwnd, index, node);  INC(index);
        IF NOT node^.collapsed THEN
            ShowChildren (hwnd, index, node);
        END (*IF*);
    END DisplayTree;

(************************************************************************)

PROCEDURE InitialDisplay (hwnd: OS2.HWND;  root: DirEntryPtr);

    (* Displays the tree whose root is given in a listbox window. *)

    VAR buffer: ARRAY [0..31] OF CHAR;  index: CARDINAL;

    BEGIN
        OS2.WinSendMsg (hwnd, OS2.LM_DELETEALL, NIL, NIL);
        IF root = NIL THEN
            buffer := "<No directories set>";
            OS2.WinSendMsg (hwnd, OS2.LM_INSERTITEM,
                              OS2.MPFROMSHORT(0), SYSTEM.ADR(buffer));
        ELSE
            index := 0;
            DisplayTree (hwnd, index, root);
        END (*IF*);
    END InitialDisplay;

(************************************************************************)

PROCEDURE RedisplayTree (hwnd: OS2.HWND;  VAR (*INOUT*) index: SYSTEM.INT16;
                                     node: DirEntryPtr);         FORWARD;

(************************************************************************)

PROCEDURE ReshowChildren (hwnd: OS2.HWND;  VAR (*INOUT*) index: SYSTEM.INT16;
                                     node: DirEntryPtr);

    VAR p: DirEntryPtr;

    BEGIN
        p := node^.FirstChild;
        WHILE p <> NIL DO
            RedisplayTree (hwnd, index, p);
            p := p^.next;
        END (*WHILE*);
    END ReshowChildren;

(************************************************************************)

PROCEDURE RedisplayNode (hwnd: OS2.HWND;  index: CARDINAL;  node: DirEntryPtr);

    VAR buffer: ARRAY [0..511] OF CHAR;

    (* Refreshes the line for this node into the listbox window hwnd. *)

    BEGIN
        DirNameToText (node, buffer);
        OS2.WinSendMsg (hwnd, OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), SYSTEM.ADR(buffer));
    END RedisplayNode;

(************************************************************************)

PROCEDURE RedisplayTree (hwnd: OS2.HWND;  VAR (*INOUT*) index: SYSTEM.INT16;
                                                       node: DirEntryPtr);

    (* Rewrites listbox entries for the tree whose root is 'node'.  We  *)
    (* assume that the tree is already displayed, but that we want to   *)
    (* refresh it because some details of the nodes have changed.  On   *)
    (* return, index is one step beyond the last node we displayed.     *)

    BEGIN
        RedisplayNode (hwnd, index, node);  INC(index);
        IF NOT node^.collapsed THEN
            ReshowChildren (hwnd, index, node);
        END (*IF*);
    END RedisplayTree;

(****************************************************************************)
(*                     UPDATING DIRECTORY INFORMATION                       *)
(****************************************************************************)

PROCEDURE AddChild (father: DirEntryPtr;  newname: ARRAY OF CHAR;
                                               FileExists, IsADirectory: BOOLEAN);

    (* Adds a new (unexpanded) child node below father, unless the name matches *)
    (* one of the existing children.  On entry the second parameter identifies  *)
    (* the present last child of father.                                        *)

    VAR current: DirEntryPtr;   buffer: FileName;

    BEGIN
        (* Check whether the child already exists. *)

        current := father^.FirstChild;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            Nodes.NameOf (current^.nodeinfo, buffer);
            IF NameMatch (buffer, newname) THEN EXIT(*LOOP*) END(*IF*);
            current := current^.next;
        END (*LOOP*);

        IF current = NIL THEN
            AppendChild (father, newname, FileExists, IsADirectory);
        END (*IF*);

    END AddChild;

(********************************************************************************)

PROCEDURE ExpandNode (VAR (*INOUT*) p: DirEntryPtr);

    (* Makes sure that all subdirectories of the directory corresponding to     *)
    (* node p are included as children of this node.                            *)

    VAR D: DirectorySearchHandle;
        parent, Name: FileName;
        MoreToGo: BOOLEAN;

    BEGIN
        IF (p <> NIL) AND NOT p^.expanded THEN
            Nodes.QueryPhysicalPath (p^.nodeinfo, parent);
            IF (parent[0] <> Nul) AND p^.exists AND NOT p^.IsAFile THEN
                MoreToGo := FirstSubdirectory (parent, Name, D);
                WHILE MoreToGo DO
                    IF (Name[0] = '.') AND (Name[1]=Nul)
                                 OR ((Name[1]='.') AND (Name[2] = Nul)) THEN

                        (* Ignore . and .. entries *)

                    ELSE
                        AddChild (p, Name, TRUE, TRUE);
                    END (*IF*);
                    MoreToGo := NextSubdirectory (D, Name);

                END (*WHILE*);
                SubdirSearchDone (D);
            END (*IF*);
            p^.expanded := TRUE;
        END (*IF*);

    END ExpandNode;

(********************************************************************************)

PROCEDURE PartialExpandTree (p: DirEntryPtr);

    (* Input assumption: all ancestors of p are uncollapsed. *)

    (* Output goal: the subtree whose root is p has all its internal nodes        *)
    (* uncollapsed and expanded, and all its leaf nodes collapsed and unexpanded. *)

    VAR subtree: DirEntryPtr;

    BEGIN
        IF p <> NIL THEN
            CategoriseNode (p);
            subtree := p^.FirstChild;
            IF subtree = NIL THEN
                p^.collapsed := TRUE;  p^.expanded := FALSE;
            ELSE
                p^.collapsed := FALSE;
                REPEAT
                    PartialExpandTree (subtree);  subtree := subtree^.next;
                UNTIL subtree = NIL;
                ExpandNode (p);
            END (*IF*);
        END (*IF*);
    END PartialExpandTree;

(************************************************************************)
(*                  INITIAL LOAD OF PERMISSION DATA                     *)
(************************************************************************)

PROCEDURE LoadPermissions (username: ARRAY OF CHAR): DirEntryPtr;

    (* Loads the permission tree from the INI file.  We assume that     *)
    (* the caller has already opened the INI file.                      *)

    VAR bufptr: CharArrayPointer;  result: DirEntryPtr;
        BufferSize: CARDINAL;

    BEGIN
        IF username[0] = Nul THEN
            BufferSize := 0;
        ELSIF NOT ItemSize (username, "Volume", BufferSize) THEN
            BufferSize := 0;
        END (*IF*);
        IF BufferSize = 0 THEN
            result := NIL;
        ELSE
            ALLOCATE (bufptr, BufferSize);
            IF INIFetchBinary (username, "Volume", bufptr^, BufferSize) THEN
                result := LoadTreeData (bufptr, BufferSize);
                PartialExpandTree (result);
            ELSE
                result := NIL;
            END (*IF*);
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);
        IF result = NIL THEN
            result := CreateDefaultRoot();
        END (*IF*);
        RETURN result;
    END LoadPermissions;

(************************************************************************)
(*                        EDITING A TREE NODE                           *)
(************************************************************************)

PROCEDURE EditNode (hwnd: OS2.HWND;  pnode: DirEntryPtr);

    (* Edits the data for this node.  Makes consequential changes, if   *)
    (* needed, to the subtree below the node.                           *)

    VAR OldName, NewName, OldPPath, NewPPath: FileName;
        NameChanged, PathChanged: BOOLEAN;

    BEGIN
        Nodes.NameOf (pnode^.nodeinfo, OldName);
        Nodes.QueryPhysicalPath (pnode^.nodeinfo, OldPPath);
        Nodes.Edit (hwnd, pnode^.nodeinfo);
        Nodes.NameOf (pnode^.nodeinfo, NewName);
        Nodes.QueryPhysicalPath (pnode^.nodeinfo, NewPPath);
        NameChanged := NOT NameMatch (NewName, OldName);
        PathChanged := NOT NameMatch (NewPPath, OldPPath);
        IF PathChanged THEN
            RemoveObsoleteNodes (pnode);
        END (*IF*);
        IF NameChanged THEN
            IF pnode^.parent = NIL THEN
                pnode^.vpath := "";
            ELSE
                pnode^.vpath := pnode^.parent^.vpath;
                Strings.Append ('/', pnode^.vpath);
                Strings.Append (NewName, pnode^.vpath);
            END (*IF*);
            RecomputeVPath (pnode);
        END (*IF*);
    END EditNode;

(************************************************************************)
(*                         THE TREE DIALOGUE                            *)
(************************************************************************)

PROCEDURE CollapseSubtree (hbox: OS2.HWND;  index: CARDINAL;
                                             node: DirEntryPtr);

    (* Takes the subtrees of current out of the listbox display, and        *)
    (* readjusts left and right thread pointers.  The screen cursor is      *)
    (* not moved.                                                           *)

    (************************************************************************)

    PROCEDURE HideChildren (node: DirEntryPtr;  index: CARDINAL);

        (* Takes the subtrees of node out of the listbox display.  The      *)
        (* screen cursor should be on node on entry; this procedure does    *)
        (* not move it.                                                     *)
        (* Assumption: the parent is uncollapsed.                           *)

        VAR child: DirEntryPtr;

        BEGIN
            child := node^.FirstChild;
            WHILE child <> NIL DO
                OS2.WinSendMsg (hbox, OS2.LM_DELETEITEM,
                                 OS2.MPFROMSHORT(index), NIL);
                IF NOT child^.collapsed THEN
                    HideChildren (child, index);
                END (*IF*);
                child := child^.next;
            END (*WHILE*);
        END HideChildren;

    (************************************************************************)
    (*                      Body of CollapseSubtree                         *)
    (************************************************************************)

    BEGIN
        OS2.WinEnableWindowUpdate (hbox, FALSE);
        IF NOT node^.collapsed THEN
            node^.collapsed := TRUE;
            RedisplayNode (hbox, index, node);
            IF node^.FirstChild <> NIL THEN
                HideChildren (node, index+1);
            END (*IF*);
        END (*IF*);
        OS2.WinEnableWindowUpdate (hbox, TRUE);
    END CollapseSubtree;

(****************************************************************************)

PROCEDURE UncollapseSubtree (box: OS2.HWND;  index: CARDINAL;
                                             node: DirEntryPtr);

    (* Puts the subtrees of current back into the listbox display.      *)
    (* Side-effect: the cursor moves to the last reinstated child.      *)

    (********************************************************************)

    PROCEDURE ReinstateChildren (node: DirEntryPtr);

        (* Puts the subtrees of node back into the listbox display. *)

        VAR child: DirEntryPtr;

        BEGIN
            child := node^.FirstChild;
            WHILE child <> NIL DO
                INC (index);
                DisplayNode (box, index, child);
                IF (NOT child^.collapsed) AND (child^.FirstChild <> NIL) THEN
                    ReinstateChildren (child);
                END (*IF*);
                child := child^.next;
            END (*WHILE*);
        END ReinstateChildren;

    (************************************************************************)
    (*                     Body of UncollapseSubtree                        *)
    (************************************************************************)

    BEGIN
        OS2.WinEnableWindowUpdate (box, FALSE);
        IF node^.collapsed THEN
            node^.collapsed := FALSE;
            RedisplayNode (box, index, node);
            ExpandNode (node);
            ReinstateChildren (node);
        END (*IF*);
        OS2.WinEnableWindowUpdate (box, TRUE);
    END UncollapseSubtree;

(********************************************************************************)

PROCEDURE TogglePermission (box: OS2.HWND;  index: CARDINAL;
                                      node: DirEntryPtr;  perm: Permission);

    (* Toggles a node permission, redisplays node. *)

    BEGIN
        IF perm IN node^.flags THEN EXCL(node^.flags, perm)
        ELSE INCL(node^.flags, perm);
        END (*IF*);
        RedisplayNode (box, index, node);
    END TogglePermission;

(********************************************************************************)
(*                           MAIN DIALOGUE PROCEDURE                            *)
(********************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the permissions dialogue. *)

    CONST BoxID = 999;

    VAR NotificationCode, ButtonID: CARDINAL;
        index: SYSTEM.INT16;
        pnode, target: DirEntryPtr;
        hbox: OS2.HWND;
        NodeIsEditable, WasCollapsed: BOOLEAN;
        RowBuffer: ARRAY [0..511] OF CHAR;
        ch: CHAR;

    BEGIN
        hbox := OS2.WinWindowFromID (hwnd, DID.TreeBox);
        index := OS2.SHORT1FROMMP(OS2.WinSendMsg (hbox, OS2.LM_QUERYSELECTION,
                                 OS2.MPFROMSHORT(OS2.LIT_CURSOR), NIL));
        IF index = OS2.LIT_NONE THEN
            pnode := NIL;
        ELSE
            pnode := OS2.WinSendMsg (hbox, OS2.LM_QUERYITEMHANDLE,
                                 OS2.MPFROMSHORT(index), NIL);
        END (*IF*);

        CASE msg OF

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.TreeAdd:
                          CollapseSubtree (hbox, index, pnode);
                          AppendChild (pnode, "", FALSE, TRUE);
                          target := pnode^.LastChild;
                          UncollapseSubtree (hbox, index, pnode);
                          REPEAT
                              INC (index);
                              pnode := OS2.WinSendMsg (hbox, OS2.LM_QUERYITEMHANDLE,
                                                   OS2.MPFROMSHORT(index), NIL);
                          UNTIL pnode = target;
                          OS2.WinSendDlgItemMsg (hwnd, DID.TreeBox, OS2.LM_SELECTITEM,
                                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                          OS2.WinPostMsg (hwnd, OS2.WM_COMMAND, OS2.MPFROMSHORT(DID.TreeEdit), NIL);

                     | DID.TreeEdit:
                          NodeIsEditable := Nodes.IsVirtual(pnode^.nodeinfo) OR NOT pnode^.exists;
                          IF NodeIsEditable THEN
                              WasCollapsed := pnode^.collapsed;
                              CollapseSubtree (hbox, index, pnode);
                              EditNode (hwnd, pnode);
                              DirNameToText (pnode, RowBuffer);
                              OS2.WinSendMsg (hbox, OS2.LM_SETITEMTEXT,
                                             OS2.MPFROMSHORT(index), SYSTEM.ADR(RowBuffer));
                              IF pnode^.parent = NIL THEN
                                  OS2.WinSetDlgItemText (hwnd, DID.VirtualPath, "/");
                              ELSE
                                  OS2.WinSetDlgItemText (hwnd, DID.VirtualPath, pnode^.vpath);
                              END (*IF*);
                              Nodes.QueryPhysicalPath (pnode^.nodeinfo, RowBuffer);
                              OS2.WinSetDlgItemText (hwnd, DID.PhysicalPath, RowBuffer);
                              IF NOT WasCollapsed THEN
                                  UncollapseSubtree (hbox, index, pnode);
                              END (*IF*);
                          END (*IF*);

                     | DID.TreeDelete:
                          NodeIsEditable := Nodes.IsVirtual(pnode^.nodeinfo) OR NOT pnode^.exists;
                          IF (NodeIsEditable OR pnode^.IsAFile) AND (pnode^.parent <> NIL) THEN
                              CollapseSubtree (hbox, index, pnode);
                              DeleteTree (pnode);
                              OS2.WinSendDlgItemMsg (hwnd, DID.TreeBox, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                              IF index >= OS2.SHORT1FROMMP(OS2.WinSendMsg (hbox, OS2.LM_QUERYITEMCOUNT, NIL, NIL)) THEN
                                  DEC (index);
                              END (*IF*);
                              OS2.WinSendDlgItemMsg (hwnd, DID.TreeBox, OS2.LM_SELECTITEM,
                                              OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                              pnode := OS2.WinSendMsg (hbox, OS2.LM_QUERYITEMHANDLE,
                                              OS2.MPFROMSHORT(index), NIL);
                          END (*IF*);

                     | DID.TreeInherit:
                          IF pnode^.parent = NIL THEN
                              pnode^.flags := PermissionSet {Visible, AllowRead}
                          ELSE
                              pnode^.flags := pnode^.parent^.flags;
                          END (*IF*);
                          DirNameToText (pnode, RowBuffer);
                          OS2.WinSendMsg (hbox, OS2.LM_SETITEMTEXT,
                                             OS2.MPFROMSHORT(index), SYSTEM.ADR(RowBuffer));
                          RETURN NIL;

                     | DID.TreePropagate:
                          CopyDownPermissions (pnode);
                          RedisplayTree (hbox, index, pnode);
                          RETURN NIL;

                     | DID.TreeDone:
                          OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                          RETURN NIL;

                     | DID.Plus:
                          UncollapseSubtree (hbox, index, pnode);
                          RETURN NIL;

                     | DID.Minus:
                          CollapseSubtree (hbox, index, pnode);
                          RETURN NIL;

                     | DID.V:
                          TogglePermission (hbox, index, pnode, Visible);
                          RETURN NIL;
                     | DID.R:
                          TogglePermission (hbox, index, pnode, AllowRead);
                          RETURN NIL;
                     | DID.W:
                          TogglePermission (hbox, index, pnode, AllowWrite);
                          RETURN NIL;
                     | DID.D:
                          TogglePermission (hbox, index, pnode, AllowDelete);
                          RETURN NIL;
                     | DID.N:
                          TogglePermission (hbox, index, pnode, AllowRename);
                          RETURN NIL;

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

          |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;

                   IF ButtonID = DID.TreeBox THEN

                       IF NotificationCode = OS2.LN_SELECT THEN

                           IF pnode <> NIL THEN
                               IF pnode^.parent = NIL THEN
                                   OS2.WinSetDlgItemText (hwnd, DID.VirtualPath, "/");
                               ELSE
                                   OS2.WinSetDlgItemText (hwnd, DID.VirtualPath, pnode^.vpath);
                               END (*IF*);
                               Nodes.QueryPhysicalPath (pnode^.nodeinfo, RowBuffer);
                               OS2.WinSetDlgItemText (hwnd, DID.PhysicalPath, RowBuffer);
                           END (*IF*);

                       ELSIF NotificationCode = OS2.LN_ENTER THEN

                           (* Treat this one as if the edit button had been clicked. *)

                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.TreeEdit), NIL);
                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*IF*);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CHAR:
                   ch := OS2.CHAR1FROMMP (mp2);
                   CASE CAP(ch) OF
                     | 'V':  ButtonID := DID.V;
                     | 'R':  ButtonID := DID.R;
                     | 'W':  ButtonID := DID.W;
                     | 'D':  ButtonID := DID.D;
                     | 'N':  ButtonID := DID.N;
                     | '+':  ButtonID := DID.Plus;
                     | '-':  ButtonID := DID.Minus;
                   ELSE
                         RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);
                   OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                   OS2.MPFROMSHORT(ButtonID), NIL);
                   RETURN NIL;

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        (* Enable/disable some of the controls, depending on whether    *)
        (* we're on an editable node.  The possible node types are:     *)
        (*   virtual: pseudo-directory or link, always editable         *)
        (*   real directory or real file: not editable                  *)
        (*   nonexistent directory or file: editable                    *)

        (* Note one special case: a non-directory real file can appear  *)
        (* in the window if the user wants to override the default      *)
        (* attributes.  Because we might want to cancel such overriding *)
        (* the entry has to be non-editable but deletable.              *)

        NodeIsEditable := (pnode <> NIL) AND
                         (Nodes.IsVirtual(pnode^.nodeinfo) OR NOT pnode^.exists);

        IF NodeIsEditable THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TreeEdit), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TreeEdit), FALSE);
        END (*IF*);

        IF (NodeIsEditable OR pnode^.IsAFile) AND (pnode^.parent <> NIL) THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TreeDelete), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TreeDelete), FALSE);
        END (*IF*);

        RETURN NIL;

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  VAR (*INOUT*) name: ARRAY OF CHAR;
                                  root: DirEntryPtr);

    (* Edit the permission tree of the user called "name".   *)

    VAR hwnd: OS2.HWND;
        title: ARRAY [0..255] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.TreeDialogue,                (* dialogue ID *)
                       NIL);               (* creation parameters *)
        hwndtop := hwnd;
        Remote.SetInitialWindowPosition (hwnd, "Tree");
        title := "Username: ";
        Strings.Append (name, title);
        OS2.WinSetWindowText (hwnd, title);
        InitialDisplay (OS2.WinWindowFromID(hwnd, DID.TreeBox), root);
        OS2.WinSendDlgItemMsg (hwnd, DID.TreeBox, OS2.LM_SELECTITEM,
                        OS2.MPFROMSHORT(0), OS2.MPFROMSHORT(ORD(TRUE)));
        OS2.WinProcessDlg(hwnd);

        Remote.StoreWindowPosition (hwnd, "Tree", TRUE);
        OS2.WinDestroyWindow (hwnd);

    END Edit;

(************************************************************************)

END Tree.

