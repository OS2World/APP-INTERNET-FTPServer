(**************************************************************************)
(*                                                                        *)
(*  Text-mode setup for FtpServer                                         *)
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

IMPLEMENTATION MODULE PermissionEditor;

        (********************************************************)
        (*                                                      *)
        (*         Editor for the user access rights            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 December 1997                 *)
        (*  Last edited:        22 October 2001                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(*     Syntax for the permissions data:                                 *)
(*                                                                      *)
(*     <result>     ->  <dirlist>                                       *)
(*     <diritem>    ->  <dirname> <dirrule>                             *)
(*     <dirname>    ->  <namestring>  |  <namestring> = <namestring>    *)
(*     <dirrule>    -> { <permission> }* { ( <dirlist> ) }              *)
(*     <dirlist>    ->  <diritem> { , <diritem> }*                      *)
(*     <dirlist>    -> { <diritem> }+                                   *)
(*     <permission> ->  V+ | V- | R+ | R- | W- | W+ | D- | D+           *)
(*     <namestring> ->  <string1> | " <string2> " | ' <string3> '       *)
(*     <string1>    -> any string not including space char              *)
(*     <string2>    -> any string not including double quote            *)
(*     <string3>    -> any string not including single quote            *)
(*                                                                      *)
(*  Notation: {} means optional, {}* means zero or more, {}+ means      *)
(*            one or more.                                              *)
(*                                                                      *)
(************************************************************************)

IMPORT Strings, OS2;

FROM SYSTEM IMPORT ADR, ADDRESS;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, DestroyListBox, HighlightOn, HighlightOff,
                WindowOf, CursorBackward, CursorForward,
                LBAppend, ReplaceCurrent, LBDeleteCurrent, LBDeleteNext,
                LBInsertAfter, DisableScreenOutput, ClearListBox, Repaint;

FROM MultiScreen IMPORT
    (* type *)  VirtualScreen,
    (* proc *)  VirtualScreenOf, MapToVirtualScreen;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, WriteChar, WriteString, WriteLn,
                SetCursor, PressAnyKey, EditString, GetKey, GetScreenSize,
                EraseLine, ClearWindow, PutOnTop, Hide, ColourSwap;

FROM Keyboard IMPORT
    (* proc *)  PutBack;

FROM FileOps IMPORT
    (* type *)  FileAttr, FileAttribute, DirectoryEntry,
                FirstDirEntry, NextDirEntry, DirSearchDone;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Assert;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

CONST Nul = CHR(0);  CR = CHR(13);  Esc = CHR(1BH);  Space = " ";

TYPE
    CharSet = SET OF CHAR;
    FileNameSubscript = [0..255];
    FileNameString = ARRAY FileNameSubscript OF CHAR;

    (* The following declarations relate to the data structure that we keep,    *)
    (* for a logged-in user, to show which directories and files are accessible.*)
    (* For each directory, we have a linked list of subdirectories.  The left   *)
    (* and right pointers are filled in when we need to thread the structure.   *)
    (* The altright pointer is a saved copy of right for use when this node has *)
    (* a collapsed subtree.  It turns out that you can't do a similar thing for *)
    (* the left threads; they have to be readjusted each time we expand or      *)
    (* collapse a subtree.                                                      *)

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
    (*   virtual       this record is for a symbolic link                       *)
    (*   ppath         full physical path name                                  *)
    (*   vpath         virtual path, i.e. path as seen by the ftp user          *)

    DirEntry = RECORD
                   indent: CARDINAL;
                   flags: PermissionSet;
                   parent, FirstChild, LastChild, next: DirEntryPtr;
                   name: FileNameString;
                   left, right, altright: DirEntryPtr;
                   collapsed, expanded, exists, IsAFile: BOOLEAN;
                   virtual: BOOLEAN;
                   ppath, vpath: FileNameString;
               END (*RECORD*);

VAR ScreenRows: CARDINAL;

(********************************************************************************)
(*                       PERMISSION ENCODING/DECODING                           *)
(********************************************************************************)

PROCEDURE CharToPermission (ch: CHAR): Permission;

    (* Translates a code (V,R,W,D,N) to a permission. *)

    BEGIN
        ch := CAP(ch);
        IF ch = 'R' THEN RETURN AllowRead
        ELSIF ch = 'W' THEN RETURN AllowWrite
        ELSIF ch = 'D' THEN RETURN AllowDelete
        ELSIF ch = 'N' THEN RETURN AllowRename
        ELSE RETURN Visible
        END (*IF*);
    END CharToPermission;

(********************************************************************************)
(*                            DEBUGGING OUTPUT                                  *)
(********************************************************************************)

(*
PROCEDURE DumpNode (p: DirEntryPtr;  indent: CARDINAL);

    (* Dumps a single node. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 1 TO indent DO
            WriteChar (debug, " ");
        END (*FOR*);
        IF p^.collapsed THEN
            WriteChar (debug, "+");
        ELSE
            WriteChar (debug, " ");
        END (*IF*);
        IF p^.virtual THEN
            WriteChar (debug, "*");
        ELSE
            WriteChar (debug, " ");
        END (*IF*);
        WriteString (debug, p^.name);
        IF p^.virtual THEN
            WriteString (debug, " = ");  WriteString (debug, p^.ppath);
        END (*IF*);
        WriteLn (debug);
        FOR j := 1 TO indent+2 DO
            WriteChar (debug, " ");
        END (*FOR*);
        IF p^.left <> NIL THEN
            WriteString (debug, " left: ");  WriteString (debug, p^.left^.name);
        END (*IF*);
        IF p^.right <> NIL THEN
            WriteString (debug, " right: ");  WriteString (debug, p^.right^.name);
        END (*IF*);
        IF p^.altright <> NIL THEN
            WriteString (debug, " altright: ");  WriteString (debug, p^.altright^.name);
        END (*IF*);
        (*WriteLn (debug);*)
        PressAnyKey (debug);
    END DumpNode;

(********************************************************************************)

PROCEDURE DumpTree (root: DirEntryPtr;  indent: CARDINAL);

    (* Dumps an entire tree. *)

    VAR current: DirEntryPtr;

    BEGIN
        IF root <> NIL THEN
            DumpNode (root, indent);
            INC (indent, 4);
            current := root^.FirstChild;
            WHILE current <> NIL DO
                DumpTree (current, indent);
                current := current^.next;
            END (*WHILE*);
        END (*IF*);
    END DumpTree;
*)

(********************************************************************************)

PROCEDURE CheckLinks (p, leftptr, rightptr: DirEntryPtr;
                                          VAR (*OUT*) LastNode: DirEntryPtr): BOOLEAN;

    (* Aborts if the subtree with root p has faulty links.  Returns TRUE if     *)
    (* the links are consistent.  Collapsed subtrees are not checked.           *)

    VAR child: DirEntryPtr;

    BEGIN
        Assert (p^.left = leftptr);
        IF p^.FirstChild = NIL THEN
            Assert (p^.right = rightptr);
            Assert (p^.right = p^.altright);
            LastNode := p;
        ELSIF p^.collapsed THEN
            Assert (p^.right = rightptr);
            Assert (p^.altright = p^.FirstChild);
            LastNode := p;
        ELSE
            child := p^.FirstChild;
            Assert (p^.altright = rightptr);
            Assert (p^.right = child);
            leftptr := p;
            WHILE child^.next <> NIL DO
                Assert (CheckLinks (child, leftptr, child^.next, leftptr));
                child := child^.next;
            END (*WHILE*);
            Assert (CheckLinks (child, leftptr, rightptr, LastNode));
        END (*IF*);
        RETURN TRUE;
    END CheckLinks;

(********************************************************************************)

PROCEDURE CheckChildLinks (p: DirEntryPtr): BOOLEAN;

    (* Checks that LastChild pointers make sense in this subtree. *)

    VAR current, tail: DirEntryPtr;

    BEGIN
        tail := NIL;  current := p^.FirstChild;
        WHILE current <> NIL DO
            tail := current;  Assert (CheckChildLinks(tail));
            current := current^.next;
        END (*WHILE*);
        Assert (p^.LastChild = tail);
        RETURN TRUE;
    END CheckChildLinks;

(********************************************************************************)

PROCEDURE LinksAreConsistent (root: DirEntryPtr): BOOLEAN;

    (* Debugging procedure.  Goes through the tree to see whether the threads   *)
    (* make sense.                                                              *)

    VAR dummy: DirEntryPtr;

    BEGIN
        Assert (CheckLinks(root, NIL, NIL, dummy) AND CheckChildLinks(root));
        RETURN TRUE;
    END LinksAreConsistent;

(********************************************************************************)
(*                  ADDING/DELETING NODES IN THE DIRECTORY TREE                 *)
(********************************************************************************)

PROCEDURE LastVisibleNode (p: DirEntryPtr): DirEntryPtr;

    (* Returns the last node in the subtree, whose root is p, which is not      *)
    (* hidden by collapsed ancestors.  (Assumption: p is not itself hidden by   *)
    (* collapsed ancestors.)                                                    *)

    VAR result: DirEntryPtr;

    BEGIN
        result := p;
        WHILE (NOT p^.collapsed) AND (result^.LastChild <> NIL) DO
            result := result^.LastChild;
        END (*WHILE*);
        RETURN result;
    END LastVisibleNode;

(********************************************************************************)

PROCEDURE ToggleCollapsedState (p: DirEntryPtr);

    (* Changes the state of p from collapsed to uncollapsed, or vice versa. *)

    VAR oldright: DirEntryPtr;

    BEGIN
        oldright := p^.right;
        p^.right := p^.altright;  p^.altright := oldright;
        IF p^.right <> NIL THEN
            p^.right^.left := p;
        END (*IF*);
        p^.collapsed := NOT p^.collapsed;

        (* If we've just uncollapsed the node, we might have exposed a          *)
        (* descendant node that needs to be put back in the traversal list.     *)

        IF NOT p^.collapsed AND (oldright <> NIL) THEN
            oldright^.left := LastVisibleNode(p);
        END (*IF*);

    END ToggleCollapsedState;

(********************************************************************************)

PROCEDURE RecomputeVPath (root: DirEntryPtr);

    (* The caller must have set root^.vpath correctly.  This procedure sets     *)
    (* the vpath field for all descendant nodes.  (We need to do this only in   *)
    (* the case where root^.vpath has changed for some reason.                  *)

    VAR current: DirEntryPtr;

    BEGIN
        current := root^.FirstChild;
        WHILE current <> NIL DO
            current^.vpath := root^.vpath;
            Strings.Append ('/', current^.vpath);
            Strings.Append (current^.name, current^.vpath);
            RecomputeVPath (current);
            current := current^.next;
        END (*WHILE*);
    END RecomputeVPath;

(********************************************************************************)

PROCEDURE ComputeAltLinks (root, rightnode: DirEntryPtr): DirEntryPtr;

    (* Computes the "altright" links for the subtree whose root is "root".      *)
    (* The second argument is the "altright" link for the root, and it might    *)
    (* also be used lower down in the tree.   NOTE: for a collapsed node the    *)
    (* "right" and "altright" links were swapped at the time of collapsing, so  *)
    (* what we have to compute in that case is what the "right" link would have *)
    (* been if the node had not been collapsed.                                 *)

    (* The function result is the rightmost node that isn't hidden by collapsed *)
    (* ancestors.                                                               *)

    VAR current, result: DirEntryPtr;

    BEGIN
        IF root^.collapsed AND (root^.FirstChild <> NIL) THEN
            root^.altright := root^.FirstChild;
        ELSE
            root^.altright := rightnode;
        END (*IF*);
        current := root^.FirstChild;
        IF current = NIL THEN
            result := root;
        ELSE
            WHILE current^.next <> NIL DO
                EVAL (ComputeAltLinks (current, current^.next));
                current := current^.next;
            END (*WHILE*);
            result := ComputeAltLinks (current, rightnode);
        END (*IF*);
        IF root^.collapsed THEN
            RETURN root;
        ELSE
            RETURN result;
        END (*IF*);
    END ComputeAltLinks;

(********************************************************************************)

PROCEDURE AppendChild (father: DirEntryPtr;  newname: ARRAY OF CHAR;
                                               FileExists, IsADirectory: BOOLEAN);

    (* Creates a new node and adds it the the children of father.        *)
    (* Assumption: neither father nor any of its ancestors is collapsed. *)

    VAR q, nextright, brother, leftlink: DirEntryPtr;

    BEGIN
        Assert ((father <> NIL) AND NOT father^.collapsed);
        NEW (q);
        brother := father^.LastChild;  father^.LastChild := q;
        IF brother = NIL THEN
            father^.FirstChild := q;
            nextright := father^.right;
        ELSIF brother^.collapsed THEN
            nextright := brother^.right;
        ELSE
            nextright := brother^.altright;
        END (*IF*);

        WITH q^ DO
            indent := father^.indent + 3;
            flags := father^.flags;
            parent := father;
            FirstChild := NIL;  LastChild := NIL;
            next := NIL;
            Strings.Assign (newname, name);
            Strings.Assign (father^.ppath, ppath);
            IF ppath[0] <> Nul THEN
                Strings.Append ('/', ppath);
            END (*IF*);
            Strings.Append (name, ppath);
            Strings.Assign (father^.vpath, vpath);
            Strings.Append ('/', vpath);
            Strings.Append (name, vpath);
            IF nextright <> NIL THEN
                nextright^.left := q;
            END (*IF*);
            right := nextright;  altright := nextright;
            collapsed := TRUE;
            expanded := FALSE;
            exists := FileExists;
            IsAFile := NOT IsADirectory;
            virtual := newname[0] = Nul;
        END (*WITH*);

        (* We still haven't filled in the link to the node to the left of us. *)

        IF brother = NIL THEN
            leftlink := father;
        ELSE

            (* The subtree whose root is brother can have links pointing out of *)
            (* the subtree, and these could be wrong now that we've inserted a  *)
            (* new node at the right of brother.  The unique "right" link to q  *)
            (* will be fixed below, but meanwhile there could be altright links *)
            (* that still need fixing.                                          *)

            brother^.next := q;
            leftlink := ComputeAltLinks (brother, q);

        END (*IF*);
        leftlink^.right := q;
        q^.left := leftlink;

    END AppendChild;

(********************************************************************************)

PROCEDURE DeleteLeafNode (VAR (*INOUT*) p: DirEntryPtr);

    (* Removes a node from the tree, adjusts links in remaining nodes as needed. *)
    (* Assumption: this node has no children, and no collapsed ancestors.        *)

    VAR brother, current: DirEntryPtr;

    BEGIN
        Assert (p^.FirstChild = NIL);
        Assert (p^.parent^.collapsed = FALSE);

        (* Remove this node from the left/right chain. *)

        IF p^.left <> NIL THEN
            p^.left^.right := p^.right;
        END (*IF*);
        IF p^.right <> NIL THEN
            p^.right^.left := p^.left;
        END (*IF*);

        (* Find the left brother of this node. *)

        brother := NIL;  current := p^.parent^.FirstChild;
        WHILE current <> p DO
            brother := current;  current := current^.next;
        END (*WHILE*);

        (* Remove this node from the tree. *)

        IF brother = NIL THEN
            p^.parent^.FirstChild := p^.next;
        ELSE
            brother^.next := p^.next;
        END (*IF*);
        IF p^.parent^.LastChild = p THEN
            p^.parent^.LastChild := brother;
        END (*IF*);

        (* Fix up the alt links in the brother's subtree. *)

        IF brother <> NIL THEN
            EVAL (ComputeAltLinks (brother, p^.right));
        END (*IF*);

        (* Remove this node. *)

        DISPOSE (p);

    END DeleteLeafNode;

(********************************************************************************)

PROCEDURE RemoveChildren (p: DirEntryPtr);

    (* Deletes all children of p from the tree.  We assume that either p is     *)
    (* collapsed or that it has a collapsed ancestor.                           *)

    VAR child, temp: DirEntryPtr;

    BEGIN
        child := p^.FirstChild;
        WHILE child <> NIL DO
            RemoveChildren (child);
            temp := child;  child := child^.next;
            DISPOSE (temp);
        END (*WHILE*);
        p^.FirstChild := NIL;  p^.LastChild := NIL;
        p^.expanded := FALSE;
        IF p^.collapsed THEN p^.altright := p^.right
        ELSE p^.right := p^.altright
        END (*IF*);
    END RemoveChildren;

(********************************************************************************)

PROCEDURE DeleteSubtree (p: DirEntryPtr);

    (* Removes an entire subtree from the tree.          *)
    (* Assumption: this node has no collapsed ancestors. *)

    BEGIN
        IF NOT p^.collapsed THEN
            ToggleCollapsedState (p);
        END (*IF*);
        RemoveChildren (p);
        DeleteLeafNode (p);
    END DeleteSubtree;

(********************************************************************************)

PROCEDURE CreateDefaultRoot(): DirEntryPtr;

    (* Creates a node with default values for the root. *)

    VAR result: DirEntryPtr;

    BEGIN
        NEW (result);
        WITH result^ DO
            indent := 0;
            flags := PermissionSet {Visible, AllowRead};
            parent := NIL;  FirstChild := NIL;  LastChild := NIL;  next := NIL;
            name := "";
            left := NIL;  right := NIL;  altright := NIL;
            collapsed := FALSE;  expanded := FALSE;  exists := TRUE;  IsAFile := FALSE;
            virtual := TRUE;
            ppath := "";  vpath := "";
        END (*WITH*);
        RETURN result;
    END CreateDefaultRoot;

(********************************************************************************)
(*                                PARSER                                        *)
(********************************************************************************)

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
        WHILE (j <= HIGH(result)) AND NOT (NextChar IN Stoppers) DO
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

    CONST SingleQuote = "'";  DoubleQuote = '"';

    VAR Delimiter: CHAR;

    BEGIN
        INCL (Stoppers, Nul);
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
                         VAR (*INOUT*) NextChar: CHAR): DirEntryPtr;  FORWARD;

(****************************************************************************)

PROCEDURE DirList (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                   VAR (*INOUT*) NextChar: CHAR): DirEntryPtr;

    (*     <dirlist>   ->  <diritem> { , <diritem> }*       *)
    (* Result returned: a linked list of directory nodes.   *)

    CONST Comma = ",";

    VAR result, lastnode: DirEntryPtr;

    BEGIN
        result := DirItem (bufptr, position, BufferSize, mother, NextChar);
        lastnode := result;
        WHILE (NextChar = Comma) OR (NextChar = ';') DO
            NextChar := Scan (bufptr, position, BufferSize);
            lastnode^.next := DirItem (bufptr, position, BufferSize, mother, NextChar);
            lastnode := lastnode^.next;
        END (*WHILE*);
        RETURN result;
    END DirList;

(****************************************************************************)

PROCEDURE DirRule (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  pnode: DirEntryPtr;
                            VAR (*INOUT*) NextChar: CHAR);

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
            pnode^.FirstChild := DirList (bufptr, position, BufferSize, pnode, NextChar);
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

    VAR result: DirEntryPtr;  k: CARDINAL;  NewName: FileNameString;

    BEGIN
        NameString (bufptr, position, BufferSize, NewName, NextChar, CharSet{});
        AppendChild (mother, NewName, FALSE, FALSE);  result := mother^.LastChild;
        IF NextChar = '=' THEN
            NextChar := Scan (bufptr, position, BufferSize);
            result^.virtual := TRUE;
            NameString (bufptr, position, BufferSize, NewName, NextChar, CharSet{});

            (* Transition arrangement: get rid of the trailing '/'.  *)

            k := LENGTH (NewName);
            IF k > 0 THEN
                DEC (k);
                IF (NewName[k] = '/') OR (NewName[k] = '\') THEN
                    NewName[k] := Nul;
                END (*IF*);
            END (*IF*);

            result^.ppath := NewName;
        END (*IF*);

        RETURN result;

    END DirName;

(****************************************************************************)

PROCEDURE DirItem (bufptr: CharArrayPointer;  VAR (*INOUT*) position: CARDINAL;
                            BufferSize: CARDINAL;  mother: DirEntryPtr;
                                VAR (*INOUT*) NextChar: CHAR): DirEntryPtr;

    (*     <diritem>   -> <dirname> <dirrule>                            *)

    VAR result: DirEntryPtr;

    BEGIN
        result := DirName (bufptr, position, BufferSize, mother, NextChar);
        DirRule (bufptr, position, BufferSize, result, NextChar);
        RETURN result;
    END DirItem;

(****************************************************************************)

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
        IF NextChar = Nul THEN
            root^.FirstChild := NIL;
        ELSE
            root^.FirstChild := DirList (bufptr, position, BufferSize,
                                       root, NextChar);
        END (*IF*);

        (* Decide whether we really need a pseudo-directory as     *)
        (* the root directory.                                     *)

        IF root^.FirstChild <> NIL THEN
            IF root^.FirstChild^.next = NIL THEN
                temp := root;
                root := root^.FirstChild;
                DISPOSE (temp);
                root^.parent := NIL;  root^.left := NIL;
                root^.vpath := "";
                RecomputeVPath (root);
            END (*IF*);
        END (*IF*);

        Assert (LinksAreConsistent (root));
        RETURN root;

    END LoadTreeData;

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

    BEGIN
        AppendQuotedString (D^.name, result, index);
        IF D^.virtual THEN
            result[index] := '=';  INC (index);
            AppendQuotedString (D^.ppath, result, index);
        END (*IF*);
        StorePermAndSubdir (D, DefaultFlags, result, index);
    END StoreTreeData;

(************************************************************************)

PROCEDURE SizeOfTreeData (D: DirEntryPtr;
                               DefaultFlags: PermissionSet): CARDINAL;

    (* Returns the number of characters needed to store the INI file    *)
    (* data for this directory.                                         *)

    VAR result: CARDINAL;

    BEGIN
        result := LENGTH(D^.name) + 2 + SizeOfPermAndSubdir (D, DefaultFlags);
        IF D^.virtual THEN
            INC (result, 3 + LENGTH(D^.ppath));
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
            DISPOSE (p);
            p := q;
        END (*IF*);
    END KillList;

(********************************************************************************)
(*                           STRING COMPARISON                                  *)
(********************************************************************************)

PROCEDURE NameMatch (first, second: ARRAY OF CHAR): BOOLEAN;

    (* Equality test, modulo upper/lower case differences.  That is, the result *)
    (* is TRUE even if the letters in first and second don't have the same      *)
    (* case, as long as the strings are otherwise equal.                        *)

    VAR k: CARDINAL;

    BEGIN
        k := 0;
        LOOP
            IF k > HIGH(first) THEN RETURN (k > HIGH(second));
            ELSIF first[k] = Nul THEN RETURN (second[k] = Nul);
            ELSIF CAP(first[k]) <> CAP(second[k]) THEN RETURN FALSE;
            ELSE INC(k);
            END (*IF*);
        END (*LOOP*);
    END NameMatch;

(************************************************************************)
(*                   "Not yet implemented" message                      *)
(************************************************************************)

(*
PROCEDURE NotImplemented;

    VAR w: Window;

    BEGIN
        OpenWindow (w, yellow, red, 12, 15, 15, 64, noframe, nodivider);
        WriteLn (w);
        WriteString (w, " Sorry, that feature is not yet implemented");
        PressAnyKey (w);
        CloseWindow (w);
    END NotImplemented;
*)

(********************************************************************************)
(*                       UPDATING DIRECTORY INFORMATION                         *)
(********************************************************************************)

PROCEDURE PruneTree (p, root: DirEntryPtr;  defaults: PermissionSet);

    (* Removes the tree nodes that aren't necessary because their permissions   *)
    (* are directly inherited from their parents.  Note: virtual nodes must     *)
    (* never be pruned, or we would lose the link information.                  *)

    VAR current, next: DirEntryPtr;

    BEGIN
        current := p;
        WHILE current <> NIL DO
            Assert (LinksAreConsistent (root));
            next := current^.next;
            IF current^.collapsed THEN
                Assert (LinksAreConsistent (root));
                ToggleCollapsedState (current);
                Assert (LinksAreConsistent (root));
            END (*IF*);
            Assert (LinksAreConsistent (root));
            PruneTree (current^.FirstChild, root, current^.flags);
            Assert (LinksAreConsistent (root));
            current^.expanded := FALSE;
            IF (current^.FirstChild = NIL) AND (current^.flags = defaults)
                      AND (current^.parent <> NIL) AND NOT current^.virtual THEN
                Assert (LinksAreConsistent (root));
                DeleteLeafNode (current);
                Assert (LinksAreConsistent (root));
            END (*IF*);
            current := next;
        END (*WHILE*);
    END PruneTree;

(********************************************************************************)

PROCEDURE CleanUpTree (VAR (*INOUT*) root: DirEntryPtr);

    (* Removes the tree nodes that aren't necessary because their permissions   *)
    (* are directly inherited from their parents.                               *)

    BEGIN
        Assert (LinksAreConsistent (root));
        PruneTree (root, root, PermissionSet {Visible, AllowRead} );
        Assert (LinksAreConsistent (root));
    END CleanUpTree;

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

    VAR D: DirectoryEntry;  fullname: FileNameString;
        InfoBuf: OS2.FSALLOCATE;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        fullname := p^.ppath;
        IF fullname[0] = Nul THEN

            (* This is a pseudo-directory. *)

            p^.exists := TRUE;  p^.IsAFile := FALSE;

        ELSIF (fullname[2] = Nul) AND (fullname[1] = ':') THEN

            (* Root of a filesystem volume. *)

            p^.exists := OS2.DosQueryFSInfo (ORD(CAP(fullname[0])) - ORD('A') + 1,
                           1, ADR(InfoBuf), SIZE(InfoBuf)) = 0;
            p^.IsAFile := NOT p^.exists;

        ELSE
            (* We have a real name, but we still have to check three    *)
            (* possibilities: this is a file, this is a directory, this *)
            (* is something that doesn't exist on our disk.             *)

            p^.exists := FirstDirEntry (fullname, FALSE, TRUE, D);
            IF p^.exists THEN
                p^.IsAFile := NOT(directory IN D.attr);
            END (*IF*);
            DirSearchDone (D);
        END (*IF*);

    END CategoriseNode;

(********************************************************************************)

PROCEDURE RemoveObsoleteNodes (p: DirEntryPtr);

    (* Called when p^.ppath might have changed.  We delete non-virtual children *)
    (* and their subtrees, but leave the virtual children intact.               *)

    VAR child, next: DirEntryPtr;  WasCollapsed: BOOLEAN;

    BEGIN
        CategoriseNode (p);
        WasCollapsed := p^.collapsed;
        IF WasCollapsed THEN
            ToggleCollapsedState (p);
        END (*IF*);
        next := p^.FirstChild;
        WHILE next <> NIL DO
            child := next;  next := child^.next;
            IF NOT child^.virtual THEN
                DeleteSubtree (child);
            END (*IF*);
        END (*WHILE*);
        p^.expanded := FALSE;
        IF WasCollapsed THEN
            ToggleCollapsedState (p);
        END (*IF*);
    END RemoveObsoleteNodes;

(********************************************************************************)

PROCEDURE AddChild (father: DirEntryPtr;  newname: ARRAY OF CHAR;
                                               FileExists, IsADirectory: BOOLEAN);

    (* Adds a new (unexpanded) child node below father, unless the name matches *)
    (* one of the existing children.  On entry the second parameter identifies  *)
    (* the present last child of father.                                        *)

    VAR current: DirEntryPtr;

    BEGIN
        (* Check whether the child already exists. *)

        current := father^.FirstChild;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            IF NameMatch (current^.name, newname) THEN EXIT(*LOOP*) END(*IF*);
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

    VAR D: DirectoryEntry;
        SearchString: FileNameString;
        MoreToGo: BOOLEAN;

    BEGIN
        IF (p <> NIL) AND NOT p^.expanded THEN
            IF (p^.ppath[0] <> Nul) AND p^.exists AND NOT p^.IsAFile THEN
                Strings.Assign (p^.ppath, SearchString);
                Strings.Append ('/*', SearchString);
                MoreToGo := FirstDirEntry (SearchString, TRUE, TRUE, D);
                WHILE MoreToGo DO
                    IF (D.name[0] = '.') AND (D.name[1]=Nul)
                                 OR ((D.name[1]='.') AND (D.name[2] = Nul)) THEN

                        (* Ignore . and .. entries *)

                    ELSIF directory IN D.attr THEN
                        AddChild (p, D.name, TRUE, TRUE);
                    END (*IF*);
                    MoreToGo := NextDirEntry (D);

                END (*WHILE*);
                DirSearchDone (D);
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
    END PartialExpandTree;

(********************************************************************************)

PROCEDURE SetUpInitialTree (root: DirEntryPtr);

    (* Takes the tree, which has been constructed from the INI file data, and   *)
    (* expands all existing nodes to include all their subdirectories.  Also    *)
    (* ensures that all threads are set up correctly.                           *)

    BEGIN
        PartialExpandTree (root);
        Assert (LinksAreConsistent (root));
    END SetUpInitialTree;

(********************************************************************************)
(*                       EDITING THE DIRECTORY PERMISSIONS                      *)
(********************************************************************************)

PROCEDURE PermissionsToString (flags: PermissionSet;
                                 VAR (*OUT*) result: ARRAY OF CHAR);

    (* Turns the flags into a five-character string.  We assume that result is  *)
    (* at least five characters long, and we don't add a string terminator.     *)

    BEGIN
        IF Visible IN flags THEN result[0] := 'V' ELSE result[0] := ' ' END(*IF*);
        IF AllowRead IN flags THEN result[1] := 'R' ELSE result[1] := ' ' END(*IF*);
        IF AllowWrite IN flags THEN result[2] := 'W' ELSE result[2] := ' ' END(*IF*);
        IF AllowDelete IN flags THEN result[3] := 'D' ELSE result[3] := ' ' END(*IF*);
        IF AllowRename IN flags THEN result[4] := 'N' ELSE result[4] := ' ' END(*IF*);
    END PermissionsToString;

(********************************************************************************)

PROCEDURE DirNameToText (p: DirEntryPtr;  VAR (*OUT*) Entry: ARRAY OF CHAR);

    (* Creates a text string containing the permissions, collapsed marker, and  *)
    (* directory name for this node.                                            *)

    VAR j: CARDINAL;

    BEGIN
        PermissionsToString (p^.flags, Entry);
        FOR j := 5 TO p^.indent+7 DO
            IF j <= MAX(FileNameSubscript) THEN
                Entry[j] := ' ';
            END (*IF*);
        END (*FOR*);
        IF p^.IsAFile THEN
            Entry[6] := '#';
        ELSIF NOT p^.exists THEN
            Entry[6] := '?';
        ELSIF p^.collapsed THEN
            Entry[6] := '+';
        END (*IF*);
        IF p^.virtual THEN
            Entry[7] := '*';
        END (*IF*);
        IF p^.indent+8 <= HIGH(Entry) THEN
            Entry[p^.indent+8] := Nul;
        END (*IF*);
        Strings.Append (p^.name, Entry);
    END DirNameToText;

(********************************************************************************)

PROCEDURE EditNodeName (current: DirEntryPtr;  Screen: VirtualScreen);

    (* Allows editing of the name and (if virtual) the path of current. *)

    VAR PPath, DerivedPath, EditedPath: FileNameString;
        w1: Window;  IsVirtual: BOOLEAN;

    (****************************************************************************)

    PROCEDURE ShowPath;

        (* Sets PPath, displays resulting value of PPath. *)

        BEGIN
            IF IsVirtual THEN
                PPath := EditedPath;
            ELSE
                PPath := DerivedPath;
            END (*IF*);
            SetCursor (w1, 5, 3);
            WriteString (w1, PPath);
            EraseLine (w1, 1);
        END ShowPath;

    (****************************************************************************)

    VAR NewName, VPath, ParentPPath, ParentVPath: FileNameString;
        item, olditem: [0..3];  k: CARDINAL;  ch: CHAR;
        NameChanged, PathChanged: BOOLEAN;

    BEGIN
        NewName := current^.name;
        EditedPath := current^.ppath;
        VPath := current^.vpath;
        IF current^.parent = NIL THEN
            ParentVPath := "";  ParentPPath := "";  DerivedPath := "";
            current^.virtual := TRUE;
        ELSE
            ParentVPath := current^.parent^.vpath;
            ParentPPath := current^.parent^.ppath;
            DerivedPath := ParentPPath;  Strings.Append ('/', DerivedPath);
            Strings.Append (NewName, DerivedPath);
        END (*IF*);
        IsVirtual := current^.virtual;

        OpenWindowHidden (w1, white, black, 10, 16,
              13, 67, simpleframe, nodivider);
        MapToVirtualScreen (w1, Screen);
        SetCursor (w1, 1, 8);
        WriteString (w1, "Type Esc when you have finished editing");
        SetCursor (w1, 2, 2);  WriteString (w1, "Name");
        SetCursor (w1, 3, 2);  WriteString (w1, "Link");
        SetCursor (w1, 4, 2);  WriteString (w1, "Path");

        item := 1;  ShowPath;
        REPEAT
            SetCursor (w1, 3, 10);
            IF IsVirtual THEN
                WriteString (w1, "    yes");
            ELSE
                WriteString (w1, "no     ");
            END (*IF*);

            CASE item OF
              | 1:   (* Edit the node name. *)

                     SetCursor (w1, 2, 8);
                     EditString (w1, NewName, MAX(FileNameSubscript)+1, 44);
                     IF current^.parent <> NIL THEN
                         VPath := ParentVPath;  Strings.Append ('/', VPath);
                         Strings.Append (NewName, VPath);
                         DerivedPath := ParentPPath;
                         Strings.Append ('/', DerivedPath);
                         Strings.Append (NewName, DerivedPath);
                     END (*IF*);

              | 2:   (* User may modify the "virtual" flag. *)

                     SetCursor (w1, 3, 10);  WriteString (w1, "no  yes");
                     IF IsVirtual THEN k := 14 ELSE k := 10
                     END (*IF*);
                     ColourSwap (w1, 3, k, 3);
                     ch := GetKey(w1);
                     IF ch = Nul THEN
                         ch := GetKey(w1);
                         IF ch = "K" THEN
                             IsVirtual := FALSE;
                             ColourSwap (w1, 3, k, 3);
                             ColourSwap (w1, 3, 10, 3);
                         ELSIF ch = "M" THEN
                             IsVirtual := TRUE;
                             ColourSwap (w1, 3, k, 3);
                             ColourSwap (w1, 3, 14, 3);
                         ELSE PutBack(ch);  PutBack(Nul);
                         END (*IF*);
                     ELSE
                         PutBack (ch);
                     END (*IF*);

              | 3:   (* Edit the physical path. *)

                     SetCursor (w1, 5, 3);
                     EditString (w1, EditedPath, MAX(FileNameSubscript)+1, 49);
                     k := LENGTH(EditedPath);
                     IF k > 0 THEN
                         DEC (k);
                         IF (EditedPath[k] = '/') OR (EditedPath[k] = '\') THEN
                             EditedPath[k] := Nul;
                         END (*IF*);
                     END (*IF*);

            END (*CASE*);

            ShowPath;
            olditem := item;
            ch := GetKey(w1);
            IF ch = Esc THEN item := 0;
            ELSIF ch = CR THEN                     (* return *)
                item := item MOD 3 + 1;
            ELSIF ch = Nul THEN
                ch := GetKey(w1);
                IF ch = "H" THEN                   (* cursor up *)
                    DEC (item);
                    IF item = 0 THEN
                        item := 3;
                    END (*IF*);
                ELSIF ch = "P" THEN                (* cursor down *)
                    item := item MOD 3 + 1;
                ELSIF (item = 2) AND ((ch = 'K') OR (ch = 'M')) THEN
                    PutBack (ch);  PutBack (Nul);
                END (*IF*);
            END (*IF*);
            IF (item = 3) AND NOT IsVirtual THEN
                item := 2;
            END (*IF*);
            IF (item = 2) AND (current^.parent = NIL) THEN
                item := 4 - olditem;
            END (*IF*);

        UNTIL item = 0;

        CloseWindow (w1);

        current^.virtual := IsVirtual;
        NameChanged := NOT NameMatch (NewName, current^.name);
        PathChanged := NOT NameMatch (PPath, current^.ppath);
        IF NameChanged OR PathChanged THEN
            current^.name := NewName;
            current^.ppath := PPath;
            current^.vpath := VPath;
            IF PathChanged THEN
                RemoveObsoleteNodes (current);
            END (*IF*);
            IF NameChanged THEN
                RecomputeVPath (current);
            END (*IF*);
        END (*IF*);

    END EditNodeName;

(****************************************************************************)

PROCEDURE EditPermissionListBox (root: DirEntryPtr;  w: Window;  LB: ListBox;
                                                             BoxHeight: CARDINAL);

    (* This is the editor for directory permissions.  On entry ListBox LB has   *)
    (* already been filled and its contents displayed on the screen.  This      *)
    (* procedure allows the user to walk through the tree via the listbox,      *)
    (* modifying nodes.  Parameter "root" can afford to be a value parameter    *)
    (* because the root node is never replaced (though some of the fields in    *)
    (* the root record might change).  Nodes below the root might, however,     *)
    (* be added or deleted.                                                     *)

    VAR current: DirEntryPtr;
        RowBuffer: FileNameString;
        prompt: Window;
        Screen: VirtualScreen;

    (****************************************************************************)

    PROCEDURE RedisplaySubtree;

        (* Rebuilds the listbox contents, for the subtree headed by current.    *)
        (* On exit current is the first entry beyond that subtree in the        *)
        (* listbox - unless we reach the end of the list, in which case         *)
        (* current is the last entry.                                           *)

        VAR child: DirEntryPtr;

        BEGIN
            child := current^.FirstChild;
            IF current^.collapsed OR (child = NIL) THEN
                IF current^.right <> NIL THEN
                    current := current^.right;
                    EVAL (CursorForward(LB));
                END (*IF*);
            ELSE
                EVAL (CursorForward(LB));
                WHILE child <> NIL DO
                    current := child;
                    DirNameToText (child, RowBuffer);
                    ReplaceCurrent (LB, RowBuffer);
                    RedisplaySubtree;
                    child := child^.next;
                END (*WHILE*);
            END (*IF*);
        END RedisplaySubtree;

    (****************************************************************************)

    PROCEDURE StepBackward(): BOOLEAN;

        (* Moves one step back in the list, or returns FALSE if this would      *)
        (* take us off the top.                                                 *)

        BEGIN
            IF CursorBackward (LB) THEN
                current := current^.left;
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        END StepBackward;

    (****************************************************************************)

    PROCEDURE StepForward;

        (* Moves one step forward in the list, if possible. *)

        BEGIN
            EVAL (CursorForward (LB));
            IF current^.right <> NIL THEN
                current := current^.right;
            END (*IF*);
        END StepForward;

    (****************************************************************************)

    PROCEDURE CollapseSubtree;

        (* Takes the subtrees of current out of the listbox display, and        *)
        (* readjusts left and right thread pointers.  The screen cursor is      *)
        (* not moved.                                                           *)

        (************************************************************************)

        PROCEDURE HideChildren (node: DirEntryPtr);

            (* Takes the subtrees of node out of the listbox display.  The      *)
            (* screen cursor should be on node on entry; this procedure does    *)
            (* not move it.                                                     *)
            (* Assumption: the parent is uncollapsed.                           *)

            VAR child: DirEntryPtr;

            BEGIN
                child := node^.FirstChild;
                WHILE child <> NIL DO
                    LBDeleteNext (LB);
                    IF NOT child^.collapsed THEN
                        HideChildren (child);
                    END (*IF*);
                    child := child^.next;
                END (*WHILE*);
            END HideChildren;

        (************************************************************************)
        (*                      Body of CollapseSubtree                         *)
        (************************************************************************)

        BEGIN
            IF NOT current^.collapsed THEN
                ToggleCollapsedState (current);
                DirNameToText (current, RowBuffer);
                ReplaceCurrent (LB, RowBuffer);
                IF current^.FirstChild <> NIL THEN
                    HideChildren (current);
                END (*IF*);
            END (*IF*);
        END CollapseSubtree;

    (****************************************************************************)

    PROCEDURE UncollapseSubtree;

        (* Puts the subtrees of current back into the listbox display, and      *)
        (* readjusts left and right thread pointers.  Side-effect: the cursor   *)
        (* moves to the last reinstated child.                                  *)

        (************************************************************************)

        PROCEDURE ReinstateChildren (node: DirEntryPtr): DirEntryPtr;

            (* Puts the subtrees of node back into the listbox display. *)
            (* Returns a pointer to the last non-collapsed node.        *)

            VAR child, tail: DirEntryPtr;

            BEGIN
                child := node^.FirstChild;  tail := node;
                WHILE child <> NIL DO
                    DirNameToText (child, RowBuffer);
                    LBInsertAfter (LB, RowBuffer);
                    IF child^.collapsed OR (child^.FirstChild = NIL) THEN
                        tail := child;
                    ELSE
                        tail := ReinstateChildren (child);
                    END (*IF*);
                    child := child^.next;
                END (*WHILE*);
                RETURN tail;
            END ReinstateChildren;

        (************************************************************************)
        (*                     Body of UncollapseSubtree                        *)
        (************************************************************************)

        BEGIN
            IF current^.collapsed THEN
                Assert (LinksAreConsistent (root));
                ToggleCollapsedState (current);
                DirNameToText (current, RowBuffer);
                ReplaceCurrent (LB, RowBuffer);
                Assert (LinksAreConsistent (root));
                ExpandNode (current);
                Assert (LinksAreConsistent (root));
                current := ReinstateChildren (current);
                Assert (LinksAreConsistent (root));
            END (*IF*);
        END UncollapseSubtree;

    (****************************************************************************)

    PROCEDURE DisplayPaths;

        (* Screen display of current physical and virtual paths. *)

        BEGIN
            IF current <> NIL THEN
                SetCursor (prompt, 0, 10);
                WriteString (prompt, current^.ppath);  EraseLine (prompt, 1);
                SetCursor (prompt, 1, 10);
                IF current^.vpath[0] = Nul THEN
                    WriteString (prompt, '/');
                ELSE
                    WriteString (prompt, current^.vpath);
                END (*IF*);
                EraseLine (prompt, 1);
            END (*IF*);
        END DisplayPaths;

    (****************************************************************************)
    (*                      Body of EditPermissionListBox                       *)
    (****************************************************************************)

    VAR bottombar, ExtraBottomBar: Window;  ch: CHAR;
        perm: Permission;  father, temp: DirEntryPtr;
        j: CARDINAL;
        NodeIsEditable, WasCollapsed: BOOLEAN;

    BEGIN
        w := WindowOf(LB);  Screen := VirtualScreenOf(w);
        IF root = NIL THEN
            OpenWindowHidden (bottombar, yellow, red, 14, 15,
                  23, 53, noframe, nodivider);
            MapToVirtualScreen (bottombar, Screen);
            WriteString (bottombar, "Home directory does not exist.");
            PressAnyKey (bottombar);
            CloseWindow (bottombar);
            RETURN;
        END (*IF*);

        OpenWindowHidden (bottombar, yellow, red, ScreenRows-2, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (bottombar, Screen);
        WriteString (bottombar, " VRWDN (toggle)  I inherit  P propagate  + expand  - collapse");
        WriteLn (bottombar);
        WriteString (bottombar, "   A add child");
        SetCursor (bottombar, 0, 72);  WriteString (bottombar, "B back");
        SetCursor (bottombar, 1, 72);  WriteString (bottombar, "X exit");

        OpenWindowHidden (ExtraBottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 16, 45, noframe, nodivider);
        MapToVirtualScreen (ExtraBottomBar, Screen);
        WriteString (ExtraBottomBar, "E edit  Del delete");

        (* Create the label window. *)

        OpenWindowHidden (prompt, yellow, red, 1, 2, 0, 79, noframe, nodivider);
        MapToVirtualScreen (prompt, Screen);
        SetCursor (prompt, 0, 0);  WriteString (prompt, "Physical:");
        SetCursor (prompt, 1, 0);  WriteString (prompt, "Virtual :");

        HighlightOn (LB);
        current := root;
        LOOP
            Assert (LinksAreConsistent(root));
            DisplayPaths;
            NodeIsEditable := current^.virtual OR NOT current^.exists;
            IF NodeIsEditable THEN
                PutOnTop (ExtraBottomBar);
            ELSE
                Hide (ExtraBottomBar);
            END (*IF*);
            ch := GetKey(w);
            IF ch = CHR(0) THEN
                ch := GetKey(w);
                IF ch = "H" THEN                        (* cursor up *)
                    EVAL (StepBackward());
                ELSIF ch = "P" THEN                     (* cursor down *)
                    StepForward;
                ELSIF ch = "G" THEN                     (* home *)
                    DisableScreenOutput (LB);
                    WHILE StepBackward() DO
                    END (*WHILE*);
                    Repaint (LB);
                ELSIF ch = "O" THEN                     (* end *)
                    DisableScreenOutput (LB);
                    WHILE current^.right <> NIL DO
                        StepForward;
                    END (*WHILE*);
                    Repaint (LB);
                ELSIF ch = "I" THEN                     (* page up *)
                    DisableScreenOutput (LB);
                    FOR j := 1 TO BoxHeight-1 DO
                        EVAL (StepBackward());
                    END (*FOR*);
                    Repaint (LB);
                ELSIF ch = "Q" THEN                     (* page down *)
                    DisableScreenOutput (LB);
                    FOR j := 1 TO BoxHeight-1 DO
                        StepForward;
                    END (*FOR*);
                    Repaint (LB);
                ELSIF ch = 'S' THEN                      (* Del = delete *)
                    IF NodeIsEditable AND (current^.parent <> NIL) THEN
                        DisableScreenOutput (LB);
                        CollapseSubtree;
                        temp := current^.right;
                        IF temp = NIL THEN temp := current^.left END(*IF*);
                        DeleteSubtree (current);
                        LBDeleteCurrent(LB);
                        Repaint(LB);
                        current := temp;
                    END (*IF*);
                END (*IF*);
            ELSIF CAP(ch) IN CharSet {'V','R','W','D','N'} THEN
                perm := CharToPermission (ch);
                IF perm IN current^.flags THEN EXCL(current^.flags, perm)
                ELSE INCL(current^.flags, perm);
                END (*IF*);
                DirNameToText (current, RowBuffer);
                ReplaceCurrent (LB, RowBuffer);
            ELSIF CAP(ch) = 'A' THEN                    (* A = add *)
                DisableScreenOutput (LB);
                father := current;
                UncollapseSubtree;
                 Assert (LinksAreConsistent(root));
                AppendChild (father, "", TRUE, TRUE);
                 Assert (LinksAreConsistent(root));
                WHILE current <> father^.LastChild^.left DO
                    StepForward;
                END (*WHILE*);
                current := current^.right;
                current^.virtual := TRUE;  current^.ppath := "";
                 Assert (LinksAreConsistent(root));
                DirNameToText (current, RowBuffer);
                LBInsertAfter (LB, RowBuffer);
                Repaint (LB);
                PutBack ('E');
            ELSIF (ch = Esc) OR (CAP(ch) = 'B') THEN    (* B = back *)
                EXIT (*LOOP*);
            ELSIF CAP(ch) = 'E' THEN                    (* E = edit *)
                IF NodeIsEditable THEN
                    DisableScreenOutput (LB);
                    WasCollapsed := current^.collapsed;
                    CollapseSubtree;
                    EditNodeName (current, Screen);
                    DirNameToText (current, RowBuffer);
                    ReplaceCurrent (LB, RowBuffer);
                    IF NOT WasCollapsed THEN
                        UncollapseSubtree;
                    END (*IF*);
                    Repaint (LB);
                END (*IF*);
            ELSIF CAP(ch) = 'I' THEN                    (* I = inherit *)
                IF current^.parent = NIL THEN
                    current^.flags := PermissionSet {Visible, AllowRead}
                ELSE
                    current^.flags := current^.parent^.flags;
                END (*IF*);
                DirNameToText (current, RowBuffer);
                ReplaceCurrent (LB, RowBuffer);
            ELSIF CAP(ch) = 'P' THEN                    (* P = propagate *)
                CopyDownPermissions (current);
                RedisplaySubtree;
            ELSIF (ch = CR) OR (CAP(ch) = '+') THEN     (* + = expand *)
                IF current^.collapsed THEN
                    DisableScreenOutput (LB);
                    UncollapseSubtree;
                    Repaint (LB);
                END (*IF*);
                StepForward;
            ELSIF CAP(ch) = '-' THEN                    (* - = collapse *)
                IF NOT current^.collapsed THEN
                    DisableScreenOutput (LB);
                    CollapseSubtree;
                    Repaint (LB);
                END (*IF*);
                StepForward;
            ELSIF CAP(ch) = 'X' THEN                    (* X = exit *)
                PutBack ('X');  PutBack (CHR(27));
                EXIT (*LOOP*);
            END (*IF*);

        END (*LOOP*);

        HighlightOff (LB);
        CloseWindow (prompt);
        CloseWindow (ExtraBottomBar);
        CloseWindow (bottombar);

    END EditPermissionListBox;

(********************************************************************************)

PROCEDURE ShowTree (LB: ListBox;  p: DirEntryPtr;  indent: CARDINAL);

    VAR Entry: FileNameString;

    BEGIN
        IF p <> NIL THEN
            p^.indent := indent;
            DirNameToText (p, Entry);
            LBAppend (LB, Entry);
            p := p^.FirstChild;
            WHILE p <> NIL DO
                ShowTree (LB, p, indent+3);
                p := p^.next;
            END (*WHILE*);
        END (*IF*);
    END ShowTree;

(********************************************************************************)

PROCEDURE EditDirectoryData (root: DirEntryPtr;  Screen: VirtualScreen);

    (* Allows the user to edit the directory tree. *)

    CONST BoxTop = 3;  BoxLeft = 0;  BoxWidth = 77;

    VAR LBWindow: Window;
        LB: ListBox;
        BoxHeight: CARDINAL;

    BEGIN
        BoxHeight := ScreenRows - BoxTop - 4;

        (* Set up the listbox of directory permissions.  *)

        OpenWindowHidden (LBWindow, white, blue, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+2, noframe, nodivider);
        MapToVirtualScreen (LBWindow, Screen);
        LB := CreateListBox (LBWindow, 1, 1, BoxHeight, BoxWidth);

        (* Load the listbox with the directory information. *)

        SetUpInitialTree (root);
        ClearListBox (LB);
        ShowTree (LB, root, 1);

        (* Edit the directory information. *)

        EditPermissionListBox (root, LBWindow, LB, BoxHeight);

        (* Tidy up. *)

        CleanUpTree (root);
        DestroyListBox (LB);
        CloseWindow (LBWindow);

    END EditDirectoryData;

(********************************************************************************)

PROCEDURE DirectorySummary (w: Window;  root: DirEntryPtr;  rows: CARDINAL);

    (* Writes an overview of the tree to window w.  The third parameter *)
    (* specifies how much space is available in the window.             *)

    VAR k: CARDINAL;  p: DirEntryPtr;

    BEGIN
        ClearWindow (w);  SetCursor (w, 1, 1);
        IF root^.ppath[0] = Nul THEN
            (* Top-level directory is a pseudo-directory, so give the   *)
            (* summary at one level down.                               *)
            p := root^.FirstChild;
            IF (p = NIL) OR (p^.ppath[0] = Nul) THEN
                WriteString (w, "A root directory has not yet been set");
            ELSE
                WriteString (w, "Summary of top-level directories:");
            END (*IF*);
        ELSE
            p := root;
            WriteString (w, "User's root directory:");
        END (*IF*);

        k := 3;
        LOOP
            SetCursor (w, k, 2);
            IF p = NIL THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF k >= rows-1 THEN
                WriteString (w, "   (listing truncated by lack of screen space)");
                EXIT (*LOOP*);
            END (*IF*);
            WriteString (w, p^.ppath);
            p := p^.next;  INC(k);
        END (*LOOP*);

    END DirectorySummary;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
END PermissionEditor.

