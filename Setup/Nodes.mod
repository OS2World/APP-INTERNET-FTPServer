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

IMPLEMENTATION MODULE Nodes;

        (************************************************************)
        (*                                                          *)
        (*                  PM Setup for FtpServer                  *)
        (*       Dialogue to edit the data for one tree node        *)
        (*                                                          *)
        (*    Started:        26 November 1999                      *)
        (*    Last edited:    7 January 2013                        *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, Remote, DID, Strings;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM Directories IMPORT
    (* const*)  FileNameLength,
    (* type *)  FileName;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    NodePtr = POINTER TO NodeData;

    (* Explanation of some of the fields in a NodeEntry record:                 *)
    (*                                                                          *)
    (*   virtual       this record is for a symbolic link                       *)
    (*   ppath         full physical path name                                  *)

    NodeData = RECORD
                   virtual: BOOLEAN;
                   father: NodePtr;
                   ppath: FileName;
                   name: FileName;
               END (*RECORD*);

VAR
    ArgPtr: NodePtr;

(************************************************************************)
(*                    CREATING AND DESTROYING A NODE                    *)
(************************************************************************)

PROCEDURE CreateNode (NodeName: ARRAY OF CHAR;  parent: NodePtr): NodePtr;

    (* Creates a new node, making it virtual iff parent = NIL. *)

    VAR result: NodePtr;

    BEGIN
        NEW (result);
        WITH result^ DO
            Strings.Assign (NodeName, name);
            father := parent;
            IF parent = NIL THEN
                virtual := TRUE;
                ppath := "";
            ELSE
                virtual := FALSE;
                Strings.Assign (parent^.ppath, ppath);
            END (*IF*);
            IF ppath[0] <> Nul THEN
                Strings.Append ('\', ppath);
            END (*IF*);
            Strings.Append (NodeName, ppath);
        END (*WITH*);
        RETURN result;
    END CreateNode;

(************************************************************************)

PROCEDURE DetachFromParent (pnode: NodePtr);

    (* Sets the parent of this node to NIL, leaves all other properties *)
    (* unchanged.                                                       *)

    BEGIN
        pnode^.father := NIL;
    END DetachFromParent;

(************************************************************************)

PROCEDURE DiscardNode (VAR (*INOUT*) pnode: NodePtr);

    (* Destroys the node. *)

    BEGIN
        DISPOSE (pnode);
    END DiscardNode;

(************************************************************************)
(*                      BASIC PROPERTIES OF A NODE                      *)
(************************************************************************)

PROCEDURE NameOf (pnode: NodePtr;  VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the name of the node. *)

    BEGIN
        Strings.Assign (pnode^.name, name);
    END NameOf;

(************************************************************************)

PROCEDURE IsVirtual (pnode: NodePtr): BOOLEAN;

    (* Returns TRUE iff this is a pseudo-directory or link. *)

    BEGIN
        RETURN pnode^.virtual;
    END IsVirtual;

(************************************************************************)

PROCEDURE QueryPhysicalPath (pnode: NodePtr;  VAR (*OUT*) path: ARRAY OF CHAR);

    (* Returns the physical path of this node. *)

    BEGIN
        Strings.Assign (pnode^.ppath, path);
    END QueryPhysicalPath;

(************************************************************************)

PROCEDURE MakeVirtual (pnode: NodePtr;  path: ARRAY OF CHAR);

    (* Makes this node virtual, and sets its physical path. *)

    BEGIN
        pnode^.virtual := TRUE;
        Strings.Assign (path, pnode^.ppath);
    END MakeVirtual;

(************************************************************************)
(*                STORING DIALOGUE DATA BACK INTO THE NODE              *)
(************************************************************************)

PROCEDURE CommitChanges (dlg: OS2.HWND);

    (* Updates ArgPtr^ from the data in the dialogue. *)

    VAR PseudoNode: BOOLEAN;  k: CARDINAL;

    BEGIN
        OS2.WinQueryDlgItemText (dlg, DID.NodeName, FileNameLength, ArgPtr^.name);
        ArgPtr^.virtual := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (dlg, DID.NodeSubdir,
                                           OS2.BM_QUERYCHECK, NIL, NIL)) = 0;
        PseudoNode := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (dlg, DID.NodePseudo,
                                           OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF PseudoNode THEN
            ArgPtr^.ppath := "";
        ELSIF ArgPtr^.virtual THEN

            OS2.WinQueryDlgItemText (dlg, DID.NodePath, FileNameLength, ArgPtr^.ppath);

        ELSIF ArgPtr^.father = NIL THEN

            (* We force root of tree to be virtual. *)

            MakeVirtual (ArgPtr, ArgPtr^.name);

        ELSE

            (* Non-virtual node, inherit path from father. *)

            ArgPtr^.ppath := ArgPtr^.father^.ppath;
            Strings.Append ('\', ArgPtr^.ppath);
            Strings.Append (ArgPtr^.name, ArgPtr^.ppath);

        END (*IF*);

        (* Get rid of any trailing '\' or '/' in the physical path.  *)

        k := LENGTH (ArgPtr^.ppath);
        IF k > 0 THEN
            DEC (k);
            IF (ArgPtr^.ppath[k] = '/') OR (ArgPtr^.ppath[k] = '\') THEN
                ArgPtr^.ppath[k] := Nul;
            END (*IF*);
        END (*IF*);

    END CommitChanges;

(************************************************************************)
(*                       THE NODE DATA DIALOGUE                         *)
(************************************************************************)

PROCEDURE SetPathVisibility (dlg: OS2.HWND;  show: BOOLEAN);

    (* Shows or hides the "path" dialogue item, depending on "show".    *)

    BEGIN
        IF show THEN
            OS2.WinShowWindow (OS2.WinWindowFromID (dlg, DID.NodePath), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID (dlg, DID.NodePathLabel), TRUE);
        ELSE
            OS2.WinShowWindow (OS2.WinWindowFromID (dlg, DID.NodePath), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID (dlg, DID.NodePathLabel), FALSE);
        END (*IF*);
    END SetPathVisibility;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    CONST BoxID = 999;

    VAR NotificationCode, ButtonID: CARDINAL;  ch: CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   IF ArgPtr <> NIL THEN
                       IF ArgPtr^.father = NIL THEN
                           (*IF NOT ArgPtr^.virtual OR (ArgPtr^.ppath[0] <> Nul) THEN*)
                               OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.NodePath));
                           (*END (*IF*);*)
                       ELSE
                           OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.NodeName));
                       END (*IF*);
                   END (*IF*);
                   RETURN NIL;

           |  OS2.WM_CHAR:
                   (* Stop alphabetic characters from leaking through to parent. *)
                   (* Also catch '+' and '-'.                                    *)

                   ch := OS2.CHAR1FROMMP (mp2);
                   IF ((CAP(ch) >= 'A') AND (CAP(ch) <= 'Z'))
                              OR (ch = '+') OR (ch = '-') THEN
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.NodeOK:
                          CommitChanges (hwnd);
                          OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                          RETURN NIL;

                     | DID.NodeCancel:
                          OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                          RETURN NIL;

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

          |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF (NotificationCode = OS2.BN_CLICKED) AND
                            (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, ButtonID,
                                                 OS2.BM_QUERYCHECK, NIL, NIL)) <> 0) THEN
                       CASE ButtonID OF

                         | DID.NodeSubdir:
                             SetPathVisibility (hwnd, FALSE);
                             RETURN NIL;
                         | DID.NodeLink:
                             SetPathVisibility (hwnd, TRUE);
                             RETURN NIL;
                         | DID.NodePseudo:
                             SetPathVisibility (hwnd, FALSE);
                             RETURN NIL;

                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*CASE*);

                   END (*IF*);

                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  VAR (*INOUT*) node: NodePtr);

    (* Edit the properties of a single tree node.  *)

    VAR hwnd: OS2.HWND;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.TreeNodeDialogue,                (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "EditNode");
        OS2.WinSetDlgItemText (hwnd, DID.NodePath, node^.ppath);
        IF node^.father = NIL THEN
            OS2.WinSetDlgItemText (hwnd, DID.NodeName, "/");
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.NodeName), FALSE);
            node^.name := "";
            IF NOT node^.virtual OR (node^.ppath[0] <> Nul) THEN
                OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.NodePath));
            END (*IF*);
        ELSE
            OS2.WinSetDlgItemText (hwnd, DID.NodeName, node^.name);
            OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.NodeName));
        END (*IF*);
        IF node^.virtual THEN
            IF node^.ppath[0] = Nul THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.NodePseudo, OS2.BM_SETCHECK,
                                       OS2.MPFROMSHORT(1), NIL);
            ELSE
                OS2.WinSendDlgItemMsg (hwnd, DID.NodeLink, OS2.BM_SETCHECK,
                                       OS2.MPFROMSHORT(1), NIL);
            END (*IF*);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.NodeSubdir, OS2.BM_SETCHECK,
                                   OS2.MPFROMSHORT(1), NIL);
        END (*IF*);
        ArgPtr := node;
        SetPathVisibility (hwnd, ArgPtr^.virtual AND (ArgPtr^.ppath[0] <> Nul));

        OS2.WinProcessDlg(hwnd);

        Remote.StoreWindowPosition (hwnd, "EditNode", TRUE);
        OS2.WinDestroyWindow (hwnd);
    END Edit;

(************************************************************************)

END Nodes.

