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

IMPLEMENTATION MODULE EditUsers;

        (********************************************************)
        (*                                                      *)
        (*             FtpServer user editor                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            22 January 1998                 *)
        (*  Last edited:        22 January 2014                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT INIData, Strings;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen,
                RemoveVirtualScreen, RemoveScreenGroup;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, DestroyListBox, CursorMovements,
                LBAppend, LBCurrent, HighlightOn, LBDeleteCurrent,
                LBInsertAfter, ReplaceCurrent, LBSort;

FROM Users IMPORT
    (* proc *)  EditUser, RemoveUser;

FROM PermissionEditor IMPORT
    (* type *)  CharArrayPointer;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor, SetActivePage,
                WriteString, WriteLn, GetKey, GetScreenSize;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    NameStringIndex = [0..31];
    NameString = ARRAY NameStringIndex OF CHAR;

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* The screen group for our screen page.  *)

    OurGroup: ScreenGroup;

    (* The screen page used by this module. *)

    OurPage: VirtualScreen;

    (* Header and footer windows. *)

    TopBar, BottomBar: Window;

(************************************************************************)
(*                  DISPLAYING THE LIST OF USERS                        *)
(************************************************************************)

PROCEDURE FillList (LB: ListBox);

    (* Adds all known users to the listbox. *)

    VAR hini: INIData.HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        name: NameString;
        j, k: CARDINAL;
        null: ARRAY [0..0] OF CHAR;

    BEGIN
        (* Get the list of users from the INI file. *)

        hini := OurINIHandle();
        null := '';
        IF INIData.INIValid (hini)
                  AND INIData.ItemSize (hini, null, '', BufferSize)
                  AND (BufferSize > 0) THEN
            ALLOCATE (bufptr, BufferSize);
            IF INIData.INIGetTrusted (hini, null, null, bufptr^, BufferSize) THEN
                j := 0;

                (* Each time around this loop we extract one user name. *)

                WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO
                    k := 0;
                    REPEAT
                        name[k] := bufptr^[j];
                        INC (k);  INC (j);
                    UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                            OR (k > MAX(NameStringIndex));
                    IF NOT Strings.Equal (name, "$SYS") THEN
                        LBAppend (LB, name);
                    END (*IF*);
                END (*WHILE*);
            END (*IF*);
            DEALLOCATE (bufptr, BufferSize);
            LBSort (LB);

        END (*IF*);
    END FillList;

(************************************************************************)
(*                          MAIN EDITING TASK                           *)
(************************************************************************)

PROCEDURE UserEditor;

    (* Runs as an autonomous task. *)

    CONST Esc = CHR(1BH);
        BoxTop = 2;  BoxLeft = 2;  BoxWidth = 35;

    VAR ListWindow: Window;  UserList: ListBox;
        ch: CHAR;
        UserName: ARRAY [0..BoxWidth-1] OF CHAR;
        BoxHeight: CARDINAL;

    BEGIN
        BoxHeight := ScreenRows - BoxTop - 4;

        OpenWindowHidden (ListWindow, black, white, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+2, noframe, nodivider);
        MapToVirtualScreen (ListWindow, OurPage);
        UserList := CreateListBox (ListWindow, 1, 1, BoxHeight, BoxWidth);
        FillList (UserList);
        HighlightOn (UserList);

        LOOP
            EVAL (CursorMovements (UserList));
            ch := GetKey(ListWindow);
            IF ch = Nul THEN
                ch := GetKey(ListWindow);
                IF ch = 'S' THEN                      (* Del = delete *)
                    LBCurrent (UserList, UserName);
                    RemoveUser (UserName);
                    LBDeleteCurrent (UserList);
                END (*IF*);
            ELSIF CAP(ch) = 'A' THEN                       (* A = add *)
                UserName := "";
                EditUser (OurPage, UserName, FALSE);
                LBInsertAfter (UserList, UserName);
            ELSIF CAP(ch) = 'C' THEN                       (* C = clone *)
                LBCurrent (UserList, UserName);
                EditUser (OurPage, UserName, TRUE);
                LBInsertAfter (UserList, UserName);
            ELSIF (CAP(ch) = 'E') OR (ch = CHR(13)) THEN   (* E = edit *)
                LBCurrent (UserList, UserName);
                EditUser (OurPage, UserName, FALSE);
                ReplaceCurrent (UserList, UserName);
            ELSIF (CAP(ch) = 'X') OR (ch = Esc) THEN       (* X = exit *)
                SetActivePage (0);
                StuffKeyboardBuffer (CHR(27));
            END (*IF*);
        END (*LOOP*);

    END UserEditor;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

PROCEDURE StartUserEditor(): BOOLEAN;

    (* Initialisation of the Edit Users page. *)

    BEGIN
        RETURN CreateTask (UserEditor, 3, "User editor");
    END StartUserEditor;

(************************************************************************)

PROCEDURE CreateHeadings;

    BEGIN
        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);
        WriteString (TopBar, "    FTPSERVER USER EDITOR");
        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, OurPage);
        WriteString (BottomBar, " A add  C clone  E edit  Del delete  X exit");
        SetCursor (BottomBar, 0, 55);
        WriteString (BottomBar, "F4,F5 previous/next page");
    END CreateHeadings;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    OurGroup := CreateScreenGroup (1);
    OurPage := CreateVirtualScreen (OurGroup);
    CreateHeadings;
FINALLY
    CloseWindow (BottomBar);
    CloseWindow (TopBar);
    RemoveVirtualScreen (OurPage);
    RemoveScreenGroup (OurGroup);
END EditUsers.

