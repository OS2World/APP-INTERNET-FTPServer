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

IMPLEMENTATION MODULE Users;

        (********************************************************)
        (*                                                      *)
        (*         Editor for the user access rights            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 December 1997                 *)
        (*  Last edited:        8 January 2013                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT ADDRESS, ADR, CAST;

IMPORT INIData, Strings;

FROM PermissionEditor IMPORT
    (* type *)  DirEntryPtr, CharArrayPointer,
    (* proc *)  LoadTreeData, SizeOfDirectoryData, StoreDirectoryData,
                KillList, CreateDefaultRoot, EditDirectoryData,
                DirectorySummary;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM INIData IMPORT
    (* proc *)  INIGet, INIGetString;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, DestroyListBox;

FROM InetUtilities IMPORT
    (* proc *)  ToLower;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  CreateField, MenuField, CardinalField, StringField,
                Combine, ScreenEdit, DeleteStructure;

FROM FieldEditor IMPORT
    (* type *)  FieldType,
    (* proc *)  DefineFieldType, DiscardFieldType;

FROM Menus IMPORT
    (* type *)  Menu, ItemText,
    (* proc *)  CreateMenu, MenuColours;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, WriteChar, WriteString, WriteLn,
                SaveCursor, SetCursor, GetKey, EditString, GetScreenSize, ColourSwap,
                ReadCharWithoutEcho;

FROM MultiScreen IMPORT
    (* type *)  VirtualScreen,
    (* proc *)  MapToVirtualScreen, EnableScreenSwitching;

FROM Keyboard IMPORT
    (* proc *)  PutBack;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    FileNameSubscript = [0..255];
    FileNameString = ARRAY FileNameSubscript OF CHAR;
    PassString = ARRAY [0..31] OF CHAR;
    PassStringPtr = POINTER TO PassString;
    UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser, NormalUser, Manager);
    CategoryMapType = ARRAY UserCategory OF CHAR;

CONST CategoryMap = CategoryMapType {'?', 'N', 'G', 'U', 'M'};

    (* The fields in a UserPermission record have the following meanings.       *)
    (*      category     What sort of user this is                              *)
    (*      Password     The user's password                                    *)
    (*      UserLimit    Maximum number of instances of this user               *)
    (*      SpeedLimit   Rate limit (bytes per second)                          *)
    (*      TreeRoot     The private structure that stores information about    *)
    (*                    the directories to which the user has access.         *)

TYPE
    User = POINTER TO UserPermission;

    UserPermission = RECORD
                         category: UserCategory;
                         RealName: ARRAY [0..127] OF CHAR;
                         Password: PassString;
                         UserLimit: CARDINAL;
                         SpeedLimit: CARDINAL;
                         SuppressLog: BOOLEAN;
                         TreeRoot: DirEntryPtr;
                     END (*RECORD*);

VAR
    (* Type for field editor. *)

    ShortString: FieldType;

    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

(********************************************************************************)
(*                    CHECKING WHETHER A USER EXISTS                            *)
(********************************************************************************)

PROCEDURE UserInINIFile (VAR (*OUT*) hini: INIData.HINI;
                                username: ARRAY OF CHAR): BOOLEAN;

    (* If username is in INI file, initialises hini and returns TRUE. *)

    VAR size: CARDINAL;  result: BOOLEAN;

    BEGIN
        IF username[0] = Nul THEN
            result := FALSE;
        ELSE
            hini := OurINIHandle();
            IF NOT INIData.INIValid (hini) THEN
                result := FALSE;
            ELSE
                result := INIData.ItemSize (hini, username, '', size)
                                       AND (size <> 0);
            END (*IF*);
        END (*IF*);
        RETURN result;
    END UserInINIFile;

(********************************************************************************)

PROCEDURE FindUser (UserName: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the user data exists.        *)

    VAR hini: INIData.HINI;

    BEGIN
        RETURN UserInINIFile (hini, UserName);
    END FindUser;

(********************************************************************************)
(*                                  PARSER                                      *)
(********************************************************************************)

PROCEDURE ReadUserData (name: ARRAY OF CHAR): User;

    (* Fetches the password, etc., for the user whose username is specified     *)
    (* as the argument.  Returns a NIL result if no user data is found.         *)

    VAR hini: INIData.HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        active: BOOLEAN;
        result: User;
        key: ARRAY [0..6] OF CHAR;

    BEGIN       (* Body of ReadUserData *)

        IF UserInINIFile (hini, name) AND
                   (NOT INIGet (hini, name, "Active", active) OR active) THEN

            NEW (result);

            IF NOT INIGet (hini, name, "Category", result^.category) THEN
                result^.category := NormalUser;
            END (*IF*);
            IF NOT INIGetString (hini, name, "RealName", result^.RealName) THEN
                result^.RealName := "";
            END (*IF*);
            IF NOT INIGetString (hini, name, "Password", result^.Password) THEN
                result^.Password := "@";
            END (*IF*);
            IF NOT INIGet (hini, name, "UserLimit", result^.UserLimit) THEN
                result^.UserLimit := MAX(CARDINAL);
            END (*IF*);
            IF NOT INIGet (hini, name, "SpeedLimit", result^.SpeedLimit) THEN
                result^.SpeedLimit := MAX(CARDINAL);
            ELSIF result^.SpeedLimit = 0 THEN
                result^.SpeedLimit := 1;
            END (*IF*);
            IF NOT INIGet (hini, name, "SuppressLog", result^.SuppressLog) THEN
                result^.SuppressLog := FALSE;
            END (*IF*);
            IF INIData.ItemSize (hini, name, "Volume", BufferSize)
                                                    AND (BufferSize <> 0) THEN
                ALLOCATE (bufptr, BufferSize);
                key := "Volume";
                EVAL (INIData.INIGetTrusted (hini, name, key,
                                                  bufptr^, BufferSize));
            ELSE
                bufptr := NIL;
            END (*IF*);

            result^.TreeRoot := LoadTreeData (bufptr, BufferSize);
            IF bufptr <> NIL THEN
                DEALLOCATE (bufptr, BufferSize);
            END (*IF*);

        ELSE
            result := NIL;
        END (*IF*);

        RETURN result;

    END ReadUserData;

(********************************************************************************)
(*                      WRITING USER DATA TO THE INI FILE                       *)
(********************************************************************************)

PROCEDURE StoreUserData (U: User;  username: ARRAY OF CHAR);

    (* Writes permission data to the INI file. *)

    VAR hini: INIData.HINI;
        size: CARDINAL;
        bufptr: CharArrayPointer;

    BEGIN
        hini := OurINIHandle();
        IF NOT INIData.INIValid(hini) THEN
            RETURN;
        END (*IF*);
        INIData.INIPut (hini, username, "Category", U^.category);
        INIData.INIPut (hini, username, "Password", U^.Password);
        INIData.INIPut (hini, username, "RealName", U^.RealName);
        INIData.INIPut (hini, username, "UserLimit", U^.UserLimit);
        INIData.INIPut (hini, username, "SpeedLimit", U^.SpeedLimit);
        INIData.INIPut (hini, username, "SuppressLog", U^.SuppressLog);

        (* Store all volume information in the bufptr^ array. *)

        size := SizeOfDirectoryData (U^.TreeRoot);
        ALLOCATE (bufptr, size);
        StoreDirectoryData (U^.TreeRoot, bufptr^);

        (* Copy the result from bufptr^ to the INI file. *)

        IF size > 0 THEN
            INIData.INIPutBinary (hini, username, "Volume", bufptr^, size);
            DEALLOCATE (bufptr, size);
        END (*IF*);

    END StoreUserData;

(********************************************************************************)
(*                        DISCARDING A USER RECORD                              *)
(********************************************************************************)

PROCEDURE DestroyUserData (VAR (*INOUT*) U: User);

    (* Discards the data structure.  *)

    BEGIN
        KillList (U^.TreeRoot);
    END DestroyUserData;

(************************************************************************)

PROCEDURE RemoveUser (name: ARRAY OF CHAR);

    (* Deletes this user's INI file entry. *)

    VAR hini: INIData.HINI;

    BEGIN
        hini := OurINIHandle();
        IF INIData.INIValid (hini) THEN
           INIData.INIDeleteApp (hini, name);
        END (*IF*);
    END RemoveUser;

(************************************************************************)
(*                        ShortString EDITOR                            *)
(************************************************************************)

PROCEDURE WriteShortString (w: Window;  p: ADDRESS;  width: CARDINAL);

    VAR address: PassStringPtr;

    BEGIN
        address := p;
        WriteString (w, address^);
    END WriteShortString;

(************************************************************************)

PROCEDURE EditShortString (w: Window;  VAR (*INOUT*) p: ADDRESS;
                                         stringsize, width: CARDINAL);

    VAR address: PassStringPtr;

    BEGIN
        address := p;
        EditString (w, address^, stringsize, width);
    END EditShortString;

(********************************************************************************)
(*                          MASTER EDITING PROCEDURE                            *)
(********************************************************************************)

PROCEDURE EditUser (Screen: VirtualScreen;  VAR (*INOUT*) name: ARRAY OF CHAR;
                                                                clone: BOOLEAN);

    (* Top-level editor for the user permission data. *)

    VAR UserData, ErrorWindow: Window;
        ch: CHAR;

    (****************************************************************************)

    PROCEDURE WaitForKey;

        (* Called when we are waiting for the user to type something after     *)
        (* we've displayed an "invalid user name" error message.               *)

        VAR ch: CHAR;

        BEGIN
            SetCursor (UserData, 1, 20);
            ColourSwap (UserData, 1, 20, 32);
            ReadCharWithoutEcho (UserData, ch);  PutBack(ch);
            ColourSwap (UserData, 1, 20, 32);
        END WaitForKey;

    (****************************************************************************)

    VAR TopBar, Background, DirStatus, bottombar: Window;
        UserName: PassString;
        R: Structure;
        saverow, savecol, length: CARDINAL;
        abort: BOOLEAN;
        U: User;
        HomeLabel: FileNameString;
        M, M2: Menu;
        CatLabel: ARRAY [0..4] OF ItemText;
        LogLabel: ARRAY [0..2] OF ItemText;
        CategoryNumber, SuppressCode: CARDINAL;

    BEGIN
        EnableScreenSwitching (FALSE);
        Strings.Assign (name, UserName);

        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, Screen);
        WriteString (TopBar, "Username: ");
        WriteString (TopBar, UserName);

        OpenWindowHidden (Background, yellow, black, 1, ScreenRows-2, 0, 79, noframe, nodivider);
        MapToVirtualScreen (Background, Screen);
        SetCursor (Background, 11, 20);
        WriteString (Background, "Cursor down to set up the directories");

        OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (bottombar, Screen);
        WriteString (bottombar, " Esc exit  ");
        WriteChar (bottombar, CHR(24));  WriteChar (bottombar, CHR(25));
        WriteString (bottombar, " select");

        U := ReadUserData (name);
        IF clone THEN
            name[0] := Nul;  UserName[0] := Nul;
        END (*IF*);
        IF U = NIL THEN
            NEW (U);
            U^.RealName := "";
            U^.category := NormalUser;
            U^.UserLimit := 10;
            U^.SpeedLimit := MAX(CARDINAL);
            U^.SuppressLog := FALSE;
            Strings.Assign ("", U^.Password);
            U^.TreeRoot := NIL;
        END (*IF*);
        IF U^.TreeRoot = CAST(DirEntryPtr, NIL) THEN
            U^.TreeRoot := CreateDefaultRoot();
        END (*IF*);

        OpenWindowHidden (UserData, white, blue, 2, 10, 1, 70, noframe, nodivider);
        MapToVirtualScreen (UserData, Screen);

        (* Create the menu of user categories. *)

        CatLabel[1] := "NoPassword";
        CatLabel[2] := "Guest";
        CatLabel[3] := "User";
        CatLabel[4] := "Manager";
        CreateMenu (M, 4, CatLabel, 4);
        MenuColours (M, white, blue, blue, cyan, yellow, darkgrey);
        CategoryNumber := ORD (U^.category);

        (* Create the menu for "suppress log". *)

        LogLabel[1] := "No";
        LogLabel[2] := "Yes";
        CreateMenu (M2, 2, LogLabel, 2);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        SuppressCode := ORD (U^.SuppressLog) + 1;

        (* Create the username/category/password editing structure. *)

        SetCursor (UserData, 1, 2);  WriteString (UserData, "User name");
        SetCursor (UserData, 2, 2);  WriteString (UserData, "Real name");
        SetCursor (UserData, 3, 2);  WriteString (UserData, "Category");
        SetCursor (UserData, 4, 2);  WriteString (UserData, "Password");
        SetCursor (UserData, 5, 2);  WriteString (UserData, "User limit");
        SetCursor (UserData, 6, 2);  WriteString (UserData, "Speed limit");
        SetCursor (UserData, 6, 33);  WriteString (UserData, "bytes/second");
        SetCursor (UserData, 7, 2);  WriteString (UserData, "Suppress log");

        R := CreateField (ADR(UserName), SIZE(UserName), ShortString, 1, 20, 32);
        Combine (R, StringField (U^.RealName, 2, 20, 48));
        Combine (R, MenuField (CategoryNumber, 3, 20, 1, 48, M));
        Combine (R, CreateField (ADR(U^.Password), SIZE(PassString), ShortString, 4, 20, 32));
        Combine (R, CardinalField (U^.UserLimit, 5, 20, 12));
        Combine (R, CardinalField (U^.SpeedLimit, 6, 20, 12));
        Combine (R, MenuField (SuppressCode, 7, 20, 1, 10, M2));

        (* Create the directory overview window.  *)

        OpenWindowHidden (DirStatus, white, black, 13, ScreenRows-3,
                                            1, 78, noframe, nodivider);
        MapToVirtualScreen (DirStatus, Screen);

        (* Here's the main editing loop. *)

        SetCursor (UserData, 1, 1);
        LOOP
            LOOP
                DirectorySummary (DirStatus, U^.TreeRoot, ScreenRows-13);
                ScreenEdit (UserData, R, abort);
                ToLower (UserName);
                U^.category := VAL(UserCategory, CategoryNumber);
                U^.SuppressLog := SuppressCode > 1;
                IF abort THEN EXIT(*LOOP*) END(*IF*);

                (* Check the character that took us off the edge.  If it's *)
                (* "cursor down", proceed to editing directories.  If it's *)
                (* "cursor up", leave this procedure.                      *)

                ch := GetKey (UserData);
                IF ch = CHR(0) THEN
                    ch := GetKey (UserData);
                    IF ch = 'H' THEN
                        EXIT (*LOOP*);
                    ELSIF ch = 'P' THEN
                        EditDirectoryData (U^.TreeRoot, Screen);
                        SetCursor (UserData, 5, 1);
                    END (*IF*);
                END (*IF*);

            END (*LOOP*);

            (* Remove leading space and '$' characters from UserName. *)

            SaveCursor (UserData, saverow, savecol);
            length := LENGTH(UserName);
            WHILE (UserName[0] = ' ') OR (UserName[0] = '$') DO
                Strings.Delete (UserName, 0, 1);
                DEC (length);
                SetCursor (UserData, 1, 20+length);  WriteChar (UserData, ' ');
                SetCursor (UserData, 1, 20);  WriteString (UserData, UserName);
            END (*WHILE*);
            SetCursor (UserData, saverow, savecol);

            (* At this point, "name" holds the original username, and "UserName"    *)
            (* holds the new name.  We need to check for the following errors:      *)
            (*    1.  UserName is empty.                                            *)
            (*    2.  UserName duplicates an existing name.                         *)

            IF UserName[0] = CHR(0) THEN
                OpenWindowHidden (ErrorWindow, white, black, 13, 15, 20, 59, noframe, nodivider);
                MapToVirtualScreen (ErrorWindow, Screen);
                WriteLn (ErrorWindow);
                WriteString (ErrorWindow, "  You have not assigned a user name");
                WaitForKey;
                CloseWindow (ErrorWindow);
            ELSE
                Strings.Assign (name, HomeLabel);
                ToLower (HomeLabel);
                IF NOT Strings.Equal (HomeLabel, UserName) AND FindUser (UserName) THEN
                    OpenWindowHidden (ErrorWindow, white, black, 13, 16, 16, 63, noframe, nodivider);
                    MapToVirtualScreen (ErrorWindow, Screen);
                    WriteLn (ErrorWindow);
                    WriteString (ErrorWindow, " You've duplicated the name of an existing user");
                    WriteLn (ErrorWindow);
                    WriteString (ErrorWindow, "       Please assign a different name");
                    WaitForKey;
                    CloseWindow (ErrorWindow);
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        CloseWindow (UserData);
        CloseWindow (TopBar);
        CloseWindow (DirStatus);

        (* Destroy the old user permission file, and create a new one. *)

        IF name[0] <> Nul THEN
            RemoveUser (name);
        END (*IF*);
        Strings.Assign (UserName, name);
        StoreUserData (U, name);
        DestroyUserData (U);

        CloseWindow (bottombar);
        CloseWindow (Background);
        EnableScreenSwitching (TRUE);

    END EditUser;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    ShortString := DefineFieldType (WriteShortString, EditShortString);
FINALLY
    DiscardFieldType (ShortString);
END Users.

