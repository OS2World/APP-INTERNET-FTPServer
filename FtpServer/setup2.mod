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

IMPLEMENTATION MODULE Setup2;

        (********************************************************)
        (*                                                      *)
        (*       A second page of FtpServer setup options       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            18 January 1999                 *)
        (*  Last edited:        30 May 2012                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM Security IMPORT
    (* proc *)  CardinalToDotted, StringToIPAddress;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIPut;

FROM InetUtilities IMPORT
    (* proc *)  Swap4;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen,
                RemoveVirtualScreen, RemoveScreenGroup;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor, SetActivePage,
                WriteString, WriteLn, GetKey, GetScreenSize, EditString,
                ColourSwap, EraseLine;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Sockets IMPORT
    (* proc *)  sock_init, gethostid;

(************************************************************************)

CONST Nul = CHR(0);  Esc = CHR(27);

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* The screen group for our screen page.  *)

    OurGroup: ScreenGroup;

    (* The screen page used by this module. *)

    OurPage: VirtualScreen;

    (* Header and footer windows. *)

    TopBar, BottomBar: Window;

    (* Handle for our INI file. *)

    hini: HINI;

    (* Address to which the server should bind. *)

    BindAddr: CARDINAL;

(************************************************************************)
(*                   LOADING AND STORING INI FILE DATA                  *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "ftpd.ini". *)

    VAR SYSapp: ARRAY [0..4] OF CHAR;

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                         VAR (*OUT*) variable: CARDINAL): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (****************************************************************)

    BEGIN
        hini := OurINIHandle();
        SYSapp := "$SYS";
        IF NOT (INIValid(hini) AND GetItem("BindAddr", BindAddr)) THEN
            BindAddr := 0;
        END (*IF*);
    END LoadINIData;

(********************************************************************)

PROCEDURE StoreINIData;

    (* Writes data back to the INI file. *)

    VAR SYSapp: ARRAY [0..4] OF CHAR;

    PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: CARDINAL);

        BEGIN
            INIPut (hini, SYSapp, name, variable);
        END PutItem;

    (****************************************************************)

    BEGIN
        SYSapp := "$SYS";
        IF INIValid (hini) THEN
            PutItem ("BindAddr", BindAddr);
        END (*IF*);
    END StoreINIData;

(************************************************************************)
(*                          MAIN EDITING TASK                           *)
(************************************************************************)

PROCEDURE EditorTask;

    (* Runs as a separate task.  This is for allowing the user to       *)
    (* specify what address the server will bind to.                    *)

    VAR TextBuffer: ARRAY [0..15] OF CHAR;
        SpecificAddress: BOOLEAN;
        w: Window;  ch: CHAR;  DefaultBindAddr: CARDINAL;

    BEGIN
        OpenWindowHidden (w, white, blue, ScreenRows DIV 2 - 5, ScreenRows DIV 2 + 5,
                                                  11, 68, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);

        SpecificAddress := BindAddr <> 0;
        IF SpecificAddress THEN
            DefaultBindAddr := BindAddr;
        ELSE
            DefaultBindAddr := Swap4(gethostid());
        END (*IF*);

        SetCursor (w, 3, 2);  WriteString (w, "Bind server to:  ");
        WriteString (w, "all interfaces   specific address");

        REPEAT
            IF SpecificAddress THEN

                (* Highlight the words "specific address". *)

                ColourSwap (w, 3, 36, 16);

                (* Edit the IP address. *)

                SetCursor (w, 4, 36);
                CardinalToDotted (BindAddr, TextBuffer);
                EditString (w, TextBuffer, 16, 16);
                BindAddr := StringToIPAddress (TextBuffer);
                DefaultBindAddr := BindAddr;

            ELSE

                (* Remove the address from the screen, and highlight    *)
                (* the words "all interfaces".                          *)

                SetCursor (w, 4, 0);  EraseLine (w, 1);
                ColourSwap (w, 3, 19, 14);

            END (*IF*);

            ch := GetKey(w);

            (* Cancel the screen highlighting. *)

            IF SpecificAddress THEN
                ColourSwap (w, 3, 36, 16);
            ELSE
                ColourSwap (w, 3, 19, 14);
            END (*IF*);

            IF ch = Nul THEN
                ch := GetKey(w);
                IF ch = "K" THEN
                    SpecificAddress := FALSE;  BindAddr := 0;
                ELSIF ch = "M" THEN
                    SpecificAddress := TRUE;  BindAddr := DefaultBindAddr;
                END (*IF*);
            END (*IF*);

        UNTIL ch = Esc;

        CloseWindow (w);
        SetActivePage (0);
        StuffKeyboardBuffer (CHR(27));

    END EditorTask;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

PROCEDURE StartPage2Editor(): BOOLEAN;

    (* Initialisation for page 2 of VIOSetup. *)

    BEGIN
        RETURN CreateTask (EditorTask, 2, "Setup2 editor");
    END StartPage2Editor;

(************************************************************************)

PROCEDURE CreateHeadings;

    BEGIN
        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);
        WriteString (TopBar, "  MORE FTPSERVER SETUP OPTIONS");
        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, OurPage);
        WriteString (BottomBar, " Esc exit");
        SetCursor (BottomBar, 0, 55);
        WriteString (BottomBar, "F4,F5 previous/next page");
    END CreateHeadings;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    sock_init;
    GetScreenSize (ScreenRows, dummy);
    hini := OurINIHandle();
    LoadINIData;
    OurGroup := CreateScreenGroup (1);
    OurPage := CreateVirtualScreen (OurGroup);
    CreateHeadings;
FINALLY
    StoreINIData;
    CloseWindow (BottomBar);
    CloseWindow (TopBar);
    RemoveVirtualScreen (OurPage);
    RemoveScreenGroup (OurGroup);
END Setup2.

