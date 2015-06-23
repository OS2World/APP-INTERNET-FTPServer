(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE MaintenancePages;

        (********************************************************)
        (*                                                      *)
        (*      Support for "maintenance page" screen output    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        21 September 1998               *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  EnableHotKeys, CreateScreenGroup, CreateVirtualScreen,
                RemoveVirtualScreen, MapToVirtualScreen;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, WriteString, PutOnPage, PutOnTop, SetCursor;

(************************************************************************)

TYPE MaintenancePage = VirtualScreen;

VAR
    (* All maintenance pages are collected together as a single group.  *)

    MPGroup: ScreenGroup;

    (* The prompt window appears on every maintenance page.     *)

    prompt: Window;

(************************************************************************)
(*                      EXTERNALLY CALLABLE PROCEDURES                  *)
(************************************************************************)

PROCEDURE CreateMaintenancePage (VAR (*OUT*) page: MaintenancePage);

    (* Creates a new maintenance page.  *)

    BEGIN
        page := CreateVirtualScreen (MPGroup);
    END CreateMaintenancePage;

(************************************************************************)

PROCEDURE RemoveMaintenancePage (VAR (*INOUT*) page: MaintenancePage);

    (* Destroys all associations between the given page and its screen  *)
    (* windows (but does not close the windows), and permanently        *)
    (* removes this page from the collection of maintenance pages.      *)

    BEGIN
        RemoveVirtualScreen (page);
    END RemoveMaintenancePage;

(************************************************************************)

PROCEDURE Associate (w: Window;  page: MaintenancePage);

    (* Before calling this procedure, both w and page must have been    *)
    (* created.  This procedure ensures that window w is visible on     *)
    (* the screen only when the given maintenance page is active.       *)

    BEGIN
        MapToVirtualScreen (w, page);
    END Associate;

(************************************************************************)
(*                         MODULE INITIALISATION                        *)
(************************************************************************)

CONST
    AltO = CHR(24);   (* second code returned by Alt/O key         *)
    AltP = CHR(25);   (* second code returned by Alt/P key         *)
    F6 = '@';         (* second code returned by F6 function key   *)

BEGIN
    MPGroup := CreateScreenGroup (1);
    EnableHotKeys (TRUE, AltO, TRUE, AltP, TRUE, F6);
    OpenWindowHidden (prompt, blue, cyan, 24, 24, 0, 79, noframe, nodivider);
    PutOnPage (prompt, 1);
    WriteString (prompt, "  F6 cycle");
    SetCursor (prompt, 0, 48);
    WriteString (prompt, "Alt/P maintenance pages on/off");
    PutOnTop (prompt);
END MaintenancePages.

