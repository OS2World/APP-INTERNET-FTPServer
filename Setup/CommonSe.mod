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

IMPLEMENTATION MODULE CommonSettings;

        (****************************************************************)
        (*                                                              *)
        (*                  Setup program for FtpServer                 *)
        (*             Data common to the main Setup notebook           *)
        (*                                                              *)
        (*    Started:        20 October 2003                           *)
        (*    Last edited:    30 May 2012                               *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, Strings, INIData;

(************************************************************************)

CONST
    Nul = CHR(0);
    INIFileName0 = "Setup";

VAR
    OurFontName: FontName;
    INIFileName: ARRAY [0..127] OF CHAR;
    UseTNI: BOOLEAN;

(************************************************************************)

PROCEDURE CurrentFont (VAR (*OUT*) fontname: FontName);

    (* Returns the currently set font for this notebook. *)

    BEGIN
        fontname:= OurFontName;
    END CurrentFont;

(************************************************************************)

PROCEDURE UpdateFontFrom (hwnd: OS2.HWND);

    (* Takes the font setting from window hwnd and propagates it to the *)
    (* entire notebook.  Note that this will often be a "no operation"  *)
    (* because the font is no different from the presently active one.  *)

    VAR NewFontName: FontName;
        AttrFound, length: CARDINAL;
        hini: INIData.HINI;  target: OS2.HWND;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                     AttrFound, FontNameSize, NewFontName,
                                      0(*OS2.QPF_NOINHERIT*));
        IF length < FontNameSize THEN
            NewFontName[length] := Nul;
        END (*IF*);

        IF NOT Strings.Equal (NewFontName, OurFontName) THEN

            OurFontName := NewFontName;
            hini := INIData.OpenINIFile (INIFileName, UseTNI);
            app := "Font";
            INIData.INIPutString (hini, app, "MainNotebook", OurFontName);
            INIData.CloseINIFile (hini);

            (* For reasons that are still a mystery to me, we have to go    *)
            (* up three levels in the hierarchy to get from here to the     *)
            (* frame.  (I calculated two, but there's apparently an extra   *)
            (* container window that we're not told about.)                 *)

            target := OS2.WinQueryWindow(hwnd, OS2.QW_PARENT);
            target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
            target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
            OS2.WinSendMsg (target, FONTCHANGED, NIL, NIL);

        END (*IF*);

    END UpdateFontFrom;

(************************************************************************)

PROCEDURE SetDefaultFont (TNImode: BOOLEAN);

    (* Sets initial font if one isn't stored; also records whether to   *)
    (* use a TNI file for the Setup data.                               *)

    VAR hini: INIData.HINI;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        OurFontName := "";
        INIFileName := INIFileName0;
        IF UseTNI THEN
            Strings.Append (".TNI", INIFileName);
        ELSE
            Strings.Append (".INI", INIFileName);
        END (*IF*);
        hini := INIData.OpenINIFile(INIFileName, UseTNI);
        IF INIData.INIValid(hini) THEN
            app := "Font";
            IF NOT INIData.INIGetString (hini, app, "MainNotebook", OurFontName)
                          OR (OurFontName[0] = Nul) THEN
                OurFontName := "10.System Proportional";
                INIData.INIPutString (hini, app, "MainNotebook", OurFontName);
            END (*IF*);
            INIData.CloseINIFile (hini);
        ELSE
            OurFontName := "10.System Proportional";
        END (*IF*);
    END SetDefaultFont;

(************************************************************************)

END CommonSettings.

