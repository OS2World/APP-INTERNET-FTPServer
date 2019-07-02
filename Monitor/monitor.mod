(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
(*  Copyright (C) 2018   Peter Moylan                                     *)
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

MODULE Monitor;

        (************************************************************)
        (*                                                          *)
        (*             FtpServer Monitor - PM version               *)
        (*                                                          *)
        (*    Started:        26 March 2000                         *)
        (*    Last edited:    10 December 2017                      *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, OS2RTL, MainDialogue, FtpCl2, INIData, IOChan, TextIO, Strings;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM Init IMPORT
    (* proc *)  MainHab;

FROM MonINI IMPORT
    (* proc *)  SetINIFileName;

(********************************************************************************)

VAR hab: OS2.HAB;            (* anchor block handle *)
    qmsg: OS2.QMSG;          (* message structure *)

(********************************************************************************)
(*                  PICK UP COMMAND-LINE PARAMETER IF ANY                       *)
(********************************************************************************)

PROCEDURE GetParameters;

    (* Picks up program arguments from the command line.                *)
    (* In this version, the only options are to specify the TNI mode    *)
    (* and the name of the INI or TNI file to use.                      *)

    TYPE CharNumber = [0..255];

    VAR args: IOChan.ChanId;  pos: CARDINAL;  TNImode, explicit: BOOLEAN;
        INIFileName: ARRAY CharNumber OF CHAR;
        tail: ARRAY [0..4] OF CHAR;

    BEGIN
        TNImode := FALSE;
        explicit := FALSE;
        INIFileName := "";
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, INIFileName);
            WHILE INIFileName[0] = ' ' DO
                Strings.Delete (INIFileName, 0, 1);
            END (*WHILE*);
            IF INIFileName[0] = '-' THEN
                Strings.Delete (INIFileName, 0, 1);
                IF CAP(INIFileName[0]) = 'T' THEN
                    TNImode := TRUE;
                    explicit := TRUE;
                ELSIF CAP(INIFileName[0]) = 'I' THEN
                    TNImode := TRUE;
                    explicit := TRUE;
                END (*IF*);
            END (*IF*);
            WHILE INIFileName[0] = ' ' DO
                Strings.Delete (INIFileName, 0, 1);
            END (*WHILE*);
            IF INIFileName[0] <> CHR(0) THEN
                pos := LENGTH(INIFileName);
                IF pos >= 4 THEN
                    DEC (pos, 4);
                    Strings.Extract (INIFileName,
                                     pos, 4, tail);
                    Strings.Capitalize (tail);
                    IF Strings.Equal (tail, ".TNI") THEN
                        TNImode := TRUE;
                    ELSIF Strings.Equal (tail, ".INI") THEN
                        TNImode := FALSE;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        IF INIFileName[0] = CHR(0) THEN
            IF explicit THEN
                INIData.CommitTNIDecision ("Monitor", TNImode);
            ELSIF NOT INIData.ChooseDefaultINI ("Monitor", TNImode) THEN
                TNImode := FALSE;
            END (*IF*);
            IF TNImode THEN
                INIFileName := "MONITOR.TNI";
            ELSE
                INIFileName := "MONITOR.INI";
            END (*IF*);
        END (*IF*);
        SetINIFileName (INIFileName, TNImode);
    END GetParameters;

(********************************************************************************)
(*              MAIN PROGRAM: INITIALISATION AND MESSAGE DISPATCHING            *)
(********************************************************************************)

BEGIN
    hab := MainHab();

    (* NOTE:  clean up from here is handled by the DosExitList processing *)
    (* Since signal exceptions are not handled by RTS yet, using module   *)
    (* finalization for clean up is incorrect. This will be changed in the*)
    (* next release.                                                      *)

    GetParameters;
    MainDialogue.Create;

    (* Get/Dispatch Message loop *)

    WHILE OS2.WinGetMsg (hab, qmsg, 0, 0, 0) DO
        OS2.WinDispatchMsg (hab, qmsg);
    END;

END Monitor.
