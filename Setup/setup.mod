(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
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

MODULE Setup;

        (************************************************************)
        (*                                                          *)
        (*                   PM Setup for FtpServer                 *)
        (*                                                          *)
        (*    Started:        7 October 1999                        *)
        (*    Last edited:    14 December 2020                      *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, OS2RTL, OpeningDialogue, CommonSettings, IOChan, TextIO;

FROM PMInit IMPORT
    (* proc *)  OurHab;

FROM RINIData IMPORT
    (* proc *)  ChooseDefaultINI, CommitTNIDecision;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

(********************************************************************************)

VAR hab: OS2.HAB;            (* anchor block handle *)
    qmsg: OS2.QMSG;          (* message structure *)

(********************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) LocalRemote: CARDINAL;
                            VAR (*INOUT*) UseTNI: BOOLEAN;
                                    VAR (*OUT*) explicit: BOOLEAN);

    (* Picks up program arguments from the command line.                *)
    (* The meaning of LocalRemote is:                                   *)
    (*         0   let user specify local or remote                     *)
    (*         1   force local setup                   (option -L)      *)
    (*         2   force remote setup                  (option -R)      *)
    (*         3   force whichever was used last time  (option -G)      *)
    (* explicit is true iff the UseTNI value has been set by a -T or    *)
    (* -I specifier.                                                    *)

    TYPE CharNumber = [0..79];

    VAR j: CARDINAL;
        args: IOChan.ChanId;
        Options: ARRAY CharNumber OF CHAR;
        ch: CHAR;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            LOOP
                IF Options[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = MAX(CharNumber) THEN
                    Options[j] := CHR(0);  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END SkipBlanks;

    (****************************************************************************)

    BEGIN
        explicit := FALSE;
        LocalRemote := 0;
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, Options);
            j := 0;  SkipBlanks;
            LOOP
                ch := CAP(Options[j]);  INC(j);
                CASE ch OF
                    CHR(0):   EXIT (*LOOP*);
                  | 'L':      LocalRemote := 1;
                  | 'G':      IF LocalRemote = 0 THEN
                                  LocalRemote := 3;
                              END (*IF*);
                  | 'R':      LocalRemote := 2;
                  | 'I':      UseTNI := FALSE;
                              explicit := TRUE;
                  | 'T':      UseTNI := TRUE;
                              explicit := TRUE;
                ELSE
                    (* unknown option, ignore it *)
                END (*CASE*);
                SkipBlanks;
            END (*LOOP*);
        END (*IF*);
    END GetParameters;

(********************************************************************************)
(*              MAIN PROGRAM: INITIALISATION AND MESSAGE DISPATCHING            *)
(********************************************************************************)

VAR LocalRemote: CARDINAL;
    UseTNI, explicit: BOOLEAN;

BEGIN
    hab := OurHab();

    (* NOTE:  clean up from here is handled by the DosExitList processing *)
    (* Since signal exceptions are not handled by RTS yet, using module   *)
    (* finalization for clean up is incorrect. This will be changed in the*)
    (* next release.                                                      *)

    UseTNI := FALSE;
    GetParameters (LocalRemote, UseTNI, explicit);

    (* We can now make the INI/TNI decision for the Setup INI file.  For    *)
    (* the main ftpd INI file the INI/TNI decision has to wait until we are *)
    (* certain about remote operation, and that will not be finalised until *)
    (* we are in the OpeningDialogue module.                                *)

    IF explicit THEN
        CommitTNIDecision ("SETUP", UseTNI);
    ELSIF NOT ChooseDefaultINI ("SETUP", UseTNI) THEN
        UseTNI := FALSE;
    END (*IF*);
    CommonSettings.SetDefaultFont (UseTNI);

    OpeningDialogue.CreateOpeningDialogue (LocalRemote, UseTNI, explicit);

    (* Get/Dispatch Message loop *)

    WHILE OS2.WinGetMsg (hab, qmsg, 0, 0, 0) DO
        OS2.WinDispatchMsg (hab, qmsg);
    END;

END Setup.
