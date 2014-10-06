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

MODULE VIOSetup;

        (********************************************************)
        (*                                                      *)
        (*             FtpServer set-up utility                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            22 January 1998                 *)
        (*  Last edited:        22 January 2014                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT EditUsers, Security, Setup2, OS2, TextIO, INIData;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow, WriteChar, WriteString, WriteLn,
                SetCursor, GetKey, GetScreenSize, SaveCursor, Blink;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIPut;

FROM Menus IMPORT
    (* type *)  Menu, ItemText,
    (* proc *)  CreateMenu, MenuColours;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  CardinalField, MenuField, Combine, ScreenEdit, DeleteStructure;

FROM MultiScreen IMPORT
    (* proc *)  EnableHotKeys;

FROM SetupINI IMPORT
    (* proc *)  OpenOurINIFile, OurINIHandle;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR, EVAL;

(************************************************************************)

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Prompt line at bottom of screen.  *)

    bottombar: Window;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE EditDefaultParameters;

    VAR w: Window;
        R: Structure;
        port, MaxUsers, GuestLimit, FreeSpaceThreshold,
              UserLogging, TimeoutLimit,
              TransferLogLevel, TransactionLogLevel,
              PipeAndSyslog, temp: CARDINAL;
        abort: BOOLEAN;
        hini: HINI;
        M1, M2: Menu;
        TrlOption: ARRAY [0..4] OF ItemText;
        Log2Option: ARRAY [0..4] OF ItemText;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE LoadINIData;

        (* Loads setup parameters from "ftpd.ini". *)

        PROCEDURE GetItem (name: ARRAY OF CHAR;
                             VAR (*OUT*) variable: CARDINAL): BOOLEAN;

            BEGIN
                RETURN INIGet (hini, SYSapp, name, variable);
            END GetItem;

        (****************************************************************)

        BEGIN
            hini := OurINIHandle();
            IF NOT INIValid (hini) THEN
                WriteString (w, "Could not open FTPD.INI");
                WriteLn (w);
            ELSE
                TransactionLogLevel := 1;
                IF NOT GetItem ("ServerPort", port) THEN
                    port := 21;
                END (*IF*);
                IF NOT GetItem ("MaxUsers", MaxUsers) THEN
                    MaxUsers := 10;
                END (*IF*);
                IF NOT GetItem ("GuestLimit", GuestLimit) THEN
                    GuestLimit := MaxUsers - 1;
                END (*IF*);
                IF GetItem ("DebugLevel", TransactionLogLevel) THEN
                    TransactionLogLevel := 2*TransactionLogLevel + 1;
                END (*IF*);
                IF GetItem ("TransLevel", TransactionLogLevel) THEN
                    PipeAndSyslog := IAND (TransactionLogLevel, 0C0H);
                    temp := TransactionLogLevel DIV 16;
                    TransactionLogLevel := TransactionLogLevel MOD 16;
                    TransferLogLevel := TransactionLogLevel DIV 4;
                    INC (TransferLogLevel);
                    TransactionLogLevel := IOR (TransactionLogLevel, temp);
                    TransactionLogLevel := TransactionLogLevel MOD 4;
                    INC (TransactionLogLevel);
                END (*IF*);
                IF NOT GetItem ("SpaceThreshold", FreeSpaceThreshold) THEN
                    FreeSpaceThreshold := 10;
                END (*IF*);
                IF NOT GetItem ("LogLevel", UserLogging) THEN
                    UserLogging := 1;
                END (*IF*);
                IF NOT GetItem ("TimeOut", TimeoutLimit) THEN
                    TimeoutLimit := 900;
                END (*IF*);
            END (*IF*);
        END LoadINIData;

    (********************************************************************)

    PROCEDURE StoreINIData;

        (* Writes data back to the INI file. *)

        PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: CARDINAL);

            BEGIN
                INIPut (hini, SYSapp, name, variable);
            END PutItem;

        (****************************************************************)

        BEGIN
            IF INIValid (hini) THEN
                PutItem ("ServerPort", port);
                PutItem ("MaxUsers", MaxUsers);
                PutItem ("GuestLimit", GuestLimit);
                IF TransactionLogLevel > 0 THEN
                    DEC (TransactionLogLevel);
                END (*IF*);
                IF TransferLogLevel > 0 THEN
                    DEC (TransferLogLevel);
                END (*IF*);
                TransactionLogLevel := 16*TransactionLogLevel
                                           + 4*TransferLogLevel
                                           + PipeAndSyslog;
                PutItem ("TransLevel", TransactionLogLevel);
                PutItem ("SpaceThreshold", FreeSpaceThreshold);
                PutItem ("LogLevel", UserLogging);
                PutItem ("TimeOut", TimeoutLimit);

                (* Get rid of the obsolete "DebugLevel" entry. *)

                INIData.INIDeleteKey (hini, SYSapp, "DebugLevel");

            END (*IF*);
        END StoreINIData;

    (********************************************************************)

    BEGIN
        SYSapp := "$SYS";
        OpenWindow (w, white, blue, ScreenRows DIV 2 - 4, ScreenRows DIV 2 + 7,
                                                  14, 65, noframe, nodivider);
        LoadINIData;

        SetCursor (w, 1, 7);  WriteString (w, "Server port");
        SetCursor (w, 2, 7);  WriteString (w, "Maximum number of users");
        SetCursor (w, 3, 7);  WriteString (w, "Maximum number of guest users");
        SetCursor (w, 4, 7);  WriteString (w, "Free space threshold (MB)");
        SetCursor (w, 5, 7);  WriteString (w, "Timeout (seconds)");
        SetCursor (w, 10, 2);  WriteString (w, "Transaction logging");
        SetCursor (w, 8, 7);  WriteString (w, "Transfer logging level");
        SetCursor (w, 7, 2);  WriteString (w, "Transfer log format");

        R := CardinalField (port, 1, 37, 8);
        Combine (R, CardinalField (MaxUsers, 2, 37, 8));
        Combine (R, CardinalField (GuestLimit, 3, 37, 8));
        Combine (R, CardinalField (FreeSpaceThreshold, 4, 37, 8));
        Combine (R, CardinalField (TimeoutLimit, 5, 37, 8));

        (* Create the menu of transfer log options. *)

        Log2Option[1] := "None";
        Log2Option[2] := "User";
        Log2Option[3] := "Common";
        Log2Option[4] := "Both";
        CreateMenu (M2, 4, Log2Option, 4);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (TransferLogLevel, 7, 23, 1, 28, M2));

        Combine (R, CardinalField (UserLogging, 8, 32, 8));

        (* Create the menu of transaction log options. *)

        TrlOption[1] := "None";
        TrlOption[2] := "Disk";
        TrlOption[3] := "Screen";
        TrlOption[4] := "Both";
        CreateMenu (M1, 4, TrlOption, 4);
        MenuColours (M1, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (TransactionLogLevel, 10, 23, 1, 28, M1));

        SetCursor (w, 1, 32);
        LOOP
            ScreenEdit (w, R, abort);
            IF abort THEN EXIT(*LOOP*) END(*IF*);

            (* Consume the character that took us off the edge, and     *)
            (* wrap to the other edge.                                  *)

            IF (GetKey(w) = CHR(0)) AND (GetKey(w) = 'H') THEN
                SetCursor (w, 11, 8);
            ELSE
                SetCursor (w, 1, 32);
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        StoreINIData;
        CloseWindow (w);

    END EditDefaultParameters;

(************************************************************************)

PROCEDURE PostUpdated (semName: ARRAY OF CHAR);

    (* Posts on a public event semaphore. *)

    VAR changehev: OS2.HEV;  count: CARDINAL;

    BEGIN
        changehev := 0;
        IF OS2.DosOpenEventSem (semName, changehev) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, changehev, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (changehev);
        OS2.DosResetEventSem (changehev, count);
        OS2.DosCloseEventSem(changehev);
    END PostUpdated;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

PROCEDURE GetCommandLine (w: Window): BOOLEAN;

    (* Picks up program arguments (there should be only one) from the   *)
    (* command line, returns TRUE iff the T option is specified.        *)

    TYPE CommandStringIndex = [0..127];

    VAR j: CARDINAL;
        UseTNI: BOOLEAN;
        Options: ARRAY CommandStringIndex OF CHAR;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            LOOP
                IF Options[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = MAX(CommandStringIndex) THEN
                    Options[j] := CHR(0);  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END SkipBlanks;

    (****************************************************************************)

    VAR args: ChanId;
        row, col: CARDINAL;
        FirstError: BOOLEAN;

    BEGIN
        UseTNI := FALSE;
        FirstError := TRUE;
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, Options);
            j := 0;  SkipBlanks;
            LOOP
                CASE CAP(Options[j]) OF
                    CHR(0):   EXIT (*LOOP*);
                  | 'T':      UseTNI := TRUE;
                ELSE
                    IF FirstError THEN
                        WriteLn (w);
                        FirstError := FALSE;
                    END (*IF*);
                    WriteLn (w);
                    WriteString (w, "           Unknown option ");
                    WriteChar (w, Options[j]);
                    INC(j);
                    SaveCursor (w, row, col);
                    Blink (w, row, 10, 18);
                END (*CASE*);
                SkipBlanks;
            END (*LOOP*);
        END (*IF*);

        RETURN UseTNI;

    END GetCommandLine;

(************************************************************************)

PROCEDURE RunTheProgram;

    CONST Esc = CHR(1BH);
          UpdateSemName = "\SEM32\FTPSERVER\UPDATED";

    VAR w0, w1: Window;

    BEGIN
        OpenWindow (w0, yellow, red, 0, 8, 0, 39, noframe, nodivider);
        WriteLn (w0);
        WriteLn (w0);
        WriteString (w0, "          FTPSERVER SETUP");
        WriteLn (w0);  WriteLn (w0);
        WriteString (w0, "       Type F4 to edit users");  WriteLn (w0);
        WriteString (w0, "        F5 for more options");  WriteLn (w0);
        WriteString (w0, "        Esc to exit program");

        OpenWindow (w1, black, green, 0, 8, 40, 79, noframe, nodivider);
        WriteLn (w1);
        WriteLn (w1);
        WriteString (w1, "    NOTE: Parameters set on this");  WriteLn (w1);
        WriteString (w1, "     page will take effect the");  WriteLn (w1);
        WriteString (w1, "    next time the server is run");  WriteLn (w1);
        WriteString (w1, "          on this machine");

        OpenOurINIFile(GetCommandLine(w1));
        EVAL(Setup2.StartPage2Editor);
        EVAL(Security.StartSecurityEditor);
        EVAL(EditUsers.StartUserEditor);
        EditDefaultParameters;
        CloseWindow (w1);
        CloseWindow (w0);
        PostUpdated (UpdateSemName);

    END RunTheProgram;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    EnableHotKeys (TRUE, CHR(62), TRUE, CHR(63), TRUE, CHR(64));
    OpenWindow (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
    WriteString (bottombar, " Esc exit");
    SetCursor (bottombar, 0, 55);
    WriteString (bottombar, "F4,F5 previous/next page");
    RunTheProgram;
FINALLY
    CloseWindow (bottombar);
END VIOSetup.

