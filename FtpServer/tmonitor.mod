(**************************************************************************)
(*                                                                        *)
(*  Text-mode monitor for FtpServer                                       *)
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

MODULE TMonitor;

        (********************************************************)
        (*                                                      *)
        (*     FtpServer text-mode utility to display users     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 December 1997                *)
        (*  Last edited:        22 January 2014                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

IMPORT Strings;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, DestroyListBox, ClearListBox,
                HighlightOn, HighlightOff, LBAppend, CursorBackward, CursorForward,
                LBCurrent;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow, WriteChar, WriteString, WriteLn,
                GetKey, GetScreenSize;

FROM FtpCl2 IMPORT
    (* proc *)  EditAccessParameters, GetHostAddresses, HostAddress,
                ConnectToServer, Login, SendCommand, GetResponse;

FROM Queues IMPORT
    (* type *)  Queue,
    (* proc *)  CreateQueue, DestroyQueue, AddToQueue, TakeFromQueue;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* proc *)  CreateTask, CreateTask1;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM InetUtilities IMPORT
    (* proc *)  IPToString;

(********************************************************************************)

CONST
    Nul = CHR(0);
    Esc = CHR(27);

TYPE
    MessagePointer = POINTER TO Message;

    Message = RECORD
                  category: (normal, lastline, upkey, downkey, reset, kill,
                             killserver, exit);
                  text: ARRAY [0..79] OF CHAR;
              END (*RECORD*);

(********************************************************************************)

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Prompt windows at top and bottom of screen. *)

    TopTitle, bottombar: Window;

    (* Queue for messages to the display task. *)

    MessageQueue: Queue;

    (* Semaphore to tell the TalkToServer task that it's OK to proceed. *)

    OKToConnect: Semaphore;

    (* Flag to tell the TalkToServer task to log out. *)

    DropConnection: BOOLEAN;

    (* Data used when killing a user.  To kill, put the ID into userID, and     *)
    (* set KillFlag to TRUE.  The command has been accepted when KillFlag       *)
    (* goes FALSE again.                                                        *)

    KillFlag: BOOLEAN;
    userID: ARRAY [0..5] OF CHAR;

    (* Flag to say that the server should be shut down. *)

    ShutdownOption: CHAR;

    (* Flag to tell all tasks to terminate. *)

    ShutdownRequest: BOOLEAN;

    (* Semaphore by which the subsidiary tasks say that they have finished. *)

    AllDone: Semaphore;

(********************************************************************************)
(*                        COMMUNICATION WITH FTP SERVER                         *)
(********************************************************************************)

PROCEDURE TalkToServer;

    (* Task that deals with all the ftp traffic. *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR w1: Window;  text: ARRAY [0..79] OF CHAR;
        LoggedIn, MoreToCome, TimedOut: BOOLEAN;
        p: MessagePointer;
        j, NumberOfAddresses, address: CARDINAL;

    BEGIN
        OpenWindow (w1, white, black, ScreenRows DIV 2, ScreenRows DIV 2, 22, 57, noframe, nodivider);
        WriteString (w1, " Not yet connected to server");
        LoggedIn := FALSE;
        LOOP
            Wait (OKToConnect);

            (* Not yet connected to server. *)

            MoreToCome := FALSE;
            WHILE NOT (DropConnection OR MoreToCome) DO
                NumberOfAddresses := GetHostAddresses();
                j := 0;  MoreToCome := FALSE;
                REPEAT
                    address := HostAddress(j);
                    WriteLn (w1);
                    WriteString (w1, " Trying ");
                    IPToString (address, TRUE, text);
                    WriteString (w1, text);
                    MoreToCome := ConnectToServer (address);
                    IF NOT MoreToCome THEN
                        INC (j);
                        IF j < NumberOfAddresses THEN
                            TimedWait (OKToConnect, 1000, TimedOut);
                        ELSE
                            TimedWait (OKToConnect, 10000, TimedOut);
                        END (*IF*);
                    END (*IF*);
                UNTIL MoreToCome OR DropConnection OR (j >= NumberOfAddresses);
            END (*WHILE*);

            (* Now connected, log in. *)

            LOOP
                IF DropConnection THEN EXIT(*LOOP*) END(*IF*);
                WriteLn (w1);
                WriteString (w1, " Attempting to log in to server");
                IF Login() THEN
                    LoggedIn := TRUE;  EXIT(*LOOP*);
                END(*IF*);
                TimedWait (OKToConnect, 10000, TimedOut);
            END (*LOOP*);

            IF LoggedIn AND NOT DropConnection THEN
                WriteLn (w1);
                WriteString (w1, " Logged in to server");
            END (*IF*);

            (* This is the main loop where we keep polling the server. *)

            LOOP
                IF DropConnection THEN EXIT(*LOOP*) END(*IF*);
                IF ShutdownOption = 'G' THEN
                    LoggedIn := SendCommand ("SITE MNGR GXIT");
                    REPEAT
                    UNTIL NOT GetResponse(text, MoreToCome) OR NOT MoreToCome;
                    ShutdownOption := Nul;
                ELSIF ShutdownOption = 'Q' THEN
                    LoggedIn := SendCommand ("SITE MNGR EXIT");
                    REPEAT
                    UNTIL NOT GetResponse(text, MoreToCome) OR NOT MoreToCome;
                    ShutdownOption := Nul;
                ELSIF KillFlag THEN
                    Strings.Assign ("SITE MNGR KILL ", text);
                    Strings.Append (userID, text);
                    IF SendCommand (text) THEN
                        REPEAT
                            LoggedIn := GetResponse (text, MoreToCome);
                        UNTIL NOT (LoggedIn AND MoreToCome);
                    ELSE
                        LoggedIn := FALSE;
                    END (*IF*);
                    KillFlag := FALSE;
                ELSIF SendCommand ("SITE MNGR LIST") THEN
                    REPEAT
                        NEW (p);
                        IF GetResponse (p^.text, MoreToCome) THEN
                            IF MoreToCome AND (p^.text[0] IN Digits) THEN

                                (* Discard the initial comment line. *)

                                DISPOSE (p);
                            ELSE
                                IF MoreToCome THEN p^.category := normal
                                ELSE p^.category := lastline;
                                END (*IF*);
                                AddToQueue (MessageQueue, p);
                            END (*IF*);
                        ELSE
                            MoreToCome := FALSE;
                            LoggedIn := FALSE;
                        END (*IF*);
                    UNTIL NOT MoreToCome;
                ELSE
                    LoggedIn := FALSE;
                END (*IF*);
                IF NOT LoggedIn THEN
                    EXIT (*LOOP*);
                END (*IF*);
                TimedWait (OKToConnect, 10000, TimedOut);
            END (*LOOP*);

            (* We reach this point either if the connection is lost or if the   *)
            (* user wants to log out.                                           *)

            IF DropConnection THEN
                IF LoggedIn THEN
                    WriteLn (w1);
                    WriteString (w1, " Logging out");
                    IF SendCommand ("QUIT") THEN
                        REPEAT
                        UNTIL NOT GetResponse(text, MoreToCome) OR NOT MoreToCome;
                    END (*IF*);
                END (*IF*);
                WriteLn (w1);
                WriteString (w1, " Logged out");
                LoggedIn := FALSE;  DropConnection := FALSE;
                IF ShutdownRequest THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSE
                WriteLn (w1);
                WriteString (w1, " Connection lost");
                Sleep (5000);
                Signal (OKToConnect);
            END (*IF*);

        END (*LOOP*);

        CloseWindow (w1);
        Signal (AllDone);

    END TalkToServer;

(********************************************************************************)
(*                            KEYBOARD COMMANDS                                 *)
(********************************************************************************)

PROCEDURE KeyboardCommands (param: ADDRESS);

    (* Task to interpret keyboard input. *)

    VAR ch: CHAR;  p: MessagePointer;  w: Window;

    BEGIN
        DropConnection := FALSE;
        Signal (OKToConnect);
        w := CAST(Window, param);
        LOOP
            ch := GetKey(w);
            IF ch = CHR(0) THEN
                ch := GetKey(w);
                IF ch = "H" THEN                        (* cursor up *)
                    NEW (p);  p^.category := upkey;
                    AddToQueue (MessageQueue, p);
                ELSIF ch = "P" THEN                     (* cursor down *)
                    NEW (p);  p^.category := downkey;
                    AddToQueue (MessageQueue, p);
                END (*IF*);
            ELSIF ch = CHR(11) THEN                     (* ^K = kill server *)
                NEW (p);  p^.category := killserver;
                AddToQueue (MessageQueue, p);
            ELSIF CAP(ch) = "K" THEN                    (* K = kill *)
                NEW (p);  p^.category := kill;
                AddToQueue (MessageQueue, p);
            ELSIF CAP(ch) = 'S' THEN                    (* S = setup *)
                DropConnection := TRUE;
                Signal (OKToConnect);
                NEW (p);  p^.category := reset;
                AddToQueue (MessageQueue, p);
                WHILE DropConnection DO
                    Sleep (60);
                END (*WHILE*);
                EditAccessParameters;
                DropConnection := FALSE;
                Signal (OKToConnect);
            ELSIF CAP(ch) = 'X' THEN                    (* X = exit *)
                NEW (p);  p^.category := exit;
                AddToQueue (MessageQueue, p);
                WHILE NOT ShutdownRequest DO
                    Sleep (300);
                END (*WHILE*);
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        Signal (AllDone);

    END KeyboardCommands;

(********************************************************************************)
(*                         OPTION TO SHUT DOWN THE SERVER                       *)
(********************************************************************************)

PROCEDURE ShutDownServer;

    (* User options: G for gradual shutdown, Q for quick shutdown, Esc to       *)
    (* abort this operation.                                                    *)

    VAR w: Window;  ch: CHAR;  done: BOOLEAN;

    BEGIN
        OpenWindow (w, white, blue, ScreenRows DIV 2 - 4, ScreenRows DIV 2 - 1,
                                     14, 65, noframe, nodivider);
        WriteLn (w);
        WriteString (w, " Type G for gradual shutdown, Q for quick shutdown,");
        WriteLn (w);
        WriteString (w, "            Esc to abort this operation");
        done := FALSE;
        REPEAT
            ch := CAP(GetKey(w));
            IF (ch = 'G') OR (ch = 'Q') THEN
                ShutdownOption := ch;
                done := TRUE;
            ELSIF ch = Esc THEN
                ShutdownOption := Nul;
                done := TRUE;
            END (*IF*);
        UNTIL done;
        Signal (OKToConnect);
        CloseWindow (w);
    END ShutDownServer;

(********************************************************************************)
(*                              DISPLAY TASK                                    *)
(********************************************************************************)

PROCEDURE DisplayUsers;

    VAR w: Window;  p: MessagePointer;  NewCycle, Finished: BOOLEAN;
        D: ListBox;
        currentitem: ARRAY [0..79] OF CHAR;

    BEGIN
        OpenWindow (w, black, white, 1, ScreenRows-2, 0, 79, noframe, nodivider);
        D := CreateListBox (w, 0, 0, ScreenRows-2, 80);
        EVAL(CreateTask1 (KeyboardCommands, 2, "Kbd input", CAST(ADDRESS,w)));
        NewCycle := TRUE;  Finished := FALSE;
        REPEAT
            p := TakeFromQueue (MessageQueue);
            CASE p^.category OF
              |  normal:    IF NewCycle THEN
                                ClearListBox (D);  NewCycle := FALSE;
                            END (*IF*);
                            LBAppend (D, p^.text);
                            HighlightOn (D);

              |  lastline:  NewCycle := TRUE;

              |  upkey:     EVAL(CursorBackward (D));

              |  downkey:   EVAL(CursorForward (D));

              |  reset:     HighlightOff (D);
                            ClearListBox (D);

              |  kill:      LBCurrent (D, currentitem);
                            IF LENGTH(currentitem) > 6 THEN
                                WHILE KillFlag DO
                                    Sleep (300);
                                END (*WHILE*);
                                Strings.Extract (currentitem, 1, 6, userID);
                                KillFlag := TRUE;
                                Signal (OKToConnect);
                                Signal (OKToConnect);
                            END (*IF*);

              |  killserver:
                            ShutDownServer;

              |  exit:      Finished := TRUE;

            ELSE
                WriteLn (w);
                WriteString (w, "Unknown category");
            END (*CASE*);
            DISPOSE (p);

        UNTIL Finished;

        DestroyListBox (D);
        CloseWindow (w);

        (* Tell the TalkToServer task to log out and exit, and tell the         *)
        (* KeyboardCommands task to terminate.                                  *)

        ShutdownRequest := TRUE;  DropConnection := TRUE;
        Signal (OKToConnect);
        Wait (AllDone);  Wait (AllDone);

    END DisplayUsers;

(********************************************************************************)
(*                                  MAIN PROGRAM                                *)
(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    KillFlag := FALSE;  userID := "      ";
    ShutdownRequest := FALSE;  DropConnection := FALSE;
    ShutdownOption := Nul;
    CreateSemaphore (AllDone, 0);
    CreateSemaphore (OKToConnect, 0);
    OpenWindow (TopTitle, yellow, red, 0, 0, 0, 79, noframe, nodivider);
    WriteString (TopTitle, "  FTPSERVER USER DISPLAY");
    OpenWindow (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
    WriteString (bottombar, " S setup  K kill user  ^K kill server  X exit  ");
    WriteChar (bottombar, CHR(24));  WriteChar (bottombar, CHR(25));
    WriteString (bottombar, " select");
    CreateQueue (MessageQueue);
    EVAL(CreateTask (TalkToServer, 2, "Talk to server"));
    DisplayUsers;
FINALLY
    DestroyQueue (MessageQueue);
    CloseWindow (bottombar);  CloseWindow (TopTitle);
    DestroySemaphore (OKToConnect);
    DestroySemaphore (AllDone);
END TMonitor.

