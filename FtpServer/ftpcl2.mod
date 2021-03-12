(**************************************************************************)
(*                                                                        *)
(*  Text-mode monitor for FtpServer                                       *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE FtpCl2;

        (********************************************************)
        (*                                                      *)
        (* FtpServer administration - communication with server *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 October 1997                 *)
        (*  Last edited:        2 October 2019                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings;

FROM SYSTEM IMPORT ADR, LOC, ADDRESS;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, AddressFamily, SocketType, SockAddr,
    (* proc *)  sock_init, socket, connect, gethostid, send, recv, soclose;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY,
    (* type *)  InternetSocketAddress,
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* const*)  MAXADDRS,
    (* type *)  HostEntPtr,
    (* proc *)  gethostbyname;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CloseINIFile, INIValid,
                INIDeleteKey, INIGet, INIGetString, INIPut, INIPutString, INIPutBinary;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4, IPToString;

FROM MaintenancePages IMPORT
    (* type *)  MaintenancePage,
    (* proc *)  CreateMaintenancePage, Associate, RemoveMaintenancePage;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  CardinalField, StringField, Combine, ScreenEdit,
                DeleteStructure;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, OpenWindowHidden, CloseWindow, SetCursor,
                WriteChar, WriteString, WriteLn, GetKey,
                GetScreenSize, PutOnTop;

FROM NumericIO IMPORT
    (* proc *)  WriteCard;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    NameString = ARRAY [0..31] OF CHAR;

VAR
    (* Transition arrangement, to deal with INI data from an old version. *)

    OldName: NameString;

    (* Number of rows available on the screen. *)

    ScreenRows: CARDINAL;

    (* INI file handle. *)

    hini: HINI;

    (* The socket we use for talking to the server. *)

    CommandSocket: Socket;

    (* The server's name, addresses, and port number. *)

    ServerName: ARRAY [0..511] OF CHAR;
    ServerAddr: ARRAY [0..MAXADDRS] OF CARDINAL;
    ServerPort: CARDINAL;

    (* Username and password for logging in to the server. *)

    UserName, Password: NameString;

    (* Screen message windows. *)

    status, bottombar: Window;

    (* Buffer to hold responses received on the command channel.  We    *)
    (* need a global buffer because responses can arrive before we're   *)
    (* ready to deal with them.                                         *)

    ResponseBuffer: ARRAY [0..127] OF CHAR;

    (* RBpos is the character position we're up to in ResponseBuffer,   *)
    (* and RBlength is the number of characters in ResponseBuffer.      *)

    RBpos, RBlength: CARDINAL;

    (* Maintenance page for debug messages. *)

    DebugPage: MaintenancePage;

(************************************************************************)
(*                            SOCKET I/O                                *)
(************************************************************************)

PROCEDURE Getch (s: Socket): CHAR;

    (* Result is CHR(0) if connection fails. *)

    VAR result: CHAR;  NullResponseCount: CARDINAL;

    BEGIN
        IF RBpos >= RBlength THEN
            NullResponseCount := 0;
            REPEAT
                RBlength := recv (s, ResponseBuffer, SIZE(ResponseBuffer), 0);
                IF RBlength = 0 THEN
                    INC (NullResponseCount);
                    IF NullResponseCount > 20 THEN
                        RBlength := MAX(CARDINAL);
                    END (*IF*);
                END (*IF*);
            UNTIL RBlength <> 0;
            IF RBlength = MAX(CARDINAL) THEN
                RBpos := 0;  RBlength := 0;
                WriteString (status, "## recv failed");  WriteLn (status);
                RETURN CHR(0);
            END (*IF*);
            RBpos := 0;
        END (*IF*);
        result := ResponseBuffer[RBpos];  INC(RBpos);
        RETURN result;
    END Getch;

(************************************************************************)

PROCEDURE Send (s: Socket;  command: ARRAY OF CHAR): BOOLEAN;

    (* Sends a string to the server. *)

    VAR length: CARDINAL;  CRLF: ARRAY [0..1] OF CHAR;

    BEGIN
        CRLF[0] := CR;  CRLF[1] := LF;
        WriteString (status, "To server> ");
        WriteString (status, command);  WriteLn (status);
        length := send (s, command, LENGTH(command), 0);
        IF length = MAX(CARDINAL) THEN
            RETURN FALSE;
        ELSE
            RETURN send (s, CRLF, 2, 0) <> MAX(CARDINAL);
        END (*IF*);
    END Send;

(************************************************************************)
(*                PROCESSING THE REPLY FROM THE SERVER                  *)
(************************************************************************)

PROCEDURE GetLine (s: Socket;  VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Receives a single line of text from the server.  A function      *)
    (* return of FALSE meant that the connection failed.                *)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
        j := 0;
        LOOP
            ch := Getch(s);
            IF ch = CHR(0) THEN RETURN FALSE;
            ELSIF ch = LF THEN WriteLn (status);  EXIT(*LOOP*);
            ELSIF ch <> CR THEN
                WriteChar (status, ch);
                IF j <= HIGH(result) THEN
                    result[j] := ch;  INC(j);
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
        RETURN TRUE;
    END GetLine;

(************************************************************************)

PROCEDURE GetResponse (VAR (*OUT*) result: ARRAY OF CHAR;
                                VAR (*OUT*) MoreToCome: BOOLEAN): BOOLEAN;

    (* Returns one line of the server response to a command.            *)
    (* MoreToCome is set if this is part of a multi-line response, and  *)
    (* it's not the last line.   The function result is FALSE if we've  *)
    (* lost the connection.                                             *)

    TYPE CharSet = SET OF CHAR;

    CONST Digits = CharSet {'0'..'9'};

    VAR status: BOOLEAN;

    BEGIN
        status := GetLine (CommandSocket, result);
        MoreToCome := NOT(result[0] IN Digits)
                         OR
                         ((result[1] IN Digits) AND (result[2] IN Digits)
                                AND (result[3] = '-'));
        RETURN status;
    END GetResponse;

(************************************************************************)

PROCEDURE ReplyCode(): CARDINAL;

    (* Receives a (possibly multi-line) response from the server, and   *)
    (* returns the first digit of the numeric code.  The values are:    *)
    (*      0  Connection lost                                          *)
    (*      1  OK, another reply still to come                          *)
    (*      2  OK, command done                                         *)
    (*      3  OK, another command expected                             *)
    (*      4  Transient failure, try again later                       *)
    (*      5  Definite failure                                         *)

    VAR line: ARRAY [0..127] OF CHAR;  active, MoreToCome: BOOLEAN;

    BEGIN
        REPEAT
            active := GetResponse (line, MoreToCome);
        UNTIL NOT (MoreToCome AND active);
        IF active THEN
            RETURN ORD(line[0]) - ORD('0');
        ELSE
            RETURN 0;
        END (*IF*);
    END ReplyCode;

(********************************************************************************)
(*                             SCREEN OUTPUT                                    *)
(********************************************************************************)

PROCEDURE WriteIPAddress (w: Window;  ID: CARDINAL);

    (* Writes ID to screen in dotted quad notation. *)

    VAR result: ARRAY [0..16] OF CHAR;

    BEGIN
        IPToString (ID, TRUE, result);
        WriteString (w, result);
    END WriteIPAddress;

(********************************************************************************)
(*                        FINDING THE SERVER IP ADDRESSES                       *)
(********************************************************************************)

PROCEDURE GetHostAddresses(): CARDINAL;

    (* Loads the ServerAddr array with all known IP addresses for the host.     *)
    (* The return value is the number of addresses found.                       *)

    TYPE CharSet = SET OF CHAR;

    VAR count: CARDINAL;
        EntPtr: HostEntPtr;

    BEGIN
        count := 0;
        IF ServerName[0] IN CharSet {'0'..'9'} THEN
            ServerAddr[0] := inet_addr (ServerName);
            count := 1;
        ELSE
            EntPtr := gethostbyname (ServerName);
            IF (EntPtr <> NIL) AND (EntPtr^.h_addr_list <> NIL) THEN
                WHILE (count <= MAXADDRS)
                          AND (EntPtr^.h_addr_list^[count] <> NIL) DO
                    ServerAddr[count] := EntPtr^.h_addr_list^[count]^;
                    INC (count);
                END (*WHILE*);
            END (*IF*);
        END (*IF*);

        (* If all else fails, use the local host address. *)

        IF count = 0 THEN
            ServerAddr[0] := Swap4(gethostid());
            count := 1;
        END (*IF*);

        (* Add a zero entry to terminate the list. *)

        ServerAddr[count] := 0;
        RETURN count;

    END GetHostAddresses;

(************************************************************************)

PROCEDURE HostAddress (j: CARDINAL): CARDINAL;

    (* Returns the j'th address found by GetHostAddresses. *)

    BEGIN
        RETURN ServerAddr[j];
    END HostAddress;

(************************************************************************)
(*                     CONNECTING AND LOGGING IN                        *)
(************************************************************************)

PROCEDURE ConnectToServer (address: CARDINAL): BOOLEAN;

    (* Establishes a connection, returns TRUE iff successful. *)

    TYPE CharSet = SET OF CHAR;

    VAR target: SockAddr;

    BEGIN
        IF CommandSocket <> NotASocket THEN
            soclose (CommandSocket);
        END (*IF*);
        CommandSocket := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
        IF CommandSocket = NotASocket THEN
            RETURN FALSE;
        END (*IF*);

        (* Socket open, connect to the server. *)

        WITH target DO
            family := AF_INET;
            WITH in_addr DO
                port := Swap2 (ServerPort);
                addr := address;
                zero := Zero8;
            END (*WITH*);
        END (*WITH*);

        WriteString (status, "Trying ");
        WriteIPAddress (status, target.in_addr.addr);  WriteString (status, " port ");
        WriteCard (status, ServerPort);
        WriteLn (status);
        IF connect (CommandSocket, target, SIZE(target)) THEN

            soclose (CommandSocket);
            RETURN FALSE;

        ELSE
            (* Get the server's response to connection attempt. *)

            RETURN ReplyCode() = 2;

        END (*IF*);

    END ConnectToServer;

(************************************************************************)

PROCEDURE Login(): BOOLEAN;

    (* Attempts to log in, returns TRUE iff successful. *)

    VAR response: CARDINAL;
        buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        buffer := "USER ";
        Strings.Append (UserName, buffer);
        IF Send (CommandSocket, buffer) THEN
            response := ReplyCode();
            IF response = 3 THEN
                buffer := "PASS ";
                Strings.Append (Password, buffer);
                IF Send (CommandSocket, buffer) THEN
                    RETURN ReplyCode() = 2;
                ELSE
                    RETURN FALSE;
                END (*IF*);
            ELSE RETURN response = 2;
            END (*IF*);
        ELSE
            RETURN FALSE;
        END (*IF*);
    END Login;

(************************************************************************)

PROCEDURE SendCommand (command: ARRAY OF CHAR): BOOLEAN;

    (* Sends a command to the server.  The function result is FALSE if  *)
    (* we've lost the connection.                                       *)

    BEGIN
        RETURN Send (CommandSocket, command);
    END SendCommand;

(************************************************************************)
(*                       DEALING WITH MONITOR.INI                       *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "monitor.ini". *)

    VAR ServerApp: ARRAY [0..6] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, ServerApp, name, variable);
        END GetItem;

    (********************************************************************)

    PROCEDURE GetStringItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, ServerApp, name, variable);
        END GetStringItem;

    (********************************************************************)

    VAR FName: ARRAY [0..11] OF CHAR;

    BEGIN
        ServerApp := "Server";
        FName := "Monitor.INI";
        hini := OpenINIFile (FName);
        IF NOT INIValid (hini) THEN
            WriteString (status, "Could not open MONITOR.INI");
            WriteLn (status);
        ELSE
            IF NOT GetItem ("Port", ServerPort) THEN
                ServerPort := 21;
            END (*IF*);
            IF NOT GetStringItem ("UserName", UserName) THEN
                UserName := "";
            END (*IF*);
            IF NOT GetStringItem ("Password", Password) THEN
                Password := "";
            END (*IF*);
            IF GetItem ("IP", OldName) THEN
                Strings.Assign (OldName, ServerName);

                (* Remove obsolete INI file entry. *)

                INIDeleteKey (hini, ServerApp, "IP");

            ELSIF NOT GetStringItem ("Hostname", ServerName) THEN
                IPToString (Swap4(gethostid()), FALSE, ServerName);
            END (*IF*);
        END (*IF*);

    END LoadINIData;

(************************************************************************)

PROCEDURE EditAccessParameters;

    (* Allows the user to specify user name, etc., for accessing the server. *)

    VAR w, help: Window;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;

    BEGIN
        OpenWindow (help, yellow, red, ScreenRows-3, ScreenRows-1, 0, 79, noframe, nodivider);
        WriteLn (help);
        WriteString (help,
          "    These settings are needed to allow this program to log in to the server");

        OpenWindow (w, white, blue, ScreenRows DIV 2 - 4, ScreenRows DIV 2 + 5, 1, 78, noframe, nodivider);
        WriteLn (w);
        WriteString (w, " Access details for connecting to server");

        SetCursor (w, 3, 2);  WriteString (w, "Server hostname");
        SetCursor (w, 4, 2);  WriteString (w, "Server port");
        SetCursor (w, 5, 2);  WriteString (w, "User name");
        SetCursor (w, 6, 2);  WriteString (w, "Password");
        SetCursor (w, 8, 1);  WriteString (w, " Type Esc when finished");

        R := StringField (ServerName, 3, 21, 55);
        Combine (R, CardinalField (ServerPort, 4, 21, 8));
        Combine (R, StringField (UserName, 5, 21, 32));
        Combine (R, StringField (Password, 6, 21, 32));
        PutOnTop (w);

        LOOP
            ScreenEdit (w, R, abort);
            IF abort THEN EXIT(*LOOP*) END(*IF*);

            (* Consume the character that took us off the edge. *)

            ch := GetKey (w);
            IF ch = CHR(0) THEN
                EVAL (GetKey (w));
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        CloseWindow (help);
        CloseWindow (w);

    END EditAccessParameters;

(************************************************************************)

PROCEDURE CloseINI;

    (* Writes data back to the INI file, then closes the INI file. *)

    VAR ServerApp: ARRAY [0..6] OF CHAR;

    (********************************************************************)

    PROCEDURE PutItem (name: ARRAY OF CHAR;
                                      VAR (*OUT*) variable: ARRAY OF LOC);

        BEGIN
            INIPut (hini, ServerApp, name, variable);
        END PutItem;

    (********************************************************************)

    PROCEDURE PutStringItem (name: ARRAY OF CHAR;
                                      VAR (*OUT*) variable: ARRAY OF CHAR);

        BEGIN
            INIPutString (hini, ServerApp, name, variable);
        END PutStringItem;

    (********************************************************************)

    BEGIN
        ServerApp := "Server";
        IF INIValid(hini) THEN
            PutItem ("Port", ServerPort);
            PutStringItem ("UserName", UserName);
            PutStringItem ("Password", Password);
            PutStringItem ("Hostname", ServerName);
            CloseINIFile (hini);
        ELSE
            WriteString (status, "Could not close INI file");
        END (*IF*);
    END CloseINI;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    sock_init;
    CommandSocket := NotASocket;
    CreateMaintenancePage (DebugPage);
    OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
    Associate (bottombar, DebugPage);
    SetCursor (bottombar, 0, 50);
    WriteString (bottombar, "Alt/P return to user display");
    OpenWindowHidden (status, blue, cyan, 3, 20, 1, 78, simpleframe, nodivider);
    Associate (status, DebugPage);
    LoadINIData;
    RBpos := 0;  RBlength := 0;
FINALLY
    CloseINI;
    CloseWindow (status);
    IF CommandSocket <> NotASocket THEN
        soclose (CommandSocket);
    END (*IF*);
    CloseWindow (bottombar);
    RemoveMaintenancePage (DebugPage);
END FtpCl2.

