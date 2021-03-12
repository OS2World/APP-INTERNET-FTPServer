(**************************************************************************)
(*                                                                        *)
(*  Monitor for FtpServer                                                 *)
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

IMPLEMENTATION MODULE FtpCl2;

        (********************************************************)
        (*                                                      *)
        (* FtpServer administration - communication with server *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 October 1997                 *)
        (*  Last edited:        20 November 2020                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, OS2, DID;

FROM SYSTEM IMPORT ADR, LOC, INT16, ADDRESS, CAST;

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

FROM MonINI IMPORT
    (* proc *)  GetINIFileName, OpenINIFile, CloseINIFile;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid,
                INIGet, INIGetString, INIPut, INIPutString,
                SetInitialWindowPosition, StoreWindowPosition;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4, IPToString;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    NameString = ARRAY [0..31] OF CHAR;

VAR
    (* The socket we use for talking to the server. *)

    CommandSocket: Socket;

    (* The server's name, addresses, and port number. *)

    ServerName: ARRAY [0..511] OF CHAR;
    ServerAddr: ARRAY [0..MAXADDRS] OF CARDINAL;
    ServerPort: CARDINAL;

    (* Username and password for logging in to the server. *)

    UserName, Password: NameString;

    (* Cycle time, in seconds. *)

    UpdateInterval: CARDINAL;

    (* Option to keep monitor window on top of the z-order. *)

    AlwaysOnTop: BOOLEAN;

    (* Buffer to hold responses received on the command channel.  We    *)
    (* need a global buffer because responses can arrive before we're   *)
    (* ready to deal with them.                                         *)

    ResponseBuffer: ARRAY [0..127] OF CHAR;

    (* RBpos is the character position we're up to in ResponseBuffer,   *)
    (* and RBlength is the number of characters in ResponseBuffer.      *)

    RBpos, RBlength: CARDINAL;

    (* Temporary arrangment for debugging. *)

    debug: OS2.HWND;

(************************************************************************)
(*                           DEBUGGING CODE                             *)
(************************************************************************)

PROCEDURE SetMessageWindow (w: OS2.HWND);

    (* Window for debugging messages. *)

    BEGIN
        debug := w;
    END SetMessageWindow;

(************************************************************************)
(*                   RETURNING THE SAMPLING INTERVAL                    *)
(************************************************************************)

PROCEDURE SamplingInterval(): CARDINAL;

    (* Returns the sampling interval, in seconds. *)

    BEGIN
        RETURN UpdateInterval;
    END SamplingInterval;

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
                RETURN CHR(0);
            END (*IF*);
            RBpos := 0;
        END (*IF*);
        result := ResponseBuffer[RBpos];  INC(RBpos);
        RETURN result;
    END Getch;

(************************************************************************)

PROCEDURE Send (s: Socket;  command: ARRAY OF CHAR): BOOLEAN;

    (* Sends a character string to the server. *)

    VAR length: CARDINAL;  CRLF: ARRAY [0..1] OF CHAR;

    BEGIN
        CRLF[0] := CR;  CRLF[1] := LF;
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
            ELSIF ch = LF THEN EXIT(*LOOP*);
            ELSIF ch <> CR THEN
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
(*                      DEALING WITH INI FILE DATA                      *)
(************************************************************************)

PROCEDURE LoadINIData(): BOOLEAN;

    (* Loads setup parameters from the INI file, returns the value      *)
    (* of the "always on top" option.                                   *)

    VAR hini: HINI;
        AppServer: ARRAY [0..6] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, AppServer, name, variable);
        END GetItem;

    (********************************************************************)

    PROCEDURE GetString (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, AppServer, name, variable);
        END GetString;

    (********************************************************************)

    BEGIN
        AppServer := "Server";
        ServerPort := 21;
        UserName := "";
        Password := "";
        ServerName := "";
        UpdateInterval := 10;
        AlwaysOnTop := FALSE;

        hini := OpenINIFile();
        IF INIValid (hini) THEN
            IF NOT GetItem ("Port", ServerPort) THEN
                ServerPort := 21;
            END (*IF*);
            IF NOT GetString ("UserName", UserName) THEN
                UserName := "";
            END (*IF*);
            IF NOT GetString ("Password", Password) THEN
                Password := "";
            END (*IF*);
            IF NOT GetString ("Hostname", ServerName) THEN
                IPToString (Swap4(gethostid()), FALSE, ServerName);
            END (*IF*);
            IF NOT GetItem ("UpdateInterval", UpdateInterval) THEN
                UpdateInterval := 10;
            END (*IF*);
            IF NOT GetItem ("AlwaysOnTop", AlwaysOnTop) THEN
                AlwaysOnTop := FALSE;
            END (*IF*);

            CloseINIFile (hini);

        END (*IF*);
        RETURN AlwaysOnTop;

    END LoadINIData;

(************************************************************************)

PROCEDURE LoadData (hwnd: OS2.HWND);

    (* Fills the dialogue elements with data from the INI file.       *)

    BEGIN
        OS2.WinSetDlgItemText (hwnd, DID.Hostname, ServerName);
        OS2.WinSetDlgItemShort (hwnd, DID.Port, ServerPort, FALSE);
        OS2.WinSetDlgItemText (hwnd, DID.Username, UserName);
        OS2.WinSetDlgItemText (hwnd, DID.Password, Password);
        OS2.WinSetDlgItemShort (hwnd, DID.UpdateInterval, UpdateInterval, FALSE);
        OS2.WinSendDlgItemMsg (hwnd, DID.AlwaysOnTop, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(AlwaysOnTop)), NIL);
    END LoadData;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Updates our global variables from the dialogue, also stores      *)
    (* the updated values in our INI file.                              *)

    VAR hini: HINI;  temp: INT16;
        AppServer: ARRAY [0..6] OF CHAR;

    BEGIN
        AppServer := "Server";
        hini := OpenINIFile();

        OS2.WinQueryDlgItemText (hwnd, DID.Hostname, 512, ServerName);
        INIPutString (hini, AppServer, "Hostname", ServerName);

        OS2.WinQueryDlgItemShort (hwnd, DID.Port, temp, FALSE);
        ServerPort := CAST (CARDINAL, temp);
        INIPut (hini, AppServer, "Port", ServerPort);

        OS2.WinQueryDlgItemText (hwnd, DID.Username, 32, UserName);
        INIPutString (hini, AppServer, "UserName", UserName);

        OS2.WinQueryDlgItemText (hwnd, DID.Password, 32, Password);
        INIPutString (hini, AppServer, "Password", Password);

        OS2.WinQueryDlgItemShort (hwnd, DID.UpdateInterval, temp, FALSE);
        UpdateInterval := CAST (CARDINAL, temp);
        INIPut (hini, AppServer, "UpdateInterval", UpdateInterval);

        AlwaysOnTop := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.AlwaysOnTop,
                                              OS2.BM_QUERYCHECK, NIL, NIL)) > 0;
        INIPut (hini, AppServer, "AlwaysOnTop", AlwaysOnTop);

        CloseINIFile (hini);

    END StoreData;

(************************************************************************)
(*                       THE SETUP DIALOGUE                             *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   LoadData (hwnd);
                   RETURN NIL;

           |  OS2.WM_COMMAND:

                   IF OS2.SHORT1FROMMP(mp1) = DID.SetupOK THEN
                       StoreData (hwnd);
                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_CLOSE:
                   StoreData (hwnd);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE SetupDialogue (owner: OS2.HWND): BOOLEAN;

    (* Creates the remote setup dialogue box.  Returns the state of     *)
    (* the "always on top" checkbox.                                    *)

    VAR hwnd: OS2.HWND;
        ININame: ARRAY [0..511] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.SetupDialogue,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)


        GetINIFileName (ININame);
        SetInitialWindowPosition (hwnd, ININame, "Setup");
        OS2.WinShowWindow (hwnd, TRUE);

        OS2.WinProcessDlg(hwnd);
        StoreWindowPosition (hwnd, ININame, "Setup");
        OS2.WinDestroyWindow (hwnd);
        RETURN AlwaysOnTop;
    END SetupDialogue;

(************************************************************************)

BEGIN
    sock_init;
    CommandSocket := NotASocket;
    RBpos := 0;  RBlength := 0;
FINALLY
    IF CommandSocket <> NotASocket THEN
        soclose (CommandSocket);
    END (*IF*);
END FtpCl2.

