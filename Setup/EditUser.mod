(**************************************************************************)
(*                                                                        *)
(*  Setup for FtpServer                                                   *)
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

IMPLEMENTATION MODULE EditUser;

        (************************************************************)
        (*                                                          *)
        (*                  PM Setup for FtpServer                  *)
        (*          Dialogue to edit the data for one user          *)
        (*                                                          *)
        (*    Started:        25 October 1999                       *)
        (*    Last edited:    9 December 2018                       *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM, OS2, Remote, DID, Strings, Tree, UIPControls, IPFilters, HideList;

FROM Arith64 IMPORT
    (* const*)  Zero64, Max64,
    (* type *)  CARD64,
    (* proc *)  Compare64;

FROM MiscFuncs IMPORT
    (* proc *)  ToLower, EVAL;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

FROM FSUINI IMPORT
    (* proc *)  OpenINIFile, OpenINIForUser, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIFetch, INIGetCard, INIGetString, INIPutString, INIPut,
                INIDeleteApp, INIDeleteKey, ItemSize;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;
    RealNameSize = 64;
    PasswordSize = 32;
    NoteFieldSize = 2048;
    MEG = 1048576;

TYPE
    UserCategory = (NoSuchUser, NoPasswordNeeded, GuestUser, NormalUser,
                    Manager, UserTemplate);
    NameString = ARRAY [0..NameLength-1] OF CHAR;
    PassString = ARRAY [0..PasswordSize] OF CHAR;

VAR
    User: RECORD
              LoginLimit: CARDINAL;
              Active, NewUser: BOOLEAN;
              NameChanged, SingleUse, DeleteExpired, SuppressLog: BOOLEAN;
              UseTemplate, OldUseTemplate: BOOLEAN;
              PasswordChanged, EncryptPassword,
                         OriginalEncryptPassword : BOOLEAN;
              category: UserCategory;
              name, OldName, TemplateName: NameString;
              Password, OriginalPassword: PassString;
              RealName: ARRAY [0..RealNameSize-1] OF CHAR;
              Notes: ARRAY [0..NoteFieldSize-1] OF CHAR;
              limit, speedlimit, upspeedlimit: CARDINAL;
              upsizelimit: CARD64;
              UserTreeRoot, TemplateTreeRoot: Tree.DirEntryPtr;
              UIPList: IPFilters.ListPtr;
              HideList: HideList.ListPtr;
          END (*RECORD*);

    swp: ARRAY [0..3] OF OS2.SWP;
        (* These are positions for the two variable-position subwindows. *)

    HidePasswords, DetectLLChanges: BOOLEAN;
    passwindow: OS2.HWND;

(************************************************************************)
(*                ENABLING FIELDS RELATED TO LOGIN LIMIT                *)
(************************************************************************)

PROCEDURE SetLoginLimit (hwnd: OS2.HWND;  limit: CARDINAL);

    (* Saves limit in User.LoginLimit.  Shows or hides some dialogue    *)
    (* elements depending on whether it is zero or nonzero.             *)

    VAR w1, w2, w3, w5, w6: OS2.HWND;

    BEGIN
        DetectLLChanges := FALSE;
        User.LoginLimit := limit;
        OS2.WinSendDlgItemMsg (hwnd, DID.LimitLogins, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(limit>0)), NIL);
        WinSetDlgItemCard (hwnd, DID.LoginLimit, limit);

        w1 := OS2.WinWindowFromID(hwnd, DID.LimitLogins);
        w2 := OS2.WinWindowFromID(hwnd, DID.LoginLimit);
        w3 := OS2.WinWindowFromID(hwnd, DID.TextLogins);
        w5 := OS2.WinWindowFromID(hwnd, DID.limit_delete);
        w6 := OS2.WinWindowFromID(hwnd, DID.limit_deactivate);

        IF User.DeleteExpired THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.limit_delete, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.limit_deactivate, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
            OS2.WinSetWindowPos (w2, 0, swp[0].x, swp[0].y, 0, 0, OS2.SWP_MOVE);
            OS2.WinSetWindowPos (w3, 0, swp[1].x, swp[1].y, 0, 0, OS2.SWP_MOVE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.limit_delete, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.limit_deactivate, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
            OS2.WinSetWindowPos (w2, 0, swp[2].x, swp[2].y, 0, 0, OS2.SWP_MOVE);
            OS2.WinSetWindowPos (w3, 0, swp[3].x, swp[3].y, 0, 0, OS2.SWP_MOVE);
        END (*IF*);

        IF limit = 0 THEN
            OS2.WinEnableWindow (w2, FALSE);
            OS2.WinEnableWindow (w3, FALSE);
            OS2.WinEnableWindow (w5, FALSE);
            OS2.WinEnableWindow (w6, FALSE);
        ELSE
            OS2.WinEnableWindow (w2, TRUE);
            OS2.WinEnableWindow (w3, TRUE);
            OS2.WinEnableWindow (w5, TRUE);
            OS2.WinEnableWindow (w6, TRUE);
            IF limit = 1 THEN
                OS2.WinSetWindowText (w3, "login");
            ELSE
                OS2.WinSetWindowText (w3, "logins");
            END (*IF*);
        END (*IF*);
        DetectLLChanges := TRUE;

    END SetLoginLimit;

(************************************************************************)
(*               TURNING 'TEMPLATE' OPTIONS ON AND OFF                  *)
(************************************************************************)

PROCEDURE EnableUseTemplateFields (hwnd: OS2.HWND;  enable: BOOLEAN);

    (* Enables or disables the dialogue elements associated with the    *)
    (* "use a template" option.                                         *)

    VAR subwindow1, subwindow2: OS2.HWND;
        root: Tree.DirEntryPtr;

    BEGIN
        subwindow1 := OS2.WinWindowFromID(hwnd, DID.TemplateName);
        subwindow2 := OS2.WinWindowFromID(hwnd, DID.EditDirButton);
        IF enable THEN
             OS2.WinEnableWindow (subwindow1, TRUE);
             OS2.WinEnableWindow (subwindow2, FALSE);
             root := User.TemplateTreeRoot;
        ELSE
             OS2.WinEnableWindow (subwindow1, FALSE);
             OS2.WinEnableWindow (subwindow2, TRUE);
             root := User.UserTreeRoot;
        END (*IF*);
        User.UseTemplate := enable;
        User.OldUseTemplate := enable;
        Tree.InitialDisplay (OS2.WinWindowFromID(hwnd, DID.DirSummary), root);
    END EnableUseTemplateFields;

(************************************************************************)

PROCEDURE EnableIsTemplateFields (hwnd: OS2.HWND;  enable: BOOLEAN);

    (* Enables or disables the dialogue elements associated with the    *)
    (* "is a template" option.                                          *)

    VAR title: ARRAY [0..NameLength-1] OF CHAR;
        UseTemplateWasOn: BOOLEAN;

    BEGIN
        IF enable THEN

            IF User.OldName[0] = Nul THEN
                title := "New template";
            ELSE
                title := "Template name: ";
            END (*IF*);

            (* Turn off the "use template" option. *)

            UseTemplateWasOn := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.UseTemplate,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
            OS2.WinSendDlgItemMsg (hwnd, DID.UseTemplate, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(FALSE)), NIL);
            EnableUseTemplateFields (hwnd, FALSE);
            User.OldUseTemplate := UseTemplateWasOn;
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UseTemplate), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.TemplateName), FALSE);

            (* Turn off a bunch of other things. *)

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.Inactive), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LastLoginLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LastLogin), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LimitLogins), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.limit_delete), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.limit_deactivate), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LoginLimit), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.TextLogins), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SuppressLog), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel1), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel2), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel3), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel4), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitGroup), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimit), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UpSpeedLimit), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UserLimitLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UserLimit), FALSE);
            OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd, DID.RealNameLabel),
                                 0, 0, 0, 150, 25, OS2.SWP_SIZE);
            OS2.WinSetDlgItemText (hwnd, DID.RealNameLabel, "Template name");
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.RealName), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UsernameLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PasswordLabel), FALSE);
            OS2.WinShowWindow (passwindow, FALSE);

        ELSE
            IF User.OldName[0] = Nul THEN
                title := "New user";
            ELSE
                title := "Username: ";
            END (*IF*);

            IF User.OldUseTemplate THEN
                OS2.WinSendDlgItemMsg (hwnd, DID.UseTemplate, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(TRUE)), NIL);
            END (*IF*);
            EnableUseTemplateFields (hwnd, User.OldUseTemplate);

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.Inactive), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LastLoginLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LastLogin), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LimitLogins), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.limit_delete), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.limit_deactivate), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.LoginLimit), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.TextLogins), TRUE);
            SetLoginLimit (hwnd, User.LoginLimit);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SuppressLog), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel1), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel2), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel3), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitLabel4), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimitGroup), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SpeedLimit), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UpSpeedLimit), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UserLimitLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UserLimit), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.RealName), TRUE);
            OS2.WinSetWindowPos (OS2.WinWindowFromID(hwnd, DID.RealNameLabel),
                                 0, 0, 0, 92, 25, OS2.SWP_SIZE);
            OS2.WinSetDlgItemText (hwnd, DID.RealNameLabel, "Real name");

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UsernameLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PasswordLabel), TRUE);
            OS2.WinShowWindow (passwindow, TRUE);

            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UseTemplate), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.TemplateName), TRUE);

        END (*IF*);

        Strings.Append (User.OldName, title);
        OS2.WinSetWindowText (hwnd, title);

    END EnableIsTemplateFields;

(************************************************************************)
(*                     INITIAL LOAD FROM INI FILE                       *)
(************************************************************************)

PROCEDURE LoadDialogueData (hwnd: OS2.HWND;  username: ARRAY OF CHAR;
                                             clone: BOOLEAN);

    (* Fills in all fields for the dialogue. *)

    VAR val: CARDINAL;
        SingleUse: BOOLEAN;
        passstring: PassString;
        subwindow1, subwindow2: OS2.HWND;

    BEGIN
        DetectLLChanges := FALSE;
        Strings.Assign (username, User.name);
        OpenINIFile;
        IF NOT INIFetch ('$SYS', "HidePasswords", HidePasswords) THEN
            HidePasswords := FALSE;
        END (*IF*);
        CloseINIFile;

        OS2.WinSetDlgItemText (hwnd, DID.LastLoginLabel, "Last login:");
        passstring := "never";
        OS2.WinSetDlgItemText (hwnd, DID.LastLogin, passstring);

        OS2.WinSetDlgItemText (hwnd, DID.UserName, username);
        subwindow1 := OS2.WinWindowFromID(hwnd, DID.Password);
        subwindow2 := OS2.WinWindowFromID(hwnd, DID.PasswordH);
        IF HidePasswords THEN
            OS2.WinShowWindow (subwindow1, FALSE);
            passwindow := subwindow2;
        ELSE
            OS2.WinShowWindow (subwindow2, FALSE);
            passwindow := subwindow1;
        END (*IF*);
        OS2.WinShowWindow (passwindow, FALSE);

        User.Active := TRUE;
        User.NewUser := username[0] = Nul;
        User.SingleUse := FALSE;
        User.DeleteExpired := FALSE;
        User.SuppressLog := FALSE;
        User.LoginLimit := 0;
        User.Password := "";
        User.RealName := "";
        User.Notes := "";
        User.UserTreeRoot := NIL;
        User.TemplateTreeRoot := NIL;
        User.UseTemplate := FALSE;
        User.TemplateName := "";
        User.UIPList := NIL;

        IF User.NewUser THEN

            User.OriginalEncryptPassword := FALSE;
            User.EncryptPassword := HidePasswords;
            User.category := NormalUser;

            (* Limits. *)

            val := MAX(CARDINAL);
            WinSetDlgItemCard (hwnd, DID.UserLimit, val);
            WinSetDlgItemCard (hwnd, DID.SpeedLimit, val);
            WinSetDlgItemCard (hwnd, DID.UpSpeedLimit, val);
            OS2.WinSetDlgItemText (hwnd, DID.UpSizeLimit, "unlimited");

            OpenINIFile;

        ELSE

            OpenINIForUser(username);

            IF NOT INIGetString (username, "LastLogin", passstring) THEN
                passstring := "unknown";
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, DID.LastLogin, passstring);

            IF NOT INIFetch (username, "Active", User.Active) THEN
                User.Active := TRUE;
            END (*IF*);

            IF INIFetch (username, "SingleUse", SingleUse) THEN
                (* Obsolete option *)
                IF SingleUse THEN
                    SetLoginLimit (hwnd, 1);
                ELSE
                    SetLoginLimit (hwnd, 0);
                END (*IF*);
                INIDeleteKey (username, "SingleUse");
            END (*IF*);

            EVAL (INIFetch (username, "LoginLimit", User.LoginLimit));
            EVAL (INIFetch (username, "DeleteExpired", User.DeleteExpired));
            EVAL (INIFetch (username, "SuppressLog", User.SuppressLog));

            IF INIFetch (username, "EncryptPassword", User.EncryptPassword) THEN
                User.OriginalEncryptPassword := User.EncryptPassword;
            ELSE
                User.OriginalEncryptPassword := FALSE;
                User.EncryptPassword := HidePasswords;
            END (*IF*);

            EVAL (INIGetString (username, "Password", User.Password));
            EVAL (INIGetString (username, "RealName", User.RealName));
            EVAL (INIGetString (username, "Notes", User.Notes));

            IF (NOT INIFetch (username, "Category", User.category))
                           OR (User.category = NoSuchUser) THEN
                User.category := NormalUser;
            END (*IF*);

            EVAL (INIFetch (username, "UseTemplate", User.UseTemplate));
            EVAL (INIGetString (username, "TemplateName", User.TemplateName));

            (* User limit and speed limits. *)

            IF NOT INIGetCard (username, "UserLimit", val) THEN
                val := MAX(CARDINAL);
            END (*IF*);
            WinSetDlgItemCard (hwnd, DID.UserLimit, val);

            IF NOT INIGetCard (username, "SpeedLimit", val) THEN
                val := MAX(CARDINAL);
            ELSIF val = 0 THEN
                val := 1;
            END (*IF*);
            WinSetDlgItemCard (hwnd, DID.SpeedLimit, val);

            IF NOT INIGetCard (username, "UpSpeedLimit", val) THEN
                (* Keep previous val. *)
            ELSIF val = 0 THEN
                val := 1;
            END (*IF*);
            WinSetDlgItemCard (hwnd, DID.UpSpeedLimit, val);

            (* UpSizeLimit is a special case.  We store it in the INI   *)
            (* file as a CARD64, but for display we divide it by 2^20   *)

            IF NOT INIFetch (username, "UpSizeLimit", User.upsizelimit) THEN
                User.upsizelimit := Zero64;
            END (*IF*);
            IF (Compare64(User.upsizelimit,Zero64)=0) OR (User.upsizelimit.high > 4095) THEN
                OS2.WinSetDlgItemText (hwnd, DID.UpSizeLimit, "unlimited");
            ELSE
                (* Divide by 2^20. *)
                val := 4096*User.upsizelimit.high + User.upsizelimit.low / MEG;
                WinSetDlgItemCard (hwnd, DID.UpSizeLimit, val);
            END (*IF*);

        END (*IF*);

        User.TemplateTreeRoot := Tree.LoadPermissions (User.TemplateName);
        User.UserTreeRoot := Tree.LoadPermissions (username);

        CloseINIFile;

        User.UIPList := IPFilters.LoadIPFilterList (username);
        IF (User.UIPList = NIL) OR (User.UIPList^.next = NIL) THEN
            OS2.WinSetDlgItemText (hwnd, DID.EditUIPListButton, "Add IP address controls");
        ELSE
            OS2.WinSetDlgItemText (hwnd, DID.EditUIPListButton, "Modify IP address controls");
        END (*IF*);

        User.HideList := HideList.LoadList (username);
        IF (User.HideList = NIL) OR (User.HideList^.next = NIL) THEN
            OS2.WinSetDlgItemText (hwnd, DID.EditHideListButton, "Add HideList");
        ELSE
            OS2.WinSetDlgItemText (hwnd, DID.EditHideListButton, "Modify HideList");
        END (*IF*);

        User.OldUseTemplate := User.UseTemplate;
        User.OriginalPassword := User.Password;
        User.PasswordChanged := FALSE;
        SetLoginLimit (hwnd, User.LoginLimit);

        OS2.WinSendDlgItemMsg (hwnd, DID.Inactive, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(NOT User.Active)), NIL);
        IF User.Active THEN
            val := 9;      (* dark blue *)
        ELSE
            val := 2;      (* light red *)
        END (*IF*);
        OS2.WinSetPresParam(
                  OS2.WinWindowFromID(hwnd, DID.Inactive),
                  OS2.PP_FOREGROUNDCOLORINDEX,
                  SIZE(val), SYSTEM.ADR(val));
        OS2.WinSendDlgItemMsg (hwnd, DID.SuppressLog, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(User.SuppressLog)), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.EncryptPassword, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(User.EncryptPassword)), NIL);

        passstring := User.Password;
        CASE User.category OF
          |  NoSuchUser:
          |  NoPasswordNeeded:
                OS2.WinSendDlgItemMsg (hwnd, DID.NoPassword, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(1), NIL);
                passstring := "";
          |  GuestUser:
                OS2.WinSendDlgItemMsg (hwnd, DID.Guest, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(1), NIL);
                passstring := "@";
          |  NormalUser:
                OS2.WinSendDlgItemMsg (hwnd, DID.User, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(1), NIL);
          |  Manager:
                OS2.WinSendDlgItemMsg (hwnd, DID.Manager, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(1), NIL);
          |  UserTemplate:
                OS2.WinSendDlgItemMsg (hwnd, DID.IsTemplate, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(1), NIL);
                passstring := "";
        END (*CASE*);

        subwindow1 := passwindow;
        subwindow2 := OS2.WinWindowFromID(hwnd, DID.EncryptPassword);
        IF (User.category = NoPasswordNeeded)
                       OR (User.category = GuestUser)
                       OR (User.category = UserTemplate) THEN
             OS2.WinEnableWindow (subwindow1, FALSE);
             OS2.WinShowWindow (subwindow2, FALSE);
        ELSE
             OS2.WinEnableWindow (subwindow1, TRUE);
             OS2.WinShowWindow (subwindow2, TRUE);
        END (*IF*);

        OS2.WinSetWindowText (passwindow, passstring);
        OS2.WinSetDlgItemText (hwnd, DID.RealName, User.RealName);
        OS2.WinSetDlgItemText (hwnd, DID.UserNotes, User.Notes);

        OS2.WinSendDlgItemMsg (hwnd, DID.UseTemplate, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(ORD(User.UseTemplate)), NIL);
        OS2.WinSetDlgItemText (hwnd, DID.TemplateName, User.TemplateName);

        EnableUseTemplateFields (hwnd, User.UseTemplate);
        EnableIsTemplateFields (hwnd, User.category = UserTemplate);
        IF clone THEN
            Strings.Append ("_1", User.name);
            OS2.WinSetDlgItemText (hwnd, DID.UserName, User.name);
        END (*IF*);

    END LoadDialogueData;

(************************************************************************)
(*                           SAVING THE DATA                            *)
(************************************************************************)

PROCEDURE SaveUserData (hwnd: OS2.HWND);

    (* Copies dialogue data to the User record, but does not yet write  *)
    (* it back to the INI file.                                         *)

    VAR val: CARDINAL;

    BEGIN
        OS2.WinQueryDlgItemText (hwnd, DID.UserName, SIZE(NameString), User.name);

        (* Remove leading spaces from username, and make it lowercase. *)

        WHILE User.name[0] = ' ' DO
            Strings.Delete (User.name, 0, 1);
        END (*WHILE*);
        ToLower (User.name);

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.NoPassword,
                                   OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            User.category := NoPasswordNeeded;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.Guest,
                                   OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            User.category := GuestUser;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.User,
                                   OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            User.category := NormalUser;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.Manager,
                                   OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            User.category := Manager;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.IsTemplate,
                                   OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            User.category := UserTemplate;
        END (*IF*);


        User.Active := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.Inactive,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) = 0;
        User.SuppressLog := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.SuppressLog,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        User.UseTemplate := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.UseTemplate,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;

        IF (User.category = NormalUser) OR (User.category = Manager) THEN
            User.EncryptPassword := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.EncryptPassword,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
            OS2.WinQueryWindowText (passwindow, PasswordSize+1, User.Password);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.RealName, RealNameSize, User.RealName);
        OS2.WinQueryDlgItemText (hwnd, DID.UserNotes, NoteFieldSize, User.Notes);
        OS2.WinQueryDlgItemText (hwnd, DID.TemplateName, NameLength, User.TemplateName);
        WinQueryDlgItemCard (hwnd, DID.LoginLimit, User.LoginLimit);
        WinQueryDlgItemCard (hwnd, DID.UserLimit, User.limit);
        WinQueryDlgItemCard (hwnd, DID.SpeedLimit, User.speedlimit);
        WinQueryDlgItemCard (hwnd, DID.UpSpeedLimit, User.upspeedlimit);
        WinQueryDlgItemCard (hwnd, DID.UpSizeLimit, val);
        IF (val = 0) OR (val = MAX(CARDINAL)) THEN
            User.upsizelimit := Max64;
        ELSE
            (* Multiply val by 2^20 to get User.upsizelimit. *)
            User.upsizelimit.high := val / 4096;
            User.upsizelimit.low := (val MOD 4096) * MEG;
        END (*IF*);
        User.PasswordChanged := (User.EncryptPassword <> User.OriginalEncryptPassword)
                                OR NOT Strings.Equal (User.Password, User.OriginalPassword);

    END SaveUserData;

(**************************************************************************)

PROCEDURE CommitChanges;

    (* Copies dialogue data back to the INI file. *)

    VAR ctx: MD5_CTX;  digest: MD5_DigestType;
        buffer: PassString;

    BEGIN
        OpenINIForUser (User.name);
        IF User.NewUser THEN
            buffer := "never";
            INIPutString (User.name, "LastLogin", buffer);
        END (*IF*);
        INIPut (User.name, "Category", User.category);
        INIPut (User.name, 'Active', User.Active);
        INIPut (User.name, 'DeleteExpired', User.DeleteExpired);
        INIPut (User.name, 'LoginLimit', User.LoginLimit);
        INIPut (User.name, 'SuppressLog', User.SuppressLog);
        INIPut (User.name, 'UseTemplate', User.UseTemplate);
        INIPut (User.name, 'EncryptPassword', User.EncryptPassword);
        IF User.PasswordChanged THEN
            buffer := User.Password;
            IF User.EncryptPassword THEN
                ctx := MD5Init();
                MD5Update (ctx, buffer, LENGTH(buffer));
                MD5Final (ctx, digest);
                MD5DigestToString (digest, buffer);
            END (*IF*);
            INIPutString (User.name, "Password", buffer);
            User.PasswordChanged := FALSE;
        END (*IF*);
        INIPutString (User.name, "RealName", User.RealName);
        INIPutString (User.name, "Notes", User.Notes);
        INIPutString (User.name, "TemplateName", User.TemplateName);
        INIPut (User.name, "UserLimit", User.limit);
        INIPut (User.name, "SpeedLimit", User.speedlimit);
        INIPut (User.name, "UpSpeedLimit", User.upspeedlimit);
        INIPut (User.name, "UpSizeLimit", User.upsizelimit);
        IF User.UseTemplate THEN
            Tree.SavePermissions (User.name, NIL);
        ELSE
            Tree.SavePermissions (User.name, User.UserTreeRoot);
        END (*IF*);
        IPFilters.StoreIPFilterList (User.name, User.UIPList);
        HideList.StoreList (User.name, User.HideList);
        CloseINIFile;
    END CommitChanges;

(************************************************************************)

PROCEDURE UserExists (name: ARRAY OF CHAR): BOOLEAN;

    VAR size: CARDINAL;  result: BOOLEAN;

    BEGIN
        ToLower (name);
        OpenINIForUser (name);
        result := ItemSize (name, "", size) AND (size > 0);
        CloseINIFile;
        RETURN result;
    END UserExists;

(************************************************************************)
(*                       THE USER DATA DIALOGUE                         *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    TYPE CharSet = SET OF CHAR;

    CONST BoxID = 999;
        Digits = CharSet {'0'..'9'};

    VAR NotificationCode, ButtonID, val: CARDINAL;
        colour: SYSTEM.CARD8;
        PasswordWasDisabled: BOOLEAN;
        encryptwindow, w: OS2.HWND;
        Message: ARRAY [0..255] OF CHAR;
        ButtonValue: BOOLEAN;

    BEGIN
        encryptwindow := OS2.WinWindowFromID(hwnd, DID.EncryptPassword);
        CASE msg OF

              OS2.WM_INITDLG:

                   (* Store the positions of the four moveable subwindows. *)

                   w := OS2.WinWindowFromID (hwnd, DID.LoginLimit);
                   OS2.WinQueryWindowPos (w, swp[0]);
                   w := OS2.WinWindowFromID (hwnd, DID.TextLogins);
                   OS2.WinQueryWindowPos (w, swp[1]);
                   w := OS2.WinWindowFromID (hwnd, DID.LoginLimit2);
                   OS2.WinQueryWindowPos (w, swp[2]);
                   w := OS2.WinWindowFromID (hwnd, DID.TextLogins2);
                   OS2.WinQueryWindowPos (w, swp[3]);

                   RETURN NIL;

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.EditUIPListButton:

                       UIPControls.Edit (hwnd, User.UIPList);
                       IF (User.UIPList = NIL) OR
                              ((User.UIPList^.next = NIL) AND User.UIPList^.allow) THEN
                           OS2.WinSetDlgItemText (hwnd, DID.EditUIPListButton, "Add IP address controls");
                       ELSE
                           OS2.WinSetDlgItemText (hwnd, DID.EditUIPListButton, "Modify IP address controls");
                       END (*IF*);
                       RETURN NIL;

                     | DID.EditHideListButton:

                       HideList.Edit (hwnd, User.HideList);
                       IF (User.HideList = NIL) OR (User.HideList^.next = NIL) THEN
                           OS2.WinSetDlgItemText (hwnd, DID.EditHideListButton, "Add HideList");
                       ELSE
                           OS2.WinSetDlgItemText (hwnd, DID.EditHideListButton, "Modify HideList");
                       END (*IF*);
                       RETURN NIL;

                     | DID.EditDirButton:

                       Tree.Edit (hwnd, User.name, User.UserTreeRoot);
                       Tree.InitialDisplay (OS2.WinWindowFromID(hwnd, DID.DirSummary),
                                              User.UserTreeRoot);
                       RETURN NIL;

                     | DID.UserOK:

                       SaveUserData (hwnd);
                       User.NameChanged := NOT Strings.Equal (User.name, User.OldName);
                       IF User.name[0] = Nul THEN
                           Message := "You must have a non-blank user name";
                           OS2.WinMessageBox (OS2.HWND_DESKTOP, hwnd, Message, NIL,
                                      BoxID, OS2.MB_OK + OS2.MB_ERROR);
                           OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.UserName));
                           RETURN NIL;
                       ELSIF NOT User.NameChanged THEN
                           CommitChanges;
                           OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                           RETURN NIL;
                       END (*IF*);

                       (* If we reach this point then the username has changed.  We have   *)
                       (* to check for duplication of an existing name, and also check     *)
                       (* whether data for the old name should be deleted.                 *)

                       IF UserExists (User.name) THEN

                           Message := "You have duplicated the name of an existing user";
                           OS2.WinMessageBox (OS2.HWND_DESKTOP, hwnd, Message, NIL,
                                      BoxID, OS2.MB_OK + OS2.MB_ERROR);
                           OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.UserName));
                           RETURN NIL;

                       ELSIF User.OldName[0] <> Nul THEN

                           Message := "This will cause username '";
                           Strings.Append (User.OldName, Message);
                           Strings.Append ("' to be deleted", Message);
                           IF OS2.WinMessageBox (OS2.HWND_DESKTOP, hwnd, Message, "Warning",
                                      BoxID, OS2.MB_OKCANCEL + OS2.MB_INFORMATION) = OS2.MBID_OK THEN
                               OpenINIForUser (User.OldName);
                               INIDeleteApp (User.OldName);
                               CloseINIFile;
                           ELSE
                               RETURN NIL;
                           END (*IF*);

                       END (*IF*);

                       CommitChanges;
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;

                     | DID.UserCancel:
                       User.NameChanged := FALSE;
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

          |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   ButtonValue := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, ButtonID,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;

                   IF (ButtonID = DID.UseTemplate)
                                AND (ButtonValue <> User.UseTemplate) THEN

                       EnableUseTemplateFields (hwnd, ButtonValue);
                       RETURN NIL;

                   ELSIF ButtonID = DID.LimitLogins THEN
                       SetLoginLimit (hwnd, ORD(ButtonValue));

                   ELSIF ButtonID = DID.limit_delete THEN
                       User.DeleteExpired := TRUE;
                       SetLoginLimit (hwnd, User.LoginLimit);

                   ELSIF ButtonID = DID.limit_deactivate THEN
                       User.DeleteExpired := FALSE;
                       SetLoginLimit (hwnd, User.LoginLimit);

                   ELSIF (ButtonID = DID.LoginLimit) AND DetectLLChanges
                                    AND (NotificationCode = OS2.EN_CHANGE) THEN

                       (* Precaution: if the field doesn't start with a digit,  *)
                       (* don't change the login limit just yet.                *)

                       OS2.WinQueryDlgItemText (hwnd, DID.LoginLimit, 8, Message);
                       IF Message[0] IN Digits THEN
                           WinQueryDlgItemCard (hwnd, DID.LoginLimit, val);
                           SetLoginLimit (hwnd, val);
                       END (*IF*);

                   ELSIF ButtonID = DID.Inactive THEN
                       IF ButtonValue THEN
                           colour := 2;
                       ELSE
                           colour := 9;
                       END (*IF*);
                       OS2.WinSetPresParam(
                                 OS2.WinWindowFromID(hwnd, DID.Inactive),
                                 OS2.PP_FOREGROUNDCOLORINDEX,
                                 SIZE(colour), SYSTEM.ADR(colour));
                       RETURN NIL;

                   ELSIF (NotificationCode = OS2.BN_CLICKED) AND ButtonValue THEN

                       PasswordWasDisabled := (User.category = NoPasswordNeeded)
                                              OR (User.category = GuestUser)
                                              OR (User.category = UserTemplate);
                       EnableIsTemplateFields (hwnd, ButtonID = DID.IsTemplate);
                       CASE ButtonID OF

                         | DID.NoPassword:
                           User.category := NoPasswordNeeded;
                           IF NOT PasswordWasDisabled THEN
                               OS2.WinQueryWindowText (passwindow, PasswordSize+1, User.Password);
                           END (*IF*);
                           OS2.WinSetWindowText (passwindow, "");
                           OS2.WinEnableWindow (passwindow, FALSE);
                           OS2.WinShowWindow (encryptwindow, FALSE);
                           RETURN NIL;

                         | DID.Guest:
                           User.category := GuestUser;
                           IF NOT PasswordWasDisabled THEN
                               OS2.WinQueryWindowText (passwindow, PasswordSize+1, User.Password);
                           END (*IF*);
                           OS2.WinSetWindowText (passwindow, "@");
                           OS2.WinEnableWindow (passwindow, FALSE);
                           OS2.WinShowWindow (encryptwindow, FALSE);
                           RETURN NIL;

                         | DID.User:
                           User.category := NormalUser;
                           IF PasswordWasDisabled THEN
                               OS2.WinEnableWindow (passwindow, TRUE);
                               OS2.WinShowWindow (encryptwindow, TRUE);
                               OS2.WinSetWindowText (passwindow, User.Password);
                           END (*IF*);
                           RETURN NIL;

                         | DID.Manager:
                           User.category := Manager;
                           IF PasswordWasDisabled THEN
                               OS2.WinEnableWindow (passwindow, TRUE);
                               OS2.WinShowWindow (encryptwindow, TRUE);
                               OS2.WinSetWindowText (passwindow, User.Password);
                           END (*IF*);
                           RETURN NIL;

                         | DID.IsTemplate:
                           User.category := UserTemplate;
                           IF NOT PasswordWasDisabled THEN
                               OS2.WinQueryWindowText (passwindow, PasswordSize+1, User.Password);
                           END (*IF*);
                           OS2.WinSetWindowText (passwindow, "");
                           OS2.WinEnableWindow (passwindow, FALSE);
                           OS2.WinShowWindow (encryptwindow, FALSE);
                           RETURN NIL;

                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*CASE*);

                   ELSIF (ButtonID = DID.TemplateName)
                               AND (NotificationCode = OS2.EN_CHANGE) THEN

                       (* The template name has changed. *)

                       OS2.WinQueryDlgItemText (hwnd, DID.TemplateName, NameLength, User.TemplateName);
                       Tree.DeleteTree (User.TemplateTreeRoot);
                       IF User.TemplateName[0] <> Nul THEN
                           OpenINIForUser (User.TemplateName);
                           User.TemplateTreeRoot := Tree.LoadPermissions (User.TemplateName);
                           CloseINIFile;
                       END (*IF*);
                       Tree.InitialDisplay (OS2.WinWindowFromID(hwnd, DID.DirSummary),
                                            User.TemplateTreeRoot);
                       RETURN NIL;

                   ELSIF (ButtonID = DID.DirSummary) AND NOT User.UseTemplate
                               AND (NotificationCode = OS2.LN_ENTER) THEN
                       (* Treat this one as if the "edit directory" button had been clicked. *)
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                             OS2.MPFROMSHORT(DID.EditDirButton), NIL);
                       RETURN NIL;

                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_CLOSE:
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  VAR (*INOUT*) name: ARRAY OF CHAR;
                                                clone: BOOLEAN): BOOLEAN;

    (* Edit the properties of the alias called "name".  *)
    (* Returns TRUE iff the name has changed.           *)

    VAR hwnd: OS2.HWND;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        DetectLLChanges := FALSE;
        IF clone THEN
            User.OldName := "";
        ELSE
            Strings.Assign (name, User.OldName);
        END (*IF*);
        Strings.Assign (name, User.name);
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.EditUserDialogue,                (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (hwnd, "EditUser");

        LoadDialogueData (hwnd, name, clone);
        OS2.WinSetFocus (OS2.HWND_DESKTOP, OS2.WinWindowFromID(hwnd, DID.UserName));

        OS2.WinProcessDlg(hwnd);

        Strings.Assign (User.name, name);
        Remote.StoreWindowPosition (hwnd, "EditUser", TRUE);
        OS2.WinDestroyWindow (hwnd);
        Tree.DeleteTree (User.UserTreeRoot);
        Tree.DeleteTree (User.TemplateTreeRoot);
        RETURN User.NameChanged;

    END Edit;

(************************************************************************)

END EditUser.

