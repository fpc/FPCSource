(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: AppLaunchCmd.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Pilot launch commands for applications.  Some launch commands
 * are treated differently by different apps.  The different
 * parameter blocks used by the apps are kept here.
 *
 * History:
 *    7/23/96 rsf - Created by Roger Flores
 *    7/28/98 dia - Added generic LaunchWithCommand.  Made
 *                  AppLaunchWithCommand() use it.
 *
 *****************************************************************************)

unit applaunchcmd;

interface

uses  palmos, systemmgr;

(*
#define LaunchWithCommand(type, creator, command, commandParams) \
{ \
   UInt16            cardNo; \
   LocalID           dbID; \
   DmSearchStateType searchState; \
   Err               err; \
   DmGetNextDatabaseByTypeCreator(true, &searchState, type, \
      creator, true, &cardNo, &dbID); \
   ErrNonFatalDisplayIf(!dbID, "Could not find app"); \
   if (dbID) { \
      err = SysUIAppSwitch(cardNo, dbID, command, commandParams); \
      ErrNonFatalDisplayIf(err, "Could not launch app"); \
      } \
   }

#define AppLaunchWithCommand(appCreator, appCommand, appCommandParams) \
   LaunchWithCommand (sysFileTApplication, appCreator, appCommand, appCommandParams)

#define AppCallWithCommand(appCreator, appCommand, appCommandParams) \
{ \
   UInt16            cardNo; \
   LocalID           dbID; \
   DmSearchStateType searchState; \
   UInt32            result; \
   Err               err; \
   DmGetNextDatabaseByTypeCreator(true, &searchState, sysFileTApplication, \
      appCreator, true, &cardNo, &dbID); \
   ErrNonFatalDisplayIf(!dbID, "Could not find app"); \
   if (dbID) { \
      err = SysAppLaunch(cardNo, dbID, 0, appCommand, (MemPtr) appCommandParams, &result); \
      ErrNonFatalDisplayIf(err, "Could not launch app"); \
      } \
   }
*)

(************************************************************
 * Param Block passsed with the sysAppLaunchCmdLookup Command
 *************************************************************)

//-------------------------------------------------------------------
// sysAppLaunchCmdLookup parameter block for the Address Book
//-------------------------------------------------------------------

// This is a list of fields by which data may be looked up.
type
  AddressLookupFields = Enum;

const
  addrLookupName = 0;
  addrLookupFirstName = Succ(addrLookupName);
  addrLookupCompany = Succ(addrLookupFirstName);
  addrLookupAddress = Succ(addrLookupCompany);
  addrLookupCity = Succ(addrLookupAddress);
  addrLookupState = Succ(addrLookupCity);
  addrLookupZipCode = Succ(addrLookupState);
  addrLookupCountry = Succ(addrLookupZipCode);
  addrLookupTitle = Succ(addrLookupCountry);
  addrLookupCustom1 = Succ(addrLookupTitle);
  addrLookupCustom2 = Succ(addrLookupCustom1);
  addrLookupCustom3 = Succ(addrLookupCustom2);
  addrLookupCustom4 = Succ(addrLookupCustom3);
  addrLookupNote = Succ(addrLookupCustom4);         // This field is assumed to be < 4K
  addrLookupWork = Succ(addrLookupNote);
  addrLookupHome = Succ(addrLookupWork);
  addrLookupFax = Succ(addrLookupHome);
  addrLookupOther = Succ(addrLookupFax);
  addrLookupEmail = Succ(addrLookupOther);
  addrLookupMain = Succ(addrLookupEmail);
  addrLookupPager = Succ(addrLookupMain);
  addrLookupMobile = Succ(addrLookupPager);
  addrLookupSortField = Succ(addrLookupMobile);
  addrLookupListPhone = Succ(addrLookupSortField);
  addrLookupFieldCount = Succ(addrLookupListPhone); // add new fields above this one

  addrLookupNoField = $ff;

const
  addrLookupStringLength = 12;

type
  AddrLookupParamsType = record
    title: PChar;
      // Title to appear in the title bar.  If NULL the default is used.

    pasteButtonText: PChar;
      // Text to appear in paste button.  If NULL "paste" is used.

    lookupString: array [0..addrLookupStringLength-1] of Char;
      // Buffer containing string to lookup.  If the string matches
      // only one record then that record is used without
      // presenting the user with the lookup dialog.

    field1: AddressLookupFields;
      // Field to search by.  This field appears on the left side
      // of the lookup dialog.  If the field is the sort field then
      // searches use a binary search.  If the field isn't the sort
      // field then the data does appear in sorted order and searching
      // is performed by a linear search (can get slow).

    field2: AddressLookupFields;
      // Field to display on the right.  Often displays some
      // information about the person.  If it is a phone field
      // and a record has multiple instances of the phone type
      // then the person appears once per instance of the phone
      // type. Either field1 or field2 may be a phone field but
      // not both.

    field2Optional: Boolean;
      // True means that the record need not have field2 for
      // the record to be listed.  False means that field2 is
      // required in the record for it to be listed.

    userShouldInteract: Boolean;
      // True means that the user should resolve non unique
      // lookups.  False means a non unique and complete lookup
      // returns resultStringH set to 0 and recordID set to 0;

    formatStringP: PChar;
      // When the user selects the paste button a string is generated
      // to return data from the record.  The format of the result string
      // is controlled by this string.  All characters which appear
      // in this string are copied straight to the result string unless
      // they are a field (a '^' follow by the field name).  For
      // example, the format string "^first - ^home" might result in
      // "Roger - 123-4567".

      // The field arguments are name, first, company, address, city
      // state, zipcode, country, title, custom1, custom2, custom3,
      // custom4, work, home, fax, other, email, main, pager, mobile,
      // and listname.

    resultStringH: MemHandle;
      // If there is a format string a result string is allocated on
      // the dynamic heap and its handle is returned here.

    uniqueID: UInt32;
      // The unique ID of the found record or 0 if none was found.
   end;

type
  AddrLookupParamsPtr = ^AddrLookupParamsType;

(************************************************************
 * Param Block passsed with the sysAppLaunchCmdSetActivePanel Command
 *************************************************************)

const
  prefAppLaunchCmdSetActivePanel = sysAppLaunchCmdCustomBase + 1;
                                                // Record this panel so switching to the Prefs app
                                                // causes this panel to execute.

type
  PrefActivePanelParamsType = record
    activePanel: UInt32;
      // The creator ID of a panel.  Usually sent by a panel so the prefs
      // apps will switch to it.  This allows the last used panel to appear
      // when switching to the Prefs app.
  end;

  PrefActivePanelParamsPtr = ^PrefActivePanelParamsType;

(************************************************************
 * Param Block passsed with the sysAppLaunchCmdAddRecord Command
 *************************************************************)

//-------------------------------------------------------------------
// sysAppLaunchCmdAddRecord parameter block for the Mail application
//-------------------------------------------------------------------
// Param Block passsed with the sysAppLaunchCmdAddRecord Command

type
  MailMsgPriorityType = Enum;

const
  mailPriorityHigh = 0;
  mailPriorityNormal = Succ(mailPriorityHigh);
  mailPriorityLow = Succ(mailPriorityNormal);

type
  MailAddRecordParamsType = record
    secret: Boolean;
      // True means that the message should be marked secret

    signature: Boolean;
      // True means that signature from the Mail application's preferences
      // should be attached to the message.

    confirmRead: Boolean;
      // True means that a comfirmation should be sent when the message
      // is read.

    confirmDelivery: Boolean;
      // True means that a comfirmation should be sent when the message
      // is deliveried

    priority: MailMsgPriorityType;
      // high, normial, or low.

    padding: UInt8;

    subject: PChar;
      // Message's subject, a null-terminated string (optional).

    from: PChar;
      // Message's send, a null-terminated string (not currently used).

    to_: PChar;
      // Address the the recipient, a null-terminated string (required).

    cc: PChar;
      // Copy Addresses, a null-terminated string (required).

    bcc: PChar;
      // Blind copy Addresses, a null-terminated string (required).

    replyTo: PChar;
      // Reply to address, a null-terminated string (required).

    body: PChar;
      // The text of the message, a null-terminated string (required).
  end;

  MailAddRecordParamsPtr = ^MailAddRecordParamsType;

//-------------------------------------------------------------------
// sysAppLaunchCmdAddRecord parameter block for the Messaging application
//-------------------------------------------------------------------
// Param Block passsed with the sysAppLaunchCmdAddRecord Command

//category defines
const
  MsgInboxCategory   = 0;
  MsgOutboxCategory  = 1;
  MsgDeletedCategory = 2;
  MsgFiledCategory   = 3;
  MsgDraftCategory   = 4;

type
  MsgAddRecordParamsType = record
    category: UInt16;
      //is this an outgoing mesage? Or should it be put into a different category

    edit: Boolean;
      // True means that the message should be opened in the editor,instead of
      // just dropped into the category (only applies to outBox category)

    signature: Boolean;
      // True means that signature from the Mail application's preferences
      // should be attached to the message.

    subject: PChar;
      // Message's subject, a null-terminated string (optional).

    from: PChar;
      // Message's send, a null-terminated string (not currently used).

    to_: PChar;
      // Address the the recipient, a null-terminated string (required).

    replyTo: PChar;
      // Reply to address, a null-terminated string (required).

    body: PChar;
      // The text of the message, a null-terminated string (required).
  end;

  MsgAddRecordParamsPtr = ^MsgAddRecordParamsType;

implementation

end.
