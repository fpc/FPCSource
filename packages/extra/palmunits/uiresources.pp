{$MACRO ON}
{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: UIResources.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines UI resource types & ids.
 *
 * History:
 *    ??/??/?? ???   Created.
 *    06/29/99 CS    Added constantRscType & ResLoadConstant().
 *    07/07/99 kwk   Added fepFieldExtraBytesID, maxCategoryWidthID,
 *                   extraStackSpaceID.
 *    07/09/99 kwk   Added silkscreenRscType & formRscType.
 *    07/12/99 kwk   Added sysFatalAlert.
 *    07/18/99 kwk   Added strListRscType, system string list resources.
 *    08/08/99 kwk   Added sysEditMenuJapAddWord/LookupWord.
 *    09/07/99 kwk   Added StrippedBase/GenericLaunchErrAlert
 *    09/17/99 jmp   Added a new NoteView form and menu to eliminate the goto
 *                   top/bottom menu items and other extraneous UI elements
 *                   that we no longer use in the built-in apps. We need to keep
 *                   the old NoteView form and menu around for backwards
 *                   compatibility.
 *    12/10/99 kwk   Deleted silkscreenRscType, use sysResTSilkscreen instead.
 *    07/06/00 kwk   Deleted fepFieldExtraBytesID and extraStackSpaceID, since
 *                   these are now features.
 *    07/12/00 gap   Remove unused MenuCtlRsc definition (tcbr).
 *    07/12/00 gap   Remove unused MenuCtlRsc definition (tcbr).
 *    09/04/00 ppl   Add constant for Current and default profiel name for ConnectionMgr
 *    09/07/00 kwk   Deleted daysOfWeekStrID, dayFullNamesStrID, monthNamesStrID,
 *                   and monthFullNamesStrID, since these are no longer in the
 *                   ROM as of 4.0 (tSTR=10000...10003).
 *
 *****************************************************************************)

unit uiresources;

interface

uses palmos, coretraps;

// System Default app icon (for apps missing a tAIB)
const
  defaultAppIconBitmap                        = 10000;
  defaultAppSmallIconBitmap                   = 10001;

//  System version string ID - this is hidden in
//  the SystemVersion.rsrc resource, because the 'system' resources
//  don't have ResEdit formats.
  systemVersionID                             = 10000;

//------------------------------------------------------------
// Resource Type Constants
//------------------------------------------------------------

  strRsc                 = Rsc('tSTR');
  ainRsc                 = Rsc('tAIN');
  iconType               = Rsc('tAIB');
  bitmapRsc              = Rsc('Tbmp');
  bsBitmapRsc            = Rsc('Tbsb');
  alertRscType           = Rsc('Talt');
  kbdRscType             = Rsc('tkbd');
  MenuRscType            = Rsc('MBAR');
  fontRscType            = Rsc('NFNT');
  verRsc                 = Rsc('tver');
  appInfoStringsRsc      = Rsc('tAIS');
  fontIndexType          = Rsc('fnti');
  midiRsc                = Rsc('MIDI');
  colorTableRsc          = Rsc('tclt');

  constantRscType        = Rsc('tint');
  formRscType            = Rsc('tFRM');

  strListRscType         = Rsc('tSTL');
  wrdListRscType         = Rsc('wrdl');
  defaultCategoryRscType = Rsc('taic');
  binaryGeneralRscType   = Rsc('tbin');

//------------------------------------------------------------
// App Version Constants
//------------------------------------------------------------

  appVersionID                                = 1; // our apps use tver 1 resource
  appVersionAlternateID                       = 1000; // CW Constructor uses tver 1000 resource
                                                     // so we'll look for ours first, then try theirs
  ainID                                       = 1000;

  oemVersionID                                = 10001; // Per-DB version provided by OEMs

//------------------------------------------------------------
// System Information Constants
//------------------------------------------------------------

  maxCategoryWidthID                          = 10001; // Max pixel width for category trigger.

//------------------------------------------------------------
// System Alerts
//------------------------------------------------------------

  SelectACategoryAlert                        = 10000;

// This alert broke 1.0 applications and is now disabled until later.
// It is redefined below (10015).
//  RemoveCategoryAlert         = 10001;
//  RemoveCategoryRecordsButton = 0;
//  RemoveCategoryNameButton    = 1;
//  RemoveCategoryCancelButton  = 2;

  LowBatteryAlert                             = 10002;
  VeryLowBatteryAlert                         = 10003;
  UndoAlert                                   = 10004;
  UndoCancelButton                            = 1;

  MergeCategoryAlert                          = 10005;
  MergeCategoryYes                            = 0;
  MergeCategoryNo                             = 1;

  privateRecordInfoAlert                      = 10006;

  ClipboardLimitAlert                         = 10007;

  CategoryExistsAlert                         = 10012;

  DeviceFullAlert                             = 10013;

  categoryAllUsedAlert                        = 10014;

  RemoveCategoryAlert                         = 10015; // See alert 10001
  RemoveCategoryYes                           = 0;
  RemoveCategoryNo                            = 1;

  DemoUnitAlert                               = 10016;

// The "no data to send" message is a shared error message that is displayed
// when no data is selected when a beam or send command is issued.
  NoDataToBeamAlert                           = 10017;
  NoDataToSendAlert                           = 10017;

// New for PalmOS 3.1
  LowCradleChargedBatteryAlert                = 10018; // (Not present in Palm VII)
  VeryLowCradleChargedBatteryAlert            = 10019; // (Not present in Palm VII)

// New for PalmOS 3.1 (Instant Karma only)
  CategoryTooLongAlert                        = 10020; // (Not present in Palm VII)

// New for PalmOS 3.2 - Alerts used by the ErrAlertCustom()  call.
  ErrOKAlert                                  = 10021; // Error Alert with just an OK button
  ErrOKCancelAlert                            = 10022; // Error Alert with an OK & Cancel button
  ErrCancelAlert                              = 10023; // Error Alert with just Cancel button.  Special case for antenna down alert.
  InfoOKAlert                                 = 10024; // Info alert with just an OK button
  InfoOKCancelAlert                           = 10025; // Info alert with an OK & Cancel button
  InfoCancelAlert                             = 10026; // Info alert with just a Cancel button
  PrivacyWarningAlert                         = 10027; // Privacy warning for weblib
  ConfirmationOKAlert                         = 10028; // Confirmation alert with just an OK button
  ConfirmationOKCancelAlert                   = 10029; // Confirmation alert with an OK & Cancel button
  ConfirmationCancelAlert                     = 10030; // Confirmation alert with just a Cancel button
  WarningOKAlert                              = 10031; // Warning Alert with just an OK button
  WarningOKCancelAlert                        = 10032; // Warning Alert with an OK & Cancel button
  WarningCancelAlert                          = 10033; // Warning Alert with just Cancel button.  Special case for antenna down alert.

// New for PalmOS 3.5 - Launch error alerts
  StrippedBaseLaunchErrAlert                  = 10034; // Launch error because of stripped base.
  GenericLaunchErrAlert                       = 10035; // Generic launch error.

// New for PalmOS 3.5 - Fatal Alert template
  sysFatalAlert_                              = 10100; // Template for fatal alert

// New for PalmOS 3.5 - Alerts used by new security traps
  secInvalidPasswordAlert                     = 13250;
  secGotoInvalidRecordAlert                   = 13251;
  secShowPrivatePermanentPassEntryAlert       = 13261;
  secShowMaskedPrivatePermanentPassEntryAlert = 13265;
  secHideRecordsAlert                         = 13268;
  secMaskRecordsAlert                         = 13269;
  secHideMaskRecordsOK                        = 0;
  secHideMaskRecordsCancel                    = 1;

// New for PalmOS 4.0 -  General purpose password prompt alert
  secEnterPasswordAlert                       = 13300;
  secEnterPasswordOK                          = 0;
  secEnterPasswordCancel                      = 1;

// command-bar bitmaps
  BarCutBitmap                                = 10030;
  BarCopyBitmap                               = 10031;
  BarPasteBitmap                              = 10032;
  BarUndoBitmap                               = 10033;
  BarBeamBitmap                               = 10034;
  BarSecureBitmap                             = 10035;
  BarDeleteBitmap                             = 10036;
  BarInfoBitmap                               = 10037;

//Masking bitmaps
  SecLockBitmap                               = 10050;
  SecLockWidth                                = 6;
  SecLockHeight                               = 8;

// System Menu Bar and Menus
  sysEditMenuID                               = 10000;
  sysEditMenuUndoCmd                          = 10000;
  sysEditMenuCutCmd                           = 10001;
  sysEditMenuCopyCmd                          = 10002;
  sysEditMenuPasteCmd                         = 10003;
  sysEditMenuSelectAllCmd                     = 10004;
  sysEditMenuSeparator                        = 10005;
  sysEditMenuKeyboardCmd                      = 10006;
  sysEditMenuGraffitiCmd                      = 10007;

  sysNetworkProgress01Bitmap                  = 10020;
  sysNetworkProgress02Bitmap                  = 10021;
  sysNetworkProgress03Bitmap                  = 10022;
  sysNetworkProgress04Bitmap                  = 10023;
  sysNetworkProgress05Bitmap                  = 10024;
  sysNetworkProgress06Bitmap                  = 10025;

// Dynamically added to System Edit menu at runtime
  sysEditMenuJapAddWord                       = 10100;
  sysEditMenuJapLookupWord                    = 10101;

// Note View Menu Bar and Menus
  noteMenuID                                  = 10200; // Old NoteView MenuBar
  noteUndoCmd                                 = sysEditMenuUndoCmd;
  noteCutCmd                                  = sysEditMenuCutCmd;
  noteCopyCmd                                 = sysEditMenuCopyCmd;
  notePasteCmd                                = sysEditMenuPasteCmd;
  noteSelectAllCmd                            = sysEditMenuSelectAllCmd;
  noteSeparator                               = sysEditMenuSeparator;
  noteKeyboardCmd                             = sysEditMenuKeyboardCmd;
  noteGraffitiCmd                             = sysEditMenuKeyboardCmd;

  noteFontCmd                                 = 10200; // These are here for backwards
  noteTopOfPageCmd                            = 10201; // compatibility.  The built-in
  noteBottomOfPageCmd                         = 10202; // apps no longer use them.
  notePhoneLookupCmd                          = 10203;

  newNoteMenuID                               = 10300; // The Edit Menu for the new NoteView.
  newNoteFontCmd                              = 10300; // MenuBar is the same as it is for
  newNotePhoneLookupCmd                       = 10301; // the old NoteView MenuBar.

// Note View (used by Datebook, To Do, Address, and Expense apps)
  NoteView                                    = 10900; // The new NoteView is "new" as of Palm OS 3.5.
  NewNoteView                                 = 10950; // Same as old NoteView, but points to newNoteMenuID and doesn't ref UI objects listed below.
  NoteField                                   = 10901;
  NoteDoneButton                              = 10902;
  NoteSmallFontButton                         = 10903; // Not in NewNoteView, use FontCmd instead.
  NoteLargeFontButton                         = 10904; // Not in NewNoteView, use FontCmd instead.
  NoteDeleteButton                            = 10905;
  NoteUpButton                                = 10906; // Not in NewNoteView, use scrollbars now.
  NoteDownButton                              = 10907; // Not in NewNoteView, use scrollbars now.
  NoteScrollBar                               = 10908;
  NoteFontGroup                               = 1;
  noteViewMaxLength                           = 4096; // not including null, tied to tFLD rsrc 10901

//  About Box - used by Datebook, Memo, Address, To Do, & others
  aboutDialog                                 = 11000;
  aboutNameLabel                              = 11001;
  aboutVersionLabel                           = 11002;
  aboutErrorStr                               = 11003;

// Category New Name Dialog (used for new and renamed categories)
  categoryNewNameDialog                       = 11100;
  categoryNewNameField                        = 11103;
  categoryNewNameOKButton                     = 11104;

// Categories Edit Dialog
  CategoriesEditForm                          = 10000;
  CategoriesEditList                          = 10002;
  CategoriesEditOKButton                      = 10003;
  CategoriesEditNewButton                     = 10004;
  CategoriesEditRenameButton                  = 10005;
  CategoriesEditDeleteButton                  = 10006;

// Graffiti Reference Dialog
  graffitiReferenceDialog                     = 11200;
  graffitiReferenceDoneButton                 = 11202;
  graffitiReferenceUpButton                   = 11203;
  graffitiReferenceDownButton                 = 11204;
  graffitiReferenceFirstBitmap                = 11205;

// System string resources
  categoryAllStrID                            = 10004;
  categoryEditStrID                           = 10005;
  menuCommandStrID                            = 10006;
  launcherBatteryStrID                        = 10007;
  systemNameStrID                             = 10008;
  phoneLookupTitleStrID                       = 10009;
  phoneLookupAddStrID                         = 10010;
  phoneLookupFormatStrID                      = 10011;

//------------------------------------------------------------
// Misc. resource routines
//------------------------------------------------------------

function ResLoadForm(rscID: UInt16): Pointer; syscall sysTrapResLoadForm;

function ResLoadMenu(rscID: UInt16): Pointer; syscall sysTrapResLoadMenu;

//!!!function ResLoadString(rscID: UInt16): PChar;

function ResLoadConstant(rscID: UInt16): UInt32; syscall sysTrapResLoadConstant;

implementation

end.
