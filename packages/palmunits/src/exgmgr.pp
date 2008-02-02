{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ErrorMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Error Management that depend on ERROR_CHECK_LEVEL
 *    All the rest of the old ErrorMgr.h is in ErrorBase.h
 *
 * History:
 *    10/25/94  RM - Created by Ron Marianetti
 *    10/9/98  Bob - Fill in all macros, fix defns w/ do{}while(0)
 *    7/21/99  Bob - split invariant stuff out into ErrorBase.h
 *    12/23/99 jmp   Fix <> vs. "" problem.
 *
 *****************************************************************************)

unit exgmgr;

interface

uses palmos, coretraps, errorbase, datamgr;

const
  exgMemError         = exgErrorClass or 1;
  exgErrStackInit     = exgErrorClass or 2;  // stack could not initialize
  exgErrUserCancel    = exgErrorClass or 3;
  exgErrNoReceiver    = exgErrorClass or 4;  // receiver device not found
  exgErrNoKnownTarget = exgErrorClass or 5;  // can't find a target app
  exgErrTargetMissing = exgErrorClass or 6;  // target app is known but missing
  exgErrNotAllowed    = exgErrorClass or 7;  // operation not allowed
  exgErrBadData       = exgErrorClass or 8;  // internal data was not valid
  exgErrAppError      = exgErrorClass or 9;  // generic application error
  exgErrUnknown       = exgErrorClass or 10; // unknown general error
  exgErrDeviceFull    = exgErrorClass or 11; // device is full
  exgErrDisconnected  = exgErrorClass or 12; // link disconnected
  exgErrNotFound      = exgErrorClass or 13; // requested object not found
  exgErrBadParam      = exgErrorClass or 14; // bad parameter to call
  exgErrNotSupported  = exgErrorClass or 15; // operation not supported by this library
  exgErrDeviceBusy    = exgErrorClass or 16; // device is busy
  exgErrBadLibrary    = exgErrorClass or 17; // bad or missing ExgLibrary

type
  ExgGoToType = record
    dbCardNo: UInt16;    // card number of the database
    dbID: LocalID;       // LocalID of the database
    recordNum: UInt16;   // index of record that contain a match
    uniqueID: UInt32;    // postion in record of the match.
    matchCustom: UInt32; // application specific info
  end;

  ExgGoToPtr = ^ExgGoToType;

  ExgSocketType = record
    libraryRef: UInt16;      // identifies the Exg library in use
    socketRef: UInt32;       // used by Exg library to identify this connection
    target: UInt32;          // Creator ID of application this is sent to
    count: UInt32;           // # of objects in this connection (usually 1)
    length: UInt32;          // # total byte count for all objects being sent (optional)
    time: UInt32;            // last modified time of object (optional)
    appData: UInt32;         // application specific info
    goToCreator: UInt32;     // creator ID of app to launch with goto after receive
    goToParams: ExgGoToType; // If launchCreator then this contains goto find info
    bits: UInt16;
{
    UInt16  localMode:1;     // Exchange with local machine only mode
    UInt16  packetMode:1;    // Use connectionless packet mode (Ultra)
    UInt16  noGoTo:1;        // Do not go to app (local mode only)
    UInt16  noStatus:1;      // Do not display status dialogs
    UInt16  reserved:12;     // reserved system flags
}
    description: PChar;      // text description of object (for user)
    type_: PChar;            // Mime type of object (optional)
    name: PChar;             // name of object, generally a file name (optional)
  end;

  ExgSocketPtr = ^ExgSocketType;

// structures used for sysAppLaunchCmdExgAskUser launch code parameter
// default is exgAskDialog (ask user with dialog...
type
  ExgAskResultType = Enum;

const
  exgAskDialog = 0;
  exgAskOk = Succ(exgAskDialog);
  exgAskCancel = Succ(exgAskOk);

type
  ExgAskParamType = record
    socketP: ExgSocketPtr;
    result: ExgAskResultType; // what to do with dialog
    reserved: UInt8;
  end;

  ExgAskParamPtr = ^ExgAskParamType;

// Optional parameter structure used with ExgDoDialog for category control
  ExgDialogInfoType = record
    version: UInt16;       // version of this structure (should be zero)
    db: DmOpenRef;         // open database ref (for category information)
    categoryIndex: UInt16; // index of selected category
  end;

const
  exgSeparatorChar    = #9;    // '\t' char used to separate multiple registry entries

  exgRegLibraryID     = $fffc; // library register thier presence
  exgRegExtensionID   = $fffd; // filename extenstion registry
  exgRegTypeID        = $fffe; // MIME type registry

  exgDataPrefVersion  = 0;
  exgMaxTitleLen      = 20;    // max size for title from exgLibCtlGetTitle

  exgLibCtlGetTitle   = 1;     // get title for Exg dialogs
  exgLibCtlSpecificOp = $8000; // start of range for library specific control codes

type
  ExgDBReadProc = function(dataP: Pointer; var sizeP: UInt32; userDataP: Pointer): Err;
  ExgDBDeleteProc = function(const nameP: PChar; version, cardNo: UInt16; dbID: LocalID; userDataP: Pointer): Boolean;
  ExgDBWriteProc = function(const dataP: Pointer; var sizeP: UInt32; userDataP: Pointer): Err;

function ExgInit: Err; syscall sysTrapExgInit;

function ExgConnect(socketP: ExgSocketPtr): Err; syscall sysTrapExgConnect;

function ExgPut(socketP: ExgSocketPtr): Err; syscall sysTrapExgPut;

function ExgGet(socketP: ExgSocketPtr): Err; syscall sysTrapExgGet;

function ExgAccept(socketP: ExgSocketPtr): Err; syscall sysTrapExgAccept;

function ExgDisconnect(socketP: ExgSocketPtr; error: Err): Err; syscall sysTrapExgDisconnect;

function ExgSend(socketP: ExgSocketPtr; const bufP: Pointer; const bufLen: UInt32; var err: Err): UInt32; syscall sysTrapExgSend;

function ExgReceive(socketP: ExgSocketPtr; bufP: Pointer; const bufLen: UInt32; var err: Err): UInt32; syscall sysTrapExgReceive;

function ExgRegisterData(const creatorID: UInt32; const id: UInt16; const dataTypesP: PChar): Err; syscall sysTrapExgRegisterData;

function ExgNotifyReceive(socketP: ExgSocketPtr): Err; syscall sysTrapExgNotifyReceive;


function ExgDBRead(readProcP: ExgDBReadProc; deleteProcP: ExgDBDeleteProc;
                   userDataP: Pointer; var dbIDP: LocalID; cardNo: UInt16;
                   var needResetP: Boolean; keepDates: Boolean): Err; syscall sysTrapExgDBRead;

function ExgDBWrite(writeProcP: ExgDBWriteProc;
                    userDataP: Pointer; const nameP: PChar; dbID: LocalID; cardNo: UInt16): Err; syscall sysTrapExgDBWrite;


function ExgDoDialog(socketP: ExgSocketPtr; var infoP: ExgDialogInfoType; var errP: Err): Boolean; syscall sysTrapExgDoDialog;

implementation

end.
