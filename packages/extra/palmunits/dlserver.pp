{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: DLServer.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Desktop Link Protocol(DLP) Server implementation definitions.
 *
 * History:
 *    vmk   7/12/95  Created by Vitaly Marty Kruglikov
 *    vmk   7/12/96  Converted to HTAL architecture
 *    jmp   12/23/99 Fix <> vs. "" problem.
 *
 *****************************************************************************)

unit dlserver;

interface

uses palmos, coretraps, errorbase, datamgr;

(************************************************************
 * DLK result codes
 * (dlkErrorClass is defined in SystemMgr.h)
 *************************************************************)

const
  dlkErrParam                = dlkErrorClass or 1; // invalid parameter
  dlkErrMemory               = dlkErrorClass or 2; // memory allocation error
  dlkErrNoSession            = dlkErrorClass or 3; // could not establish a session

  dlkErrSizeErr              = dlkErrorClass or 4; // reply length was too big

  dlkErrLostConnection       = dlkErrorClass or 5; // lost connection
  dlkErrInterrupted          = dlkErrorClass or 6; // sync was interrupted (see sync state)
  dlkErrUserCan              = dlkErrorClass or 7; // cancelled by user
  dlkErrIncompatibleProducts = dlkErrorClass or 8; // incompatible desktop version
  dlkErrNPOD                 = dlkErrorClass or 9; // New Password, Old Desktop

(********************************************************************
 * Desktop Link system preferences resource for user info
 * id = sysResIDDlkUserInfo, defined in SystemResources.h
 ********************************************************************)

const
  dlkMaxUserNameLength = 40;
  dlkUserNameBufSize   = dlkMaxUserNameLength + 1;

const
  dlkMaxLogSize = 20 * 1024;
// dlkMaxLogSize = 2 * 1024;

type
  DlkSyncStateType = Enum;

const
  dlkSyncStateNeverSynced = 0;                                    // never synced
  dlkSyncStateInProgress = Succ(dlkSyncStateNeverSynced);         // sync is in progress
  dlkSyncStateLostConnection = Succ(dlkSyncStateInProgress);      // connection lost during sync
  dlkSyncStateLocalCan = Succ(dlkSyncStateLostConnection);        // cancelled by local user on handheld
  dlkSyncStateRemoteCan = Succ(dlkSyncStateLocalCan);             // cancelled by user from desktop
  dlkSyncStateLowMemoryOnTD = Succ(dlkSyncStateRemoteCan);        // sync ended due to low memory on handheld
  dlkSyncStateAborted = Succ(dlkSyncStateLowMemoryOnTD);          // sync was aborted for some other reason
  dlkSyncStateCompleted = Succ(dlkSyncStateAborted);              // sync completed normally

  // Added in PalmOS v3.0:
  dlkSyncStateIncompatibleProducts = Succ(dlkSyncStateCompleted); // sync ended because desktop HotSync product
                                                                  // is incompatible with this version
                                                                  // of the handheld HotSync
  dlkSyncStateNPOD = Succ(dlkSyncStateIncompatibleProducts);      // New Password, Old Desktop

const
  dlkUserInfoPrefVersion = $0102; // current user info pref version: 1.2

type
  DlkUserInfoHdrType = record
    version: UInt16;                 // pref version number
    userID: UInt32;                  // user id
    viewerID: UInt32;                // id assigned to viewer by the desktop
    lastSyncPC: UInt32;              // last sync PC id
    succSyncDate: UInt32;            // last successful sync date
    lastSyncDate: UInt32;            // last sync date
    lastSyncState: DlkSyncStateType; // last sync status
    reserved1: UInt8;                // Explicitly account for 16-bit alignment padding
    lanSyncEnabled: UInt16;          // if non-zero, LAN Sync is enabled
    hsTcpPortNum: UInt32;            // TCP/IP port number of Desktop HotSync
    dwReserved1: UInt32;             // RESERVED -- set to NULL!
    dwReserved2: UInt32;             // RESERVED -- set to NULL!
    userNameLen: UInt8;              // length of name field(including null)
    reserved2: UInt8;                // Explicitly account for 16-bit alignment padding
    syncLogLen: UInt16;              // length of sync log(including null)
  end;

  DlkUserInfoType = record
    header: DlkUserInfoHdrType;       // fixed size header
    nameAndLog: array [0..1] of Char; // user name, followed by sync log;
                                      // both null-terminated(for debugging)
  end;

  DlkUserInfoPtr = ^DlkUserInfoType;  // user info pointer

(********************************************************************
 * Desktop Link system preferences resource for the Conduit Filter Table
 * id = sysResIDDlkCondFilterTab, defined in SystemResources.h
 ********************************************************************)

//
// Table for specifying conduits to "filter out" during HotSync
//

// This table consists of DlkCondFilterTableHdrType header followed by a
// variable number of DlkCondFilterEntryType entries

type
  DlkCondFilterTableHdrType = record
    entryCount: UInt16;
  end;
  DlkCondFilterTableHdrPtr = ^DlkCondFilterTableHdrType;

  DlkCondFilterEntryType = record
    creator: UInt32;
    type_: UInt32;
  end;
  DlkCondFilterEntryPtr = ^DlkCondFilterEntryType;

  DlkCondFilterTableType = record
    hdr: DlkCondFilterTableHdrType; // table header
    entry: array [0..0] of DlkCondFilterEntryType; // variable number of entries
  end;
  DlkCondFilterTablePtr = ^DlkCondFilterTableType;

(********************************************************************
 * DLK Session Structures
 ********************************************************************)

// DesktopLink event notification callback.  If non-zero is returned,
// sync will be cancelled as soon as a safe point is reached.
type
  DlkEventType = Enum;

const
  dlkEventOpeningConduit = 1;                              // conduit is being opened -- paramP
                                                           // is null;

  dlkEventDatabaseOpened = Succ(dlkEventOpeningConduit);   // client has opened a database -- paramP
                                                           // points to DlkEventDatabaseOpenedType;

  dlkEventCleaningUp = Succ(dlkEventDatabaseOpened);       // last stage of sync -- cleaning up (notifying apps, etc) --
                                                           // paramP is null

  dlkEventSystemResetRequested = Succ(dlkEventCleaningUp); // system reset was requested by the desktop client
                                                           // (the normal action is to delay the reset until
                                                           // end of sync) -- paramP is null

// Prototype for the event notification callback
type
  DlkEventProc = function(eventRef: UInt32; dlkEvent: DlkEventType; paramP: Pointer): Int16;

// Parameter structure for dlkEventDatabaseOpened
// Added new fields for Pilot v2.0      vmk 12/24/96
type
  DlkEventDatabaseOpenedType = record
    dbR: DmOpenRef;    // open database ref (v2.0)
    dbNameP: PChar;    // database name
    dbType: UInt32;    // databse type (v2.0)
    dbCreator: UInt32; // database creator
  end;

// Prototype for the "user cancel" check callback function

type
  DlkUserCanProc = function(canRef: UInt32): Int16;

//
// List of modified database creators maintained by DLP Server
//
  DlkDBCreatorList = record
    count: UInt16;    // number of entries in the list
    listH: MemHandle; // chunk MemHandle of the creators list
  end;

//
// Desktop Link Server state flags
//
const
  dlkStateFlagVerExchanged = $8000;
  dlkStateFlagSyncDateSet  = $4000;

//
// DLP Server session information
//

type
  DlkServerSessionType = record
    htalLibRefNum: UInt16;       // HTAL library reference number - the library has a live connection
    maxHtalXferSize: UInt32;     // Maximum transfer block size

    // Information supplied by user
    eventProcP: DlkEventProc;    // ptr to DesktopLink event notification proc
    eventRef: UInt32;            // user reference value for event proc
    canProcP: DlkUserCanProc;    // ptr to user-cancel function
    canRef: UInt32;              // parameter for canProcP()
    condFilterH: MemHandle;      // MemHandle of conduit filter table(DlkCondFilterTableHdrPtr) or 0 for none

    // Current database information
    dlkDBID: UInt8;              // Desktop Link database MemHandle of the open database
    reserved1: UInt8;
    dbR: DmOpenRef;              // TouchDown database access pointer -- if null, no current db
    cardNo: UInt16;              // memory module number
    dbCreator: UInt32;           // creator id
    dbName: array [0..dmDBNameLength-1] of Char; // DB name
    dbOpenMode: UInt16;          // database open mode
    created: Boolean;            // true if the current db was created
    isResDB: Boolean;            // set to true if resource database
    ramBased: Boolean;           // true if the db is in RAM storage
    readOnly: Boolean;           // true if the db is read-only
    dbLocalID: LocalID;          // TouchDown LocalID of the database
    initialModNum: UInt32;       // initial DB modification number
    curRecIndex: UInt32;         // current record index for enumeration functions
                                 // (0=beginning)

    // List of modified database creators maintained by DLP Server
    creatorList: DlkDBCreatorList;

    // Session status information
    syncState: DlkSyncStateType; // current sync state;

    complete: Boolean;           // set to true when completion request
                                 // has been received

    conduitOpened: Boolean;      // set to true after the first coduit
                                 // is opened by remote

    logCleared: Boolean;         // set to true after sync log has been
                                 // cleared during the current session;
                                 // The log will be cleared before any new entries are added or at
                                 // the end of sync in case no new entries were added.
                                 // (we do not clear the log at the beginning of sync in case the
                                 // user cancels during the "identifying user" phase; in this
                                 // event, the spec calls for preserving the original log)

    resetPending: Boolean;       // set to true if system reset is pending;
                                 // the reset will be carried out at end
                                 // of sync

    // Current request information
    gotCommand: Boolean;         // set to true when got a request
    cmdTID: UInt8;               // current transaction ID
    reserved2: UInt8;
    cmdLen: UInt16;              // size of data in request buffer
    cmdP: Pointer;               // pointer to command
    cmdH: MemHandle;             // MemHandle of command buffer

    // Fields added in PalmOS v3.0
    wStateFlags: UInt16;         // bitfield of dlkStateFlag... bits
    dbSearchState: DmSearchStateType; // database search state for iterative
                                 // searches using DmGetNextDatabaseByTypeCreator

    // Fields added in PalmOS v4.0
    openFileRefsH: MemHandle ;   // Table of open file refs
    numOpenFileRefs: Int16;      // Current size of the file ref table.
    pre40Desktop: Boolean;       // are we using a pre-4.0 desktop (DLP v1.2)
    passwordSet: Boolean;        // is a password set?
  end;

  DlkServerSessionPtr = ^DlkServerSessionType;

(********************************************************************
 * DLK Function Parameter Structures
 ********************************************************************)

//
// Parameter passed to DlkControl()
//

type
  DlkCtlEnum = Enum;

const
  dlkCtlFirst = 0;                                           // reserve 0

  //
  // Pilot v2.0 control codes:
  //
  dlkCtlGetPCHostName = Succ(dlkCtlFirst);                   // param1P = ptr to text buffer; (can be null if *(UInt16 *)param2P is 0)
                                                             // param2P = ptr to buffer size(UInt16);
                                                             // returns actual length, including null, in *(UInt16 *)param2P which may be bigger than # of bytes copied.

  dlkCtlSetPCHostName = Succ(dlkCtlGetPCHostName);           // param1P = ptr to host name(zero-terminated) or NULL if *param2 is 0
                                                             // param2P = ptr to length(UInt16), including NULL (if length is 0, the current name is deleted)

  dlkCtlGetCondFilterTable = Succ(dlkCtlSetPCHostName);      // param1P =    ptr to destination buffer for filter table, or NULL if *param2 is 0
                                                             // param2P =    on entry, ptr to size of buffer(UInt16) (the size may be 0)
                                                             //              on return, size, in bytes, of the actual filter table

  dlkCtlSetCondFilterTable = Succ(dlkCtlGetCondFilterTable); // param1P =    ptr to to conduit filter table, or NULL if *param2 is 0
                                                             // param2P =    ptr to size of filter table(UInt16) (if size is 0, the current table will be deleted)

  dlkCtlGetLANSync = Succ(dlkCtlSetCondFilterTable);         // param1P =    ptr to store for the LANSync setting(UInt16): 0 = off, otherwise on
                                                             // param2P =    not used, set to NULL

  dlkCtlSetLANSync = Succ(dlkCtlGetLANSync);                 // param1P =    ptr to the LANSync setting(UInt16): 0 = off, otherwise on
                                                             // param2P =    not used, set to NULL

  dlkCtlGetHSTCPPort = Succ(dlkCtlSetLANSync);               // param1P =    ptr to store for the Desktop HotSync TCP/IP port number(UInt32) -- zero if not set
                                                             // param2P =    not used, set to NULL

  dlkCtlSetHSTCPPort = Succ(dlkCtlGetHSTCPPort);             // param1P =    ptr to the Desktop HotSync TCP/IP port number(UInt32)
                                                             // param2P =    not used, set to NULL

  dlkCtlSendCallAppReply = Succ(dlkCtlSetHSTCPPort);         // param1P =    ptr to DlkCallAppReplyParamType structure
                                                             // param2P =    not used, set to NULL
                                                             //
                                                             // RETURNS: send error code; use this error code
                                                             // as return value from the action code handler

  dlkCtlGetPCHostAddr = Succ(dlkCtlSendCallAppReply);        // param1P = ptr to text buffer; (can be null if *(UInt16 *)param2P is 0)
                                                             // param2P = ptr to buffer size(UInt16);
                                                             // returns actual length, including null, in *(UInt16 *)param2P which may be bigger than # of bytes copied.

  dlkCtlSetPCHostAddr = Succ(dlkCtlGetPCHostAddr);           // param1P = ptr to host address string(zero-terminated) or NULL if *param2 is 0
                                                             // param2P = ptr to length(UInt16), including NULL (if length is 0, the current name is deleted)


  dlkCtlGetPCHostMask = Succ(dlkCtlSetPCHostAddr);            // param1P = ptr to text buffer; (can be null if *(UInt16 *)param2P is 0)
                                                              // param2P = ptr to buffer size(UInt16);
                                                              // returns actual length, including null, in *(UInt16 *)param2P which may be bigger than # of bytes copied.

  dlkCtlSetPCHostMask = Succ(dlkCtlGetPCHostMask);            // param1P = ptr to subnet mask string(zero-terminated) or NULL if *param2 is 0
                                                              // param2P = ptr to length(UInt16), including NULL (if length is 0, the current name is deleted)


  dlkCtlLAST = Succ(dlkCtlSetPCHostMask);                     // *KEEP THIS ENTRY LAST*

//
// Parameter passed to DlkStartServer()
//

type
  DlkServerParamType = record
    htalLibRefNum: UInt16;       // HTAL library reference number - the library has a live connection
    eventProcP: DlkEventProc;    // ptr to DesktopLink event notification proc
    eventRef: UInt32;            // user reference value for event proc
    reserved1: UInt32;           // reserved - set to NULL
    reserved2: UInt32;           // reserved - set to NULL
    condFilterH: MemHandle;      // MemHandle of conduit filter table(DlkCondFilterTableHdrPtr) or 0 for none
  end;

  DlkServerParamPtr = ^DlkServerParamType;

//
// Parameter passed with DlkControl()'s dlkCtlSendCallAppReply code
//

type
  DlkCallAppReplyParamType = record
    pbSize: UInt16;       // size of this parameter block (set to sizeof(DlkCallAppReplyParamType))
    dwResultCode: UInt32; // result code to be returned to remote caller
    {const} resultP: Pointer; // ptr to result data
    dwResultSize: UInt32; // size of reply data in number of bytes
    dlRefP: Pointer;      // DesktopLink reference pointer from
                          // SysAppLaunchCmdHandleSyncCallAppType
    dwReserved1: UInt32;  // RESERVED -- set to null!!!
  end;

(********************************************************************
 * DesktopLink Server Routines
 ********************************************************************)

//
// SERVER API
//

// * RETURNED:  0 if session ended successfully; otherwise: dlkErrParam,
// *                dlkErrNoSession, dlkErrLostConnection, dlkErrMemory,
// *                dlkErrUserCan

function DlkStartServer(paramP: DlkServerParamPtr): Err; syscall sysTrapDlkStartServer;

function DlkGetSyncInfo(var succSyncDateP, lastSyncDateP: UInt32;
                        var syncStateP: DlkSyncStateType; nameBufP, logBufP: PChar;
                        var logLenP: Int32): Err; syscall sysTrapDlkGetSyncInfo;

procedure DlkSetLogEntry(const textP: PChar; textLen: Int16; append: Boolean); syscall sysTrapDlkSetLogEntry;

// Dispatch a DesktopLink request (exposed for patching)
function DlkDispatchRequest(sessP: DlkServerSessionPtr): Err; syscall sysTrapDlkDispatchRequest;

function DlkControl(op: DlkCtlEnum; param1P, param2P: Pointer): Err; syscall sysTrapDlkControl;

(********************************************************************
 * DLK Macros
 ********************************************************************)

implementation

end.
