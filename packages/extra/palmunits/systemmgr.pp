{$MACRO ON}
{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SystemMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Pilot system equates
 *
 * History:
 *    10/27/94 RM    Created by Ron Marianetti
 *    10/07/96 SCL   Added sysAppLaunchFlagDataRelocated flag
 *    11/13/96 vmk   Added sysErrDelayWakened error code
 *    08/12/98 dia   Added sysFtrNumGremlinsSupportGlobals.
 *    08/18/98 SCL   Added sysFtrNumHwrMiscFlags and ...FlagsExt.
 *                   Redefined sysFtrNumProcessorID.
 *    08/23/98 SCL   Merged in tsmErrorClass.
 *    09/07/98 kwk   Added SysWantEvent routine declaration.
 *    10/05/98 jfs   Added SysLCDContrast trap descriptor
 *    04/08/99 kwk   Added sysFtrNumVendor (OS 3.3 and later)
 *    06/28/99 kwk   Added omErrorClass.
 *    08/11/99 kwk   Added sysFtrNumCharEncodingFlags.
 *    11/01/99 kwk   Moved SysWantEvent to SystemPrv.h
 *    12/03/99 SCL   Moved SysAppInfoType, SysAppStartup, and SysAppExit
 *                   here from SystemPrv.h (for StartupCode/Runtime)
 *    07/19/00 gap   Added launch code sysAppLaunchCmdAttention for use by
 *                   Attention Manager
 *    7/26/00  jhl   Integrate HSIMgr functionality
 *    09/22/00 BGT   Integrated sysAppLaunchNppiNoUI and sysAppLaunchNppiUI
 *
 *****************************************************************************)

unit systemmgr;

interface

uses palmos, coretraps, libtraps, errorbase, bitmap, datamgr, systemresources, event_;

(************************************************************
 * System Constants
 *************************************************************)

// Define the number of ticks/second
// NOTE:  It is strongly recommended that developers avoid using these
// defines, and use the SysTicksPerSecond() API (below) instead....
//#if EMULATION_LEVEL == EMULATION_MAC
// #define sysTicksPerSecond    60     // 60/sec on Macintosh
//#elif EMULATION_LEVEL == EMULATION_NONE
const
  sysTicksPerSecond_ = 100; // 100/sec on Pilot
//#elif EMULATION_LEVEL == EMULATION_WINDOWS
// #define sysTicksPerSecond    1000    // 1000/sec on Windows PC
//#elif EMULATION_LEVEL == EMULATION_UNIX
// #define sysTicksPerSecond    1000
// // 1000/sec on Linux
//#else
// #error Invalid EMULATION_LEVEL
//#endif

(************************************************************
 * Rules for creating and using the Command Parameter Block
 * passed to SysUIAppSwitch
 *************************************************************)

// A parameter block containing application-specific information may be passed
// to an application when launching it via SysUIAppSwitch.  To create the
// parameter block, you allocate a memory block using MemPtrNew and then you must
// call MemPtrSetOwner to set the block's owner ID to 0.  This assigns the block's
// ownership to the system so that it will not be automatically freed by the system
// when the calling app exits. The command block must be self contained. It must not
// have pointers to anything on the stack or in memory blocks owned by an application.
// The launching and launched applications do not need to worry about freeing the
// command block since the system will do this after the launched application exits.
// If no parameter block is being passed, this parameter must be NULL.

(************************************************************
 * Action Codes
 *
 * IMPORTANT ACTION CODE CONSIDERATIONS:
 *
 * Many action codes are "sent" to apps via a direct function call into the app's
 * PilotMain() function without launching the app.  For these action codes, the
 * application's global and static variables are *not* available, unless the
 * application is already running. Some action codes are synchronized with the
 * currently running UI applcation via the event manager (alarm action codes,
 * for example), while others, such as HotSync action codes, are sent from a
 * background thread. To find out if your app is running (is the current UI
 * app) when an action code is received, test the sysAppLaunchFlagSubCall flag
 * (defined in SystemMgr.h) which is passed to your PilotMain in the
 * launchFlags parameter (the third PilotMain parameter). If it is non-zero,
 * you may assume that your app is currently running and the global variables
 * are accessible. This information is useful if your app maintains an open
 * data database (or another similar resource) when it is running. If the app
 * receives an action code and the sysAppLaunchFlagSubCall is set in
 * launchFlags, the handler may access global variables and use the open
 * database handle while handling the call. On the other hand, if the
 * sysAppLaunchFlagSubCall flag is not set (ie., zero), the handler will need
 * to open and close the database itself and is not allowed to access global
 * or static variables.
 *
 *************************************************************)

// NOTE: for defining custom action codes, see sysAppLaunchCmdCustomBase below.

// System SysAppLaunch Commands
const
  sysAppLaunchCmdNormalLaunch = 0; // Normal Launch

  sysAppLaunchCmdFind = 1; // Find string

  sysAppLaunchCmdGoTo = 2; // Launch and go to a particular record

  sysAppLaunchCmdSyncNotify = 3; // Sent to apps whose databases changed during
               // HotSync after the sync has been completed,
               // including when the app itself has been installed
               // by HotSync. The data database(s) must have the
               // same creator ID as the application for this
               // mechanism to function correctly. This is a
               // good opportunity to update/initialize/validate
               // the app's data, such as resorting records,
               // setting alarms, etc.
               //
               // Parameter block: None.
               // Restrictions: No accessing of global or
               //  static variables; no User Interface calls.
               // Notes: This action code is sent via a
               //  direct function call into the app's
               //  PilotMain function from the background
               //  thread of the HotSync application.

  sysAppLaunchCmdTimeChange = 4; // Sent to all applications and preference
               // panels when the system time is changed.
               // This notification is the right place to
               // update alarms and other time-related
               // activities and resources.
               //
               // Parameter block: None.
               // Restrictions: No accessing of global or
               //  static variables; no User Interface calls.
               // Notes: This action code is sent via a direct
               //  function call into the app's PilotMain
               //  function without "launching" the app.

  sysAppLaunchCmdSystemReset = 5; // Sent to all applications and preference
               // panels when the system is either soft-reset
               // or hard-reset.  This notification is the
               // right place to initialize and/or validate
               // your application's preferences/features/
               // database(s) as well as to update alarms and
               // other time-related activities and resources.
               //
               // Parameter block: SysAppLaunchCmdSystemResetType
               // Restrictions: No accessing of global or
               //  static variables; no User Interface calls.
               // Notes: This action code is sent via a direct
               //  function call into the app's PilotMain
               //  function without "launching" the app.

  sysAppLaunchCmdAlarmTriggered = 6; // Sent to an application at the time its
               // alarm time expires (even when another app
               // is already displaying its alarm dialog box).
               // This call is intended to allow the app to
               // perform some very quick activity, such as
               // scheduling the next alarm or performing a
               // quick maintenance task.  The handler for
               // sysAppLaunchCmdAlarmTriggered must take as
               // little time as possible and is *not* allowed
               // to block (this would delay notification for
               // alarms set by other applications).
               //
               // Parameter block: SysAlarmTriggeredParamType
               //  (defined in AlarmMgr.h)
               // Restrictions: No accessing of global or
               //  static variables unless sysAppLaunchFlagSubCall
               //  flag is set, as discussed above.
               // Notes: This action code is sent via a direct
               //  function call into the app's PilotMain
               //  function without "launching" the app.

  sysAppLaunchCmdDisplayAlarm = 7; // Sent to an application when it is time
               // to display the alarm UI. The application
               // is responsible for making any alarm sounds
               // and for displaying the alarm UI.
               // sysAppLaunchCmdDisplayAlarm calls are ordered
               // chronoligically and are not overlapped.
               // This means that your app will receive
               // sysAppLaunchCmdDisplayAlarm only after
               // all earlier alarms have been displayed.
               //
               // Parameter block: SysDisplayAlarmParamType
               //  (defined in AlarmMgr.h)
               // Restrictions: No accessing of global or
               //  static variables unless sysAppLaunchFlagSubCall
               //  flag is set, as discussed above.  UI calls are
               //  allowed to display the app's alarm dialog.
               // Notes: This action code is sent via a direct
               //  function call into the app's PilotMain
               //  function without "launching" the app.

  sysAppLaunchCmdCountryChange = 8; // The country has changed

  sysAppLaunchCmdSyncRequestLocal = 9; // Sent to the HotSync application to request a
               // local HotSync.  ("HotSync" button was pressed.)

  sysAppLaunchCmdSyncRequest = sysAppLaunchCmdSyncRequestLocal; // for backward compatibility

  sysAppLaunchCmdSaveData = 10; // Sent to running app before sysAppLaunchCmdFind
               // or other action codes that will cause data
               // searches or manipulation.

  sysAppLaunchCmdInitDatabase = 11; // Sent to an application when a database with
               // a matching Creator ID is created during
               // HotSync (in response to a "create db"
               // request). This allows the application to
               // initialize a newly-created database during
               // HotSync.  This might include creating some
               // default records, setting up the database's
               // application and sort info blocks, etc.
               //
               // Parameter block: SysAppLaunchCmdInitDatabaseType
               // Restrictions: No accessing of global or
               //  static variables; no User Interface calls.
               // Notes: This action code is sent via a
               //  direct function call into the app's
               //  PilotMain function from the background
               //  thread of the HotSync application.

  sysAppLaunchCmdSyncCallApplicationV10 = 12; // Used by DesktopLink Server command "call application";
               // Pilot v1.0 only!!!

//------------------------------------------------------------------------
// New launch codes defined for PalmOS 2.0
//------------------------------------------------------------------------

  sysAppLaunchCmdPanelCalledFromApp = 13; // The panel should display a done
                // button instead of the pick list.
                // The Done button will return the user
                // to the last app.

  sysAppLaunchCmdReturnFromPanel = 14; // A panel returned to this app

  sysAppLaunchCmdLookup = 15; // Lookup info managed by an app

  sysAppLaunchCmdSystemLock = 16; // Lock the system until a password is entered.

  sysAppLaunchCmdSyncRequestRemote = 17; // Sent to the HotSync application to request
                // a remote HotSync.  ("Remote HotSync" button
                // was pressed.)

  sysAppLaunchCmdHandleSyncCallApp = 18; // Pilot v2.0 and greater.  Sent by DesktopLink Server to an application to handle
                // the "call application" command; use DlkControl with
                // control code dlkCtlSendCallAppReply to send the reply(see DLServer.h).
                // This action code replaces the v1.0 code sysAppLaunchCmdSyncCallApplication.
                // vmk 11/26/96

  sysAppLaunchCmdAddRecord = 19; // Add a record to an applications's database.

//------------------------------------------------------------------------
// Standard Service Panel launch codes (used by network panel, dialer panel, etc.)
//------------------------------------------------------------------------

  sysSvcLaunchCmdSetServiceID    = 20;
  sysSvcLaunchCmdGetServiceID    = 21;
  sysSvcLaunchCmdGetServiceList  = 22;
  sysSvcLaunchCmdGetServiceInfo  = 23;

  sysAppLaunchCmdFailedAppNotify = 24; // An app just switched to failed.
  sysAppLaunchCmdEventHook       = 25; // Application event hook callback
  sysAppLaunchCmdExgReceiveData  = 26; // Exg command for app to receive data.
  sysAppLaunchCmdExgAskUser      = 27; // Exg command sent before asking user.

//------------------------------------------------------------------------
// Standard Dialer Service launch codes (30 - 39 reserved)
//------------------------------------------------------------------------

// sysDialLaunchCmdDial: dials the modem(optionally displays dial progress UI), given service id
// and serial library reference number
  sysDialLaunchCmdDial = 30;
// sysDialLaunchCmdHangUp: hangs up the modem(optionally displays disconnect progress UI), given service id
// and serial library reference number
  sysDialLaunchCmdHangUp = 31;
  sysDialLaunchCmdLast = 39;

//------------------------------------------------------------------------
// Additional standard Service Panel launch codes (used by network panel, dialer panel, etc)
// (40-49 reserved)
//------------------------------------------------------------------------

  sysSvcLaunchCmdGetQuickEditLabel = 40; // SvcQuickEditLabelInfoType
  sysSvcLaunchCmdLast = 49;

//------------------------------------------------------------------------
// New launch codes defined for PalmOS 3.x where x >= 1
//------------------------------------------------------------------------

  sysAppLaunchCmdURLParams = 50; // Sent from the Web Clipper application.
                 // This launch code gets used to satisfy
                 // URLs like the following:
                 //    palm:memo.appl?param1=value1&param2=value2
                 // Everything in the URL past the '?' is passed
                 // to the app as the cmdPBP parameter of PilotMain().

  sysAppLaunchCmdNotify = 51; // This is a NotifyMgr notification sent
                 // via SysNotifyBroadcast.  The cmdPBP parameter
                 // points to a SysNotifyParamType structure
                 // containing more specific information
                 // about the notification (e.g., what it's for).

  sysAppLaunchCmdOpenDB = 52; // Sent to switch to an application and have it
                 // "open" up the given data file. The cmdPBP
                 // pointer is a pointer to a SysAppLaunchCmdOpenDBType
                 // structure that has the cardNo and localID of the database
                 // to open. This action code is used by the Launcher
                 // to launch data files, like Eleven PQA files that
                 // have the dmHdrAttrLaunchableData bit set in their
                 // database attributes.

  sysAppLaunchCmdAntennaUp = 53; // Sent to switch only to the launcher when
                 // the antenna is raised and the launcher
                 // is the application in the buttons preferences
                 // that is to be run when the antenna is raised is
                 // the launcher.

  sysAppLaunchCmdGoToURL = 54; // Sent to Clipper to have it launch and display
                 // a given URL.  cmdPBP points to the URL string.

// Begin Change - BGT 03/21/2000

//------------------------------------------------------------------------
// New launch codes defined for Network panel plug-in
//------------------------------------------------------------------------

  sysAppLaunchNppiNoUI = 55; // Sent to network panel plug-in ("nppi") to have it launch
                             // without UI and load to netlib

  sysAppLaunchNppiUI = 56;   // Sent to network panel plug-in ("nppi") to have it launch
                             // with UI
// End Change - BGT 03/21/2000

//------------------------------------------------------------------------
// New launch codes defined for PalmOS 4.x where x >= 0
//------------------------------------------------------------------------

  sysAppLaunchCmdExgPreview = 57; // Sent to an application by the Exchange Manager when the
                                  // application needs to produce a preview.


  sysAppLaunchCmdCardLaunch = 58; // Sent to an application by the Launcher when the
                                  // application is being run from a card.

  sysAppLaunchCmdExgGetData = 59; // Exg command for app to send data requested by an ExgGet

  sysAppLaunchCmdAttention  = 60; // sent to an application by the attention manager
                                  // when the application needs to take action on an entry
                                  // that has been submitted to the attention manager queue.

  sysAppLaunchPnpsPreLaunch = 61; // pre-launch code for Pnps devices,
                                  // cmdPBP points to SysAppLaunchCmdPnpsType

// ***ADD NEW SYSTEM ACTION CODES BEFORE THIS COMMENT***

//------------------------------------------------------------------------
// Custom action code base (custom action codes begin at this value)
//------------------------------------------------------------------------

  sysAppLaunchCmdCustomBase = $8000;

// Your custom launch codes can be defined like this:
//
// type
//   MyAppCustomActionCodes = WordEnum;
// const
//   myAppCmdDoSomething = sysAppLaunchCmdCustomBase;
//   myAppCmdDoSomethingElse = Succ(myAppCmdDoSomething);
//   myAppCmdEtcetera = Succ(myAppCmdDoSomethingElse);

//------------------------------------------------------------------------
// SysAppLaunch flags (passed to PilotMain)
//------------------------------------------------------------------------

  sysAppLaunchFlagNewThread = $01; // create a new thread for application
               //  - implies sysAppLaunchFlagNewStack
  sysAppLaunchFlagNewStack = $02; // create separate stack for application
  sysAppLaunchFlagNewGlobals = $04; // create new globals world for application
               //  - implies new owner ID for Memory chunks
  sysAppLaunchFlagUIApp = $08; // notifies launch routine that this is a UI app being
               //  launched.
  sysAppLaunchFlagSubCall = $10; // notifies launch routine that the app is calling it's
               //  entry point as a subroutine call. This tells the launch
               //  code that it's OK to keep the A5 (globals) pointer valid
               //  through the call.
               // IMPORTANT: This flag is for internal use by
               //  SysAppLaunch only!!! It should NEVER be set
               //  by the caller.
  sysAppLaunchFlagDataRelocated = $80; // global data (static ptrs) have been "relocated"
               //  by either SysAppStartup or StartupCode.c
               // IMPORTANT: This flag is for internal use by
               //  SysAppLaunch only!!! It should NEVER be set
               //  by the caller.

// The set of private, internal flags that should never be set by the caller
  sysAppLaunchFlagPrivateSet = sysAppLaunchFlagSubCall or sysAppLaunchFlagDataRelocated;

//-------------------------------------------------------------------
// Parameter blocks for action codes
// NOTE: The parameter block for the  sysAppLaunchCmdFind  and sysAppLaunchCmdGoTo
//  action codes are defined in "Find.h";
//---------------------------------------------------------------------------

// For sysAppLaunchCmdSaveData
type
  SysAppLaunchCmdSaveDataType = record
    uiComing: Boolean; // true if system dialog will be put up
                       // before coming action code arrives.
    reserved1: UInt8;
  end;

// For sysAppLaunchCmdSystemReset
  SysAppLaunchCmdSystemResetType = record
    hardReset: Boolean;       // true if system was hardReset, false if soft-reset.
    createDefaultDB: Boolean; // true if app should create default database.
  end;

// For sysAppLaunchCmdInitDatabase
  SysAppLaunchCmdInitDatabaseType = record
    dbP: DmOpenRef;        // Handle of the newly-created database,
                 //  already open for read/write access.
                 //  IMPORTANT: The handler *MUST* leave
                 //  this database handle open on return.
    creator: UInt32;       // Creator ID of the newly-created database
    type_: UInt32;         // Type ID of the newly-created database
    version: UInt16;       // Version number of the newly-created database
  end;

// For sysAppLaunchCmdSyncCallApplicationV10
// This structure used on Pilot v1.0 only.  See sysAppLaunchCmdHandleSyncCallApp
// for later platforms.
  SysAppLaunchCmdSyncCallApplicationTypeV10 = record
    action: UInt16;      // call action id (app-specific)
    paramSize: UInt16;   // parameter size
    paramP: Pointer;     // ptr to parameter
    remoteSocket: UInt8; // remote socket id
    tid: UInt8;          // command transaction id
    handled: Boolean;    // if handled, MUST be set true by the app
    reserved1: UInt8;
  end;

// For sysAppLaunchCmdHandleSyncCallApp (Pilot v2.0 and greater).
// This structure replaces SysAppLaunchCmdSyncCallApplicationType
// which was used in Pilot v1.0
  SysAppLaunchCmdHandleSyncCallAppType = record
    pbSize: UInt16;      // this parameter block size (set to sizeof SysAppLaunchCmdHandleSyncCallAppType)
    action: UInt16;      // call action id (app-specific)
    paramP: Pointer;     // ptr to parameter
    dwParamSize: UInt32; // parameter size
    dlRefP: Pointer;     // DesktopLink reference pointer for passing
                         // to DlkControl()'s dlkCtlSendCallAppReply code

    handled: Boolean;    // initialized to FALSE by DLServer; if
              // handled, MUST be set TRUE by the app(the
              // handler MUST call DlkControl with
              // control code dlkCtlSendCallAppReply);
              // if the handler is not going to send a reply,
              // it should leave this field set to FALSE, in which
              // case DesktopLink Server will send the default
              // "unknown request" reply.

    reserved1: UInt8;

    replyErr: Err; // error from dlkCtlSendCallAppReply

    // RESERVED FOR FUTURE EXTENSIONS
    dwReserved1: UInt32; // RESERVED -- set to null!!!
    dwReserved2: UInt32; // RESERVED -- set to null!!!

 // Target executable creator and type for testing the mechanism
 // in EMULATION MODE ONLY!!!
// #if EMULATION_LEVEL != EMULATION_NONE
//    creator: UInt32;
//    type_: UInt32;
// #endif
  end;

// For sysAppLaunchCmdFailedAppNotify
  SysAppLaunchCmdFailedAppNotifyType = record
    creator: UInt32;
    type_: UInt32;
    result: Err;
  end;

// For sysAppLaunchCmdOpenDB
  SysAppLaunchCmdOpenDBType = record
    cardNo: UInt16;
    dbID: LocalID;
  end;

// For sysAppLaunchCmdCardLaunch
type
  SysAppLaunchCmdCardType = record
    err: Err;
    volRefNum: UInt16;
    path: PChar;
    startFlags: UInt16; // See vfsStartFlagXXX constants below
  end;

const
  sysAppLaunchStartFlagAutoStart    = $0001; // this bit in the 'startFlags' field is set for an app which is run automatically on card insertion
  sysAppLaunchStartFlagNoUISwitch   = $0002; // set this bit in the 'startFlags' field to prevent a UI switch to the start.prc app
  sysAppLaunchStartFlagNoAutoDelete = $0004; // set this bit in the 'startFlags' field to prevent VFSMgr from deleting start.prc app on volume unmount

//for launch code sysAppLaunchPnpsPreLaunch
type
  SysAppLaunchCmdPnpsType = record
    error: Err;            // an error code from the pre-launch application, set to errNone to prevent normal launching
    volRefNum: UInt16;     // Non-zero if an optional file system was mounted
    slotLibRefNum: UInt16; // always valid for a slot driver call
    slotRefNum: UInt16;    // always valid for a slot driver call
  end;

(************************************************************
 * Structure of Application info for an application. Applications
 *  do not necessarily have to be on their own thread - there
 *  can be more than 1 app on the same AMX task. Each application
 *  has an assocated SysAppInfoType structure which holds the
 *  application specific information like the database MemHandle of the
 *  app, the code MemHandle, the stack chunk pointer, the owner ID, etc.
 *
 * As of PalmOS 3.X, one of these structures is created for each
 *  app running as an action code.
 *
 ****
 ****IMPORTANT: ADD NEW FIELDS AT THE END OF THE STRUCTURE FOR
 ****           BACKWARD COMPATIBILITY
 ****
 *************************************************************)

  SysAppInfoTag = record
    cmd: Int16;            // command code for app
    cmdPBP: MemPtr;        // cmd ParamBlock
    launchFlags: UInt16;   // launch flags

    taskID: UInt32;        // AMX task ID of task that app runs in
    codeH: MemHandle;      // code MemHandle of the main code segment
    dbP: DmOpenRef;        // Application database access MemPtr of App
    stackP: ^UInt8;        // stack chunk for the App
    globalsChunkP: ^UInt8; // globals chunk for the App

    memOwnerID: UInt16;    // owner ID for Memory Manager chunks
    dmAccessP: MemPtr;     // pointer to linked list of opened DB's
    dmLastErr: Err;        // Last error from Data Manager
    errExceptionP: MemPtr; // ErrTry,Catch exception list

    // PalmOS v3.0 fields begin here
    a5Ptr: ^UInt8;         // A5 MemPtr for this app
    stackEndP: ^UInt8;     // stack chunk end for the App (last byte)
    globalEndP: ^UInt8;    // global chunk end for the App (last byte)
    rootP: ^SysAppInfoType;// Points to the SysAppInfoType first
                           // allocated for this thread.
    extraP: MemPtr;        // unused MemPtr for the App.
  end;
  SysAppInfoType = SysAppInfoTag;
  SysAppInfoPtr = ^SysAppInfoType;

(************************************************************
 * Function prototype for libraries
 *************************************************************)

// ***IMPORTANT***
// ***IMPORTANT***
// ***IMPORTANT***
//
// The assembly level TrapDispatcher() function uses a hard-coded value for
// the size of the structure SysLibTblEntryType to obtain a pointer to a
// library entry in the library table.  Therefore, any changes to this structure,
// require corresponding changes in TrapDispatcher() in ROMBoot.c.  Furthermore,
// it is advantageous to keep the size of the structure a power of 2 as this
// improves performance by allowing the entry offset to be calculated by shifting
// left instead of using the multiply instruction.  vmk 8/27/96 (yes, I fell into
// this trap myself)
  SysLibTblEntryType = record
    dispatchTblP: ^MemPtr;  // pointer to library dispatch table
    globalsP: Pointer;      // Library globals

    // New INTERNAL fields for v2.0 (vmk 8/27/96):
    dbID: LocalID;          // database id of the library
    codeRscH: Pointer;      // library code resource handle for RAM-based libraries
  end;
  SysLibTblEntryPtr = ^SysLibTblEntryType;

// Emulated versions of libraries have a slightly different dispatch table
// Enough for the offset to the library name and the name itself.
//#if EMULATION_LEVEL != EMULATION_NONE
  SimDispatchTableType = record
    numEntries: UInt32 ;              // number of library entries
    entries: array [0..0] of Pointer; // dispatch routine entries
                                      // followed by pointer to name
  end;
  SimDispatchTablePtr = ^SimDispatchTableType;
//#endif

// Library entry point procedure
  SysLibEntryProcPtr = function(refNum: UInt16; entryP: SysLibTblEntryPtr): Err;

// This library refNum is reserved for the Debugger comm library
const
  sysDbgCommLibraryRefNum = 0;

// This portID is reserved for identifying the debugger's port
  sysDbgCommPortID = $C0FF;

// This refNum signals an invalid refNum
  sysInvalidRefNum = $FFFF;

(************************************************************
 * Function prototype for Kernel
 *************************************************************)

// Task termination procedure prototype for use with SysTaskSetTermProc
type
  SysTermProcPtr = procedure(taskID: UInt32; reason: Int32);

// Timer procedure for use with SysTimerCreate
  SysTimerProcPtr = procedure(timerID, param: Int32);

(************************************************************
 * Structure of the pref=0 resource in applications. Note, this
 *  structure must mirror the structure of the sysResTAppPrefs
 *  resource as defined in SystemResources.h.
 *************************************************************)

type
  SysAppPrefs = record
    priority: UInt16;     // task priority
    stackSize: UInt32;    // required stack space
    minHeapSpace: UInt32; // minimum heap space required
  end;
  SysAppPrefsType = SysAppPrefs;
  SysAppPrefsPtr = ^SysAppPrefsType;

(************************************************************
 * Structure of the xprf=0 resource in resource DBs. Note, this
 * structure must mirror the structure of the sysResTExtPrefs
 * resource as defined in SystemResources.h. Also, fields can only
 * be added (at the end), never removed or changed.
 *************************************************************)

const
  sysExtPrefsVers = 1;

// Flags defined for SysExtPrefsType.flags
const
  sysExtPrefsNoOverlayFlag = $00000001;

type
  SysExtPrefsType = record
    version: UInt16; // version of structure.
    flags: UInt32;   // 32 boolean flags.
  end;

(************************************************************
 * System Errors
 *************************************************************)

const
  sysErrTimeout         = sysErrorClass or 1;
  sysErrParamErr        = sysErrorClass or 2;
  sysErrNoFreeResource  = sysErrorClass or 3;
  sysErrNoFreeRAM       = sysErrorClass or 4;
  sysErrNotAllowed      = sysErrorClass or 5;
  sysErrSemInUse        = sysErrorClass or 6;
  sysErrInvalidID       = sysErrorClass or 7;
  sysErrOutOfOwnerIDs   = sysErrorClass or 8;
  sysErrNoFreeLibSlots  = sysErrorClass or 9;
  sysErrLibNotFound     = sysErrorClass or 10;
  sysErrDelayWakened    = sysErrorClass or 11; // SysTaskDelay wakened by SysTaskWake before delay completed.
  sysErrRomIncompatible = sysErrorClass or 12;
  sysErrBufTooSmall     = sysErrorClass or 13;
  sysErrPrefNotFound    = sysErrorClass or 14;

// NotifyMgr error codes:
  sysNotifyErrEntryNotFound      = sysErrorClass or 16; // could not find registration entry in the list
  sysNotifyErrDuplicateEntry     = sysErrorClass or 17; // identical entry already exists
  sysNotifyErrBroadcastBusy      = sysErrorClass or 19; // a broadcast is already in progress - try again later.
  sysNotifyErrBroadcastCancelled = sysErrorClass or 20; // a handler cancelled the broadcast

// AMX error codes continued - jb 10/20/98
  sysErrMbId    = sysErrorClass or 21;
  sysErrMbNone  = sysErrorClass or 22;
  sysErrMbBusy  = sysErrorClass or 23;
  sysErrMbFull  = sysErrorClass or 24;
  sysErrMbDepth = sysErrorClass or 25;
  sysErrMbEnv   = sysErrorClass or 26;

// NotifyMgr Phase #2 Error Codes:
  sysNotifyErrQueueFull    = sysErrorClass or 27; // deferred queue is full.
  sysNotifyErrQueueEmpty   = sysErrorClass or 28; // deferred queue is empty.
  sysNotifyErrNoStackSpace = sysErrorClass or 29; // not enough stack space for a broadcast
  sysErrNotInitialized     = sysErrorClass or 30; // manager is not initialized

// AMX error/warning codes continued - jed 9/10/99
  sysErrNotAsleep  = sysErrorClass or 31; // Task woken by SysTaskWake was not asleep, 1 wake pending
  sysErrNotAsleepN = sysErrorClass or 32; // Task woken by SysTaskWake was not asleep, >1 wake pending

// Power Manager error codes - soe, srj 9/19/00
  pwrErrNone      = pwrErrorClass or 0;
  pwrErrBacklight = pwrErrorClass or 1;
  pwrErrRadio     = pwrErrorClass or 2;
  pwrErrBeam      = pwrErrorClass or 3;
  pwrErrGeneric   = pwrErrorClass or 4;

(************************************************************
 * System Features
 *************************************************************)

  sysFtrCreator = sysFileCSystem; // Feature Creator

  sysFtrNumROMVersion = 1; // ROM Version
   // 0xMMmfsbbb, where MM is major version, m is minor version
   // f is bug fix, s is stage: 3-release,2-beta,1-alpha,0-development,
   // bbb is build number for non-releases
   // V1.12b3   would be: 0x01122003
   // V2.00a2   would be: 0x02001002
   // V1.01     would be: 0x01013000

  sysFtrNumProcessorID = 2; // Product id
   // 0xMMMMRRRR, where MMMM is the processor model and RRRR is the revision.
  sysFtrNumProcessorMask = $FFFF0000; // Mask to obtain processor model
  sysFtrNumProcessor328  = $00010000; // Motorola 68328   (Dragonball)
  sysFtrNumProcessorEZ   = $00020000; // Motorola 68EZ328 (Dragonball EZ)
  sysFtrNumProcessorVZ   = $00030000; // Motorola 68VZ328 (Dragonball VZ)
  sysFtrNumProductID     = sysFtrNumProcessorID; // old (obsolete) define

  sysFtrNumBacklight = 3; // Backlight
   // bit 0: 1 if present. 0 if Feature does not exist or backlight is not present

  sysFtrNumEncryption = 4; // Which encryption schemes are present
  sysFtrNumEncryptionMaskDES = $00000001; // bit 0: 1 if DES is present

  sysFtrNumCountry = 5;    // International ROM identifier
   // Result is of type CountryType as defined in Preferences.h.
   // Result is essentially the "default" country for this ROM.
   // Assume cUnitedStates if sysFtrNumROMVersion >= 02000000
   // and feature does not exist. Result is in low sixteen bits.

  sysFtrNumLanguage = 6;    // Language identifier
   // Result is of untyped; values are defined in Incs:BuildRules.h
   // Result is essentially the "default" language for this ROM.
   // This is new for the WorkPad (v2.0.2) and did NOT exist for any of the
   // following: GermanPersonal, GermanPro, FrenchPersonal, FrenchPro
   // Thus we can't really assume anything if the feature doesn't exist,
   // though the actual language MAY be determined from sysFtrNumCountry,
   // above. Result is in low sixteen bits.

  sysFtrNumDisplayDepth = 7;  // Display depth
   // Result is the "default" display depth for the screen.     (PalmOS 3.0)
   // This value is used by ScrDisplayMode when setting the default display depth.

  sysFtrNumHwrMiscFlags = 8;    // GHwrMiscFlags value   (PalmOS 3.1)
  sysFtrNumHwrMiscFlagsExt = 9; // GHwrMiscFlagsExt value  (PalmOS 3.1)

  sysFtrNumIntlMgr = 10;
   // Result is a set of flags that define functionality supported
   // by the Int'l Manager.               (PalmOS 3.1)

  sysFtrNumEncoding = 11;
   // Result is the character encoding (defined in PalmLocale.h) supported
   // by this ROM. If this feature doesn't exist then the assumed encoding
   // is Palmlatin (superset of Windows code page 1252).  (PalmOS 3.1)

  sysFtrDefaultFont = 12;
   // Default font ID used for displaying text.         (PalmOS 3.1)

  sysFtrDefaultBoldFont = 13;
   // Default font ID used for displaying bold text.       (PalmOS 3.1)

  sysFtrNumGremlinsSupportGlobals = 14; // Globals for supporting gremlins.
   // This value is a pointer to a memory location that stores global variables needed
   // for intelligently supporting gremlins.  Currently, it is only used in Progress.c.
   // It is only initialized on first use (gremlins and progress bar in combination)
   // when ERROR_CHECK_LEVEL == ERROR_CHECK_FULL.        (PalmOS 3.2)

  sysFtrNumVendor = 15;
   // Result is the vendor id, in the low sixteen bits.      (PalmOS 3.3)

  sysFtrNumCharEncodingFlags = 16;
   // Flags for a given character encoding, specified in TextMgr.h  (PalmOS 3.5)

  sysFtrNumNotifyMgrVersion = 17; // version of the NotifyMgr, if any  (PalmOS 3.5)

  sysFtrNumOEMROMVersion = 18; // Supplemental ROM version, provided by OEM
   // This value may be present in OEM devices, and is in the same format
   // as sysFtrNumROMVersion.               (PalmOS 3.5)

  sysFtrNumErrorCheckLevel = 19; // ROM build setting of ERROR_CHECK_LEVEL
   // May be set to ERROR_CHECK_NONE, ERROR_CHECK_PARTIAL, or ERROR_CHECK_FULL
   // as defined in <BuildDefines.h>.            (PalmOS 3.5)

  sysFtrNumOEMCompanyID        = 20; // GHwrOEMCompanyID value        (PalmOS 3.5)
  sysFtrNumOEMDeviceID         = 21; // GHwrOEMDeviceID value         (PalmOS 3.5)
  sysFtrNumOEMHALID            = 22; // GHwrOEMHALID value            (PalmOS 3.5)
  sysFtrNumDefaultCompression  = 23; // Default Clipper's compression (Palmos 3.5)
  sysFtrNumWinVersion          = 24; // Window version                (PalmOS 4.0)
  sysFtrNumAccessorTrapPresent = 25; // If accessor trap exists       (PalmOS 4.0)

(************************************************************
 * ROM token information (for SysGetROMToken, below)
 *************************************************************)

// Additional tokens and token information is located in <Hardware.h>
  sysROMTokenSnum = Rsc('snum'); // Memory Card Flash ID (serial number)

(************************************************************
 * Macros for extracting and combining ROM/OS version components
 *************************************************************)

// ROM/OS stage numbers
  sysROMStageDevelopment = 0;
  sysROMStageAlpha       = 1;
  sysROMStageBeta        = 2;
  sysROMStageRelease     = 3;

// MACRO: sysMakeROMVersion
//
// Builds a ROM version value from the major, minor, fix, stage, and build numbers
//

function sysMakeROMVersion(major, minor, fix, stage: UInt8; buildNum: UInt16): UInt32;


// Macros for parsing the ROM version number
// (the system OS version is obtained by calling
// FtrGet(sysFtrCreator, sysFtrNumROMVersion, dwOSVerP), where dwOSVerP is
// a pointer to to a UInt32 variable that is to receive the OS version number)

function sysGetROMVerMajor(dwROMVer: UInt32): UInt16;
function sysGetROMVerMinor(dwROMVer: UInt32): UInt16;
function sysGetROMVerFix(dwROMVer: UInt32): UInt16;
function sysGetROMVerStage(dwROMVer: UInt32): UInt16;
function sysGetROMVerBuild(dwROMVer: UInt32): UInt16;

(************************************************************
 * System Types
 *************************************************************)

// Types of batteries installed.
type
  SysBatteryKind = Enum;

const
  sysBatteryKindAlkaline=0;
  sysBatteryKindNiCad = Succ(sysBatteryKindAlkaline);
  sysBatteryKindLiIon = Succ(sysBatteryKindNiCad);
  sysBatteryKindRechAlk = Succ(sysBatteryKindLiIon);
  sysBatteryKindNiMH = Succ(sysBatteryKindRechAlk);
  sysBatteryKindLiIon1400 = Succ(sysBatteryKindNiMH);
  sysBatteryKindLast = $FF; // insert new battery types BEFORE this one

// Different battery states (output of hwrBattery)
type
  SysBatteryState = Enum;

const
  sysBatteryStateNormal = 0;
  sysBatteryStateLowBattery = Succ(sysBatteryStateNormal);
  sysBatteryStateCritBattery = Succ(sysBatteryStateLowBattery);
  sysBatteryStateShutdow = Succ(sysBatteryStateCritBattery);

// SysCreateDataBaseList can generate a list of database.
type
  SysDBListItemType = record
    name: array [0..dmDBNameLength-1] of Char;
    creator: UInt32;
    type_: UInt32;
    version: UInt16;
    dbID: LocalID;
    cardNo: UInt16;
    iconP: BitmapPtr;
  end;

// Structure of a generic message that can be send to a mailbox
// through the SysMailboxSend call. Note, this structure MUST
// be  CJ_MAXMSZ bytes large, where CJ_MAXMSZ is defined in
// the AMX includes.
  SysMailboxMsgType = record
    data: array [0..2] of UInt32;
  end;

// Constants used by the SysEvGroupSignal call
const
  sysEvGroupSignalConstant = 0;
  sysEvGroupSignalPulse    = 1;

// Constants used by the SysEvGroupWait call
  sysEvGroupWaitOR  = 0;
  sysEvGroupWaitAND = 1;

(************************************************************
 * System Pre-defined "file descriptors"
 * These are used by applications that use the  Net Library's
 *   NetLibSelect() call
 *************************************************************)

  sysFileDescStdIn  = 0;

//============================================================================
// jhl 7/26/00 Integrate HSIMgr functionality
//============================================================================
  sysNotifyHSISerialPortInUseEvent          = Rsc('hsiu'); // Sent when serial port is in use
  sysNotifyHSIPeripheralRespondedEvent      = Rsc('hspr'); // Sent with peripheral response
  sysNotifyHSIPeripheralNotRespondingEvent  = Rsc('hspn'); // Sent when peripheral does not respond
  sysNotifyHSINoConnectionEvent             = Rsc('ncon'); // Sent on VID of no connection
  sysNotifyHSIUSBCradleEvent                = sysPortUSBDesktop;                                             // Sent on VID of USB Cradle
  sysNotifyHSIRS232CradleEvent              = Rsc('rs2c'); // Sent on VID of RS232 Cradle
  sysNotifyHSIUSBPeripheralEvent            = sysPortUSBPeripheral;                                          // Sent on VID of USB Peripheral
  sysNotifyHSIRS232PeripheralEvent          = Rsc('rs2p'); // Sent on VID of RS232 Peripheral
  sysNotifyHSIDebugEvent                    = Rsc('dbug'); // Sent on VID of Debug

  sysMaxHSIResponseSize            = 64;
  sysHSISerialInquiryBaud          = 9600;
  sysHSISerialInquiryString        = 'ATI3\015\012';
  sysHSISerialInquiryStringLen     = 6;
  sysHSISerialInterChrTimeout      = 3;  // ticks (20-30 ms)
  sysHSISerialInquiryTimeout       = 11; // ticks (100-110 ms)

type
  SysHSIResponseType = record
    // "Voltage ID" from modem pin converted to 4 character VID
// VID: UInt32;
    // Actual voltage detected on modem VID pin
// mVolts: UInt16;
    // Character string received in response to inquiry string
    // (will be NUL terminated)
    responseBuffer: array [0..sysMaxHSIResponseSize-1] of Char;
    // Length of string in responseBuffer
    responseLength: UInt16;
  end;

(************************************************************
 * Function Prototypes
 *************************************************************)

// Prototype for Pilot applications entry point

// UInt32 PilotMain(UInt16 cmd, void *cmdPBP, UInt16 launchFlags);

// SystemMgr routines
procedure SysUnimplemented; syscall sysTrapSysUnimplemented;

procedure SysColdBoot(card0P: Pointer; card0Size: UInt32; card1P: Pointer;
                      card1Size, sysCardHeaderOffset: UInt32); syscall sysTrapSysColdBoot;

procedure SysInit; syscall sysTrapSysInit;

procedure SysReset; syscall sysTrapSysReset;

//procedure SysPowerOn(card0P: Pointer; card0Size: UInt32; card1P: Pointer;
//                     card1Size, sysCardHeaderOffset: UInt32; reFormat: Boolean); syscall sysTrapSysPowerOn;

procedure SysDoze(onlyNMI: Boolean); syscall sysTrapSysDoze;

function SysSetPerformance(var sysClockP: UInt32; var cpuDutyP: UInt16): Err; syscall sysTrapSysSetPerformance;

procedure SysSleep(untilReset, emergency: Boolean); syscall sysTrapSysSleep;

function SysSetAutoOffTime(seconds: UInt16): UInt16; syscall sysTrapSysSetAutoOffTime;

function SysTicksPerSecond: UInt16; syscall sysTrapSysTicksPerSecond;

function SysLaunchConsole: Err; syscall sysTrapSysLaunchConsole;

function SysHandleEvent(var eventP: EventType): Boolean; syscall sysTrapSysHandleEvent;

procedure SysUILaunch; syscall sysTrapSysUILaunch;

function SysUIAppSwitch(cardNo: UInt16; dbID: LocalID; cmd: UInt16; cmdPBP: MemPtr): Err; syscall sysTrapSysUIAppSwitch;

function SysCurAppDatabase(var cardNoP: UInt16; var dbIDP: LocalID): Err; syscall sysTrapSysCurAppDatabase;

function SysBroadcastActionCode(cmd: UInt16; cmdPBP: MemPtr): Err; syscall sysTrapSysBroadcastActionCode;

function SysAppLaunch(cardNo: UInt16; dbID: LocalID; launchFlags, cmd: UInt16;
                      cmdPBP: MemPtr; var resultP: UInt32): Err; syscall sysTrapSysAppLaunch;

function SysNewOwnerID: UInt16; syscall sysTrapSysNewOwnerID;

function SysSetA5(newValue: UInt32): UInt32; syscall sysTrapSysSetA5;

// Routines used by startup code
function SysAppStartup(var appInfoPP: SysAppInfoPtr; var prevGlobalsP, globalsPtrP: MemPtr): Err; syscall sysTrapSysAppStartup;

function SysAppExit(appInfoP: SysAppInfoPtr; prevGlobalsP, globalsP: MemPtr): Err; syscall sysTrapSysAppExit;

//#if EMULATION_LEVEL != EMULATION_NONE
// Simulator-specific routines
//MemPtr SysCardImageInfo(UInt16 cardNo, UInt32 *sizeP;
//
//void  SysCardImageDeleted(UInt16 cardNo;
//#endif  // EMULATION_LEVEL != EMULATION_NONE

function SysUIBusy(set_, value: Boolean): UInt16; syscall sysTrapSysUIBusy;

function SysLCDContrast(set_: Boolean; newContrastLevel: UInt8): UInt8; syscall sysTrapSysLCDContrast;

function SysLCDBrightness(set_: Boolean; newBrightnessLevel: UInt8): UInt8; syscall sysTrapSysLCDBrightness;

// System Dialogs
procedure SysBatteryDialog; syscall sysTrapSysBatteryDialog;

// Utilities
function SysSetTrapAddress(trapNum: UInt16; procP: Pointer): Err; syscall sysTrapSysSetTrapAddress;

function SysGetTrapAddress(trapNum: UInt16): Pointer; syscall sysTrapSysGetTrapAddress;

function SysDisableInts: UInt16; syscall sysTrapSysDisableInts;

procedure SysRestoreStatus(status: UInt16); syscall sysTrapSysRestoreStatus;

function SysGetOSVersionString: PChar; syscall sysTrapSysGetOSVersionString;

// The following trap is a public definition of HwrGetROMToken from <Hardware.h>
// See token definitions (like sysROMTokenSerial) above...

function SysGetROMToken(cardNo: UInt16; token: UInt32; var dataP: UInt8Ptr; var sizeP: UInt16): Err; syscall sysTrapHwrGetROMToken;


// Library Management
function SysLibInstall(libraryP: SysLibEntryProcPtr; var refNumP: UInt16): Err; syscall sysTrapSysLibInstall;

function SysLibLoad(libType, libCreator: UInt32; var refNumP: UInt16): Err; syscall sysTrapSysLibLoad;

function SysLibRemove(refNum: UInt16): Err; syscall sysTrapSysLibRemove;

function SysLibFind(const nameP: PChar; var refNumP: UInt16): Err; syscall sysTrapSysLibFind;

function SysLibTblEntry(refNum: UInt16): SysLibTblEntryPtr; syscall sysTrapSysLibTblEntry;

// Generic Library calls
function SysLibOpen(refNum: UInt16): Err; syscall sysLibTrapOpen;
function SysLibClose(refNum: UInt16): Err; syscall sysLibTrapClose;
function SysLibSleep(refNum: UInt16): Err; syscall sysLibTrapSleep;
function SysLibWake(refNum: UInt16): Err; syscall sysLibTrapWake;

//-----------------------------------------------------
// Kernel Prototypes
//-----------------------------------------------------

// Task Creation and deleation
function SysTranslateKernelErr(err: Err): Err; syscall sysTrapSysTranslateKernelErr;

function SysTaskCreate(var taskIDP, creator: UInt32; codeP: ProcPtr; stackP: MemPtr;
                       stackSize, attr, priority, tSlice: UInt32): Err; syscall sysTrapSysTaskCreate;

function SysTaskDelete(taskID, priority: UInt32): Err; syscall sysTrapSysTaskDelete;

function SysTaskTrigger(taskID: UInt32): Err; syscall sysTrapSysTaskTrigger;

function SysTaskID: UInt32; syscall sysTrapSysTaskID;

function SysTaskDelay(delay: Int32): Err; syscall sysTrapSysTaskDelay;

function SysTaskSetTermProc(taskID: UInt32; termProcP: SysTermProcPtr): Err; syscall sysTrapSysTaskSetTermProc;

function SysTaskSwitching(enable: Boolean): Err; syscall sysTrapSysTaskSwitching;

function SysTaskWait(timeout: Int32): Err; syscall sysTrapSysTaskWait;

function SysTaskWake(taskID: UInt32): Err; syscall sysTrapSysTaskWake;

procedure SysTaskWaitClr; syscall sysTrapSysTaskWaitClr;

function SysTaskSuspend(taskID: UInt32): Err; syscall sysTrapSysTaskSuspend;

function SysTaskResume(taskID: UInt32): Err; syscall sysTrapSysTaskResume;

// Counting Semaphores
function SysSemaphoreCreate(var smIDP, tagP: UInt32; initValue: Int32): Err; syscall sysTrapSysSemaphoreCreate;

function SysSemaphoreDelete(smID: UInt32): Err; syscall sysTrapSysSemaphoreDelete;

function SysSemaphoreWait(smID, priority: UInt32; timeout: Int32): Err; syscall sysTrapSysSemaphoreWait;

function SysSemaphoreSignal(smID: UInt32): Err; syscall sysTrapSysSemaphoreSignal;

function SysSemaphoreSet(smID: UInt32): Err; syscall sysTrapSysSemaphoreSet;

// Resource Semaphores
function SysResSemaphoreCreate(var smIDP, tagP: UInt32): Err; syscall sysTrapSysResSemaphoreCreate;

function SysResSemaphoreDelete(smID: UInt32): Err; syscall sysTrapSysResSemaphoreDelete;

function SysResSemaphoreReserve(smID, priority: UInt32; timeout: Int32): Err; syscall sysTrapSysResSemaphoreReserve;

function SysResSemaphoreRelease(smID: UInt32): Err; syscall sysTrapSysResSemaphoreRelease;

// Timers
function SysTimerCreate(var timerIDP, tagP: UInt32; timerProc: SysTimerProcPtr;
                        periodicDelay, param: UInt32): Err; syscall sysTrapSysTimerCreate;

function SysTimerDelete(timerID: UInt32): Err; syscall sysTrapSysTimerDelete;

function SysTimerWrite(timerID, value: UInt32): Err; syscall sysTrapSysTimerWrite;

function SysTimerRead(timerID: UInt32; var valueP: UInt32): Err; syscall sysTrapSysTimerRead;

// Information
function SysKernelInfo(paramP: Pointer): Err; syscall sysTrapSysKernelInfo;

function SysCreateDataBaseList(type_, creator: UInt32; var dbCount: UInt16;
                               var dbIDs: MemHandle; lookupName: Boolean): Boolean; syscall sysTrapSysCreateDataBaseList;

function SysCreatePanelList(var panelCount: UInt16; var panelIDs: MemHandle): Boolean; syscall sysTrapSysCreatePanelList;

function SysBatteryInfo(set_: Boolean; var warnThresholdP, criticalThresholdP: UInt16;
                        var maxTicksP: Int16; var kindP: SysBatteryKind; var pluggedIn: Boolean;
                        var percentP: UInt8): UInt16; syscall sysTrapSysBatteryInfo;

function SysBatteryInfoV20(set_: Boolean; warnThresholdP, criticalThresholdP: UInt16;
                           var maxTicksP: Int16; var kindP: SysBatteryKind; var pluggedIn: Boolean): UInt16; syscall sysTrapSysBatteryInfoV20;

function SysGetStackInfo(var startPP, endPP: MemPtr): Boolean; syscall sysTrapSysGetStackInfo;

// Mailboxes
function SysMailboxCreate(var mbIDP, tagP: UInt32; depth: UInt32): Err; syscall sysTrapSysMailboxCreate;

function SysMailboxDelete(mbID: UInt32): Err; syscall sysTrapSysMailboxDelete;

function SysMailboxFlush(mbID: UInt32): Err; syscall sysTrapSysMailboxFlush;

function SysMailboxSend(mbID: UInt32; msgP: Pointer; wAck: UInt32): Err; syscall sysTrapSysMailboxSend;

function SysMailboxWait(mbID: UInt32; msgP: Pointer; priority: UInt32; timeout: Int32): Err; syscall sysTrapSysMailboxWait;

// Event Groups
function SysEvGroupCreate(var evIDP, tagP: UInt32; init: UInt32): Err; syscall sysTrapSysEvGroupCreate;

//Err  SysEvGroupDelete(UInt32 evID)  // save trap table space - don't need
//; syscall sysTrapSysEvGroupDelete;

function SysEvGroupSignal(evID, mask, value: UInt32; type_: Int32): Err; syscall sysTrapSysEvGroupSignal;

function SysEvGroupRead(evID: UInt32; var valueP: UInt32): Err; syscall sysTrapSysEvGroupRead;

function SysEvGroupWait(evID, mask, value: UInt32; matchType, timeout: Int32): Err; syscall sysTrapSysEvGroupWait;

(************************************************************
 * Assembly Function Prototypes
 *************************************************************)

//#define _SysSemaphoreSignal ASM_; syscall sysTrapSysSemaphoreSignal

//#define _SysSemaphoreSet ASM_; syscall sysTrapSysSemaphoreSet

//#define _SysDoze ASM_; syscall sysTrapSysDoze

implementation

function sysMakeROMVersion(major, minor, fix, stage: UInt8; buildNum: UInt16): UInt32;
begin
  sysMakeROMVersion :=
         ((major and $0FF) shl 24) or
             ((minor and $00F) shl 20) or
             ((fix   and $00F) shl 16) or
             ((stage and $00F) shl 12) or
             (buildNum and $0FFF);
end;

function sysGetROMVerMajor(dwROMVer: UInt32): UInt16;
begin
  sysGetROMVerMajor := (dwROMVer shr 24) and $00FF;
end;

function sysGetROMVerMinor(dwROMVer: UInt32): UInt16;
begin
  sysGetROMVerMinor := (dwROMVer shr 20) and $000F;
end;

function sysGetROMVerFix(dwROMVer: UInt32): UInt16;
begin
  sysGetROMVerFix := (dwROMVer shr 16) and $000F;
end;

function sysGetROMVerStage(dwROMVer: UInt32): UInt16;
begin
  sysGetROMVerStage := (dwROMVer shr 12) and $000F;
end;

function sysGetROMVerBuild(dwROMVer: UInt32): UInt16;
begin
  sysGetROMVerBuild := dwROMVer and $0FFF;
end;

end.
