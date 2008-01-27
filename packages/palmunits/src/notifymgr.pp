{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: NotifyMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Notification Manager
 *
 * History:
 *    6/15/98  JED - Created by Jesse Donaldson
 *    12/23/99 jmp - Fix <> vs. "" problem.
 *    10/19/00 PPL - Change cncNotifyCurrentConfigurationChange 'conf'
 *                   to cncNotifyProfileEvent 'cncp'
 *
 *****************************************************************************)

unit notifymgr;

interface

uses palmos, coretraps, datamgr, localemgr, systemresources;

type
  SysNotifyParamType = record
    notifyType:     UInt32;    // What type of event occurred?
    broadcaster:    UInt32;    // normally creator code of broadcasting app
    notifyDetailsP: Pointer;   // ptr to notification-specific data, if any
    userDataP:      Pointer;   // user specified ptr passed back with notification
    handled:        Boolean;   // true if event is handled yet
    reserved2:      UInt8;
  end;

type
  SysNotifyProc = function (var notifyParamsP: SysNotifyParamType): Err;
  SysNotifyProcPtr = SysNotifyProc;

const
  sysNotifyNormalPriority   = 0;              // clients should use this priority

  sysNotifyBroadcasterCode  = sysFileCSystem; // broadcaster code for events broadcast by the system

  sysNotifyNoDatabaseID     = $FFFFFFFF;      // for system use only!!!

  sysNotifyVersionNum       = 1;              // version of the NotifyMgr, obtained from the feature

// The queue is much smaller in debug builds so that we aren't tempted to overflow it
// (and larger for release builds so that when we do, it will work).

  sysNotifyDefaultQueueSize = 30;

(*
   Notify Manager Events:

   Note that in general, for notifications regarding the creation of information
   (e.g. sysNotifyDBAddedEvent) the broadcast goes out AFTER the fact (e.g.,
   after the database has been created) and that notifications regarding
   the deletion of information are broadcast BEFORE the fact.  A notable and
   unfortunate exception to this rule is the database deleted notification, which
   is sent out after the fact.

   The sleep & wake notifications require a little special attention.  These
   notifications are _not_ guaranteed to always be broadcast, and hence are unsuitable
   for applications where syscall hardware must be shut off to conserve power when
   we go to sleep.  For example, the sleep notification will not be broadcast when
   we do an emergency  shutdown due to low battery conditions.  Also note that any
   sort of prolonged activity (especially displaying UI sich as a dialog) in response
   to these noticfications can complicate things immensely.  UI is very tricky because
   these notifications are broadcast from SysHandleEvent.  The result is that you may
   receive more than one sleep notification if one of the notification clients puts up
   a dialog or something, and it remains there long enough for another AutoOff event
   to come through.  There are also possible problems with reentrancy and stack space
   usage since the notification is broadcast from SysHandleEvent inside an application's
   event loop.
*)

  sysNotifySyncStartEvent          = Rsc('hots');
                                     // Sent at the start of a hotsync.
                                     // Always sent from UI thread.
                                     // param: none

  sysNotifySyncFinishEvent         = Rsc('sync');
                                     // Sent at the end of a hotsync.
                                     // Always sent from UI thread.
                                     // param: none

  sysNotifyAntennaRaisedEvent      = Rsc('tena');
                                     // Sent when the antenna is raised on a
                                     // Palm VII series device.
                                     // Always sent from UI thread.
                                     // param: none

  sysNotifyResetFinishedEvent      = Rsc('rstf');
                                     // Broadcast after all app launchcodes
                                     // are sent after a reset.
                                     // Always sent from UI thread.
                                     // param: none

  sysNotifyForgotPasswordEvent     = Rsc('bozo');
                                     // Broadcast when the user presses the
                                     // 'Forgotten Password' button in the
                                     // security app, just before every database's
                                     // private records are deleted.

  sysNotifyTimeChangeEvent         = Rsc('time');
                                     // Broadcast when the time is changed by the user.
                                     // param: Int32*, ptr to time change delta in
                                     // seconds (e.g., a value of 100 means that the
                                     // user set the clock ahead 100 seconds).

  sysNotifySleepRequestEvent       = Rsc('slpq');
                                     // A "Sleep Request Event" is broadcast when the device
                                     // is about to go to sleep, and is a chance for clients
                                     // to do perform an action or even delay going to sleep
                                     // for a little while.  This may be broadcast multiple
                                     // times if one or more clients defer the sleep. Parameter
                                     // is a pointer to a SleepEventParamType.  See below
                                     // details on this structure. Note that this event is
                                     // NOT guaranteed to be broadcast.  See the big comment
                                     // at the top of this list for important detials & warnings.
                                     // Always sent from UI thread.

  sysNotifySleepNotifyEvent        = Rsc('slp!');
                                     // A "Sleep Notify Event" is broadcast when the device is
                                     // definitely going to sleep as soon as the broadcast
                                     // is finished.  Parameter is unused.
                                     // Note that this event is NOT guaranteed to be broadcast.
                                     // See the big comment at the top of this list for
                                     // important detials & warnings.
                                     // Always sent from UI thread.

  sysNotifyEarlyWakeupEvent        = Rsc('worm');
                                     // ...the early bird gets the worm...
                                     // Broadcast just after the device wakes up
                                     // at the early stage where the screen may
                                     // still be turned off and we may quickly go
                                     // back to sleep after handling an alarm or
                                     // charger event.
                                     // Always sent from UI thread.

  sysNotifyLateWakeupEvent         = Rsc('lazy');
                                     // Broadcast after the device wakes up
                                     // at the later stage of the wakeup
                                     // process after we turn the screen on,
                                     // broadcast from EvtResetAutoOffEvent
                                     // the first time it is called implying
                                     // that the device will remain awake
                                     // for at least a little while.
                                     // Always sent from UI thread.

  sysNotifyDisplayChangeEvent      = Rsc('scrd');
                                     // Sent when the display depth is
                                     // changed, notifyDetailsP has old/new depth
                                     // see SysNotifyDisplayChangeDetailsType

  sysNotifyMenuCmdBarOpenEvent     = Rsc('cbar');
                                     // Sent by FormHandleEvent when a menuCmdBarOpenEvent
                                     // passes through. The system will not open the toolbar
                                     // if the 'handled' field is set in the parameter block,
                                     // so most clients should not set it.  The notification
                                     // is provided so that Hack-like entities can catch
                                     // it and add their own buttons to the bar. These
                                     // buttons will show up to the left of cut/copy/paste/undo and
                                     // to the right of everything else. Without this
                                     // notification, people would hack SysHandleEvent... ick.

  cncNotifyProfileEvent            = Rsc('cncp');
                                     // sent by the Connection Panel to inform its clients
                                     // that the current configuration has change
                                     // void * notifyDetailsP : has to be casted as UInt32
                                     // to get the Broadcasted Profile ID user specified ptr
                                     // passed back with notification
                                     // void * userDataP : has to be casted as UInt16
                                     // to get the device kind of the Broadcasted Profile ID

  syssyscallConnectorAttachEvent  = Rsc('ecna');
                                     // Broadcast when anything is attached to
                                     // the syscall connector.

  syssyscallConnectorDetachEvent  = Rsc('ecnd');
                                     // Broadcast when anything is detached from
                                     // the syscall connector.

  sysNotifyCardInsertedEvent       = Rsc('crdi');
                                     // Broadcast when an ExpansionMgr card is
                                     // inserted into a slot, and the slot driver
                                     // calls ExpCardInserted.  Always broadcast
                                     // from UI task.
                                     // ExpansionMgr will play a sound & attempt to
                                     // mount a volume unless the corresponding
                                     // bits in the 'handled' field are set by a
                                     // notification handler (see ExpansionMgr.h).
                                     // PARAMETER: slot number cast as void*

  sysNotifyCardRemovedEvent        = Rsc('crdo');
                                     // Broadcast when an ExpansionMgr card is
                                     // removed from a slot, and the slot driver
                                     // calls ExpCardRemoved.  Always broadcast
                                     // from UI task.
                                     // ExpansionMgr will play a sound & attempt to
                                     // unmount a volume unless the corresponding
                                     // bits in the 'handled' field are set by a
                                     // notification handler (see ExpansionMgr.h).
                                     // PARAMETER: slot number cast as void*

  sysNotifyVolumeMountedEvent      = Rsc('volm');
                                     // Broadcast when a VFSMgr volume is
                                     // mounted, Always broadcast from UI task.
                                     // VFSMgr will run start.prc (if present),
                                     // and SysUIAppSwitch  to it or the Launcher
                                     // unless the appropriate bits in the 'handled'
                                     // field are set by a notification handler.
                                     // PARAMETER: VFSAnyMountParamPtr cast as void*

  sysNotifyVolumeUnmountedEvent    = Rsc('volu');
                                     // Broadcast AFTER a VFSMgr volume is
                                     // unmounted, Always broadcast from UI task.
                                     // VFSMgr will delete start.prc (if it was loaded).
                                     // The volume ref number for the unmounted volume is
                                     // NO LONGER VALID, so don't bother trying to use it.
                                     // PARAMETER: volume refNum cast as void*

  sysNotifyHelperEvent             = Rsc('hlpr');
                                     // Sent by Address Book (or any
                                     // 3rd party application) to
                                     // communicate with Phone Apps

  sysNotifyPOSEMountEvent          = Rsc('pose');
                                     // Sent by HostFS to communicate with itself,
                                     // saving on stack depth.

  sysNotifyLocaleChangedEvent      = Rsc('locc');
                                     // Notify apps/panel that the system locale
                                     // has changed.
                                     // This is broadcast by the language picker after it
                                     // has changed the locale.
                                     // Always sent from UI thread.
                                     // param: ptr to SysNotifyLocaleChangedType

  sysNotifyRetryEnqueueKey         = Rsc('retk');
                                     // An EvtEnqueueKey failed, so retry.

  sysNotifyGotUsersAttention       = Rsc('attn');
                                     // Got users attention.

  sysNotifyDBDeletedEvent          = Rsc('dbs-');
                                     // broadcast AFTER a database is removed from the device.
                                     // Note that the database ID in the parameter block is
                                     // NO LONGER VALID, and you WILL CRASH the device if you
                                     // try to pass it to any DataMgr routines.
                                     // notifyDetailsP: ptr to SysNotifyDBDeletedType.

  sysNotifyDeviceUnlocked          = Rsc('unlk');
                                     // Broadcasted by the Security app After the device is
                                     //unlocked.
                                     //notifyDetailsP: none

  sysNotifyPhoneEvent              = Rsc('fone');
                                     // Sent by third-party applications to communicate
                                     // with built-in or syscall phones.
                                     // NOTE THAT THIS EVENT IS FOR FUTURE USE

  sysNotifyNetLibIFMediaEvent      = Rsc('neti');
                                     // Broadcasted by NetLib to communicate its interfaces media Up and Down requests
                                     // to registered Apps.
                                     // notifyDetailsP: ptr to SysNotifyNetLibIFMediaType.

  sysNotifyDeleteProtectedEvent    = Rsc('-pdb');
                                     // Broadcast BEFORE a database is removed from the device when a user
                                     // or installer application wants to remove a protected database.  This
                                     // will ONLY be issued from the UI task and should be issued before each
                                     // user driven deletion that targets a protected database that the app
                                     // running does not own (i.e. needs to request be unprotected).
                                     // This gives the unlucky client a chance to do any required
                                     // cleanup work.  Note that if an application has multiple protected
                                     // databases, this notification may be sent out more than once.
                                     // notifyDetailsP: ptr to SysNotifyDBInfoType.

  sysNotifyIrDASniffEvent          = Rsc('irda');
                                     // Sent by IrDA sniffing code when an incomming IrDA frame is detected

// for sysNotifyDisplayChangeEvent
type
  SysNotifyDisplayChangeDetailsTag = record
    oldDepth: UInt32;
    newDepth: UInt32;
 end;
  SysNotifyDisplayChangeDetailsType = SysNotifyDisplayChangeDetailsTag;

// For sysNotifyLocaleChangedEvent
type
  SysNotifyLocaleChangedTag = record
    oldLocale: LmLocaleType;
    newLocale: LmLocaleType;
  end;
  SysNotifyLocaleChangedType = SysNotifyLocaleChangedTag;

// Possible values for the sleep-reason for the sysNotifySleepEvent
const
  sysSleepPowerButton = 0;
  sysSleepAutoOff     = 1;
  sysSleepResumed     = 2;
  sysSleepUnknown     = 3;

// for sysNotifySleepRequestEvent
type
  SleepEventParamTag = record
    reason: UInt16;     // contains the reason we are going to sleep. See above list for possible values.
    deferSleep: UInt16; // Only used for the sysNotifySleepRequestEvent, this should be incremented by
                        // clients that want to defer the sleep.  They are then responsible for enqueing
                        // a resumeSleepChr in the event queue.
  end;
  SleepEventParamType = SleepEventParamTag;

// for sysNotifyDeleteProtectedEvent, and possibly others...
type
  SysNotifyDBInfoTag = record
    dbID: LocalID;         // database ID of dabatase
    cardNo: UInt16;        // card number that dabatase resides on
    attributes: UInt16;    // database attributes
    dbName: array [0..dmDBNameLength-1] of Char; // name of database
    creator: UInt32;       // creator code of dabatase
    type_: UInt32;         // type of dabatase
  end;
  SysNotifyDBInfoType = SysNotifyDBInfoTag;

// for sysNotifyDBDeletedEvent
type
  SysNotifyDBDeletedTag = record
    oldDBID: LocalID;      // old database ID of dabatase, no longer valid
    cardNo: UInt16;        // card number that dabatase resides on
    attributes: UInt16;    // database attributes
    dbName: array [0..dmDBNameLength-1] of Char; // name of database
    creator: UInt32;       // creator code of dabatase
    type_: UInt32;         // type code of dabatase
  end;
  SysNotifyDBDeletedType = SysNotifyDBDeletedTag;

(*
Possible event types to be added in the future:
NOTE that these are NOT implemented yet!!!!

 sysNotifyDBAddedEvent          = Rsc('dbs+');
                                 // Broadcast after a database is added to the device
                                 // param: ptr to SysNotifyDBInfoType.

 sysNotifyInitializeEvent       = Ord('helo');
                                 // broadcast to an app after its installed
                                 // so it can do any necessary initalization
                                 // This event is always broadcast.
                                 // param: ptr to SysNotifyDBInfoType.

 sysNotifyCleanupEvent          = Ord('gbye');
                                 // broadcast to an app just before its deleted
                                 // so it can do any necessary cleanup work.
                                 // This event is always broadcast.
                                 // The database's type will have been set to
                                 // sysFileTTemp so that if case the handler
                                 // crashes, the DB will be deleted on reset.
                                 // param: ptr to SysNotifyDBInfoType.

 sysNotifyCardChangedEvent      = Ord('card');
                                 // broadcast when the owner's business card
                                 // has changed, allowing apps to adjust for that.
                                 // param: ptr to new owner data.
*)

function SysNotifyRegister(cardNo: UInt16; dbID: LocalID; notifyType: UInt32;
                           callbackP: SysNotifyProcPtr; priority: Int8; userDataP: Pointer): Err; syscall sysTrapSysNotifyRegister;

function SysNotifyUnregister(cardNo: UInt16; dbID: LocalID; notifyType: UInt32; priority: Int8): Err; syscall sysTrapSysNotifyUnregister;

(*
 * WARNING: Never call SysNotifyBroadcast from a background task
 *       (or from any code that might be called from a BG task)
 *       with the memory semaphore reserved.  Deadlock will
 *       result when the broadcast is deferred and the UI task
 *       tries to acquire the mem semaphore in order to send it out.
 *)
function SysNotifyBroadcast(var notify: SysNotifyParamType): Err; syscall sysTrapSysNotifyBroadcast;

function SysNotifyBroadcastDeferred(var notify: SysNotifyParamType; paramSize: Int16): Err; syscall sysTrapSysNotifyBroadcastDeferred;

// Kick off a notification from an interrupt handler.
// Note: non-interrupt code should use SysNotifyBroadcast
function SysNotifyBroadcastFromInterrupt(notifyType: UInt32; broadcaster: UInt32;
                                         notifyDetailsP: Pointer): Err; syscall sysTrapSysNotifyBroadcastFromInterrupt;

implementation

end.
