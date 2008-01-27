{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: AlarmMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Alarm Manager
 *
 * History:
 *    4/11/95  VMK - Created by Vitaly Kruglikov
 *
 *****************************************************************************)

unit alarmmgr;

interface

uses  palmos, coretraps, errorbase;

(************************************************************
 * Alarm Manager result codes
 * (almErrorClass is defined in ErrorBase)
 *************************************************************)

const
  almErrMemory = almErrorClass or 1; // ran out of memory
  almErrFull   = almErrorClass or 2; // alarm table is full

(********************************************************************
 * Alarm Manager Structures
 ********************************************************************)

// Structure passed with the sysAppLaunchCmdAlarmTriggered action code:
//
// This is a notification that an alarm set by the creator has
// gone off.  The action code handler should not do anything time-
// consuming here.  The intended use is to set the next alarm and/or
// to perform some quick maintenance task.  Particularly, this action code
// handler is not allowed to display any UI(dialogs, etc.) -- this would delay
// notification for alarms set by others.  This action code may be ignored.

type
  SysAlarmTriggeredParamType = record
    ref: UInt32;          // --> alarm reference value passed by caller;
    alarmSeconds: UInt32; // --> alarm date/time in seconds since 1/1/1904;

    purgeAlarm: Boolean;  // <-- if set to true on return, this alarm
                          // will be removed from the alarm table and the
                          // display notification will NOT be generated for it
    padding: UInt8;
  end;

// Structure passed with the sysAppLaunchCmdDisplayAlarm action code:
//
// This is a notification to display an alarm.  This action code
// will be called sometime after the app receives a sysAppLaunchCmdAlarmTriggered
// notification(perhaps after a significant delay).  It is possible that this
// notification will not be sent at all in the event the alarm table becomes full and
// the alarm table entry is used to hold a new alarm (this does NOT apply to the
// sysAppLaunchCmdAlarmTriggered notification).  This action code may be ignored.

  SysDisplayAlarmParamType = record
    ref: UInt32;          // alarm reference value passed by caller;
    alarmSeconds: UInt32; // alarm date/time in seconds since 1/1/1904;
    soundAlarm: Boolean;  // non-zero if alarm needs to be sounded;
    padding: UInt8;
  end;

(************************************************************
 * <chg 4-1-98 RM>
 *
 * New PalmOS 3.2 support for procedure alarms. These alarms
 *  are designed to call a procedure pointer rather than send
 *  an action code to an application.
 *
 * They are set using the AlmSetProcAlarm() macro. The caller
 * passes a pointer to a procedure of type AlmAlarmProc and
 * this procedure will be called when the alarm goes off.
 *
 * When the alarm fires, the alarm proc will be called with
 *  an almProcCmd of almProcCmdTriggered and paramP containing
 * to the alarm parameters.
 *
 * When a system time or date change occurs, the alarm proc will
 * be called with a almProcCmdReschedule cmd. The alarm proc should
 * reschedule itself at this time using AlmSetProcAlarm().
 *
 * The almProcCmd's at almProcCmdCustom are available for custom
 * use by the alarm proc as it sees fit.
 *
 *************************************************************)

type
  AlmProcCmdEnum = WordEnum;

const
  almProcCmdTriggered = 0; // Alarm triggered
  almProcCmdReschedule = Succ(almProcCmdTriggered); // Reschedule (usually as a result of time change)

  // Alarm manager reserves all enums up to almProcCmdCustom
  almProcCmdCustom = $8000;

type
  AlmAlarmProc = procedure({AlmProcCmdEnum} almProcCmd: UInt16; var paramP: SysAlarmTriggeredParamType);

const
  almProcAlarmCardNo = $8000; // passed in cardNo to AlmSetAlarm
                              //  and AlmGetAlarm

(********************************************************************
 * Alarm Manager Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

//-------------------------------------------------------------------
// Initialization
//-------------------------------------------------------------------

//
// ISSUES:
//      1.  Is the Alarms Database always on Card 0 ?
//
//      A: We will store alarm info on the dynamic heap.  Upon reset and
//          time change, apps will be notified via action code and will re-
//          submit their alarms.
//
//      2.  Should a semaphore be used by the Alarm Manager ?
//
//      A:  No.  Present implementation does not require it.  May add one
//          in the future to ensure data integrity between tasks.
//
//      3.  Pilot will need to go back to sleep even if the alarms dialog box is
//          not closed after some interval.
//
//      A:  This will happen in GetNextEvent.
//
//      4.  We will need to sound the alarm for all newly triggered alarms
//          even while another alarm dialog box is on-screen.
//
//      A:  Yes.  We will keep a flag in our globals to indicate when the
//          alarm manager is displaying an alarm.  This way we do not hog
//          stack and dynamic heap memory with additional alarm boxes.
//
//      5.  Should the alarm dialog box be system-modal ?
//
//      A:  Yes -- by swallowing the "QUIT" (and/or others) message in the alarm dialog's
//          event loop.
//


// AlmInit()
//
// Initializes the Alarm Manager.
//
// Create the Alarm Globals.
//

function AlmInit: Err; syscall sysTrapAlmInit;

//-------------------------------------------------------------------
// API
//-------------------------------------------------------------------

// AlmSetAlarm()
//
// Sets an alarm for the given application.  If an alarm for that
// application had been previously set, it will be replaced.  Passing
// a zero for alarmSeconds cancels the current alarm for the application.
//

function AlmSetAlarm(cardNo: UInt16; dbID: LocalID; ref, alarmSeconds: UInt32; quiet: Boolean): Err; syscall sysTrapAlmSetAlarm;

// AlmGetAlarm()
//
// Gets the alarm seconds for a given app.
// Zero is returned if there is no alarm setting for the app.

function AlmGetAlarm(cardNo: UInt16; dbID: LocalID; var refP: UInt32): UInt32; syscall sysTrapAlmGetAlarm;

// AlmEnableNotification
//
// Enables/disables Alarm Manager's notification mechanism.  For example,
// the HotSync application disables Alarm notifications during the sync
// to ensure that apps do not try to access their data database until
// the DesktopLink server had a chance to notify the apps whose databases
// were modified during the session.  This also prevents the alarm dialogs from
// blocking the HotSync UI.  A call to disable MUST always
// precede the call to enable.
//

procedure AlmEnableNotification(enable: Boolean); syscall sysTrapAlmEnableNotification;

// AlmDisplayAlarm()
//
// Displays any alarms that have gone off.
//
// This function is called by the Event Manager executing on some app's
// thread.  This permits us to access resources and execute system calls
// which would not be possible at interrupt time.
//  12/8/98 jb  Added return code.

function AlmDisplayAlarm(okToDisplay: Boolean): Boolean; syscall sysTrapAlmDisplayAlarm;

// AlmCancelAll()
//
// Cancels all alarms managed by the Alarm Manager.  This
// function is presently called by the Time Manager to cancel all alarms
// when the user changes date/time.
//

procedure AlmCancelAll; syscall sysTrapAlmCancelAll;

// AlmAlarmCallback()
//
// This function is called at interrupt time by the Time Manager when
// an alarm goes off.
//

procedure AlmAlarmCallback; syscall sysTrapAlmAlarmCallback;

// AlmTimeChange()
//
// This function gets called by TimSetSeconds() and gives the alarm manager
//  a chance to notify all procedure alarms of the time change.
//

procedure AlmTimeChange; syscall sysTrapAlmTimeChange;

procedure AlmSetProcAlarm(procP: AlmAlarmProc; ref, alarmSeconds: UInt32);

function AlmGetProcAlarm(procP: AlmAlarmProc; var refP: UInt32): UInt32;

implementation

// macros

procedure AlmSetProcAlarm(procP: AlmAlarmProc; ref, alarmSeconds: UInt32);
begin
  AlmSetAlarm(almProcAlarmCardNo, LocalID(procP), ref, alarmSeconds, True);
end;

function AlmGetProcAlarm(procP: AlmAlarmProc; var refP: UInt32): UInt32;
begin
  AlmGetProcAlarm := AlmGetAlarm(almProcAlarmCardNo, LocalID(procP), refP);
end;

end.
