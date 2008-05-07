{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007-2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Automatically converted by H2Pas 1.0.0 from notify.h
  The following command line parameters were used:
    -d
    -c
    -w
    notify.h
}

unit notify;

interface

uses windows;

{$calling cdecl}

const
   NOTIFICATION_EVENT_NONE = 0;   
   NOTIFICATION_EVENT_TIME_CHANGE = 1;   
   NOTIFICATION_EVENT_SYNC_END = 2;   
   NOTIFICATION_EVENT_ON_AC_POWER = 3;   
   NOTIFICATION_EVENT_OFF_AC_POWER = 4;   
   NOTIFICATION_EVENT_NET_CONNECT = 5;   
   NOTIFICATION_EVENT_NET_DISCONNECT = 6;   
   NOTIFICATION_EVENT_DEVICE_CHANGE = 7;   
   NOTIFICATION_EVENT_IR_DISCOVERED = 8;   
   NOTIFICATION_EVENT_RS232_DETECTED = 9;   
   NOTIFICATION_EVENT_RESTORE_END = 10;   
   NOTIFICATION_EVENT_WAKEUP = 11;   
   NOTIFICATION_EVENT_TZ_CHANGE = 12;   
   NOTIFICATION_EVENT_MACHINE_NAME_CHANGE = 13;   
   NOTIFICATION_EVENT_LAST = NOTIFICATION_EVENT_MACHINE_NAME_CHANGE;   
{
 * String passed on the command line when an app is run as the result
 * of a call to CeRunAppAtTime().
  }
   APP_RUN_AT_TIME = 'AppRunAtTime';   
{
 * Prefix of the command line when the user requests to run the application
 * that "owns" a notification.  It is followed by a space, and the
 * stringized version of the notification handle.
  }
   APP_RUN_TO_HANDLE_NOTIFICATION = 'AppRunToHandleNotification';   
{
 * Strings passed on the command line when an event occurs that the
 * app has requested via CeRunAppAtEvent.  Note that some of these
 * strings will be used as the command line *prefix*, since the rest
 * of the command line will be used as a parameter.
  }
   APP_RUN_AFTER_TIME_CHANGE = 'AppRunAfterTimeChange';   
   APP_RUN_AFTER_SYNC = 'AppRunAfterSync';   
   APP_RUN_AT_AC_POWER_ON = 'AppRunAtAcPowerOn';   
   APP_RUN_AT_AC_POWER_OFF = 'AppRunAtAcPowerOff';   
   APP_RUN_AT_NET_CONNECT = 'AppRunAtNetConnect';   
   APP_RUN_AT_NET_DISCONNECT = 'AppRunAtNetDisconnect';   
   APP_RUN_AT_DEVICE_CHANGE = 'AppRunDeviceChange';   
   APP_RUN_AT_IR_DISCOVERY = 'AppRunAtIrDiscovery';   
   APP_RUN_AT_RS232_DETECT = 'AppRunAtRs232Detect';   
   APP_RUN_AFTER_RESTORE = 'AppRunAfterRestore';   
   APP_RUN_AFTER_WAKEUP = 'AppRunAfterWakeup';   
   APP_RUN_AFTER_TZ_CHANGE = 'AppRunAfterTzChange';   
   APP_RUN_AFTER_EXTENDED_EVENT = 'AppRunAfterExtendedEvent';   
{
 * Strings passed on the end of the command line for the event,
 * NOTIFICATION_EVENT_DEVICE_CHANGE.  The general form will be
 * "/op devicename" for instance "/ADD COM2:"
  }
   NOTIFY_DEVICE_ADD = '/ADD';   
   NOTIFY_DEVICE_REMOVE = '/REMOVE';   
{
 * @struct CE_USER_NOTIFICATION | User Notification Structure
 *
 * @comm  This structure is passed in to <f CeGetUserNotificationPreferences>.
 * Initial settings are used to populate the dialog.  If the function
 * returns TRUE, the returned settings should be saved, and considered when
 * calling <f CeSetUserNotification>.  Settings for hardware not on the
 * current device will be ignored.
 *
 * It is also used when calling <f CeSetUserNotification>, to describe
 * what should happen when the notification time is reached.  
  }
{ PUN_* flags.  Flags not valid on a given }
{ hardware platform will be ignored. }
   PUN_LED = 1;   {@flag PUN_LED | LED flag.  Set if the LED should be }
{ flashed when the notification occurs. }
   PUN_VIBRATE = 2;   {@flag PUN_VIBRATE | Vibrate flag.  Set if the device should }
{ be vibrated. }
   PUN_DIALOG = 4;   {@flag PUN_DIALOG | Dialog flag.  Set if a dialog should be }
{ displayed (the app must provide title and text }
{ when calling <f CeSetUserNotification>). }
   PUN_SOUND = 8;   {@flag PUN_SOUND | Sound flag.  Set if the sound specified }
{ in pwszSound should be played. }
   PUN_REPEAT = 16;   {@flag PUN_REPEAT | Sound repeat flag.  Set if the sound }
{ specified in pwszSound should be repeated progressively. }
   PUN_PRIVATE = 32;   {@flag PUN_PRIVATE | Dialog box z-order flag.  Set if the }
{ notification dialog box should come up behind the password. }
{@field Action Flags.  Any combination of the }
{@field Dialog Title.  Required if PUN_DIALOG is set, ignored }
{ otherwise. Ignored by CeGetUserNotificationPreferences(). }
{@field Dialog Text.  Required if PUN_DIALOG is set, ignored }
{ otherwise. Ignored by CeGetUserNotificationPreferences(). }
{@field Sound string as supplied to sndPlaySound. }
{ CeSetUserNotification() ignores it if PUN_SOUND is not set. }
{@field Max Sound string length.  Specifies the }
{ maximum length of the string that can be copied }
{ into the pwszSound buffer by }
{ CeGetUserNotificationPreferences(). }
{ Should be at least MAX_PATH * sizeof(TCHAR). }
{ Ignored by CeSetUserNotification(). }
{	union  }
{@field Reserved.  Must be NULL or a pointer to }
{ platform-defined expansion structure. }
{ The first dword of the structure }
{ indicates the size of the structure. }
{	    void *pExpansion; }
{	; }
type
   UserNotificationType = record
        ActionFlags : DWORD;
        pwszDialogTitle : ^TCHAR;
        pwszDialogText : ^TCHAR;
        pwszSound : ^TCHAR;
        nMaxSound : DWORD;
        dwReserved : DWORD;
     end;
   CE_USER_NOTIFICATION = UserNotificationType;
   PCE_USER_NOTIFICATION = ^UserNotificationType;

const
   CNT_EVENT = 1;   {@flag CNT_EVENT  | System event notification }
   CNT_TIME = 2;   {@flag CNT_TIME   | Time-based notification }
   CNT_PERIOD = 3;   {@flag CNT_PERIOD | Time-based notification is active for }
{ time period between stStart and stEnd }
   CNT_CLASSICTIME = 4;   {@flag CNT_CLASSICTIME | equivalent to using (obsolete) }
{ CeSetUserNotification function - standard command line is }
{ supplied. lpszArguments must be NULL }
{@field dwType Notification type }
{@field dwEvent - type of event if dwType == CNT_EVENT }
{@field lpszApplication - name of application to execute }
{@field lpszArguments   - command line (sans app name) }
{@field stStartTime - begin of notification period }
{@field stEndTime   - end of notification period }
type
   UserNotificationTrigger = record
        dwSize : DWORD;
        dwType : DWORD;
        dwEvent : DWORD;
        lpszApplication : ^TCHAR;
        lpszArguments : ^TCHAR;
        stStartTime : SYSTEMTIME;
        stEndTime : SYSTEMTIME;
     end;
   CE_NOTIFICATION_TRIGGER = UserNotificationTrigger;
   PCE_NOTIFICATION_TRIGGER = ^UserNotificationTrigger;
{ }
{	Application name can be prefixed with the following strings to specify different }
{	named objects rather than an application. The action varies depending on the prefix. }
{  In case of named event, the event gets signaled. }
{ }

const
   NAMED_EVENT_PREFIX_TEXT = '\\.\Notifications\NamedEvents\';
   NAMED_EVENT_PREFIX_LEN = 30;
   CNS_SIGNALLED = 1;

type
   UserNotificationInfoHeader = record
        hNotification : HANDLE;
        dwStatus : DWORD;
        pcent : ^CE_NOTIFICATION_TRIGGER;
        pceun : ^CE_USER_NOTIFICATION;
     end;
   CE_NOTIFICATION_INFO_HEADER = UserNotificationInfoHeader;
   PCE_NOTIFICATION_INFO_HEADER = ^UserNotificationInfoHeader;
{ Declarations of User Notification APIs.  }
{ C++ extern C conditionnal removed }
{__cplusplus }
{ @CESYSGEN IF GWES_NOTIFY }

function CeGetUserNotificationPreferences(hWndParent:HWND; lpNotification:PCE_USER_NOTIFICATION):BOOL;external KernelDLL name 'CeGetUserNotificationPreferences';
function CeSetUserNotificationEx(hNotification:HANDLE; pcnt:PCE_NOTIFICATION_TRIGGER; pceun:PCE_USER_NOTIFICATION):HANDLE;external KernelDLL name 'CeSetUserNotificationEx';
function CeClearUserNotification(hNotification:HANDLE):BOOL;external KernelDLL name 'CeClearUserNotification';
function CeGetUserNotification(hNotification:HANDLE; cBufferSize:DWORD; pcBytesNeeded:LPDWORD; pBuffer:LPBYTE):BOOL;external KernelDLL name 'CeGetUserNotification';
function CeGetUserNotificationHandles(rghNotifications:PHANDLE; cHandles:DWORD; pcHandlesNeeded:LPDWORD):BOOL;external KernelDLL name 'CeGetUserNotificationHandles';
{	Obsolete; provided to maintain compatibility only }
function CeSetUserNotification(hNotification:HANDLE; pwszAppName:pTCHAR; var lpTime:SYSTEMTIME; lpUserNotification:PCE_USER_NOTIFICATION):HANDLE;external KernelDLL name 'CeSetUserNotification';
function CeRunAppAtTime(pwszAppName:pTCHAR; var lpTime:SYSTEMTIME):BOOL;external KernelDLL name 'CeRunAppAtTime';
function CeRunAppAtEvent(pwszAppName:pTCHAR; lWhichEvent:LONG):BOOL;external KernelDLL name 'CeRunAppAtEvent';
function CeHandleAppNotifications(pwszAppName:pTCHAR):BOOL;external KernelDLL name 'CeHandleAppNotifications';

implementation

end.
