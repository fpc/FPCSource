{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
// Module Name:
//
//     pm.h
//
// Abstract:
//
//     Definitions and API for the WCE Power Manager component.
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit PM;

{$CALLING cdecl}

interface

uses Windows, WinIOCtl;

//**********************************************************************
// WinCE Device Interface GUIDs for Power Manager controlled devices.
// NEVER CHANGE THESE VALUES! They indicate that a device is
// power manageable. The device MUST expose an appropriate class
// via the IClass registry key or the AdvertiseInterface() API to
// receive power management IOCTLs.  For example,
//   "IClass"=multi_sz:"{A32942B7-920C-486b-B0E6-92A702A99B35}"
// OEMs may define other classes in addition to the ones listed here.
//**********************************************************************
const
      PMCLASS_GENERIC_DEVICE          = '{A32942B7-920C-486B-B0E6-92A702A99B35}';
      PMCLASS_NDIS_MINIPORT           = '{98C5250D-C29A-4985-AE5F-AFE5367E5006}';
      PMCLASS_BLOCK_DEVICE            = '{8DD679CE-8AB4-43C8-A14A-EA4963FAA715}';
      PMCLASS_DISPLAY                 = '{EB91C7C9-8BF6-4A2D-9AB8-69724EED97D1}';

// backwards compatibility definitions from the PM's CE .NET release
      DEVCLASS_POWER_MANAGER_STRING   = PMCLASS_GENERIC_DEVICE;
      DEVCLASS_POWER_MANAGER_GUID:GUID = (D1: $A32942B7; D2: $920C; D3: $486B; D4: ($B0, $E6, $92, $A7, $02, $A9, $9B, $35));
//    DEVCLASS_POWER_MANAGER_GUID:GUID = '{A32942B7-920C-486B-B0E6-92A702A99B35));


//*****************************************************************************
// D E F I N E S
//*****************************************************************************

// Power Manager's System Power key
const
      PWRMGR_REG_KEY           = 'SYSTEM\CurrentControlSet\Control\Power';
      PM_SUPPORT_PB_RELEASE    = 'SupportPowerButtonRelease';

//
// System Power (Source/State/Option) Flags
//

// upper bytes: common power state bits
function POWER_STATE(f:DWORD):DWORD;                      // power state mask

const


      POWER_STATE_ON           = $00010000;        // on state
      POWER_STATE_OFF          = $00020000;        // no power, full off
      POWER_STATE_CRITICAL     = $00040000;        // critical off
      POWER_STATE_BOOT         = $00080000;        // boot state
      POWER_STATE_IDLE         = $00100000;        // idle state
      POWER_STATE_SUSPEND      = $00200000;        // suspend state
      POWER_STATE_UNATTENDED   = $00400000;        // Unattended state.
      POWER_STATE_RESET        = $00800000;        // reset state
      POWER_STATE_USERIDLE     = $01000000;        // user idle state
      POWER_STATE_BACKLIGHTON  = $02000000;        // device scree backlight on
      POWER_STATE_PASSWORD     = $10000000;        // This state is password protected.

      PM_DEFAULT_SZ            = 'Default';
      PM_FLAGS_SZ              = 'Flags';
      MAX_STATE_NAMEL          = MAX_PATH;    // max system power state name length

//
// Power Requirement Flags
//
const
      POWER_NAME              = $00000001; // default
      POWER_FORCE             = $00001000;
      POWER_DUMPDW            = $00002000;// Calling CaptureDumpFileOnDevice() before entering this state.

//
// POWER IOCTLS
//
// We are NOT APCI, we just borrow this unused code from winioctl.h
const
      FILE_DEVICE_POWER   = FILE_DEVICE_ACPI;

{
Required
InBuf:  PPOWER_RELATIONSHIP - defines the target device for parent/bus drivers, else NULL
OutBuf: PPOWER_CAPABILITIES - defines the devices power caps

If a driver fails this ioctl the PM assumes the target driver does not support power ioctls.
}
const
      IOCTL_POWER_CAPABILITIES = (FILE_DEVICE_POWER shl 16) or ($400 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

{ ++
Required
InBuf:  PPOWER_RELATIONSHIP   - defines the target device for parent/bus drivers, else NULL
OutBuf: PCEDEVICE_POWER_STATE - returns the device's current state (Dx).

PM will only send this ioctl to drivers that support the power ioctls.
-- }
const
      IOCTL_POWER_GET = (FILE_DEVICE_POWER shl 16) or ($401 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

{ ++
Required
InBuf:  PPOWER_RELATIONSHIP   - defines the target device for parent/bus drivers, else NULL
OutBuf: PCEDEVICE_POWER_STATE - device state (Dx) in which to put the device.

If the driver does not support the proposed Dx then it should write it's adjusted Dx
into the OutBuf (Dx).

PM will only send this ioctl to drivers that support the power ioctls.
-- }
const
      IOCTL_POWER_SET = (FILE_DEVICE_POWER shl 16) or ($402 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

{++
Required
InBuf:  PPOWER_RELATIONSHIP   - defines the target device for parent/bus drivers, else NULL
OutBuf: PCEDEVICE_POWER_STATE - device state (Dx) that the system is querying for a
                                pending IOCTL_POWER_SET operation.

To veto the query the driver should write PwrDeviceUnspecified (-1)
into the OutBuf (Dx), else PM assumes the driver accepted.

PM will only send this ioctl to drivers that support the power ioctls.
-- }
const
      IOCTL_POWER_QUERY = (FILE_DEVICE_POWER shl 16) or ($403 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

{
Required
InBuf:  NULL
OutBuf: NULL

PM does not care the return value from this IOCTL.   It's there to let the Parent device
to register all devices it controls.
}
const
      IOCTL_REGISTER_POWER_RELATIONSHIP = (FILE_DEVICE_POWER shl 16) or ($406 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//*****************************************************************************
// T Y P E D E F S
//*****************************************************************************

//
// Device Power States
//
type
     _CEDEVICE_POWER_STATE = (PwrDeviceUnspecified := -1,
                              D0 := 0, // Full On: full power,  full functionality
                              D1,     // Low Power On: fully functional at low power/performance
                              D2,     // Standby: partially powered with automatic wake
                              D3,     // Sleep: partially powered with device initiated wake
                              D4,     // Off: unpowered
                              PwrDeviceMaximum
                             );
     CEDEVICE_POWER_STATE = _CEDEVICE_POWER_STATE;
     PCEDEVICE_POWER_STATE = ^_CEDEVICE_POWER_STATE;

function DX_MASK(Dx:DWORD):DWORD;

function VALID_DX(dx:DWORD):BOOL;

//
// Device or OAL Power Capabilities
//
type
     _POWER_CAPABILITIES = record
       DeviceDx:UCHAR;
       WakeFromDx:UCHAR;
       InrushDx:UCHAR;
       Power:array[0..DWORD(PwrDeviceMaximum)-1] of DWORD;
       Latency:array[0..DWORD(PwrDeviceMaximum)-1] of DWORD;
       Flags:DWORD;
     end;
     POWER_CAPABILITIES = _POWER_CAPABILITIES;
     PPOWER_CAPABILITIES = ^_POWER_CAPABILITIES;

const
      POWER_CAP_PARENT        = $00000001;      // parent/bus driver

//
// Defines the target of IOCTL_POWER_Xxx commands to parent/bus drivers
// if there is a relationship established via RegisterPowerRelationship.
//
type
     _POWER_RELATIONSHIP = record
       hParent:HANDLE;    // Handle to parent node
       pwsParent:LPCWSTR;  // Named parent node, e.g. "NDS0:"
       hChild:HANDLE;     // Handle to child node, returned from RegisterPowerRelationship
       pwsChild:LPCWSTR;   // Named child node, e.g. "NE20001"
     end;
     POWER_RELATIONSHIP = _POWER_RELATIONSHIP;
     PPOWER_RELATIONSHIP = ^_POWER_RELATIONSHIP;


//
// Power Broadcast Types -- there are up to 32 of these
//
const
      PBT_TRANSITION          = $00000001;  // broadcast specifying system power state transition
      PBT_RESUME              = $00000002;  // broadcast notifying a resume, specifies previous state
      PBT_POWERSTATUSCHANGE   = $00000004;  // power supply switched to/from AC/DC
      PBT_POWERINFOCHANGE     = $00000008;  // some system power status field has changed

      PBT_SUSPENDKEYPRESSED   = $00000100;  // Suspend Key has been pressed.

// OEMS may define power notifications starting with this ID and
// going up by powers of 2.
      PBT_OEMBASE             = $00010000;

// This bitmask indicates that an application would like to receive all
// types of power notifications.
const
      POWER_NOTIFY_ALL        = $FFFFFFFF;

//
// Power Broadcast -- this is a variable length structure.
//
type
     _POWER_BROADCAST  = record
       _Message:DWORD;    // one of PBT_Xxx
       Flags:DWORD;      // one of POWER_STATE_Xxx
       Length:DWORD;     // byte count of data starting at SystemPowerStateName
       SystemPowerState:array[0..0] of WideChar;    // variable length field, must be smaller than MAX_PATH + 1
     end;
     POWER_BROADCAST = _POWER_BROADCAST;
     PPOWER_BROADCAST = ^_POWER_BROADCAST;

// This structure is used instead of a string name (SystemPowerState) in
// the POWER_BROADCAST, if the broadcast is of type PBT_POWERINFOCHANGE.
//
// For example:
//  PPOWER_BROADCAST ppb;
//  PPOWER_BROADCAST_POWER_INFO ppbpi =
//        (PPOWER_BROADCAST_POWER_INFO) ppb->SystemPowerState;
//
type
     _POWER_BROADCAST_POWER_INFO = record
      // levels available in battery flag fields, see BatteryDrvrGetLevels()
       dwNumLevels:DWORD;

      // see GetSystemPowerStatusEx2()
       dwBatteryLifeTime:DWORD;
       dwBatteryFullLifeTime:DWORD;
       dwBackupBatteryLifeTime:DWORD;
       dwBackupBatteryFullLifeTime:DWORD;
       bACLineStatus:byte;
       bBatteryFlag:byte;
       bBatteryLifePercent:byte;
       bBackupBatteryFlag:byte;
       bBackupBatteryLifePercent:byte;
     end;
     POWER_BROADCAST_POWER_INFO = _POWER_BROADCAST_POWER_INFO;
     PPOWER_BROADCAST_POWER_INFO = ^_POWER_BROADCAST_POWER_INFO;

//*****************************************************************************
// P R O T O S
//*****************************************************************************
function GetSystemPowerState(pBuffer:LPWSTR; dwBufChars:DWORD; pdwFlags:PDWORD):DWORD; external KernelDLL name 'GetSystemPowerState'; // index 15E

function SetSystemPowerState(pwsSystemState:LPCWSTR; StateFlags:DWORD; Options:DWORD):DWORD; external KernelDLL name 'SetSystemPowerState'; // index 15F

function SetPowerRequirement(pvDevice:PVOID;
                             DeviceState:CEDEVICE_POWER_STATE;
                             DeviceFlags:ULONG;
                             pvSystemState:PVOID;
                             StateFlags:ULONG):HANDLE; external KernelDLL name 'SetPowerRequirement'; // index 160

function ReleasePowerRequirement(hPowerReq:HANDLE):DWORD; external KernelDLL name 'ReleasePowerRequirement'; // index 161

function RequestPowerNotifications(hMsgQ:HANDLE; Flags:DWORD):HANDLE; external KernelDLL name 'RequestPowerNotifications'; // index 162

function StopPowerNotifications(h:HANDLE):DWORD; external KernelDLL name 'StopPowerNotifications'; // index 163

function DevicePowerNotify(pvDevice:PVOID;	DeviceState:CEDEVICE_POWER_STATE; Flags:DWORD):DWORD; external KernelDLL name 'DevicePowerNotify'; // index 164

function RegisterPowerRelationship(pvParent:PVOID;
                                   pvChild:PVOID;
                                   pCaps:PPOWER_CAPABILITIES;
                                   Flags:DWORD):HANDLE; external KernelDLL name 'RegisterPowerRelationship'; // index 165

function ReleasePowerRelationship(hChild:HANDLE):DWORD; external KernelDLL name 'ReleasePowerRelationship'; // index 166

function SetDevicePower(pvDevice:PVOID; dwDeviceFlags:DWORD; dwState:CEDEVICE_POWER_STATE):DWORD; external KernelDLL name 'SetDevicePower'; // index 167

function GetDevicePower(pvDevice:PVOID; dwDeviceFlags:DWORD; pdwState:PCEDEVICE_POWER_STATE):DWORD; external KernelDLL name 'GetDevicePower'; // index 168

//*** Begin of pmpolicy.h declarations

//
// pmpolicy.h
//
//
// This routine contains definitions and APIs for communicating with a platform-specific
// power policy manager.
//

// notification messages
const
      PPN_REEVALUATESTATE             = $0001;  // dwData is reserved, use 0
      PPN_POWERCHANGE                 = $0002;  // dwData is reserved, use 0
      PPN_UNATTENDEDMODE              = $0003;  // dwData is TRUE or FALSE
      PPN_SUSPENDKEYPRESSED           = $0004;  // dwData is reserved. use 0
      PPN_SUSPENDKEYRELEASED          = $0005;  // dwData is reserved. use 0
      PPN_APPBUTTONPRESSED            = $0006;  // dwData is reserved. use 0
      PPN_POWERBUTTONPRESSED          = PPN_SUSPENDKEYPRESSED;

// OEMs can define values higher than this (as PPN_OEMBASE+0, +1, etc.)
const
      PPN_OEMBASE                     = $10000;

// This routine notifies the power policy manager of events it needs in order to
// implement the OEM's power policy.  The dwMessage parameter is one of the PPN_
// values (or an OEM-defined one).  The dwData parameter is a 32-bit value whose
// interpretation varies with the notification message.  A return value of TRUE
// indicates success.
function PowerPolicyNotify(dwMessage:DWORD; dwData:DWORD):BOOL; external KernelDLL name 'PowerPolicyNotify'; // index 169

//*** End of pmpolicy.h declarations

implementation

// upper bytes: common power state bits
function POWER_STATE(f:DWORD):DWORD; inline;
begin
  POWER_STATE:=f and $FFFF0000;
end;

function DX_MASK(Dx:DWORD):DWORD; inline;
begin
  DX_MASK:=$00000001 shl Dx;
end;

function VALID_DX(dx:DWORD):BOOL; inline;
begin
  VALID_DX:=(dx>DWORD(PwrDeviceUnspecified)) and (dx<DWORD(PwrDeviceMaximum));
end;

end.