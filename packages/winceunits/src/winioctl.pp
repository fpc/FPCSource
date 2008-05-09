{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
// *
// * Module Name: winioctl.h
// * Abstract: this module defines device IO control codes.
// *

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit WinIOCtl;

interface

uses Windows;

// The definition below is intentionally left SO FAR because some of useful
// constants or macros declared here may be redeclared elsewhere by pure accident.
{$IFNDEF _WINIOCTL_}
  {$DEFINE _WINIOCTL_}
{$ENDIF _WINIOCTL_}

type
     DEVICE_TYPE = ULONG;

//
// Macro definition for defining IOCTL and FSCTL function control codes.  Note
// that function codes 0-2047 are reserved for Microsoft Corporation, and
// 2048-4095 are reserved for customers.
//
function CTL_CODE(_DeviceType:DEVICE_TYPE; _Function:DWORD; _Method:DWORD; _Access:DWORD):DWORD;
{ This macro was declared as:
#define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
    ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
)
}


//
// Define the method codes for how buffers are passed for I/O and FS controls
//
const
      METHOD_BUFFERED                 = 0;
      METHOD_IN_DIRECT                = 1;
      METHOD_OUT_DIRECT               = 2;
      METHOD_NEITHER                  = 3;

//
// Define the access check value for any access
//
//
// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
const
      FILE_ANY_ACCESS                 = 0;
      FILE_READ_ACCESS                = $0001;    // file & pipe
      FILE_WRITE_ACCESS               = $0002;    // file & pipe

//
// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.
//
const
      FILE_DEVICE_BEEP                = $00000001;
      FILE_DEVICE_CD_ROM              = $00000002;
      FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
      FILE_DEVICE_CONTROLLER          = $00000004;
      FILE_DEVICE_DATALINK            = $00000005;
      FILE_DEVICE_DFS                 = $00000006;
      FILE_DEVICE_DISK                = $00000007;
      FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
      FILE_DEVICE_FILE_SYSTEM         = $00000009;
      FILE_DEVICE_INPORT_PORT         = $0000000a;
      FILE_DEVICE_KEYBOARD            = $0000000b;
      FILE_DEVICE_MAILSLOT            = $0000000c;
      FILE_DEVICE_MIDI_IN             = $0000000d;
      FILE_DEVICE_MIDI_OUT            = $0000000e;
      FILE_DEVICE_MOUSE               = $0000000f;
      FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
      FILE_DEVICE_NAMED_PIPE          = $00000011;
      FILE_DEVICE_NETWORK             = $00000012;
      FILE_DEVICE_NETWORK_BROWSER     = $00000013;
      FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
      FILE_DEVICE_NULL                = $00000015;
      FILE_DEVICE_PARALLEL_PORT       = $00000016;
      FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
      FILE_DEVICE_PRINTER             = $00000018;
      FILE_DEVICE_SCANNER             = $00000019;
      FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
      FILE_DEVICE_SERIAL_PORT         = $0000001b;
      FILE_DEVICE_SCREEN              = $0000001c;
      FILE_DEVICE_SOUND               = $0000001d;
      FILE_DEVICE_STREAMS             = $0000001e;
      FILE_DEVICE_TAPE                = $0000001f;
      FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
      FILE_DEVICE_TRANSPORT           = $00000021;
      FILE_DEVICE_UNKNOWN             = $00000022;
      FILE_DEVICE_VIDEO               = $00000023;
      FILE_DEVICE_VIRTUAL_DISK        = $00000024;
      FILE_DEVICE_WAVE_IN             = $00000025;
      FILE_DEVICE_WAVE_OUT            = $00000026;
      FILE_DEVICE_8042_PORT           = $00000027;
      FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
      FILE_DEVICE_BATTERY             = $00000029;
      FILE_DEVICE_BUS_EXTENDER        = $0000002a;
      FILE_DEVICE_MODEM               = $0000002b;
      FILE_DEVICE_VDM                 = $0000002c;
      FILE_DEVICE_MASS_STORAGE        = $0000002d;
      FILE_DEVICE_SMB                 = $0000002e;
      FILE_DEVICE_KS                  = $0000002f;
      FILE_DEVICE_CHANGER             = $00000030;
      FILE_DEVICE_SMARTCARD           = $00000031;
      FILE_DEVICE_ACPI                = $00000032;
      FILE_DEVICE_DVD                 = $00000033;
      FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
      FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
      FILE_DEVICE_DFS_VOLUME          = $00000036;

//
// Windows CE Specific Defines
//
const
      FILE_DEVICE_HAL                 = $00000101;
      FILE_DEVICE_CONSOLE             = $00000102;
      FILE_DEVICE_PSL                 = $00000103;
      FILE_DEVICE_SERVICE             = $00000104;
      FILE_DEVICE_NLED                = $00000105;
      FILE_DEVICE_NOTIFY              = $00000106;
      FILE_DEVICE_GPS                 = $00000107;
      FILE_DEVICE_SQM                 = $00000108;

implementation

function CTL_CODE(_DeviceType:DEVICE_TYPE; _Function:DWORD; _Method:DWORD; _Access:DWORD):DWORD; inline;
begin
  CTL_CODE:=(_DeviceType shl 16) or (_Access shl 14) or (_Function shl 2) or _Method;
end;

end.