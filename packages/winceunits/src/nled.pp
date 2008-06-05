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
//File Name:	nled.h
//
//Abstract:   Notification LED interface.
//
//Notes:
//
//The notification LED is distinguished from other LED's which may be on the system
//in that it can be on or blinking even if the rest of the system is powered down.  This
//implies a certain level of hardware support for this functionality.
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit NLed;

{$CALLING cdecl}

interface

uses Windows, WinIOCtl;

// @CESYSGEN IF COREDLL_NLED

//
// NLedDriverGetDeviceInfo query definitions
//

{		@DOC	EXTERNAL DRIVERS
		@const	ULONG | NLED_COUNT_INFO_ID |

Id for <f NLedDriverGetDeviceInfo> to get count of notification LED's

		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><t NLDI_COUNT_INFO><nl>

}
const
      NLED_COUNT_INFO_ID	= 0;


{      @DOC    EXTERNAL DRIVERS
		@STRUCT	NLED_COUNT_NFO |

Info about number of notification LED's.

		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><c NLED_COUNT_INFO_ID><nl>

		@COMM

A device will usually have one notification LED.  Some devices may have
none however.  It would be unusual to have more than one because of the
power drain.


}
type
     NLED_COUNT_INFO = record
	    	 cLeds:UINT;				// @FIELD 	Count of LED's
     end;

{		@DOC	EXTERNAL DRIVERS
		@const	ULONG | NLED_SUPPORTS_INFO_ID |

Id for <f NLedDriverGetDeviceInfo> to get supported capabilities of a
notification LED.


		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><t NLDI_SUPPORTS_INFO><nl>

}
const
      NLED_SUPPORTS_INFO_ID	= 1;


{      @DOC    EXTERNAL DRIVERS
		@STRUCT	NLED_SUPPORTS_NFO |

Info about what a notification LED supports.

		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><c NLED_SUPPORTS_INFO_ID><nl>

		@COMM

Caller should first get the number of notification LED's on the system.  
This is usually one but may be 0 and could be greater than 1.  Caller 
should fill in the LedNum and then call <f NLedDriverGetDeviceInfo>.  Led 
numbering starts at 0.

The lCycleAdjust field is the granularity to which cycle time adjustments 
can be made.  E.g., if the granularity is 1/16 second, lCycleAdjust = 
62500 microseconds.  If the LED does not support blinking, this value 
should be 0.

Values are given in microseconds only to deal with computations involving 
fractional milliseconds.  Usually only settings in the milliseconds range 
are meaningful.  

Usually a device will report that it supports up to two of:

	fAdjustTotalCycleTime
	fAdjustOnTime
	fAdjustOffTime

since any two determine the third.  The most likely settings are:

	1. The cycle time is not adjustable:

		fAdjustTotalCycleTime == FALSE
		fAdjustOnTime == FALSE
		fAdjustOffTime == FALSE

	2. Only the overall cycle time is adjustable:

		fAdjustTotalCycleTime == TRUE
		fAdjustOnTime == FALSE
		fAdjustOffTime == FALSE

	3. The on and off times are independently adjustable.

		fAdjustTotalCycleTime == FALSE
		fAdjustOnTime == TRUE
		fAdjustOffTime == TRUE
}

type
     NLED_SUPPORTS_INFO = record
	    	 LedNum:UINT;						// @FIELD 	LED number, 0 is first LED
	      lCycleAdjust:LONG;				// @FIELD	Granularity of cycle time adjustments (microseconds)
	      fAdjustTotalCycleTime:BOOL;		// @FIELD	LED has an adjustable total cycle time
	      fAdjustOnTime:BOOL;				// @FIELD	LED has separate on time
	      fAdjustOffTime:BOOL;				// @FIELD	LED has separate off time
	      fMetaCycleOn:BOOL;				// @FIELD	LED can do blink n, pause, blink n, ...
	      fMetaCycleOff:BOOL;				// @FIELD	LED can do blink n, pause n, blink n, ...
     end;


{		@DOC	EXTERNAL DRIVERS
		@const	ULONG | NLED_SETTINGS_INFO_ID |

Id for <f NLedDriverGetDeviceInfo> to get current settings of a
notification LED.


		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><t NLDI_SETTINGS_INFO><nl>
}
const
      NLED_SETTINGS_INFO_ID	= 2;


{      @DOC    EXTERNAL DRIVERS
		@STRUCT	NLED_SETTINGS_NFO |

Info about the current settings of a notification LED.

		@XREF

			<tab><f NLedDriverGetDeviceInfo><nl>
			<tab><c NLED_SETTINGS_INFO_ID><nl>

		@COMM

Caller should first get the number of notification LED's on the system.
This is usually one but may be 0 and could be greater than 1.  Caller
should fill in the LedNum and then call <f NLedDriverGetDeviceInfo>.  Led
numbering starts at 0.

Note that the hardware may have some minimum on or off time, so setting the OnTime or OffTime
fields to 0 may still result in a non-zero on or off time.
}
type
     NLED_SETTINGS_INFO = record
       LedNum:UINT;                 // @FIELD   LED number, 0 is first LED
       OffOnBlink:longint;             // @FIELD   0 == off, 1 == on, 2 == blink
       TotalCycleTime:LONG;         // @FIELD   total cycle time of a blink in microseconds
       OnTime:LONG;                 // @FIELD   on time of a cycle in microseconds
       OffTime:LONG;                // @FIELD   off time of a cycle in microseconds
       MetaCycleOn:longint;            // @FIELD   number of on blink cycles
       MetaCycleOff:longint;           // @FIELD   number of off blink cycles
     end;


// NLED driver IOCTL codes
const
      IOCTL_NLED_GETDEVICEINFO = (FILE_DEVICE_NLED shl 16) or
                                 (FILE_ANY_ACCESS shl 14) or
                                 ($0100 shl 2) or
                                 METHOD_BUFFERED;

      IOCTL_NLED_SETDEVICE =(FILE_DEVICE_NLED shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            ($0101 shl 2) or
                            METHOD_BUFFERED;

// Battery driver interface GUID (used with AdvertiseInterface())
const
      NLED_DRIVER_CLASS        = '{CBB4F234-F35F-485b-A490-ADC7804A4EF3}';

// API event name for the NLED interface.  If calling OpenEvent() on this name returns a valid
// handle, the NLED APIs exist on the platform.  The handle will be signaled when the APIs become
// ready.
const
      NLED_API_EVENT_NAME     = 'SYSTEM/NLedAPIsReady';

type
     PFN_NLED_SET_DEVICE = function(nDeviceId:UINT; pInput:pointer):BOOL; cdecl;
     PFN_NLED_GET_DEVICE_INFO = function(nInfoId:UINT; pOutput:pointer):BOOL; cdecl;

function NLedGetDeviceInfo(nInfoId:UINT; pOutput:pointer):BOOL; external KernelDLL name 'NLedGetDeviceInfo'; // index 49F
function NLedSetDevice(nDeviceId:UINT; pInput:pointer):BOOL;  external KernelDLL name 'NLedSetDevice'; // index 4A0

// @CESYSGEN ENDIF COREDLL_NLED

implementation

end.