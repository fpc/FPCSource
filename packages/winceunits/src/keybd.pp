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
// Module: keybd.h
//
// Non Win32 defines and structs for WinCE keyboard.
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit keybd;

interface

uses windows, WinIOCtl;

// EXTERNAL DRIVERS
// KEY_STATE_FLAGS | Flags for keyboard events and shift state.
{
These flags perform a number of functions:

The low order bits are used to keep track of a key state.  This
allows using 256 UINT8's for the key state array just like Windows does
already.

The upper bits keep track of shift state on a per key basis.  Since
we are not maintaining a complete key state array for each task, this
gives us some extra info without a big memory penalty.

The KeyStateDownFlag does double duty internal to the driver to
identify key up or key down events.

KeyShiftDeadFlag is set by the <f KeybdDriverVKeyToUnicode> function
to signify that the character generated is a dead character.

KeyShiftNoCharacterFlag is set by the <f KeybdDriverVKeyToUnicode>
function to signify that there is no valid character to generate for the
given virtual key event.  This may be the case on a key up event or a key
which only changes the shift state.

The control, alt, shift and capital flags are set by the <f
KeybdDriverVKeyToUnicode> function to encapsulate the shift state when the
character was generated.

KeybdDriverVKeyToUnicode
KeybdEventCallback
}

type
     KEY_STATE_FLAGS = UINT32;

const
      KeyStateToggledFlag			 = $0001;	//	Key is toggled.
      KeyStateGetAsyncDownFlag = $0002;	//	Key went down since last GetAsyncKey call.
      KeyStateReserved4			   = $0004;
      KeyStateReserved8			   = $0008;
      KeyStateReserved10			 = $0010;
      KeyStateReserved20			 = $0020;
      KeyStatePrevDownFlag		 = $0040;	//	Key was previously down.
      KeyStateDownFlag			   = $0080;	//	Key is currently down.

      KeyStateKeyEventFlag		= $80000000;	//	Internal
      KeyShiftAnyCtrlFlag			= $40000000;  //  L or R control is down.
      KeyShiftAnyShiftFlag		= $20000000;  //  L or R shift is down.
      KeyShiftAnyAltFlag			= $10000000;  //  L or R alt is down.
      KeyShiftCapitalFlag			= $08000000;  //  VK_CAPITAL is toggled.
      KeyShiftLeftCtrlFlag		= $04000000;  //  L control is down.
      KeyShiftLeftShiftFlag		= $02000000;  //  L shift is down.
      KeyShiftLeftAltFlag			= $01000000;  //  L alt is down.
      KeyShiftLeftWinFlag			= $00800000;  //  L Win key is down.
      KeyShiftRightCtrlFlag		= $00400000;  //  R control is down.
      KeyShiftRightShiftFlag	= $00200000;  //  R shift is down.
      KeyShiftRightAltFlag		= $00100000;  //  R alt is down.
      KeyShiftRightWinFlag		= $00080000;  //  R Win key is down.
      KeyShiftReserved40000		= $00040000;  //  Reserved.
      KeyShiftDeadFlag			  = $00020000;  //  Corresponding char is dead char.
      KeyShiftNoCharacterFlag	= $00010000;  //  No corresponding char.

			KeyShiftLanguageFlag1		= $00008000;  //  Use for language specific shifts.
      KeyShiftKeybdEventFlag	= $00004000;	//	Not for external use.

      KeyShiftUseVKNullFlag		= $00002000;  //  Not for external use.
      KeyShiftNumLockFlag			= $00001000;  //  NumLock toggled state.
      KeyShiftScrollLockFlag	= $00000800;  //  ScrollLock toggled state.
      KeyShiftReserved400			= $00000400;  //  Reserved.
      KeyShiftReserved200			= $00000200;  //  Reserved.
      KeyShiftReserved100			= $00000100;  //  Reserved.

// Japanese keyboard
      KeyShiftKanaFlag			  = KeyShiftLanguageFlag1;  //  Kana lock is toggled.


function KeyStateIsDown(Flags:KEY_STATE_FLAGS):BOOL;

function KeyStateIsPrevDown(Flags:KEY_STATE_FLAGS):BOOL;

function KeyStateIsToggled(Flags:KEY_STATE_FLAGS):BOOL;

function KeyStateIsDownTransition(Flags:KEY_STATE_FLAGS):BOOL;

const
      COUNT_VKEYS     = 256;

{
		EXTERNAL DRIVERS
		KEY_STATE       | Array of virtual key states.
}
type
     KEY_STATE = array[0..COUNT_VKEYS-1] of UINT8;

//
// KeyboardDriverGetDeviceInfo query capability definitions
//


//	EXTERNAL DRIVERS
//	ULONG | KBDI_VKEY_TO_UNICODE_INFO_ID |
//  Id for KeybdDriverGetInfo to get Unicode conversion info.
//
// xref
//	KeybdDriverGetInfo
//	KBDI_VKEY_TO_UNICODE_INFO
//	KeybdDriverVKeyToUnicode
const
      KBDI_VKEY_TO_UNICODE_INFO_ID	= 0;


// EXTERNAL DRIVERS KEYBD_DRIVER
// KBDI_VKEY_TO_UNICODE_INFO
// Info required to set up for Unicode conversion.
//
// xref
//	KeybdDriverGetInfo
//	KBDI_VKEY_TO_UNICODE_INFO_ID
//	KeybdDriverVKeyToUnicode
//	TO_UNICODE_STATE
{
cbToUnicodeState is the number of bytes necessary to store the
driver specific state which is required to generate characters from a
virtual key.  For example, a French keyboard driver may need to remember
that an accent key was previously pressed in order to decide on a specific
Unicode character to generate for a virtual key.  This value may be 0 if
no other state besides <t KEY_STATE> is required to generate characters,
e.g., English keyboard drivers.

cMaxToUnicodeCharacters is the maximum number of characters that may
be generated by a call to KeybdDriverVKeyToUnicode.  For example, if a
user presses the '^' key and then the 'b' key, a French keyboard driver
would generate the two characters, '^' 'b', when the second key is
pressed.  Note that this is not the count of bytes required to store the
characters, it is just the number of characters.
}
type
     KBDI_VKEY_TO_UNICODE_INFO = record
       cbToUnicodeState:UINT32;				// @FIELD 	Count of bytes required
											                //			for state info for Unicode
											                //			character generation.
       cMaxToUnicodeCharacters:UINT32;		// @FIELD	Maximum number of characters
											                    //			generated for a single virtual key.
     end;

// EXTERNAL DRIVERS
// ULONG | KBDI_AUTOREPEAT_INFO_ID
// Id for KeybdDriverGetInfo to get keyboard auto-repeat info.
//
// xref
//  KeybdDriverGetInfo
//  KBDI_AUTOREPEAT_INFO
//	KBDI_AUTOREPEAT_SELECTIONS_INFO_ID
const
      KBDI_AUTOREPEAT_INFO_ID	= 1;


// Info about the keyboard autorepeat capabilities and settings.
{
cInitialDelaysSelectable gives the number of initial delays which
may be set.  Query again using <c KBDI_AUTOREPEAT_SELECTIONS_INFO_ID> to
get the actual values available.

cRepeatRatesSelectable is similar to cInitialDelaysSelectable except
that it gives the number of repeat rates available.
}
type
     KBDI_AUTOREPEAT_INFO = record
	   	 CurrentInitialDelay:INT32;		// @FIELD	Current initial delay in milliseconds.
	     CurrentRepeatRate:INT32;			// @FIELD	Current repeat rate in keys per second.
	     cInitialDelaysSelectable:INT32;	// @FIELD	Number of initial delays selectable.
	     cRepeatRatesSelectable:INT32;		// @FIELD	Number of repeat rates supported.
     end;

const
      KBD_AUTO_REPEAT_INITIAL_DELAY_DEFAULT  =   500;
      KBD_AUTO_REPEAT_INITIAL_DELAY_MIN      =   250;
      KBD_AUTO_REPEAT_INITIAL_DELAY_MAX      =  1000;

      KBD_AUTO_REPEAT_KEYS_PER_SEC_DEFAULT   =    20;
      KBD_AUTO_REPEAT_KEYS_PER_SEC_MIN       =     2;
      KBD_AUTO_REPEAT_KEYS_PER_SEC_MAX       =    30;


// Id for KeybdDriverGetInfo to get keyboard auto-repeat selections info.
{
When KeybdDriverGetInfo is called with this value, the lpOutput
parameter should be a pointer to an array of INT32's to hold the selection
info.  The initial delays will be put at the beginning of the array
followed by the repeat rate selections.  The number of initial delay
values is determined by calling <f KeybdDriverGetInfo> using <c
KBDI_AUTOREPEAT_INFO_ID> and looking at the returned
cInitialDelaysSelectable field.  If this value is -1, there will be two
(2) values, the min and max and the initial delay may be set to any value
in this range.  This value may be 0 if the initial delay is not settable.
Similarly, if cRepeatRatesSelectable is 0, there will be no repeat rate
information.  If it is -1, there will be two (2) values, the min and max.

Initial delay values are in milliseconds.  Repeat rates are in keys per
second.
}
const
      KBDI_AUTOREPEAT_SELECTIONS_INFO_ID	= 2;

// INTERNATIONAL
const
      KBDI_KEYBOARD_STATUS_ID = 3;

const
      KBDI_KEYBOARD_PRESENT	  = $0001;
      KBDI_KEYBOARD_ENABLED	  = $0002;
      KBDI_KEYBOARD_ENTER_ESC	= $0004;
      KBDI_KEYBOARD_ALPHA_NUM	= $0008;


// Reserve for SHIME_MODE.
// Pass in the SHIME_MODE as the input parameter.
const
      KBDI_SHIME_MODE_ID = 4;

      KBDI_SHIME_MODE_NONE                = $0000;
      KBDI_SHIME_MODE_SPELL               = $0001;
      KBDI_SHIME_MODE_SPELL_CAPS          = $0002;
      KBDI_SHIME_MODE_SPELL_CAPS_LOCK     = $0003;
      KBDI_SHIME_MODE_AMBIGUOUS           = $0004;
      KBDI_SHIME_MODE_AMBIGUOUS_CAPS      = $0005;
      KBDI_SHIME_MODE_AMBIGUOUS_CAPS_LOCK = $0006;
      KBDI_SHIME_MODE_NUMBERS             = $0007;
      KBDI_SHIME_MODE_CUSTOM              = $0008;


// External keyboard interface
const
      DEVCLASS_KEYBOARD_STRING = '{CBE6DDF2-F5D4-4E16-9F61-4CCC0B6695F3}';
      DEVCLASS_KEYBOARD_GUID:System.TGUID = (D1: $CBE6DDF2; D2: $F5D4; D3: $4E16; D4: ($9F, $61, $4C, $CC, $0B, $66, $95, $F3));
//      DEVCLASS_KEYBOARD_GUID:System.TGUID = '{CBE6DDF2-F5D4-4E16-9F61-4CCC0B6695F3}';


// Pass in the KEY_STATE_FLAGS as the input parameter.
const
      IOCTL_KBD_SET_MODIFIERS = (FILE_DEVICE_KEYBOARD shl 16) or
                                (1 shl 2) or
                                METHOD_BUFFERED or
                                (FILE_ANY_ACCESS shl 14);


      IOCTL_HID_SET_MODIFIERS = IOCTL_KBD_SET_MODIFIERS;

// Pass in the KBDI_AUTOREPEAT_INFO as the input parameter.
const
      IOCTL_KBD_SET_AUTOREPEAT = (FILE_DEVICE_KEYBOARD shl 16) or
                                 (2 shl 2) or
                                 METHOD_BUFFERED or
                                 (FILE_ANY_ACCESS shl 14);

      IOCTL_HID_SET_AUTOREPEAT = IOCTL_KBD_SET_AUTOREPEAT;

// Pass in the Input Language's fLocaleFlags as the input parameter.
const
      IOCTL_KBD_SET_LOCALE_FLAGS = (FILE_DEVICE_KEYBOARD shl 16) or
                                   (3 shl 2) or
                                   METHOD_BUFFERED or
                                   (FILE_ANY_ACCESS shl 14);

implementation

function KeyStateIsDown(Flags:KEY_STATE_FLAGS):BOOL; inline;
begin
  KeyStateIsDown:=(Flags and KeyStateDownFlag)<>0;
end;

function KeyStateIsPrevDown(Flags:KEY_STATE_FLAGS):BOOL; inline;
begin
  KeyStateIsPrevDown:=(Flags and KeyStatePrevDownFlag)<>0;
end;

function KeyStateIsToggled(Flags:KEY_STATE_FLAGS):BOOL; inline;
begin
  KeyStateIsToggled:=(Flags and KeyStateToggledFlag)<>0;
end;

function KeyStateIsDownTransition(Flags:KEY_STATE_FLAGS):BOOL; inline;
begin
  KeyStateIsDownTransition:=KeyStateIsDown(Flags) and (not KeyStateIsPrevDown(Flags));
end;

end.