{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    input event structures

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit inputevent;

interface

uses
  exec, utility, timer;

const

{------ constants -------------------------------------------------}

{   --- InputEvent.ie_Class --- }
  IECLASS_NULL           = $00; // A NOP input event
  IECLASS_RAWKEY         = $01; // A raw keycode from the keyboard device
  IECLASS_RAWMOUSE       = $02; // The raw mouse report from the game port device
  IECLASS_EVENT          = $03; // A private console event
  IECLASS_POINTERPOS     = $04; // A Pointer Position report
  IECLASS_TIMER          = $06; // A timer event
  IECLASS_GADGETDOWN     = $07; // select button pressed down over a Gadget (address in ie_EventAddress)
  IECLASS_GADGETUP       = $08; // select button released over the same Gadget (address in ie_EventAddress)
  IECLASS_REQUESTER      = $09; // some Requester activity has taken place.  See Codes REQCLEAR and REQSET
  IECLASS_MENULIST       = $0A; // this is a Menu Number transmission (Menu number is in ie_Code)
  IECLASS_CLOSEWINDOW    = $0B; // User has selected the active Window's Close Gadget
  IECLASS_SIZEWINDOW     = $0C; // this Window has a new size
  IECLASS_REFRESHWINDOW  = $0D; // the Window pointed to by ie_EventAddress needs to be refreshed
  IECLASS_NEWPREFS       = $0E; // new preferences are available
  IECLASS_DISKREMOVED    = $0F; // the disk has been removed
  IECLASS_DISKINSERTED   = $10; // the disk has been inserted
  IECLASS_ACTIVEWINDOW   = $11; // the window is about to be been made active
  IECLASS_INACTIVEWINDOW = $12; // the window is about to be made inactive
  IECLASS_NEWPOINTERPOS  = $13; // extended-function pointer position report (V36)
  IECLASS_MENUHELP       = $14; // Help key report during Menu session (V36)
  IECLASS_CHANGEWINDOW   = $15; // the Window has been modified with move, size, zoom, or change (V36)
  // AROS
  IECLASS_NEWMOUSE       = $16; // NewMouse standard


  IECLASS_MAX            = $16; // the last Class

{  --- InputEvent.ie_SubClass --- }
{  IECLASS_NEWPOINTERPOS }
{      like IECLASS_POINTERPOS }
  IESUBCLASS_COMPATIBLE  = $00;
  IESUBCLASS_PIXEL       = $01; // ie_EventAddress points to struct IEPointerPixel
  IESUBCLASS_TABLET      = $02; // ie_EventAddress points to struct IEPointerTablet
  IESUBCLASS_NEWTABLET   = $03; // ie_EventAddress points to struct IENewTablet


{ pointed to by ie_EventAddress for IECLASS_NEWPOINTERPOS,
 * and IESUBCLASS_PIXEL.
 *
 * You specify a screen and pixel coordinates in that screen
 * at which you'd like the mouse to be positioned.
 * Intuition will try to oblige, but there will be restrictions
 * to positioning the pointer over offscreen pixels.
 *
 * IEQUALIFIER_RELATIVEMOUSE is supported for IESUBCLASS_PIXEL.
 }
type
   PIEPointerPixel = ^TIEPointerPixel;
   TIEPointerPixel = record
    iepp_Screen: Pointer;   {PScreen pointer to an open screen }
    iepp_Position: record
       x, y: SmallInt;
    end;
   end;

{ pointed to by ie_EventAddress for IECLASS_NEWPOINTERPOS,
 * and IESUBCLASS_TABLET.
 *
 * You specify a range of values and a value within the range
 * independently for each of X and Y (the minimum value of
 * the ranges is always normalized to 0).
 *
 * Intuition will position the mouse proportionally within its
 * natural mouse position rectangle limits.
 *
 * IEQUALIFIER_RELATIVEMOUSE is not supported for IESUBCLASS_TABLET.
 }

   PIEPointerTablet = ^TIEPointerTablet;
   TIEPointerTablet = record
    iept_Range: record   // 0 is min, these are max
       x, y: word;
    end;
    iept_Value : record  // between 0 AND iept_Range
       x, y: word;
    end;
    iept_Pressure: SmallInt; // -128 to 127 (unused, set to 0)
   end;

{ The ie_EventAddress of an IECLASS_NEWPOINTERPOS event of subclass
 * IESUBCLASS_NEWTABLET points at an IENewTablet structure.
 *
 *
 * IEQUALIFIER_RELATIVEMOUSE is not supported for IESUBCLASS_NEWTABLET.
 }

   PIENewTablet = ^TIENewTablet;
   TIENewTablet = record
    { Pointer to a hook you wish to be called back through, in
     * order to handle scaling.  You will be provided with the
     * width and height you are expected to scale your tablet
     * to, perhaps based on some user preferences.
     * If NULL, the tablet's specified range will be mapped directly
     * to that width and height for you, and you will not be
     * called back.
     }
    ient_CallBack: PHook;

    { Post-scaling coordinates and fractional coordinates.
     * DO NOT FILL THESE IN AT THE TIME THE EVENT IS WRITTEN!
     * Your driver will be called back and provided information
     * about the width and height of the area to scale the
     * tablet into.  It should scale the tablet coordinates
     * (perhaps based on some preferences controlling aspect
     * ratio, etc.) and place the scaled result into these
     * fields.  The ient_ScaledX and ient_ScaledY fields are
     * in screen-pixel resolution, but the origin ( [0,0]-point )
     * is not defined.  The ient_ScaledXFraction and
     * ient_ScaledYFraction fields represent sub-pixel position
     * information, and should be scaled to fill a UWORD fraction.
     }
    ient_ScaledX, ient_ScaledY,
    ient_ScaledXFraction, ient_ScaledYFraction: word;

    { Current tablet coordinates along each axis: }
    ient_TabletX, ient_TabletY: LongWord;

    { Tablet range along each axis.  For example, if ient_TabletX
     * can take values 0-999, ient_RangeX should be 1000.
     }
    ient_RangeX, ient_RangeY: LongWord;

    { Pointer to tag-list of additional tablet attributes.
     * See <intuition/intuition.h> for the tag values.
     }
    ient_TagList: PTagItem;
 end;


CONST
{   --- InputEvent.ie_Code ---   }
// IECLASS_RAWKEY
    IECODE_UP_PREFIX            = $80;
    IECODE_KEY_CODE_FIRST       = $00;
    IECODE_KEY_CODE_LAST        = $77;
    IECODE_COMM_CODE_FIRST      = $78;
    IECODE_COMM_CODE_LAST       = $7F;

// IECLASS_ANSI
    IECODE_C0_FIRST             = $00;
    IECODE_C0_LAST              = $1F;
    IECODE_ASCII_FIRST          = $20;
    IECODE_ASCII_LAST           = $7E;
    IECODE_ASCII_DEL            = $7F;
    IECODE_C1_FIRST             = $80;
    IECODE_C1_LAST              = $9F;
    IECODE_LATIN1_FIRST         = $A0;
    IECODE_LATIN1_LAST          = $FF;

// IECLASS_RAWMOUSE
    IECODE_LBUTTON              = $68; // also uses IECODE_UP_PREFIX
    IECODE_RBUTTON              = $69;
    IECODE_MBUTTON              = $6A;
    IECODE_NOBUTTON             = $FF;

// IECLASS_EVENT
    IECODE_NEWACTIVE            = $01; // active input window changed }
    IECODE_NEWSIZE              = $02; // resize of window }
    IECODE_REFRESH              = $03; // refresh of window }

// IECLASS_REQUESTER Codes
    IECODE_REQSET               = $01; // REQSET is broadcast when the first Requester (not subsequent ones) opens in the Window
    IECODE_REQCLEAR             = $00; // REQCLEAR is broadcast when the last Requester clears out of the Window

{   --- InputEvent.ie_Qualifier --- }
    IEQUALIFIER_LSHIFT          = $0001;
    IEQUALIFIER_RSHIFT          = $0002;
    IEQUALIFIER_CAPSLOCK        = $0004;
    IEQUALIFIER_CONTROL         = $0008;
    IEQUALIFIER_LALT            = $0010;
    IEQUALIFIER_RALT            = $0020;
    IEQUALIFIER_LCOMMAND        = $0040;
    IEQUALIFIER_RCOMMAND        = $0080;
    IEQUALIFIER_NUMERICPAD      = $0100;
    IEQUALIFIER_REPEAT          = $0200;
    IEQUALIFIER_INTERRUPT       = $0400;
    IEQUALIFIER_MULTIBROADCAST  = $0800;
    IEQUALIFIER_MIDBUTTON       = $1000;
    IEQUALIFIER_RBUTTON         = $2000;
    IEQUALIFIER_LEFTBUTTON      = $4000;
    IEQUALIFIER_RELATIVEMOUSE   = $8000;

    IEQUALIFIERB_LSHIFT         = 0;
    IEQUALIFIERB_RSHIFT         = 1;
    IEQUALIFIERB_CAPSLOCK       = 2;
    IEQUALIFIERB_CONTROL        = 3;
    IEQUALIFIERB_LALT           = 4;
    IEQUALIFIERB_RALT           = 5;
    IEQUALIFIERB_LCOMMAND       = 6;
    IEQUALIFIERB_RCOMMAND       = 7;
    IEQUALIFIERB_NUMERICPAD     = 8;
    IEQUALIFIERB_REPEAT         = 9;
    IEQUALIFIERB_INTERRUPT      = 10;
    IEQUALIFIERB_MULTIBROADCAST = 11;
    IEQUALIFIERB_MIDBUTTON      = 12;
    IEQUALIFIERB_RBUTTON        = 13;
    IEQUALIFIERB_LEFTBUTTON     = 14;
    IEQUALIFIERB_RELATIVEMOUSE  = 15;


{------ InputEvent ------------------------------------------------}

type
  PInputEvent = ^TInputEvent;
  TInputEvent = record
    ie_NextEvent: PInputEvent;
    ie_Class: byte;              // IECLASS_*
    ie_SubClass: byte;           // IESUBCLASS_*
    ie_Code: word;               // IECODE_*
    ie_Qualifier: word;          // IEQUALIFIER_*
    ie_Position: record
        case LongInt of
           0:(ie_xy: record
                ie_x: SmallInt;
                ie_y: SmallInt;
             end);
           1:(ie_addr: APTR);
           2:(ie_dead: record
                ie_prev1DownCode: byte;
                ie_prev1DownQual: byte;
                ie_prev2DownCode: byte;
                ie_prev2DownQual: byte;
             end);
        end;
    ie_TimeStamp: TTimeVal;
  end;

implementation

end.
