{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    input.device event definitions unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
unit inputevent;

interface

uses exec, utility, timer;


{ * input.device event definitions
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

const
  IECLASS_NULL = $00;
  IECLASS_RAWKEY = $01;
  IECLASS_RAWMOUSE = $02;
  IECLASS_EVENT = $03;
  IECLASS_POINTERPOS = $04;
  IECLASS_TIMER = $06;
  IECLASS_GADGETDOWN = $07;
  IECLASS_GADGETUP = $08;
  IECLASS_REQUESTER = $09;
  IECLASS_MENULIST = $0A;
  IECLASS_CLOSEWINDOW = $0B;
  IECLASS_SIZEWINDOW = $0C;
  IECLASS_REFRESHWINDOW = $0D;
  IECLASS_NEWPREFS = $0E;
  IECLASS_DISKREMOVED = $0F;
  IECLASS_DISKINSERTED = $10;
  IECLASS_ACTIVEWINDOW = $11;
  IECLASS_INACTIVEWINDOW = $12;
  IECLASS_NEWPOINTERPOS = $13;
  IECLASS_MENUHELP = $14;
  IECLASS_CHANGEWINDOW = $15;

{$ifndef IECLASS_NEWMOUSE}
 const
   IECLASS_NEWMOUSE = $16;
{$endif}


const
  IECLASS_MAX = $16;
  IESUBCLASS_COMPATIBLE = $00;
  IESUBCLASS_PIXEL = $01;
  IESUBCLASS_TABLET = $02;
  IESUBCLASS_NEWTABLET = $03;

type
  PIEPointerPixel = ^TIEPointerPixel;
  TIEPointerPixel = record
    iepp_Screen : Pointer;
    iepp_Position : record
      X : Integer;
      Y : Integer;
    end;
  end;

  PIEPointerTablet = ^TIEPointerTablet;
  TIEPointerTablet = record
    iept_Range : record
      X : Word;
      Y : Word;
    end;
    iept_Value : record
      X : Word;
      Y : Word;
    end;
    iept_Pressure : Integer;
  end;

  PIENewTablet = ^TIENewTablet;
  TIENewTablet = record
    ient_CallBack : PHook;
    ient_ScaledX : Word;
    ient_ScaledY : Word;
    ient_ScaledXFraction : Word;
    ient_ScaledYFraction : Word;
    ient_TabletX : DWord;
    ient_TabletY : DWord;
    ient_RangeX : DWord;
    ient_RangeY : DWord;
    ient_TagList : PTagItem;
  end;


const
  IECODE_UP_PREFIX = $80;
  IECODE_KEY_CODE_FIRST = $00;
  IECODE_KEY_CODE_LAST = $77;
  IECODE_COMM_CODE_FIRST = $78;
  IECODE_COMM_CODE_LAST = $7F;
  IECODE_C0_FIRST = $00;
  IECODE_C0_LAST = $1F;
  IECODE_ASCII_FIRST = $20;
  IECODE_ASCII_LAST = $7E;
  IECODE_ASCII_DEL = $7F;
  IECODE_C1_FIRST = $80;
  IECODE_C1_LAST = $9F;
  IECODE_LATIN1_FIRST = $A0;
  IECODE_LATIN1_LAST = $FF;
  IECODE_LBUTTON = $68;
  IECODE_RBUTTON = $69;
  IECODE_MBUTTON = $6A;
  IECODE_NOBUTTON = $FF;
  IECODE_NEWACTIVE = $01;
  IECODE_NEWSIZE = $02;
  IECODE_REFRESH = $03;
  IECODE_REQSET = $01;
  IECODE_REQCLEAR = $00;
  IEQUALIFIER_LSHIFT = $0001;
  IEQUALIFIER_RSHIFT = $0002;
  IEQUALIFIER_CAPSLOCK = $0004;
  IEQUALIFIER_CONTROL = $0008;
  IEQUALIFIER_LALT = $0010;
  IEQUALIFIER_RALT = $0020;
  IEQUALIFIER_LCOMMAND = $0040;
  IEQUALIFIER_RCOMMAND = $0080;
  IEQUALIFIER_NUMERICPAD = $0100;
  IEQUALIFIER_REPEAT = $0200;
  IEQUALIFIER_INTERRUPT = $0400;
  IEQUALIFIER_MULTIBROADCAST = $0800;
  IEQUALIFIER_MIDBUTTON = $1000;
  IEQUALIFIER_RBUTTON = $2000;
  IEQUALIFIER_LEFTBUTTON = $4000;
  IEQUALIFIER_RELATIVEMOUSE = $8000;
  IEQUALIFIERB_LSHIFT = 0;
  IEQUALIFIERB_RSHIFT = 1;
  IEQUALIFIERB_CAPSLOCK = 2;
  IEQUALIFIERB_CONTROL = 3;
  IEQUALIFIERB_LALT = 4;
  IEQUALIFIERB_RALT = 5;
  IEQUALIFIERB_LCOMMAND = 6;
  IEQUALIFIERB_RCOMMAND = 7;
  IEQUALIFIERB_NUMERICPAD = 8;
  IEQUALIFIERB_REPEAT = 9;
  IEQUALIFIERB_INTERRUPT = 10;
  IEQUALIFIERB_MULTIBROADCAST = 11;
  IEQUALIFIERB_MIDBUTTON = 12;
  IEQUALIFIERB_RBUTTON = 13;
  IEQUALIFIERB_LEFTBUTTON = 14;
  IEQUALIFIERB_RELATIVEMOUSE = 15;

{ * NewMouse events. }
{$ifndef NM_WHEEL_UP}
 const
   NM_WHEEL_UP = $7a;
{$endif}
{$ifndef NM_WHEEL_DOWN}
 const
   NM_WHEEL_DOWN = $7b;
{$endif}
{$ifndef NM_WHEEL_LEFT}
 const
   NM_WHEEL_LEFT = $7c;
{$endif}
{$ifndef NM_WHEEL_RIGHT}
 const
   NM_WHEEL_RIGHT = $7d;
{$endif}
{$ifndef NM_BUTTON_FOURTH}
 const
   NM_BUTTON_FOURTH = $7e;
{$endif}

type
  PInputEvent = ^TInputEvent;
  TInputEvent = record
    ie_NextEvent : PInputEvent;
    ie_Class : Byte;
    ie_SubClass : Byte;
    ie_Code : Word;
    ie_Qualifier : Word;
    ie_position : record
    case longint of
      0 : ( ie_xy : record
              ie_x : Integer;
              ie_y : Integer;
            end );
      1 : ( ie_addr : Pointer );
      2 : ( ie_dead : record
              ie_prev1DownCode : Byte;
              ie_prev1DownQual : Byte;
              ie_prev2DownCode : Byte;
              ie_prev2DownQual : Byte;
            end );
    end;
    ie_TimeStamp : TTimeval;
  end;


implementation

end.
