{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  TX11FlagsEnum = (PTC_X11_FULLSCREEN,
                   PTC_X11_LEAVE_DISPLAY,
                   PTC_X11_LEAVE_WINDOW,
                   PTC_X11_TRY_DGA1,
                   PTC_X11_TRY_DGA2,
                   PTC_X11_TRY_XF86VIDMODE,
                   PTC_X11_TRY_XRANDR,
                   PTC_X11_TRY_XSHM,
                   PTC_X11_DITHER,
                   PTC_X11_FULLSCREEN_CURSOR_VISIBLE,
                   PTC_X11_WINDOWED_CURSOR_INVISIBLE,
                   PTC_X11_GRAB_MOUSE);
  TX11Flags = set of TX11FlagsEnum;

type
  TX11Display = class
  protected
    { Conversion object }
    FCopy: TPTCCopy;
    FClear: TPTCClear;
    FPalette: TPTCPalette;

    FArea: TPTCArea;
    FClip: TPTCArea;

    FEventQueue: TEventQueue;

    FFlags: TX11Flags;
    FWidth, FHeight: Integer;
    FFormat: TPTCFormat;

    FDisplay: PDisplay;
    FScreen: Integer;

    FCMap: TColormap;
    FColours: PXColor;

    FFunctionKeys: PInteger;
    FNormalKeys: PInteger;

    function NextEvent(var AEvent: TPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; virtual; abstract;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): TPTCEvent; virtual; abstract;

    function GetX11DirectFormat(ABitsPerPixel: Integer;
                                AR, AG, AB: DWord;
                                AByteOrder: cint): TPTCFormat;
    function GetX11Format(const AFormat: TPTCFormat): TPTCFormat;

    { initialise the keyboard mapping table }
    procedure SetKeyMapping;

    { Data access }
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPitch: Integer; virtual; abstract;
    function GetFormat: TPTCFormat;
    function GetArea: TPTCArea;
  public
    constructor Create(ADisplay: PDisplay; AScreen: Integer; const AFlags: TX11Flags); virtual;
    destructor Destroy; override;

    procedure Open(ATitle: string; AWidth, AHeight: Integer; const AFormat: TPTCFormat); virtual; abstract;

    { This will always return a windowed console. The first version
      fills the whole window, the second one has a custom size }
    procedure Open(AWindow: TWindow; const AFormat: TPTCFormat); virtual; abstract;
    procedure Open(AWindow: TWindow; const AFormat: TPTCFormat; AX, AY, AWidth, AHeight: Integer); virtual; abstract;

    procedure Close; virtual; abstract;

    procedure Update; virtual; abstract;
    procedure Update(const AArea: TPTCArea); virtual; abstract;

    function Lock: Pointer; virtual; abstract;
    procedure Unlock; virtual; abstract;

    { load pixels to console }
    procedure Load(const APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   const AFormat: TPTCFormat; const APalette: TPTCPalette); virtual;
    procedure Load(const APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   const AFormat: TPTCFormat; const APalette: TPTCPalette;
                   const ASource, ADestination: TPTCArea); virtual;

    { save console pixels }
    procedure Save(APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   const AFormat: TPTCFormat; const APalette: TPTCPalette); virtual;
    procedure Save(APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   const AFormat: TPTCFormat; const APalette: TPTCPalette;
                   const ASource, ADestination: TPTCArea); virtual;

    { clear surface }
    procedure Clear(const AColor: TPTCColor); virtual;
    procedure Clear(const AColor: TPTCColor; const AArea: TPTCArea); virtual;

    { Console palette }
    procedure Palette(const APalette: TPTCPalette); virtual; abstract;
    function Palette: TPTCPalette; virtual;

    { console clip area }
    procedure Clip(const AArea: TPTCArea);

    { cursor control }
    procedure SetCursor(AVisible: Boolean); virtual; abstract;

    { mouse grab control }
    procedure SetMouseGrab(AGrabMouse: Boolean); virtual; abstract;

    { Data access }
    function Clip: TPTCArea;

    function IsFullScreen: Boolean; virtual; abstract;

    { set flags (only used internally now!) }
    procedure SetFlags(AFlags: TX11Flags);

    procedure GetModes(var AModes: TPTCModeDynArray); virtual; abstract;

    { X11 helper functions for your enjoyment }

    { return the display we are using }
    function GetX11Display: PDisplay;

    { return the screen we are using }
    function GetX11Screen: Integer;

    { return our window (0 if DGA) }
    function GetX11Window: TWindow; virtual; abstract;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pitch: Integer read GetPitch;
    property Area: TPTCArea read GetArea;
    property Format: TPTCFormat read GetFormat;
  end;
