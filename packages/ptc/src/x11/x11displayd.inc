{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2013 Nikolay Nikolov (nickysn@users.sourceforge.net)
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
                   PTC_X11_GRAB_MOUSE,
                   PTC_X11_INTERCEPT_WINDOW_CLOSE,
                   PTC_X11_RESIZABLE_WINDOW,
                   PTC_X11_USE_OPENGL);
  TX11Flags = set of TX11FlagsEnum;

type
  TX11Display = class
  protected
    { Conversion object }
    FCopy: TPTCCopy;
    FClear: TPTCClear;
    FPalette: IPTCPalette;

    FArea: IPTCArea;
    FClip: IPTCArea;

    FEventQueue: TEventQueue;

    FFlags: TX11Flags;
    FWidth, FHeight: Integer;
    FFormat: IPTCFormat;

    FDisplay: PDisplay;
    FScreen: Integer;

    FCMap: TColormap;
    FColours: PXColor;

    FFunctionKeys: PInteger;
    FNormalKeys: PInteger;

    function GetInterceptClose: Boolean;
    procedure SetInterceptClose(AInterceptClose: Boolean);

    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; virtual; abstract;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent; virtual; abstract;

    function GetX11DirectFormat(ABitsPerPixel: Integer;
                                AR, AG, AB: DWord;
                                AByteOrder: cint): IPTCFormat;
    function GetX11Format(AFormat: IPTCFormat): IPTCFormat;

    { initialise the keyboard mapping table }
    procedure SetKeyMapping;

    { Data access }
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPitch: Integer; virtual; abstract;
    function GetFormat: IPTCFormat;
    function GetArea: IPTCArea;
    procedure HandleKeyEvent(const e: TXKeyEvent);
    property InterceptClose: Boolean read GetInterceptClose write SetInterceptClose;
  public
    constructor Create(ADisplay: PDisplay; AScreen: Integer; const AFlags: TX11Flags); virtual;
    destructor Destroy; override;

    procedure Open(ATitle: string; AWidth, AHeight: Integer; AFormat: IPTCFormat; const AOpenGLAttributes: IPTCOpenGLAttributes); virtual; abstract;

    { This will always return a windowed console. The first version
      fills the whole window, the second one has a custom size }
    procedure Open(AWindow: TWindow; AFormat: IPTCFormat); virtual; abstract;
    procedure Open(AWindow: TWindow; AFormat: IPTCFormat; AX, AY, AWidth, AHeight: Integer); virtual; abstract;

    procedure Close; virtual; abstract;

    procedure InternalResize(AWidth, AHeight: Integer); virtual;

    procedure Update; virtual; abstract;
    procedure Update(AArea: IPTCArea); virtual; abstract;

    function Lock: Pointer; virtual; abstract;
    procedure Unlock; virtual; abstract;

    { load pixels to console }
    procedure Load(const APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat; APalette: IPTCPalette); virtual;
    procedure Load(const APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat; APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); virtual;

    { save console pixels }
    procedure Save(APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat; APalette: IPTCPalette); virtual;
    procedure Save(APixels: Pointer; AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat; APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); virtual;

    { clear surface }
    procedure Clear(AColor: IPTCColor); virtual;
    procedure Clear(AColor: IPTCColor; AArea: IPTCArea); virtual;

    { Console palette }
    procedure Palette(APalette: IPTCPalette); virtual; abstract;
    function Palette: IPTCPalette; virtual;

    { console clip area }
    procedure Clip(AArea: IPTCArea);

    { cursor control }
    procedure SetCursor(AVisible: Boolean); virtual; abstract;

    { mouse grab control }
    procedure SetMouseGrab(AGrabMouse: Boolean); virtual; abstract;

    { Data access }
    function Clip: IPTCArea;

    function IsFullScreen: Boolean; virtual; abstract;
    function IsOpen: Boolean; virtual; abstract;

    { set flags (only used internally now!) }
    procedure SetFlags(AFlags: TX11Flags);

    procedure GetModes(var AModes: TPTCModeList); virtual; abstract;

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
    property Area: IPTCArea read GetArea;
    property Format: IPTCFormat read GetFormat;

    procedure OpenGL_SwapBuffers; virtual;
    procedure OpenGL_SetSwapInterval(AInterval: Integer); virtual;
    function OpenGL_GetSwapInterval: Integer; virtual;
  end;
