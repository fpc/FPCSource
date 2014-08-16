{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2013  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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
  TPrimaryModeEnum = (DIRECT, SECONDARY);
  TDirectXConsole = class(TPTCOpenGLLessConsole)
  private
    { title data }
{    FTitle: array [0..1023] of Char;}
    FTitle: AnsiString;

    { flags }
    FOpen: Boolean;
    FLocked: Boolean;
    FCursor: Boolean;
    FGrabMouse: Boolean;

    { option data }
    FFrequency: Integer;
    FDefaultWidth: Integer;
    FDefaultHeight: Integer;
//    FDefaultPages: Integer;
    FCenterWindow: Boolean;
    FSynchronizedUpdate: Boolean;
    FDefaultFormat: IPTCFormat;
    FOutputMode: (DEFAULT, WINDOWED, FULLSCREEN); {Output}
    FWindowMode: (RESIZABLE, FIXED); {Window}
    FPrimaryModeWindowed: TPrimaryModeEnum; {Primary}
    FPrimaryModeFullscreen: TPrimaryModeEnum; {Primary}
    FNearestMode: (NEAREST_DEFAULT, NEAREST_CENTERING, NEAREST_STRETCHING); {Nearest}
    FCursorMode: TWin32CursorMode; {Cursor}
    FFullscreen: Boolean;
    FInterceptClose: Boolean;

    { objects }
    FCopy: TPTCCopy;
    FEventQueue: TEventQueue;

    { Win32 objects }
    FWindow: TWin32Window;
    FKeyboard: TWin32Keyboard;
    FMouse: TWin32Mouse;
    FResize: TWin32Resize;
    FWin32Cursor: TWin32Cursor;

    { DirectX objects }
    FHook: TDirectXHook;
    FLibrary: TDirectXLibrary;
    FDisplay: TDirectXDisplay;
    FPrimary: TDirectXPrimary;

    { internal console management routines }
    procedure internal_open(const _title: string; window: HWND; const mode: IPTCMode; _pages: Integer; exact: Boolean);
    procedure internal_recycle(const _title: string; window: HWND; const mode: IPTCMode; _pages: Integer; exact: Boolean);
    procedure internal_close;
    procedure internal_shutdown;

    { internal console open management routines }
    procedure internal_open_start(const _title: string; window: HWND);
    procedure internal_open_finish;
    procedure internal_open_reset;

    { internal fullscreen open routines }
    procedure internal_open_fullscreen_start(window: HWND; const mode: IPTCMode; exact: Boolean);
    procedure internal_open_fullscreen_change(const mode: IPTCMode; exact: Boolean);
    procedure internal_open_fullscreen_surface(const mode: IPTCMode; _pages: Integer);
    procedure internal_open_fullscreen_finish;

    { internal windowed open routines }
    procedure internal_open_windowed_start(window: HWND; const mode: IPTCMode; exact: Boolean);
    procedure internal_open_windowed_change(const mode: IPTCMode; exact: Boolean);
    procedure internal_open_windowed_surface(const mode: IPTCMode; _pages: Integer);
    procedure internal_open_windowed_finish;

    { internal console open recycling routines }
    procedure internal_recycle_fullscreen(const _title: string; window: HWND; const mode: IPTCMode; _pages: Integer; exact: Boolean);
    procedure internal_recycle_windowed(const _title: string; window: HWND; const mode: IPTCMode; _pages: Integer; exact: Boolean);

{$IFDEF DEBUG}
    { debug }
    procedure CHECK_OPEN(AMsg: String);
    procedure CHECK_LOCK(AMsg: String);
{$ENDIF}

    { painting }
    procedure Paint;

    { cursor state }
    procedure UpdateCursor;

    procedure SetInterceptClose(AInterceptClose: Boolean);
    property InterceptClose: Boolean read FInterceptClose write SetInterceptClose;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure(const AFileName: String); override;
    function Option(const AOption: String): Boolean; override;
    function Modes: TPTCModeList; override;
    procedure Open(const ATitle: string; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AFormat: IPTCFormat;
                   APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AWidth, AHeight: Integer;
                   AFormat: IPTCFormat; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AMode: IPTCMode;
                   APages: Integer = 0); overload; override;
    procedure Close; override;
    procedure InternalResize(AWidth, AHeight: Integer); override;
    procedure Flush; override;
    procedure Finish; override;
    procedure Update; override;
    procedure Update(AArea: IPTCArea); override;
    procedure Copy(ASurface: IPTCSurface); override;
    procedure Copy(ASurface: IPTCSurface;
                   ASource, ADestination: IPTCArea); override;
    function Lock: Pointer; override;
    procedure Unlock; override;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;
    procedure Clear; override;
    procedure Clear(AColor: IPTCColor); override;
    procedure Clear(AColor: IPTCColor;
                    AArea: IPTCArea); override;
    procedure Palette(APalette: IPTCPalette); override;
    function Palette: IPTCPalette; override;
    procedure Clip(AArea: IPTCArea); override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetPitch: Integer; override;
    function GetPages: Integer; override;
    function GetArea: IPTCArea; override;
    function Clip: IPTCArea; override;
    function GetFormat: IPTCFormat; override;
    function GetName: string; override;
    function GetTitle: string; override;
    function GetInformation: string; override;
    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; override;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent; override;
  end;
