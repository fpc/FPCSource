{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

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
  TTextFX2Console = class(TPTCOpenGLLessConsole)
  private
    { data }
    FModes: array of IPTCMode;
    FTitle: string;
    FInformation: string;

    { flags }
    FOpen: Boolean;
    FLocked: Boolean;

    { option data }
    FDefaultWidth: Integer;
    FDefaultHeight: Integer;
    FDefaultFormat: IPTCFormat;

    { objects }
    FCopy: TPTCCopy;
    FClear: TPTCClear;

    FEventQueue: TEventQueue;

    { Dos objects }
    FKeyboard: TDosKeyboard;
    FMouse: TDosMouse;
    FPrimary: TPTCSurface;
    F160x100buffer: TPTCSurface;

    { internal console management routines }
    procedure internal_pre_open_setup(const _title: String);
    procedure internal_open_fullscreen_start;
    procedure internal_open_fullscreen(_width, _height: Integer; _format: IPTCFormat);
    procedure internal_open_fullscreen_finish(_pages: Integer);
    procedure internal_post_open_setup;
    procedure internal_reset;
    procedure internal_close;

    procedure HandleEvents;

    { console debug checks }
    procedure check_open;
    procedure check_unlocked;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure(const AFileName: string); override;
    function Option(const AOption: string): Boolean; override;
    function Modes: TPTCModeList; override;
    procedure open(const _title: string; _pages: Integer); overload; override;
    procedure open(const _title: string; _format: IPTCFormat;
                   _pages: Integer); overload; override;
    procedure open(const _title: string; _width, _height: Integer;
                   _format: IPTCFormat; _pages: Integer); overload; override;
    procedure open(const _title: string; _mode: IPTCMode;
                   _pages: Integer); overload; override;
    procedure close; override;
    procedure flush; override;
    procedure finish; override;
    procedure update; override;
    procedure update(_area: IPTCArea); override;
    procedure copy(surface: IPTCSurface); override;
    procedure copy(surface: IPTCSurface;
                   source, destination: IPTCArea); override;
    function lock: Pointer; override;
    procedure unlock; override;
    procedure load(const pixels: Pointer;
                   _width, _height, _pitch: Integer;
                   _format: IPTCFormat;
                   _palette: IPTCPalette); override;
    procedure load(const pixels: Pointer;
                   _width, _height, _pitch: Integer;
                   _format: IPTCFormat;
                   _palette: IPTCPalette;
                   source, destination: IPTCArea); override;
    procedure save(pixels: Pointer;
                   _width, _height, _pitch: Integer;
                   _format: IPTCFormat;
                   _palette: IPTCPalette); override;
    procedure save(pixels: Pointer;
                   _width, _height, _pitch: Integer;
                   _format: IPTCFormat;
                   _palette: IPTCPalette;
                   source, destination: IPTCArea); override;
    procedure clear; override;
    procedure Clear(AColor: IPTCColor); override;
    procedure clear(color: IPTCColor;
                    _area: IPTCArea); override;
    procedure Palette(_palette: IPTCPalette); override;
    function Palette: IPTCPalette; override;
    procedure Clip(_area: IPTCArea); override;
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
    function NextEvent(out event: IPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean; override;
    function PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): IPTCEvent; override;
  end;
