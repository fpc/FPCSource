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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

type
  TCGAConsole = class(TPTCOpenGLLessConsole)
  private
    { data }
    m_modes: array of IPTCMode;
    m_title: AnsiString;
    m_information: AnsiString;

    { flags }
    m_open: Boolean;
    m_locked: Boolean;

    { option data }
    m_default_width: Integer;
    m_default_height: Integer;
    m_default_format: IPTCFormat;

    { objects }
    m_copy: TPTCCopy;
    m_clear: TPTCClear;

    FEventQueue: TEventQueue;

    { Dos objects }
    m_keyboard: TDosKeyboard;
    FMouse: TDosMouse;
    m_primary: TPTCSurface;

    { internal console management routines }
    procedure internal_pre_open_setup(const _title: AnsiString);
    procedure internal_open_fullscreen_start;
    procedure internal_open_fullscreen(_width, _height: Integer; const _format: IPTCFormat);
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
    procedure Configure(const AFileName: AnsiString); override;
    function option(const _option: AnsiString): Boolean; override;
    function modes: TPTCModeList; override;
    procedure open(const _title: AnsiString; _pages: Integer); overload; override;
    procedure open(const _title: AnsiString; _format: IPTCFormat;
                   _pages: Integer); overload; override;
    procedure open(const _title: AnsiString; _width, _height: Integer;
                   _format: IPTCFormat; _pages: Integer); overload; override;
    procedure open(const _title: AnsiString; _mode: IPTCMode;
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
    procedure clear(color: IPTCColor); override;
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
    function GetName: AnsiString; override;
    function GetTitle: AnsiString; override;
    function GetInformation: AnsiString; override;
    function NextEvent(out event: IPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean; override;
    function PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): IPTCEvent; override;
  end;
