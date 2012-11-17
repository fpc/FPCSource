{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TDirectXDisplay = class
  private
    FOpen: Boolean;
    FFullscreen: Boolean;
    FMode: IPTCMode;
    FWindow: HWND;
    FDDraw: IDirectDraw2;
    FModesCount: Integer;
    FResolutionsCount: Integer;
    FModes: array of IPTCMode;
    FResolutions: array of IPTCMode;
    FInformation: string;

    FCursorSaved: Boolean;
    FCursorX, FCursorY: Integer;
{    FForeground: HWND;
    FForegroundRect: RECT;
    FForegroundPlacement: WINDOWPLACEMENT;}

    procedure internal_open(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);
    procedure internal_open_nearest(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);
    procedure internal_dispose_modes;
    procedure internal_dispose_resolutions;

    function GetModes: TPTCModeList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Setup(const ADDraw: IDirectDraw2);
    function Test(const AMode: IPTCMode; AExact: Boolean): Boolean;
    procedure Cooperative(AWindow: HWND; AFullscreen: Boolean);
    procedure Open;
    procedure Open(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);
    procedure Close;
    procedure Save;
    procedure Restore;
    property Modes: TPTCModeList read GetModes;
    property Mode: IPTCMode read FMode;
    property Fullscreen: Boolean read FFullscreen;
    property Information: string read FInformation;
  end;
