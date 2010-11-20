{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TDirectXPrimary = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FPages: Integer;
    FArea: TPTCArea;
    FClip: TPTCArea;
    FFormat: TPTCFormat;

    FActive: Boolean;
    FBlocking: Boolean;
    FCentering: Boolean;
    FFullscreen: Boolean;
    FSynchronize: Boolean;

    FClear: TPTCClear;

    FWindow: TWin32Window;

    FLocked: Pointer;

    FPalette: TPTCPalette;

    FPrimaryWidth: Integer;
    FPrimaryHeight: Integer;

    FSecondaryWidth: Integer;
    FSecondaryHeight: Integer;

    FDD2: IDirectDraw2;

    FDDSPrimary: IDirectDrawSurface;
    FDDSPrimaryBack: IDirectDrawSurface;
    FDDSPrimaryPage: array [0..31] of IDirectDrawSurface;

    FDDSSecondary: IDirectDrawSurface;

    FDDC: IDirectDrawClipper;

    FBack, FFront: IDirectDrawSurface;

    function Pack(const AColor: TPTCColor; const AFormat: TPTCFormat): Uint32;
    procedure Analyse(AMask: Uint32; out ABase, ASize: Integer);
    function GetDDS: IDirectDrawSurface;
    function GetPitch: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(AWindow: TWin32Window; const ADDraw2: IDirectDraw2);
    procedure Primary(APages: Integer; AVideo, AFullscreen, APalette, AComplex: Boolean);
    procedure Secondary(AWidth, AHeight: Integer);
    procedure Synchronize(AUpdate: Boolean);
    procedure Centering(ACenter: Boolean);
    procedure Close;

    procedure Update;

    function Lock: Pointer;
    procedure Unlock;

    procedure Clear;
    procedure Clear(const AColor: TPTCColor; const AArea: TPTCArea);

    procedure Palette(const APalette: TPTCPalette);
    function Palette: TPTCPalette;

    procedure Clip(const AArea: TPTCArea);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pages: Integer read FPages;
    property Pitch: Integer read GetPitch;
    property Area: TPTCArea read FArea;
    function Clip: TPTCArea;
    property Format: TPTCFormat read FFormat;

    property DDS: IDirectDrawSurface read GetDDS;
    property DDSPrimary: IDirectDrawSurface read FDDSPrimary;
    property DDSSecondary: IDirectDrawSurface read FDDSSecondary;

    procedure Activate;
    procedure Deactivate;
    property Active: Boolean read FActive;
    procedure Block;
    procedure Save;
    procedure Restore;

    procedure Paint;

    procedure Blocking(ABlocking: Boolean);
    
    property Fullscreen: Boolean read FFullscreen;
  end;
