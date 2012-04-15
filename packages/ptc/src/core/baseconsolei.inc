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
  TPTCBaseConsole = class(TInterfacedObject, IPTCConsole, IPTCSurface)
  private
    FReleaseEnabled: Boolean;

    procedure SetKeyReleaseEnabled(AValue: Boolean);
    function GetKeyReleaseEnabled: Boolean;

    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetPitch: Integer; virtual; abstract;
    function GetArea: IPTCArea; virtual; abstract;
    function GetFormat: IPTCFormat; virtual; abstract;

    function GetPages: Integer; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetTitle: string; virtual; abstract;
    function GetInformation: string; virtual; abstract;
  public
    constructor Create; virtual;

    procedure Copy(ASurface: IPTCSurface); virtual; abstract;
    procedure Copy(ASurface: IPTCSurface;
                   ASource, ADestination: IPTCArea); virtual; abstract;
    function Lock: Pointer; virtual; abstract;
    procedure Unlock; virtual; abstract;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); virtual; abstract;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); virtual; abstract;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); virtual; abstract;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Clear(AColor: IPTCColor); virtual; abstract;
    procedure Clear(AColor: IPTCColor;
                    AArea: IPTCArea); virtual; abstract;
    procedure Palette(APalette: IPTCPalette); virtual; abstract;
    procedure Clip(AArea: IPTCArea); virtual; abstract;
    function Option(const AOption: String): Boolean; virtual; abstract;
    function Clip: IPTCArea; virtual; abstract;
    function Palette: IPTCPalette; virtual; abstract;

    procedure Configure(const AFileName: String); virtual; abstract;
    function Modes: TPTCModeList; virtual; abstract;
    procedure Open(const ATitle: string; APages: Integer = 0); overload; virtual; abstract;
    procedure Open(const ATitle: string; AFormat: IPTCFormat;
                   APages: Integer = 0); overload; virtual; abstract;
    procedure Open(const ATitle: string; AWidth, AHeight: Integer;
                   AFormat: IPTCFormat; APages: Integer = 0); overload; virtual; abstract;
    procedure Open(const ATitle: string; AMode: IPTCMode;
                   APages: Integer = 0); overload; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Flush; virtual; abstract;
    procedure Finish; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Update(AArea: IPTCArea); virtual; abstract;

    { event handling }
    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; virtual; abstract;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent; virtual; abstract;

    { key handling }
    function KeyPressed: Boolean;
    function PeekKey(out AKey: IPTCKeyEvent): Boolean;
    procedure ReadKey(out AKey: IPTCKeyEvent);
    procedure ReadKey;
    property KeyReleaseEnabled: Boolean read GetKeyReleaseEnabled write SetKeyReleaseEnabled;

    property Pages: Integer read GetPages;
    property Name: string read GetName;
    property Title: string read GetTitle;
    property Information: string read GetInformation;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pitch: Integer read GetPitch;
    property Area: IPTCArea read GetArea;
    property Format: IPTCFormat read GetFormat;
  end;

constructor TPTCBaseConsole.Create;
begin
  FReleaseEnabled := False;
end;

function TPTCBaseConsole.KeyPressed: Boolean;
var
  k, kpeek: IPTCEvent;
begin
  repeat
    kpeek := PeekEvent(False, [PTCKeyEvent]);
    if kpeek = nil then
      exit(False);
    if FReleaseEnabled or (kpeek as IPTCKeyEvent).Press then
      exit(True);
    NextEvent(k, False, [PTCKeyEvent]);
  until False;
end;

procedure TPTCBaseConsole.ReadKey(out AKey: IPTCKeyEvent);
var
  ev: IPTCEvent;
begin
  repeat
    NextEvent(ev, True, [PTCKeyEvent]);
  until FReleaseEnabled or (ev as IPTCKeyEvent).Press;
  AKey := ev as IPTCKeyEvent;
end;

function TPTCBaseConsole.PeekKey(out AKey: IPTCKeyEvent): Boolean;
begin
  if KeyPressed then
  begin
    ReadKey(AKey);
    Result := True;
  end
  else
    Result := False;
end;

procedure TPTCBaseConsole.ReadKey;
var
  k: IPTCKeyEvent;
begin
  ReadKey(k);
end;

procedure TPTCBaseConsole.SetKeyReleaseEnabled(AValue: Boolean);
begin
  FReleaseEnabled := AValue;
end;

function TPTCBaseConsole.GetKeyReleaseEnabled: Boolean;
begin
  Result := FReleaseEnabled;
end;
