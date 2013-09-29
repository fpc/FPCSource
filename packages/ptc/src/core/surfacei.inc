{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TPTCSurface = class(TInterfacedObject, IPTCSurface)
  private
    {data}
    FWidth: Integer;
    FHeight: Integer;
    FPitch: Integer;
    FArea: IPTCArea;
    FClip: IPTCArea;
    FFormat: IPTCFormat;
    FLocked: Boolean;
    FPixels: Pointer;
    {objects}
    FCopy: TPTCCopy;
    FClear: TPTCClear;
    FPalette: IPTCPalette;
  public
    constructor Create(AWidth, AHeight: Integer; AFormat: IPTCFormat);
    destructor Destroy; override;
    procedure Copy(ASurface: IPTCSurface);
    procedure Copy(ASurface: IPTCSurface;
                   ASource, ADestination: IPTCArea);
    function Lock: Pointer;
    procedure Unlock;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette);
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea);
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette);
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea);
    procedure Clear;
    procedure Clear(AColor: IPTCColor);
    procedure Clear(AColor: IPTCColor; AArea: IPTCArea);
    procedure Palette(APalette: IPTCPalette);
    function Palette: IPTCPalette;
    procedure Clip(AArea: IPTCArea);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPitch: Integer;
    function GetArea: IPTCArea;
    function Clip: IPTCArea;
    function GetFormat: IPTCFormat;
    function Option(const AOption: string): Boolean;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pitch: Integer read GetPitch;
    property Area: IPTCArea read GetArea;
    property Format: IPTCFormat read GetFormat;
  end;

class function TPTCSurfaceFactory.CreateNew(AWidth, AHeight: Integer; AFormat: IPTCFormat): IPTCSurface;
begin
  Result := TPTCSurface.Create(AWidth, AHeight, AFormat);
end;

constructor TPTCSurface.Create(AWidth, AHeight: Integer; AFormat: IPTCFormat);
var
  size: Integer;
begin
  FLocked := False;
  LOG('creating surface');
  LOG('width', AWidth);
  LOG('height', AHeight);
  LOG('format', AFormat);
  FWidth := AWidth;
  FHeight := AHeight;
  FFormat := AFormat;
  FArea := TPTCArea.Create(0, 0, AWidth, AHeight);
  FClip := TPTCArea.Create(FArea);
  FPitch := AWidth * AFormat.Bytes;
  size := AWidth * AHeight * AFormat.Bytes;
  if size = 0 then
    raise TPTCError.Create('zero surface size');
  FPixels := GetMem(size);
  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FPalette := TPTCPalette.Create;
  clear;
end;

destructor TPTCSurface.Destroy;
begin
  if FLocked then
  begin
    LOG('destroying surface that is still locked!!!');
  end;
  FCopy.Free;
  FClear.Free;
  FreeMem(FPixels);
  inherited Destroy;
end;

procedure TPTCSurface.Copy(ASurface: IPTCSurface);
begin
  ASurface.Load(FPixels, FWidth, FHeight, FPitch, FFormat, FPalette);
end;

procedure TPTCSurface.Copy(ASurface: IPTCSurface;
                           ASource, ADestination: IPTCArea);
begin
  ASurface.Load(FPixels, FWidth, FHeight, FPitch, FFormat, FPalette,
                ASource, ADestination);
end;

function TPTCSurface.Lock: Pointer;
begin
  if FLocked then
    raise TPTCError.Create('surface is already locked');
  FLocked := True;
  Result := FPixels;
end;

procedure TPTCSurface.Unlock;
begin
  if not FLocked then
    raise TPTCError.Create('surface is not locked');
  FLocked := False;
end;

procedure TPTCSurface.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
begin
  if FClip.Equals(FArea) then
  begin
    FCopy.Request(AFormat, FFormat);
    FCopy.Palette(APalette, FPalette);
    FCopy.Copy(APixels, 0, 0, AWidth, AHeight, APitch, FPixels, 0, 0,
               FWidth, FHeight, FPitch);
  end
  else
    Load(APixels, AWidth, AHeight, APitch, AFormat, APalette,
         TPTCArea.Create(0, 0, AWidth, AHeight), FArea);
end;

procedure TPTCSurface.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
var
  clipped_source: IPTCArea;
  clipped_destination: IPTCArea;
begin
  TPTCClipper.Clip(ASource, TPTCArea.Create(0, 0, AWidth, AHeight),
                   clipped_source,
                   ADestination, FClip,
                   clipped_destination);
  FCopy.Request(AFormat, FFormat);
  FCopy.Palette(APalette, FPalette);
  FCopy.Copy(APixels, clipped_source.left, clipped_source.top,
             clipped_source.width, clipped_source.height, APitch,
             FPixels, clipped_destination.left, clipped_destination.top,
             clipped_destination.width, clipped_destination.height, FPitch);
end;

procedure TPTCSurface.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
begin
  if FClip.Equals(FArea) then
  begin
    FCopy.Request(FFormat, AFormat);
    FCopy.Palette(FPalette, APalette);
    FCopy.Copy(FPixels, 0, 0, FWidth, FHeight, FPitch, APixels, 0, 0,
               AWidth, AHeight, APitch);
  end
  else
    Save(APixels, AWidth, AHeight, APitch, AFormat, APalette,
         FArea, TPTCArea.Create(0, 0, width, height));
end;

procedure TPTCSurface.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
var
  clipped_source: IPTCArea;
  clipped_destination: IPTCArea;
begin
  TPTCClipper.Clip(ASource, FClip,
                   clipped_source,
                   ADestination, TPTCArea.Create(0, 0, AWidth, AHeight),
                   clipped_destination);
  FCopy.Request(FFormat, AFormat);
  FCopy.Palette(FPalette, APalette);
  FCopy.Copy(FPixels, clipped_source.left, clipped_source.top,
             clipped_source.width, clipped_source.height, FPitch,
             APixels, clipped_destination.left, clipped_destination.top,
             clipped_destination.width, clipped_destination.height, APitch);
end;

procedure TPTCSurface.Clear;
var
  Color: IPTCColor;
begin
  if Format.Direct then
    Color := TPTCColor.Create(0, 0, 0, 0)
  else
    Color := TPTCColor.Create(0);

  Clear(Color);
end;

procedure TPTCSurface.Clear(AColor: IPTCColor);
begin
  Clear(AColor, FArea);
end;

procedure TPTCSurface.Clear(AColor: IPTCColor; AArea: IPTCArea);
var
  clipped_area: IPTCArea;
begin
  clipped_area := TPTCClipper.Clip(AArea, FClip);
  FClear.Request(FFormat);
  FClear.Clear(FPixels, clipped_area.left, clipped_area.top,
               clipped_area.width, clipped_area.height, FPitch, AColor);
end;

procedure TPTCSurface.Palette(APalette: IPTCPalette);
begin
  FPalette.Load(APalette.Data^);
end;

function TPTCSurface.Palette: IPTCPalette;
begin
  Result := FPalette;
end;

procedure TPTCSurface.Clip(AArea: IPTCArea);
begin
  FClip := TPTCClipper.Clip(AArea, FArea);
end;

function TPTCSurface.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TPTCSurface.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TPTCSurface.GetPitch: Integer;
begin
  Result := FPitch;
end;

function TPTCSurface.GetArea: IPTCArea;
begin
  Result := FArea;
end;

function TPTCSurface.Clip: IPTCArea;
begin
  Result := FClip;
end;

function TPTCSurface.GetFormat: IPTCFormat;
begin
  Result := FFormat;
end;

function TPTCSurface.Option(const AOption: string): Boolean;
begin
  Result := FCopy.Option(AOption);
end;
