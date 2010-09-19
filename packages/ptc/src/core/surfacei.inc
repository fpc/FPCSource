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

constructor TPTCSurface.Create(AWidth, AHeight: Integer; const AFormat: TPTCFormat);
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
  FFormat := TPTCFormat.Create(AFormat);
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
  FPalette.Free;
  FClip.Free;
  FArea.Free;
  FFormat.Free;
  FreeMem(FPixels);
  inherited Destroy;
end;

procedure TPTCSurface.Copy(ASurface: TPTCBaseSurface);
begin
  ASurface.Load(FPixels, FWidth, FHeight, FPitch, FFormat, FPalette);
end;

procedure TPTCSurface.Copy(ASurface: TPTCBaseSurface;
                           const ASource, ADestination: TPTCArea);
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
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
var
  Area_: TPTCArea;
begin
  if FClip.Equals(FArea) then
  begin
    FCopy.Request(AFormat, FFormat);
    FCopy.Palette(APalette, FPalette);
    FCopy.Copy(APixels, 0, 0, AWidth, AHeight, APitch, FPixels, 0, 0,
               FWidth, FHeight, FPitch);
  end
  else
  begin
    Area_ := TPTCArea.Create(0, 0, AWidth, AHeight);
    try
      Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, Area_, FArea);
    finally
      Area_.Free;
    end;
  end;
end;

procedure TPTCSurface.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
var
  clipped_source: TPTCArea = nil;
  clipped_destination: TPTCArea = nil;
  area_: TPTCArea = nil;
begin
  try
    clipped_source := TPTCArea.Create;
    clipped_destination := TPTCArea.Create;
    area_ := TPTCArea.Create(0, 0, AWidth, AHeight);
    TPTCClipper.Clip(ASource, area_, clipped_source, ADestination, FClip,
                     clipped_destination);
    FCopy.Request(AFormat, FFormat);
    FCopy.Palette(APalette, FPalette);
    FCopy.Copy(APixels, clipped_source.left, clipped_source.top,
               clipped_source.width, clipped_source.height, APitch,
               FPixels, clipped_destination.left, clipped_destination.top,
               clipped_destination.width, clipped_destination.height, FPitch);
  finally
    clipped_source.Free;
    clipped_destination.Free;
    area_.Free;
  end;
end;

procedure TPTCSurface.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
var
  area_: TPTCArea;
begin
  if FClip.Equals(FArea) then
  begin
    FCopy.Request(FFormat, AFormat);
    FCopy.Palette(FPalette, APalette);
    FCopy.Copy(FPixels, 0, 0, FWidth, FHeight, FPitch, APixels, 0, 0,
               AWidth, AHeight, APitch);
  end
  else
  begin
    area_ := TPTCArea.Create(0, 0, width, height);
    try
      Save(APixels, AWidth, AHeight, APitch, AFormat, APalette, FArea, area_);
    finally
      area_.Free;
    end;
  end;
end;

procedure TPTCSurface.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
var
  clipped_source: TPTCArea = nil;
  clipped_destination: TPTCArea = nil;
  area_: TPTCArea = nil;
begin
  try
    clipped_source := TPTCArea.Create;
    clipped_destination := TPTCArea.Create;
    area_ := TPTCArea.Create(0, 0, AWidth, AHeight);
    TPTCClipper.Clip(ASource, FClip, clipped_source, ADestination, area_,
                     clipped_destination);
    FCopy.Request(FFormat, AFormat);
    FCopy.Palette(FPalette, APalette);
    FCopy.Copy(FPixels, clipped_source.left, clipped_source.top,
               clipped_source.width, clipped_source.height, FPitch,
               APixels, clipped_destination.left, clipped_destination.top,
               clipped_destination.width, clipped_destination.height, APitch);
  finally
    clipped_source.Free;
    clipped_destination.Free;
    area_.Free;
  end;
end;

procedure TPTCSurface.Clear;
var
  Color: TPTCColor;
begin
  if Format.Direct then
    Color := TPTCColor.Create(0, 0, 0, 0)
  else
    Color := TPTCColor.Create(0);
  try
    Clear(Color);
  finally
    Color.Free;
  end;
end;

procedure TPTCSurface.Clear(const AColor: TPTCColor);
begin
  Clear(AColor, FArea);
end;

procedure TPTCSurface.Clear(const AColor: TPTCColor; const AArea: TPTCArea);
var
  clipped_area: TPTCArea;
begin
  clipped_area := TPTCClipper.clip(AArea, FClip);
  try
    FClear.Request(FFormat);
    FClear.Clear(FPixels, clipped_area.left, clipped_area.top,
                 clipped_area.width, clipped_area.height, FPitch, AColor);
  finally
    clipped_area.Free;
  end;
end;

procedure TPTCSurface.Palette(const APalette: TPTCPalette);
begin
  FPalette.Load(APalette.data^);
end;

function TPTCSurface.Palette: TPTCPalette;
begin
  Result := FPalette;
end;

procedure TPTCSurface.Clip(const AArea: TPTCArea);
var
  tmp: TPTCArea;
begin
  tmp := TPTCClipper.Clip(AArea, FArea);
  try
    FClip.Assign(tmp);
  finally
    tmp.Free;
  end;
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

function TPTCSurface.GetArea: TPTCArea;
begin
  Result := FArea;
end;

function TPTCSurface.Clip: TPTCArea;
begin
  Result := FClip;
end;

function TPTCSurface.GetFormat: TPTCFormat;
begin
  Result := FFormat;
end;

function TPTCSurface.Option(const AOption: string): Boolean;
begin
  Result := FCopy.Option(AOption);
end;
