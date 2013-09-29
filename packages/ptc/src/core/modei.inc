{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2007, 2009-2012  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TPTCMode = class(TInterfacedObject, IPTCMode)
  private
    FValid: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FFormat: IPTCFormat;
    function GetValid: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetFormat: IPTCFormat;
  public
    constructor Create;
    constructor Create(AWidth, AHeight: Integer; AFormat: IPTCFormat);
    constructor Create(AMode: IPTCMode);
{    procedure Assign(const mode: TPTCMode);}
    function Equals(AMode: IPTCMode): Boolean; reintroduce;
{    property Valid: Boolean read GetValid;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Format: IPTCFormat read GetFormat;}
  end;

class function TPTCModeFactory.CreateNew: IPTCMode;
begin
  Result := TPTCMode.Create;
end;

class function TPTCModeFactory.CreateNew(AWidth, AHeight: Integer; AFormat: IPTCFormat): IPTCMode;
begin
  Result := TPTCMode.Create(AWidth, AHeight, AFormat);
end;

class function TPTCModeFactory.CreateNew(AMode: IPTCMode): IPTCMode;
begin
  Result := TPTCMode.Create(AMode);
end;

constructor TPTCMode.Create;
begin
  FFormat := TPTCFormat.Create;
  FWidth := 0;
  FHeight := 0;
  FValid := False;
end;

constructor TPTCMode.Create(AWidth, AHeight: Integer; AFormat: IPTCFormat);
begin
  FFormat := AFormat;
  FWidth := AWidth;
  FHeight := AHeight;
  FValid := True;
end;

constructor TPTCMode.Create(AMode: IPTCMode);
begin
  FFormat := AMode.Format;
  FWidth := AMode.Width;
  FHeight := AMode.Height;
  FValid := AMode.Valid;
end;

{procedure TPTCMode.Assign(const mode: TPTCMode);
begin
  FFormat := mode.FFormat;
  FWidth := mode.FWidth;
  FHeight := mode.FHeight;
  FValid := mode.FValid;
end;
}
function TPTCMode.Equals(AMode: IPTCMode): Boolean;
begin
  Result := (FValid = AMode.Valid) and
            (FWidth = AMode.Width) and
            (FHeight = AMode.Height) and
             FFormat.Equals(AMode.Format);
end;

function TPTCMode.GetValid: Boolean;
begin
  Result := FValid;
end;

function TPTCMode.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TPTCMode.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TPTCMode.GetFormat: IPTCFormat;
begin
  Result := FFormat;
end;
