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
  TPTCColor = class(TInterfacedObject, IPTCColor)
  private
    FIndex: Integer;
    FRed, FGreen, FBlue, FAlpha: Single;
    FIndexed: Boolean;

    function GetIndex: Integer;
    function GetR: Single;
    function GetG: Single;
    function GetB: Single;
    function GetA: Single;
    function GetDirect: Boolean;
    function GetIndexed: Boolean;
    function Equals(AColor: IPTCColor): Boolean; reintroduce;
  public
    constructor Create;
    constructor Create(AIndex: Integer);
    constructor Create(ARed, AGreen, ABlue: Single; AAlpha: Single = 1);
    constructor Create(AColor: IPTCColor);
  end;

class function TPTCColorFactory.CreateNew: IPTCColor;
begin
  Result := TPTCColor.Create;
end;

class function TPTCColorFactory.CreateNew(AIndex: Integer): IPTCColor;
begin
  Result := TPTCColor.Create(AIndex);
end;

class function TPTCColorFactory.CreateNew(ARed, AGreen, ABlue: Single; AAlpha: Single = 1): IPTCColor;
begin
  Result := TPTCColor.Create(ARed, AGreen, ABlue, AAlpha);
end;

class function TPTCColorFactory.CreateNew(AColor: IPTCColor): IPTCColor;
begin
  Result := TPTCColor.Create(AColor);
end;

constructor TPTCColor.Create;
begin
  FIndexed := False;
  FIndex   := 0;
  FRed     := 0;
  FGreen   := 0;
  FBlue    := 0;
  FAlpha   := 1;
end;

constructor TPTCColor.Create(AIndex: Integer);
begin
  FIndexed := True;
  FIndex   := AIndex;
  FRed     := 0;
  FGreen   := 0;
  FBlue    := 0;
  FAlpha   := 1;
end;

constructor TPTCColor.Create(ARed, AGreen, ABlue: Single; AAlpha: Single = 1);
begin
  FIndexed := False;
  FIndex   := 0;
  FRed     := ARed;
  FGreen   := AGreen;
  FBlue    := ABlue;
  FAlpha   := AAlpha;
end;

constructor TPTCColor.Create(AColor: IPTCColor);
begin
  FIndex   := AColor.Index;
  FRed     := AColor.R;
  FGreen   := AColor.G;
  FBlue    := AColor.B;
  FAlpha   := AColor.A;
  FIndexed := AColor.Indexed;
end;

function TPTCColor.Equals(AColor: IPTCColor): Boolean;
begin
  Result := (FIndexed = AColor.Indexed) and
            (FIndex   = AColor.Index) and
            (FRed     = AColor.R) and
            (FGreen   = AColor.G) and
            (FBlue    = AColor.B) and
            (FAlpha   = AColor.A);
end;

function TPTCColor.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TPTCColor.GetR: Single;
begin
  Result := FRed;
end;

function TPTCColor.GetG: Single;
begin
  Result := FGreen;
end;

function TPTCColor.GetB: Single;
begin
  Result := FBlue;
end;

function TPTCColor.GetA: Single;
begin
  Result := FAlpha;
end;

function TPTCColor.GetDirect: Boolean;
begin
  Result := not FIndexed;
end;

function TPTCColor.GetIndexed: Boolean;
begin
  Result := FIndexed;
end;
