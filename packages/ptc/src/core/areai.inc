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
  TPTCArea = class(TInterfacedObject, IPTCArea)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetRight: Integer;
    function GetBottom: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function Equals(AArea: IPTCArea): Boolean; reintroduce;
  public
    constructor Create;
    constructor Create(ALeft, ATop, ARight, ABottom: Integer);
    constructor Create(AArea: IPTCArea);
  end;

class function TPTCAreaFactory.CreateNew: IPTCArea;
begin
  Result := TPTCArea.Create;
end;

class function TPTCAreaFactory.CreateNew(ALeft, ATop, ARight, ABottom: Integer): IPTCArea;
begin
  Result := TPTCArea.Create(ALeft, ATop, ARight, ABottom);
end;

class function TPTCAreaFactory.CreateNew(AArea: IPTCArea): IPTCArea;
begin
  Result := TPTCArea.Create(AArea);
end;

constructor TPTCArea.Create(ALeft, ATop, ARight, ABottom: Integer);
begin
  if ALeft < ARight then
  begin
    FLeft := ALeft;
    FRight := ARight;
  end
  else
  begin
    FLeft := ARight;
    FRight := ALeft;
  end;
  if ATop < ABottom then
  begin
    FTop := ATop;
    FBottom := ABottom;
  end
  else
  begin
    FTop := ABottom;
    FBottom := ATop;
  end;
end;

constructor TPTCArea.Create;
begin
  FLeft   := 0;
  FRight  := 0;
  FTop    := 0;
  FBottom := 0;
end;

constructor TPTCArea.Create(AArea: IPTCArea);
begin
  FLeft   := AArea.Left;
  FTop    := AArea.Top;
  FRight  := AArea.Right;
  FBottom := AArea.Bottom;
end;

function TPTCArea.Equals(AArea: IPTCArea): Boolean;
begin
  Result := (FLeft   = AArea.Left) and
            (FTop    = AArea.Top) and
            (FRight  = AArea.Right) and
            (FBottom = AArea.Bottom);
end;

function TPTCArea.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TPTCArea.GetTop: Integer;
begin
  Result := FTop;
end;

function TPTCArea.GetRight: Integer;
begin
  Result := FRight;
end;

function TPTCArea.GetBottom: Integer;
begin
  Result := FBottom;
end;

function TPTCArea.GetWidth: Integer;
begin
  Result := FRight - FLeft;
end;

function TPTCArea.GetHeight: Integer;
begin
  Result := FBottom - FTop;
end;
