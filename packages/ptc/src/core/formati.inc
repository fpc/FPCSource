{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2006  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

constructor TPTCFormat.Create;
begin
  { defaults }
  FFormat.r := 0;
  FFormat.g := 0;
  FFormat.b := 0;
  FFormat.a := 0;
  FFormat.bits := 0;
  FFormat.indexed := False;

  { initialize hermes }
  if not Hermes_Init then
    raise TPTCError.Create('could not initialize hermes');
end;

constructor TPTCFormat.Create(ABits: Integer);
begin
  { check bits per pixel }
  if ABits <> 8 then
    raise TPTCError.Create('unsupported bits per pixel');

  { indexed color }
  FFormat.r := 0;
  FFormat.g := 0;
  FFormat.b := 0;
  FFormat.a := 0;
  FFormat.bits := ABits;
  FFormat.indexed := True;

  { initialize hermes }
  if not Hermes_Init then
    raise TPTCError.Create('could not initialize hermes');
end;

constructor TPTCFormat.Create(ABits: Integer;
                              ARedMask, AGreenMask, ABlueMask: Uint32;
                              AAlphaMask: Uint32 = 0);
begin
  { check bits per pixel }
  if ((ABits and 7) <> 0) or (ABits <= 0) or (ABits > 32) then
    raise TPTCError.Create('unsupported bits per pixel');

  { direct color }
  FFormat.r := ARedMask;
  FFormat.g := AGreenMask;
  FFormat.b := ABlueMask;
  FFormat.a := AAlphaMask;
  FFormat.bits := ABits;
  FFormat.indexed := False;

  { initialize hermes }
  if not Hermes_Init then
    raise TPTCError.Create('could not initialize hermes');
end;

constructor TPTCFormat.Create(const format: TPTCFormat);
begin
  { initialize hermes }
  if not Hermes_Init then
    raise TPTCError.Create('could not initialize hermes');

  Hermes_FormatCopy(@format.FFormat, @FFormat)
end;

{$INFO TODO: check what happens if Hermes_Init blows up in the constructor...}
destructor TPTCFormat.Destroy;

begin
  Hermes_Done;
  inherited Destroy;
end;

procedure TPTCFormat.Assign(const format: TPTCFormat);
begin
  if Self = format then
    exit;
  Hermes_FormatCopy(@format.Fformat, @Fformat);
end;

function TPTCFormat.Equals(const format: TPTCFormat): Boolean;
begin
  Result := Hermes_FormatEquals(@format.FFormat, @FFormat);
end;

function TPTCFormat.GetDirect: Boolean;
begin
  Result := not FFormat.indexed;
end;

function TPTCFormat.GetBytes: Integer;
begin
  Result := FFormat.bits shr 3;
end;
