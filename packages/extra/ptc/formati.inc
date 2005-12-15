{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Constructor TPTCFormat.Create;

Begin
  { defaults }
  Fformat.r := 0;
  Fformat.g := 0;
  Fformat.b := 0;
  Fformat.a := 0;
  Fformat.bits := 0;
  Fformat.indexed := False;

  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');
End;

Constructor TPTCFormat.Create(_bits : Integer);

Begin
  { check bits per pixel }
  If _bits <> 8 Then
    Raise TPTCError.Create('unsupported bits per pixel');

  { indexed color }
  Fformat.r := 0;
  Fformat.g := 0;
  Fformat.b := 0;
  Fformat.a := 0;
  Fformat.bits := _bits;
  Fformat.indexed := True;

  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');
End;

Constructor TPTCFormat.Create(_bits : Integer; _r, _g, _b, _a : int32);

Begin
  { check bits per pixel }
  If ((_bits And 7) <> 0) Or (_bits <= 0) Or (_bits > 32) Then
    Raise TPTCError.Create('unsupported bits per pixel');

  { direct color }
  Fformat.r := _r;
  Fformat.g := _g;
  Fformat.b := _b;
  Fformat.a := _a;
  Fformat.bits := _bits;
  Fformat.indexed := False;

  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');
End;

Constructor TPTCFormat.Create(_bits : Integer; _r, _g, _b : int32);

Begin
  { check bits per pixel }
  If ((_bits And 7) <> 0) Or (_bits <= 0) Or (_bits > 32) Then
    Raise TPTCError.Create('unsupported bits per pixel');

  { direct color }
  Fformat.r := _r;
  Fformat.g := _g;
  Fformat.b := _b;
  Fformat.a := 0;
  Fformat.bits := _bits;
  Fformat.indexed := False;

  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');
End;

Constructor TPTCFormat.Create(Const format : TPTCFormat);

Begin
  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');

  Hermes_FormatCopy(@format.Fformat, @Fformat)
End;

Destructor TPTCFormat.Destroy;

Begin
  Hermes_Done;
  Inherited Destroy;
End;

Procedure TPTCFormat.Assign(Const format : TPTCFormat);

Begin
  If Self = format Then
    Raise TPTCError.Create('self assignment is not allowed');
  Hermes_FormatCopy(@format.Fformat, @Fformat)
End;

Function TPTCFormat.Equals(Const format : TPTCFormat) : Boolean;

Begin
  Equals := Hermes_FormatEquals(@format.Fformat, @Fformat);
End;

Function TPTCFormat.direct : Boolean;

Begin
  direct := Not Fformat.indexed;
End;

Function TPTCFormat.bytes : Integer;

Begin
  bytes := Fformat.bits Shr 3;
End;
