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

Constructor TPTCColor.Create;

Begin
  m_indexed := False;
  m_direct := False;
  m_index := 0;
  m_r := 0;
  m_g := 0;
  m_b := 0;
  m_a := 1;
End;

Constructor TPTCColor.Create(_index : Integer);

Begin
  m_indexed := True;
  m_direct := False;
  m_index := _index;
  m_r := 0;
  m_g := 0;
  m_b := 0;
  m_a := 1;
End;

Constructor TPTCColor.Create(_r, _g, _b, _a : Real);

Begin
  m_indexed := False;
  m_direct := True;
  m_index := 0;
  m_r := _r;
  m_g := _g;
  m_b := _b;
  m_a := _a;
End;

Constructor TPTCColor.Create(_r, _g, _b : Real);

Begin
  m_indexed := False;
  m_direct := True;
  m_index := 0;
  m_r := _r;
  m_g := _g;
  m_b := _b;
  m_a := 1;
End;

Constructor TPTCColor.Create(Const color : TPTCColor);

Begin
  ASSign(color);
End;

Destructor TPTCColor.Destroy;

Begin
  Inherited Destroy;
End;

Procedure TPTCColor.Assign(Const color : TPTCColor);

Begin
  If Self = color Then
    Raise TPTCError.Create('self assignment is not allowed');
  m_index := color.index;
  m_r := color.r;
  m_g := color.g;
  m_b := color.b;
  m_a := color.a;
  m_direct := color.direct;
  m_indexed := color.indexed;
End;

Function TPTCColor.Equals(Const color : TPTCColor) : Boolean;

Begin
  If m_direct And color.m_direct Then
    If (m_r = color.m_r) And (m_g = color.m_g) And
       (m_b = color.m_b) And (m_a = color.m_a) Then
      Equals := True
    Else
      Equals := False
  Else
    If m_index = color.m_index Then
      Equals := True
    Else
      Equals := False;
End;
