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

Constructor TPTCMode.Create;

Begin
  m_format := Nil;
  m_format := TPTCFormat.Create;
  m_valid := False;
  m_width := 0;
  m_height := 0;
End;

Constructor TPTCMode.Create(_width, _height : Integer; Const _format : TPTCFormat);

Begin
  m_format := Nil;
  m_valid := True;
  m_width := _width;
  m_height := _height;
  m_format := TPTCFormat.Create(_format);
End;

Constructor TPTCMode.Create(Const mode : TPTCMode);

Begin
  m_format := Nil;
  m_format := TPTCFormat.Create;
  ASSign(mode);
End;

Destructor TPTCMode.Destroy;

Begin
  m_format.Free;
  Inherited Destroy;
End;

Procedure TPTCMode.Assign(Const mode : TPTCMode);

Begin
  If Self = mode Then
    Raise TPTCError.Create('self assignment is not allowed');
  m_valid := mode.valid;
  m_width := mode.width;
  m_height := mode.height;
  m_format.ASSign(mode.format);
End;

Function TPTCMode.Equals(Const mode : TPTCMode) : Boolean;

Begin
  Equals := (m_valid = mode.m_valid) And
            (m_width = mode.m_width) And
            (m_height = mode.m_height) And
             m_format.Equals(mode.m_format);
End;
