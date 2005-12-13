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

Type
  TPTCCopy=Class(TObject)
  Private
    Procedure update;
    m_handle : THermesHandle;
    m_flags : LongInt;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure request(Const source, destination : TPTCFormat);
    Procedure palette(Const source, destination : TPTCPalette);
    Procedure copy(Const source_pixels : Pointer; source_x, source_y,
                   source_width, source_height, source_pitch : Integer;
                   destination_pixels : Pointer; destination_x, destination_y,
                   destination_width, destination_height, destination_pitch : Integer);
    Function option(Const _option : String) : Boolean;
  End;
