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
  TPTCColor=Class(TObject)
  Private
    m_index : Integer;
    m_r, m_g, m_b, m_a : Single;
    m_direct : Boolean;
    m_indexed : Boolean;
  Public
    Constructor Create;
    Constructor Create(_index : Integer);
    Constructor Create(_r, _g, _b, _a : Real);
    Constructor Create(_r, _g, _b : Real);
    Constructor Create(Const color : TPTCColor);
    Destructor Destroy; Override;
    Procedure Assign(Const color : TPTCColor);
    Function Equals(Const color : TPTCColor) : Boolean;
    Property index : Integer read m_index;
    Property r : Single read m_r;
    Property g : Single read m_g;
    Property b : Single read m_b;
    Property a : Single read m_a;
    Property direct : Boolean read m_direct;
    Property indexed : Boolean read m_indexed;
  End;
