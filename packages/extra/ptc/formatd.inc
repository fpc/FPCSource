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
  TPTCFormat=Class(TObject)
  Private
    Fformat : THermesFormat;
  Public
    Constructor Create;
    Constructor Create(_bits : Integer);
    Constructor Create(_bits : Integer; _r, _g, _b : int32);
    Constructor Create(_bits : Integer; _r, _g, _b, _a : int32);
    Constructor Create(Const format : TPTCFormat);
    Destructor Destroy; Override;
    Procedure Assign(Const format : TPTCFormat);
    Function Equals(Const format : TPTCFormat) : Boolean;
    Property r : int32 read Fformat.r;
    Property g : int32 read Fformat.g;
    Property b : int32 read Fformat.b;
    Property a : int32 read Fformat.a;
    Property bits : Integer read Fformat.bits;
    Property indexed : Boolean read Fformat.indexed;
    Function direct : Boolean;
    Function bytes : Integer;
  End;
