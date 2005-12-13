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

Constructor TPTCArea.Create(_left, _top, _right, _bottom : Integer);

Begin
  If _left < _right Then
  Begin
    Fleft := _left;
    Fright := _right;
  End
  Else
  Begin
    Fleft := _right;
    Fright := _left;
  End;
  If _top < _bottom Then
  Begin
    Ftop := _top;
    Fbottom := _bottom;
  End
  Else
  Begin
    Ftop := _bottom;
    Fbottom := _top;
  End;
End;

Constructor TPTCArea.Create;

Begin
  Fleft := 0;
  Fright := 0;
  Ftop := 0;
  Fbottom := 0;
End;

Constructor TPTCArea.Create(Const area : TPTCArea);

Begin
  ASSign(area);
End;

Destructor TPTCArea.Destroy;

Begin
  Inherited Destroy;
End;

Procedure TPTCArea.Assign(Const area : TPTCArea);

Begin
  If Self = area Then
    Raise TPTCError.Create('self assignment is not allowed');
  Fleft := area.Fleft;
  Ftop := area.Ftop;
  Fright := area.Fright;
  Fbottom := area.Fbottom;
End;

Function TPTCArea.Equals(Const area : TPTCArea) : Boolean;

Begin
  Equals := (Fleft = area.Fleft) And
	    (Ftop = area.Ftop) And
	    (Fright = area.Fright) And
	    (Fbottom = area.Fbottom);
End;

Function TPTCArea.width : Integer;

Begin
  width := Fright - Fleft;
End;

Function TPTCArea.height : Integer;

Begin
  height := Fbottom - Ftop;
End;
