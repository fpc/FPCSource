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

Procedure X11Check(result : TStatus);

{Var
  ErrStr : String;}

Begin
  {todo: fix X11 error handling}
{  If result = Success Then
    Exit;
  Case result Of
    BadRequest        : ErrStr := 'BadRequest';
    BadValue          : ErrStr := 'BadValue';
    BadWindow         : ErrStr := 'BadWindow';
    BadPixmap         : ErrStr := 'BadPixmap';
    BadAtom           : ErrStr := 'BadAtom';
    BadCursor         : ErrStr := 'BadCursor';
    BadFont           : ErrStr := 'BadFont';
    BadMatch          : ErrStr := 'BadMatch';
    BadDrawable       : ErrStr := 'BadDrawable';
    BadAccess         : ErrStr := 'BadAccess';
    BadAlloc          : ErrStr := 'BadAlloc';
    BadColor          : ErrStr := 'BadColor';
    BadGC             : ErrStr := 'BadGC';
    BadIDChoice       : ErrStr := 'BadIDChoice';
    BadName           : ErrStr := 'BadName';
    BadLength         : ErrStr := 'BadLength';
    BadImplementation : ErrStr := 'BadImplementation';
    Else
      Str(result, ErrStr);
  End;
  Raise TPTCError.Create('X11 Error: ' + ErrStr);}
End;

Procedure X11Check(result : TStatus; Const message : String);

Begin
  Try
    X11Check(result);
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(message, error);
  End;
End;
