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

Procedure TPTCError.defaults;

Begin
  Fmessage := '';
End;

Constructor TPTCError.Create;

Begin
  defaults;
End;

Constructor TPTCError.Create(Const _message : String);

Begin
  Fmessage := _message;
  LOG('error', Self);
End;

Constructor TPTCError.Create(Const _message : String; Const error : TPTCError);

Begin
  Fmessage := _message + #13 + #10 + error.Fmessage;
  LOG('composite error', Self);
End;

Constructor TPTCError.Create(Const error : TPTCError);

Begin
  defaults;
  ASSign(error);
End;

Destructor TPTCError.Destroy;

Begin
  Inherited Destroy;
End;

Procedure TPTCError.Assign(Const error : TPTCError);

Begin
  If Self = error Then
    Raise TPTCError.Create('self assignment is not allowed');
  Fmessage := error.Fmessage;
End;

Function TPTCError.Equals(Const error : TPTCError) : Boolean;

Begin
  Equals := (Fmessage = error.Fmessage);
End;

Procedure TPTCError.report;

{$IFDEF WIN32}
Var
  txt : ShortString;
{$ENDIF WIN32}

Begin
  LOG('error report', Self);
  {$IFDEF GO32V2}
  RestoreTextMode;
  Writeln(stderr, Fmessage);
  {$ENDIF GO32V2}

  {$IFDEF WIN32}
  Win32Cursor_resurrect;
  txt := Fmessage + #0;
  MessageBox(0, @txt[1], 'Error', MB_OK Or MB_ICONERROR Or MB_SETFOREGROUND Or MB_TOPMOST);
  {$ENDIF WIN32}

  {$IFDEF UNIX}
  Writeln(stderr, 'error: ', Fmessage);
  {$ENDIF UNIX}

  Halt(1);
End;

Function TPTCError.message : String;

Begin
  message := Fmessage;
End;
