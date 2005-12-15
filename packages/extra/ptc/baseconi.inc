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

Constructor TPTCBaseConsole.Create;

Begin
  FReleaseEnabled := False;
End;

Procedure TPTCBaseConsole.open(Const _title : String);{ Overload;}

Begin
  open(_title, 0);
End;

Procedure TPTCBaseConsole.open(Const _title : String; Const _format : TPTCFormat);{ Overload;}

Begin
  open(_title, _format, 0);
End;

Procedure TPTCBaseConsole.open(Const _title : String; _width, _height : Integer;
                               Const _format : TPTCFormat);{ Overload;}

Begin
  open(_title, _width, _height, _format, 0);
End;

Procedure TPTCBaseConsole.open(Const _title : String; Const _mode : TPTCMode);{ Overload;}

Begin
  open(_title, _mode, 0);
End;

Function TPTCBaseConsole.KeyPressed : Boolean;

Var
  k : TPTCKey;

Begin
  k := TPTCKey.Create;
  Try
    Repeat
      If internal_PeekKey(k) = False Then
        Exit(False);
      If FReleaseEnabled Or k.Press Then
        Exit(True);
      internal_ReadKey(k);
    Until False;
  Finally
    k.Free;
  End;
End;

Procedure TPTCBaseConsole.ReadKey(k : TPTCKey);

Begin
  Repeat
    internal_ReadKey(k);
  Until FReleaseEnabled Or k.Press;
End;

Function TPTCBaseConsole.PeekKey(k : TPTCKey) : Boolean;

Begin
  If KeyPressed Then
  Begin
    ReadKey(k);
    Result := True;
  End
  Else
    Result := False;
End;

Procedure TPTCBaseConsole.ReadKey;

Var
  k : TPTCKey;

Begin
  k := TPTCKey.Create;
  Try
    ReadKey(k);
  Finally
    k.Free;
  End;
End;
