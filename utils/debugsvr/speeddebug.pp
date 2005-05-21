{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    speed test for debug server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program testdebug;

uses dbugintf,sysutils;

Var
  i : integer;
  S : String;

begin
  For I:=1 to 10000 do
    begin
    S:=Format('Message no %d',[i]);
    SendDebugEx(S,TDebugLevel(I mod 3));
    end;
end.
