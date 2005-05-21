{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Program source for GTK debug server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program debugserver;
{$mode objfpc}
{$H+}
{ $apptype gui}

uses fpgtk,fpglib,fpgtkext,frmmain;

begin
  application := TFPgtkApplication.Create;
  application.MainWindow := TMainForm.Create;
  application.Run;
  application.Free;
end.
