{ Copyright (C) <2005> <Andrew Haines> chmcmd.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING, included in this distribution,
  for details about the copyright.
}
program chmcmd;

{$mode objfpc}{$H+}

uses
  Classes, chmfilewriter;

var
  OutStream: TFileStream;
  Project: TChmProject;
begin
  if Paramcount = 1 then begin
    Project := TChmProject.Create;
    Project.LoadFromFile(ParamStr(1));
    OutStream := TFileStream.Create(Project.OutputFileName, fmCreate, fmOpenWrite);
    Project.WriteChm(OutStream);
    OutStream.Free;
    Project.Free;
  end;
end.

