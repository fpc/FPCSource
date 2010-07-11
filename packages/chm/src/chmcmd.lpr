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
  Classes, Sysutils, chmfilewriter;

Procedure Usage;

begin
  Writeln(StdErr,'Usage: chmcmd  <filename>');
  Halt(1);
end;

procedure OnError (Project: TChmProject;errorkind:TChmProjectErrorKind;msg:String);
begin
  writeln(ChmErrorKindText[errorkind],': ',msg);
end;

var
  OutStream: TFileStream;
  Project: TChmProject;
  name   : string;
  xmlname: string;
  ishhp  : boolean;

begin
  if (Paramcount=1) and (ParamStr(1)<>'-h') and (ParamStr(1)<>'-?') then
    begin
      name:=paramstr(1);
      ishhp:=uppercase(extractfileext(name))='.HHP';
      Project := TChmProject.Create;
      if ishhp then
        begin
          xmlname:=changefileext(name,'.hhp.xml');
          Project.LoadFromHHP(name,false) ;          // we need a param for this second param later
          Project.SaveToFile(xmlname);
        end
      else
        Project.LoadFromFile(name);
      OutStream := TFileStream.Create(Project.OutputFileName, fmCreate, fmOpenWrite);
      Project.WriteChm(OutStream);
      OutStream.Free;
      Project.Free;
    end
  else
    begin
      Usage;
    end;
end.

