{
  $Id$
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    Check if redir can use COMSPEC environment variable.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program dotest;
uses
  dos,
  redir;

Const
  FailName = 'fail';
{$ifdef UNIX}
  ExeExt='';
{$else UNIX}
  ExeExt='exe';
{$endif UNIX}


function ForceExtension(Const HStr,ext:String):String;
{
  Return a filename which certainly has the extension ext
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  if Ext<>'' then
   ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext
  else
   ForceExtension:=Copy(Hstr,1,j-1);
end;



procedure RunFail;
var
  outname,
  TestExe : string;
begin
  TestExe:=ForceExtension(FailName,ExeExt);
  TestExe:=FExpand(TestExe);
  ExecuteRedir(TestExe,'','','','');
  if (DosError<>0) or (ExecuteResult<>1) then
    writeln('exit code not returned correctly');
end;


begin
  if (paramcount>0) and (paramstr(1)='-x') then
    UseComSpec:=false
  else
    UseComSpec:=true;
  RunFail;
end.
{
  $Log$
  Revision 1.2  2003-04-29 21:12:17  pierre
   * win32 specific unit added

  Revision 1.1  2002/12/05 16:04:26  pierre
   * new files for checking if ComSpec returns error

}
