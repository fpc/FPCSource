{$mode objfpc}
{$h+}
unit utexec;

interface

uses
    sysutils;

Function IsExecInvocation : Boolean;
Function TestExecInvocation : Boolean;

Implementation

uses punit, utrtl;

const
  comparestr='-Fu/usr/local/lib/fpc/1.0.10/units/freebsd/rtl/*';

Function IsExecInvocation : Boolean;

begin
  Result:=ParamStr(1)=comparestr
end;

Function TestExecInvocation : Boolean;

var
  i : Longint;

begin
  I:=1;
  Result:=True;
  While Result and (I<=11) do
    begin
    Result:=ParamStr(i)=comparestr;
    Inc(i);
    end;
  Result:=Result and (paramstr(12)='');
end;

Function TestExecuteProcess : String;

var
  cmd,cmdline : String;
  i           : Longint;


begin
  AllowDirectorySeparators:=['/','\'];
  cmd:=ExtractFileName(Paramstr(0));
{$ifdef unix}
  cmd:='./'+cmd;
{$endif}
  cmdline:='';
  for i:=0 to 10 do
   begin
   if Cmdline<>'' then
     CmdLine:=CmdLine+' ';
   cmdline:=cmdline+comparestr;
   end;
  if Not AssertEquals('Failed to execute test command',0,ExecuteProcess(cmd,cmdline)) Then exit;
  // test illegal command
  try
    ExecuteProcess('afsdfdas',cmdline);
    Result:='Failed to raise exception for unknown command';
  except
    Result:=''
  end;
end;

begin
  SysUtilsTest('TestExecuteProcess',@TestExecuteProcess);
end.
