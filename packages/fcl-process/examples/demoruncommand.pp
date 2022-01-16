{$mode objfpc}
{$h+}

program demoruncommand;
{
  Demonstrate the RunCommand program.

  Before running this demo program, compile the echoparams program first,
  make sure both binaries are in the same directory.
}

uses sysutils, process;

Var
  S : String;

begin
  if RunCommand(ExtractFilePath(ParamStr(0))+'echoparams',['a','b','c'],S,[]) then
    Writeln('echoparams returned : ',S)
  else
    Writeln('Command failed');
end.

