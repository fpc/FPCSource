program demoproject;

{$mode objfpc}{$H+}

uses
  sysutils, process;

{$R *.res}

Var
  I : integer;

begin
  if ParamCount<>0 then
    begin
    Writeln('This is executable: "',ParamStr(0),'"');
    Writeln('Got parameters:');
    For I:=1 to 10 do
      Writeln('"',ParamStr(I),'"');
    end
  else
    With TProcess.Create(Nil) do
      try
        Executable:=ParamStr(0);
        Writeln(Format('Starting executable: "%s"',[Executable]));
        For I:=1 to 10 do
          Parameters.Add('Parameter '+IntToStr(I));
        Execute;
      finally
        Free;
      end;
end.

