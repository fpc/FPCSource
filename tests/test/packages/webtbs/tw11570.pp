program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process
  { you can add units after this };

var
  p: TProcess;
begin
  try
    p := TProcess.Create(nil);
    p.Active := true;
  except
    on eprocess do
      begin
        writeln('ok');
        halt(0);
      end;
  end;
  halt(1);
end.
