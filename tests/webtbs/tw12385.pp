{$ifdef fpc}
{$mode objfpc}
{$endif}
program tw12385;

uses
  SysUtils;

var
  s: string; 
  cr: Extended; 

begin
  cr := 1234.567;
  s:=FormatFloat('00000000.00', cr);
  if s<>'00001234.57' then 
    begin
    Writeln(S,'<> 00001234.57 (latter is correct)');
    Halt(1);
    end;
  cr := -1234.567;
  s:=FormatFloat('00000000.00', cr);
  if s<>'-00001234.57' then 
    begin
    Writeln(S,'<> -00001234.57 (latter is correct)');
    Halt(1);
    end;
  cr := -1234.567;
  s:=FormatFloat('000.00', cr);
  if s<>'-1234.57' then 
    begin
    Writeln(S,'<> -1234.57 (latter is correct)');
    Halt(1);
    end;
  S:=FormatFloat('000.000',-1); //returns 0-1.000
  if s<>'-001.000' then
     begin
     Writeln(S,'<> -001.000 (latter is correct)');
     Halt(1);
     end;
end.
