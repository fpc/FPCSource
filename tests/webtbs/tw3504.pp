{$mode objfpc}{$H+}

uses SysUtils;

function Func1( Args :array of string ) :string;
begin
  WriteLn('project2 Arg0-'+Args[0] +' Arg1-'+Args[1]);
  if (Args[0]='1') and (Args[1]='2') then
    Result := 'Ok'
  else
    Result := 'Err';
end;

var
  Int1, Int2  :integer;
  s : string;
begin
  Int1 := 1;
  Int2 := 2;
  s:=Func1( [ IntToStr(Int1), IntToStr(Int2)] );
  writeln(s);
  if s<>'Ok' then
    halt(1);
end.
