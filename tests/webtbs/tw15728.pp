program TT;

{$mode delphi}

uses
  SysUtils;

type
  t_R = record
   R1:integer;
  end;

t_X = function:t_R;

function A:t_R;
  begin
    Result.R1:=123;
  end;

var X:t_X;

begin
  X:=A;
  if x.r1<>123 then
    halt(1);
  writeln(X.R1); // Error: Illegal qualifier
  writeln(X().R1); // OK
  with X do
    begin
      if r1<>123 then
        halt(2);
      writeln(R1); //Error: Expression type must be class or record 
    end;
  with X() do writeln(R1); // OK
end.
