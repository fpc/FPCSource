{$ifdef fpc}{$mode delphi}{$endif}

type
  BRec = record
    fu: Function:Longint;
  end;

var
  a : Longint;
  b : BRec;

function f1:longint;
begin
  result:=10;
end;

begin
  b.fu:=f1;
//  a:=b.fu(); // works well
  a:=b.fu;   // causes "Error: incompatible types"
  writeln(a);
  if a<>10 then
   halt(1);
end.
