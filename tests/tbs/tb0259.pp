{ Old file: tbs0302.pp }
{ inherited property generates wrong assembler         OK 0.99.13 (PFV) }

{$ifdef fpc}{$mode objfpc}{$endif}
type
  c1=class
    Ffont : longint;
    property Font:longint read Ffont;
  end;

  c2=class(c1)
    function GetFont:longint;
  end;

function c2.GetFont:longint;
begin
  result:=Font;
  result:=inherited Font;
end;

begin
end.
