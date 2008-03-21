{ Old file: tbs0302.pp }
{ inherited property generates wrong assembler         OK 0.99.13 (PFV) }

{$ifdef fpc}{$mode objfpc}{$endif}
type
  c1=class
    Ffont : longint;
    property Font:longint read Ffont write Ffont;
  end;

  c2=class(c1)
    function GetFont:longint;
    procedure setfont(l: longint);
  end;

function c2.GetFont:longint;
begin
  result:=inherited Font;
end;


procedure c2.SetFont(l: longint);
begin
  inherited font := l;  
end;

var
  c: c2;
begin
  c:=c2.create;
  c.ffont:=5;
  if c.getfont<>5 then
    halt(1);
  c.setfont(10);
  if c.getfont<>10 then
    halt(2);
  if c.ffont<>10 then
    halt(3);
end.
