{ Source provided for Free Pascal Bug Report 3353 }
{ Submitted by "Vincent Snijders" on  2004-10-11 }
{ e-mail: vslist@zonnet.nl }
uses uw3353;

{$ifdef fpc}{$mode objfpc}{$endif}

var
        gfx:    TGPGraphics;
    a: integer;
begin
  err:=true;
  gfx := TGPGraphics.Create(a);
  if err then
    halt(1);
end.
