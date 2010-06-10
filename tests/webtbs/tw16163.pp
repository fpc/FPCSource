{ %norun }

program test;

{$mode objfpc}

type
  TFColor = record
    b, g, r : Byte;
    // m : Byte; // uncomment it to avoid InternalError 200301231
  end;

  TFColorA = record
    c : TFColor;
    a : Byte;
    // adding some field here, or chaning a type to Word or Interger
    // also fixed the problem. 
  end;

function FColorToFColorA(C : TFColor) : TFColorA;
begin
  Result.c:=C;
  Result.a:=255;
end;

var
  t : TFColor;
  a : TFColor;
begin
  FillChar(a, sizeof(a), $55);
  t:=FColorToFColorA(a).c; // IE 200301231 why?
  if (t.b<>$55) or
     (t.r<>$55) or
     (t.g<>$55) then
    halt(1);
end.
