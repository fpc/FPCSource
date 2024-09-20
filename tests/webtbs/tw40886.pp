{ %opt=-gl }
program test;
{$modeswitch implicitfunctionspecialization}

generic procedure FillChar<T>(var x; count: SizeInt; value: Byte);
begin
end;

var v: array [0..0] of Byte;

begin
  FillChar(v, 0, 0);
end.
