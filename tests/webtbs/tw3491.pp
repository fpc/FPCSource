{ Source provided for Free Pascal Bug Report 3491 }
{ Submitted by "Marek Mauder" on  2004-12-29 }
{ e-mail: pentar@seznam.cz }
program ErrorGen;

{$ifdef fpc}{$mode delphi}{$endif}
type
  TEnum = (
    e0 = 10,
    e1 = 11,
    e2 = 12,
    e3 = 15);
  {errorgen.pas(9,27) Error: enums with assignments can't be used as array index}
  TEnumArray = array[TEnum] of Byte;
begin
  {writes 6}
  WriteLn(SizeOf(TEnumArray));
  if SizeOf(TEnumArray)<>6 then
    halt(1);
end.
