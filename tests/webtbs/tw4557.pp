{ Source provided for Free Pascal Bug Report 4557 }
{ Submitted by "Marek Mauder" on  2005-11-29 }
{ e-mail: pentar@seznam.cz }
program FPCBugs;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
type
  TEnum = (Item1 = 5, Item2 = 10, Item3 = 20);
var
  Enum: TEnum = Item1;
begin
  Enum := Succ(Enum);
end.
