{ Source provided for Free Pascal Bug Report 3494 }
{ Submitted by "Danny Milosavljevic" on  2004-12-31 }
{ e-mail: danny_milo@yahoo.com }
program ivar;
{$mode objfpc}
uses variants;

type
  ti = interface(iinterface)
  end;

var
  v: Variant;
  i: ti;
begin
  i := nil;
  v := i;
  i := v;
end.
