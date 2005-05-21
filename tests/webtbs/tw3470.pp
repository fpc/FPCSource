{ Source provided for Free Pascal Bug Report 3470 }
{ Submitted by "Danny Milosavljevic" on  2004-12-25 }
{ e-mail: danny_milo@yahoo.com }

{$mode objfpc}
program a;
uses variants;

type 
  IBla = interface
  end;

  TBla = class(TInterfacedObject, IBla)
  public
    constructor Create;
  end;

constructor TBla.Create;
begin
end;
   
var
  v: Variant;
  bla: IBla;
begin
  bla := TBla.Create;
  v := bla;
end.
