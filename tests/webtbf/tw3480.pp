{ %fail }

{ Source provided for Free Pascal Bug Report 3480 }
{ Submitted by "Danny Milosavljevic" on  2004-12-26 }
{ e-mail: danny_milo@yahoo.com }
program z;

{$mode delphi}

type
  IBla = interface
  class function X: Boolean;
  end;

  TBla = class(TInterfacedObject, IBla, IInterface)
  public
  class function X: Boolean;
  end;

begin
  TBla.X;
end.

