{ %fail }

{ Source provided for Free Pascal Bug Report 3480 }
{ Submitted by "Danny Milosavljevic" on  2004-12-26 }
{ e-mail: danny_milo@yahoo.com }
program z;

{$ifdef fpc}{$mode delphi}{$endif}

type
  IBla = interface
  function X: Boolean;
  end;

  TBla = class(TInterfacedObject, IBla, IInterface)
  public
  class function X: Boolean;
  end;

class function TBLA.X: Boolean;
begin

end;

begin
  TBla.X;
end.
