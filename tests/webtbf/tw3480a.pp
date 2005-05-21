{ %fail }

{ Source provided for Free Pascal Bug Report 3480 }
{ Submitted by "Danny Milosavljevic" on  2004-12-26 }
{ e-mail: danny_milo@yahoo.com }
program z;

{$ifdef fpc}{$mode delphi}{$endif}

type
  IBla = interface
  { This is not allowed }
  class function X: Boolean;
  end;

end;

begin
end.
