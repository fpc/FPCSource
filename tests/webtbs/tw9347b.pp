{$ifdef fpc}
{$mode delphi}
{$endif}

{$r-}
uses
  SysUtils, Classes, TypInfo, Variants;

type
  TBla = class(TPersistent)
  private
    fBool: Boolean;
    fint: integer;
  published
    property Bool: Boolean read fBool write fBool;
    property int: integer read fint write fint;
  end;

begin
  { fails in Delphi 6, succeeds in Delphi 7 }
  SetPropValue(TBla.Create, 'int', 'true');
end.
