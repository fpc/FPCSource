{$ifdef fpc}
{$mode delphi}
{$endif}
program tw9347b;

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
  { fails in Delphi 6, succeeds in Delphi 7, fails in D2007 }
  try
    SetPropValue(TBla.Create, 'int', 'true');
  except
    on E: EVariantError do
      halt(0);
  end;
  halt(1);
end.
