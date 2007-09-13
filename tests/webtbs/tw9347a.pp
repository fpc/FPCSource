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
  try
    { delphi gives a range error here, also if range checking is off }
    SetPropValue(TBla.Create, 'Bool', 2);
  except on ERangeError do
    halt(0);
  end;
  halt(1);
end.
