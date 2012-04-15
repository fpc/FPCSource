{%fail}
{$ifdef fpc}
  {$mode delphi}
{$endif}
type
  TTestObject = class(TObject)
  private
    fValue: integer;
  protected
    property Value: integer read fValue write fValue;
    property Value: integer read fValue;
  end;

begin
end.
