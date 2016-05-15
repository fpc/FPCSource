program tw26993;

{$mode delphi}

uses
  Classes, SysUtils;

type

  { TExtendedTestCase }

  TExtendedTestCase = record
  private
    FValue: extended;
  public
    property Value: extended read FValue write FValue;
    class operator Add(v1, v2: TExtendedTestCase): TExtendedTestCase;
    class operator Multiply(v1, v2: TExtendedTestCase): TExtendedTestCase;
  end;


{ TExtendedTestCase }

class operator TExtendedTestCase.Add(v1, v2: TExtendedTestCase): TExtendedTestCase;
begin
  Result.Value := v1.Value + v2.Value;
end;

class operator TExtendedTestCase.Multiply(v1, v2: TExtendedTestCase):
TExtendedTestCase;
begin
  Result.Value := v1.Value * v2.Value;
end;

var
  e1,e2,e3: textendedtestcase;
begin
  e1.fvalue:=2.0;
  e2.fvalue:=3.0;
  e3:=e1+e2;
  if (e3*e2).fvalue<>15.0 then
    halt(1);

end.





