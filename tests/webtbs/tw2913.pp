{ Source provided for Free Pascal Bug Report 2913 }
{ Submitted by "Micha Nelissen" on  2004-01-21 }
{ e-mail:  }
program EnumName;

{$mode objfpc}
{$h+}

uses
  Classes, TypInfo;

type
  TEnumTest = (etOne, etTwo, etThree);

  TA = class(TPersistent)
  private
    FTest: TEnumTest;
  published
    property Test: TEnumTest read FTest write FTest;
  end;

begin
  writeln(GetEnumName(GetPropInfo(TA, 'Test')^.PropType, 2));
end.
