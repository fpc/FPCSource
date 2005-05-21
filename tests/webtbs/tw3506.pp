{ Source provided for Free Pascal Bug Report 3506 }
{ Submitted by "Frank Kintrup" on  2005-01-04 }
{ e-mail: frank.kintrup@gmx.de }
{$MODE Delphi}

type
  TEnumType = (enum0, enum1, enum2);

type
  TTestClass = class (TObject)
  private
    function GetPropValue(
      nIndex : TEnumType) : Integer;

  public
    property Prop0 : Integer index enum0
      read GetPropValue;
  end;

function TTestClass.GetPropValue(
  nIndex : TEnumType) : Integer;
begin
  Result := Integer(nIndex);
end;

begin
end.
