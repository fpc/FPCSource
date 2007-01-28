{ %norun }
{$mode objfpc}
unit tw8232;

interface

type
  PTestItem1 = ^TTestItem1;
  TTestItem1 = record
    IntegerItem: Integer;
    ShortStringItem: string;
  end;
  TTestClass1 = class(TObject)
  private
    function GetItems(Index: Integer): TTestItem1;
  public
    property Items[Index: Integer]: TTestItem1 read GetItems;
  end;

  PTestItem2 = ^TTestItem2;
  TTestItem2 = record
    ShortStringItem: string[255];
  end;
  TTestClass2 = class(TObject)
  private
    function GetItems(Index: Integer): TTestItem2;
  public
    property Items[Index: Integer]: TTestItem2 read GetItems;
  end;

  PTestItem3 = ^TTestItem3;
  TTestItem3 = record
    IntegerItem: Integer;
    ShortStringItem: string[255];
  end;
  TTestClass3 = class(TObject)
  private
    function GetItems(Index: Integer): TTestItem3;
  public
    property Items[Index: Integer]: TTestItem3 read GetItems;
  end;

implementation

function TTestClass1.GetItems(Index: Integer): TTestItem1;
begin
end;

function TTestClass2.GetItems(Index: Integer): TTestItem2;
begin
end;

function TTestClass3.GetItems(Index: Integer): TTestItem3;
begin
end;

end.
