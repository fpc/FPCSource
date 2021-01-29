program tw36775;

{$mode objfpc}

{ NOTE: The important part of this test is on line 65, the FindByName method }

uses
  SysUtils;

type
  TTestItem = class;

  TTestCollection = class
    private
      FItemArray: array of TTestItem;
      function GetItem(const Index: Integer): TTestItem;
      function GetCount: Integer;
    public
      destructor Destroy; override;
      function Add(const Item: TTestItem): Integer;
      function FindByName(const Val: ansistring; Ignore: TTestItem = nil): TTestItem;
      property Items[Index: Integer]: TTestItem read GetItem;
      property Count: Integer read GetCount;
  end;

  TTestItem = class
    private
      FName: ansistring;
    public
      constructor Create(Owner: TTestCollection; AName: ansistring);
      property Name: ansistring read FName;
  end;

{ TTestCollection }

destructor TTestCollection.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      FItemArray[i].Free;

    inherited Destroy;
  end;

function TTestCollection.GetItem(const Index: Integer): TTestItem;
  begin
    Result := FItemArray[Index];
  end;

function TTestCollection.GetCount: Integer;
  begin
    Result := Length(FItemArray);
  end;

function TTestCollection.Add(const Item: TTestItem): Integer;
  begin
    Result := Length(FItemArray);
    SetLength(FItemArray, Result + 1);
    FItemArray[Result] := Item;
  end;

{ NOTE - The construction of the internal loop in the method below, specifically
    the setting of Result, is paramount for triggering Internal Error 200405231 }
function TTestCollection.FindByName(const Val: ansistring; Ignore: TTestItem): TTestItem;
  var
    i: Integer;
  begin
    i := Count - 1;
    while i >= 0 do
    begin
      Result := Items[i];
      { If either one of the conditions is removed, the internal error does not trigger }
      if (AnsiCompareText(Result.Name, Val) = 0) and (Ignore <> Result) then
        Exit;
      Dec(i);
    end;
    Result := nil;
  end;

{ TTestItem }

constructor TTestItem.Create(Owner: TTestCollection; AName: ansistring);
  begin
    FName := AName;
    Owner.Add(Self);
  end;

const
  TestName1 = 'Low';
  TestName2 = 'Defrost';
  TestName3 = 'Medium';
  TestName4 = 'Medium High';
  TestName5 = 'Cook';
  TestNameX = 'High';

var
  Collection: TTestCollection;
  ReturnedItem, IgnoreMe: TTestItem;
begin
  Collection := TTestCollection.Create;
  try
    TTestItem.Create(Collection, TestName1);
    TTestItem.Create(Collection, TestName2);
    TTestItem.Create(Collection, TestName3);
    TTestItem.Create(Collection, TestName4);
    TTestItem.Create(Collection, TestName5);
    IgnoreMe := TTestItem.Create(Collection, TestName3); { A second item named "Medium" }

    ReturnedItem := Collection.FindByName(TestName2);
    if not Assigned(ReturnedItem) then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestName2, ') returned nil.');
        Halt(1);
      end
    else if ReturnedItem.Name <> TestName2 then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestName2, ') returned the wrong item (', ReturnedItem.Name, ').');
        Halt(1);
      end;

    ReturnedItem := Collection.FindByName(TestNameX);
    if Assigned(ReturnedItem) then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestNameX, ') did not return nil (', ReturnedItem.Name, ').');
        Halt(1);
      end;

    ReturnedItem := Collection.FindByName(TestName3, IgnoreMe);
    if not Assigned(ReturnedItem) then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestName3, ') returned nil.');
        Halt(1);
      end
    else if ReturnedItem.Name <> TestName3 then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestName3, ') returned the wrong item (', ReturnedItem.Name, ').');
        Halt(1);
      end
    else if ReturnedItem = IgnoreMe then
      begin
        WriteLn('ERROR: Collection.FindByName(', TestName3, ') returned the ignored item.');
        Halt(1);
      end;

  finally
    Collection.Free;
  end;

  WriteLn('ok');
end.

