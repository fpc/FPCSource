{ %norun }
{$Mode ObjFpc}
{$ModeSwitch AdvancedRecords}

uses TypInfo;

{ Check that extended RTTI writes the type information of a record's property
  and method types when nothing else names those types: the program did not
  link }

type
  TRecIndex = 1..100;
  TRecValue = 1..200;
  TRecParam = 1..300;
  TRecResult = 1..400;

  {$RTTI EXPLICIT
    FIELDS([vcPublic])
    PROPERTIES([vcPublic,vcPublished])
    METHODS([vcPublic,vcPublished])
  }
  { the getter is private, so its types reach the RTTI only through the
    property, and Calc's only through the method table }
  TTestRec = record
  private
    function GetItem(aIndex: TRecIndex): TRecValue;
  public
    function Calc(a: TRecParam): TRecResult;
    property Items[aIndex: TRecIndex]: TRecValue read GetItem;
  end;

function TTestRec.GetItem(aIndex: TRecIndex): TRecValue;
begin
  Result := aIndex;
end;

function TTestRec.Calc(a: TRecParam): TRecResult;
begin
  Result := a;
end;

begin
  if TypeInfo(TTestRec)=nil then
    Halt(1);
end.
