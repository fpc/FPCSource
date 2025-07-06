program project6;

{$MODE DELPHI}
{$OPTIMIZATION REGVAR}

uses
  Math, SysUtils;

type
  IUniqueValues = interface
    function GetValueCount: SizeInt;
    function GetNullCount: SizeInt;
    function GetRefCount(Index: SizeInt): SizeInt;
    function GetMaxRefCountIndex: SizeInt;
    property ValueCount: SizeInt read GetValueCount;
    property NullCount: SizeInt read GetNullCount;
    property RefCounts[Index: SizeInt]: SizeInt read GetRefCount;
    property MaxRefCountIndex: SizeInt read GetMaxRefCountIndex;
  end;

  TUniqueValues = class (TInterfacedObject, IUniqueValues)
    function GetValueCount: SizeInt;
    function GetNullCount: SizeInt;
    function GetRefCount({%H-}Index: SizeInt): SizeInt;
    function GetMaxRefCountIndex: SizeInt;
  end;

  TUniqueValuesContainer = class
    UniqueValues: IUniqueValues;
  end;

function TUniqueValues.GetValueCount: SizeInt;
begin
  Result := 1;
end;

function TUniqueValues.GetNullCount: SizeInt;
begin
  Result := 0;
end;

function TUniqueValues.GetRefCount(Index: SizeInt): SizeInt;
begin
  Result := 1;
end;

function TUniqueValues.GetMaxRefCountIndex: SizeInt;
begin
  Result := 0;
end;

type
  TMyClass2 = class
    FOwner: TUniqueValuesContainer;
    FRowCount: SizeInt;
    FAvg: Double;
    FSKO: Double;
    FHistoIndex: TArray<SizeInt>;
    FHistoPercent: array of Double;
    FScaleRanking: array of Double;
    procedure Test;
  end;

//{$OPTIMIZATION NOREGVAR}
procedure TMyClass2.Test;
var
  i, {j,} m, k, xCount, xValueCount: SizeInt;
  v1, v2: Double;
  xUniqueValues: IUniqueValues;
begin
  xUniqueValues := FOwner.UniqueValues;
  FSKO := 0;
  i{j} := xUniqueValues.MaxRefCountIndex;
  if i{j} <> -1 then
  begin
    xCount := FRowCount - xUniqueValues.NullCount;
    xValueCount := xUniqueValues.ValueCount;
    SetLength(FHistoPercent, xValueCount);
    FHistoPercent[0] := xUniqueValues.RefCounts[0] / xCount;
    for i := 1 to xValueCount - 1 do
      FHistoPercent[i] := FHistoPercent[i - 1] + xUniqueValues.RefCounts[i] / xCount;
    // ...
    SetLength(FScaleRanking, xValueCount);
    m := 1; //FHistoIndex[0];
    FScaleRanking[m] := 1;
    for i := 1 to xValueCount - 1 do
    begin
      k := FHistoIndex[i];
      FScaleRanking[k] := FScaleRanking[m] + xUniqueValues.RefCounts[FHistoIndex[i - 1]] / xUniqueValues.RefCounts[FHistoIndex[i]];
      m := k;
    end;
    // ...
    v1 := 0; v2 := 0;
    for i := 0 to xValueCount - 1 do
    begin
      m := xUniqueValues.RefCounts[i];
      v1 := v1 + m * FScaleRanking[i]; // <-- ERROR!!! ` * FScaleRanking[i]` - mov rax, [rsp-$000000xx]; mulsd xmm0, [rax*8+rax]
      v2 := v2 + (m * FScaleRanking[i] * FScaleRanking[i]);
    end;
    FAvg := v1 / xCount;
    v1 := v2 / xCount - FAvg * FAvg;
    if v1 > 0 then
      FSKO := Sqrt(v1)
    else
      FSKO := 0;
  end;
  // ...
end;

var
  Obj: TMyClass2;
  OK: Boolean;
begin
  Obj := TMyClass2.Create;
  Obj.FOwner := TUniqueValuesContainer.Create;
  Obj.FOwner.UniqueValues := TUniqueValues.Create;
  Obj.FRowCount := 1;
  Obj.Test;
  OK := Obj.FAvg = 0;
  Obj.FOwner.Free;
  Obj.Free;
  if not OK then
    Halt(1);
end.
