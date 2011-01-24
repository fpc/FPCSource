program tforin25;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TIntArray = array[0..3] of Integer;

  TEnumerator = record
  private
    FIndex: Integer;
    FArray: TIntArray;
    function GetCurrent: Integer;
  public
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

  TMyArray = record
    F: array[0..3] of Integer;
    function GetEnumerator: TEnumerator;
  end;

function TEnumerator.MoveNext: Boolean;
begin
  inc(FIndex);
  Result := FIndex < Length(FArray);
end;

function TEnumerator.GetCurrent: Integer;
begin
  Result := FArray[FIndex];
end;

function TMyArray.GetEnumerator: TEnumerator;
begin
  Result.FArray := F;
  Result.FIndex := -1;
end;
{ this will compile too
operator Enumerator(const A: TMyArray): TEnumerator;
begin
  Result.FArray := A.F;
  Result.FIndex := -1;
end;
}
var
  Arr: TMyArray;
  I: Integer;
begin
  for I in Arr do
    WriteLn(I);
end.
