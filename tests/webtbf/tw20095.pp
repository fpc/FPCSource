{ %fail }

program project1;

{$mode objfpc}{$H+}

uses
  sysutils, Classes;

type

generic TBroken<_T> = class
  private type
    T_TArray = array of _T;
  private var
    FArray: T_TArray;
  private
    function FGetTopEntry(): _T;
    procedure FSetTopEntry(Value: _T);
  public
    constructor Create(Len: integer);

    property TopEntry: _T read FGetTopEntry write FSetTopEntry;
end;

TRecord = record
  x, y, z: integer;
end;

TMaybeBroken = class
  private
    FArray: array of TRecord;

    function FGetTopEntry(): TRecord;
    procedure FSetTopEntry(Value: TRecord);
  public
    constructor Create(Len: integer);

    property TopEntry: TRecord read FGetTopEntry write FSetTopEntry;
end;


TBrokenRecord = specialize TBroken<TRecord>; // pun intended

var
  a: TBrokenRecord;
  b: TMaybeBroken;
  i: integer;

constructor TBroken.Create(Len: integer);
  var
    i: integer;
  begin
    SetLength(FArray, Len);
    FillChar(FArray[0], SizeOf(_T) * Len, 0);
end;

function TBroken.FGetTopEntry(): _T;
  begin
    Result := FArray[High(FArray)];
end;

procedure TBroken.FSetTopEntry(Value: _T);
  begin
    FArray[High(FArray)] := Value;
end;

constructor TMaybeBroken.Create(Len: integer);
  var
    i: integer;
  begin
    SetLength(FArray, Len);
    FillChar(FArray[0], SizeOf(TRecord) * Len, 0);
end;

function TMaybeBroken.FGetTopEntry(): TRecord;
  begin
    Result := FArray[High(FArray)];
end;

procedure TMaybeBroken.FSetTopEntry(Value: TRecord);
  begin
    FArray[High(FArray)] := Value;
end;

begin
  a := TBrokenRecord.Create(10);
  Inc(a.TopEntry.x);

  for i := 0 to 9 do writeln(inttostr(a.FArray[i].x));

  a.Free();

  writeln('---');

  b := TMaybeBroken.Create(10);
  Inc(b.TopEntry.x);

  for i := 0 to 9 do writeln(inttostr(b.FArray[i].x));

  b.Free();

end.
