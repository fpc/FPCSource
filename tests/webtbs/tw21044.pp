{ %norun}
program tw21044;

{$mode Delphi}

uses
  SysUtils, Classes;

type
  { TTestRecord }

  TTestRecord = record
  public
    function Test(const Lhs, Rhs: TTestRecord): TTestRecord;
    // operator overloads
    class operator Add(const Lhs, Rhs: TTestRecord): TTestRecord;
    // this part changes the size of record and so the way of parameter handling
    // on some 64bit systems
  case Boolean of
    False: (Value: Single);
    True: (AsInteger: Integer);
  end;

{ TTestRecord }

function TTestRecord.Test(const Lhs, Rhs: TTestRecord): TTestRecord;
begin
  Result.AsInteger := Lhs.AsInteger + Rhs.AsInteger;
end;

class operator TTestRecord.Add(const Lhs, Rhs: TTestRecord): TTestRecord;
begin
  Result.Value := Lhs.Value + Rhs.Value;
end;

begin
end.
