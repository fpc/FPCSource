program WrongHint;
type
  PRecord = ^TRecord;
  TRecord = record
  end;
var
  x: PRecord;
begin

  New(x);
  Dispose(x);
end.
