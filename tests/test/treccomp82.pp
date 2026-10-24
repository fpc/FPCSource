{ Record composition: passing a composing record with a composed AnsiString by
  value to a function and returning it as result keeps the value and the
  reference count is back to 1 afterwards }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    S: AnsiString;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
    B: Integer;
  end;

function PassThrough(c: TComposed): TComposed;
begin
  if StringRefCount(c.S)<2 then
    halt(1);
  Result := c;
  Result.A := c.A+1;
end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.A := 1;
  c1.S := 'abc';
  UniqueString(c1.S);
  c1.B := 2;
  c2 := PassThrough(c1);
  WriteLn('c2.S: ', c2.S);
  if (c2.A<>2) or (c2.S<>'abc') or (c2.B<>2) then
    halt(2);
  WriteLn('StringRefCount: ', StringRefCount(c1.S));
  if StringRefCount(c1.S)<>2 then
    halt(3);
  c2.S := '';
  if StringRefCount(c1.S)<>1 then
    halt(4);
  WriteLn('ok');
end.
