{ Record composition: an unnamed composition of a record with an AnsiString and
  a dynamic array field, copying the composing record increases the reference
  counts and changing one copy does not change the other }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TIntArray = array of Integer;

  TChildRec = record
    S: AnsiString;
    Arr: TIntArray;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
    B: Integer;
  end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.S := 'abc';
  UniqueString(c1.S);
  SetLength(c1.Arr, 2);
  c1.Arr[0] := 3;
  c1.Arr[1] := 4;
  c2 := c1;
  if StringRefCount(c1.S)<>2 then
    halt(1);
  if Pointer(c1.Arr)<>Pointer(c2.Arr) then
    halt(2);
  if PSizeInt(Pointer(c1.Arr)-2*SizeOf(SizeInt))^<>2 then
    halt(3);
  SetLength(c2.Arr, 3);
  c2.Arr[0] := 7;
  WriteLn('c1.Arr[0]: ', c1.Arr[0]);
  WriteLn('c2.Arr[0]: ', c2.Arr[0]);
  if (Length(c1.Arr)<>2) or (c1.Arr[0]<>3) or (c1.Arr[1]<>4) then
    halt(4);
  if (Length(c2.Arr)<>3) or (c2.Arr[0]<>7) then
    halt(5);
  c2.S := 'x';
  if (c1.S<>'abc') or (StringRefCount(c1.S)<>1) then
    halt(6);
  WriteLn('ok');
end.
