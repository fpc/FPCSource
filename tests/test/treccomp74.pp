{ Record composition: a named composition of a record with an AnsiString field,
  copying the composing record shares the string and copy-on-write keeps
  the copies independent }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    S: AnsiString;
  end;

  TComposed = record
    A: Integer;
    contains child: TChildRec;
    B: Integer;
  end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.S := 'abc';
  UniqueString(c1.S);
  if c1.child.S<>'abc' then
    halt(1);
  c2 := c1;
  WriteLn('StringRefCount: ', StringRefCount(c1.S));
  if StringRefCount(c1.S)<>2 then
    halt(2);
  if Pointer(c1.S)<>Pointer(c2.child.S) then
    halt(3);
  c2.S[1] := 'x';
  WriteLn('c1.S: ', c1.S);
  WriteLn('c2.S: ', c2.S);
  if (c1.S<>'abc') or (c2.S<>'xbc') then
    halt(4);
  if (StringRefCount(c1.S)<>1) or (StringRefCount(c2.S)<>1) then
    halt(5);
  WriteLn('ok');
end.
