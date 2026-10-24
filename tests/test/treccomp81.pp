{ Record composition: "contains alias" of an existing field of a record with an
  AnsiString field, assigning via the composed member and copying the
  composing record keeps the reference count correct }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    S: AnsiString;
  end;

  TComposed = record
  strict private
    FChild: TChildRec;
  public
    contains alias FChild;
    function GetChildS: AnsiString;
  end;

function TComposed.GetChildS: AnsiString;
begin
  Result := FChild.S;
end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.S := 'abc';
  UniqueString(c1.S);
  c2 := c1;
  WriteLn('StringRefCount: ', StringRefCount(c1.S));
  if StringRefCount(c1.S)<>2 then
    halt(1);
  c2.S := 'x';
  if StringRefCount(c1.S)<>1 then
    halt(2);
  { check last, the temporary function results hold references }
  if (c1.GetChildS<>'abc') or (c2.GetChildS<>'x') then
    halt(3);
  WriteLn('ok');
end.
