{ Record composition: an unnamed composition of a record with management
  operators Copy and AddRef, assigning the composing record calls Copy on the
  composed record and passing it by value calls AddRef }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
    class operator Copy(constref aSrc: TChildRec; var aDst: TChildRec);
    class operator AddRef(var r: TChildRec);
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
    B: Integer;
  end;

var
  CopyCount: Integer = 0;
  AddRefCount: Integer = 0;

class operator TChildRec.Copy(constref aSrc: TChildRec; var aDst: TChildRec);
begin
  aDst.C := aSrc.C+1;
  Inc(CopyCount);
end;

class operator TChildRec.AddRef(var r: TChildRec);
begin
  Inc(AddRefCount);
end;

procedure TakeValue(c: TComposed);
begin
  if c.C<>11 then
    halt(1);
end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.A := 1;
  c1.C := 10;
  c1.B := 2;
  c2 := c1;
  WriteLn('CopyCount: ', CopyCount);
  WriteLn('c2.C: ', c2.C);
  if CopyCount<>1 then
    halt(2);
  if (c2.A<>1) or (c2.C<>11) or (c2.B<>2) then
    halt(3);
  AddRefCount := 0;
  TakeValue(c2);
  WriteLn('AddRefCount: ', AddRefCount);
  if AddRefCount<>1 then
    halt(4);
  WriteLn('ok');
end.
