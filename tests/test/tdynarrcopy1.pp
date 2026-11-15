{ %skipcpu=jvm }
{ Copy of a dynamic array calls a function in its array argument once,
  whatever the further arguments, and before them: the array it returned is
  the one copied even if a further argument releases the variable it came
  from. Not for the JVM targets, which have a Copy of their own. }
program tdynarrcopy1;

{$mode objfpc}{$H+}
{$inline on}

type
  TBytesA = array of Byte;
  TStrings3 = array of AnsiString;

var
  gCalls: LongInt = 0;
  gErrors: LongInt = 0;
  gShared: TBytesA;
  gText: AnsiString;

function Three: TBytesA;
begin
  Inc(gCalls);
  Result := nil;
  SetLength(Result, 3);
  Result[0] := 1;
  Result[1] := 2;
  Result[2] := 3;
end;

{ a different array at each call: a second call would give another length }
function Growing: TBytesA;
begin
  Inc(gCalls);
  Result := nil;
  SetLength(Result, gCalls);
end;

function Names: TStrings3;
begin
  Inc(gCalls);
  Result := nil;
  SetLength(Result, 2);
  Result[0] := 'one';
  Result[1] := 'two';
end;

function Offset: LongInt;
begin
  Inc(gCalls, 100);
  Result := 1;
end;

function CountOne: LongInt;
begin
  Inc(gCalls, 10000);
  Result := 1;
end;

function Shared: TBytesA;
begin
  Inc(gCalls);
  Result := gShared;
end;

{ releases the array Shared has just returned from }
function ClearShared: LongInt;
begin
  Inc(gCalls, 100);
  gShared := nil;
  Result := 0;
end;

{ two references to a string made at run time }
function Texts: TStrings3;
begin
  Inc(gCalls);
  Result := nil;
  SetLength(Result, 2);
  Result[0] := gText;
  Result[1] := gText;
end;

function Raising: LongInt;
begin
  Result := 0;
  raise TObject.Create;
end;

function Tail(aFrom: LongInt): TBytesA; inline;
begin
  Result := Copy(Three, aFrom);
end;

{ the array a function returned for Copy is released when the routine
  is left, normally and after an exception in a further argument }
procedure Lifetime;
var
  lNames: TStrings3;
begin
  lNames := Copy(Texts, 0, 1);
  if (Length(lNames) <> 1) or (lNames[0] <> gText) then
    begin
      writeln('Copy(Texts, 0, 1): wrong result');
      Inc(gErrors);
    end;
  try
    lNames := Copy(Texts, Raising, 1);
    writeln('Copy(Texts, Raising, 1): no exception');
    Inc(gErrors);
  except
  end;
end;

procedure Check(const aWhat: string; aLength, aWantLength, aWantCalls: LongInt);
begin
  if (aLength <> aWantLength) or (gCalls <> aWantCalls) then
    begin
      writeln(aWhat, ': length ', aLength, ' (expected ', aWantLength, '), calls ', gCalls, ' (expected ', aWantCalls, ')');
      Inc(gErrors);
    end;
  gCalls := 0;
end;

var
  lCopy: TBytesA;
  lNames: TStrings3;
  lVar: TBytesA;
  lRefs: SizeInt;
begin
  lCopy := Copy(Three);
  Check('Copy(F)', Length(lCopy), 3, 1);
  lCopy := Copy(Three, 1);
  Check('Copy(F, 1)', Length(lCopy), 2, 1);
  lCopy := Copy(Three, 0, 3);
  Check('Copy(F, 0, 3)', Length(lCopy), 3, 1);
  lCopy := Copy(Three, 1, MaxInt);
  Check('Copy(F, 1, MaxInt)', Length(lCopy), 2, 1);
  if (lCopy[0] <> 2) or (lCopy[1] <> 3) then
    begin
      writeln('Copy(F, 1, MaxInt): wrong elements');
      Inc(gErrors);
    end;
  lCopy := Copy(Growing);
  Check('Copy(Growing)', Length(lCopy), 1, 1);
  { the further arguments are evaluated once as well }
  lCopy := Copy(Three, Offset, 1);
  Check('Copy(F, G, 1)', Length(lCopy), 1, 101);
  lCopy := Copy(Three, Offset, CountOne);
  Check('Copy(F, G, H)', Length(lCopy), 1, 10101);
  if lCopy[0] <> 2 then
    begin
      writeln('Copy(F, G, H): wrong element');
      Inc(gErrors);
    end;
  { the array is evaluated before the further arguments, and held }
  gShared := Three;
  gCalls := 0;
  lCopy := Copy(Shared, ClearShared, 3);
  Check('Copy(Shared, ClearShared, 3)', Length(lCopy), 3, 101);
  if (Length(lCopy) = 3) and ((lCopy[0] <> 1) or (lCopy[1] <> 2) or (lCopy[2] <> 3)) then
    begin
      writeln('Copy(Shared, ClearShared, 3): wrong elements');
      Inc(gErrors);
    end;
  if gShared <> nil then
    begin
      writeln('Copy(Shared, ClearShared, 3): the further argument did not run');
      Inc(gErrors);
    end;
  { inside an inlined routine, used twice }
  lCopy := Tail(1);
  Check('Tail(1)', Length(lCopy), 2, 1);
  lCopy := Tail(2);
  Check('Tail(2)', Length(lCopy), 1, 1);
  if lCopy[0] <> 3 then
    begin
      writeln('Tail(2): wrong element');
      Inc(gErrors);
    end;
  { what Copy held is released: the reference count is the same as before }
  Str(Length(lCopy) + 12345, gText);
  lRefs := StringRefCount(gText);
  Lifetime;
  gCalls := 0;
  if StringRefCount(gText) <> lRefs then
    begin
      writeln('Lifetime: ', StringRefCount(gText), ' references to the string are left (expected ', lRefs, ')');
      Inc(gErrors);
    end;
  { managed elements }
  lNames := Copy(Names, 1, 1);
  Check('Copy(Names, 1, 1)', Length(lNames), 1, 1);
  if lNames[0] <> 'two' then
    begin
      writeln('Copy(Names, 1, 1): wrong element');
      Inc(gErrors);
    end;
  { a variable is copied as before }
  lVar := Three;
  gCalls := 0;
  lCopy := Copy(lVar, 1, 1);
  Check('Copy(variable, 1, 1)', Length(lCopy), 1, 0);
  lCopy[0] := 9;
  if lVar[1] <> 2 then
    begin
      writeln('Copy(variable): the copy shares the source');
      Inc(gErrors);
    end;
  if gErrors <> 0 then
    Halt(1);
  writeln('ok');
end.
