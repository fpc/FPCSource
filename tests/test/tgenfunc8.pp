{ %NORUN }

{ overloads with other generic functions work correctly }

program tgenfunc8;

{$mode objfpc}

operator := (aOther: LongInt): String;
begin
  Str(aOther, Result);
end;

operator := (aOther: String): LongInt;
var
  code: LongInt;
begin
  Val(aOther, Result, code);
end;

generic function Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

generic function Add<S, T>(aLeft, aRight: S): T;
begin
  Result := aLeft + aRight;
end;

generic function Add<T>(aLeft: T): T;
begin
  Result := aLeft + aLeft;
end;

begin
  Writeln(specialize Add<LongInt>(4, 5));
  Writeln(specialize Add<LongInt, String>(3, 8));
  Writeln(specialize Add<String, LongInt>('3', '8'));
  Writeln(specialize Add<LongInt>(2));
  Writeln(specialize Add<String>('Test'));
end.
