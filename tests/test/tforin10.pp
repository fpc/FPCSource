{ %FAIL}

// test that it is imposible to use non-valid enumerator operator

program tforin10;

{$mode objfpc}{$H+}
{$apptype console}

type
  TSomeClass = class
  end;

  TSomeClassEnumerator = class
  end;

operator enumerator(s1, s2: TSomeClass): TSomeClassEnumerator;
begin
  Result := nil;
end;

var
  s: TSomeClass;
  c: char;
begin
  for c in s do
    write(c);
end.

