{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test open arrays and array constructors
}

program timpfuncspez32;
uses
  typinfo;

generic function DoThis<T>(a: array of T): TTypeKind;
begin
  writeln(GetTypeKind(T), ': ', Length(a));
  result := GetTypeKind(T);
end;

type
  TIntArray = array of integer;
var
  s1: array of integer;
  s2: array[0..2] of integer;
begin
  s1 := [1,2,3];
  if DoThis(s1) <> tkInteger then
    Halt(-1);

  s2 := [1,2,3];
  if DoThis(s2) <> tkInteger then
    Halt(-1);

  s1 := TIntArray.Create(1,2,3);
  if DoThis(s1) <> tkInteger then
    Halt(-1);
end.