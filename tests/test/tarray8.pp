{ Test correct RTTI handling of open arrays with managed elements.
  See also webtbs/tw18859.pp }

{$mode objfpc}{$h+}

procedure test3(out arr: array of string);
begin
  { implicit initialize happens here }
  arr[0] := '';  // if initialization does not happen correctly, teststring will be destroyed
end;

var
  teststring: string;
  arrs: array[0..3] of string;

begin
  teststring := 'test';
  uniquestring(teststring);
  // Must be a string with refcount>1, otherwise decref before call will release it and
  // zero the pointer, thus masking the issue.
  arrs[0] := teststring;
  { implicit decref happens here }
  test3(arrs);
  if teststring <> 'test' then
    Halt(1);
  Halt(0);  
end.
