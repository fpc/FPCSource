procedure error(const s: string);
begin
  writeln('error: ',s);
  halt(1);
end;

procedure testlohiword;
var
  w: word;
  i: smallint;
begin
  w := $1234;
  i := $1234;
  if lo(w) <> (w and 255) then
    error('lo word');
  if lo(i) <> (i and 255) then
    error('lo integer');

  if hi(w) <> (w shr 8) then
    error('hi word');
  if hi(i) <> (i shr 8) then
    error('hi integer');
end;


procedure testlohilong;
var
  w: cardinal;
  i: longint;
begin
  w := $12345678;
  i := $12345678;
  if lo(w) <> (w and $ffff) then
    error('lo cardinal');
  if lo(i) <> (i and $ffff) then
    error('lo longint');

  if hi(w) <> (w shr 16) then
    error('hi cardinal');
  if hi(i) <> (i shr 16) then
    error('hi longint');
end;


procedure testlohiqword;
var
  w: qword;
  i: int64;
begin
  w := $12345678;
  w := w shl 32;
  w := w or $98765432;
  i := int64(w);
  if lo(w) <> (cardinal(w)) then
    error('lo qword');
  if longint(lo(i)) <> (longint(w)) then
    error('lo int64');

  if hi(w) <> (w shr 32) then
    error('hi qword');
  if hi(i) <> (i shr 32) then
    error('hi int64');
end;

begin
  testlohiword;
  testlohilong;
  testlohiqword;
end.
