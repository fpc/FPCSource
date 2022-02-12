program testobj3;
uses
  Objects;

procedure Error(ErrNo: Integer);
begin
  Writeln('Error! ', ErrNo);
  Halt(1);
end;

procedure Test1;
var
  coll: PRawByteStringCollection;
  S, S2: AnsiString;
begin
  Writeln('Test1');
  SetLength(S, 5);
  S[1] := 'H';
  S[2] := 'e';
  S[3] := 'l';
  S[4] := 'l';
  S[5] := 'o';
  writeln(stringrefcount(s));
  if stringrefcount(S)<>1 then
    Error(1);
  coll := New(PRawByteStringCollection, Init(100, 100));
  coll^.AtInsert(0, S);
  if stringrefcount(S)<>2 then
    Error(2);
  S2 := RawByteString(coll^.At(0));
  if stringrefcount(S)<>3 then
    Error(3);
  if S2<>'Hello' then
    Error(4);
  if RawByteString(coll^.At(0))<>'Hello' then
    Error(5);
  Dispose(coll, Done);
  if stringrefcount(S)<>2 then
    Error(6);
end;

begin
  Test1;
  Writeln('Ok!');
end.
