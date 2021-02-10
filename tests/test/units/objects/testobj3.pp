program testobj3;
uses
  Objects;

function GetRefCount(const S: RawByteString): SizeInt;
begin
  GetRefCount:=PSizeInt(PByte(S)-2*SizeOf(SizeInt))^;
end;

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
  if GetRefCount(S)<>1 then
    Error(1);
  coll := New(PRawByteStringCollection, Init(100, 100));
  coll^.AtInsert(0, S);
  if GetRefCount(S)<>2 then
    Error(2);
  S2 := RawByteString(coll^.At(0));
  if GetRefCount(S)<>3 then
    Error(3);
  if S2<>'Hello' then
    Error(4);
  if RawByteString(coll^.At(0))<>'Hello' then
    Error(5);
  Dispose(coll, Done);
  if GetRefCount(S)<>2 then
    Error(6);
end;

begin
  Test1;
  Writeln('Ok!');
end.
