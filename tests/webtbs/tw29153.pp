program bug;
{$IFDEF FPC}
{$CODEPAGE UTF8}
{$ENDIF}
const
  c1: RawByteString = 'a';
  c2: RawByteString = 'aa';
  c3: RawByteString = 'aaa';
begin
  writeln(StringCodePage(c1));
  writeln(StringCodePage(c2));
  writeln(StringCodePage(c3));
  if stringcodepage(c1)<>CP_UTF8 then
    halt(1);
  if stringcodepage(c2)<>CP_UTF8 then
    halt(2);
  if stringcodepage(c3)<>CP_UTF8 then
    halt(3);

  c1:='a';
  c2:='aa';
  c3:='aaa';
  writeln(StringCodePage(c1));
  writeln(StringCodePage(c2));
  writeln(StringCodePage(c3));
  if stringcodepage(c1)<>CP_UTF8 then
    halt(4);
  if stringcodepage(c2)<>CP_UTF8 then
    halt(5);
  if stringcodepage(c3)<>CP_UTF8 then
    halt(6);
end.
