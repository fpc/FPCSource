// to have correct test result with delphi set codepage option to 866
program tcpstr17;
{$apptype console}
{$ifdef fpc}
  {$mode delphi}
  {$codepage cp866}
{$endif}

{$ifdef unix}
uses
  cwstring;
{$endif}

procedure TestRawByte(const Source: RawByteString; cp: word; const reason: integer);
begin
  Writeln(StringCodePage(Source), ' ', Source);
  if StringCodePage(Source) <> cp then
    halt(reason);
end;

const
  test: array[0..4] of ansichar = 'test'#0;
var
  s: rawbytestring;
  ss: shortstring;
  c: ansichar;
  w: widechar;
begin
  s := 'test';
  ss := 'test';
  TestRawByte(s, 866, 1);
  TestRawByte(ss, DefaultSystemCodePage, 2);
  TestRawByte(AnsiChar('t'), 866, 3);
  c := 't';
  TestRawByte(c, DefaultSystemCodePage, 4);
  TestRawByte(WideChar('t'), 866, 5);
  w := 't';
  TestRawByte(w, DefaultSystemCodePage, 6);
  TestRawByte(test, DefaultSystemCodePage, 7);
  TestRawByte(PAnsiChar(@test[0]), DefaultSystemCodePage, 8);
end.
