program tcpstr28;

{$codepage utf8}

const
  Test = '𝄞𤽜';
  UTF8Test = UTF8String(Test);
  UTF16Test = UnicodeString(Test);

var
  utf8str: UTF8String = Test;
  utf16str: UnicodeString = Test;

begin
  if Length(UTF8Test) <> 8 then
    Halt(1);
  if Length(utf8str) <> 8 then
    Halt(1);
  if Length(UTF16Test) <> 4 then
    Halt(1);
  if Length(utf16str) <> 4 then
    Halt(1);
end.
