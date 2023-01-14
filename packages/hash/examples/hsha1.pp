// See some samples in: http://en.wikipedia.org/wiki/Hash-based_message_authentication_code
program hsha1;

{$mode objfpc}
{$H+}

uses
{$ifdef unix}
  cwstring,
{$endif}
  HMAC;

var
  S : String;

begin
  // for HMAC_SHA1("", "") = 0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d
  S:=HMACSHA1Print(HMACSHA1Digest('', ''));
  WriteLn('Example 1: ', S);
  if (S<>'fbdb1d1b18aa6c08324b7d64b71fb76370690e1d') then
    begin
    Writeln('Failed 1');
    Halt(1);
    end;
  // for HMAC_SHA1("key", "The quick brown fox jumps over the lazy dog") = 0xde7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9
  S:=HMACSHA1('key', 'The quick brown fox jumps over the lazy dog');
  WriteLn('Example 2: ', S);
  if (S<>'de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9') then
    begin
    Writeln('Failed 2');
    Halt(2);
    end;
end.

