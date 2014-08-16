// See some samples in: http://en.wikipedia.org/wiki/Hash-based_message_authentication_code
program hsha1;

{$mode objfpc}{$H+}

uses
  HMAC;

begin
  // for HMAC_SHA1("", "") = 0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d
  WriteLn('Example 1: ', HMACSHA1Print(HMACSHA1Digest('', '')));
  // for HMAC_SHA1("key", "The quick brown fox jumps over the lazy dog") = 0xde7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9
  WriteLn('Example 2: ', HMACSHA1('key', 'The quick brown fox jumps over the lazy dog'));
end.

