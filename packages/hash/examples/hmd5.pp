// See some samples in: http://en.wikipedia.org/wiki/Hash-based_message_authentication_code
program hmd5;

{$mode objfpc}{$H+}

uses
  HMAC;

begin
  // for HMAC_MD5("", "") = 0x74e6f7298a9c2d168935f58c001bad88
  WriteLn('Example 1: ', HMACMD5Print(HMACMD5Digest('', '')));
  // for HMAC_MD5("key", "The quick brown fox jumps over the lazy dog") = 0x80070713463e7749b90c2dc24911e275
  WriteLn('Example 2: ', HMACMD5('key', 'The quick brown fox jumps over the lazy dog'));
end.

