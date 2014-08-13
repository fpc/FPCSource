program sha1test;
{$mode objfpc}{$h+}

uses SysUtils, sha1;

function performTest: cardinal;
// Runs test and returns result code (0=success)
var
  s, sdig: string;
  i: integer;
  ctx: TSHA1Context;
  d: TSHA1Digest;
begin
  result := 0;
  sdig := SHA1Print(SHA1String('abc'));
  if sdig <> 'a9993e364706816aba3e25717850c26c9cd0d89d' then
    result := result or 1;

  sdig := SHA1Print(SHA1String('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'));
  if sdig <> '84983e441c3bd26ebaae4aa1f95129e5e54670f1' then
    result := result or 2;

  // SHA-1 of a million 'a' symbols
  SetLength(s, 1000);
  for i := 1 to 1000 do s[i] := 'a';
  SHA1Init(ctx);
  for i := 0 to 999 do
    SHA1Update(ctx, PChar(s)^, 1000);
  SHA1Final(ctx, d);
  sdig := SHA1Print(d);
  if sdig <> '34aa973cd4c4daa4f61eeb2bdbad27316534016f' then
    result := result or 4;
end;

var
  StartTime, EndTime: TDateTime;
  code: cardinal;
begin
  writeln('Performing basic SHA-1 test...');
  code:=performTest;

  if code = 0 then
    writeln('Basic SHA-1 tests passed')
  else
  begin
    writeln('SHA-1 tests failed: ', code);
  end;
  Halt(code);	
end.
