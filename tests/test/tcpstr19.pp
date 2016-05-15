{ %skiptarget=android }
program tcpstr19;

// test conversions from and to rawbytestring
// test that copy function returns the same def as argument
// this test can be only run with the compiler built right now on the
// same system

{$APPTYPE CONSOLE}
{$ifdef fpc}
  {$MODE DELPHIUNICODE}
{$endif}

uses
  {$ifdef unix} {$ifdef darwin}iosxwstr{$else}cwstring{$endif}, {$endif}
  SysUtils;
var
  S: AnsiString;
  R: RawByteString;
begin
  S := UTF8Encode('Test');
  if StringCodePage(S) <> CP_UTF8 then
    halt(1);
  S := Copy('Test', 1, 2);
  if StringCodePage(S) <> DefaultSystemCodePage then
    halt(2);
  if StringCodePage(Copy(UTF8Encode('Test'), 1, 2)) <> CP_UTF8 then
    halt(3);
  R := 'Test';
  if StringCodePage(R) <> DefaultSystemCodePage then
    halt(4);
end.
