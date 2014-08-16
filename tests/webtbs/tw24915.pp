program err;

{$mode objfpc}{$H+}

uses SysUtils;

function test(const a, b: string): string; inline;
begin
  result:=b;
end;

const path: string = 'C:\123456789012345678901234567890\test.txt';
var t: string;

begin
  t:=test(ExtractFilePath(path), ExtractFilePath(path));
  writeln(stringcodepage(path));
  writeln('Path: '+t);
end.
