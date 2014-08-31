program test;

{$mode objfpc}{$h+}

uses SysUtils;

var a: ansistring;

begin
  defaultfilesystemcodepage:=CP_UTF8;
  defaultrtlfilesystemcodepage:=CP_ASCII;
  a := DirectorySeparator+'.';
  a := ExpandFileName(a);
  if StringCodePage(a)<> defaultrtlfilesystemcodepage then
   halt(1);
end.