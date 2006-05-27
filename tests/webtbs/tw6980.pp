{ %target=win32,go32v2,win64 }
program Project1;

{$mode objfpc}{$H+}

uses
  SysUtils
  { add your units here };

var
  p: string;
  e: string;
begin
  p := 'C:\test';
  e := ExpandFileName('c:\test');
  writeln('Expanded: ',e);
  if (p<>e) then halt(1);
  writeln('ok');
end.
