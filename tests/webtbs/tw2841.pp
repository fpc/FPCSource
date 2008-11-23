{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 2841 }
{ Submitted by "Mattias Gaertner" on  2003-12-09 }
{ e-mail: mattias@freepascal.org }
program ShortStringNotInitialized;

{$mode objfpc}{$H+}

procedure DoSomething;
var s: shortstring;
begin
  s[1]:=chr(3);
end;

begin
end.
