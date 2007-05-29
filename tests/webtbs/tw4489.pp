{ Source provided for Free Pascal Bug Report 4489 }
{ Submitted by "Vincent Snijders" on  2005-11-04 }
{ e-mail: vsnijders@quicknet.nl }
program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here };

var
  s: string;
  u: SizeUInt;
  d: qword;

begin
   d:=11111111;
   u:=1111;
   s := format('pid=%d', [u]);
   writeln(s);
   s := format('pid=%d', [d]);
   writeln(s);
end.

