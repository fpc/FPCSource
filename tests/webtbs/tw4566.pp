{ Source provided for Free Pascal Bug Report 4566 }
{ Submitted by "Vincent Snijders" on  2005-12-04 }
{ e-mail: vsnijders@quicknet.nl }
program bug4566;

{$mode objfpc}{$H+}

var
  s: string;

begin
  //accidently use #1310 instead of #13#10
  s := 'Message Text' + #1310 + #1310 + 'More Text';
end.
