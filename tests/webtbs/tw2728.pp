{ Source provided for Free Pascal Bug Report 2728 }
{ Submitted by "marco (the gory bugs department)" on  2003-10-09 }
{ e-mail:  }
{$mode delphi}
type baseclass = class end;
     tbaseclass= class of baseclass;

function test (c : tbaseclass):longint;

var o :tobject;

begin
  o:=tobject(c);   // illegal type conversion here
end;

begin
end.
