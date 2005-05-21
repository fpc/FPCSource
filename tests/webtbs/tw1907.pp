{ Source provided for Free Pascal Bug Report 1907 }
{ Submitted by "Michail A.Baikov" on  2002-03-31 }
{ e-mail: russia@freepascal.org }
{$ifdef fpc}{$MODE DELPHI}{$endif}

function f:pointer;
begin
  f:=nil;
end;

function f1(k:byte):Pointer; cdecl;
begin
  result:=f;
end;

var
    a:function (k:byte):Pointer; cdecl;
    b:function:Pointer;
begin
    a:=f1;
    b:=a(0);
end.
