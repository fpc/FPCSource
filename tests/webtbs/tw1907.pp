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

{
  $Log$
  Revision 1.3  2002-09-16 19:05:31  peter
    * fix compile mode

  Revision 1.2  2002/09/07 15:40:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/04/13 08:05:38  carl
  + web bug test #1907

}
