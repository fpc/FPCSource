{ Source provided for Free Pascal Bug Report 1907 }
{ Submitted by "Michail A.Baikov" on  2002-03-31 }
{ e-mail: russia@freepascal.org }
{$MODE DELPHI}
var
    a:function (k:byte):Pointer; cdecl;
    b:function:Pointer;
begin
    b:=a(0);
end.

{
  $Log$
  Revision 1.2  2002-09-07 15:40:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/04/13 08:05:38  carl
  + web bug test #1907

}
