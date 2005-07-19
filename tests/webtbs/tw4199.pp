{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
uses
  Classes, SysUtils;
const
 stringconst = 'abc';
 widestringconst = widestring('abc');
var
 str1,str2 : widestring;
begin
 writeln('1 ',stringconst);             //ok
 writeln('2 ',widestring(stringconst)); //ok
 writeln('3 ',widestringconst);         //bad
 str1:= widestringconst;
 writeln('4 ',str1);                    //bad
 str2:= copy(widestringconst,1,3);
 writeln('5 ',str2);                    //bad
 if widestringconst<>stringconst then
   halt(1);
 if str1<>stringconst then
   halt(2);
 if str2<>stringconst then
   halt(3);
end.
