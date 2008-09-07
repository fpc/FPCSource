program punicodechartest;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;
var
 astr: ansistring;
 wstr: widestring;
 ustr: unicodestring;
begin
 astr:= '';
 wstr:= '';
 ustr:= '';
 writeln(ptrint(pansichar(astr)));
 flush(output);
 writeln(ptrint(pwidechar(wstr)));
 flush(output);
 writeln(ptrint(punicodechar(ustr)));
 flush(output);
 writeln(ord(pansichar(astr)^));
 flush(output);
 writeln(ord(pwidechar(wstr)^));
 flush(output);
 writeln(ord(punicodechar(ustr)^));
 flush(output);
end.
