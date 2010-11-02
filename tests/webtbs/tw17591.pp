program comparetext;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 sysutils{,msesysintf};
var
 int1: integer;
 ustr1,ustr2: unicodestring;
begin
 ustr1:= 'A';
 ustr2:= 'a';
 writeln(unicodecomparestr(ustr1,ustr2));
 writeln(unicodecomparetext(ustr1,ustr2));
end.
