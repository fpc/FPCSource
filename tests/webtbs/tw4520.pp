{ Source provided for Free Pascal Bug Report 4520 }
{ Submitted by "Martin Schreiber" on  2005-11-17 }
{ e-mail:  }
program project1;
 { $mode objfpc}{$h+}
uses
 sysutils;
var
 rea1,rea2: real;
 str1: string;
begin
 rea1:= 1e100;
 str1:= floattostr(rea1);
 if str1<>'1E100' then
   halt(1);
 writeln('1: ',rea1);
 writeln('2: ',str1);
end.
