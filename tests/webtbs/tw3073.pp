{ Source provided for Free Pascal Bug Report 3073 }
{ Submitted by "Martin Schreiber" on  2004-04-30 }
{ e-mail:  }
program project1;

 {$mode objfpc}{$H+}

uses
 Classes;
var
 po1,po2: pwidechar;
begin
 po1:= 'abc';
 po2:= po1+1;
 if po2-po1<>1 then
   halt(1);
 if pwidechar(24)-pwidechar(22)<>1 then
  halt(1);
end.
