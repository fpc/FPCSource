{ Source provided for Free Pascal Bug Report 3083 }
{ Submitted by "Martin Schreiber" on  2004-05-04 }
{ e-mail:  }
program project1;

{$mode delphi}{$H+}

var
 ar1: array of ansistring;
begin
 setlength(ar1,1);
 ar1[0]:= 'abc';
 ar1:= nil;          //av!
end.
