{ %fail }

{ Source provided for Free Pascal Bug Report 4911 }
{ Submitted by "Joost van der Sluis" on  2006-03-17 }
{ e-mail: joost at cnoc - nl }
program LongintTest;

{$mode objfpc}{$H+}

var l : longint;
begin
//l :=  335544569;  Gives an exception in my case
  l :=  43;         // results garbage
  writeln('Errpr: ' + l);
end.
