{ Source provided for Free Pascal Bug Report 3579 }
{ Submitted by "Martin Schreiber" on  2005-01-21 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}
uses
 classes;
type
 stringarty = array of string;
var
 ar1,ar2: stringarty;

begin
 setlength(ar1,4);
 ar2:= copy(ar1);       //AV
end.
