{ %OPT=-glhc }
{ Source provided for Free Pascal Bug Report 3661 }
{ Submitted by "Martin Schreiber" on  2005-02-16 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}
uses
  Classes;

begin
 HaltOnNotReleased := true;
 writeln('abc');
end.
