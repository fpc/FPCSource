{ %fail }

{ Source provided for Free Pascal Bug Report 2362 }
{ Submitted by "Maarten Bekers" on  2003-02-07 }
{ e-mail: fpc-bugs@elebbs.com }
unit tw2362;

interface

type blah = function: integer;
function iee: integer;


var blah: blah;

begin
  blah := iee;
end.
