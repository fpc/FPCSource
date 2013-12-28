{ %opt=-Oonoconstprop }
{ %result=201 }

{ Source provided for Free Pascal Bug Report 2690 }
{ Submitted by "Tom Verhoeff" on  2003-09-22 }
{ e-mail: Tom.Verhoeff@acm.org }
program RangeError;
{$R+} { Range Checking Enabled }
var
  c: Char;
  d: 'a' .. 'z';  { subtype of Char }
  a: array [ 'a' .. 'z' ] of Integer;
begin
  c := chr ( ord ( 'a' ) - 1 )       { OK }
; d := c   { value of c is outside the range of d,
             should be caught, but is not }
; a [ d ] := 0  { this is now dangerous! }
end.
