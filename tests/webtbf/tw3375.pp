{ %fail }

{ Source provided for Free Pascal Bug Report 3375 }
{ Submitted by "Vincent Snijders" on  2004-10-29 }
{ e-mail: vslist@zonnet.nl }
program typecast;

{$mode objfpc}{$H+}

type
  T4bytes = array[0..3] of byte;

var
  w: word;
  p: pchar;
  a: T4bytes;

begin
  w := 0;
  p := pchar(w); // should fail, because sizeof(w)<>sizeof(p) on this platform.
  //a := T4Bytes(w); // should fail, and it does
end.

