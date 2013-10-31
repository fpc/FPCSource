{ %version=1.1 }
{ Source provided for Free Pascal Bug Report 2129 }
{ Submitted by "Bill Rayer" on  2002-09-18 }
{ e-mail: lingolanguage@hotmail.com }

(*
Comp() cast has different effect in FPC.
Compiles using Delphi4:
  dcc32 -CC fpc19
Compiles in FPC 1.0.6:
  ppc386 -WC fpc19

When run, the Delphi version shows -6.5E+18, but the FPC version
shows zero. In Delphi, the comp() cast actually moves 8 bytes from
the double into the comp without converting the data, but FPC uses
floating point instructions to convert the data and therefore prints
zero.

In Delphi, if you want to convert a double to a float, you just use
the assignment "comp1 := dbl1" which corresponds to the FLD/FIST
opcodes. FPC should not use the comp() cast for doing this, since
it introduces a subtle incompatibility with Delphi.
*)

program fpc19;
{ the same as tw2129.pp for cpu's were comp = int64 }
var
  comp1 : comp;
  dbl1 : double;
  s : string;
begin
{$ifdef cpui386}
  dbl1 := -1e-128;
  comp1 := comp(dbl1);
  str(comp1:23,s);
  if s<>' 0.00000000000000E+0000' then
    begin
      writeln('error: ',s);
      halt(1);
    end;
{$endif cpui386}
end.
