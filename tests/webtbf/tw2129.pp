{ %version=1.1 }
{ %fail }
{ Source provided for Free Pascal Bug Report 2129 }
{ Submitted by "Bill Rayer" on  2002-09-18 }
{ e-mail: lingolanguage@hotmail.com }

{$mode delphi}

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
var
  single1 : single;
  dbl1 : double;
begin
  single1 := single(dbl1);
end.
