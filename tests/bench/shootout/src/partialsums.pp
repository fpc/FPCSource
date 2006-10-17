(* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Josh Goldfoot *)

program partialSums;

uses SysUtils, Math;
const
   twoThirds = 2.0 / 3.0;
var
   n, code, k: longint;
   sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8, sum9, k3, sink, cosk, a: double;

begin
   if paramCount() = 1
      then Val (ParamStr (1),n,Code)
      else n := 25000;
   a := -1.0;
   for k := 1 to n do
   begin
      k3 := intpower(k, 3);
      sink := sin(k);
      cosk := cos(k);
      a := -a;
      sum1 := sum1 + intpower(twoThirds,k - 1);
      sum2 := sum2 + 1.0 /sqrt(k);
      sum3 := sum3 + 1.0/(k*(k+1.0));
      sum4 := sum4 + 1.0/(k3*(sink*sink));
      sum5 := sum5 + 1.0/(k3*(cosk*cosk));
      sum6 := sum6 + 1.0/k;
      sum7 := sum7 + 1.0/intpower(k, 2);
      sum8 := sum8 + a/k;
      sum9 := sum9 + a/(k + k - 1.0 );
   end;

   writeLn(sum1:10:9, #9, '(2/3)^k');
   writeLn(sum2:10:9, #9,'k^-0.5');
   writeLn(sum3:10:9, #9,'1/k(k+1)');
   writeLn(sum4:10:9, #9,'Flint Hills');
   writeLn(sum5:10:9, #9,'Cookson Hills');
   writeLn(sum6:10:9, #9,'Harmonic');
   writeLn(sum7:10:9, #9,'Riemann Zeta');
   writeLn(sum8:10:9, #9,'Alternating Harmonic');
   writeLn(sum9:10:9, #9,'Gregory');
end.