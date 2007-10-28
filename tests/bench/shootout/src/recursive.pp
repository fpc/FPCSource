(* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Josh Goldfoot
   modified by Vincent Snijders
*)

program recursive;

{$mode objfpc}{$I-}{$OPTIMIZATION TAILREC}

var
   n : longint;

function Ack(x : longint; y : longint): longint;
begin
   if x = 0 then
      Ack := y + 1
   else if y = 0 then
      Ack := Ack(x - 1, 1)
   else Ack := Ack(x-1, Ack(x, y-1));
end; { Ack }

function Fib(n : longint): longint;
begin
   if n < 2 then
      Fib := 1
   else Fib := Fib(n - 2) + Fib(n - 1)
end; { Fib }

function FibFP(n : double): double; inline;
begin
   if n < 2 then
      FibFP := 1
   else FibFP := FibFP(n - 2) + FibFP(n - 1)
end; { FibFP }

function Tak(x : longint; y: longint; z : longint): longint;
begin
   if y < x then
      Tak := Tak( Tak(x-1, y, z), Tak(y-1, z, x), Tak(z-1, x, y) )
   else Tak := z;
end; { Tak }

function TakFP(x : double; y: double; z : double): double;
begin
   if y < x then
      TakFP := TakFP( TakFP(x-1, y, z), TakFP(y-1, z, x), TakFP(z-1, x, y) )
   else TakFP := z;
end; { TakFP }

begin
   if ParamCount = 1 then begin
      Val(ParamStr(1), n);
      n := n - 1;
   end
      else n := 2;

   writeLn('Ack(3,', n + 1, '): ', Ack(3, n+1));
   writeLn('Fib(', (28.0 + n):1:1, '): ', FibFP(28.0 + n):1:1);
   writeLn('Tak(', 3 * n,',', 2 * n, ',', n, '): ', Tak(3*n, 2*n, n));
   writeLn('Fib(3): ', Fib(3));
   writeLn('Tak(3.0,2.0,1.0): ', TakFP(3.0,2.0,1.0):1:1);
end.

