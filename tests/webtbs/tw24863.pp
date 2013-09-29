{ %OPT=-Sc }
program test2;

{$mode objfpc}{$H+}

// This demo program abuses FPC's custom operator facility to simulate
// Vector Pascal's \+ (reduce-add) operator, which is derived from APL's
// +/ function/operator.
//
// If the dummy record type (TReduce) is empty, there will be a runtime error
// when the operator result is assigned. This can be fixed either by
// explicitly importing the Variants unit, or by changing the operator
// function to return an integer.

type	t1= array of integer;
	TReduce= record
//	           x: variant
	         end;

var	a1: t1;
	reduce: TReduce = ();


procedure print(const a: t1);

var	i: integer;

begin
  for i := Low(a) to High(a) do
    Write(a[i], ' ');
  WriteLn
end { print } ;


operator + (const r: TReduce; const a: t1): variant;

var	i: integer;

begin
  result := 0;
  for i := Low(a) to High(a) do
    result += a[i]
end { + } ;


begin
//  a1 := t1.create(1,2,3,4,5);	Requires trunk
//  a1 := t1([1,2,3,4,5]);	Doesn't work without tuple support
  SetLength(a1, 5);
  a1[0] := 1;
  a1[1] := 2;
  a1[2] := 3;
  a1[3] := 4;
  a1[4] := 5;
  WriteLn('a1:');
  print(a1);
  WriteLn('+/ a1:');
  WriteLn(reduce + a1);
  WriteLn
end.

