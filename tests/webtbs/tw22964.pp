
{$ifdef fpc}
  {$mode delphi}
{$else}
  {$apptype console}
{$endif}

uses types;


function CopyTest(const S: string): string;
begin
  writeln(Copy(S, 1, 5));
  writeln(Copy(S, 2));
  // writeln(Copy(S));
end;

function arrTest(const S: TIntegerDynArray): string;
var x : TIntegerDynArray;
    i : integer;
begin
  x:=Copy(S, 1, 5);
  write('1:');
  for i:=0 to length(x)-1 do
    write(' ',x[i]:5);
  writeln;
  x:=Copy(S, 2);
  write('2:');
  for i:=0 to length(x)-1 do
    write(' ',x[i]:5);
  writeln;
end;

var testarr : TIntegerDynArray;
    i:integer;
begin
 setlength(testarr,10);
 for i:=0 to 9 do
   testarr[i]:=i;     // element values 0 based
 copytest('1234567'); // element values 1 based
 arrtest(testarr);
end.

{

Delphi XE output:

12345
234567
1:     1     2     3     4     5
2:     2     3     4     5     6     7     8     9
}
