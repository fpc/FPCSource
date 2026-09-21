{ an exception raised while halt ends the program -- here by the finalization
  of a unit -- goes through the frame of halt, which never returns, to the
  handler around the call of halt. On win64 the return address of the last
  call such a routine makes must not be its final ret: the system's
  unwinder would take it for the routine's epilogue and read the caller's
  return address from the wrong place. Clobber leaves zeroes where that
  wrong place is, so that such an unwinder finds no handler at all }
program tb0771;

{$mode objfpc}

uses
  ub0771;

var
  caught: boolean;

procedure Clobber;
var
  a: array[0..255] of qword;
  i: longint;
begin
  for i:=low(a) to high(a) do
    a[i]:=0;
  if a[3]<>0 then
    writeln(a[5]);
end;

procedure DoHalt;
begin
  Clobber;
  halt(1);
end;

begin
  caught:=false;
  try
    DoHalt;
  except
    caught:=true;
  end;
  if not caught then
    halt(2);
  writeln('ok');
  halt(0);
end.
