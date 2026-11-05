{ %CPU=i386,x86_64 }
{ %OPT=-O3 -CpCOREI }

{ This test evaluates the CMOV blocks of OptPass2Jcc when both sets of moves
  write the same register and the second set still needs its old value.  A
  move that stays unconditional, or that TryCmpCMovOpts places ahead of the
  comparison, executes whichever way the condition goes, so neither may be
  used then.  -O3 because the latter needs a second pass. }

program tcond3;

{$mode objfpc}
{$goto on}

uses
  CPU;

var
  Failed: Boolean = False;

procedure Check(const Name: string; Got, Want: Int64);
begin
  if Got <> Want then
    begin
      WriteLn('FAIL: ', Name, ' gave ', Got, ', expected ', Want);
      Failed := True;
    end;
end;

function DoubleCardinal(Sel, R, L: Cardinal; out Old: Cardinal): Cardinal; noinline;
var
  D: Cardinal;
begin
  if Sel = 1 then
    begin
      D := L;
      R := 100;
    end
  else
    begin
      D := R;
      R := 200;
    end;
  Old := D;
  Result := R;
end;

function DoubleInt64(Sel: Cardinal; R, L: Int64; out Old: Int64): Int64; noinline;
var
  D: Int64;
begin
  if Sel = 1 then
    begin
      D := L;
      R := 100;
    end
  else
    begin
      D := R;
      R := 200;
    end;
  Old := D;
  Result := R;
end;

function DoublePointer(Sel: Cardinal; R, L, A, B: Pointer; out Old: Pointer): Pointer; noinline;
var
  D: Pointer;
begin
  if Sel = 1 then
    begin
      D := L;
      R := A;
    end
  else
    begin
      D := R;
      R := B;
    end;
  Old := D;
  Result := R;
end;

function DoubleWord(Sel: Cardinal; R, L: Word; out Old: Word): Word; noinline;
var
  D: Word;
begin
  if Sel = 1 then
    begin
      D := L;
      R := 100;
    end
  else
    begin
      D := R;
      R := 200;
    end;
  Old := D;
  Result := R;
end;

function DoubleRegisters(Sel, R, L, A, B: Cardinal; out Old: Cardinal): Cardinal; noinline;
var
  D: Cardinal;
begin
  if Sel = 1 then
    begin
      D := L;
      R := A;
    end
  else
    begin
      D := R;
      R := B;
    end;
  Old := D;
  Result := R;
end;

function DoubleTruncated(Sel: Cardinal; R: PtrUInt; L: Cardinal; out Old: Cardinal): PtrUInt; noinline;
var
  D: Cardinal;
begin
  if Sel = 1 then
    begin
      D := L;
      R := 100;
    end
  else
    begin
      D := Cardinal(R);
      R := 200;
    end;
  Old := D;
  Result := R;
end;

function DoubleReference(P, Q, S: PCardinal; L: Cardinal; out Old: Cardinal): PCardinal; noinline;
var
  D: Cardinal;
begin
  if P^ = 5 then
    begin
      D := L;
      P := Q;
    end
  else
    begin
      D := P^;
      P := S;
    end;
  Old := D;
  Result := P;
end;

function BranchSame(Outer, Sel, R, L: Cardinal; out Old: Cardinal): Cardinal; noinline;
var
  D: Cardinal;
begin
  D := 0;
  if Outer = 0 then
    begin
      if Sel = 1 then
        begin
          D := L;
          R := 100;
        end
      else
        begin
          D := R;
          R := 200;
        end;
    end
  else
    begin
      D := L + Outer;
      R := R + Outer;
    end;
  Old := D;
  Result := R;
end;

function BranchDifferent(Sel, R, L: Cardinal; out Old: Cardinal): Cardinal; noinline;
label
  First, Second;
var
  D: Cardinal;
begin
  if Sel = 1 then
    begin
      D := L;
      R := 100;
      goto First;
    end
  else
    begin
      D := R;
      R := 200;
      goto Second;
    end;
First:
  Old := D;
  Result := R + 1;
  Exit;
Second:
  Old := D;
  Result := R + 2;
end;

var
  U1: Cardinal;
  I1: Int64;
  W1: Word;
  P1: Pointer;
  V5: Cardinal = 5;
  V6: Cardinal = 6;
  V7: Cardinal = 7;
  V8: Cardinal = 8;
  PR: PCardinal;
begin
  if not CMOVSupport then
    begin
      WriteLn('unsupported');
      Halt(0);
    end;

  Check('DoubleCardinal 1', DoubleCardinal(1, 11, 22, U1), 100);
  Check('DoubleCardinal 1 old', U1, 22);
  Check('DoubleCardinal 2', DoubleCardinal(2, 11, 22, U1), 200);
  Check('DoubleCardinal 2 old', U1, 11);

  Check('DoubleInt64 1', DoubleInt64(1, 11, 22, I1), 100);
  Check('DoubleInt64 1 old', I1, 22);
  Check('DoubleInt64 2', DoubleInt64(2, 11, 22, I1), 200);
  Check('DoubleInt64 2 old', I1, 11);

  Check('DoublePointer 1', PtrInt(DoublePointer(1, Pointer(11), Pointer(22), Pointer(33), Pointer(44), P1)), 33);
  Check('DoublePointer 1 old', PtrInt(P1), 22);
  Check('DoublePointer 2', PtrInt(DoublePointer(2, Pointer(11), Pointer(22), Pointer(33), Pointer(44), P1)), 44);
  Check('DoublePointer 2 old', PtrInt(P1), 11);

  Check('DoubleWord 1', DoubleWord(1, 11, 22, W1), 100);
  Check('DoubleWord 1 old', W1, 22);
  Check('DoubleWord 2', DoubleWord(2, 11, 22, W1), 200);
  Check('DoubleWord 2 old', W1, 11);

  Check('DoubleRegisters 1', DoubleRegisters(1, 11, 22, 33, 44, U1), 33);
  Check('DoubleRegisters 1 old', U1, 22);
  Check('DoubleRegisters 2', DoubleRegisters(2, 11, 22, 33, 44, U1), 44);
  Check('DoubleRegisters 2 old', U1, 11);

  Check('DoubleTruncated 1', DoubleTruncated(1, 11, 22, U1), 100);
  Check('DoubleTruncated 1 old', U1, 22);
  Check('DoubleTruncated 2', DoubleTruncated(2, 11, 22, U1), 200);
  Check('DoubleTruncated 2 old', U1, 11);

  PR := DoubleReference(@V5, @V6, @V7, 22, U1);
  Check('DoubleReference 1', PR^, 6);
  Check('DoubleReference 1 old', U1, 22);
  PR := DoubleReference(@V8, @V6, @V7, 22, U1);
  Check('DoubleReference 2', PR^, 7);
  Check('DoubleReference 2 old', U1, 8);

  Check('BranchSame 1', BranchSame(0, 1, 11, 22, U1), 100);
  Check('BranchSame 1 old', U1, 22);
  Check('BranchSame 2', BranchSame(0, 2, 11, 22, U1), 200);
  Check('BranchSame 2 old', U1, 11);
  Check('BranchSame 3', BranchSame(3, 2, 11, 22, U1), 14);
  Check('BranchSame 3 old', U1, 25);

  Check('BranchDifferent 1', BranchDifferent(1, 11, 22, U1), 101);
  Check('BranchDifferent 1 old', U1, 22);
  Check('BranchDifferent 2', BranchDifferent(2, 11, 22, U1), 202);
  Check('BranchDifferent 2 old', U1, 11);

  if Failed then
    Halt(1);
  WriteLn('ok');
end.
