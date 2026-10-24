{ %OPT=-O2 -Cro }
program toverflow2;
{$mode objfpc}{$H+}{$R+}{$Q+}
uses SysUtils;

var
  Checks, Failures: LongInt;

procedure Check(Signed: Boolean; Bits: Integer; Op: Char;
  A, B, Expected: QWord; Overflow: Boolean = False);
var
  A32, B32, R32: LongInt;
  U32, V32, W32: LongWord;
  A64, B64, R64: Int64;
  Actual: QWord;
  Raised: Boolean;
begin
  Inc(Checks);
  Raised := False;
  Actual := 0;
  try
    if Signed then
      if Bits = 32 then
        begin
          A32 := LongInt(A); B32 := LongInt(B);
          case Op of
            '+': R32 := A32 + B32;
            '-': R32 := A32 - B32;
            '*': R32 := A32 * B32;
          end;
          Actual := QWord(Int64(R32));
        end
      else
        begin
          A64 := Int64(A); B64 := Int64(B);
          case Op of
            '+': R64 := A64 + B64;
            '-': R64 := A64 - B64;
            '*': R64 := A64 * B64;
          end;
          Actual := QWord(R64);
        end
    else if Bits = 32 then
      begin
        U32 := LongWord(A); V32 := LongWord(B);
        case Op of
          '+': W32 := U32 + V32;
          '-': W32 := U32 - V32;
          '*': W32 := U32 * V32;
        end;
        Actual := W32;
      end
    else
      case Op of
        '+': Actual := A + B;
        '-': Actual := A - B;
        '*': Actual := A * B;
      end;
  except
    on E: EIntOverflow do Raised := True;
    on E: ERangeError do Raised := True;
  end;
  if (Raised <> Overflow) or (not Raised and (Actual <> Expected)) then
    begin
      Inc(Failures);
      WriteLn('FAIL ', Checks, ': signed=', Signed, ' bits=', Bits,
        ' ', A, Op, B, ' result=', Actual, ' expected=', Expected,
        ' raised=', Raised, ' overflow=', Overflow);
    end;
end;

procedure SignedChecks(Bits: Integer; Lo, Hi: Int64);

  procedure C(Op: Char; A, B, Expected: Int64; Overflow: Boolean = False);
  begin
    Check(True, Bits, Op, QWord(A), QWord(B), QWord(Expected), Overflow);
  end;

begin
  C('+', 0, 0, 0); C('+', 0, 1, 1); C('+', 1, 0, 1);
  C('+', Hi, 0, Hi); C('+', Lo, 0, Lo); C('+', 0, Lo, Lo);
  C('+', Lo, Hi, -1); C('+', -1, 1, 0); C('+', 1, -1, 0);
  C('+', Hi, 1, 0, True); C('+', 1, Hi, 0, True);
  C('+', Lo, -1, 0, True); C('+', -1, Lo, 0, True);
  C('+', Hi, Hi, 0, True); C('+', Lo, Lo, 0, True);
  C('-', 0, 0, 0); C('-', 1, 0, 1); C('-', 0, 1, -1);
  C('-', Hi, 0, Hi); C('-', Lo, 0, Lo);
  C('-', Hi, Hi, 0); C('-', Lo, Lo, 0); C('-', -1, Lo, Hi);
  C('-', Hi, -1, 0, True); C('-', Lo, 1, 0, True);
  C('-', 0, Lo, 0, True); C('-', Lo, Hi, 0, True);
  C('-', Hi, Lo, 0, True);
  C('*', 0, 0, 0); C('*', 0, 1, 0); C('*', 1, 0, 0);
  C('*', 1, 1, 1); C('*', 0, Lo, 0); C('*', Lo, 0, 0);
  C('*', Hi, 0, 0); C('*', 1, Lo, Lo); C('*', Lo, 1, Lo);
  C('*', 1, Hi, Hi); C('*', Hi, 1, Hi); C('*', Hi, -1, -Hi);
  C('*', -1, -1, 1); C('*', -2, 2, -4); C('*', -2, -2, 4);
  C('*', Hi, 2, 0, True); C('*', Lo, 2, 0, True);
  C('*', Lo, -1, 0, True); C('*', -1, Lo, 0, True);
end;

procedure UnsignedChecks(Bits: Integer; Hi: QWord);

  procedure C(Op: Char; A, B, Expected: QWord; Overflow: Boolean = False);
  begin
    Check(False, Bits, Op, A, B, Expected, Overflow);
  end;

begin
  C('+', 0, 0, 0); C('+', 0, 1, 1); C('+', 1, 0, 1);
  C('+', Hi, 0, Hi); C('+', 0, Hi, Hi); C('+', Hi - 1, 1, Hi);
  C('+', Hi, 1, 0, True); C('+', 1, Hi, 0, True);
  C('+', Hi, Hi, 0, True);
  C('-', 0, 0, 0); C('-', 1, 0, 1); C('-', 1, 1, 0);
  C('-', Hi, 0, Hi); C('-', Hi, Hi, 0); C('-', Hi, Hi - 1, 1);
  C('-', 0, 1, 0, True); C('-', 0, Hi, 0, True);
  C('-', Hi - 1, Hi, 0, True);
  C('*', 0, 0, 0); C('*', 0, 1, 0); C('*', 1, 0, 0);
  C('*', 1, 1, 1); C('*', 0, Hi, 0); C('*', Hi, 0, 0);
  C('*', 1, Hi, Hi); C('*', Hi, 1, Hi); C('*', 2, 2, 4);
  C('*', Hi, 2, 0, True); C('*', 2, Hi, 0, True);
  C('*', Hi, Hi, 0, True);
end;

begin
  SignedChecks(32, Low(LongInt), High(LongInt));
  UnsignedChecks(32, High(LongWord));
  SignedChecks(64, Low(Int64), High(Int64));
  UnsignedChecks(64, High(QWord));
  Check(True, 32, '*', 46340, 46340, 2147395600);
  Check(True, 32, '*', 46341, 46341, 0, True);
  Check(False, 32, '*', 65535, 65535, 4294836225);
  Check(False, 32, '*', 65536, 65536, 0, True);
  Check(True, 64, '*', 3037000499, 3037000499, 9223372030926249001);
  Check(True, 64, '*', 3037000500, 3037000500, 0, True);
  Check(False, 64, '*', 4294967295, 4294967295, 18446744065119617025);
  Check(False, 64, '*', 4294967296, 4294967296, 0, True);
  if Failures <> 0 then Halt(1);
end.
