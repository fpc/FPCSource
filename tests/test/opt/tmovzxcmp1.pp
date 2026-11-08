{ %OPT=-O2 }
{ tests that the x86 peephole optimizer does not narrow the comparison of a
  zero-extended byte or word to the byte or word itself when the flags are
  then read with a signed condition, or when the constant is negative: the
  narrowed value is negative whenever its top bit is set }
program tmovzxcmp1;

{$mode objfpc}

type
  TQ = (q0, q1, q2);

var
  gDummy: longint;
  gErrors: longint;

function LookupByte(i: longint): byte;
  begin
    result:=byte(i);
  end;

function LookupWord(i: longint): word;
  begin
    result:=word(i);
  end;

{ in each test the early exit keeps the result live across the call; with the
  register allocation of the compiler this was written against, the looked-up
  value then stays in the register the call returned it in and is
  zero-extended there right before the comparison, the shape the optimizations
  narrow.  UnsignedAbove and Is200 are read with an unsigned and an equality
  condition, which may still be narrowed }

function UnsignedAbove(i: longint; p: plongint): longint;
  var
    raw: cardinal;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw>2 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function Is200(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw=200 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function ClampByte(i: longint; p: plongint): TQ;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=q0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw>ord(high(TQ)) then
      result:=q2
    else
      result:=TQ(raw);
  end;

function ClampWord(i: longint; p: plongint): TQ;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=q0;
        exit;
      end;
    raw:=LookupWord(p^ xor p^ xor i);
    if raw>ord(high(TQ)) then
      result:=q2
    else
      result:=TQ(raw);
  end;

function IsPositive(i: longint; p: plongint): TQ;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=q0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw>0 then
      result:=q1
    else
      result:=TQ(raw);
  end;

function JumpAbove(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw>2 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function JumpMinusOne(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    if raw=-1 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function JumpMinusOneWord(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupWord(p^ xor p^ xor i);
    if raw=-1 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function SetAbove(i: longint; p: plongint): boolean;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=false;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    result:=raw>2;
  end;

function SetMinusOne(i: longint; p: plongint): boolean;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=false;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    result:=raw=-1;
  end;

{ a byte compared out of a zero-extended word must not become a word
  comparison }
function LowByteIs2(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupWord(p^ xor p^ xor i);
    if byte(raw)=2 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function LowByteBelow3(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupWord(p^ xor p^ xor i);
    if byte(raw)<3 then
      begin
        result:=gDummy+7;
        exit;
      end;
    result:=raw;
  end;

function CaseRange(i: longint; p: plongint): longint;
  var
    raw: longint;
  begin
    if not assigned(p) then
      p:=@gDummy;
    if i>=$110000 then
      begin
        result:=0;
        exit;
      end;
    raw:=LookupByte(p^ xor p^ xor i);
    case raw of
      0:
        result:=gDummy+3;
      1..200:
        result:=gDummy+5;
    else
      result:=gDummy+7;
    end;
  end;

procedure Check(const name: string; arg, got, expected: longint);
  begin
    if got<>expected then
      begin
        writeln('error: ',name,'(',arg,') = ',got,', expected ',expected);
        inc(gErrors);
      end;
  end;

const
  ByteArgs: array[0..6] of longint = (0, 2, 3, 127, 128, 200, 255);
  WordArgs: array[0..5] of longint = (0, 2, 3, 32767, 32768, 65535);
  LowByteArgs: array[0..5] of longint = (2, 3, 258, 515, 32770, 65535);

var
  k, v: longint;
begin
  gErrors:=0;
  for k:=low(ByteArgs) to high(ByteArgs) do
    begin
      v:=ByteArgs[k];
      if v>2 then
        begin
          Check('ClampByte',v,ord(ClampByte(v,nil)),2);
          Check('JumpAbove',v,JumpAbove(v,nil),7);
        end
      else
        begin
          Check('ClampByte',v,ord(ClampByte(v,nil)),v);
          Check('JumpAbove',v,JumpAbove(v,nil),v);
        end;
      Check('IsPositive',v,ord(IsPositive(v,nil)),ord(v>0));
      Check('JumpMinusOne',v,JumpMinusOne(v,nil),v);
      if v>2 then
        Check('UnsignedAbove',v,UnsignedAbove(v,nil),7)
      else
        Check('UnsignedAbove',v,UnsignedAbove(v,nil),v);
      if v=200 then
        Check('Is200',v,Is200(v,nil),7)
      else
        Check('Is200',v,Is200(v,nil),v);
      Check('SetAbove',v,ord(SetAbove(v,nil)),ord(v>2));
      Check('SetMinusOne',v,ord(SetMinusOne(v,nil)),0);
      if v=0 then
        Check('CaseRange',v,CaseRange(v,nil),3)
      else if v<=200 then
        Check('CaseRange',v,CaseRange(v,nil),5)
      else
        Check('CaseRange',v,CaseRange(v,nil),7);
    end;
  for k:=low(WordArgs) to high(WordArgs) do
    begin
      v:=WordArgs[k];
      if v>2 then
        Check('ClampWord',v,ord(ClampWord(v,nil)),2)
      else
        Check('ClampWord',v,ord(ClampWord(v,nil)),v);
      Check('JumpMinusOneWord',v,JumpMinusOneWord(v,nil),v);
    end;
  for k:=low(LowByteArgs) to high(LowByteArgs) do
    begin
      v:=LowByteArgs[k];
      if byte(v)=2 then
        Check('LowByteIs2',v,LowByteIs2(v,nil),7)
      else
        Check('LowByteIs2',v,LowByteIs2(v,nil),v);
      if byte(v)<3 then
        Check('LowByteBelow3',v,LowByteBelow3(v,nil),7)
      else
        Check('LowByteBelow3',v,LowByteBelow3(v,nil),v);
    end;
  if gErrors<>0 then
    halt(1);
  writeln('ok');
end.
