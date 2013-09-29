program tgeneric94;

{$mode objfpc}

type
  generic TTest<T> = record
    f: T;
  end;

  TRec = record
    x, y: LongInt;
  end;

type
  TTestTRec_Global = specialize TTest<TRec>;
const
  TRecSize_Global = SizeOf(TRec);

procedure DoTest;
type
  TRec = packed record
    a, b: Byte;
  end;
  TTestTRec_DoTest = specialize TTest<TRec>;
const
  TRecSize_DoTest = SizeOf(TRec);

  procedure Nested(out aActual, aExpected: LongInt);
  type
    TRec = packed record
      f1, f2: Word;
    end;
    TTestTRec_Nested = specialize TTest<TRec>;
  const
    TRecSize_Nested = SizeOf(TRec);
  var
    t: TTestTRec_Nested;
  begin
    aActual := SizeOf(t.f);
    aExpected := TRecSize_Nested;
  end;

procedure DoError(const aMessage: String);
begin
  Writeln(aMessage);
  ExitCode := 1;
  Halt;
end;

var
  tg: TTestTRec_Global;
  tt: TTestTRec_DoTest;
  act, expt: LongInt;
begin
  if SizeOf(tg.f) <> TRecSize_Global then
    DoError('Unexpected size of global TRec');
  if SizeOf(tt.f) <> TRecSize_DoTest then
    DoError('Unexpected size of DoTest TRec');
  Nested(act, expt);
  if act <> expt then
    DoError('Unexpected size of Nested TRec');
end;

begin
  DoTest;
end.
