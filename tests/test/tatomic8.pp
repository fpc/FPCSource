{ %NORUN }

program tatomic8;

{$mode objfpc}

procedure AvoidHint(aArg: array of Boolean);
begin
end;

var
  l: LongInt;
  b: Boolean;
  bb: ByteBool;
  wb: WordBool;
  lb: LongBool;
  qb: QWordBool;
  b8: Boolean8;
  b16: Boolean16;
  b32: Boolean32;
  b64: Boolean64;
begin
  l := 0;
  AtomicCmpExchange(l, 42, 84, b);
  AtomicCmpExchange(l, 42, 84, bb);
  AtomicCmpExchange(l, 42, 84, wb);
  AtomicCmpExchange(l, 42, 84, lb);
  AtomicCmpExchange(l, 42, 84, qb);
  AtomicCmpExchange(l, 42, 84, b8);
  AtomicCmpExchange(l, 42, 84, b16);
  AtomicCmpExchange(l, 42, 84, b32);
  AtomicCmpExchange(l, 42, 84, b64);
  AvoidHint([b, bb, wb, lb, qb, b8, b16, b32, b64]);
end.
