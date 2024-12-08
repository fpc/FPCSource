program tatomic1;

{ test all possible variants of the Atomic* functions }

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TMyDigits = 0..9;

var
  ri8, i8: Int8;
  ru8, u8: UInt8;
  ri16, i16: Int16;
  ru16, u16: UInt16;
  ri32, i32: Int32;
  ru32, u32: UInt32;
  ri64, i64: Int64;
  ru64, u64: UInt64;
  rNatInt, NatInt: NativeInt;
  rNatUInt, NatUInt: NativeUInt;
  rMyDigits, MyDigits: TMyDigits;
  rp, p: Pointer;
  pl: PLongInt;
  bool: Boolean;
begin
  {* test 1 byte types *}

  i8 := 3;
  ri8 := AtomicIncrement(i8);
  if ri8 <> 4 then halt({$I %LINENUM%});
  if i8 <> 4 then halt({$I %LINENUM%});

  i8 := 4;
  ri8 := AtomicDecrement(i8);
  if ri8 <> 3 then halt({$I %LINENUM%});
  if i8 <> 3 then halt({$I %LINENUM%});

  u8 := 23;
  ru8 := AtomicIncrement(u8);
  if ru8 <> 24 then halt({$I %LINENUM%});
  if u8 <> 24 then halt({$I %LINENUM%});

  u8 := 12;
  ru8 := AtomicDecrement(u8);
  if ru8 <> 11 then halt({$I %LINENUM%});
  if u8 <> 11 then halt({$I %LINENUM%});

  i8 := 13;
  ri8 := AtomicIncrement(i8, 10);
  if ri8 <> 23 then halt({$I %LINENUM%});
  if i8 <> 23 then halt({$I %LINENUM%});

  i8 := 13;
  ri8 := AtomicDecrement(i8, 10);
  if ri8 <> 3 then halt({$I %LINENUM%});
  if i8 <> 3 then halt({$I %LINENUM%});

  u8 := 13;
  ru8 := AtomicIncrement(u8, 10);
  if ru8 <> 23 then halt({$I %LINENUM%});
  if u8 <> 23 then halt({$I %LINENUM%});

  u8 := 23;
  ru8 := AtomicDecrement(u8, 10);
  if ru8 <> 13 then halt({$I %LINENUM%});
  if u8 <> 13 then halt({$I %LINENUM%});

  u8 := 66;
  ru8 := AtomicExchange(u8, 44);
  if ru8 <> 66 then halt({$I %LINENUM%});
  if u8 <> 44 then halt({$I %LINENUM%});

  i8 := 66;
  ri8 := AtomicExchange(i8, 44);
  if ri8 <> 66 then halt({$I %LINENUM%});
  if i8 <> 44 then halt({$I %LINENUM%});

  u8 := 66;
  ru8 := AtomicCmpExchange(u8, 44, 77);
  if ru8 <> 66 then halt({$I %LINENUM%});
  if u8 <> 66 then halt({$I %LINENUM%});

  {* test 2 byte types *}

  i16 := 3;
  ri16 := AtomicIncrement(i16);
  if ri16 <> 4 then halt({$I %LINENUM%});
  if i16 <> 4 then halt({$I %LINENUM%});

  i16 := 4;
  ri16 := AtomicDecrement(i16);
  if ri16 <> 3 then halt({$I %LINENUM%});
  if i16 <> 3 then halt({$I %LINENUM%});

  u16 := 13;
  ru16 := AtomicIncrement(u16);
  if ru16 <> 14 then halt({$I %LINENUM%});
  if u16 <> 14 then halt({$I %LINENUM%});

  u16 := 12;
  ru16 := AtomicDecrement(u16);
  if ru16 <> 11 then halt({$I %LINENUM%});
  if u16 <> 11 then halt({$I %LINENUM%});

  u16 := 65;
  ru16 := AtomicExchange(u16, 97);
  if ru16 <> 65 then halt({$I %LINENUM%});
  if u16 <> 97 then halt({$I %LINENUM%});

  i16 := 65;
  ri16 := AtomicExchange(i16, 97);
  if ri16 <> 65 then halt({$I %LINENUM%});
  if i16 <> 97 then halt({$I %LINENUM%});

  i16 := 13;
  ri16 := AtomicIncrement(i16, 10);
  if ri16 <> 23 then halt({$I %LINENUM%});
  if i16 <> 23 then halt({$I %LINENUM%});

  i16 := 13;
  ri16 := AtomicDecrement(i16, 10);
  if ri16 <> 3 then halt({$I %LINENUM%});
  if i16 <> 3 then halt({$I %LINENUM%});

  u16 := 13;
  ru16 := AtomicIncrement(u16, 10);
  if ru16 <> 23 then halt({$I %LINENUM%});
  if u16 <> 23 then halt({$I %LINENUM%});

  u16 := 13;
  ru16 := AtomicDecrement(u16, 10);
  if ru16 <> 3 then halt({$I %LINENUM%});
  if u16 <> 3 then halt({$I %LINENUM%});

  {* test 4 byte types *}

  i32 := 3;
  ri32 := AtomicIncrement(i32);
  if ri32 <> 4 then halt({$I %LINENUM%});
  if i32 <> 4 then halt({$I %LINENUM%});

  i32 := 12;
  ri32 := AtomicDecrement(i32);
  if ri32 <> 11 then halt({$I %LINENUM%});
  if i32 <> 11 then halt({$I %LINENUM%});

  u32 := 3;
  ru32 := AtomicIncrement(u32);
  if ru32 <> 4 then halt({$I %LINENUM%});
  if u32 <> 4 then halt({$I %LINENUM%});

  u32 := 12;
  ru32 := AtomicDecrement(u32);
  if ru32 <> 11 then halt({$I %LINENUM%});
  if u32 <> 11 then halt({$I %LINENUM%});

  u32 := 65;
  ru32 := AtomicExchange(u32, 97);
  if ru32 <> 65 then halt({$I %LINENUM%});
  if u32 <> 97 then halt({$I %LINENUM%});

  i32 := 66;
  ri32 := AtomicCmpExchange(i32, 44, 77);
  if ri32 <> 66 then halt({$I %LINENUM%});
  if i32 <> 66 then halt({$I %LINENUM%});

  u32 := 13;
  ru32 := AtomicIncrement(u32, 10);
  if ru32 <> 23 then halt({$I %LINENUM%});
  if u32 <> 23 then halt({$I %LINENUM%});

{$ifdef fpc}
  u32 := 13;
  ru32 := AtomicDecrement(u32, 10);
  if ru32 <> 3 then halt({$I %LINENUM%});
  if u32 <> 3 then halt({$I %LINENUM%});
{$endif}

  i32 := 13;
  ri32 := AtomicIncrement(i32, 10);
  if ri32 <> 23 then halt({$I %LINENUM%});
  if i32 <> 23 then halt({$I %LINENUM%});

  i32 := 13;
  ri32 := AtomicDecrement(i32, 10);
  if ri32 <> 3 then halt({$I %LINENUM%});
  if i32 <> 3 then halt({$I %LINENUM%});

  {* test 8 byte types *}

  u64 := 3;
  ru64 := AtomicIncrement(u64);
  if ru64 <> 4 then halt({$I %LINENUM%});
  if u64 <> 4 then halt({$I %LINENUM%});

  u64 := 12;
  ru64 := AtomicDecrement(u64);
  if ru64 <> 11 then halt({$I %LINENUM%});
  if u64 <> 11 then halt({$I %LINENUM%});

  i64 := 3;
  ri64 := AtomicIncrement(i64);
  if ri64 <> 4 then halt({$I %LINENUM%});
  if i64 <> 4 then halt({$I %LINENUM%});

  i64 := 12;
  ri64 := AtomicDecrement(i64);
  if ri64 <> 11 then halt({$I %LINENUM%});
  if i64 <> 11 then halt({$I %LINENUM%});

  i64 := 65;
  ri64 := AtomicExchange(i64, 97);
  if ri64 <> 65 then halt({$I %LINENUM%});
  if i64 <> 97 then halt({$I %LINENUM%});

  u64 := 66;
  ru64 := AtomicCmpExchange(u64, 44, 77);
  if ru64 <> 66 then halt({$I %LINENUM%});
  if u64 <> 66 then halt({$I %LINENUM%});

  i64 := 13;
  ri64 := AtomicIncrement(i64, 10);
  if ri64 <> 23 then halt({$I %LINENUM%});
  if i64 <> 23 then halt({$I %LINENUM%});

  i64 := 13;
  ri64 := AtomicDecrement(i64, 10);
  if ri64 <> 3 then halt({$I %LINENUM%});
  if i64 <> 3 then halt({$I %LINENUM%});

  u64 := 13;
  ru64 := AtomicIncrement(u64, 10);
  if ru64 <> 23 then halt({$I %LINENUM%});
  if u64 <> 23 then halt({$I %LINENUM%});

{$ifdef fpc}
  u64 := 13;
  ru64 := AtomicDecrement(u64, 10);
  if ru64 <> 3 then halt({$I %LINENUM%});
  if u64 <> 3 then halt({$I %LINENUM%});
{$endif}

  {* test platform depending byte types *}

  NatInt := 3;
  rNatInt := AtomicIncrement(NatInt);
  if rNatInt <> 4 then halt({$I %LINENUM%});
  if NatInt <> 4 then halt({$I %LINENUM%});

  NatInt := 3;
  rNatInt := AtomicDecrement(NatInt);
  if rNatInt <> 2 then halt({$I %LINENUM%});
  if NatInt <> 2 then halt({$I %LINENUM%});

  NatUInt := 12;
  rNatUInt := AtomicIncrement(NatUInt);
  if rNatUInt <> 13 then halt({$I %LINENUM%});
  if NatUInt <> 13 then halt({$I %LINENUM%});

  NatUInt := 12;
  rNatUInt := AtomicDecrement(NatUInt);
  if rNatUInt <> 11 then halt({$I %LINENUM%});
  if NatUInt <> 11 then halt({$I %LINENUM%});

  NatUInt := 65;
  rNatUInt := AtomicExchange(NatUInt, 97);
  if rNatUInt <> 65 then halt({$I %LINENUM%});
  if NatUInt <> 97 then halt({$I %LINENUM%});

  NatUInt := 66;
  rNatUInt := AtomicCmpExchange(NatUInt, 44, 77);
  if rNatUInt <> 66 then halt({$I %LINENUM%});
  if NatUInt <> 66 then halt({$I %LINENUM%});

  NatInt := 13;
  rNatInt := AtomicIncrement(NatInt, 10);
  if rNatInt <> 23 then halt({$I %LINENUM%});
  if NatInt <> 23 then halt({$I %LINENUM%});

  NatInt := 13;
  rNatInt := AtomicDecrement(NatInt, 10);
  if rNatInt <> 3 then halt({$I %LINENUM%});
  if NatInt <> 3 then halt({$I %LINENUM%});

  NatUInt := 13;
  rNatUInt := AtomicIncrement(NatUInt, 10);
  if rNatUInt <> 23 then halt({$I %LINENUM%});
  if NatUInt <> 23 then halt({$I %LINENUM%});

{$ifdef fpc}
  NatUInt := 13;
  rNatUInt := AtomicDecrement(NatUInt, 10);
  if rNatUInt <> 3 then halt({$I %LINENUM%});
  if NatUInt <> 3 then halt({$I %LINENUM%});
{$endif}

  {* test pointer type *}

  (*p := Pointer($3);
  rp := AtomicIncrement(p);
  if rp <> Pointer($4) then halt({$I %LINENUM%});
  if p <> Pointer($4) then halt({$I %LINENUM%});

  p := Pointer($3);
  rp := AtomicDecrement(p);
  if rp <> Pointer($2) then halt({$I %LINENUM%});
  if p <> Pointer($2) then halt({$I %LINENUM%});*)

  p := Pointer($65);
  rp := AtomicExchange(p, Pointer($97));
  if rp <> Pointer($65) then halt({$I %LINENUM%});
  if p <> Pointer($97) then halt({$I %LINENUM%});

  p := Pointer($66);
  rp := AtomicCmpExchange(p, Pointer($44), Pointer($77));
  if rp <> Pointer($66) then halt({$I %LINENUM%});
  if p <> Pointer($66) then halt({$I %LINENUM%});

  (*p := Pointer($13);
  rp := AtomicIncrement(p, Pointer($10));
  if rp <> Pointer($23) then halt({$I %LINENUM%});
  if p <> Pointer($23) then halt({$I %LINENUM%});

  p := Pointer($13);
  rp := AtomicDecrement(p, Pointer($10));
  if rp <> Pointer($3) then halt({$I %LINENUM%});
  if p <> Pointer($3) then halt({$I %LINENUM%});*)

  {* test subrange type *}

  MyDigits := 7;
  rMyDigits := AtomicIncrement(MyDigits);
  if rMyDigits <> 8 then halt({$I %LINENUM%});
  if MyDigits <> 8 then halt({$I %LINENUM%});

  MyDigits := 4;
  rMyDigits := AtomicDecrement(MyDigits);
  if rMyDigits <> 3 then halt({$I %LINENUM%});
  if MyDigits <> 3 then halt({$I %LINENUM%});

  MyDigits := 6;
  rMyDigits := AtomicExchange(MyDigits, 3);
  if rMyDigits <> 6 then halt({$I %LINENUM%});
  if MyDigits <> 3 then halt({$I %LINENUM%});

  MyDigits := 4;
  rMyDigits := AtomicIncrement(MyDigits, 4);
  if rMyDigits <> 8 then halt({$I %LINENUM%});
  if MyDigits <> 8 then halt({$I %LINENUM%});

  MyDigits := 8;
  rMyDigits := AtomicDecrement(MyDigits, 4);
  if rMyDigits <> 4 then halt({$I %LINENUM%});
  if MyDigits <> 4 then halt({$I %LINENUM%});

  MyDigits := 8;
  rMyDigits := AtomicCmpExchange(MyDigits, 9, 8);
  if rMyDigits <> 8 then halt({$I %LINENUM%});
  if MyDigits <> 9 then halt({$I %LINENUM%});

  {* bounds of the sub range are not respected }
  {$R-,O-}
  MyDigits := 9;
  rMyDigits := AtomicIncrement(MyDigits, 1);
  if LongInt(rMyDigits) <> 10 then Halt({$I %LINENUM%});
  if LongInt(MyDigits) <> 10 then Halt({$I %LINENUM%});

  {* a few tests of Succeeded parameter for AtomicCmpExchange *}
  bool := True;
  u8 := 66;
  ru8 := AtomicCmpExchange(u8, 44, 77, bool);
  if ru8 <> 66 then halt({$I %LINENUM%});
  if u8 <> 66 then halt({$I %LINENUM%});
  if bool then halt({$I %LINENUM%});

  bool := False;
  i32 := 66;
  ri32 := AtomicCmpExchange(i32, 66, 66, bool);
  if ri32 <> 66 then halt({$I %LINENUM%});
  if i32 <> 66 then halt({$I %LINENUM%});
  if not bool then halt({$I %LINENUM%});

  bool := False;
  NatUInt := 66;
  rNatUInt := AtomicCmpExchange(NatUInt, 44, 66, bool);
  if rNatUInt <> 66 then halt({$I %LINENUM%});
  if NatUInt <> 44 then halt({$I %LINENUM%});
  if not bool then halt({$I %LINENUM%});

  bool := False;
  p := Pointer($66);
  rp := AtomicCmpExchange(p, Pointer($44), Pointer($66), bool);
  if rp <> Pointer($66) then halt({$I %LINENUM%});
  if p <> Pointer($44) then halt({$I %LINENUM%});
  if not bool then halt({$I %LINENUM%});

  {* tests regarding range checks *}

  u8 := high(u8);
  ru8 := AtomicIncrement(u8);
  if ru8 <> low(u8) then halt({$I %LINENUM%});
  if u8 <> low(u8) then halt({$I %LINENUM%});

  u8 := low(u8);
  ru8 := AtomicDecrement(u8);
  if ru8 <> high(u8) then halt({$I %LINENUM%});
  if u8 <> high(u8) then halt({$I %LINENUM%});

  i8 := high(i8);
  ri8 := AtomicIncrement(i8);
  if ri8 <> low(i8) then halt({$I %LINENUM%});
  if i8 <> low(i8) then halt({$I %LINENUM%});

  i8 := low(i8);
  ri8 := AtomicDecrement(i8);
  if ri8 <> high(i8) then halt({$I %LINENUM%});
  if i8 <> high(i8) then halt({$I %LINENUM%});

  u16 := high(u16);
  ru16 := AtomicIncrement(u16);
  if ru16 <> low(u16) then halt({$I %LINENUM%});
  if u16 <> low(u16) then halt({$I %LINENUM%});

  u16 := low(u16);
  ru16 := AtomicDecrement(u16);
  if ru16 <> high(u16) then halt({$I %LINENUM%});
  if u16 <> high(u16) then halt({$I %LINENUM%});

  i16 := high(i16);
  ri16 := AtomicIncrement(i16);
  if ri16 <> low(i16) then halt({$I %LINENUM%});
  if i16 <> low(i16) then halt({$I %LINENUM%});

  i16 := low(i16);
  ri16 := AtomicDecrement(i16);
  if ri16 <> high(i16) then halt({$I %LINENUM%});
  if i16 <> high(i16) then halt({$I %LINENUM%});

  u32 := high(u32);
  ru32 := AtomicIncrement(u32);
  if ru32 <> low(u32) then halt({$I %LINENUM%});
  if u32 <> low(u32) then halt({$I %LINENUM%});

  u32 := low(u32);
  ru32 := AtomicDecrement(u32);
  if ru32 <> high(u32) then halt({$I %LINENUM%});
  if u32 <> high(u32) then halt({$I %LINENUM%});

  i32 := high(i32);
  ri32 := AtomicIncrement(i32);
  if ri32 <> low(i32) then halt({$I %LINENUM%});
  if i32 <> low(i32) then halt({$I %LINENUM%});

  i32 := low(i32);
  ri32 := AtomicDecrement(i32);
  if ri32 <> high(i32) then halt({$I %LINENUM%});
  if i32 <> high(i32) then halt({$I %LINENUM%});

  u64 := high(u64);
  ru64 := AtomicIncrement(u64);
  if ru64 <> low(u64) then halt({$I %LINENUM%});
  if u64 <> low(u64) then halt({$I %LINENUM%});

  u64 := low(u64);
  ru64 := AtomicDecrement(u64);
  if ru64 <> high(u64) then halt({$I %LINENUM%});
  if u64 <> high(u64) then halt({$I %LINENUM%});

  i64 := high(i64);
  ri64 := AtomicIncrement(i64);
  if ri64 <> low(i64) then halt({$I %LINENUM%});
  if i64 <> low(i64) then halt({$I %LINENUM%});

  i64 := low(i64);
  ri64 := AtomicDecrement(i64);
  if ri64 <> high(i64) then halt({$I %LINENUM%});
  if i64 <> high(i64) then halt({$I %LINENUM%});

  (* check whether intrinsics also work without using the result *)

  i8 := 42;
  AtomicDecrement(i8);
  if i8 <> 41 then halt({$I %LINENUM%});

  i16 := 42;
  AtomicIncrement(i16);
  if i16 <> 43 then halt({$I %LINENUM%});

  u8 := 42;
  AtomicIncrement(u8, 10);
  if u8 <> 52 then halt({$I %LINENUM%});

  u16 := 42;
  AtomicDecrement(u16, 10);
  if u16 <> 32 then halt({$I %LINENUM%});

  i32 := 21;
  AtomicExchange(i32, 12);
  if i32 <> 12 then halt({$I %LINENUM%});

  u32 := 21;
  AtomicCmpExchange(u32, 12, 21);
  if u32 <> 12 then halt({$I %LINENUM%});

  u32 := 21;
  AtomicCmpExchange(u32, 12, 12);
  if u32 <> 21 then halt({$I %LINENUM%});

  u32 := 21;
  AtomicCmpExchange(u32, 12, 21, bool);
  if u32 <> 12 then halt({$I %LINENUM%});
  if not bool then halt({$I %LINENUM%});

  u32 := 21;
  AtomicCmpExchange(u32, 12, 12, bool);
  if u32 <> 21 then halt({$I %LINENUM%});
  if bool then halt({$I %LINENUM%});

  {* pointer types don't need to match exactly *}

  pl := PLongInt($1234);
  p := Pointer($4321);
  AtomicExchange(pl, p);
  if pl <> PLongInt($4321) then halt({$I %LINENUM%});

  p := Pointer($4321);
  pl := PLongInt($1234);
  AtomicExchange(p, pl);
  if p <> Pointer($1234) then halt({$I %LINENUM%});
end.
