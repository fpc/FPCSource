{ Val into integer types from AnsiString and UnicodeString checks the range of the destination type }
program tw41981;

{$mode objfpc}  //same for mode delphi
{$h+}


{$R-}

uses
  sysutils;


var
  ErrCount: Integer;

{$define test_16bit}

procedure TestValSigned(DestSize: Integer; const S: String; Expected: Boolean; ExpValue: Int64);
var
  I64: Int64;
  I32: Longint;
  I16: SmallInt;
  I8: ShortInt;
  B: Boolean;
  Err: ValSInt; //Word;
const
  BoolStr: array[Boolean] of String = ('FALSE','TRUE');
begin
  write(format('Test: (signed) ''%s'': ',[S]));
  try
    case DestSize of
      1: begin Val(S, I8, Err); I64:=I8; end;
      2: begin Val(S, I16, Err); I64:=I16; end;
      4: begin Val(S, I32, Err); I64:=I32; end;
      8: begin Val(S, I64, Err); end;
      else raise exception.createfmt('Invalid value for DestSize: %d',[DestSize]);
    end;
  except
    on E: ERangeError do
    begin
      writeln(format('FAIL S="%s": !! RangeCheckError !!',[s]));
      Inc(ErrCount);
      Exit;
    end;
  end;

  B := (Err = 0);
  if (B <> Expected) then
  begin
    Inc(ErrCount);
    write(format('FAIL: S="%s", Got: %s, Expected: %s, Err: %d',[S,BoolStr[B],BoolStr[Expected],Err]));
    if B then
      writeln(format(', Converted to: %d',[I64]))
    else
      writeln(format(' ,ExpValue was: %d',[ExpValue]));
    exit;
  end;
  if B and (I64 <> ExpValue) then
  begin
    Inc(ErrCount);
    writeln(format('FAIL: S="%s", Got value: %d, ExpValue: %d.',[S,I64,ExpValue]));
    exit;
  end;
  if Expected then
    writeln('OK')
  else
    writeln('OK: Conversion failed as expected.');
end;


procedure TestValUnSigned(DestSize: Integer; const S: String; Expected: Boolean; ExpValue: QWord);
var
  U64: UInt64;
  U32: DWord;
  U16: Word;
  U8: Byte;
  B: Boolean;
  Err: ValSInt; //Word;
const
  BoolStr: array[Boolean] of String = ('FALSE','TRUE');
begin
  write(format('Test (unsigned): ''%s'': ',[S]));
  try
  case DestSize of
    1: begin Val(S, U8, Err); U64:=U8; end;
    2: begin Val(S, U16, Err); U64:=U16; end;
    4: begin Val(S, U32, Err); U64:=U32; end;
    8: begin Val(S, U64, Err); end;
    else raise exception.createfmt('Invalid value for DestSize: %d',[DestSize]);
  end;
  except
    on E: ERangeError do
    begin
      writeln(format('FAIL S="%s": !! RangeCheckError !!',[s]));
      Inc(ErrCount);
      Exit;
    end;
  end;

  B := (Err = 0);
  if (B <> Expected) then
  begin
    Inc(ErrCount);
    write(format('FAIL: S="%s", Got: %s, Expected: %s, Err: %d',[S,BoolStr[B],BoolStr[Expected],Err]));
    if B then
      writeln(format(', Converted to: %d',[U64]))
    else
      writeln(format(' ,ExpValue was: %d',[ExpValue]));
    exit;
  end;
  if B and (U64 <> ExpValue) then
  begin
    Inc(ErrCount);
    writeln(format('FAIL: S="%s", Got value: %d, ExpValue: %d.',[S,U64,ExpValue]));
    exit;
  end;
  if Expected then
    writeln('OK')
  else
    writeln('OK: Conversion failed as expected.');
end;


procedure TestInt8;
var
  i,k: ShortInt;
  code: ValSInt;
  u: Byte;
begin
  writeln('Test 8-bit signed integers.');
  writeln('Low(ShortInt) =',Low(ShortInt));
  writeln('High(ShortInt)=',High(ShortInt));

  for i := Low(ShortInt) to High(ShortInt) do
  begin
    Val(i.tostring,k,code);
    if (code<>0) or (i<>k) then
    begin
      writeln('FAIL: ',i,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;



  for u := Low(Byte) to High(Byte) do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>Byte(k)) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;


  TestValSigned(1, '127', TRUE, 127);
  TestValSigned(1, '-128', TRUE, -128);
  TestValSigned(1, '128', FALSE, 0);
  TestValSigned(1, '-129', FALSE, 0);
  TestValSigned(1, '$FF', TRUE, -1);
  TestValSigned(1, '-$FF', FALSE, 0);
end;

procedure TestInt16;
var
  i,k: SmallInt;
  code: ValSInt;
  u: Word;
begin
  writeln('Test 16-bit signed integers.');
  writeln('Low(SmallInt) =',Low(SmallInt));
  writeln('High(SmallInt)=',High(SmallInt));
  for i := Low(smallint) to High(smallint) do
  begin
    Val(i.tostring,k,code);
    if (code<>0) or (i<>k) then
    begin
      writeln('FAIL: ',i,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  for u := Low(word) to High(word) do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>word(k)) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
      //halt;
    end;
  end;

  TestValSigned(2, '32767', TRUE, 32767);
  TestValSigned(2, '-32768', TRUE, -32768);
  TestValSigned(2, '32768', FALSE, 0);
  TestValSigned(2, '-32769', FALSE, 0);
  TestValSigned(2, '$FFFF', TRUE, -1);
  TestValSigned(2, '$10000', FALSE, 0);
  TestValSigned(2, '-$FFFF', FALSE, 0);
end;

procedure TestInt32;
var
  i,k, u: Integer;
  code: ValSInt;
begin
  writeln('Test 32-bit signed integers.');
  writeln('Low(LongInt) =',Low(LongInt));
  writeln('High(LongInt)=',High(LongInt));

  for i := -65536 to 65535 do
  begin
    Val(i.tostring,k,code);
    if (code<>0) or (i<>k) then
    begin
      writeln('FAIL: ',i,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  for u := 0 to $FFFF do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>word(k)) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;


  TestValSigned(4,'2147483647',TRUE, 2147483647);   //High(Longint)
  TestValSigned(4,'2147483648', FALSE, 0);          //High(Longint) + 1
  TestValSigned(4,'-2147483648', TRUE, -2147483648);
  TestValSigned(4,'-2147483649', FALSE, 0);
  TestValSigned(4,'$FFFFFFFF', TRUE, -1);          //High(DWord)
  TestValSigned(4,'$100000000', False, 0);          //High(DWord) +1
  TestValSigned(4,'$80000000', TRUE, -2147483648);
  TestValSigned(4,'$CED4DCE3', TRUE, -824910621);
  TestValSigned(4,'4294967295', FALSE, 0);
  TestValSigned(4,'7795000000', FALSE, 0);
  TestValSigned(4,'%11111111111111111111111111111111', TRUE, -1);  //High(Dword)
  TestValSigned(4,'%100000000000000000000000000000000', FALSE, 0); //Hig(Dword) + 1
  TestValSigned(4,'&37777777777', TRUE, -1);                       //High(Dword)
  TestValSigned(4,'&40000000000', FALSE, 0);                       //Hig(Dword) + 1
  TestValSigned(4,'-$123', TRUE, -291);
  TestValSigned(4,'+$123', TRUE, 291);
  TestValSigned(4,'xFFFFFFFF', TRUE, -1);
  TestValSigned(4,'0xFFFFFFFF', TRUE, -1);
  TestValSigned(4, '$FFED290A', True, -1234678);
end;

procedure TestInt64;
var
  H: Int64 = High(Int64); //cannot use High(Int64) as constant in TestValSigned(), compiler refuses it: valtest.pas(130,42) Error: range check error while evaluating constants (9223372036854775807 must be between -2147483648 and 2147483647)
  L: Int64 = Low(Int64);
  i, u: Integer;
  k: Int64;
  code: ValSInt;
begin
  writeln('Test 64-bit signed integers.');
  writeln('Low(Int64) =',Low(Int64));
  writeln('High(Int64)=',High(Int64));


  for i := -65536 to 65535 do
  begin
    Val(i.tostring,k,code);
    if (code<>0) or (i<>k) then
    begin
      writeln('FAIL: ',i,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  for u := 0 to $FFFF do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>word(k)) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  {$PUSH}{$R-}
  TestValSigned(8, '9223372036854775807', TRUE, H);
  TestValSigned(8, '-9223372036854775808', TRUE, L);
  TestValSigned(8, '9223372036854775808', FALSE, 0);
  TestValSigned(8, '-9223372036854775809', FALSE, 0);
  TestValSigned(8, '$FFFFFFFFFFFFFFFF', TRUE, -1);
  TestValSigned(8, '-$FFFFFFFFFFFFFFFF', FALSE, 0);
  TestValSigned(8, '2147483647', TRUE, 2147483647);
  TestValSigned(8, '2147483648', TRUE, 2147483648);
  TestValSigned(8, '-2147483648', TRUE, -2147483648);
  TestValSigned(8, '-2147483649', TRUE, -2147483649);

  TestValSigned(8, '1234567890', TRUE, 1234567890);
  TestValSigned(8, '-1234567890', TRUE, -1234567890);

  {$POP}
end;

procedure TestUInt8;
var
  u,k: Byte;
  Code: ValSInt;
begin
  writeln('Test 8-bit unsigned integers');
  writeln('High(Byte)=',High(Byte),' [',High(Byte).ToHexString,'], [&',OctStr(High(Byte),3),']');
  for u := 0 to $FF do
  begin
    Val(u.ToString, k, Code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: ',u.ToString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;
  for u := 0 to $FF do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  TestValUnSigned(1, '255', TRUE, High(Byte));
  TestValUnSigned(1, '256', FALSE, 0);
  TestValUnSigned(1, '$FF', TRUE, $FF);
  TestValUnSigned(1, '$100', FALSE, 0);
  TestValUnSigned(1, '&377', TRUE, High(Byte));
  TestValUnSigned(1, '&400', FALSE, 0);
  TestValUnsigned(1, '%11111111', TRUE, High(Byte));
  TestValUnsigned(1, '%100000000', FALSE, 0);
  TestValUnsigned(1, '-1', FALSE, 0);
end;

procedure TestUInt16;
var
  u,k: word;
  Code: ValSInt;
begin
  writeln('Test 16-bit unsigned integers');
  writeln('High(Word)=',High(Word),' [',High(Word).ToHexString,'], [&',OctStr(High(Word),6),']');
  for u := 0 to $FFFF do
  begin
    Val(u.ToString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: ',u.ToString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;
  for u := 0 to $FFFF do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
      halt;
    end;
  end;
  TestValUnSigned(2, '65535', TRUE, High(Word));
  TestValUnSigned(2, '65536', FALSE, 0);
  TestValUnSigned(2, '$FFFF', TRUE, $FFFF);
  TestValUnSigned(2, '$10000', FALSE, 0);
  TestValUnSigned(2, '&177777', TRUE, High(Word));
  TestValUnSigned(2, '&200000', FALSE, 0);
  TestValUnsigned(2, '%1111111111111111', TRUE, High(Word));
  TestValUnsigned(2, '%10000000000000000', FALSE, 0);
  TestValUnsigned(2, '-1', FALSE, 0);
end;


procedure TestUInt32;
var
  u,k: DWord;
  Code: ValSInt;
begin
  writeln('Test 32-bit unsigned integers');
  writeln('High(DWord)=',High(DWord),' [',High(DWord).ToHexString,'], [&',OctStr(High(DWord),11),']');
  for u := 0 to $3FFFC do
  begin
    Val(u.ToString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: ',u.ToString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;
  for u := 0 to $3FFFC do
  begin
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;
  TestValUnSigned(4, '4294967295', TRUE, 4294967295);
  TestValUnSigned(4, '4294967296', FALSE, 0);
  TestValUnSigned(4, '$FFFFFFFF', TRUE, $FFFFFFFF);
  TestValUnSigned(4, '$100000000', FALSE, 0);
  TestValUnSigned(4, '&37777777777', TRUE, High(DWord));
  TestValUnSigned(4, '&40000000000', FALSE, 0);
  TestValUnsigned(4, '%11111111111111111111111111111111', TRUE, High(DWord));
  TestValUnsigned(4, '%100000000000000000000000000000000', FALSE, 0);
  TestValUnsigned(4, '-1', FALSE, 0);
end;

procedure TestUInt64;
var
  i: DWord;
  u,k: QWord;
  Code: ValSInt;
begin
  writeln('Test 64-bit unsigned integers');
  writeln('High(QWord)=',High(QWord),' [',High(QWord).ToHexString,'], [&',OctStr(High(QWord),22),']');

  for i := 0 to $3FFFC do  //can't have a 64-bit loop var in 32-bit
  begin
    u := i;
    Val(u.ToString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: ',u.ToString,' Code=',code,', k=',k);
      inc(errcount);
      halt;
    end;
  end;
  for i := 0 to $3FFFC do  //can't have a 64-bit loop var in 32-bit
  begin
    u := i;
    Val('$'+u.ToHexString,k,code);
    if (code<>0) or (u<>k) then
    begin
      writeln('FAIL: $',u.ToHexString,' Code=',code,', k=',k);
      inc(errcount);
    end;
  end;

  TestValUnSigned(8, '18446744073709551615', TRUE, High(QWord));
  TestValUnSigned(8, '18446744073709551616', FALSE, 0);
  TestValUnSigned(8, '$FFFFFFFFFFFFFFFF', TRUE, High(QWord));
  TestValUnSigned(8, '$10000000000000000', FALSE, 0);
  TestValUnSigned(8, '&1777777777777777777777', TRUE, High(QWord));
  TestValUnSigned(8, '&2000000000000000000000', FALSE, 0);
  TestValUnSigned(8, '%1111111111111111111111111111111111111111111111111111111111111111', TRUE, High(QWord));
  TestValUnSigned(8, '%10000000000000000000000000000000000000000000000000000000000000000', FALSE, High(QWord));
  TestValUnsigned(8, '-1', FALSE, 0);
end;


procedure TestValSignedTooLarge;
  procedure FailWithValue(InputStr: String; OutputValue: Int64);
  begin
    Inc(ErrCount);
    writeln(format('FAIL: %s -> %d [???]',[InputStr,OutputValue]));
  end;
  procedure RangeError(InputStr: String);
  begin
    Inc(ErrCount);
    writeln('FAIL: ',InputStr,': RangeCheckError !!');
  end;

var
  I8: Int8;
  I16: Int16;
  I32: Int32;
  I64: Int64;
  Err: ValSInt;
begin
  {$R+}
  writeln('TestValSignedTooLarge: RangeCheck=ON');
  try
    Val('128',I8,Err);
    if Err<>0 then writeln('OK: 128: Err=',Err) else FailWithValue('128',I8);
  except
    on E: ERangeError do RangeError('128');
  end;
  try
    Val('32768',I16,Err);
    if Err<>0 then writeln('OK: 32768: Err=',Err) else FailWithValue('32768' ,I16);
  except
    on E: ERangeError do RangeError('32768');
  end;
  try
    Val('2147483648',I32,Err);
    if Err<>0 then writeln('OK: 2147483648: Err=',Err) else FailWithValue('2147483648',I32);
  except
    on E: ERangeError do RangeError('2147483648');
  end;
  try
    Val('9223372036854775808',I64,Err);
    if Err<>0 then writeln('OK: 9223372036854775808: Err=',Err) else FailWithValue('9223372036854775808',I64);
  except
    on E: ERangeError do RangeError('9223372036854775808');
  end;


  {$R-}
  writeln('TestValSignedTooLarge: RangeCheck=OFF');
  try
    Val('128',I8,Err);
    if Err<>0 then writeln('OK: 128: Err=',Err) else FailWithValue('128',I8);
  except
    on E: ERangeError do RangeError('128');
  end;
  try
    Val('32768',I16,Err);
    if Err<>0 then writeln('OK: 32768: Err=',Err) else FailWithValue('32768' ,I16);
  except
    on E: ERangeError do RangeError('32768');
  end;
  try
    Val('2147483648',I32,Err);
    if Err<>0 then writeln('OK: 2147483648: Err=',Err) else FailWithValue('2147483648',I32);
  except
    on E: ERangeError do RangeError('2147483648');
  end;
  try
    Val('9223372036854775808',I64,Err);
    if Err<>0 then writeln('OK: 9223372036854775808: Err=',Err) else FailWithValue('9223372036854775808',I64);
  except
    on E: ERangeError do RangeError('9223372036854775808');
  end;
end;

procedure TestValUnsignedTooLarge;
  procedure FailWithValue(InputStr: String; OutputValue: Int64);
  begin
    Inc(ErrCount);
    writeln(format('FAIL: %s -> %d [???]',[InputStr,OutputValue]));
  end;
  procedure RangeError(InputStr: String);
  begin
    Inc(ErrCount);
    writeln('FAIL: ',InputStr,': RangeCheckError !!');
  end;

var
  U8: UInt8;
  U16: UInt16;
  U32: UInt32;
  U64: UInt64;
  Err: ValSInt;
begin
  {$R+}
  writeln('TestValUnSignedTooLarge: RangeCheck=ON');
  try
    Val('256',U8,Err);
    if Err<>0 then writeln('OK: 256: Err=',Err) else FailWithValue('256',U8);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('65536',U16,Err);
    if Err<>0 then writeln('OK: 65536: Err=',Err) else FailWithValue('65536' ,U16);
  except
    on E: ERangeError do RangeError('65536');
  end;
  try
    Val('4294967296',U32,Err);
    if Err<>0 then writeln('OK: 4294967296: Err=',Err) else FailWithValue('4294967296',U32);
  except
    on E: ERangeError do RangeError('4294967296');
  end;
  try
    Val('18446744073709551616',U64,Err);
    if Err<>0 then writeln('OK: 18446744073709551616: Err=',Err) else FailWithValue('18446744073709551616',U64);
  except
    on E: ERangeError do RangeError('18446744073709551616');
  end;


  {$R-}
  writeln('TestValUnSignedTooLarge: RangeCheck=OFF');
  try
    Val('256',U8,Err);
    if Err<>0 then writeln('OK: 256: Err=',Err) else FailWithValue('256',U8);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('65536',U16,Err);
    if Err<>0 then writeln('OK: 65536: Err=',Err) else FailWithValue('65536' ,U16);
  except
    on E: ERangeError do RangeError('65536');
  end;
  try
    Val('4294967296',U32,Err);
    if Err<>0 then writeln('OK: 4294967296: Err=',Err) else FailWithValue('4294967296',U32);
  except
    on E: ERangeError do RangeError('4294967296');
  end;
  try
    Val('18446744073709551616',U64,Err);
    if Err<>0 then writeln('OK: 18446744073709551616: Err=',Err) else FailWithValue('18446744073709551616',U64);
  except
    on E: ERangeError do RangeError('18446744073709551616');
  end;
end;


procedure TestValUnsignedNegative;
  procedure FailWithValue(InputStr: String; OutputValue: Int64);
  begin
    Inc(ErrCount);
    writeln(format('FAIL: %s -> %d [???]',[InputStr,OutputValue]));
  end;
  procedure RangeError(InputStr: String);
  begin
    Inc(ErrCount);
    writeln('FAIL: ',InputStr,': RangeCheckError !!');
  end;

var
  U8: UInt8;
  U16: UInt16;
  U32: UInt32;
  U64: UInt64;
  Err: ValSInt;
begin
  {$R+}
  writeln('TestValUnSignedTooLarge: RangeCheck=ON');
  try
    Val('-1',U8,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U8);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U16,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U16);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U32,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U32);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U64,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U64);
  except
    on E: ERangeError do RangeError('256');
  end;

  {$R-}
  writeln('TestValUnSignedTooLarge: RangeCheck=OFF');
  try
    Val('-1',U8,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U8);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U16,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U16);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U32,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U32);
  except
    on E: ERangeError do RangeError('256');
  end;
  try
    Val('-1',U64,Err);
    if Err<>0 then writeln('OK: -1: Err=',Err) else FailWithValue('-1',U64);
  except
    on E: ERangeError do RangeError('256');
  end;
end;


// Checks out-of-range unsigned values from a UnicodeString with range checking on
procedure TestValUnicodeString;

var
  lStr: UnicodeString;
  lByte: Byte;
  lWord: Word;
  lCode: ValSInt;

begin
  {$R+}
  writeln('TestValUnicodeString');
  try
    lStr := '256';
    Val(lStr, lByte, lCode);
    if lCode = 0 then
    begin
      writeln('FAIL: Val(UnicodeString ''256'', Byte) must set Code, got ', lByte);
      Inc(ErrCount);
    end;
    lStr := '255';
    Val(lStr, lByte, lCode);
    if (lCode <> 0) or (lByte <> 255) then
    begin
      writeln('FAIL: Val(UnicodeString ''255'', Byte) must return 255, got ', lByte, ' Code=', lCode);
      Inc(ErrCount);
    end;
    lStr := '65536';
    Val(lStr, lWord, lCode);
    if lCode = 0 then
    begin
      writeln('FAIL: Val(UnicodeString ''65536'', Word) must set Code, got ', lWord);
      Inc(ErrCount);
    end;
  except
    on E: ERangeError do
    begin
      writeln('FAIL: Val(UnicodeString) must not raise ERangeError');
      Inc(ErrCount);
    end;
  end;
  {$R-}
end;



begin
  {$ifdef CPU64}
  writeln('64-bit test');
  writeln;
  {$endif}
  {$ifdef CPU32}
  writeln('32-bit test');
  writeln;
  {$endif}

  ErrCount := 0;

  TestInt8;
  writeln;
  TestInt16;
  writeln;
  TestInt32;
  writeln;
  TestInt64;
  writeln;

  TestUInt8;
  writeln;
  TestUInt16;
  writeln;
  TestUInt32;
  writeln;
  TestUInt64;
  writeln;

  TestValSignedTooLarge;
  writeln;
  TestValUnsignedTooLarge;
  writeln;
  TestValUnsignedNegative;
  writeln;
  TestValUnicodeString;

  writeln;
  writeln('Total Errors: ',ErrCount);
  if ErrCount > 0 then
    Halt(1);
end.



