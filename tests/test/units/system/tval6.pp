{$mode objfpc}  //same for mode delphi
{$h+}
{$R+}

var
  ErrCount: Integer;
  Err, Res: ValSInt;
  Err2, Res2: Int32;
  S: String;
  QW: QWord;
  I64: Int64;
  I32: Int32;


procedure Test(DestSize: Integer; const S: String; Expected: Boolean; ExpValue: Int64);
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
  write('Test: ',S,': ');
  case DestSize of
    1: begin Val(S, I8, Err); I64:=I8; end;
    2: begin Val(S, I16, Err); I64:=I16; end;
    4: begin Val(S, I32, Err); I64:=I32; end;
    8: begin Val(S, I64, Err); end;
    otherwise 
      begin
        writeln('Invalid value for DestSize: ',DestSize);
        halt(1);
      end;
  end;


  B := (Err = 0);
  if (B <> Expected) then
  begin
    Inc(ErrCount);
    write('FAIL: S="',S,'", Got: ',B,', Expected: ',Expected,', Err: ',Err);
    if B then
      writeln(', Converted to: ',I32)
    else
      writeln(',ExpValue was: ',ExpValue);
    {$ifdef cpu64}
    //writeln;
    {$endif}
    exit;
  end;
  if B and (I64 <> ExpValue) then
  begin
    Inc(ErrCount);
    writeln('FAIL: S="',S,'", Got value: ',I64,', ExpValue: ',ExpValue,'.');
    exit;
  end;
  {$ifdef cpu64}
  //writeln;
  {$endif}
  if Expected then
    writeln('OK')
  else
    writeln('Conversion failed as expected.');
end;


procedure TestInt8;
begin
  writeln('Test 8-bit signed integers.');
  writeln('Low(ShortInt) =',Low(ShortInt));
  writeln('High(ShortInt)=',High(ShortInt));
  Test(1, '127', TRUE, 127);
  Test(1, '-128', TRUE, -128);
  Test(1, '128', FALSE, 0);
  Test(1, '-129', FALSE, 0);
  Test(1, '$FF', TRUE, -1);
  Test(1, '-$FF', FALSE, 0);
end;

procedure TestInt16;
begin
  writeln('Test 16-bit signed integers.');
  writeln('Low(SmallInt) =',Low(SmallInt));
  writeln('High(SmallInt)=',High(SmallInt));
  //debug:=false;
  Test(2, '32767', TRUE, 32767);
  Test(2, '-32768', TRUE, -32768);
  Test(2, '32768', FALSE, 0);
  Test(2, '-32769', FALSE, 0);
  Test(2, '$FFFF', TRUE, -1);
  Test(2, '$10000', FALSE, 0);
  Test(2, '-$FFFF', FALSE, 0);
end;

procedure TestInt32;
begin
  writeln('Test 32-bit signed integers.');
  writeln('Low(LongInt) =',Low(LongInt));
  writeln('High(LongInt)=',High(LongInt));
  Test(4,'2147483647',TRUE, 2147483647);   //High(Longint)
  Test(4,'2147483648', FALSE, 0);          //High(Longint) + 1
  Test(4,'-2147483648', TRUE, -2147483648);
  Test(4,'-2147483649', FALSE, 0);
  Test(4,'$FFFFFFFF', TRUE, -1);          //High(DWord)
  Test(4,'$100000000', False, 0);          //High(DWord) +1
  Test(4,'$80000000', TRUE, -2147483648);
  Test(4,'$CED4DCE3', TRUE, -824910621);
  Test(4,'4294967295', FALSE, 0);
  Test(4,'7795000000', FALSE, 0);
  Test(4,'%11111111111111111111111111111111', TRUE, -1);  //High(Dword)
  Test(4,'%100000000000000000000000000000000', FALSE, 0); //Hig(Dword) + 1
  Test(4,'&37777777777', TRUE, -1);                       //High(Dword)
  Test(4,'&40000000000', FALSE, 0);                       //Hig(Dword) + 1
  Test(4,'-$123', TRUE, -291);
  Test(4,'+$123', TRUE, 291);
  Test(4,'xFFFFFFFF', TRUE, -1);
  Test(4,'0xFFFFFFFF', TRUE, -1);
  Test(4, '$FFED290A', True, -1234678);
end;

procedure TestInt64;
var
  H: Int64 = High(Int64); //cannot use High(Int64) as constant in Test(), compiler refuses it: valtest.pas(130,42) Error: range check error while evaluating constants (9223372036854775807 must be between -2147483648 and 2147483647)
  L: Int64 = Low(Int64);
begin
  writeln('Test 64-bit signed integers.');
  writeln('Low(Int64) =',Low(Int64));
  writeln('High(Int64)=',High(Int64));
  //{$ifdef CPU64}
  {$PUSH}{$R-}
  Test(8, '9223372036854775807', TRUE, H);
  Test(8, '-9223372036854775808', TRUE, L);
  Test(8, '9223372036854775808', FALSE, 0);
  Test(8, '-9223372036854775809', FALSE, 0);
  Test(8, '$FFFFFFFFFFFFFFFF', TRUE, -1);
  Test(8, '-$FFFFFFFFFFFFFFFF', FALSE, 0);
  Test(8, '2147483647', TRUE, 2147483647);
  Test(8, '2147483648', TRUE, 2147483648);
  Test(8, '-2147483648', TRUE, -2147483648);
  Test(8, '-2147483649', TRUE, -2147483649);

  //Test(8, '1234567890', TRUE, 1234567890);
  //Test(8, '-1234567890', TRUE, -1234567890);

  {$POP}
  //{$endif CPU64}
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
  {$ifdef CPU16}
  writeln('16-bit test');
  writeln;
  {$endif}
  {$ifdef CPU8}
  writeln('8-bit test');
  writeln;
  {$endif}

  //writeln('High(shortint): ',high(shortint), ', Low(shortint): ',low(shortint));
  //writeln('High(smallint): ',high(smallint), ', Low(smallint): ',low(smallint));
  //writeln('High(longint): ',high(longint), ', Low(longint): ',low(longint));
  //writeln('High(int64): ',high(int64), ', Low(int64): ',low(int64));
  //{$PUSH}{$R-}
  //QW := QWord(1) shl (1*8-1) - 1;
  //U64 := -(Int64(1) shl (1*8-1));
  //writeln('1: ',QW, ' = ',QW.ToHexString, ', U64 = ',U64);
  //
  //QW := QWord(1) shl (2*8-1) - 1;
  //U64 := -(Int64(1) shl (2*8-1));
  //writeln('2: ',QW, ' = ',QW.ToHexString, ', U64 = ',U64);
  //
  //QW := QWord(1) shl (4*8-1) - 1;
  //U64 := -(Int64(1) shl (4*8-1));
  //writeln('4: ',QW, ' = ',QW.ToHexString, ', U64 = ',U64);
  //
  //QW := QWord(1) shl (8*8-1) - 1;
  //U64 := -(Int64(1) shl (8*8-1));
  //writeln('8: ',QW, ' = ',QW.ToHexString, ', U64 = ',U64);
  //{$POP}
  //
  //
  //EXIT;

  ErrCount := 0;

  TestInt8;
  writeln;
  TestInt16;
  writeln;
  TestInt32;
  writeln;
  TestInt64;
  writeln;
  writeln('Errors: ',ErrCount);
end.


{


}
