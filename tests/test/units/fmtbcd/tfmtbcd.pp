// A basic tests for FmtBCD unit

{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

uses SysUtils, FmtBCD, Variants;

var
  ErrorCount: integer;
  FS, DFS: TFormatSettings;
  bcd: TBCD;

procedure testBCDMultiply(bcd1,bcd2,bcd3: TBCD);
var bcdmul: TBCD;
begin
  BCDMultiply(bcd1,bcd2,bcdmul);
  if (BCDCompare(bcd3,bcdmul) <> 0) or
     (bcdtostr(bcd3) <> bcdtostr(bcdmul)) then
  begin
    writeln(bcdtostr(bcd1), ' * ', bcdtostr(bcd2), ' = ', bcdtostr(bcdmul), ' but expected ', bcdtostr(bcd3));
    writeln('Expected: (', bcd3.Precision,',',bcd3.SignSpecialPlaces, ') but calculated: (', bcdmul.Precision,',',bcdmul.SignSpecialPlaces,')');
    inc(ErrorCount);
  end;
end;

procedure testBCDDivide(bcd1,bcd2,bcd3: TBCD);
var bcddiv: TBCD;
begin
  BCDDivide(bcd1,bcd2,bcddiv);
  if (BCDCompare(bcd3,bcddiv) <> 0) or
     (bcdtostr(bcd3) <> bcdtostr(bcddiv)) then
  begin
    writeln(bcdtostr(bcd1), ' / ', bcdtostr(bcd2), ' = ', bcdtostr(bcddiv), ' but expected ', bcdtostr(bcd3));
    //writeln('Expected: ', bcd3.Precision,',',bcd3.SignSpecialPlaces, ' but calculated: ', bcddiv.Precision,',',bcddiv.SignSpecialPlaces);
    inc(ErrorCount);
  end;
end;

procedure testBCDToStrF(const s1, s2: string);
begin
  if s1 <> s2 then
  begin
    writeln('BCDToStrF: ', s1, ' Expected: ', s2);
    inc(ErrorCount);
  end;
end;

procedure testBCDPrecScale(const s: string; const prec,scale: integer);
var bcd: TBCD;
begin
  bcd := strtobcd(s);
  if (bcd.Precision <> prec) or (BCDScale(bcd) <> scale) then
  begin
    writeln('StrToBcd: ', bcdtostr(bcd), ' (', s, ') Precision:', bcd.Precision, ' Scale: ', BCDScale(bcd));
    inc(ErrorCount);
  end;
end;

procedure testBCDCompare(bcd1,bcd2: TBCD; res: integer);
begin
  if (BCDCompare(bcd1,bcd2) <> res) then
  begin
    writeln('BCDCompare failed; bcd1:', bcdtostr(bcd1), ' bcd2:', bcdtostr(bcd2));
    inc(ErrorCount);
  end;
end;

procedure testVariantOp(v1, v2: variant);
var v: variant;
    i: integer;
    d: double;
    s1: shortstring;
    s2: ansistring;
    s3: unicodestring;
begin
  //arithmetic op. ... invalid variant operation ?
  v := v1 + v2;
  v := v * v2;
  v := v / v2;
  v := v - v2;
  if VarIsFmtBCD(v1) and not VarIsFmtBCD(v) then inc(ErrorCount);

  //compare op.
  if not(v1=v) or (v1<>v) then
  begin
    writeln('Original variant: ', vartostr(v1), 'recomputed variant: ', vartostr(v));
    inc(ErrorCount);
  end;
  v := v + 1;
  if (v1 >= v) or not(v1 < v) then
  begin
    writeln('Compare2 failed; v1: ', vartostr(v1), ' v: ', vartostr(v));
    inc(ErrorCount);
  end;
  v := v - 1.1;
  if (v1 <= v) or not(v1 > v) then
  begin
    writeln('Compare3 failed; v1: ', vartostr(v1), ' v: ', vartostr(v));
    inc(ErrorCount);
  end;

  //assign op. ... invalid variant typecast ?
  //i := v;
  d := v;
  //s1 := v;
  s2 := v;
  //s3 := v;
end;

begin
  ErrorCount := 0;

  // test BCDToStrF:
  DFS:=DefaultFormatSettings;

  FS.DecimalSeparator:=',';
  FS.ThousandSeparator:=' ';
  FS.CurrencyDecimals:=2;
  FS.CurrencyString:='$';
  FS.CurrencyFormat:=3;
  DefaultFormatSettings:=FS;
  bcd:=strtobcd('123456789123456789,12345');
  testBCDToStrF(bcdtostrf(bcd, ffFixed, 30, 4), '123456789123456789,1235'); //no thousand separators
  testBCDToStrF(bcdtostrf(bcd, ffNumber, 30, 5), '123 456 789 123 456 789,12345'); //with thousand separators
  testBCDToStrF(bcdtostrf(bcd, ffCurrency, 30, 2), '123 456 789 123 456 789,12 $'); //with thousand separators

  FS.DecimalSeparator:='.';
  FS.ThousandSeparator:=',';
  FS.CurrencyFormat:=0;
  DefaultFormatSettings:=FS;
  bcd:=strtobcd('123456789123456789.12345');
  testBCDToStrF(bcdtostr(bcd), '123456789123456789.12345');
  testBCDToStrF(bcdtostrf(bcd, ffFixed, 30, 3), '123456789123456789.123'); //no thousand separators
  testBCDToStrF(bcdtostrf(bcd, ffNumber, 30, 6), '123,456,789,123,456,789.123450'); //with thousand separators
  testBCDToStrF(bcdtostrf(bcd, ffCurrency, 30, 5), '$123,456,789,123,456,789.12345'); //with thousand separators

  // test StrToBCD:
  testBCDPrecScale(' 1.0000000000000000E-0003 ', 3, 3);
  testBCDPrecScale('0.001', 3, 3);
  testBCDPrecScale('1.001', 4, 3);
  testBCDPrecScale('1001', 4, 0);
  testBCDPrecScale('1001.1001', 8, 4);

  DefaultFormatSettings := DFS;

  // test BCDMultiply:
  FS.DecimalSeparator:='.';
  FS.ThousandSeparator:=#0;
  testBCDMultiply(1000, -1000, -1000000);
  testBCDMultiply(-1000, -0.001, 1);
  testBCDMultiply(1000, 0.0001, 0.1);
  testBCDMultiply(strtobcd('12345678901234567890',FS), strtobcd('0.0000000001',FS), strtobcd('1234567890.123456789',FS));

  // test BCDDivide:
  testBCDDivide(1000, 1000, 1);
  testBCDDivide(1000, -100, -10);
  testBCDDivide(-1000, 10, -100);
  testBCDDivide(-1000, -1, 1000);
  testBCDDivide(11000, 11, 1000);
  testBCDDivide(11, 11000, 0.001);

  testBCDDivide(100, -2, -50);
  testBCDDivide(1007, 5, 201.4);

  // test BCDCompare:
  testBCDCompare(100, 100, 0);
  testBCDCompare(-100.1, -100.1, 0);
  testBCDCompare(-100.1, 100.1, -1);
  testBCDCompare(-100.1, -100.2, 1);
  testBCDCompare(100, 100.1, -1);

  // test Variant support:
  testVariantOp(varFmtBcdCreate(100), varFmtBcdCreate(-100));
  testVariantOp(double(2.5), varFmtBcdCreate(100)); //double on left side
  testVariantOp(varFmtBcdCreate(100), integer(-10));
  testVariantOp(varFmtBcdCreate(-100), shortstring(floattostr(10.2)));
  testVariantOp(varFmtBcdCreate(-100), ansistring(floattostr(0.2)));
  testVariantOp(varFmtBcdCreate(-100), unicodestring(floattostr(-0.2)));


  if ErrorCount<>0 then
  begin
    writeln('FmtBCD test program found ', ErrorCount, ' errors!');
    Halt(ErrorCount);
  end;
end.
