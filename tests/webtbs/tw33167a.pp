{ float mod operator with Single, Double and Float operands, issue #33167 }
program tw33167a;

{$mode objfpc}

{$ifndef FPUNONE}
uses
  Math;

var
  lCode: Integer;

// Halt when the Single result aValue is not aExpected
procedure CheckS(aValue, aExpected: Single);

begin
  Inc(lCode);
  if not SameValue(aValue,aExpected) then
    begin
    writeln('check ',lCode,': ',aValue,' expected ',aExpected);
    halt(lCode);
    end;
end;


// Halt when the Double result aValue is not aExpected
procedure CheckD(aValue, aExpected: Double);

begin
  Inc(lCode);
  if not SameValue(aValue,aExpected) then
    begin
    writeln('check ',lCode,': ',aValue,' expected ',aExpected);
    halt(lCode);
    end;
end;


// Halt when the Float result aValue is not aExpected
procedure CheckF(aValue, aExpected: Float);

begin
  Inc(lCode);
  if not SameValue(aValue,aExpected) then
    begin
    writeln('check ',lCode,': ',aValue,' expected ',aExpected);
    halt(lCode);
    end;
end;


var
  lS77, lS11, lS72, lS03, lS01, lS1, lS3: Single;
  lD77, lD11, lD72, lD03, lD01, lD1, lD3: Double;
  lF77, lF11, lF72, lF03, lF01, lF1, lF3: Float;

begin
  lCode:=0;
  lS77:=7.7; lS11:=1.1; lS72:=7.2; lS03:=0.3; lS01:=0.1; lS1:=1; lS3:=3;
  lD77:=7.7; lD11:=1.1; lD72:=7.2; lD03:=0.3; lD01:=0.1; lD1:=1; lD3:=3;
  lF77:=7.7; lF11:=1.1; lF72:=7.2; lF03:=0.3; lF01:=0.1; lF1:=1; lF3:=3;

  { Single, codes 1..9 }
  CheckS(lS77 mod lS11,0);
  CheckS(-lS77 mod lS11,0);
  CheckS(lS77 mod -lS11,0);
  CheckS(-lS77 mod -lS11,0);
  CheckS(lS03 mod lS01,0);
  CheckS(lS72 mod lS11,0.6);
  CheckS(-lS72 mod lS11,-0.6);
  CheckS(lS72 mod -lS11,0.6);
  CheckS(lS1 mod lS3,1);

  { Double, codes 10..18 }
  CheckD(lD77 mod lD11,0);
  CheckD(-lD77 mod lD11,0);
  CheckD(lD77 mod -lD11,0);
  CheckD(-lD77 mod -lD11,0);
  CheckD(lD03 mod lD01,0);
  CheckD(lD72 mod lD11,0.6);
  CheckD(-lD72 mod lD11,-0.6);
  CheckD(lD72 mod -lD11,0.6);
  CheckD(lD1 mod lD3,1);

  { Float, codes 19..27 }
  CheckF(lF77 mod lF11,0);
  CheckF(-lF77 mod lF11,0);
  CheckF(lF77 mod -lF11,0);
  CheckF(-lF77 mod -lF11,0);
  CheckF(lF03 mod lF01,0);
  CheckF(lF72 mod lF11,0.6);
  CheckF(-lF72 mod lF11,-0.6);
  CheckF(lF72 mod -lF11,0.6);
  CheckF(lF1 mod lF3,1);

  { remainders close to the divisor are kept, codes 28..31 }
  lS1:=100001; lS3:=100002;
  CheckS(lS1 mod lS3,100001);
  lS1:=1-1/1048576; lS3:=1;
  CheckS(lS1 mod lS3,1-1/1048576);
  lD1:=7.7; lD3:=7.7000001;
  CheckD(lD1 mod lD3,7.7);
  lF1:=5000001; lF3:=5000002;
  CheckF(lF1 mod lF3,5000001);
  writeln('ok');
{$else FPUNONE}
begin
{$endif FPUNONE}
end.
