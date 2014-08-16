{ Tests unordered comparison results. This is a basic codegeneration test, but it needs
  Math unit to silence exceptions. }
uses math;
 
const
  kNan = Sqrt(-1);
  kX = 5.8E-7;
var
  vNan, vX: real;
  code: longint;
  b: boolean;
begin
  code:=0;
  SetExceptionMask( [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if kNan = kX  then code:=1;
  if kNan < kX  then code:=code or 2;
  if kNan <= kX then code:=code or 4;
  if kNan > kX  then code:=code or 8;
  if kNan >= kX then code:=code or 16;
  code:=code or 32;
  if kX <> kNan then code:=code and (not 32);
  
  vNan:= kNan;
  vX:= kX;
  
  { Test g_flag2reg/ref }
  b:=(vNan = vX);
  if b then code:=code or 64;
  b:=(vNan < vX);
  if b then code:=code or 128;
  b:=(vNan <= vX);
  if b then code:=code or 256;
  b:=(vNan > vX);
  if b then code:=code or 512;
  b:=(vNan >= vX);
  if b then code:=code or 1024;
  b:=(vNan <> vX);
  if (not b) then code:=code or 2048;
  
  { Test a_jmp_flags }
  if vNan = vX then
    code:=code or 4096;
  if vNan < vX then
    code:=code or 8192;
  if vNan <= vX then
    code:=code or 16384;
  if vNan > vX then
    code:=code or 32768;
  if vNan >= vX then
    code:=code or 65536;  
 
  code:=code or 131072;
  if vNan <> vX then
    code:=code and (not 131072);
    
  if code=0 then
    writeln('ok') 
  else
    writeln('error: ',hexstr(code,8));
  Halt(code);
end.
 