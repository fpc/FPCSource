{ %CPU=x86_64 }
{ %TARGET=linux }
program vectorcall_pd_test1;
 
{$IFNDEF CPUX86_64}
  {$FATAL This test program can only be compiled on Linux 64-bit with an Intel processor }
{$ENDIF}
{$IFNDEF LINUX}
  {$FATAL This test program can only be compiled on Linux 64-bit with an Intel processor }
{$ENDIF}
{$MODESWITCH ADVANCEDRECORDS}
{$ASMMODE Intel}
type
  { TM128 }
  {$push}
  {$CODEALIGN RECORDMIN=16}
  {$PACKRECORDS C}
  TM128 = record
    public
    function Add(A: TM128): TM128; vectorcall;
    case Byte of
      0: (M128_F32: array[0..3] of Single);
      1: (M128_F64: array[0..1] of Double);
  end;
  {$pop}
 
{ TM128 }
 
function TM128.Add(A: TM128): TM128; vectorcall; assembler; nostackframe;
asm
  addps  xmm0, [RDI]   // expected convention for Self - add result into first parameter and return as result (xmm0 = first parameter and return value)
end;
 
var
  xm1, xm2, xm3: TM128;
 
begin
  WriteLn('Linux "vectorcall" test with Self (is it in RDI?)'#10
        + '-------------------------------------------------');
  xm1.M128_F32[0] := 1.0;
  xm1.M128_F32[1] := 2.0;
  xm1.M128_F32[2] := 3.0;
  xm1.M128_F32[3] := 4.0;
  xm2.M128_F32[0] := 5.0;
  xm2.M128_F32[1] := 6.0;
  xm2.M128_F32[2] := 7.0;
  xm2.M128_F32[3] := 8.0;
  
  WriteLn('xm1 = (', xm1.M128_F32[0], ', ', xm1.M128_F32[1], ', ', xm1.M128_F32[2], ', ', xm1.M128_F32[3]);
  WriteLn('xm2 = (', xm2.M128_F32[0], ', ', xm2.M128_F32[1], ', ', xm2.M128_F32[2], ', ', xm2.M128_F32[3]);
  xm3 := xm1.Add(xm2); { This will likely raise an exception if RDI doesn't contain Self }
  WriteLn('xm3 = (', xm3.M128_F32[0], ', ', xm3.M128_F32[1], ', ', xm3.M128_F32[2], ', ', xm3.M128_F32[3]);
  
  if (xm3.M128_F32[0] <> 6.0) or (xm3.M128_F32[1] <> 8.0) or (xm3.M128_F32[2] <> 10.0) or (xm3.M128_F32[3] <> 12.0) then
    begin
      WriteLn('FAIL');
	  Halt(1);
	end;
  
  WriteLn('ok');
end.                             