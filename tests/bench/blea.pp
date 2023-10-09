{ %CPU=i386,x86_64 }
program blea;

{$IF not defined(CPUX86) and not defined(CPUX86_64)}
  {$FATAL This test program requires an Intel x86 or x64 processor }
{$ENDIF}

{$MODE OBJFPC}
{$ASMMODE Intel}

{$DEFINE DETECTCPU}

uses
  SysUtils;
  
type
  TBenchmarkProc = function(const Input, X, Y: LongWord): LongWord;

var
  CPUName: array[0..48] of Char;

{$ifdef DETECTCPU}
function FillBrandName: Boolean; assembler; nostackframe;
asm
{$ifdef CPUX86_64}
  PUSH RBX
{$else CPUX86_64}
  PUSH EBX
{$endif CPUX86_64}
  MOV  EAX, $80000000
  CPUID
  CMP  EAX, $80000004
  JB   @Unavailable
{$ifdef CPUX86_64}
  LEA  R8,  [RIP + CPUName]
{$endif CPUX86_64}
  MOV  EAX, $80000002
  CPUID
{$ifdef CPUX86_64}
  MOV  [R8], EAX
  MOV  [R8 + 4], EBX
  MOV  [R8 + 8], ECX
  MOV  [R8 + 12], EDX
{$else CPUX86_64}
  MOV  [CPUName], EAX
  MOV  [CPUName + 4], EBX
  MOV  [CPUName + 8], ECX
  MOV  [CPUName + 12], EDX
{$endif CPUX86_64}
  MOV  EAX, $80000003
  CPUID
{$ifdef CPUX86_64}
  MOV  [R8 + 16], EAX
  MOV  [R8 + 20], EBX
  MOV  [R8 + 24], ECX
  MOV  [R8 + 28], EDX
{$else CPUX86_64}
  MOV  [CPUName + 16], EAX
  MOV  [CPUName + 20], EBX
  MOV  [CPUName + 24], ECX
  MOV  [CPUName + 28], EDX
{$endif CPUX86_64}
  MOV  EAX, $80000004
  CPUID
{$ifdef CPUX86_64}
  MOV  [R8 + 32], EAX
  MOV  [R8 + 36], EBX
  MOV  [R8 + 40], ECX
  MOV  [R8 + 44], EDX
  MOV  BYTE PTR [R8 + 48], 0
{$else CPUX86_64}
  MOV  [CPUName + 32], EAX
  MOV  [CPUName + 36], EBX
  MOV  [CPUName + 40], ECX
  MOV  [CPUName + 44], EDX
  MOV  BYTE PTR [CPUName + 48], 0
{$endif CPUX86_64}
  MOV  AL,  1
  JMP  @ExitBrand
@Unavailable:
  XOR  AL,  AL
@ExitBrand:
{$ifdef CPUX86_64}
  POP  RBX
{$else CPUX86_64}
  POP  EBX
{$endif CPUX86_64}
end;
{$else DETECTCPU}
function FillBrandName: Boolean; inline;
begin
  Result := False;	
end;
{$endif DETECTPU}

function Checksum_PAS(const Input, X, Y: LongWord): LongWord;
var
  Counter: LongWord;
begin
  Result := Input;
  Counter := Y;
  while (Counter > 0) do
    begin
      Result := Result + X + $87654321;
      Result := Result xor Counter;
      Dec(Counter);
    end;
end;

function Checksum_ADD(const Input, X, Y: LongWord): LongWord; assembler; nostackframe;
asm
@Loop1:
  ADD Input, $87654321
  ADD Input, X
  XOR Input, Y
  DEC Y
  JNZ @Loop1
  MOV Result, Input
end;

function Checksum_LEA(const Input, X, Y: LongWord): LongWord; assembler; nostackframe;
asm
@Loop2:
  LEA Input, [Input + X - 2023406815] {+$87654321 in decimal}
  XOR Input, Y
  DEC Y
  JNZ @Loop2
  MOV Result, Input
end;

function Benchmark(const name: string; proc: TBenchmarkProc; Z, X: LongWord): LongWord;
const
  internal_reps = 1000;
var
  start: TDateTime;
  time: double;
  reps: cardinal;
begin
  Result := Z;
  reps := 0;
  Write(name, ': ');
  start := Now;
  repeat
    inc(reps);
    Result := proc(Result, X, internal_reps);
  until (reps >= 100000);
  time := ((((Now - start) * SecsPerDay) * 1e9) / internal_reps) / reps;
  WriteLn(time:0:(2 * ord(time < 10)), ' ns/call');
end;

var
  Results: array[0..2] of LongWord;
  FailureCode, X: Integer;
begin
{$ifdef CPUX86_64}
  Write('64-bit');
{$else CPUX86_64}
  Write('32-bit');
{$endif CPUX86_64}
  if FillBrandName then
    begin
      WriteLn(' CPU = ', CpuName);
      X := 0;
      while CpuName[X] <> #0 do
        begin
          CpuName[X] := '-';
          Inc(X);
        end;
      WriteLn('-------------', CpuName);
    end;
  Results[0] := Benchmark('   Pascal control case', @Checksum_PAS, 5000000, 1000);
  Results[1] := Benchmark(' Using LEA instruction', @Checksum_LEA, 5000000, 1000);
  Results[2] := Benchmark('Using ADD instructions', @Checksum_ADD, 5000000, 1000);
  
  FailureCode := 0;

  if (Results[0] <> Results[1]) then
    begin
      WriteLn('ERROR: Checksum_LEA doesn''t match control case');
      FailureCode := FailureCode or 1;
    end;
  if (Results[0] <> Results[2]) then
    begin
      WriteLn('ERROR: Checksum_ADD doesn''t match control case');
      FailureCode := FailureCode or 2
    end;
    
  if FailureCode <> 0 then
    Halt(FailureCode);
end.
