{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2003 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$ASMMODE ATT}

 {this optimisations ported from Cris Kaspersky articles 
 	kpnc@itech.ru, kpnc@programme.ru
 }
 {$define FPC_MATH_HAS_MIN_BYTE}
 function Min(a, b: byte): byte;inline;assembler;nostackframe;
	 asm
		movb b, %al
		subb a ,%al
		sbbb %cl, %cl
		andb %cl, %al
		addb a, %al
	 end['AL','CL'];
  
 {$define FPC_MATH_HAS_MAX_BYTE}
 function Max(a, b: byte): byte;inline;assembler;nostackframe;
	 asm
		movb b, %al
		subb a ,%al
		sbbb %cl, %cl
		andb %al, %cl
		movb b ,%al
		subb %cl,%al
	 end['AL','CL'];

 {$define FPC_MATH_HAS_MIN_DWORD}
 function Min(a, b: LongWord): LongWord;inline;assembler;nostackframe;
	 asm
		movl b, %eax
		subl a ,%eax
		sbbl %ecx, %ecx
		andl %ecx, %eax
		addl a, %eax
	 end['EAX','ECX'];
  
 {$define FPC_MATH_HAS_MAX_DWORD}
 function Max(a, b: LongWord): LongWord;inline;assembler;nostackframe;
	 asm
		movl b, %eax
		subl a ,%eax
		sbbl %ecx, %ecx
		andl %eax, %ecx
		movl b ,%eax
		subl %ecx,%eax
	 end['EAX','ECX'];
 
 {$define FPC_MATH_HAS_MIN_QWORD}
 function Min(a, b: QWord): QWord;inline;assembler;nostackframe;
	 asm
		movl b, %eax
		movl b+4, %edx
		
		subl a ,%eax
		sbbl a+4 ,%edx
		
		sbbl %ecx, %ecx
		
		andl %ecx, %eax
		andl %ecx, %edx
		
		addl a, %eax
		adcl a+4, %edx
	 end['EAX','EBX','ECX','EDX'];
 
 {$define FPC_MATH_HAS_MAX_QWORD}
 function Max(a, b: QWord): QWord;inline;assembler;nostackframe;
	asm
		movl b, %eax
		movl b+4, %edx

		subl a ,%eax
		sbbl a+4 ,%edx

		sbbl %ebx, %ebx
		sbbl %ecx, %ecx

		andl %eax, %ebx
		andl %edx, %ecx

		subl a ,%eax
		sbbl a+4 ,%edx

		addl %ebx, %eax
		adcl %ecx, %edx
	end['EAX','EBX','ECX','EDX'];

{$define FPC_MATH_HAS_Min_DWORDS}
function MinValue(const Data : PLongWord; const N : cardinal): LongWord;assembler;
    var
        saveesi : LongWord;
    asm
        movl   %esi,saveesi
        movl Data,%esi
        movl N,%ecx
        lodsl
        movl    %eax,%edx
        decl 	%ecx
        jg 		.LFinalLineLoop
        jmp     .Lfinish
.LFinalLineLoop: 
        lodsl
        subl %eax ,%edx {eax - a, edx - result=b}
        sbbl %ebx, %ebx
        andl %ebx, %eax
        addl %eax, %edx
        decl %ecx
        jne .LFinalLineLoop
.Lfinish:
        movl %edx,%eax
        movl   saveesi, %esi
    end['EAX','EBX','ECX','EDX'];

{$define FPC_MATH_HAS_Max_DWORDS}
function MaxValue(const Data : PLongWord; const N : cardinal): LongWord;assembler;
    var
        saveesi : LongWord;
    asm
        movl   %esi,saveesi
        movl Data,%esi
        movl N,%ecx
        lodsl
        movl    %eax,%edx
        decl %ecx
        jg 		.LFinalLineLoop
        jmp     .Lfinish
.LFinalLineLoop: 

        lodsl
        pushl %edx
        subl %eax ,%edx {eax - a, edx - result=b}
        sbbl %ebx, %ebx
        andl %edx, %ebx
        popl %edx
        subl %ebx, %edx
        
        decl %ecx
        jne .LFinalLineLoop
.Lfinish:
        movl %edx,%eax
        movl   saveesi, %esi
    end['EAX','EBX','ECX','EDX'];

{$define FPC_MATH_HAS_Min_INTS}
function MinValue(const Data : PInteger; const N : cardinal): Integer;assembler;
var
    saveesi : LongWord;
    asm
        movl   %esi,saveesi
        movl    Data,%esi
        movl    N,%ecx
        lodsl
        decl %ecx
        jg 		.LFinalLineLoop
        jmp     .Lfinish
.LFinalLineLoop: 
        lodsl
        cmpl %eax ,%edx {eax - a, edx - result=b}
        jle  .LIAskip
        movl %eax ,%edx
.LIAskip:        
        decl %ecx
        jne .LFinalLineLoop
.Lfinish:
        movl  %edx,%eax
        movl   saveesi, %esi
    end['EAX','ECX','EDX'];

{$define FPC_MATH_HAS_Max_INTS}
function MaxValue(const Data : PInteger; const N : cardinal): Integer;assembler;
var
    saveesi : LongWord;
    asm
        movl   %esi,saveesi
        movl    Data,%esi
        movl    N,%ecx
        lodsl
        decl %ecx
        jg 		.LFinalLineLoop
        jmp     .Lfinish
.LFinalLineLoop: 
        lodsl
        cmpl %eax ,%edx {eax - a, edx - result=b}
        jge  .LIAskip
        movl %eax ,%edx
.LIAskip:        
        decl %ecx
        jne .LFinalLineLoop
.Lfinish:
        movl  %edx,%eax
        movl   saveesi, %esi
    end['EAX','ECX','EDX'];

{$define FPC_MATH_HAS_ARCTAN2}
function arctan2(y,x : float) : float;assembler;
  asm
     fldt y
     fldt x
     fpatan
     fwait
  end;


{$define FPC_MATH_HAS_SINCOS}
procedure sincos(theta : float;out sinus,cosinus : float);assembler;
  asm
    fldt theta
    fsincos
    fstpt (%edx)
    fstpt (%eax)
    fwait
  end;


{$define FPC_MATH_HAS_TAN}
function tan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fstp %st
    fwait
  end;


{$define FPC_MATH_HAS_COTAN}
function cotan(x : float) : float;assembler;
  asm
    fldt X
    fptan
    fdivrp
    fwait
  end;


function GetRoundMode: TFPURoundingMode;
begin
  Result := TFPURoundingMode((Get8087CW shr 10) and 3);
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW((CtlWord and $F3FF) or (Ord(RoundMode) shl 10));
  if has_sse_support then
    SetSSECSR((GetSSECSR and $ffff9fff) or (dword(RoundMode) shl 13));
  Result := TFPURoundingMode((CtlWord shr 10) and 3);
end;

function GetPrecisionMode: TFPUPrecisionMode;
begin
  Result := TFPUPrecisionMode((Get8087CW shr 8) and 3);
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW((CtlWord and $FCFF) or (Ord(Precision) shl 8));
  Result := TFPUPrecisionMode((CtlWord shr 8) and 3);
end;

function GetExceptionMask: TFPUExceptionMask;
begin
  Result := TFPUExceptionMask(Longint(Get8087CW and $3F));
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  CtlWord: Word;
begin
  CtlWord := Get8087CW;
  Set8087CW( (CtlWord and $FFC0) or Byte(Longint(Mask)) );
  if has_sse_support then
    SetSSECSR((GetSSECSR and $ffffe07f) or (dword(Mask) shl 7));
  softfloat_exception_mask:=dword(Mask);
  Result := TFPUExceptionMask(Longint(CtlWord and $3F));
end;

procedure ClearExceptions(RaisePending: Boolean);assembler;
asm
  cmpb $0,RaisePending
  je .Lclear
  fwait
.Lclear:
  fnclex
end;
