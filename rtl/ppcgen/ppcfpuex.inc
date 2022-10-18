{$ifndef FPUNONE}

const
  InvalidOperationMask = %10000000;
  OverflowMask         = %01000000;
  UnderflowMask        = %00100000;
  ZeroDivideMask       = %00010000;
  InexactMask          = %00001000;

{$ifndef aix}

const
  FP_RND_RZ = 1;
  FP_RND_RN = 0;
  FP_RND_RP = 2;
  FP_RND_RM = 3;
  FP_RND_SHIFT = 28;
  FP_RND_MASK = 3;

procedure fpc_setup_fpu;
var
  cw: TNativeFPUControlWord;
begin
  asm
    { clear all "exception happened" flags we care about}
    mtfsfi 0,0
    mtfsfi 1,0
    mtfsfi 2,0
    mtfsfi 3,0
    mtfsb0 21
    mtfsb0 22
    mtfsb0 23
  end;
  cw:=GetNativeFPUControlWord;
  cw:=(cw and not(OverflowMask or UnderflowMask or InexactMask or (FP_RND_MASK shl FP_RND_SHIFT))) or InvalidOperationMask or ZeroDivideMask or (FP_RND_RN shl FP_RND_SHIFT);
  SetNativeFPUControlWord(cw);
end;


function fpc_get_ppc_fpscr: TNativeFPUControlWord;
assembler;
var
  temp: record a,b:longint; end;
asm
  mffs f0
  stfd f0,temp
{$ifdef FPC_BIG_ENDIAN}
  lwz  r3,temp.b
{$else}
  lwz  r3,temp.a
{$endif}
end;

procedure fpc_set_ppc_fpsrc(cw: TNativeFPUControlWord);
var
  cwtemp: qword;
begin
  DefaultFPUControlWord:=cw;
  cwtemp:=cw;
  asm
    lfd f0, cwtemp
    mtfsf 255, f0
  end
end;
{$else aix}
const
  FP_RND_RZ = 0;
  FP_RND_RN = 1;
  FP_RND_RP = 2;
  FP_RND_RM = 3;

  FP_TRAP_SYNC         =  1;       { precise trapping on }
  FP_TRAP_OFF          =  0;       { trapping off        }
  FP_TRAP_QUERY        =  2;       { query trapping mode }
  FP_TRAP_IMP          =  3;       { non-recoverable imprecise trapping on }
  FP_TRAP_IMP_REC      =  4;       { recoverable imprecise trapping on }
  FP_TRAP_FASTMODE     =  128;     { select fastest available mode }
  FP_TRAP_ERROR        = -1;       { error condition }
  FP_TRAP_UNIMPL       = -2;       { requested mode not available }

function fp_is_enabled(Mask: DWord): boolean;cdecl;external;
procedure fp_enable(Mask: DWord);cdecl;external;
function feclearexcept(Mask: DWord):DWord;cdecl;external;
procedure fp_disable(Mask: DWord);cdecl;external;
function fp_read_rnd: word;cdecl;external;
function fp_swap_rnd(RoundMode: word): word;cdecl;external;
function fp_trap(flag: longint): longint;cdecl; external;

procedure fpc_setup_fpu;
var
  cw: TNativeFPUControlWord;
begin
  feclearexcept(InvalidOperationMask or OverflowMask or UnderflowMask or ZeroDivideMask or InexactMask);
  if fp_trap(FP_TRAP_SYNC)<0 then
    fp_trap(FP_TRAP_IMP_REC);
  cw:=GetNativeFPUControlWord;
  cw.rndmode:=FP_RND_RN;
  cw.exceptionmask:=InvalidOperationMask or ZeroDivideMask;
  SetNativeFPUControlWord(cw);
end;


function fpc_get_ppc_fpscr: TNativeFPUControlWord;
begin
  result.rndmode:=fp_read_rnd;
  result.exceptionmask:=0;
  if not fp_is_enabled(InvalidOperationMask) then
    result.exceptionmask:=result.exceptionmask or InvalidOperationMask;
  if not fp_is_enabled(OverflowMask) then
    result.exceptionmask:=result.exceptionmask or OverflowMask;
  if not fp_is_enabled(UnderflowMask) then
    result.exceptionmask:=result.exceptionmask or UnderflowMask;
  if not fp_is_enabled(InvalidOperationMask) then
    result.exceptionmask:=result.exceptionmask or ZeroDivideMask;
  if not fp_is_enabled(InexactMask) then
    result.exceptionmask:=result.exceptionmask or InexactMask;
end;


procedure fpc_set_ppc_fpsrc(cw: TNativeFPUControlWord);
var
  enablemask, disablemask: dword;
begin
  fp_swap_rnd(cw.rndmode);
  enablemask:=0;
  disablemask:=0;
  if (cw.exceptionmask and InvalidOperationMask)<>0 then
    disablemask:=disablemask or InvalidOperationMask
  else
    enablemask:=enablemask or InvalidOperationMask;
  if (cw.exceptionmask and OverflowMask)<>0 then
    disablemask:=disablemask or OverflowMask
  else
    enablemask:=enablemask or OverflowMask;
  if (cw.exceptionmask and UnderflowMask)<>0 then
    disablemask:=disablemask or UnderflowMask
  else
    enablemask:=enablemask or UnderflowMask;
  if (cw.exceptionmask and ZeroDivideMask)<>0 then
    disablemask:=disablemask or ZeroDivideMask
  else
    enablemask:=enablemask or ZeroDivideMask;
  if (cw.exceptionmask and InexactMask)<>0 then
    disablemask:=disablemask or InexactMask
  else
    enablemask:=enablemask or InexactMask;
  fp_enable(enablemask);
  fp_disable(disablemask);
  DefaultFPUControlWord:=cw;
end;
{$endif}


function GetNativeFPUControlWord: TNativeFPUControlWord; {$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
    result:=fpc_get_ppc_fpscr;
  end;


procedure SetNativeFPUControlWord(const cw: TNativeFPUControlWord); {$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
    fpc_set_ppc_fpsrc(cw);
  end;


{$define FPC_SYSTEM_HAS_SYSINITFPU}
procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { powerpc might use softfloat code }
  softfloat_exception_flags:=[];
  softfloat_exception_mask:=[float_flag_underflow, float_flag_overflow, float_flag_inexact, float_flag_denormal];
  fpc_setup_fpu;
end;
{$endif NOT FPU_NONE}
