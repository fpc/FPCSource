{$IFDEF OGC_INTERFACE}
//#define __stringify(rn)								#rn // ??
//#define ATTRIBUTE_ALIGN(v)							__attribute__((aligned(v)))
//#define STACK_ALIGN(type, name, cnt, alignment)		u8 _al__##name[((sizeof(type)*(cnt)) + (alignment) + (((sizeof(type)*(cnt))%(alignment)) > 0 ? ((alignment) - ((sizeof(type)*(cnt))%(alignment))) : 0))]; \
//													type *name = (type*)(((u32)(_al__##name)) + ((alignment) - (((u32)(_al__##name))&((alignment)-1))))

procedure _sync(); inline; assembler;
procedure _nop(); inline; assembler;
procedure ppcsync(); inline; assembler;
procedure ppchalt(); inline;
//procedure mfpvr(); inline;
function mfpvr(): cint32; inline;

function mfdcr(_rn: cuint): cuint32; inline;
procedure mtdcr(rn, val: cuint); inline;
function mfmsr(): cuint32; inline;
procedure mtmsr(val: cuint); inline;
function mfdec(): cuint32; inline;
procedure mtdec(_val: cuint); inline;
function mfspr(_rn: cuint): cuint32; inline;
procedure mtspr(_rn, _val: cuint); inline;

{$define mfwpar()	:= mfspr(WPAR)}
{$define mtwpar(_val)	:= mtspr(WPAR,_val)}

{$define mfmmcr0()	:= mfspr(MMCR0)}
{$define mtmmcr0(_val):= mtspr(MMCR0,_val)}
{$define mfmmcr1()	:= mfspr(MMCR1)}
{$define mtmmcr1(_val):= mtspr(MMCR1,_val)}

{$define mfpmc1()	:= mfspr(PMC1)}
{$define mtpmc1(_val) := mtspr(PMC1,_val)}
{$define mfpmc2() := mfspr(PMC2)}
{$define mtpmc2(_val) := mtspr(PMC2,_val)}
{$define mfpmc3() := mfspr(PMC3)}
{$define mtpmc3(_val) := mtspr(PMC3,_val)}
{$define mfpmc4() := mfspr(PMC4)}
{$define mtpmc4(_val) := mtspr(PMC4,_val)}

{$define mfhid0() := mfspr(HID0)}
{$define mthid0(_val) := mtspr(HID0,_val)}
{$define mfhid1() := mfspr(HID1)}
{$define mthid1(_val) := mtspr(HID1,_val)}
{$define mfhid2() := mfspr(HID2)}
{$define mthid2(_val) := mtspr(HID2,_val)}
{$define mfhid4() := mfspr(HID4)}
{$define mthid4(_val) := mtspr(HID4,_val)}

function __lhbrx(base, index: cuint32): cuint16; inline;
//procedure __lwbrx(base,index: cuint32); inline;
function __lwbrx(base,index: cuint32): cuint32; inline;
procedure __sthbrx(base,index,value: cuint32); inline;
procedure __stwbrx(base,index,value: cuint32); inline;
function cntlzw(_val: cuint32): cuint32; inline;
procedure _CPU_MSR_GET( _msr_value: cuint32 ); inline;
procedure _CPU_MSR_SET( _msr_value: cuint32 ); inline;
procedure _CPU_ISR_Enable(); inline;
procedure _CPU_ISR_Disable( _isr_cookie: cuint32 ); inline;
procedure _CPU_ISR_Restore( _isr_cookie: cuint32 ); inline;
procedure _CPU_ISR_Flash( _isr_cookie: cuint32 ); inline;
procedure _CPU_FPR_Enable(); inline;
procedure _CPU_FPR_Disable(); inline;
function bswap16(val: cuint16): cuint16; inline;
function bswap32(val: cuint32): cuint32; inline;
function bswap64(val: cuint64): cuint64; inline;

// Basic I/O
function read32(addr: cuint32): cuint32; inline;
procedure write32(addr, x: cuint32); inline;
procedure mask32(addr, clear, _set: cuint32); inline;
function read16(addr: cuint32): cuint16; inline;
procedure write16(addr: cuint32; x: cuint16); inline;
function read8(addr: cuint32): cuint8; inline;
procedure write8(addr: cuint32; x: cuint8); inline;
procedure writef32(addr: cuint32; x: f32); inline;
{$ENDIF}




{$IFDEF OGC_IMPLEMENTATION}
procedure _sync(); inline; assembler;
asm
  sync
end;

procedure _nop(); inline; assembler;
asm
  nop
end;

procedure ppcsync(); inline; assembler;
asm
  sc
end;

procedure ppchalt(); inline;
begin
	asm
    sync
  end;
	while true do
  asm
	  nop
    li 3,0
    nop
  end;
end;

function mfpvr(): cint32; inline;
var
  _rval: cuint32;
begin
  asm
//    mfpvr r3     // unrecognized opcode?
    mfspr r3,287     // unrecognized opcode?
  end;
  result := _rval;
end;

function mfdcr(_rn: cuint): cuint32; inline;
var
  _rval: cuint32;
begin
  asm
  //    mfdcr r3,_rn  // unrecognized opcode?
  end;
  result := _rval;
end;

procedure mtdcr(rn, val: cuint); inline;
begin
  asm
//    mtdcr rn,r3   // unrecognized opcode?
  end;
end;

function mfmsr(): cuint32; inline;
var
  _rval: cuint32;
begin
  asm
    mfmsr r3
  end;
  result := _rval;
end;

procedure mtmsr(val: cuint); inline;
begin
  asm
    mtmsr r3
  end;
end;

function mfdec(): cuint32; inline;
var
  _rval: cuint32;
begin
  asm
//    mfdec r3    // unrecognized opcode?
    mfspr r3,22    // unrecognized opcode?
  end;
  result := _rval;
end;

procedure mtdec(_val: cuint); inline;
begin
  asm
//    mtdec r3   // unrecognized opcode?
    mtspr 22,r3   // unrecognized opcode?
  end;
end;

function mfspr(_rn: cuint): cuint32; inline;
var
  _rval: cuint32 = 0;
begin
	asm
    mfspr r3,r4
  end;
  result := _rval;
end;

procedure mtspr(_rn, _val: cuint); inline;
begin
  asm
    mtspr r3,r4
  end;
end;

function __lhbrx(base, index: cuint32): cuint16; inline;
var
  res: cuint16;
begin
	asm
    lhbrx	r3,r4,r5
  end;
  result := res;
end;

//procedure __lwbrx(base,index: cuint32); inline;
function __lwbrx(base,index: cuint32): cuint32; inline;
var
  res: cuint32;
begin
	asm
    lwbrx	r3,r4,r5
  end;
  result := res;
end;

procedure __sthbrx(base,index,value: cuint32); inline;
begin
	asm
    sthbrx	r3,r4,f5
  end;
end;

procedure __stwbrx(base,index,value: cuint32); inline;
begin
	asm
    stwbrx	r3,r4,r5
  end;
end;

function cntlzw(_val: cuint32): cuint32; inline;
var
  _rval: cuint32;
begin
  asm
    cntlzw r3, r4
  end;
  result := _rval;
end;

procedure _CPU_MSR_GET( _msr_value: cuint32 ); inline;
begin
  repeat
    _msr_value := 0;
    asm
      mfmsr r3
    end;
  until false;
end;

procedure _CPU_MSR_SET( _msr_value: cuint32 ); inline;
begin
  asm
    mtmsr r3
  end;
end;

procedure _CPU_ISR_Enable(); inline;
var
  _val: cuint32 = 0;
begin
  asm
		mfmsr r3
		ori r3,r3,0x8000
		mtmsr r3
  end;
end;

procedure _CPU_ISR_Disable( _isr_cookie: cuint32 ); inline;
var
  _disable_mask: cuint32 = 0;
begin
	_isr_cookie := 0;
  asm
	  mfmsr r3
	  rlwinm r4,r3,0,17,15
	  mtmsr r4
	  extrwi r3,r3,1,16
  end;
end;

procedure _CPU_ISR_Restore( _isr_cookie: cuint32 ); inline;
var
  _enable_mask: cuint32 = 0;
begin
	asm
    cmpwi r3,0
    beq .L1
    mfmsr r4
    ori r4,r4,0x8000
    mtmsr r4
	.L1:
  end;
end;



procedure _CPU_ISR_Flash( _isr_cookie: cuint32 ); inline;
var
  _flash_mask: cuint32 = 0;
begin
  asm
    cmpwi r3,0
    beq .L1
    mfmsr r4
    ori r4,r4,0x8000
    mtmsr r4
    rlwinm r4,r4,0,17,15
    mtmsr r4
    .L1:
  end;
end;

procedure _CPU_FPR_Enable(); inline;
var
  _val: cuint32 = 0;
begin
  asm
    mfmsr r3;
    ori r3,r3,0x2000;
    mtmsr r3
  end;
end;

procedure _CPU_FPR_Disable(); inline;
var
  _val: cuint32 = 0;
begin
  asm
    mfmsr r3;
    rlwinm r3,r3,0,19,17;
    mtmsr r3
  end;
end;

function bswap16(val: cuint16): cuint16; inline;
var
  tmp: cuint16;
begin
	tmp := val;
  result := __lhbrx(tmp,0); //??
end;

function bswap32(val: cuint32): cuint32; inline;
var
  tmp: cuint32;
begin
  tmp := val;
	result := __lwbrx(tmp,0);
end;

function bswap64(val: cuint64): cuint64; inline;
type
	ullc = record
    case integer of
		  0: (ull: cuint64);
		  1: (ul: array [0..1] of cuint32)
	end;
var
  outv: ullc;
  tmp: cuint64;
begin
	tmp := val;
	outv.ul[0] := __lwbrx(tmp,4);
	outv.ul[1] := __lwbrx(tmp,0);

	result := outv.ull;
end;


// Basic I/O
function read32(addr: cuint32): cuint32; inline;
var
	x: cuint32;
begin
  asm
    lwz r3,0(r4) ;
    sync
  end;
	result := x;
end;

procedure write32(addr, x: cuint32); inline;
begin
  asm
    stw r3,0(r4) ;
    eieio
  end;
end;


procedure mask32(addr, clear, _set: cuint32); inline;
begin
	write32(addr, (read32(addr) and (not clear)) or _set);
end;

function read16(addr: cuint32): cuint16; inline;
var
	x: cuint16;
begin
	asm
    lhz r3,0(r4) ;
    sync
  end;
	result := x;
end;

procedure write16(addr: cuint32; x: cuint16); inline;
begin
	asm
    sth r3,0(r4) ;
    eieio
  end;
end;

function read8(addr: cuint32): cuint8; inline;
var
	x: cuint8;
begin
  asm
    lbz r3,0(r4) ;
    sync
  end;
	result := x;
end;

procedure write8(addr: cuint32; x: cuint8); inline;
begin
	asm
    stb r3,0(r4) ;
    eieio
  end;
end;

procedure writef32(addr: cuint32; x: f32); inline;
begin
	asm
    stfs r3,0(r4) ;
    eieio
  end;
end;

{$ENDIF}











