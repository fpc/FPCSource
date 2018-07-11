{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i8086+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_cpuinit;
  begin
  end;

{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure FillChar(var x;count:SizeUInt;value:byte);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // count
  jcxz @@Done
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @x
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @x
{$endif FPC_X86_DATA_NEAR}
  mov al, ss:[bx + 2 + extra_param_offset]  // value
  mov ah, al
  shr cx, 1
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  rep stosw
  adc cx, cx
  rep stosb
@@Done:
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord(var x;count : SizeInt;value : word);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // count
  or cx, cx
  jle @@Done
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @x
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @x
{$endif FPC_X86_DATA_NEAR}
  mov ax, ss:[bx + 2 + extra_param_offset]  // value
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  rep stosw
  @@Done:
end;
{$endif FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count : SizeInt;value : dword);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 6 + extra_param_offset]  // count
  or cx, cx
  jle @@Done
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 8 + extra_param_offset]  // @x
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 8 + extra_param_offset]  // @x
{$endif FPC_X86_DATA_NEAR}
  mov ax, ss:[bx + 2 + extra_param_offset]  // lo(value)
  mov bx, ss:[bx + 4 + extra_param_offset]  // hi(value)
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  cmp ax, bx
  jne @@lo_hi_different
  shl cx, 1
  rep stosw
  jmp @@Done

@@lo_hi_different:
  stosw
  xchg ax, bx
  stosw
  xchg ax, bx
  loop @@lo_hi_different

  @@Done:
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


procedure MoveData(srcseg,srcoff,destseg,destoff:Word;n:Word);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // count
  jcxz @@Done

  mov ax, ds  // backup ds
  lds si, ss:[bx + 8 + extra_param_offset]  // @source
  les di, ss:[bx + 4 + extra_param_offset]  // @dest

  cmp si, di
  jb @@BackwardsMove

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  shr cx, 1
  rep movsw
  adc cx, cx
  rep movsb
  jmp @@AfterMove  // todo, add mov ds,ax & ret here for performance reasons

@@BackwardsMove:
  std
  add si, cx
  add di, cx
  dec si
  dec di
  rep movsb  // todo: movsw
  cld

@@AfterMove:
  mov ds, ax

@@Done:
end;


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeUInt);[public, alias: 'FPC_MOVE'];assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // count
  jcxz @@Done

  mov ax, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, ax
  mov si, ss:[bx + 6 + extra_param_offset]  // @source
  mov di, ss:[bx + 4 + extra_param_offset]  // @dest
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 8 + extra_param_offset]  // @source
  les di, ss:[bx + 4 + extra_param_offset]  // @dest
{$endif FPC_X86_DATA_NEAR}

  cmp si, di
  jb @@BackwardsMove

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  shr cx, 1
  rep movsw
  adc cx, cx
  rep movsb
  jmp @@AfterMove  // todo, add mov ds,ax & ret here for performance reasons

@@BackwardsMove:
  std
  add si, cx
  add di, cx
  dec si
  dec di
  rep movsb  // todo: movsw
  cld

@@AfterMove:
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, ax
{$endif}

@@Done:
end;
{$endif FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // len
  jcxz @@NotFound
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @buf
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @buf
{$endif FPC_X86_DATA_NEAR}
  mov si, di  // save the start of the buffer in si
  mov al, ss:[bx + 2 + extra_param_offset]  // b
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  repne scasb
  je @@Found
@@NotFound:
  mov ax, 0FFFFh  // return -1
  jmp @@Done
@@Found:
  sub di, si
  xchg ax, di
  dec ax
@@Done:
end;
{$endif FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
function IndexWord(Const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // len
  jcxz @@NotFound
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @buf
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @buf
{$endif FPC_X86_DATA_NEAR}
  mov si, cx  // save the length of the buffer in si
  mov ax, ss:[bx + 2 + extra_param_offset]  // b
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  repne scasw
  je @@Found
@@NotFound:
  mov ax, 0FFFFh  // return -1
  jmp @@Done
@@Found:
  sub si, cx
  xchg ax, si
  dec ax
@@Done:
end;
{$endif FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:SizeInt;b:DWord):SizeInt; assembler; nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 6 + extra_param_offset]  // len
  jcxz @@NotFound
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 8 + extra_param_offset]  // @buf
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 8 + extra_param_offset]  // @buf
{$endif FPC_X86_DATA_NEAR}
  mov si, cx  // save the length of the buffer in si
  mov ax, ss:[bx + 2 + extra_param_offset]  // b
  mov bx, ss:[bx + 4 + extra_param_offset]
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  jmp @@LoopStart
@@SkipWord:
  scasw
@@LoopStart:
  scasw
  loopne @@SkipWord
  jne @@NotFound
  xchg ax, bx
  scasw
  je @@Found
  jcxz @@NotFound
  xchg ax, bx
  jmp @@LoopStart

@@Found:
  sub si, cx
  xchg ax, si
  dec ax
  jmp @@Done

@@NotFound:
  mov ax, 0FFFFh  // return -1
@@Done:
end;
{$endif FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
  xor ax, ax  // initialize ax=0 (it's the result register, we never use it for anything else in this function)
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // len
  jcxz @@Done

  mov dx, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, dx
  mov si, ss:[bx + 6 + extra_param_offset]  // @buf1
  mov di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 8 + extra_param_offset]  // @buf1
  les di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$endif FPC_X86_DATA_NEAR}

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  xor bx, bx
  shr cx, 1
  adc bx, bx  // remainder goes to bx
  jcxz @@BytewiseComparison
  repe cmpsw
  je @@BytewiseComparison
  // we found an unequal word
  // let's go back and compare it bytewise
  mov bl, 2
  dec si
  dec si
  dec di
  dec di
@@BytewiseComparison:
  mov cx, bx
  jcxz @@Equal
  repe cmpsb
  je @@Equal
  // ax is 0
  sbb ax, ax
  shl ax, 1
  inc ax

@@Equal:
  // ax is 0

{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, dx
{$endif}
@@Done:
end;
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
  xor ax, ax  // initialize ax=0 (it's the result register, we never use it for anything else in this function)
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // len
  jcxz @@Done

  mov dx, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, dx
  mov si, ss:[bx + 6 + extra_param_offset]  // @buf1
  mov di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 8 + extra_param_offset]  // @buf1
  les di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$endif FPC_X86_DATA_NEAR}

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  repe cmpsw
  je @@Equal
  // ax is 0
  sbb ax, ax
  shl ax, 1
  inc ax

@@Equal:
  // ax is 0

{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, dx
{$endif}
@@Done:
end;
{$endif FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
  xor ax, ax  // initialize ax=0 (it's the result register, we never use it for anything else in this function)
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // len
  jcxz @@Done
  cmp cx, 4000h
  jb @@NotTooBig
  mov cx, 4000h
@@NotTooBig:
  shl cx, 1

  mov dx, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, dx
  mov si, ss:[bx + 6 + extra_param_offset]  // @buf1
  mov di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 8 + extra_param_offset]  // @buf1
  les di, ss:[bx + 4 + extra_param_offset]  // @buf2
{$endif FPC_X86_DATA_NEAR}

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  repe cmpsw
  je @@Equal

  // ax is 0
  sbb ax, ax
  shl ax, 1
  inc ax

  shr cx, 1
  jnc @@Skip

  xchg ax, bx
  xor ax, ax
  cmpsw
  je @@hi_equal
  // ax is 0
  sbb ax, ax
  shl ax, 1
  inc ax
  jmp @@Skip

@@hi_equal:
  xchg ax, bx

@@Equal:
  // ax is 0
@@Skip:

{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, dx
{$endif}
@@Done:
end;
{$endif FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):sizeint;assembler;nostackframe;[public,alias:'FPC_PCHAR_LENGTH']; compilerproc;
asm
  mov bx, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov ax, ss:[bx + 2 + extra_param_offset]  // p
  test ax, ax
  jz @@Done
  xchg ax, di
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 2 + extra_param_offset]  // p
  mov ax, es
  or ax, di
  jz @@Done
{$endif FPC_X86_DATA_NEAR}
  mov cx, 0FFFFh
  xor ax, ax
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  repne scasb
  dec ax
  dec ax
  sub ax, cx
@@Done:
end;
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;
asm
  mov ax, sp
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov dx, ss
{$endif}
end;

{$define FPC_SYSTEM_HAS_PTR}
function Ptr(sel,off: Word):farpointer;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov si, sp
  mov ax, ss:[si + 2 + extra_param_offset]  // off
  mov dx, ss:[si + 4 + extra_param_offset]  // sel
end;

{$define FPC_SYSTEM_HAS_CSEG}
function CSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  CSeg:=fpc_x86_get_cs;
end;

{$define FPC_SYSTEM_HAS_DSEG}
function DSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  DSeg:=fpc_x86_get_ds;
end;

{$define FPC_SYSTEM_HAS_SSEG}
function SSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  SSeg:=fpc_x86_get_ss;
end;

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  mov ax, bp
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov dx, ss
{$endif}
end;
{$ENDIF not INTERNAL_BACKTRACE}

{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
Function Get_pc_addr : CodePointer;assembler;nostackframe;
asm
  mov bx, sp
  mov ax, ss:[bx]
{$ifdef FPC_X86_CODE_FAR}
  mov dx, ss:[bx+2]
{$endif FPC_X86_CODE_FAR}
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:codepointer=nil):codepointer;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_CODE_FAR}
  xor dx, dx
{$endif FPC_X86_CODE_FAR}
{$ifdef FPC_X86_DATA_NEAR}
  mov ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
 {$ifdef WIN16}
  mov cx, ax
  and al, $FE
 {$endif WIN16}
  or ax, ax
  jz @@Lg_a_null
  xchg ax, bx  // 1 byte shorter than a mov
  mov ax, [bx+2]
 {$ifdef FPC_X86_CODE_FAR}
  {$ifdef WIN16}
   test cl, 1
   jnz @@farretaddr
   mov dx, ss:[si + 2 + extra_param_offset + extra_param_offset]  // Seg(addr^)
   jmp @@retsegdone
@@farretaddr:
   mov dx, [bx+4]
@@retsegdone:
  {$else WIN16}
   mov dx, [bx+4]
  {$endif WIN16}
 {$endif FPC_X86_CODE_FAR}
{$else FPC_X86_DATA_NEAR}
  les ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
 {$ifdef WIN16}
  mov cx, ax
  and al, $FE
 {$endif WIN16}
  mov dx, es
  or dx, ax
  jz @@Lg_a_null
  xchg ax, bx  // 1 byte shorter than a mov
  mov ax, es:[bx+2]
 {$ifdef FPC_X86_CODE_FAR}
  {$ifdef WIN16}
   test cl, 1
   jnz @@farretaddr
   mov dx, ss:[si + 2 + extra_param_offset + extra_param_offset]  // Seg(addr^)
   jmp @@retsegdone
@@farretaddr:
   mov dx, es:[bx+4]
@@retsegdone:
  {$else WIN16}
   mov dx, es:[bx+4]
  {$endif WIN16}
 {$endif FPC_X86_CODE_FAR}
{$endif FPC_X86_DATA_NEAR}
@@Lg_a_null:
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:codepointer=nil):pointer;nostackframe;assembler;
asm
{$ifdef FPC_X86_DATA_NEAR}
  mov si, sp
  mov ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
 {$ifdef WIN16}
  and al, $FE
 {$endif WIN16}
  or ax, ax
  jz @@Lgnf_null
  xchg ax, si  // 1 byte shorter than a mov
  lodsw
@@Lgnf_null:
{$else FPC_X86_DATA_NEAR}
  mov si, sp
  les ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
 {$ifdef WIN16}
  and al, $FE
 {$endif WIN16}
  mov dx, es
  or dx, ax
  jz @@Lgnf_null
  xchg ax, si  // 1 byte shorter than a mov
  seges lodsw
 {$ifdef WIN16}
  and al, $FE
 {$endif WIN16}
  mov dx, es
@@Lgnf_null:
{$endif FPC_X86_DATA_NEAR}
end;

function InterLockedDecrement (var Target: smallint) : smallint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 2 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  pushf
  cli
  sub word [bx], 1
  mov ax, [bx]
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedDecrement (var Target: longint) : longint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 2 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  pushf
  cli
  sub word [bx], 1
  sbb word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedIncrement (var Target: smallint) : smallint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 2 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  pushf
  cli
  add word [bx], 1
  mov ax, [bx]
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedIncrement (var Target: longint) : longint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 2 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  pushf
  cli
  add word [bx], 1
  adc word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 4 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 4 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov ax, ss:[si + 2 + extra_param_offset]  // Source
  xchg word [bx], ax
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedExchange (var Target: longint;Source : longint) : longint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 6 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 6 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov ax, ss:[si + 2 + extra_param_offset]  // Lo(Source)
  mov dx, ss:[si + 4 + extra_param_offset]  // Hi(Source)
  pushf
  cli
  xchg word [bx], ax
  xchg word [bx+2], dx
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 4 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 4 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov di, ss:[si + 2 + extra_param_offset]  // Source
  pushf
  cli
  mov ax, [bx]
  add word [bx], di
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, ss:[si + 6 + extra_param_offset]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, ss:[si + 6 + extra_param_offset]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov di, ss:[si + 2 + extra_param_offset]  // Lo(Source)
  mov si, ss:[si + 4 + extra_param_offset]  // Hi(Source)
  pushf
  cli
  mov ax, [bx]
  mov dx, [bx+2]
  add word [bx], di
  adc word [bx+2], si
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;assembler;
asm
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, [Target]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, [Target]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov di, [Comperand]
  pushf
  cli
  mov ax, [bx]
  cmp ax, di
  jne @@not_equal
  mov di, [NewValue]
  mov [bx], di
@@not_equal:
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;

function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;assembler;
asm
{$ifdef FPC_X86_DATA_NEAR}
  mov bx, [Target]  // Target
{$else FPC_X86_DATA_NEAR}
  mov cx, ds
  lds bx, [Target]  // Target
{$endif FPC_X86_DATA_NEAR}
  mov di, word [Comperand]
  mov si, word [Comperand+2]
  pushf
  cli
  mov ax, [bx]
  mov dx, [bx+2]
  cmp ax, di
  jne @@not_equal
  cmp dx, si
  jne @@not_equal
  mov di, word [NewValue]
  mov si, word [NewValue+2]
  mov [bx], di
  mov [bx+2], si
@@not_equal:
  popf
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, cx
{$endif}
end;


{****************************************************************************
                              Stack checking
****************************************************************************}


procedure fpc_stackcheck_i8086;[public,alias:'FPC_STACKCHECK_I8086'];compilerproc;assembler;nostackframe;
const
  STACK_MARGIN=512;
asm
  { on entry: AX = required stack size to check if available
                   (function is called before stack allocation) }
{$ifdef FPC_MM_HUGE}
  push ds
  push ax
  mov ax, SEG @DATA
  mov ds, ax
  pop ax
{$endif FPC_MM_HUGE}
  add ax, STACK_MARGIN
  jc @@stack_overflow
  add ax, word ptr [__stkbottom]
  jc @@stack_overflow
  cmp ax, sp
  ja @@stack_overflow
@@no_overflow:
{$ifdef FPC_MM_HUGE}
  pop ds
{$endif FPC_MM_HUGE}
  ret

@@stack_overflow:
  { check StackError flag, to avoid recursive calls from the exit routines }
  cmp byte ptr [StackError], 1
  je @@no_overflow
  mov byte ptr [StackError], 1
  { cleanup return address (and maybe saved ds) from call to this function }
{$if defined(FPC_MM_HUGE)}
  add sp, 6
{$elseif defined(FPC_X86_CODE_FAR)}
  pop ax
  pop ax
{$else}
  pop ax
{$endif}
  { call HandleError(202) }
{$ifdef CPU8086}
  xor ax, ax
  push ax
  mov al, 202
  push ax
{$else}
  push 0
  push 202
{$endif}
  call HandleError
end;


{****************************************************************************
                                  BSR/BSF
****************************************************************************}

const
  bsr8bit: array [Byte] of Byte = (
    $ff,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
      5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    );
  bsf8bit: array [Byte] of Byte = (
    $ff,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      7,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
    );

{$define FPC_SYSTEM_HAS_BSR_BYTE}
function BsrByte(const AValue: Byte): Byte;
begin
  BsrByte := bsr8bit[AValue];
end;

{$define FPC_SYSTEM_HAS_BSF_BYTE}
function BsfByte(const AValue: Byte): Byte;
begin
  BsfByte := bsf8bit[AValue];
end;

{$define FPC_SYSTEM_HAS_BSR_WORD}
function BsrWord(const AValue: Word): Byte; assembler;
asm
     lea   bx, bsr8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ah, ah
     jz    @@0
     mov   cl, 8
     mov   al, ah
@@0: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_WORD}
function BsfWord(const AValue: Word): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  al, al
     jnz   @@0
     or    al, ah
     jz    @@0
     add   cl, 8
@@0: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSR_DWORD}
function BsrDword(const AValue: DWord): Byte; assembler;
asm
     lea   bx, bsr8bit
     mov   cl, 16
     mov   ax, word [AValue+2]
     test  ax, ax
     jnz   @@0
     xor   cl, cl
     mov   ax, word [AValue]
@@0: test  ah, ah
     jz    @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_DWORD}
function BsfDword(const AValue: DWord): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ax, ax
     jnz   @@0
     or    ax, word [AValue+2]
     jz    @@1
     mov   cl, 16
@@0: test  al, al
     jnz   @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSR_QWORD}
function BsrQword(const AValue: QWord): Byte; assembler;
asm
     lea   bx, bsr8bit
     mov   cl, 48
     mov   ax, word [AValue+6]
     test  ax, ax
     jnz   @@0
     mov   cl, 32
     or    ax, word [AValue+4]
     jnz   @@0
     mov   cl, 16
     or    ax, word [AValue+2]
     jnz   @@0
     xor   cl, cl
     mov   ax, word [AValue]
@@0: test  ah, ah
     jz    @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_QWORD}
function BsfQword(const AValue: QWord): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ax, ax
     jnz   @@0
     mov   cl, 16
     or    ax, word [AValue+2]
     jnz   @@0
     mov   cl, 32
     or    ax, word [AValue+4]
     jnz   @@0
     xor   cl, cl
     or    ax, word [AValue+6]
     jz    @@1
     mov   cl, 48
@@0: test  al, al
     jnz   @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;


{****************************************************************************
                                  HexStr
****************************************************************************}

{$define FPC_HAS_HEXSTR_POINTER_SHORTSTR}
function HexStr(Val: NearPointer): ShortString;
begin
  HexStr:=HexStr(Word(Val),4);
end;

function HexStr(Val: NearCsPointer): ShortString;
begin
  HexStr:='CS:'+HexStr(Word(Val),4);
end;

function HexStr(Val: NearDsPointer): ShortString;
begin
  HexStr:='DS:'+HexStr(Word(Val),4);
end;

function HexStr(Val: NearEsPointer): ShortString;
begin
  HexStr:='ES:'+HexStr(Word(Val),4);
end;

function HexStr(Val: NearSsPointer): ShortString;
begin
  HexStr:='SS:'+HexStr(Word(Val),4);
end;

function HexStr(Val: NearFsPointer): ShortString;
begin
  HexStr:='FS:'+HexStr(Word(Val),4);
end;

function HexStr(Val: NearGsPointer): ShortString;
begin
  HexStr:='GS:'+HexStr(Word(Val),4);
end;

function HexStr(Val: FarPointer): ShortString;
type
  TFarPointerRec = record
    Offset, Segment: Word;
  end;
begin
  HexStr:=HexStr(TFarPointerRec(Val).Segment,4)+':'+HexStr(TFarPointerRec(Val).Offset,4);
end;

function HexStr(Val: HugePointer): ShortString;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  HexStr:=HexStr(FarPointer(Val));
end;


{****************************************************************************
                                  FPU
****************************************************************************}

const
  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

{ Detects the FPU and initializes the Test8087 variable (and Default8087CW):
  0 = NO FPU
  1 = 8087
  2 = 80287
  3 = 80387+ }
procedure DetectFPU;
  var
    localfpucw: word;
  begin
    asm
      xor bx, bx  { initialization, 0=NO FPU }

      { FPU presence detection }
      fninit
      mov byte [localfpucw + 1], 0
      nop
      fnstcw localfpucw
      cmp byte [localfpucw + 1], 3
      jne @@Done  { No FPU? }
      inc bx

      { FPU found; now test if it's a 8087 }
      and byte [localfpucw], $7F  { clear the interrupt enable mask (IEM) }
      fldcw localfpucw
      fdisi                       { try to set the interrupt enable mask }
      fstcw localfpucw
      test byte [localfpucw], $80 { IEM set? }
      jnz @@Done  { if yes, we have an 8087 }
      inc bx

      { we have a 287+; now test if it's a 80287 }
      finit
      fld1
      fldz
      fdiv                        { calculate 1/0 }
      fld st                      { copy the value }
      fchs                        { change the sign }
      fcompp  { compare. if the FPU distinguishes +inf from -inf, it's a 387+ }
      fstsw localfpucw
      mov ah, byte [localfpucw + 1]
      sahf
      je @@Done
      inc bx  { 387+ }
@@Done:
      mov Test8087, bl
    end ['AX','BX'];
    if Test8087<=2 then
      Default8087CW:=$1330
    else
      Default8087CW:=$1332;
  end;

{$ifndef FPC_SYSTEM_HAS_SYSINITFPU}
{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    asm
      fninit
      fldcw   localfpucw
      fwait
    end;
  end;

{$endif ndef FPC_SYSTEM_HAS_SYSINITFPU}

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    asm
      fninit
      fwait
      fldcw   localfpucw
    end;
  end;

{$I int32p.inc}
{$I hugeptr.inc}

