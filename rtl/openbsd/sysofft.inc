

function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; assembler; [public, alias : 'FPC_SYSC_LSEEK'];

asm
  stwu    r1,-16(r1)
  mflr    r0
  stw     r0,20(r1)
  mr      r11,r4
  mr      r12,r5
  mr      r5,r3
  mr      r9,r6
  li      r3,0
  mr      r7,r11
  mr      r8,r12
  li      r6,0
  li      r4,199
//  crclr   cr1*4+eq
  crxor   6,6,6
  li      r0,198
  sc
  bso     .Lcerrorlseek
//  mr    r0,r3
//  mr      r3,r4
//  mr    r4,r0
  lwz     r0,20(r1)
  mtlr    r0
  addi    r1,r1,16
  blr
.Lcerrorlseek:
  lis   r4,Errno@ha
  stw   r3,Errno@l(r4)
  li    r3,-1
  li    r4,-1
  lwz     r0,20(r1)
  mtlr    r0
  addi    r1,r1,16
end;

function Fpftruncate(fd : cint; flength : off_t): cint; assembler; [public, alias : 'FPC_SYSC_FTRUNCATE'];

asm
stwu    r1,-16(r1)
mflr    r0
stw     r0,20(r1)
mr      r7,r5
mr      r8,r6
mr      r5,r3
li      r3,0
li      r4,201
li      r6,0

//crclr   cr1*4+eq
crxor  6,6,6
li      r0,198
sc
bso     .Lcerrortrunc
mr      r4,r3
lwz     r0,20(r1)
mtlr    r0
addi    r1,r1,16
blr
.Lcerrortrunc:
  lis     r4,Errno@ha
  stw     r3,Errno@l(r4)
  li      r3,-1
  lwz     r0,20(r1)
  mtlr    r0
  addi    r1,r1,16
end;

Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; assembler;  [public, alias: 'FPC_SYSC_MMAP'];

//Function Fpmmap(adr,len,prot,flags,fdes,off:longint):longint;  assembler; [public, alias : 'FPC_SYSC_MMAP'];

asm
  stwu    r1,-16(r1)
  mflr    r0
  stw     r0,20(r1)
  mr      r0,r5
  mr      r8,r6
  stw     r9,8(r1)
  stw     r10,12(r1)
  mr      r5,r3
  mr      r6,r4
  mr      r9,r7
  li      r3,0
  mr      r7,r0
  li      r4,197
  li      r10,0
//  crclr   cr1*4+eq
  crxor  6,6,6
  li      r0,198
  sc
  bso     .Lcerrormmap
  mr      r4,r3
  lwz     r0,20(r1)
  mtlr    r0
  addi    r1,r1,16
  blr
.Lcerrormmap:
  lis     r4,Errno@ha
  stw     r3,Errno@l(r4)
  li      r3,-1
  lwz     r0,20(r1)
  mtlr    r0
  addi    r1,r1,16
end;
