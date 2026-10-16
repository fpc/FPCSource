{ %cpu=i386 }
{ %skiptarget=os2,emx }
{ cdecl functions returning records of every size from 1 to 8 bytes, called
  directly; those of 3, 5, 6 or 7 bytes also through a procedural variable and
  for a field of the result. On i386-win32 those stopped the compiler with an
  internal error. os2 and emx still return one of 3 bytes in registers.

  On win32, assembler routines also check the convention itself, independently
  of the compiler under test: a record of 1, 2, 4 or 8 bytes is returned in AL,
  AX, EAX or EDX:EAX, one of 3, 5, 6 or 7 bytes through a hidden pointer passed
  before the first argument, which the callee leaves on the stack for its
  caller }
program tb0749;

{$mode objfpc}

type
  TR1 = record b: array[0..0] of byte; end;
  TR2 = record b: array[0..1] of byte; end;
  TR3 = record b: array[0..2] of byte; end;
  TR4 = record b: array[0..3] of byte; end;
  TR5 = record b: array[0..4] of byte; end;
  TR6 = record b: array[0..5] of byte; end;
  TR7 = record b: array[0..6] of byte; end;
  TR8 = record b: array[0..7] of byte; end;
  TR3m = packed record w: word; c: byte; end;

  TF3 = function: TR3; cdecl;
  TF5 = function: TR5; cdecl;
  TF6 = function: TR6; cdecl;
  TF7 = function: TR7; cdecl;

procedure Fill(out b; n: longint);
var
  i: longint;
begin
  for i:=0 to n-1 do
    pbyte(@b)[i]:=i+1;
end;

procedure Check(const b; n, code: longint);
var
  i: longint;
begin
  for i:=0 to n-1 do
    if pbyte(@b)[i]<>i+1 then
      halt(code);
end;

function Get1: TR1; cdecl; begin Fill(result,sizeof(result)); end;
function Get2: TR2; cdecl; begin Fill(result,sizeof(result)); end;
function Get3: TR3; cdecl; begin Fill(result,sizeof(result)); end;
function Get4: TR4; cdecl; begin Fill(result,sizeof(result)); end;
function Get5: TR5; cdecl; begin Fill(result,sizeof(result)); end;
function Get6: TR6; cdecl; begin Fill(result,sizeof(result)); end;
function Get7: TR7; cdecl; begin Fill(result,sizeof(result)); end;
function Get8: TR8; cdecl; begin Fill(result,sizeof(result)); end;

function Get3m: TR3m; cdecl;
begin
  result.w:=$0201;
  result.c:=3;
end;

{$ifdef win32}
{$asmmode intel}

const
  Magic = longint($12345678);

procedure CheckFrom(const b; n, first, code: longint);
var
  i: longint;
begin
  for i:=0 to n-1 do
    if pbyte(@b)[i]<>first+i then
      halt(code);
end;

function GetArg3(x: longint): TR3; cdecl;
begin
  result.b[0]:=x;
  result.b[1]:=x+1;
  result.b[2]:=x+2;
end;

{ The AsmN routines return by their own ret, not through the compiler's
  epilogue. Those of 3, 5, 6 and 7 bytes jump to FillHidden with the size in
  EDX: it stores the argument at [esp+8], counting up, through the hidden
  pointer at [esp+4] }
procedure FillHidden; assembler; nostackframe;
asm
  mov eax,[esp+4]
  mov ecx,[esp+8]
@next:
  mov [eax],cl
  inc eax
  inc ecx
  dec edx
  jnz @next
  mov eax,[esp+4]
  ret
end;

function Asm3(x: longint): TR3; cdecl; assembler; nostackframe;
asm
  mov edx,3
  jmp FillHidden
end;

function Asm5(x: longint): TR5; cdecl; assembler; nostackframe;
asm
  mov edx,5
  jmp FillHidden
end;

function Asm6(x: longint): TR6; cdecl; assembler; nostackframe;
asm
  mov edx,6
  jmp FillHidden
end;

function Asm7(x: longint): TR7; cdecl; assembler; nostackframe;
asm
  mov edx,7
  jmp FillHidden
end;

{ Those of 1, 2, 4 and 8 bytes take the argument at [esp+4] and return bytes
  counting up from $21, $31, $41 and $11 in AL, AX, EAX and EDX:EAX -- zeroes
  if the argument is not Magic }
function Asm1(x: longint): TR1; cdecl; assembler; nostackframe;
asm
  xor eax,eax
  cmp dword ptr [esp+4],$12345678
  jne @done
  mov al,$21
@done:
  ret
end;

function Asm2(x: longint): TR2; cdecl; assembler; nostackframe;
asm
  xor eax,eax
  cmp dword ptr [esp+4],$12345678
  jne @done
  mov ax,$3231
@done:
  ret
end;

function Asm4(x: longint): TR4; cdecl; assembler; nostackframe;
asm
  xor eax,eax
  cmp dword ptr [esp+4],$12345678
  jne @done
  mov eax,$44434241
@done:
  ret
end;

function Asm8(x: longint): TR8; cdecl; assembler; nostackframe;
asm
  xor eax,eax
  xor edx,edx
  cmp dword ptr [esp+4],$12345678
  jne @done
  mov eax,$14131211
  mov edx,$18171615
@done:
  ret
end;

{ the caller's stack pointer }
function GetEsp: pointer; assembler; nostackframe;
asm
  lea eax,[esp+4]
end;

{ GetArg3(x) into p^, called by that convention; false if the stack pointer
  was not restored }
function CallGetArg3(p: pointer; x: longint): boolean; assembler; nostackframe;
asm
  push ebx
  mov ebx,esp
  push edx
  push eax
  call GetArg3
  add esp,8
  cmp esp,ebx
  sete al
  pop ebx
end;

{ Get8 into p^, called by that convention; false if the stack pointer was not
  restored }
function CallGet8(p: pointer): boolean; assembler; nostackframe;
asm
  push ebx
  push esi
  mov esi,eax
  mov ebx,esp
  call Get8
  mov [esi],eax
  mov [esi+4],edx
  cmp esp,ebx
  sete al
  pop esi
  pop ebx
end;
{$endif win32}

var
  r1: TR1;
  r2: TR2;
  r3: TR3;
  r4: TR4;
  r5: TR5;
  r6: TR6;
  r7: TR7;
  r8: TR8;
  r3m: TR3m;
  f3: TF3;
  f5: TF5;
  f6: TF6;
  f7: TF7;
{$ifdef win32}
  sp: pointer;
{$endif win32}
begin
  r1:=Get1;
  Check(r1,sizeof(r1),1);
  r2:=Get2;
  Check(r2,sizeof(r2),2);
  r3:=Get3;
  Check(r3,sizeof(r3),3);
  r4:=Get4;
  Check(r4,sizeof(r4),4);
  r5:=Get5;
  Check(r5,sizeof(r5),5);
  r6:=Get6;
  Check(r6,sizeof(r6),6);
  r7:=Get7;
  Check(r7,sizeof(r7),7);
  r8:=Get8;
  Check(r8,sizeof(r8),8);
  r3m:=Get3m;
  if (r3m.w<>$0201) or (r3m.c<>3) then
    halt(9);
  f3:=@Get3;
  f5:=@Get5;
  f6:=@Get6;
  f7:=@Get7;
  FillChar(r3,sizeof(r3),0);
  r3:=f3();
  Check(r3,sizeof(r3),13);
  FillChar(r5,sizeof(r5),0);
  r5:=f5();
  Check(r5,sizeof(r5),15);
  FillChar(r6,sizeof(r6),0);
  r6:=f6();
  Check(r6,sizeof(r6),16);
  FillChar(r7,sizeof(r7),0);
  r7:=f7();
  Check(r7,sizeof(r7),17);
  if Get3.b[2]<>3 then
    halt(23);
  if Get5.b[4]<>5 then
    halt(25);
  if Get6.b[5]<>6 then
    halt(26);
  if Get7.b[6]<>7 then
    halt(27);
{$ifdef win32}
  sp:=GetEsp;
  FillChar(r1,sizeof(r1),0);
  r1:=Asm1(Magic);
  CheckFrom(r1,sizeof(r1),$21,30);
  FillChar(r2,sizeof(r2),0);
  r2:=Asm2(Magic);
  CheckFrom(r2,sizeof(r2),$31,31);
  FillChar(r4,sizeof(r4),0);
  r4:=Asm4(Magic);
  CheckFrom(r4,sizeof(r4),$41,32);
  FillChar(r8,sizeof(r8),0);
  r8:=Asm8(Magic);
  CheckFrom(r8,sizeof(r8),$11,33);
  FillChar(r3,sizeof(r3),0);
  r3:=Asm3(40);
  CheckFrom(r3,sizeof(r3),40,34);
  FillChar(r5,sizeof(r5),0);
  r5:=Asm5(50);
  CheckFrom(r5,sizeof(r5),50,35);
  FillChar(r6,sizeof(r6),0);
  r6:=Asm6(60);
  CheckFrom(r6,sizeof(r6),60,36);
  FillChar(r7,sizeof(r7),0);
  r7:=Asm7(70);
  CheckFrom(r7,sizeof(r7),70,37);
  Asm3(90);
  Asm7(90);
  if GetEsp<>sp then
    halt(38);
  FillChar(r3,sizeof(r3),0);
  if not CallGetArg3(@r3,100) then
    halt(39);
  CheckFrom(r3,sizeof(r3),100,40);
  FillChar(r8,sizeof(r8),0);
  if not CallGet8(@r8) then
    halt(41);
  Check(r8,sizeof(r8),42);
{$endif win32}
  writeln('ok');
end.
