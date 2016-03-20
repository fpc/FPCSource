{ %skiptarget=android }
{ %cpu=i386 }

program asm_bug;
{$IFDEF FPC}
{$mode delphi}
{$OPTIMIZATION SIZE}
{$OPTIMIZATION STACKFRAME}
{$OPTIMIZATION REGVAR}
{$CODEALIGN VARMIN=1}
{$CODEALIGN VARMAX=1}
{$CODEALIGN CONSTMIN=1}
{$CODEALIGN CONSTMAX=1}
{$PIC off}
{$ENDIF}

{$ALIGN 1}
{$APPTYPE CONSOLE}

{$IFDEF XXX}
             o  study of explicit/impicit operand size def
{$ENDIF}

type

obj1t = object  //__ fpc.exe eat "packed object" delphi 2007 does not eat _
b0 :byte;
b1 :byte;
b2 :byte;
b3 :byte;
procedure proc0();  register;
procedure proc1();  register;
procedure proc2();  register;
procedure proc3();  register;
procedure pascal(); register;
end;

var

obj1 :obj1t;
error: boolean;


procedure obj1t.proc0(); //___ it is seems to good but the last mov scrabble 3 byte after obj1 (with fpc.exe) _
ASM   //___________ proc1 __
  mov [eax].b0 , 0  //___ affable pascal like syntax but the "byte ptr" info not present (with DCC32(delphi) present)
  mov [eax].b1 , 1
  mov [eax].b2 , 2
  mov [eax].b3 , 3
end; //___________ proc1 __

procedure obj1t.proc1();
ASM   //___________ proc1 __
  mov [eax].b3 , 3  //___ reverse order to detect scrabbling _
  mov [eax].b2 , 2
  mov [eax].b1 , 1
  mov [eax].b0 , 0 //____ clear all four byte value with fpc __
end; //___________ proc1 __

procedure obj1t.proc2();
ASM   //___________ proc1 __
  mov [eax.b3] , 3     //___ this syntax preferable maybe _
  mov [eax.b2] , 2
  mov [eax.b1] , 1
  mov [eax.b0] , 0
end; //___________ proc1 __

procedure obj1t.proc3(); //___ naturally this proc work well _
ASM   //___________ proc1 __
  mov byte ptr [eax.b3] , 3     //___  _
  mov byte ptr [eax.b2] , 2
  mov byte ptr [eax.b1] , 1
  mov byte ptr [eax.b0] , 0
end; //___________ proc1 __


procedure obj1t.pascal();
begin //___________ pascal __
 b3:= 3;
 b2:= 2;
 b1:= 1;
 b0:= 0;
end; //___________ pascal __

type
str31 = string[31];
procedure wr_obj(e :str31);
begin //___________ wr_obj __
 with obj1 do writeln(b0:3,b1:3,b2:3,b3:3, '    must be:[0 1 2 3] ',e);  //___
 if (obj1.b0<>0) or
    (obj1.b1<>1) or
    (obj1.b2<>2) or
    (obj1.b3<>3) then
   error:=true;
end; //___________ wr_obj __

var
a1,a2   :ptruint;

var
c0,c1,c2,c3     :byte;  //___ the test with internal assemlber not fair because of 16-byte aligment of global variables _

begin  //____ m a i n _
  a1:= ptruint(@obj1.b0);
  a2:= ptruint(@obj1.b1);
  if ((a2-a1) <> 1) then begin writeln('obj1 not packed:'); halt(1); end;

  obj1.proc0();  wr_obj('fwd');
  obj1.proc1();  wr_obj('bwd');
  obj1.proc2();  wr_obj('bwd');
  obj1.proc3();  wr_obj('byte ptr');
  obj1.pascal(); wr_obj('pascal');

ASM
  lea  eax , c1
  sub  eax , offset c0
  mov  a1  , eax
  mov [c3] , 3 //___ there is "byte ptr" info present  _
  mov [c2] , 2
  mov [c1] , 1
  mov [c0] , 0
end ['eax'];
   writeln(c0:3,c1:3,c2:3,c3:3, '    must be:[0 1 2 3] glo');  //___
   if (c0<>0) or
      (c1<>1) or
      (c2<>2) or
      (c3<>3) then
     error:=true;
   if error then
     halt(1);
end.
