{ %maxversion=1.0.99 }

{ Old file: tbs0138.pp }
{ with problem, %esi can be crushed and is not restored OK 0.99.6 (PM) }

{program p; uncomment for a crash}
type
  tpt=^tpo;
  tpo=object
    constructor init;
    procedure pi1;
    procedure pi2;
  end;
constructor tpo.init;
begin
end;
procedure tpo.pi1;
begin
end;
procedure tpo.pi2;
begin
end;
procedure crushesi;assembler;
{$ifdef CPUI386}
asm
        movl %eax,%esi
end ['EAX','ESI'];
{$endif CPUI386}
{$ifdef CPU68K}
asm
        move.l d0,a5
end ['d0','a5'];
{$endif CPU68K}
{$ifdef CPUPOWERPC}
asm
  // doesn't matter, there is no static register used anymore for self,
  // and self is now loaded on-demand instead of always
  li r0,0
  li r3,0
  li r4,0
  li r5,0
  li r6,0
  li r7,0
  li r8,0
  li r9,0
  li r10,0
  li r11,0
  li r12,0
end;
{$endif CPUPOWERPC}
{$ifdef CPUARM}
asm
  // doesn't matter, there is no static register used anymore for self,
  // and self is now loaded on-demand instead of always
  mov r0,0
  mov r1,0
  mov r2,0
  mov r3,0
end;
{$endif CPUARM}


var
  p1 : tpt;
begin
  p1:=new(tpt,init);
  with p1^ do
   begin
     pi1;
     crushesi;  { After this the %esi should be reloaded from the tempvariable }
     pi1;
   end;
{ There is here already a tempvar for %esi, why not use it here too ? }
  p1^.pi2;
  p1^.pi2;
end.
