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
asm
        movl %eax,%esi
end ['EAX','ESI'];
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
