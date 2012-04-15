{ %cpu=i386,x86_64 }
{ %norun }
{$goto on}
var
{$ifdef cpux86_64}
  a : array[0..31] of byte =
                             ($0F,$C1,$C6,$66,$0F,$C1,$C6,$0F,$C0,$D8,$0F,$C1,$34,$24,$66,$0F,$C1,$34,$24,
                              $0F,$C0,$04,$24,$48,$0F,$C1,$C6,$48,$0F,$C1,$04,$24);
{$else cpux86_64}
  a : array[0..22] of byte =
                             ($0f,$c1,$c6,$66,$0f,$c1,$c6,$0f,$c0,$d8,$0f,$c1,$34,$24,$66,$0f,$c1,$34,$24,
                              $0f,$c0,$04,$24);
{$endif cpux86_64}


var
  p : pointer;
  i : longint;
label
  l;
begin
l:
  asm
    xaddl %eax,%esi
    xaddw %ax,%si
    xaddb %bl,%al
{$ifdef cpux86_64}
    xaddl %esi,(%rsp)
    xaddw %si,(%rsp)
    xaddb %al,(%rsp)
    xaddq %rax,%rsi
    xaddq %rax,(%rsp)
{$else cpux86_64}
    xaddl %esi,(%esp)
    xaddw %si,(%esp)
    xaddb %al,(%esp)
{$endif cpux86_64}
  end;
  for i:=low(a) to high(a) do
    if pbyte(@l+i)^<>a[i] then
      halt(1);
  writeln('ok');
end.
