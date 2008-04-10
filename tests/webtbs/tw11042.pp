{ %cpu=arm }
{ %norun }

TYPE
  ttest = record
    a : shortstring;
    b : dword;
  end;
VAR
  q : ttest;
begin
  asm
    ldr r0,[r1,r2,lsl #3]
    ldr r0,[r1]
    ldr r0,[r1, r2]
    ldr r0,[r1, -r2]
    ldr r0,[r1, r2, lsl #23]
    ldr	r0,[r1, -r2, lsl #23]
    ldr	r0,[r1, #4095]
    ldr	r0,[r1, #-4095]
    ldr	r0,[r1, r2]!
    ldr	r0,[r1, -r2]!
    ldr	r0,[r1, r2, lsl #23]!
    ldr	r0,[r1, -r2, lsl #23]!
    ldr	r0,[r1, #4095]!
    ldr	r0,[r1, #-4095]!
    ldr	r0,[r1], r2
    ldr	r0,[r1], -r2
    ldr	r0,[r1], r2, lsl #23
    ldr	r0,[r1], -r2, lsl #23
    ldr	r0,[r1], #4095
    ldr r0,[r1], #-4095

    ldr r0,[r1,q.b]

.Ltest:
    ldr r0,[.Ltest]
  end;
end.
