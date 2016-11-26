{ %cpu=aarch64 }

{$mode objfpc}

type
  trec = record
    a1,a2,a3,a4,a5,a6,a7,a8,a9,a10: double;
  end;
  prec = ^trec;

var
  r: trec;

procedure getdouble(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: double; r: prec);
assembler;
asm
  stur d0,[x0]
  add x0, x0, #8

  stur d1,[x0]
  add x0, x0, #8

  stur d2,[x0]
  add x0, x0, #8

  stur d3,[x0]
  add x0, x0, #8

  stur d4,[x0]
  add x0, x0, #8

  stur d5,[x0]
  add x0, x0, #8

  stur d6,[x0]
  add x0, x0, #8

  stur d7,[x0]
  add x0, x0, #8

  ldr  d0,[x29, #16]
  stur d0,[x0]
  add x0, x0, #8

  ldr  d0,[x29, #24]
  stur d0,[x0]
end;
       
begin
   getdouble(1,2,3,4,5,6,7,8,9,10,@r);

   if r.a1<>1.0 then
     halt(1);
   if r.a2<>2.0 then
     halt(2);
   if r.a3<>3.0 then
     halt(3);
   if r.a4<>4.0 then
     halt(4);
   if r.a5<>5.0 then
     halt(5);
   if r.a6<>6.0 then
     halt(6);
   if r.a7<>7.0 then
     halt(6);
   if r.a8<>8.0 then
     halt(8);
   if r.a9<>9.0 then
     halt(9);
   if r.a10<>10.0 then
     halt(10);
end.
