{ %cpu=i386,x86_64 }
{ %target=win32,win64,linux }
{ The elements of a dynamic array typed constant are aligned as their type is
  aligned in a record, up to the size of the header they follow (reference
  count and high bound: two pointers), as in an array allocated at run time.
  The block holding such a constant (its header and its elements) used to be
  aligned only to a pointer, so on i386 a Double element could sit at 4 mod 8.

  Where each block ends up is the linker's decision, so whether this program
  shows a misaligned element without the fix depends on the layout: it did on
  i386 when it was written, and it does not on a layout that happens to align
  every block to 8 anyway. The alignment is bounded by the target's limit for
  constants, so this runs only on targets where that limit is at least 16. }
program tb0733;

{$mode objfpc}

type
  TDoubleField = record
    b: byte;
    d: double;
  end;
  TInt64Field = record
    b: byte;
    i: int64;
  end;
  { aligned beyond the header on 32-bit targets }
  TAligned = record
    x: longint;
  end align 16;

const
  D1: array of double = (1.5, 2.5);
  B1: array of byte = (1);
  D2: array of double = (3.5, 4.5);
  B2: array of byte = (2);
  I1: array of int64 = (5, 6);
  B3: array of byte = (3);
  I2: array of int64 = (7, 8);
  B4: array of byte = (4);
  DA: array of TAligned = ((x: 9), (x: 10));
  { the control: a static array of the type is aligned as the type }
  SA: array[0..1] of TAligned = ((x: 11), (x: 12));

var
  df: TDoubleField;
  inf: TInt64Field;
  dalign, ialign, aalign: ptruint;

procedure check(p: pointer; align: ptruint; code: longint);
begin
  if (ptruint(p) mod align)<>0 then
    begin
      writeln('element at $',hexstr(p),' is not aligned to ',align);
      halt(code);
    end;
end;

begin
  dalign:=ptruint(@df.d)-ptruint(@df);
  ialign:=ptruint(@inf.i)-ptruint(@inf);
  check(@D1[0],dalign,1);
  check(@D2[0],dalign,2);
  check(@I1[0],ialign,3);
  check(@I2[0],ialign,4);
  check(@SA[0],16,6);
  aalign:=2*sizeof(pointer);
  if aalign>16 then
    aalign:=16;
  check(@DA[0],aalign,7);
  if (D1[0]<>1.5) or (D1[1]<>2.5) or (D2[0]<>3.5) or (D2[1]<>4.5) or
     (I1[0]<>5) or (I1[1]<>6) or (I2[0]<>7) or (I2[1]<>8) or
     (B1[0]<>1) or (B2[0]<>2) or (B3[0]<>3) or (B4[0]<>4) or
     (DA[0].x<>9) or (DA[1].x<>10) or (SA[1].x<>12) then
    halt(5);
  writeln('ok');
end.
