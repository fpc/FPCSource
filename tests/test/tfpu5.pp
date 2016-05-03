{ %CPU=i386 }
{ %OPT=-Cg- }
{ This test program deals with the
  the delicate problem of
  non commutative FPU instruction
  where the destination register
  is ST(1) to ST(7)

    Whereas Intel interprets
      fdiv st(1),st
    as
      st(1):=st(1) / st
    The ATT read
      fdiv %st,%st(1)
    as
      st(1):=st/st(1)
    Should be tested with
    different output styles :
    for go32v2
      -Aas -Acoff and -Anasmcoff
    for win32
      -Aas -Apecoff and -Anasmwin32
    for linux
      -Aas and -Anasmelf
    }

program  test_nasm_div;


var
  x,y,z : double;

begin
  x:=4;
  y:=2;
  Writeln('4/2=',x/y:0:2);
  if x/y <> 2.0 then
    Halt(1);
{$asmmode att}
  asm
    fldl y
    fldl x
    fdivp %st,%st(1)
    fstpl z
  end;
  Writeln('ATT result of 4/2=',z:0:2);
  if z <> 2.0 then
    Halt(1);
  asm
    fldl y
    fldl x
    fdiv %st(1),%st
    fstpl z
    fstp %st
  end;
  Writeln('ATT result of 4/2=',z:0:2);
  if z <> 2.0 then
    Halt(1);
  asm
    fldl y
    fldl x
    fdiv %st,%st(1)
    fstp %st
    fstpl z
  end;
  Writeln('ATT result of 4/2=',z:0:2);
  if z <> 2.0 then
    Halt(1);
  asm
    fldl y
    fldl x
    fadd
    fstpl z
  end;
  Writeln('ATT result of 4+2=',z:0:2);
  if z <> 6.0 then
    Halt(1);
{$asmmode intel}
  asm
    fld x
    fld y
    fdivp  st(1),st
    fstp z
  end;
  Writeln('Intel result of 4/2=',z:0:2);
  if z <> 2.0 then
    Halt(1);
  asm
    fld y
    fld x
    fdiv  st,st(1)
    fstp z
    fstp st
  end;
  Writeln('Intel result of 4/2=',z:0:2);
  if z <> 2.0 then
    Halt(1);
  asm
    fld y
    fld x
    fadd
    fstp z
  end;
  Writeln('Intel result of 4+2=',z:0:2);
  if z <> 6.0 then
    Halt(1);

  Writeln('All tests completed successfully!');
end.
