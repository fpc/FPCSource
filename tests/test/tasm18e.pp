{ %FAIL }
{ %CPU=i8086,i386,x86_64 }
program tasm18e;

{$ifdef FPC}
  {$asmmode intel}
{$else}
  {$define CPUI8086}
{$endif FPC}

const
  cval = 1;

type
  foo2 = packed record
    b1: byte;
    b2: byte;
  end;
  foo = packed record
    bb1: byte;
    fb2: foo2;
  end;

begin
  asm
    { this produces an error in TP7, while 
        test [di + 1*foo.fb2], cval
      doesn't... go figure :) }
{$ifdef CPUI8086}
    test [di + 1*foo.fb2], cval
{$endif}
{$ifdef CPUI386}
    test [edi + 1*foo.fb2], cval
{$endif}
{$ifdef CPUX86_64}
    test [rdi + 1*foo.fb2], cval
{$endif}
  end;
end.
