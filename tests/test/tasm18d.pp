{ %FAIL }
{ %CPU=i8086,i386,x86_64 }
program tasm18d;

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

begin
  asm
    { this produces an error in TP7, while 
        test [di + foo2*5], cval
      doesn't... go figure :) }
{$ifdef CPUI8086}
    test [di + 5*foo2], cval
{$endif}
{$ifdef CPUI386}
    test [edi + 5*foo2], cval
{$endif}
{$ifdef CPUX86_64}
    test [rdi + 5*foo2], cval
{$endif}
  end;
end.
