{ %FAIL }
{ %CPU=i8086,i386,x86_64 }
program tasm18b;

{$ifdef FPC}
  {$asmmode intel}
{$else}
  {$define CPUI8086}
{$endif FPC}

const
  cval = 1;

type
  foo3 = packed record
    b1: byte;
    b2: byte;
    b3: byte;
  end;
  foo = packed record
    bb1: byte;
    fb3: foo3;
  end;

begin
  asm
    { this should produce an error, because foo3 is a 3-byte record and there's
      no explicit operand size specified (i.e. no 'byte ptr' or 'word ptr') }
{$ifdef CPUI8086}
    test [di + foo.fb3], cval
{$endif}
{$ifdef CPUI386}
    test [edi + foo.fb3], cval
{$endif}
{$ifdef CPUX86_64}
    test [rdi + foo.fb3], cval
{$endif}
  end;
end.
