{ %NORUN }
{ %CPU=i386,x86_64 }

program tasm29;

{$mode objfpc}

type

  TTest = class
    procedure Test(aArg: Pointer); {$ifdef cpu386}register;{$endif} assembler; nostackframe;
  end;

procedure TTest.Test(aArg: Pointer); {$ifdef cpu386}register;{$endif} assembler; nostackframe;
asm
  movb $5, (__SELF, aArg)
  movb $5, (aArg, __SELF)
end;

begin

end.
