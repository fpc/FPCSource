{ %FAIL }
{ Old file: tbf0155.pp }
{ Asm, Missing string return for asm functions }

{ this is not a real bug but rather a feature :
  assembler function are only accepted for
  simple return values
  i.e. either in register or FPU  (PM) }

{ so for the moment this is rejected code ! }

function asmstr:string;assembler;
asm
        movl    __RESULT,%edi
        movl    $0x4101,%al
        stosw
end;

begin
  writeln(asmstr);
end;
