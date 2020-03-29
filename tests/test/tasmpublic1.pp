{ %CPU=i386,x86_64 }
program tasmpublic1;

{$goto on}
{$asmmode intel}

label
  test_gLoBaL_label;

var
  codeseg_var: LongWord; external name 'test_gLoBaL_label';

begin
  asm
    public test_gLoBaL_label
    jmp @@skip
    db 'some garbage here'
test_gLoBaL_label:
    nop
    nop
    nop
    nop
@@skip:
  end;
  if codeseg_var<>$90909090 then
  begin
    Writeln('Error!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
