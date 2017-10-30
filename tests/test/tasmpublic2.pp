{ %CPU=i8086 }
program tasmpublic2;

{$goto on}
{$asmmode intel}

label
  test_gLoBaL_label;

var
  codeseg_var: Word; external name 'test_gLoBaL_label';
  v: Word;

begin
  asm
    public test_gLoBaL_label
    jmp @@skip
    db 'some garbage here'
test_gLoBaL_label:
    dw 1234h
@@skip:
    mov ax, cs:[codeseg_var]
    mov v, ax
  end;
  if v<>$1234 then
  begin
    Writeln('Error!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
