{ %CPU=i386 }
{ Old file: tbs0316.pp }
{  }

{$asmmode intel}

procedure test(b : longint); assembler;
type
    splitlong = packed record b1, b2, b3, b4 : Byte; end;
asm
    mov splitlong(b).b2, al
end;

{$asmmode att}

procedure test2(b : longint); assembler;
type
    splitlong = packed record b1, b2, b3, b4 : Byte; end;
asm
    movb splitlong(b).b2, %al
end;

begin
end.
