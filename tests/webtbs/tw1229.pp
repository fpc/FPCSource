{ %CPU=i386 }
{ Source provided for Free Pascal Bug Report 1229 }
{ Submitted by "Rich Pasco" on  2000-11-10 }
{ e-mail: pasco@acm.org }

{$asmmode intel }

procedure SomePostScript; assembler;nostackframe;
  asm
    db '/pop2 { pop pop } def',0;
  end;
var
  st : string;
begin
  WriteLn(pchar(@SomePostScript));
  st:=strpas(pchar(@SomePostScript));
  if st<>'/pop2 { pop pop } def' then
    begin
      Writeln('Error in assembler parsing');
      if st='/pop2  def' then
        Writeln('Assembler parser removes comments');
      Halt(1);
    end;
end.
