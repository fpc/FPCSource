{%CPU=i386}
{%opt=-Cg-}

program test_word_ref;

var
  j,loc : word;
{$asmmode att}
procedure TestAtt(w : word);

begin
  asm
      movw w,%ax
      movw %ax,loc
  end;
end;

{$asmmode intel}
procedure TestIntel(w : word);

begin
  loc:=56*j;
  asm
      mov ax,w
      mov [loc],ax
  end;
end;

begin
  j:=6;
  TestAtt(6);
  if loc<>6 then
    begin
      Writeln('Error in att code');
      halt(1);
    end;
  TestIntel(46);
  if loc<>46 then
    begin
      Writeln('Error in intel code');
      halt(1);
    end;
end.
