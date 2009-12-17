{%CPU=i386}


program test_word_ref;

var
  loc : word;
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
  asm
      mov ax,[w]
      mov [loc],ax
  end;
end;

begin
  TestAtt(6);
  if loc<>6 then
    halt(1);
  TestIntel(46);
  if loc<>46 then
    halt(1);
end.
