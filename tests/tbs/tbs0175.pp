{ this will just give out a warning }
{$asmmode att}
{$R-}
var
  w : word;
begin
  asm
        movl w,%ecx
  end;
end.