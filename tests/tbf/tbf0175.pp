{ this will just give out an error }
{$asmmode att}
{$R+}
var
  w : word;
begin
  asm
        movl w,%ecx
  end;
end.