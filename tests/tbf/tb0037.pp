{ %FAIL }
{ Old file: tbf0175.pp }
{ Asm, mov word,%eax should not be allowed without casting emits a warning (or error with range checking enabled)  OK 0.99.11 (PM) }

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
