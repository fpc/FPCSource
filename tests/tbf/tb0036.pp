{ %FAIL }
{ Old file: tbf0173.pp }
{ secondbugs is parsed as asm, but should be normal pascalcode OK 0.99.9 (PFV) }

var
  secondbug : word;
procedure p;assembler;
begin
  if secondbug=0 then;
end;

begin
end.
