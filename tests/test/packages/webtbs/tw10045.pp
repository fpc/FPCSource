
uses oldregexpr;

var
  engine : tRegexprEngine;
  source, dest : ansistring;
  count : longint;
begin
  if not GenerateRegExprEngine( 'foo', [], engine) then
  begin
    writeln( 'Failed to generate regex. engine.' );
    halt(1)
  end;

  source := 'foo bur a';
  count := RegExprReplaceAll(engine, source, '@', dest);
  if (count<>1) or (dest<>'@ bur a') then
    halt(1);

  source := 'xfoo bur a';
  count := RegExprReplaceAll(engine, source, '@', dest);
  if (count<>1) or (dest<>'x@ bur a') then
    halt(1);

  source := 'foo bur a';
  count := RegExprReplaceAll(engine, source, '@', dest);
  if (count<>1) or (dest<>'@ bur a') then
    halt(1);

  DestroyRegExprEngine( engine );
  writeln('ok');
end.
