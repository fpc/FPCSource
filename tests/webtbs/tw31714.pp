{$goto on}
procedure DoIt; inline;
{IDE crashes when procedure declared inline. It does not matter whether
goto is enabled or not}
var n: integer;
label l1;
begin
  n := 0;
  l1:
  if n < 100 then begin
    inc (n);
    goto l1;
  end;
  writeln (n);
end;

begin
  DoIt;
end.
