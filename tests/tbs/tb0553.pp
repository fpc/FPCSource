{$mode objfpc}
{$goto on}
var
  a : longint;
label
  g;

begin
  try
    a:=2;
  finally
    if a>1 then
      goto g;
    writeln('Error');
    g:
  end;
end.
