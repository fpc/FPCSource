{ %fail }

{$goto on}
{$inline on}
procedure p; inline;
label x;
begin
    goto x
end;
begin
  p;
end.

