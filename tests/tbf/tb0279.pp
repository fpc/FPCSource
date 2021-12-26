{ %fail }
{$legacyifend on}

program test;
{$if defined(asdf)}
{$else}
{$endif}

begin
end.
