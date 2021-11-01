{ %fail }
{$legacyifend on}

program test;
{$ifdef asdf}
{$else}
{$ifend}

begin
end.
