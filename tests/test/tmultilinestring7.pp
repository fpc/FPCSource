{ %FAIL }

program tmultilinestring7;

{$modeswitch MultiLineStrings}
{$MultiLineStringLineEnding DOESNOTEXIST}

const Blah =
`
A
B
C
`;

begin
  Write(Blah);
end.
