{ %FAIL }

program tmultilinestring21;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 2000000}

const S = `
  A
  B
  C
`;

begin
  WriteLn(S);
end.
