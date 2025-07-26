program tmultilinestring13;

{$modeswitch MultiLineStrings}

const A =
`
``a``
`;

const B =
`
'a'
`;

begin
  Write(A);
  Write(B);
end.
