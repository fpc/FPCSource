program tmultilinestring2;

{$modeswitch MultiLineStrings}

var MyStringV: AnsiString =
`
Hey
Hey
Hey
`;

const MyStringC: AnsiString =
`
Hey
Hey
Hey
`;

begin
  Write(MyStringV);
  Write(MyStringC);
end.
