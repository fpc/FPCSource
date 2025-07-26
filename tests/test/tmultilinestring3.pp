program tmultilinestring3;

{$modeswitch MultiLineStrings}

var MyStringV: ShortString =
`
Hey
Hey
Hey
`;

const MyStringC: ShortString =
`
Hey
Hey
Hey
`;

begin
  Write(MyStringV);
  Write(MyStringC);
end.
