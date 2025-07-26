program tmultilinestring5;

{$modeswitch MultiLineStrings}

var MyStringV: UnicodeString =
`
Hey
Hey
Hey
`;

const MyStringC: UnicodeString =
`
Hey
Hey
Hey
`;

begin
  Write(MyStringV);
  Write(MyStringC);
end.
