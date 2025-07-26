program tmultilinestring4;

{$modeswitch MultiLineStrings}

var MyStringV: WideString =
`
Hey
Hey
Hey
`;

const MyStringC: WideString =
`
Hey
Hey
Hey
`;

begin
  Write(MyStringV);
  Write(MyStringC);
end.
