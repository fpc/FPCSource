program tmultilinestring6;

{$modeswitch MultiLineStrings}
{$MultiLineStringLineEnding CR}

const A =
`
😊
😊
😊
😊
😊
`;

{$MultiLineStringLineEnding CRLF}

const B =
`
😊
😊
😊
😊
😊
`;

{$MultiLineStringLineEnding LF}

const C =
`
😊
😊
😊
😊
😊
`;

{$MultiLineStringLineEnding PLATFORM}

const D =
`
😊
😊
😊
😊
😊
`;

{$MultiLineStringLineEnding SOURCE}

const E =
`
😊
😊
😊
😊
😊
`;

begin
  Write(A);
  Write(B);
  Write(C);
  Write(D);
  Write(E);
end.
