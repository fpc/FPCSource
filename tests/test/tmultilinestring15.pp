program tmultilinestring15;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 2}

const X = `
  Hello
  every
  body!
`;

const Y = `
    Goodbye
    every
    body!
    
`;

{ Test some wacky concatentation }

begin
  Write(X + Y);
  Write(Concat(X, Y));
  Write(
    '  Single line string ' +
    `
    and
    ` +
    `
    Multi
    line
    string
    ` +
    Y +
    X
  );
end.
