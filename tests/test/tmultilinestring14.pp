program tmultilinestring14;

{$modeswitch MultiLineStrings}

{$MultiLineStringTrimLeft 2}

const A = `
  A
  B
  C
  D
`;

{$MultiLineStringTrimLeft 4}

const B = `
    A
    B
    C
    D
`;

begin
  Write(A);
  Write(B);

  { The number-to-trim being larger, (even much larger) than the amount of whitespace is not a problem,
    as it stops immediately when it is no longer actually *in* whitespace regardless. }

  {$MultiLineStringTrimLeft 10000}

  { Non-leading whitespace is preserved properly, of course. }

  Write(`
        sdfs
        sd fs fs
        sd  fsfs  sdfd sfdf
        sdfs fsd
  `);
end.
