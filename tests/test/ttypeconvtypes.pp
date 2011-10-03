{%norun}
program ttypeconvtypes;

// check that type conversions work in type declarations
// also check that codepage ansistring declaration also works

type
  t866 = type AnsiString(866);
  tbytes = byte(0)..byte(15);
  tchars = ansichar(tbytes(0))..ansichar(tbytes(15));
begin
end.