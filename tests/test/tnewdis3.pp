{%fail}
{$mode macpas}

type
  pr = ^tr;
  tr = record
    a: longint;
    case byte of
      1: (s: string);
      2: (w: word);
      3: (l: ansistring);
  end;

var
  p: pr;

begin
  dispose(p,6);
end.
