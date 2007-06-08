{ %opt=-gh }
{$mode macpas}

type
  pr = ^tr;
  tr = record
    a: longint;
    case byte of
      1: (s: string);
      2: (w: word);
  end;

var
  p: pr;

begin
  new(p,6);
  p^.a:=$1234578;
  p^.w:=$9abc;
  dispose(p,6);
end.
