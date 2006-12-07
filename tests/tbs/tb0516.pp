{$mode delphi}

type
  ta = (ea,eb);
  tb = (e1,e2);
  tr = record
    case a: byte of
      -1,'c'..ea,e2..-5: (l: longint);
      'b'..eb,-1,-1,-1..-38,-100..e2: (c: cardinal);
  end;

begin
end.

