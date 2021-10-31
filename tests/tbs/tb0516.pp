{$mode delphi}

type
  ta = (ea,eb);
  tb = (e1,e2);
  tr = record
    case a: byte of
      0..5: (l: longint);
      100..20: (c: cardinal);
  end;

begin
end.

