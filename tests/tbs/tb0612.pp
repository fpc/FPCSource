type
  trec1 = record
    l : longint;
    b : byte;
  end;
  prec1 = ^trec1;

  trec2 = packed record
    a1 : array[0..3] of byte;
    b : byte;
  end;
  prec2 = ^trec2;

begin
  if ptruint(@trec1(nil^).b)<>4 then
    halt(1);
  if ptruint(@prec1(nil)^.b)<>4 then
    halt(2);
  if ptruint(@trec2(nil^).b)<>4 then
    halt(3);
  if ptruint(@prec2(nil)^.b)<>4 then
    halt(4);
end.

