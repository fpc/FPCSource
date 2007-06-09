type
  ta = array[1..high(ptrint) div 8-1] of byte;
  tr = bitpacked record
    a: byte;
    case byte of
      0: (l: longint);
      1: (e: ta);
  end;

begin
  writeln(sizeof(ta));
end.
