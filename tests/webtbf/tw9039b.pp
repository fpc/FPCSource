{ %fail }

{ the reason this compiles is that tdef.size and tabstractvarsym.getsize }
{ both return an aint, and then the size of ta is reported as low(aint)  }

type
  ta = array[0..high(ptrint)] of byte;
  tr = bitpacked record
    a: byte;
    case byte of
      0: (l: longint);
      1: (e: ta);
  end;

begin
  writeln(sizeof(ta));
end.
