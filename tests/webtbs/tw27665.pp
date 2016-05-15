{$mode objfpc}

      function UTF8CodePointLength(firstbyte: byte): SizeInt;
      var
        firstzerobit: SizeInt;
      begin
        result:=1;
        { bsr searches for the leftmost 1 bit. We are interested in the
          leftmost 0 bit, so first invert the value
        }
        firstzerobit:=BsrByte(not(firstbyte));
        { if there is no zero bit or the first zero bit is the rightmost bit
          (bit 0), this is an invalid UTF-8 byte ($ff cannot appear in an
          UTF-8-encoded string, and in the worst case bit 1 has to be zero)
        }
        if (firstzerobit=0) or (firstzerobit=255)  then
          exit;
        { the number of bytes belonging to this code point is
          7-(pos first 0-bit).
        }
        result:=7-firstzerobit;
      end;


begin
  writeln(UTF8CodePointLength(ord(' ')));
end.
