{ %cpu=i386 }
{ %fail }

program bug;
type
  br=bitpacked record	//Note! "bitpacked"
    l:0..31;
    m:0..31;
    h:0..63;
  end;
var

test:br;
{$asmmode att}
begin
  with test do
  begin
    l:=4;
    m:=8;
    l:=$f
  end;
  asm
    movb br.m,%al	//eax should be 4,but it is 32. Eight times. error!
    movb test.m,%al  //eax should be 8,but it is, a strange number. error!
  end;

  asm
    movb $br.m/8,%al		//OK, eax is 4 now, it is the right offset.
    movb test+br.m/8,%al	//OK, eax is 8 now, it is right, too.
  end;
end.

