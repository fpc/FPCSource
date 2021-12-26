    type
      { bit types for bitfields                                                  }
      _62bits    = 0 .. $3FFFFFFFFFFFFFFF;
      _63bits    = 0 .. $7FFFFFFFFFFFFFFF;
      _64bits1    = 0 .. qword($8000000000000000);
      _64bits2    = -1 .. $7FFFFFFFFFFFFFFF;
      _64bits3    = -1 .. $7F00000000000000;

    var
      v2: bitpacked record
        f1: _62bits;
        f2: _63bits;
        f3: _64bits1;
        f4: _64bits2;
        f5: _64bits3;
      end;
     
    begin
      writeln('bitsizeof(_62bits): ',bitsizeof(v2.f1)); 
      writeln('bitsizeof(_63bits): ',bitsizeof(v2.f2)); 
      writeln('bitsizeof(_64bits1): ',bitsizeof(v2.f3)); 
      writeln('bitsizeof(_64bits2): ',bitsizeof(v2.f4)); 
      writeln('bitsizeof(_64bits3): ',bitsizeof(v2.f5)); 

{$ifdef cpu64}
      if bitsizeof(v2.f1)<>62 then
        halt(1);
      if bitsizeof(v2.f2)<>63 then
        halt(1);
{$endif}
      if bitsizeof(v2.f3)<>64 then
        halt(3);
      if bitsizeof(v2.f3)<>64 then
        halt(4);
      if bitsizeof(v2.f3)<>64 then
        halt(5);
    end.
     
