{ %fail }

var
  { This shouldn't be allowed, the number of elements
    doesn't fit in the address range  }
  mem  : array[0..high(ptruint)] of byte ;

begin
end;

