{ %fail }

var
  { This shouldn't be allowed, the number of elements
    doesn't fit in a aint }
  mem  : array[0..high(aint)] of byte ;

begin
end;

