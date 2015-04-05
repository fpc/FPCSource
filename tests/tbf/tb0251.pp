{ %fail }
procedure p;
  var
    { generate big local data structures which overflow the stack for sure }
    a1,a2,a3,a4,a5 : array[0..high(sizeint) div 4] of byte;
  begin
  end;

begin
end.

