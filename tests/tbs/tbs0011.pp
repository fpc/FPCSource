{$message don't know how to make a test from bug0011 (PM)}
var
   vga : array[0..320*200-1] of byte;

procedure test(x,y : longint);

  begin
     vga[x+y mod 320]:=random(256);
     vga[x+y mod 320]:=random(256);
  end;

begin
end.

