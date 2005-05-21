{ Old file: tbs0011.pp }
{  tests div/mod bugs, where edx is scrambled, if a called procedure does a div/mod                   OK 0.9.2 }

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
