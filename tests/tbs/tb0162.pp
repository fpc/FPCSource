{ Old file: tbs0193.pp }
{ overflow checking for 8 and 16 bit operations wrong }

{$R-}
{$Q+}
var i: integer;
    b: byte;

begin
  i := 32767;
  i := i + 15;
  b := 255;
  b := b + 18;
  b := 255;
  b := b * 8;
  b := 255;
  b := b * 17
End.
