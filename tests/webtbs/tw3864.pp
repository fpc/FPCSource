{ Source provided for Free Pascal Bug Report 3864 }
{ Submitted by "Jernej" on  2005-04-01 }
{ e-mail: jernejcoder@gmail.com }
const
 rasteriso: array[0..2] of byte = // this is WAY too big
 (
   $00, $00, $00
 );

procedure glBitMap(const pb:pbyte);cdecl;
begin
end;

var
  i : longint;
begin
   i:=0;
   glBitmap(@rasteriso[(255 - i) * 16]); // Add a character to the current Display list
end.
