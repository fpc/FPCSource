var
  b: boolean;
  w: word;

begin
  w := $2;
  b := (w shr 1) <> 0;
  w := w shl 1; // project1.lpr(7,3) Error: Asm: 16 or 32 bit references not supported
  if b then w := w xor 1;
  if w<>5 then
    halt(1);
end.
