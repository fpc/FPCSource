{$mode iso}
program test(output);

label 99;

type byte = 0..255;

var f: file of byte;
    b: byte;
    i: integer;

begin

   rewrite(f);
   for b := 1 to 10 do write(f, b);
   reset(f);
   for i := 1 to 10 do begin

      if eof(f) then begin

         writeln('End of file');
         goto 99

      end;
      read(f, b);
      write(b:1, ' ')

   end;
   99:
   if b<>10 then
     halt(1);
   write;
   writeln('ok');
end.

