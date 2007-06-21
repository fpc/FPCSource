var
  outf : file of byte;
  w : word;
begin
   assign(outf, 'tb0466.tmp');
   rewrite(outf);
   {only explicit typecasting helps: byte(10)}
   write(outf, 10);
   w:=20;
   write(outf, w);
   close(outf);
   erase(outf);
end.
