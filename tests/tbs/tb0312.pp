{ show a problem with IOCHECK !!
  inside reset(file)
  we call reset(file,longint)
  but we also emit a call to iocheck after and this is wrong !!  PM }

program getret;

 uses dos;

  var
      ppfile : file;

begin
       assign(ppfile,'this_file_probably_does_not_exist&~"#');
{$I-}
       reset(ppfile,1);
       if ioresult=0 then
         begin
{$I+}
            close(ppfile);
         end
       else
         writeln('the file does not exist') ;
{$I-}
       reset(ppfile);
       if ioresult=0 then
         begin
{$I+}
            close(ppfile);
         end
       else
         writeln('the file does not exist') ;
end.
