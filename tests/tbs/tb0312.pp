{ show a problem with IOCHECK !!
  inside reset(file)
  we call reset(file,longint)
  but we also emit a call to iocheck after and this is wrong !!  PM }
program getret;

  var
      ppfile : file;

begin
{$ifndef macos}
       assign(ppfile,'this_file_probably_does_not_exist&~"#');
{$else}
       {Max 32 chars in macos fielnames}
       assign(ppfile,'this_file_probably_&~"#');
{$endif}

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
