const dirsep = '\';

begin
  if dirsep = '/'
    then
      begin
        writeln('bug!');
        Halt(1);
      end
    else
      writeln('ok');
end.
