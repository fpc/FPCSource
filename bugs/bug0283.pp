const dirsep = '\';

begin
  if dirsep = '/'
    then writeln('bug!')
    else writeln('ok');
end.
