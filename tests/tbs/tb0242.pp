{ Old file: tbs0283.pp }
{ bugs in constant char comparison evaluation           OK 0.99.13 (PFV) }

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
