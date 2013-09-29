var
  t,t1, t2: text;
  s: string;
begin
  assign(t,'tw23725.txt');
  rewrite(t);
  writeln(t);
  close(t);
  assign(t1, 'tw23725.txt'); assign(t2, 'tw23725.txt');
  write('Opening 1... '); reset(t1); writeln('done.');
  write('Opening 2... '); reset(t2); writeln('done.');
  close(t2); close(t1);
end.
