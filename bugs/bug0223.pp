
var a:string;

begin
  writeln('B:'='B:');            { debbuger evaluates this to FALSE }

  a:='A:';
  inc(a[1]);
  writeln(a='B:');               { TRUE }
end.
