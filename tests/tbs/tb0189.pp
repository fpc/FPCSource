{ Old file: tbs0223.pp }
{ wrong boolean evaluation in writeln                  OK 0.99.11 (PFV) }


uses
   erroru;

var a:string;

begin
  writeln('B:'='B:');            { debbuger evaluates this to FALSE }
  if 'B:'='B:' then
    writeln('OK')
  else
    error;
  a:='A:';
  inc(a[1]);
  writeln(a='B:');               { TRUE }
  if a='B:' then
    writeln('OK')
  else
    error;
end.
