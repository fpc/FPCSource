{$mode tp}
program notcom;

type demo=object
          constructor init;
          destructor done(x:longint);
          end;

constructor demo.init;
begin
end;

destructor demo.done(x:longint);
begin
end;

begin
end.
