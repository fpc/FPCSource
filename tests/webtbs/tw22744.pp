{$mode objfpc}

uses
  sysutils;
var
  i,j,l : longint;
const
  exception_seen : boolean = false;

begin
  {$Q+}
  i:=$78000000;
  j:=$20000000;
  l:=i-j;
  {$push} {$q-}
  l:=i+j; {$pop}
  try
  {$push} {$q-}
  l:=i+j{$pop};
  except on E : Exception do
    begin
      writeln('Simple {$Pop} exception ',E.Message);
      exception_seen:=true;
    end;
  end;
  try
  {$q-} {$push}
  l:=i+j{$q+}{$push};
  l:=0;
  {$pop}
  {$pop}
  except on E : Exception do
    begin
      writeln('Convoluted {$Q+}{$Push} Exception ',E.Message);
      exception_seen:=true;
    end;
  end;
  if exception_seen then
    begin
      writeln('This test failed');
      halt(1);
    end;
end.


