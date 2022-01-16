{ The original test is about $Q option only,
  ifor which we explicitly disabled $R.
  Here use both $Q and $R,
  as 64-bit CPU rather generate range check errors
  on that code. }

{$mode objfpc}

uses
  sysutils;
var
  i,j,l : longint;
const
  exception_seen : boolean = false;

begin
  {$Q+,R+}
  i:=$78000000;
  j:=$20000000;
  l:=i-j;
  {$push} {$q-,r-}
  l:=i+j; {$pop}
  try
  {$push} {$q-,r-}
  l:=i+j{$pop};
  except on E : Exception do
    begin
      writeln('Simple {$Pop} exception ',E.Message);
      exception_seen:=true;
    end;
  end;
  try
  {$q-,r-} {$push}
  l:=i+j{$q+,r+}{$push};
  l:=0;
  {$pop}
  {$pop}
  except on E : Exception do
    begin
      writeln('Convoluted {$Q+,R+}{$Push} Exception ',E.Message);
      exception_seen:=true;
    end;
  end;
  if exception_seen then
    begin
      writeln('This test failed');
      halt(1);
    end;
end.


