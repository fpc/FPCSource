{ %NORUN }

program tw33839a;
{$mode objfpc} // change to mode Delphi and it works
uses uw33839;
begin
  testme(100);
  testme(1.0);
  specialize testme<String>('Test');
end.
