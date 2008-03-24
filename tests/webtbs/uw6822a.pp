unit uw6822a;
{$mode objfpc}{$H+}

interface

implementation

initialization
  writeln('Unit 1');
  writeln('initialization');
finalization
  writeln('Unit 1'); // problem
  writeln('finalization'); 
end.
