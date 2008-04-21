unit uw6822a;
{$mode objfpc}{$H+}

interface

implementation

var
  t: text;

initialization
{$ifndef wince}
  writeln('Unit 1');
  writeln('initialization');
{$endif wince}
finalization
{$ifndef wince}
  writeln('Unit 1'); // problem
  writeln('finalization'); 
{$endif wince}
  assign(t,'uw6822a.txt');
  rewrite(t);
  close(t);
end.
