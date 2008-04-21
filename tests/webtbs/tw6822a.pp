{ %norun }
library tw6822a;
{$mode objfpc}{$H+}

uses
  uw6822a;

begin
{$ifndef wince}
  writeln('hello from library');
{$endif wince}
end.
