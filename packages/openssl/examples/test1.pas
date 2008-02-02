program test1;

{$mode objfpc}{$H+}

uses
  OpenSSL;

begin
  if InitSSLInterface then
    Writeln('Success')
  else
    Writeln('Holy shit!');
end.

