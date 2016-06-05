program test1;

{$mode objfpc}{$H+}

uses
  OpenSSL;

begin
  if InitSSLInterface then
    Writeln('Success')
  else
    Writeln('Load failed, missing functions: ',OpenSSL_unavailable_functions);
end.

