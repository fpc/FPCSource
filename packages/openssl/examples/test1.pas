program test1;

{$mode objfpc}{$H+}

uses
  SysUtils,OpenSSL;

Const
  Bools : Array[Boolean] of string = ('Failed','OK');

Var
  B : Boolean;

begin
  B:=InitSSLInterface(True);
  Writeln('Load ',Bools[B],', missing functions: ');
  if OpenSSL_unavailable_functions<>'' then
    Writeln(OpenSSL_unavailable_functions);
  if b then
    writeln('Version : ',OpenSSLGetVersion(0));

end.

