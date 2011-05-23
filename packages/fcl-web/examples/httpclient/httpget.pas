program httpget;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpclient;

var
  i : Integer;

begin
  if (ParamCount<>2) then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)), 'URL filename');
    Halt(1);
    end;
  With TFPHTTPClient.Create(Nil) do
    try
      Get(ParamStr(1),ParamStr(2));
      Writeln('Response headers:');
      For I:=0 to ResponseHeaders.Count-1 do
        Writeln(ResponseHeaders[i]);
    finally
      Free;
    end;
end.

