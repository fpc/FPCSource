program cgigateway;

{$mode objfpc}{$H+}

uses
  fcgigate;

begin
  With Application do
    begin
      ConfigFileName:='';
      HostName:='127.0.0.1';
      Port:=2015;
      Initialize;
      Run;
    end;
end.
