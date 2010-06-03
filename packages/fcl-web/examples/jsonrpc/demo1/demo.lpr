program demo;

{$mode objfpc}{$H+}

uses
  fpCGI, wmdemo;

// {$R *.res}


begin
  Application.Title:='FPC JSON-RPC demo';
  Application.Initialize;
  Application.Run;
end.

