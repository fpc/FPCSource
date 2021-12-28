program demorpcrtti;

{$mode objfpc}{$H+}
{$if not defined(CPU386) and not defined(WIN64)}
{$define useffi}
{$endif}

uses
  fphttpapp, rpcapi, dmRPC {$ifdef useffi}, ffi.manager	{$endif}, myapi;

begin
  Application.Title:='FPC JSON-RPC using RTTI';
  Application.Port:=8080;
  Application.Initialize;
  Application.Run;
end.

