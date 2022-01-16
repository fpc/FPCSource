{
    This file is part of the Free Component Library

    Minimal server program for JSON-RPC server.

    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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

