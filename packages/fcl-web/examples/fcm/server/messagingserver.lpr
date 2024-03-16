{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM (Firebase Cloud Messaging) - Demo program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program messagingserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cwstring, cthreads,
  {$ENDIF}
  fpwebfile, fpwebclient, opensslsockets, fphttpwebclient,
  fphttpapp, module.rpc, module.messaging;

begin
  DefaultWebClientClass:=TFPHTTPWebClient;
  TSimpleFileModule.BaseDir:='./';
  TSimpleFileModule.RegisterDefaultRoute;
  Application.Title:='FCM Send message demo';
  Application.Port:=8910;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

