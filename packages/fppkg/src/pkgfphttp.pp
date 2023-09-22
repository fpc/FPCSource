{
    This file is part of the fppkg package manager
    Copyright (c) 1999-2022 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit pkgfphttp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Classes,FpPkg.Download,FpPkg.Options,FpPkg.Repos;
{$ELSE FPC_DOTTEDUNITS}
uses Classes,pkgdownload,pkgoptions,fprepos;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TFPHTTPDownloader }

  TFPHTTPDownloader = Class(TBaseDownloader)
  Protected
    function HTTPDownload(Const URL : String; Dest : TStream): Boolean; override;
 end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,FpWeb.Http.Client, FpPkg.Globals, FpPkg.Messages;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,fphttpclient, pkgglobals, pkgmessages;
{$ENDIF FPC_DOTTEDUNITS}

function TFPHTTPDownloader.HTTPDownload(Const URL: String; Dest: TStream): Boolean;

begin
  Result := False;
  With TFPHTTPClient.Create(Nil) do
    try
      AllowRedirect := True;
      Get(URL,Dest);
      Dest.Position:=0;
      Result := True;
    finally
      Free;
    end;
end;

initialization
  RegisterDownloader('FPC',TFPHTTPDownloader);
end.
