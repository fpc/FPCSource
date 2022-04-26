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
unit pkgfphttp;

interface

uses Classes,pkgdownload,pkgoptions,fprepos;

Type

  { TFPHTTPDownloader }

  TFPHTTPDownloader = Class(TBaseDownloader)
  Protected
    function HTTPDownload(Const URL : String; Dest : TStream): Boolean; override;
 end;

implementation

uses
  sysutils,fphttpclient, pkgglobals, pkgmessages;

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
