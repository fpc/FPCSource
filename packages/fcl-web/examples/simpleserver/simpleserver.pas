{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Sample HTTP server application

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

{ $DEFINE USEGNUTLS}
{ $DEFINE USEMICROHTTP} // Note, this must match what is defined in fpsimpleserver

program simpleserver;

{$IFDEF USEMICROHTTP}
{$UNDEF USEGNUTLS}
{$ENDIF}

uses
{$IFDEF UNIX}
  cwstring,
  cthreads,
{$ENDIF}
{$IFNDEF USEMICROHTTP}
{$ifdef USEGNUTLS}
  gnutlssockets,
{$else}
  opensslsockets,
{$endif}
{$ENDIF}
fpmkunit,
  fpsimpleserver;

Type
  THTTPApplication = Class(TFPSimpleServerApplication);

Var
  Application : THTTPApplication;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

