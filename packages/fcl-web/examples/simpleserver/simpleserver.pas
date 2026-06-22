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
  sysutils, types,
  fphttp2server,
  fpmkunit,
  fpsimpleserver;

Type
  { THTTPApplication adds opt-in HTTP/2 (-2/--http2) on top of the reusable
    fpsimpleserver unit. The unit itself stays HTTP/2-agnostic; the dependency on
    fphttp2server lives only here, in the binary. }
  THTTPApplication = Class(TFPSimpleServerApplication)
  Private
    FHTTP2 : Boolean;
    FH2Handler : TFPHTTP2Handler;
  Protected
    procedure GetValidOptions(out aShort: String; out aLong: TStringDynArray); override;
    procedure ProcessOptions; override;
    procedure ConfigureServer; override;
    procedure WriteOptions; override;
  Public
    destructor Destroy; override;
  end;

procedure THTTPApplication.GetValidOptions(out aShort: String; out aLong: TStringDynArray);
begin
  inherited GetValidOptions(aShort, aLong);
  aShort := aShort + '2';
  SetLength(aLong, Length(aLong)+1);
  aLong[High(aLong)] := 'http2';
end;

procedure THTTPApplication.ProcessOptions;
begin
  inherited ProcessOptions;
  FHTTP2 := HasOption('2','http2');
end;

procedure THTTPApplication.ConfigureServer;
begin
  inherited ConfigureServer;
  if not FHTTP2 then
    Exit;
  // Attach an HTTP/2 handler to the embedded server and activate it BEFORE the
  // accept loop starts. Cleartext serves prior-knowledge/h2c; with --ssl the
  // handler advertises ALPN 'h2,http/1.1' and serves negotiated h2 over TLS.
  FH2Handler := TFPHTTP2Handler.Create(Self);
  FH2Handler.WebServer := HTTPHandler.HTTPServer;
  FH2Handler.Active := True;
end;

procedure THTTPApplication.WriteOptions;
begin
  inherited WriteOptions;
  Writeln('-2 --http2            Enable HTTP/2 (cleartext prior-knowledge/h2c, or TLS-ALPN with --ssl).');
end;

destructor THTTPApplication.Destroy;
begin
  // Deactivate the handler (unregisters the seam, restores ALPN) before teardown.
  if Assigned(FH2Handler) then
    FH2Handler.Active := False;
  FreeAndNil(FH2Handler);
  inherited Destroy;
end;

Var
  Application : THTTPApplication;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
