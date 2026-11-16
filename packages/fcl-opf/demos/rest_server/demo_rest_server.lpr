{ demo_rest_server - REST API server backed by fcl-opf
  =====================================================
  A standalone console HTTP server that exposes the film library
  data model via a JSON REST API.

  Uses FPC's built-in fphttpserver + httproute for HTTP handling
  and fcl-opf for persistence.

  Usage:
    ./demo_rest_server [--port=NNNN] [--db=path/to/db.sqlite]

  Defaults:
    --port=9090
    --db=demo_rest_server.db

  Compile:
    fpc demo_rest_server.lpr -Fu../../src -Fu../film_library

  Endpoints (Films):
    GET    /films          - list all films (optional ?search=term)
    GET    /films/:id      - get film with directors, actors, genres, loans
    POST   /films          - create film from JSON body
    PUT    /films/:id      - update film from JSON body
    DELETE /films/:id      - delete film (cascades loans)
}
program demo_rest_server;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes,
  fphttpserver, httpdefs, httproute,
  rest.setup,
  rest.base,
  rest.films,
  rest.directors,
  rest.actors,
  rest.genres,
  rest.loans,
  rest.associations;

const
  DEFAULT_PORT = 9090;
  DEFAULT_DB   = 'demo_rest_server.db';

type

  { TRequestHandler - wraps the request handler as a method for OnRequest }

  TRequestHandler = class
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

var
  Server: TFPHTTPServer;
  Handler: TRequestHandler;
  Port: Word;
  DBFile: String;

procedure ParseArgs;
var
  I: Integer;
  Arg, Val: String;
begin
  Port := DEFAULT_PORT;
  DBFile := DEFAULT_DB;
  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if Copy(Arg, 1, 7) = '--port=' then
    begin
      Val := Copy(Arg, 8, Length(Arg));
      Port := StrToIntDef(Val, DEFAULT_PORT);
    end
    else if Copy(Arg, 1, 5) = '--db=' then
      DBFile := Copy(Arg, 6, Length(Arg))
    else if (Arg = '--help') or (Arg = '-h') then
    begin
      WriteLn('Usage: demo_rest_server [--port=NNNN] [--db=path.db]');
      WriteLn('  --port=NNNN   Listen port (default: ', DEFAULT_PORT, ')');
      WriteLn('  --db=path.db  SQLite database file (default: ', DEFAULT_DB, ')');
      Halt(0);
    end;
  end;
end;

procedure TRequestHandler.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  try
    HTTPRouter.RouteRequest(ARequest, AResponse);
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.ContentType := 'application/json';
      AResponse.Content := ErrorJSON('Internal server error: ' + E.Message);
    end;
  end;
  WriteLn(FormatDateTime('hh:nn:ss', Now), ' ',
    ARequest.Method, ' ', ARequest.URI, ' -> ', AResponse.Code);
end;

begin
  ParseArgs;

  WriteLn('Setting up database: ', DBFile);
  SetupDatabase(DBFile);
  try
    { Register routes for all entity types }
    RegisterFilmRoutes;
    RegisterDirectorRoutes;
    RegisterActorRoutes;
    RegisterGenreRoutes;
    RegisterLoanRoutes;
    RegisterAssociationRoutes;

    { Create and configure HTTP server }
    Server := TFPHTTPServer.Create(nil);
    try
      Server.Port := Port;
      Handler := TRequestHandler.Create;
      Server.OnRequest := @Handler.HandleRequest;

      WriteLn;
      WriteLn('Film Library REST API Server');
      WriteLn('============================');
      WriteLn('Listening on http://localhost:', Port, '/');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET    /films          - list all films (?search=term)');
      WriteLn('  GET    /films/:id      - get film with relations');
      WriteLn('  POST   /films          - create film');
      WriteLn('  PUT    /films/:id      - update film');
      WriteLn('  DELETE /films/:id      - delete film');
      WriteLn('  GET    /directors      - list all directors');
      WriteLn('  GET    /directors/:id  - get director');
      WriteLn('  POST   /directors      - create director');
      WriteLn('  PUT    /directors/:id  - update director');
      WriteLn('  DELETE /directors/:id  - delete director');
      WriteLn('  GET    /actors         - list all actors');
      WriteLn('  GET    /actors/:id     - get actor');
      WriteLn('  POST   /actors         - create actor');
      WriteLn('  PUT    /actors/:id     - update actor');
      WriteLn('  DELETE /actors/:id     - delete actor');
      WriteLn('  GET    /genres         - list all genres');
      WriteLn('  GET    /genres/:id     - get genre');
      WriteLn('  POST   /genres         - create genre');
      WriteLn('  PUT    /genres/:id     - update genre');
      WriteLn('  DELETE /genres/:id     - delete genre');
      WriteLn('  GET    /films/:id/loans          - list loans for film');
      WriteLn('  GET    /films/:id/loans/:id      - get loan');
      WriteLn('  POST   /films/:id/loans          - create loan for film');
      WriteLn('  PUT    /films/:id/loans/:id      - update loan');
      WriteLn('  DELETE /films/:id/loans/:id      - delete loan');
      WriteLn('  POST   /films/:id/directors/:id  - add director to film');
      WriteLn('  DELETE /films/:id/directors/:id  - remove director from film');
      WriteLn('  POST   /films/:id/actors/:id     - add actor to film');
      WriteLn('  DELETE /films/:id/actors/:id     - remove actor from film');
      WriteLn('  POST   /films/:id/genres/:id     - add genre to film');
      WriteLn('  DELETE /films/:id/genres/:id     - remove genre from film');
      WriteLn;
      WriteLn('Press Ctrl+C to stop.');
      WriteLn;

      Server.Active := True;
    finally
      Handler.Free;
      Server.Free;
    end;
  finally
    TeardownDatabase;
  end;
end.
