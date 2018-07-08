program httpsearch;

// Undefine this to make a standalone HTTP server.
// The standalone HTTP server listens on port 3010,
// Change DefaultPort below to change this port.
{$define usecgi}

uses
{$ifdef usecgi}
  fpcgi,
{$else}
  fphttpapp,
{$endif}
  httpdefs, httproute, httpsearcher;

{$ifndef usecgi}
Const
  DefaultPort = 3010;
{$ENDIF}

Var
  aSearch : THTTPSearcher;

begin
  aSearch:=THTTPSearcher.Create(Application);
  HTTPRouter.RegisterRoute('/search',@aSearch.HTMLSearch,true);
  HTTPRouter.RegisterRoute('/list',@aSearch.WordList,False);
  {$ifndef usecgi}
  Application.Port:=DefaultPort;
  {$endif}
  Application.Initialize;
  Application.Run;
end.

