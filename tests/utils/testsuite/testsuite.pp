{
  Main entry for testsuite CGI/HTTP program.
}
{$mode objfpc}
{$h+}

{ $define httpserver}

program testsuite;

uses
  sysutils, httproute, fpweb, tshttp, tsconsts, tshtml, tsutils,
  {$ifdef httpserver}
  fphttpapp
  {$else}
  fpcgi
  {$endif}
  ;



begin
  if paramstr(0)<>'' then
    TestsuiteCGIURL:=TestsuiteURLPrefix+'cgi-bin/'+extractfilename(paramstr(0))
  else
    TestsuiteCGIURL:=TestsuiteURLPrefix+'cgi-bin/'+TestsuiteBin;

  HTTPRouter.RegisterRoute('*',rmAll,@HandleTestSuiteRequest,True);
  Application.Initialize;
  {$ifdef httpserver}
  Application.Port:=9090;
  {$else}
  IsCGI:=True;
  {$endif}
  Application.Run;
end.
