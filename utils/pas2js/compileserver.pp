program compileserver;

{$mode objfpc}
{$h+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF} httpcompiler;


Var
  Application : THTTPCompilerApplication;

begin
  Application:=THTTPCompilerApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

