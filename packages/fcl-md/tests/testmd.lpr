program testmd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  Classes, consoletestrunner, utest.markdown.utils, markdown.elements, Markdown.HTMLEntities,
  markdown.htmlrender, markdown.inlinetext, markdown.line, markdown.parser, markdown.render, markdown.scanner,
  markdown.utils, utest.markdown.scanner, utest.markdown.inlinetext, utest.markdown.htmlrender, utest.markdown.parser,
  utest.markdown.fpdocrender,markdown.latexrender,utest.markdown.latexrender,markdown.processors;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  with TTestRunner.Create(nil) do
    try
      Initialize;
      Title := 'FPCUnit console test runner';
      Run;
    finally
      Free;
    end;
end.
