program testmd;

{$mode objfpc}{$H+}

uses
  cwstring,Classes, consoletestrunner, utest.markdown.utils, markdown.elements, Markdown.HTMLEntities,
  markdown.htmlrender, markdown.inlinetext, markdown.line, markdown.parser, markdown.render, markdown.scanner,
  markdown.utils, utest.markdown.scanner, utest.markdown.inlinetext, utest.markdown.htmlrender, utest.markdown.parser,
  utest.markdown.fpdocrender,markdown.latexrender,utest.markdown.latexrender,markdown.processors;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
