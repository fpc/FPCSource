program testpassrc;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cwstring,
{$ENDIF}
  Classes,
  syntax.highlighter,
  syntax.pascal,
  syntax.bash,
  syntax.javascript,
  syntax.html,
  syntax.css,
  syntax.ini,
  syntax.sql,
  syntax.json,
  syntax.go,
  syntax.table,
  syntax.c,
  syntax.cpp,
  syntax.csharp,
  syntax.java,
  syntax.kotlin,
  syntax.swift,
  syntax.rust,
  syntax.zig,
  syntax.d,
  syntax.typescript,
  syntax.python,
  syntax.ruby,
  syntax.lua,
  syntax.php,
  syntax.perl,
  syntax.basic,
  syntax.yaml,
  syntax.toml,
  syntax.xml,
  syntax.diff,
  syntax.makefile,
  syntax.assembly,
  syntax.batch,
  syntax.powershell,
  syntax.dockerfile,
  syntax.fortran,
  syntax.ada,
  syntax.modula2,
  syntax.oberon,
  syntax.barye,
  syntax.markdown,
  syntax.rst,
  syntax.lfm,
  unittest.pascal,
  unittest.assembler,
  unittest.bash,
  unittest.javascript,
  unittest.css,
  unittest.html,
  unittest.ini,
  unittest.sql,
  unittest.json,
  unittest.go,
  unittest.htmlrender,
  unittest.syntaxfixture,
  unittest.c,
  unittest.cpp,
  unittest.csharp,
  unittest.java,
  unittest.kotlin,
  unittest.swift,
  unittest.rust,
  unittest.zig,
  unittest.d,
  unittest.typescript,
  unittest.python,
  unittest.ruby,
  unittest.lua,
  unittest.php,
  unittest.perl,
  unittest.basic,
  unittest.yaml,
  unittest.toml,
  unittest.xml,
  unittest.diff,
  unittest.makefile,
  unittest.assembly,
  unittest.batch,
  unittest.powershell,
  unittest.dockerfile,
  unittest.fortran,
  unittest.ada,
  unittest.modula2,
  unittest.oberon,
  unittest.barye,
  unittest.markdown,
  unittest.rst,
  unittest.lfm,
  unittest.tokenevents,
  unittest.wordtable,
  unittest.tokencategories,
  unittest.sharedtext,
  unittest.bytescanner,
  unittest.tablewords,
  consoletestrunner;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application:=TMyTestRunner.Create(nil);
  DefaultFormat:=fplain;
  DefaultRunAllTests:=True;
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
