program md2latex;

uses
  classes,
  markdown.utils,
  markdown.elements,
  markdown.scanner,
  markdown.parser,
  markdown.inlinetext,
  markdown.latexrender,
  markdown.processors,
  markdown.render
  ;

var
  Source,Dest : TStringList;
  Doc : TMarkDownDocument;

begin
  Dest:=Nil;
  Source:=TStringList.Create;
  try
    Dest:=TStringList.Create;
    Source.LoadFromFile(ParamStr(1));
    Doc:=TMarkDownParser.FastParse(Source,[]);
    With TMarkDownLatexRenderer.Create(Nil) do
      begin
      Options:=[loEnvelope];
      If ParamStr(2)='' then
        Writeln(RenderLaTeX(Doc))
      else
        begin
        Dest:=TStringList.Create;
        RenderDocument(Doc,Dest);
        Dest.SaveToFile(ParamStr(2));
        end;
      end;
  finally
    Source.Free;
    Dest.Free;
  end;
end.

