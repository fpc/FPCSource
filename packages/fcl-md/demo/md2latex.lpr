program md2latex;

{$mode objfpc}
{$h+}

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
  Renderer: TMarkDownLatexRenderer;

begin
  Dest:=Nil;
  Renderer:=Nil;
  Source:=TStringList.Create;
  try
    Dest:=TStringList.Create;
    Source.LoadFromFile(ParamStr(1));
    Doc:=TMarkDownParser.FastParse(Source,[]);
    Renderer:=TMarkDownLatexRenderer.Create(Nil);
    With Renderer do
      begin 
      Options:=[loEnvelope];
      If ParamStr(2)='' then
        Writeln(RenderLaTeX(Doc))
      else
        begin
        RenderDocument(Doc,Dest);
        Dest.SaveToFile(ParamStr(2));
        end;
      end;
  finally
    Renderer.Free;
    Source.Free;
    Dest.Free;
  end;
end.

