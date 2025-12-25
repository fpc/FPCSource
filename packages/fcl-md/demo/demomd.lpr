program demomd;

uses
  classes,
  markdown.utils,
  markdown.elements,
  markdown.scanner,
  markdown.parser,
  markdown.inlinetext,
  markdown.htmlrender,
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
    With TMarkDownHTMLRenderer.Create(Nil) do
      begin
      If ParamStr(2)='' then
        Writeln(RenderHTML(Doc))
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

