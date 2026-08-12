program md2ansi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring, termio,
  {$ENDIF}
  Classes, SysUtils,
  Markdown.Processors, Markdown.Parser, Markdown.Elements,
  Markdown.ANSIRender;

function StdoutIsTTY : Boolean;
begin
  {$IFDEF UNIX}
  Result:=IsATTY(stdout)<>0;
  {$ELSE}
  Result:=True;
  {$ENDIF}
end;

var
  lParser : TMarkDownParser;
  lDoc : TMarkdownDocument;
  lMarkDown : TStringList;
  lRenderer : TMarkDownANSIRenderer;
  i, lWidth : Integer;
  lFile, lArg : string;
  lColor : Boolean;

begin
  // Defaults: color on a terminal, unless NO_COLOR is set
  lColor:=StdoutIsTTY and (GetEnvironmentVariable('NO_COLOR')='');
  lFile:='';
  lWidth:=80;
  for i:=1 to ParamCount do
    begin
    lArg:=ParamStr(i);
    if lArg='--color' then
      lColor:=True
    else if lArg='--no-color' then
      lColor:=False
    else if lFile='' then
      lFile:=lArg
    else
      lWidth:=StrToIntDef(lArg,lWidth);
    end;
  if lFile='' then
    begin
    Writeln('Usage: md2ansi <file.md> [width] [--color|--no-color]');
    Halt(1);
    end;

  lParser:=Nil;
  lDoc:=Nil;
  lRenderer:=Nil;
  lMarkDown:=TStringList.Create;
  try
    lMarkDown.LoadFromFile(lFile);
    lParser:=TMarkDownParser.Create(Nil);
    lDoc:=lParser.Parse(lMarkDown);
    lRenderer:=TMarkDownANSIRenderer.Create(Nil);
    lRenderer.Width:=lWidth;
    lRenderer.UseColor:=lColor;
    lRenderer.Hyperlinks:=lColor;
    lRenderer.RenderDocument(lDoc);
    for i:=0 to lRenderer.Lines.Count-1 do
      Writeln(lRenderer.Lines[i]);
  finally
    lRenderer.Free;
    lDoc.Free;
    lParser.Free;
    lMarkDown.Free;
  end;
end.
