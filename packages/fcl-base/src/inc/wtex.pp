{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wtex;

interface

{$ifdef fpc}
{$mode objfpc}
{$endif}


uses wformat,classes,sysutils;

Type
  TTexWriter=Class(TFormattingWriter)
    FCellCount : Integer;
  Protected
    Procedure IncCellCount;
    Property CellCount : Integer Read FCellCount Write FCellCount;
  Public
    Procedure ScopeStart;
    Procedure ScopeEnd;
    Procedure EnvironmentStart(Const Name,Opts : String);
    Procedure EnvironmentEnd(Const Name : String);
    Function EscapeText (AText : String) : String; override;
    Procedure DocumentStart(Const Title : String); override;
    Procedure DocumentEnd; override;
    Procedure HeaderStart(Alevel : Integer); override;
    Procedure HeaderEnd(Alevel : Integer); override;
    Procedure ParagraphEnd; override;
    Procedure LineBreak; override;
    Procedure Rule; override;
    Procedure BoldStart; override;
    Procedure BoldEnd;override;
    Procedure ItalicStart;override;
    Procedure ItalicEnd;override;
    Procedure UnderlineStart;override;
    Procedure UnderlineEnd;override;
    Procedure PreformatStart; override;
    Procedure PreformatEnd; override;
    Procedure TableStart( NoCols: Integer; Border : Boolean); override;
    Procedure TableEnd; override;
    Procedure RowStart; override;
    Procedure RowEnd; override;
    Procedure CellStart; override;
    Procedure HeaderCellStart; override;
    Procedure HeaderCellEnd; override;
    Procedure ListStart(ListType : TListType); override;
    Procedure ListEnd(ListType : TListType); override;
    Procedure ListItemStart; override;
    Procedure DefinitionItem(Const Aname,AText : String); override;

  end;

Const
  ListNames : Array[TListType] of string
            = ('enumerate','itemize','definition');

implementation

{ TTexWriter }

procedure TTexWriter.BoldEnd;
begin
  ScopeEnd;
end;

procedure TTexWriter.BoldStart;
begin
  dump('\textbf');
  ScopeStart;
end;

procedure TTexWriter.CellStart;
begin
  If CellCount<>0 then
    Dump('&');
  IncCellCount;
end;

procedure TTexWriter.DefinitionItem(const Aname, AText: String);
begin
  dump('\item[');
  Write(AName);
  Dump(']');
  Write(Atext);
end;

procedure TTexWriter.DocumentEnd;
begin
  dump('\end{document}')
end;

procedure TTexWriter.DocumentStart(const Title: String);
begin
  dumpln('\documentclass{report}');
  dumpln('\usepackage{a4}');
  dumpln('\begin{document}');
  dump('\title');
  ScopeStart;
  Write(Title);
  ScopeEnd;
end;

procedure TTexWriter.EnvironmentStart(const Name,opts: String);

begin
  Dump('\begin');
  If Opts<>'' then
    Dump(Opts);
  ScopeStart;
  Dump(Name);
  ScopeEnd;
end;

procedure TTexWriter.EnvironmentEnd(const Name: String);
begin
  Dump('\end');
  ScopeStart;
  Dump(Name);
  ScopeEnd;
end;

function TTexWriter.EscapeText(AText: String): String;
begin
  Result:=StringReplace(AText,'_','\_',[rfReplaceAll]);
end;

procedure TTexWriter.HeaderCellEnd;
begin
  CellEnd;
end;

procedure TTexWriter.HeaderCellStart;
begin
  CellStart;
end;

procedure TTexWriter.HeaderEnd(Alevel: Integer);
begin
  ScopeEnd;
  Dumpln('');
end;

procedure TTexWriter.HeaderStart(Alevel: Integer);

Const
  Headers : Array [0..4] of string =
    ('\part','\chapter','\section','\subsection','\subsubsection');

begin
  dump(Headers[Alevel]);
  ScopeStart;
end;

procedure TTexWriter.IncCellCount;
begin
  Inc(FCellCount);
end;

procedure TTexWriter.ItalicEnd;
begin
  ScopeEnd;
end;

procedure TTexWriter.ItalicStart;
begin
  dump('\textit');
  ScopeStart;
end;

procedure TTexWriter.LineBreak;
begin
  Dump('\\');
end;

procedure TTexWriter.ListEnd(ListType: TListType);
begin
  EnvironmentEnd(ListNames[ListType]);
end;

procedure TTexWriter.ListItemStart;
begin
  dump('\item');
end;

procedure TTexWriter.ListStart(ListType: TListType);
begin
  EnvironmentStart(ListNames[ListType],'');
end;

procedure TTexWriter.ParagraphEnd;
begin
  DumpLn(LineFeed+LineFeed);
end;

procedure TTexWriter.PreformatEnd;
begin
  EnvironmentEnd('verbatim')
end;

procedure TTexWriter.PreformatStart;
begin
  EnvironmentStart('verbatim','')
end;

procedure TTexWriter.RowEnd;
begin
  DumpLn('\\')
end;

procedure TTexWriter.RowStart;
begin
  FCellCount:=0;
end;

procedure TTexWriter.Rule;
begin
  dump('\hline');
end;

procedure TTexWriter.ScopeEnd;
begin
  Dump('}');
end;

procedure TTexWriter.ScopeStart;
begin
  Dump('{');
end;

procedure TTexWriter.TableStart(NoCols: Integer; Border: Boolean);
begin
//  EnvironmentStart('table','');
  EnvironmentStart('tabular','');
  ScopeStart;
  Dump(StringOfChar('l',NoCols));
  ScopeEnd;
  DumpLn('');
end;

procedure TTexWriter.TableEnd;
begin
  EnvironmentEnd('tabular');
//  EnvironmentEnd('table');
end;

procedure TTexWriter.UnderlineEnd;
begin
  ScopeEnd;
end;

procedure TTexWriter.UnderlineStart;
begin
  dump('\textul');
  ScopeStart;
end;

end.
