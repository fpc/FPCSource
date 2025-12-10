{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2025 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    TMarkdownWriter - Markdown formatting writer implementation

    This unit provides TMarkdownWriter, a concrete implementation of
    TFormattingWriter that generates Markdown output. It supports:

    - Document structure (headers, paragraphs)
    - Text formatting (bold, italic, underline via HTML)
    - Tables with proper alignment
    - Lists (unordered, ordered, definition)
    - Preformatted code blocks
    - Character escaping for special markdown characters

    See demo_markdown.pp for usage examples.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit wmarkdown;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef fpc}
{$mode objfpc}
{$endif}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses Fcl.Wformat,System.Classes,System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses wformat,Classes,SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TMarkdownWriter=Class(TFormattingWriter)
  Private
    FTableCols: Integer;
    FTableHeaderWritten: Boolean;
    FInListItem: Boolean;
  Public
    Constructor Create (AStream : TStream); override;
    Function EscapeText (AText : String) : String; override;
    Procedure DocumentStart(Const Title : String); override;
    Procedure DocumentEnd; override;
    Procedure HeaderStart(Alevel : Integer); override;
    Procedure HeaderEnd(Alevel : Integer); override;
    Procedure ParagraphStart; override;
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
    Procedure CellEnd; override;
    Procedure HeaderCellStart; override;
    Procedure HeaderCellEnd; override;
    Procedure ListStart(ListType : TListType); override;
    Procedure ListEnd(ListType : TListType); override;
    Procedure ListItemStart; override;
    Procedure ListItemEnd; override;
    Procedure DefinitionItem(Const Aname,AText : String); override;
  end;

implementation

{ TMarkdownWriter }

constructor TMarkdownWriter.Create(AStream: TStream);
begin
  inherited;
  FTableCols := 0;
  FTableHeaderWritten := False;
  FInListItem := False;
end;

function TMarkdownWriter.EscapeText(AText: String): String;
begin
  // Only escape backslashes and characters that could break structure
  Result := StringReplace(AText, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '[', '\[', [rfReplaceAll]);
  Result := StringReplace(Result, ']', '\]', [rfReplaceAll]);
  // Don't escape * _ ` # as they might be intentional formatting
end;

procedure TMarkdownWriter.DocumentStart(const Title: String);
begin
  if Title <> '' then
  begin
    Dump('# ');
    Write(Title);
    DumpLn('');
    DumpLn('');
  end;
end;

procedure TMarkdownWriter.DocumentEnd;
begin
  // Markdown doesn't require explicit document ending
end;

procedure TMarkdownWriter.HeaderStart(Alevel: Integer);
var
  HeaderMarker: String;
  i: Integer;
begin
  HeaderMarker := '';
  for i := 1 to ALevel + 1 do
    HeaderMarker := HeaderMarker + '#';
  Dump(HeaderMarker + ' ');
end;

procedure TMarkdownWriter.HeaderEnd(Alevel: Integer);
begin
  DumpLn('');
  DumpLn('');
end;

procedure TMarkdownWriter.ParagraphStart;
begin
  // Markdown paragraphs don't need explicit start markers
end;

procedure TMarkdownWriter.ParagraphEnd;
begin
  DumpLn('');
  DumpLn('');
end;

procedure TMarkdownWriter.LineBreak;
begin
  DumpLn('  '); // Two spaces at end of a line creates a line break in markdown
end;

procedure TMarkdownWriter.Rule;
begin
  DumpLn('');
  DumpLn('---');
  DumpLn('');
end;

procedure TMarkdownWriter.BoldStart;
begin
  Dump('**');
end;

procedure TMarkdownWriter.BoldEnd;
begin
  Dump('**');
end;

procedure TMarkdownWriter.ItalicStart;
begin
  Dump('*');
end;

procedure TMarkdownWriter.ItalicEnd;
begin
  Dump('*');
end;

procedure TMarkdownWriter.UnderlineStart;
begin
  // Markdown doesn't have native underline, use HTML
  Dump('<u>');
end;

procedure TMarkdownWriter.UnderlineEnd;
begin
  Dump('</u>');
end;

procedure TMarkdownWriter.PreformatStart;
begin
  DumpLn('');
  DumpLn('```');
end;

procedure TMarkdownWriter.PreformatEnd;
begin
  DumpLn('```');
  DumpLn('');
end;

procedure TMarkdownWriter.TableStart(NoCols: Integer; Border: Boolean);
begin
  FTableCols := NoCols;
  FTableHeaderWritten := False;
  DumpLn('');
end;

procedure TMarkdownWriter.TableEnd;
begin
  DumpLn('');
end;

procedure TMarkdownWriter.RowStart;
begin
  Dump('|');
end;

procedure TMarkdownWriter.RowEnd;
var
  i: Integer;
begin
  DumpLn('');

  // After header row, add separator row
  if not FTableHeaderWritten then
  begin
    FTableHeaderWritten := True;
    Dump('|');
    for i := 1 to FTableCols do
      Dump('---|');
    DumpLn('');
  end;
end;

procedure TMarkdownWriter.CellStart;
begin
  Dump(' ');
end;

procedure TMarkdownWriter.CellEnd;
begin
  Dump(' |');
end;

procedure TMarkdownWriter.HeaderCellStart;
begin
  CellStart;
end;

procedure TMarkdownWriter.HeaderCellEnd;
begin
  CellEnd;
end;

procedure TMarkdownWriter.ListStart(ListType: TListType);
begin
  DumpLn('');
end;

procedure TMarkdownWriter.ListEnd(ListType: TListType);
begin
  DumpLn('');
end;

procedure TMarkdownWriter.ListItemStart;
begin
  FInListItem := True;
  Dump('- '); // Use dash for all list types for simplicity
end;

procedure TMarkdownWriter.ListItemEnd;
begin
  FInListItem := False;
  DumpLn('');
end;

procedure TMarkdownWriter.DefinitionItem(const Aname, AText: String);
begin
  Write(AName);
  DumpLn('');
  Dump(': ');
  Write(AText);
  DumpLn('');
end;

end.