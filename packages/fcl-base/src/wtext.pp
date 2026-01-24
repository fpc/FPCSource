{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    TTextWriter - Plain text formatting writer implementation

    This unit provides TTextWriter, a concrete implementation of
    TFormattingWriter that generates plain text output with line wrapping.
    It supports:

    - Configurable maximum line length (MaxLineLength property)
    - Automatic text wrapping at word boundaries
    - Headers with underlines using = and - characters
    - Tables formatted with ASCII characters and proper alignment
    - Lists with bullets and numbering
    - Preformatted text blocks with indentation
    - Clean plain text output suitable for email or console display

    See demo_text.pp for usage examples.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit WText;
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
  TTextWriter=Class(TFormattingWriter)
  Private
    FMaxLineLength: Integer;
    FCurrentLine: String;
    FCurrentIndent: Integer;
    FTableCols: Integer;
    FTableColWidths: array of Integer;
    FTableCurrentRow: TStringList;
    FListLevel: Integer;
    FListNumbers: array of Integer;
    FLastHeaderText: String;
    FLastHeaderLevel: Integer;

    procedure FlushCurrentLine;
    procedure WrapAndWrite(const AText: String);
    procedure WriteIndent;
    function FormatTableRow(const Cells: TStringList; UseSeparator: Boolean = False): String;
    procedure CalculateTableColumnWidths(const HeaderCells: TStringList);
  Public
    Constructor Create (AStream : TStream); override;
    Destructor Destroy; override;

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
    Procedure ListItem(Const AText : String); reintroduce;
    Procedure DefinitionItem(Const Aname,AText : String); override;

    // Additional methods
    procedure Write(Const AText : String); override;

  Protected
    property CurrentLine: String read FCurrentLine;

  Public
    // Properties
    property MaxLineLength: Integer read FMaxLineLength write FMaxLineLength;
  end;

implementation

{ TTextWriter }

constructor TTextWriter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FMaxLineLength := 72; // Default to 72 characters (standard email width)
  FCurrentLine := '';
  FCurrentIndent := 0;
  FTableCols := 0;
  FTableCurrentRow := TStringList.Create;
  FListLevel := 0;
  SetLength(FListNumbers, 0);
  FLastHeaderText := '';
  FLastHeaderLevel := 0;
end;

destructor TTextWriter.Destroy;
begin
  FlushCurrentLine;
  FTableCurrentRow.Free;
  inherited Destroy;
end;

procedure TTextWriter.FlushCurrentLine;
begin
  if FCurrentLine <> '' then
  begin
    inherited Write(FCurrentLine);
    DumpLn('');
    FCurrentLine := '';
  end;
end;

procedure TTextWriter.WriteIndent;
var
  i: Integer;
begin
  for i := 1 to FCurrentIndent do
    FCurrentLine := FCurrentLine + ' ';
end;

procedure TTextWriter.WrapAndWrite(const AText: String);
var
  Words: TStringList;
  i: Integer;
  Word: String;
  TestLine: String;
begin
  if AText = '' then Exit;

  Words := TStringList.Create;
  try
    // Split text into words
    Words.Delimiter := ' ';
    Words.DelimitedText := AText;

    for i := 0 to Words.Count - 1 do
    begin
      Word := Words[i];
      if Word = '' then Continue;

      // Test if adding this word would exceed line length
      if FCurrentLine = '' then
        WriteIndent;

      if FCurrentLine <> '' then
        TestLine := FCurrentLine + ' ' + Word
      else
        TestLine := StringOfChar(' ', FCurrentIndent) + Word;

      if (Length(TestLine) <= FMaxLineLength) or (Length(FCurrentLine) <= FCurrentIndent) then
      begin
        // Word fits on current line
        if (Length(FCurrentLine) > FCurrentIndent) and not (FCurrentLine[Length(FCurrentLine)] = ' ') then
          FCurrentLine := FCurrentLine + ' ';
        FCurrentLine := FCurrentLine + Word;
      end
      else
      begin
        // Word doesn't fit, start new line
        FlushCurrentLine;
        WriteIndent;
        FCurrentLine := FCurrentLine + Word;
      end;
    end;
  finally
    Words.Free;
  end;
end;

function TTextWriter.EscapeText(AText: String): String;
begin
  // Plain text doesn't need escaping
  Result := AText;
end;

procedure TTextWriter.Write(const AText: String);
begin
  // If we're writing header text, capture it
  if (FLastHeaderText = '') and (FLastHeaderLevel > 0) then
    FLastHeaderText := AText;
  WrapAndWrite(AText);
end;

procedure TTextWriter.DocumentStart(const Title: String);
begin
  if Title <> '' then
  begin
    FCurrentLine := Title;
    FlushCurrentLine;
    FCurrentLine := StringOfChar('=', Length(Title));
    FlushCurrentLine;
    FlushCurrentLine;
  end;
end;

procedure TTextWriter.DocumentEnd;
begin
  FlushCurrentLine;
end;

procedure TTextWriter.HeaderStart(Alevel: Integer);
begin
  FlushCurrentLine;
  FLastHeaderLevel := ALevel;
  FLastHeaderText := '';
end;

procedure TTextWriter.HeaderEnd(Alevel: Integer);
var
  UnderlineChar: Char;
  UnderlineLength: Integer;
begin
  FlushCurrentLine;
//  DumpLn('');

  // Choose underline character based on header level
  case ALevel of
    0, 1: UnderlineChar := '=';
    2: UnderlineChar := '-';
    else UnderlineChar := '-';
  end;

  // Use the length of the captured header text
  UnderlineLength := Length(FLastHeaderText);
  if UnderlineLength = 0 then
    UnderlineLength := 20; // Default length if we can't determine

  DumpLn(StringOfChar(UnderlineChar, UnderlineLength));
  DumpLn('');

  // Reset header tracking
  FLastHeaderText := '';
  FLastHeaderLevel := 0;
end;

procedure TTextWriter.ParagraphStart;
begin
  FlushCurrentLine;
end;

procedure TTextWriter.ParagraphEnd;
begin
  FlushCurrentLine;
  FlushCurrentLine;
end;

procedure TTextWriter.LineBreak;
begin
  FlushCurrentLine;
end;

procedure TTextWriter.Rule;
begin
  FlushCurrentLine;
  FCurrentLine := StringOfChar('-', FMaxLineLength);
  FlushCurrentLine;
  FlushCurrentLine;
end;

procedure TTextWriter.BoldStart;
begin
  WrapAndWrite('*');
end;

procedure TTextWriter.BoldEnd;
begin
  WrapAndWrite('*');
end;

procedure TTextWriter.ItalicStart;
begin
  WrapAndWrite('_');
end;

procedure TTextWriter.ItalicEnd;
begin
  WrapAndWrite('_');
end;

procedure TTextWriter.UnderlineStart;
begin
  WrapAndWrite('_');
end;

procedure TTextWriter.UnderlineEnd;
begin
  WrapAndWrite('_');
end;

procedure TTextWriter.PreformatStart;
begin
  FlushCurrentLine;
  FCurrentIndent := FCurrentIndent + 4; // Indent preformatted text
end;

procedure TTextWriter.PreformatEnd;
begin
  FlushCurrentLine;
  if FCurrentIndent >= 4 then
    FCurrentIndent := FCurrentIndent - 4;
  FlushCurrentLine;
end;

procedure TTextWriter.CalculateTableColumnWidths(const HeaderCells: TStringList);
var
  i: Integer;
  MinWidth: Integer;
begin
  SetLength(FTableColWidths, HeaderCells.Count);
  for i := 0 to HeaderCells.Count - 1 do
  begin
    MinWidth := Length(HeaderCells[i]) + 2; // +2 for padding
    if MinWidth < 8 then MinWidth := 8; // Minimum column width
    FTableColWidths[i] := MinWidth;
  end;
end;

function TTextWriter.FormatTableRow(const Cells: TStringList; UseSeparator: Boolean): String;
var
  i: Integer;
  Cell: String;
  PaddedCell: String;
begin
  Result := '|';
  for i := 0 to Cells.Count - 1 do
  begin
    if i < Length(FTableColWidths) then
    begin
      Cell := Cells[i];
      if UseSeparator then
        PaddedCell := StringOfChar('-', FTableColWidths[i] - 2)
      else
      begin
        PaddedCell := ' ' + Cell;
        while Length(PaddedCell) < FTableColWidths[i] - 1 do
          PaddedCell := PaddedCell + ' ';
      end;
      Result := Result + PaddedCell + ' |';
    end;
  end;
end;

procedure TTextWriter.TableStart(NoCols: Integer; Border: Boolean);
begin
  FlushCurrentLine;
  FTableCols := NoCols;
  FTableCurrentRow.Clear;
end;

procedure TTextWriter.TableEnd;
begin
  FlushCurrentLine;
  FlushCurrentLine;
end;

procedure TTextWriter.RowStart;
begin
  FTableCurrentRow.Clear;
end;

procedure TTextWriter.RowEnd;
var
  FormattedRow: String;
  SeparatorRow: String;
  SeparatorCells: TStringList;
  i: Integer;
begin
  if FTableCurrentRow.Count > 0 then
  begin
    // If this is the first row, calculate column widths
    if Length(FTableColWidths) = 0 then
    begin
      CalculateTableColumnWidths(FTableCurrentRow);

      // Add separator row after header
      FormattedRow := FormatTableRow(FTableCurrentRow, False);
      FCurrentLine := FormattedRow;
      FlushCurrentLine;

      SeparatorCells := TStringList.Create;
      try
        for i := 0 to FTableCurrentRow.Count - 1 do
          SeparatorCells.Add('');
        SeparatorRow := FormatTableRow(SeparatorCells, True);
        FCurrentLine := SeparatorRow;
        FlushCurrentLine;
      finally
        SeparatorCells.Free;
      end;
    end
    else
    begin
      FormattedRow := FormatTableRow(FTableCurrentRow, False);
      FCurrentLine := FormattedRow;
      FlushCurrentLine;
    end;
  end;
end;

procedure TTextWriter.CellStart;
begin
  // Cell content will be added via Write calls
end;

procedure TTextWriter.CellEnd;
begin
  // Current line content becomes the cell
  FTableCurrentRow.Add(Trim(FCurrentLine));
  FCurrentLine := '';
end;

procedure TTextWriter.HeaderCellStart;
begin
  CellStart;
end;

procedure TTextWriter.HeaderCellEnd;
begin
  CellEnd;
end;

procedure TTextWriter.ListStart(ListType: TListType);
begin
  FlushCurrentLine;
  Inc(FListLevel);
  SetLength(FListNumbers, FListLevel);
  FListNumbers[FListLevel - 1] := 0;
  FCurrentIndent := FCurrentIndent + 4; // Indent list items
end;

procedure TTextWriter.ListEnd(ListType: TListType);
begin
  FlushCurrentLine;
  if FListLevel > 0 then
  begin
    Dec(FListLevel);
    SetLength(FListNumbers, FListLevel);
    if FCurrentIndent >= 4 then
      FCurrentIndent := FCurrentIndent - 4;
  end;
  FlushCurrentLine;
end;

procedure TTextWriter.ListItemStart;
begin
  FlushCurrentLine;

  if FListLevel > 0 then
  begin
    Inc(FListNumbers[FListLevel - 1]);
    FCurrentLine := StringOfChar(' ', FCurrentIndent - 4) + IntToStr(FListNumbers[FListLevel - 1]) + '. ';
  end
  else
    FCurrentLine := StringOfChar(' ', FCurrentIndent - 4) + '*   ';
end;

procedure TTextWriter.ListItemEnd;
begin
  FlushCurrentLine;
end;

procedure TTextWriter.ListItem(const AText: String);
begin
  ListItemStart;
  Write(AText);
  ListItemEnd;
end;

procedure TTextWriter.DefinitionItem(const Aname, AText: String);
begin
  FlushCurrentLine;
  WriteIndent;
  FCurrentLine := FCurrentLine + AName;
  FlushCurrentLine;

  Inc(FCurrentIndent, 4);
  WriteIndent;
  WrapAndWrite(AText);
  FlushCurrentLine;
  if FCurrentIndent >= 4 then
    Dec(FCurrentIndent, 4);
end;

end.
