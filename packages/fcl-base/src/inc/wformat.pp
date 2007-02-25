{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wformat;

{$ifdef fpc}
{$mode objfpc}
{$endif}

Interface

uses Classes,SysUtils;

Type
  TlistType = (ltNumbered,ltOrdered,ltDefinition);

  TFormattingWriter = Class
  Private
    FStream : TStream;
  Public
    Constructor Create (AStream : TStream); Virtual;
    // To be overridden by descendents
    Function EscapeText (AText : String) : String; Virtual;
    // Quick dump.
    Procedure Dump(Const AText : String);
    Procedure DumpLn(Const AText : String);
    // Formatted write. Calls escapetext.
    Procedure Write(Const AText : String);
    Procedure WriteFmt(Const Fmt : String; Args : Array of const);
    // Document Structure
    Procedure DocumentStart(Const Title : String); Virtual;
    Procedure DocumentEnd; Virtual;
    // Header formatting
    Procedure Header(Alevel : Integer; Msg : String);
    Procedure HeaderStart(Alevel : Integer); virtual;
    Procedure HeaderEnd(Alevel : Integer); virtual;
    // Basic line formatting.
    Procedure ParagraphStart; virtual;
    Procedure ParagraphEnd; virtual;
    Procedure LineBreak; virtual;
    Procedure Rule; virtual;
    // text formatting.
    Procedure BoldStart; Virtual;
    Procedure BoldEnd;Virtual;
    Procedure ItalicStart;Virtual;
    Procedure ItalicEnd;Virtual;
    Procedure UnderlineStart;Virtual;
    Procedure UnderlineEnd;Virtual;
    // Preformatted.
    Procedure PreformatStart; virtual;
    Procedure PreformatEnd; virtual;
    // Table support
    Procedure TableStart( NoCols: Integer; Border : Boolean); virtual;
    Procedure TableEnd; virtual;
    Procedure RowStart; virtual;
    Procedure RowEnd; virtual;
    Procedure RowNext;
    Procedure CellStart; virtual;
    Procedure CellEnd; virtual;
    Procedure CellNext;
    Procedure HeaderCellStart; virtual;
    Procedure HeaderCellEnd; virtual;
    Procedure HeaderCellNext;
    // List support;
    Procedure ListStart(ListType : TListType); Virtual;
    Procedure ListEnd(ListType : TListType); Virtual;
    Procedure ListItemStart; Virtual;
    Procedure ListItemEnd; Virtual;
    Procedure ListItem(Const AText : String);
    Procedure DefinitionItem(Const Aname,AText : String); Virtual;
    Procedure WriteList(ListType : TListType; List : TStrings);
  Protected
    Property Stream : TStream Read FStream;
  end;

const
  LineFeed = LineEnding;

Implementation

{ TFormattingWriter }

procedure TFormattingWriter.BoldEnd;
begin
end;

procedure TFormattingWriter.BoldStart;
begin
end;

procedure TFormattingWriter.CellEnd;
begin
end;

procedure TFormattingWriter.CellStart;
begin
end;

procedure TFormattingWriter.CellNext;
begin
  CellEnd;
  CellStart;
end;

constructor TFormattingWriter.Create(AStream: TStream);
begin
  FStream:=AStream;
end;

procedure TFormattingWriter.DefinitionItem(const Aname, AText: String);
begin

end;

procedure TFormattingWriter.DocumentEnd;
begin

end;

procedure TFormattingWriter.DocumentStart(const Title: String);
begin

end;

procedure TFormattingWriter.Dump(const AText: String);
begin
  FStream.WriteBuffer(Atext[1],Length(AText));
end;

procedure TFormattingWriter.DumpLn(const AText: String);

begin
  Dump(Atext);
  Dump(LineFeed);
end;

Function TFormattingWriter.EscapeText(AText: String) : String;
begin
  Result:=AText;
end;

procedure TFormattingWriter.Header(Alevel: Integer; Msg: String);
begin
  HeaderStart(ALevel);
  Write(Msg);
  HeaderEnd(Alevel)
end;

procedure TFormattingWriter.HeaderCellEnd;
begin

end;

procedure TFormattingWriter.HeaderCellStart;
begin

end;

procedure TFormattingWriter.HeaderCellNext;
begin
  HeaderCellEnd;
  HeaderCellStart;
end;

procedure TFormattingWriter.HeaderEnd(Alevel: Integer);
begin
end;

procedure TFormattingWriter.HeaderStart(Alevel: Integer);
begin

end;

procedure TFormattingWriter.ItalicEnd;
begin

end;

procedure TFormattingWriter.ItalicStart;
begin

end;

procedure TFormattingWriter.LineBreak;
begin
end;

procedure TFormattingWriter.ListEnd(ListType: TListType);
begin

end;

procedure TFormattingWriter.ListItem(const AText: String);
begin
  ListItemStart;
  Write(Atext);
  ListItemEnd;
end;

procedure TFormattingWriter.ListItemEnd;
begin

end;

procedure TFormattingWriter.ListItemStart;
begin

end;

procedure TFormattingWriter.ListStart(ListType: TListType);
begin

end;

procedure TFormattingWriter.ParagraphEnd;
begin
end;

procedure TFormattingWriter.ParagraphStart;
begin
end;

procedure TFormattingWriter.PreformatEnd;
begin
end;

procedure TFormattingWriter.PreformatStart;
begin
end;

procedure TFormattingWriter.RowEnd;
begin
end;

procedure TFormattingWriter.RowStart;
begin
end;

procedure TFormattingWriter.RowNext;
begin
  RowEnd;
  RowStart;
end;

procedure TFormattingWriter.Rule;
begin
end;

procedure TFormattingWriter.TableStart(NoCols: Integer; Border: Boolean);
begin
end;

procedure TFormattingWriter.TableEnd;
begin
end;

procedure TFormattingWriter.UnderlineEnd;
begin
end;

procedure TFormattingWriter.UnderlineStart;
begin
end;

procedure TFormattingWriter.Write(const AText: String);
begin
  Dump(EscapeText(Atext));
end;

procedure TFormattingWriter.WriteFmt(const Fmt: String; Args: array of const);
begin
  Write(Format(Fmt,Args));
end;

procedure TFormattingWriter.WriteList(ListType: TListType; List: TStrings);

Var
  I,J : integer;
  N,V : String;

begin
  ListStart(ListType);
  try
    For I:=0 to List.Count-1 do
      if ListType<>ltDefinition then
        ListItem(List[i])
      else
        begin
        V:=List[i];
        J:=Pos('=',V);
        if (J>0) then
          begin
          N:=Copy(V,1,J-1);
          Delete(V,1,J);
          end;
        DefinitionItem(N,V);
        end;
  finally
    ListEnd(ListType)
  end;
end;

end.
