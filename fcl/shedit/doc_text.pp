{
    $Id$

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// Generic text document class

unit doc_text;

{$MODE objfpc}
{$H+}

interface

uses Classes;

type
  PLine = ^TLine;
  TLine = record
    info: Pointer;
    flags: LongWord;
    s: AnsiString;
  end;

  PLineArray = ^TLineArray;
  TLineArray = array[0..MaxInt div SizeOf(TLine) - 1] of TLine;

const

  {TLine.flags Syntax Highlighting Flags}
  LF_SH_Valid      = $01;
  LF_SH_Multiline1 = $02;
  LF_SH_Multiline2 = $04;
  LF_SH_Multiline3 = $08;
  LF_SH_Multiline4 = $10;
  LF_SH_Multiline5 = $20;
  LF_SH_Multiline6 = $40;
  LF_SH_Multiline7 = $80;

  {Escape character for syntax highlighting (marks start of sh sequence,
   next character is color/sh element number, beginning at #1}
  LF_Escape = #10;

type

  TTextDoc = class;

  TDocLineEvent = procedure(Sender: TTextDoc; Line: Integer) of object;

  TViewInfo = class(TCollectionItem)
  public
    OnLineInsert, OnLineRemove, OnLineChange: TDocLineEvent;
    OnClearDocument, OnModifiedChange: TNotifyEvent;
  end;

  TTextDoc = class
  protected
    RefCount: LongInt;
    FLineEnding: String;
    FModified: Boolean;
    FLineWidth,
    FLineCount: LongInt;
    FLines: PLineArray;
    FViewInfos: TCollection;
    procedure SetModified(AModified: Boolean);
    function  GetLineText(LineNumber: Integer): String;
    procedure SetLineText(LineNumber: Integer; const NewText: String);
    function  GetLineLen(LineNumber: Integer): Integer;
    function  GetLineFlags(LineNumber: Integer): Byte;
    procedure SetLineFlags(LineNumber: Integer; NewFlags: Byte);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;
    procedure Clear;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const filename: String);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const filename: String);

    procedure InsertLine(BeforeLine: Integer; const s: String);
    procedure AddLine(const s: String);
    procedure RemoveLine(LineNumber: Integer);

    property LineEnding: String read FLineEnding write FLineEnding;
    property Modified: Boolean read FModified write SetModified;
    property LineWidth: Integer read FLineWidth;
    property LineCount: Integer read FLineCount;
    property LineText[LineNumber: Integer]: String read GetLineText write SetLineText;
    property LineLen[LineNumber: Integer]: Integer read GetLineLen;
    property LineFlags[LineNumber: Integer]: Byte read GetLineFlags write SetLineFlags;

    property ViewInfos: TCollection read FViewInfos;
  end;


implementation

uses Strings;


constructor TTextDoc.Create;
begin
  FModified := False;
{$IFDEF Linux}
  LineEnding := #10;
{$ELSE}
  LineEnding := #13#10;
{$ENDIF}
  FLines := nil;
  FLineCount := 0;
  FLineWidth := 0;
  FViewInfos := TCollection.Create(TViewInfo);
  RefCount := 1;
end;

destructor TTextDoc.Destroy;
var
  i: Integer;
begin
  if Assigned(FLines) then
  begin
    for i := 0 to FLineCount - 1 do
      SetLength(FLines^[i].s, 0);
    FreeMem(FLines);
  end;

  FViewInfos.Free;
  inherited Destroy;
end;

procedure TTextDoc.AddRef;
begin
  Inc(RefCount);
end;

procedure TTextDoc.Release;
begin
  ASSERT(RefCount > 0);
  Dec(RefCount);
  if RefCount = 0 then
    Self.Free;
end;

procedure TTextDoc.Clear;
var
  i: Integer;
begin
  if Assigned(FLines) then
  begin
    for i := 0 to FLineCount - 1 do
      SetLength(FLines^[i].s, 0);
    FreeMem(FLines);
    FLineCount:=0;
  end;

  FLineWidth:=0;

  for i := 0 to FViewInfos.Count - 1 do
    if Assigned(TViewInfo(FViewInfos.Items[i]).OnClearDocument) then
      TViewInfo(FViewInfos.Items[i]).OnClearDocument(Self);
end;

procedure TTextDoc.InsertLine(BeforeLine: Integer; const s: String);
var
  l: PLine;
  i: Integer;
begin
  if (BeforeLine < 0) or (BeforeLine > FLineCount) then
    exit;  // !!!: throw an exception

  ReAllocMem(FLines, (FLineCount + 1) * SizeOf(TLine));
  Move(FLines^[BeforeLine], FLines^[BeforeLine + 1], (FLineCount - BeforeLine) * SizeOf(TLine));
  l := @FLines^[BeforeLine];
  FillChar(l^, SizeOf(TLine), 0);
  l^.s := s;

  Inc(FLineCount);
  if Length(s) > FLineWidth then
    FLineWidth := Length(s);

  for i := 0 to FViewInfos.Count - 1 do
    if Assigned(TViewInfo(FViewInfos.Items[i]).OnLineInsert) then
      TViewInfo(FViewInfos.Items[i]).OnLineInsert(Self, BeforeLine);
end;

procedure TTextDoc.AddLine(const s: String);
begin
  InsertLine(FLineCount, s);
end;

procedure TTextDoc.RemoveLine(LineNumber: Integer);
var
  i: Integer;
begin
  SetLength(FLines^[LineNumber].s, 0);	// Free the string for this line
  ReAllocMem(FLines, (FLineCount - 1) * SizeOf(TLine));
  if LineNumber < FLineCount - 1 then
    Move(FLines^[LineNumber + 1], FLines^[LineNumber],(FLineCount - LineNumber - 1) * SizeOf(TLine));
  Dec(FLineCount);

  for i := 0 to FViewInfos.Count - 1 do
    if Assigned(TViewInfo(FViewInfos.Items[i]).OnLineRemove) then
      TViewInfo(FViewInfos.Items[i]).OnLineRemove(Self, LineNumber);
  Modified := True;
end;

procedure TTextDoc.LoadFromStream(AStream: TStream);

  procedure ProcessLine(const s: String);
  var
    s2: String;
    i: Integer;
  begin
    // Expand tabs to spaces
    s2 := '';
    for i := 1 to Length(s) do
      if s[i] = #9 then
      begin
        repeat
	  s2 := s2 + ' '
	until (Length(s2) mod 8) = 0;
      end else
        s2 := s2 + s[i];
    AddLine(s2);
  end;

var
  NewData: array[0..1023] of Byte;
  buffer, p: PChar;
  BytesInBuffer, BytesRead, OldBufSize, LastEndOfLine, i, LineLength: Integer;
  line: String;
begin
  Clear;
  SetLength(line, 0);
  BytesInBuffer := 0;
  buffer := nil;

  while True do
  begin
    BytesRead := AStream.Read(NewData, SizeOf(NewData));
    if BytesRead <= 0 then
      break;
    OldBufSize := BytesInBuffer;

    // Append the new received data to the read buffer
    Inc(BytesInBuffer, BytesRead);
    ReallocMem(buffer, BytesInBuffer);
    Move(NewData, buffer[OldBufSize], BytesRead);

    LastEndOfLine := 0;
    if OldBufSize > 0 then
      i := OldBufSize - 1
    else
      i := 0;

    while i <= BytesInBuffer - 2 do
    begin
      if (buffer[i] = #13) or (buffer[i] = #10) then
      begin
        LineLength := i - LastEndOfLine;
	SetLength(line, LineLength);
	if LineLength > 0 then
	  Move(buffer[LastEndOfLine], line[1], LineLength);

	ProcessLine(line);

	if ((buffer[i] = #13) and (buffer[i + 1] = #10)) or
	   ((buffer[i] = #10) and (buffer[i + 1] = #13)) then
	  Inc(i);
	LastEndOfLine := i + 1;
      end;
      Inc(i);
    end;

    if LastEndOfLine > 0 then
    begin
      // Remove all processed lines from the buffer
      Dec(BytesInBuffer, LastEndOfLine);
      GetMem(p, BytesInBuffer);
      Move(buffer[LastEndOfLine], p^, BytesInBuffer);
      FreeMem(buffer);
      buffer := p;
    end;
  end;

  if BytesInBuffer > 0 then
    if buffer[BytesInBuffer - 1] in [#13, #10] then
    begin
      SetLength(line, BytesInBuffer - 1);
      if BytesInBuffer > 1 then
        Move(buffer^, line[1], BytesInBuffer - 1);
      ProcessLine(line);
      ProcessLine('');
    end else
    begin
      SetLength(line, BytesInBuffer);
      if BytesInBuffer > 1 then
        Move(buffer^, line[1], BytesInBuffer);
      ProcessLine(line);
    end;

  if Assigned(buffer) then
    FreeMem(buffer);
end;


procedure TTextDoc.LoadFromFile(const filename: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead);
  LoadFromStream(stream);
  stream.Free;
end;

procedure TTextDoc.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  for i := 0 to FLineCount - 2 do
  begin
    AStream.Write(FLines^[i].s, Length(FLines^[i].s));
    AStream.Write(FLineEnding, Length(FLineEnding));
  end;
  if FLineCount > 0 then
    AStream.Write(FLines^[FLineCount - 1].s, Length(FLines^[FLineCount - 1].s));
end;

procedure TTextDoc.SaveToFile(const filename: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmCreate);
  SaveToStream(stream);
  stream.Free;
end;

procedure TTextDoc.SetModified(AModified: Boolean);
var
  i: Integer;
begin
  if AModified = FModified then
    exit;
  FModified := AModified;

  for i := 0 to FViewInfos.Count - 1 do
    if Assigned(TViewInfo(FViewInfos.Items[i]).OnModifiedChange) then
      TViewInfo(FViewInfos.Items[i]).OnModifiedChange(Self);
end;

function TTextDoc.GetLineText(LineNumber: Integer): String;
begin
  if (LineNumber < 0) or (LineNumber >= FLineCount) then
    Result := ''
  else
    Result := FLines^[LineNumber].s;
end;

procedure TTextDoc.SetLineText(LineNumber: Integer; const NewText: String);
var
  i: Integer;
begin
  if FLines^[LineNumber].s <> NewText then
  begin
    FLines^[LineNumber].s := NewText;
    if Length(NewText) > FLineWidth then
      FLineWidth := Length(NewText);
    Modified := True;
    for i := 0 to FViewInfos.Count - 1 do
      if Assigned(TViewInfo(FViewInfos.Items[i]).OnLineChange) then
        TViewInfo(FViewInfos.Items[i]).OnLineChange(Self, LineNumber);
  end;
end;

function TTextDoc.GetLineLen(LineNumber: Integer): Integer;
begin
  if (LineNumber < 0) or (LineNumber >= FLineCount) then
    Result := 0
  else
    Result := Length(FLines^[LineNumber].s);
end;

function TTextDoc.GetLineFlags(LineNumber: Integer): Byte;
begin
  if (LineNumber < 0) or (LineNumber >= FLineCount) then
    Result := 0
  else
    Result := FLines^[LineNumber].flags;
end;

procedure TTextDoc.SetLineFlags(LineNumber: Integer; NewFlags: Byte);
begin
  FLines^[LineNumber].flags := NewFlags;
end;


end.


{
  $Log$
  Revision 1.13  2000-07-01 12:28:38  sg
  * Added initialisation and finalisation code which is common to all
    platforms.

  Revision 1.12  2000/02/24 13:32:10  sg
  * The last line in a document is now read correctly

  Revision 1.11  2000/02/22 14:26:52  sg
  * New, much faster stream reader
  * Added more notifiers for the attached view objects

  Revision 1.10  2000/02/19 19:05:16  sg
  * Lines are now stored as AnsiStrings instead of PChars

  Revision 1.9  2000/01/31 19:22:16  sg
  * Added support for loading from streams
    (NOTE: The new loading code is very slow at the moment)
  * Added saving support
  * The line ending to use (CR/LF, LF...) can now be specified
  * The last line of a document isn't written with a line ending, this
    preserves the original structure of a previously loaded file
  * Fixed memory leaks

  Revision 1.8  2000/01/08 12:08:58  sg
  * Set the upper bound of TLineArray to max. possible value so that this
    unit works correctly with activated range checks

  Revision 1.7  2000/01/07 01:24:34  peter
    * updated copyright to 2000

  Revision 1.6  2000/01/06 01:20:34  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:09  peter
    * moved to packages dir

  Revision 1.4  1999/12/30 21:02:14  sg
  * Shortened copyright notice

  Revision 1.3  1999/12/09 23:16:41  peter
    * cursor walking is now possible, both horz and vert ranges are now
      adapted
    * filter key modifiers
    * selection move routines added, but still no correct output to the
      screen

  Revision 1.2  1999/11/14 21:32:55  peter
    * fixes to get it working without crashes

  Revision 1.1  1999/10/29 15:59:03  peter
    * inserted in fcl

}
