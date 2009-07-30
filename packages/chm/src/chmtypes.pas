{ Copyright (C) <2005> <Andrew Haines> chmtypes.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  TSectionName = (snMSCompressed, snUnCompressed);
  
  TSectionNames = set of TSectionName;
  
   { TDirectoryChunk }

  TDirectoryChunk = class(TObject)
  private
    FHeaderSize: Integer;
    FQuickRefEntries: Word;
    Buffer: array[0..$1000-1] of byte;
    CurrentPos: Integer;
    FItemCount: Word;
    FClearCount: Integer;
  public
    function CanHold(ASize: Integer): Boolean;
    function FreeSpace: Integer;
    procedure WriteHeader(AHeader: Pointer);
    procedure WriteEntry(Size: Integer; Data: Pointer);
    procedure WriteChunkToStream(Stream: TStream); overload;
    procedure Clear;
    property ItemCount: Word read FItemCount;
    constructor Create(AHeaderSize: Integer);
  end;
  
  { TPMGIDirectoryChunk }

  TPMGIDirectoryChunk = class(TDirectoryChunk)
  private
    FChunkLevelCount: Integer;
    FParentChunk: TPMGIDirectoryChunk;
  public
    procedure WriteChunkToStream(Stream: TStream; var AIndex: Integer; Final: Boolean = False); overload;
    property ParentChunk: TPMGIDirectoryChunk read FParentChunk write FParentChunk;
    property ChunkLevelCount: Integer read FChunkLevelCount write FChunkLevelCount;
  end;

  PFileEntryRec = ^TFileEntryRec;
  TFileEntryRec = record
    Path: String;
    Name: String;
    DecompressedOffset: QWord;
    DecompressedSize: QWord;
    Compressed: Boolean; // True means it goes in section1 False means section0
  end;

  { TFileEntryList }

  TFileEntryList = class(TList)
  private
    FPaths: TStringList;
    function GetFileEntry(Index: Integer): TFileEntryRec;
    procedure SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
  public
    function AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
    procedure Delete(Index: Integer);
    property FileEntry[Index: Integer]: TFileEntryRec read GetFileEntry write SetFileEntry;
    procedure Sort;
    constructor Create;
    destructor Destroy; override;

  end;


implementation
uses chmbase;

{ TDirectoryChunk }

function TDirectoryChunk.CanHold(ASize: Integer): Boolean;
begin
  Result := CurrentPos < $1000 - ASize - (SizeOf(Word) * (FQuickRefEntries+2));
end;

function TDirectoryChunk.FreeSpace: Integer;
begin
  Result := $1000 - CurrentPos;
end;

procedure TDirectoryChunk.WriteHeader(AHeader: Pointer);
begin
  Move(AHeader^, Buffer[0], FHeaderSize);
end;

procedure TDirectoryChunk.WriteEntry(Size: Integer; Data: Pointer);
var
  ReversePos: Integer;
  Value: Word;
begin
  if not CanHold(Size) then Raise Exception.Create('Trying to write past the end of the buffer');
  Move(Data^, Buffer[CurrentPos], Size);
  Inc(CurrentPos, Size);
  Inc(FItemCount);
  
  // now put a quickref entry if needed
  if ItemCount mod 5 = 0 then begin
    Inc(FQuickRefEntries);
    ReversePos := ($1000) - SizeOf(Word) - (SizeOf(Word)*FQuickRefEntries);
    Value := NtoLE(Word(CurrentPos - Size));
    Move(Value, Buffer[ReversePos], SizeOf(Word));
  end;
end;

procedure TDirectoryChunk.WriteChunkToStream(Stream: TStream);
var
  ReversePos: Integer;
  TmpItemCount: Word;
begin
  ReversePos := $1000 - SizeOf(Word);
  TmpItemCount := NtoLE(Word(FItemCount));
  Move(TmpItemCount, Buffer[ReversePos], SizeOf(Word));

  Stream.Write(Buffer[0], $1000);
  {$IFDEF DEBUG_CHM_CHUNKS}
  WriteLn('Writing ', Copy(PChar(@Buffer[0]),0,4),' ChunkToStream');
  {$ENDIF}
end;

procedure TDirectoryChunk.Clear;
begin
  FillChar(Buffer, $1000, 0);
  FItemCount := 0;
  CurrentPos := FHeaderSize;
  FQuickRefEntries := 0;
  Inc(FClearCount);
end;

constructor TDirectoryChunk.Create(AHeaderSize: Integer);
begin
  FHeaderSize := AHeaderSize;
  CurrentPos := FHeaderSize;
end;

{ TFileEntryList }

function TFileEntryList.GetFileEntry(Index: Integer): TFileEntryRec;
begin
  Result := PFileEntryRec(Items[Index])^;
end;

procedure TFileEntryList.SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
begin
  PFileEntryRec(Items[Index])^ := AValue;
end;

function TFileEntryList.AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
var
  TmpEntry: PFileEntryRec;
begin
  New(TmpEntry);
  //WriteLn('Adding: ', AFileEntry.Path+AFileEntry.Name,' Size = ', AFileEntry.DecompressedSize,' Offset = ', AFileEntry.DecompressedOffset);
  if CheckPathIsAdded and (FPaths.IndexOf(AFileEntry.Path) < 0) then begin
    // all paths are included in the list of files in section 0 with a size and offset of 0
    FPaths.Add(AFileEntry.Path);
    TmpEntry^.Path := AFileEntry.Path;
    TmpEntry^.Name := '';
    TmpEntry^.DecompressedOffset := 0;
    TmpEntry^.DecompressedSize := 0;
    TmpEntry^.Compressed := False;
    (Self as TList).Add(TmpEntry);
    New(TmpEntry);
  end;
  TmpEntry^ := AFileEntry;
  Result := (Self as TList).Add(TmpEntry);
end;

procedure TFileEntryList.Delete(Index: Integer);
begin
  Dispose(PFileEntryRec(Items[Index]));
  Inherited Delete(Index);
end;

function FileEntrySortFunc(Item1, Item2: PFileEntryRec): Integer;
var
  Str1, Str2: String;
begin
  Str1 := Item1^.Path + Item1^.Name;
  Str2 := Item2^.Path + Item2^.Name;
  Result := ChmCompareText(Str1, Str2);
end;

procedure TFileEntryList.Sort;
begin
  Inherited Sort(TListSortCompare(@FileEntrySortFunc));
end;

constructor TFileEntryList.Create;
begin
  Inherited Create;
  FPaths := TStringList.Create;
end;

destructor TFileEntryList.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Delete(I);
  FPaths.Free;
  inherited Destroy;
end;

{ TPMGIDirectoryChunk }
procedure TPMGIDirectoryChunk.WriteChunkToStream(Stream: TStream; var AIndex: Integer
  ; Final: Boolean = False);
var
  NewBuffer: array[0..512] of byte;
  EntryLength,
  WriteSize: Integer;
  OldPos, NewPos, NewStart: Int64;
  procedure FinishBlock;
  var
    Header: TPMGIIndexChunk;
  begin
    Inc(AIndex);
    Header.PMGIsig := 'PMGI';
    Header.UnusedSpace := FParentChunk.FreeSpace;
    FParentChunk.WriteHeader(@Header);
    FParentChunk.WriteChunkToStream(Stream, AIndex, Final);
    FParentChunk.Clear;
  end;
begin
  if FItemCount < 1 then begin
    WriteLn('WHAT ARE YOU DOING!!');
    Dec(AIndex);
    Exit;
  end;
  OldPos := Stream.Position;
  WriteChunkToStream(Stream);
  NewPos := Stream.Position;
  Inc(FChunkLevelCount);
  
  if Final and (ChunkLevelCount < 2) then begin
    FParentChunk.Free;
    FParentChunk := nil;
    Exit;
  end;
  if FParentChunk = nil then FParentChunk := TPMGIDirectoryChunk.Create(FHeaderSize);

  NewStart := OldPos+FHeaderSize;
  Stream.Position := NewStart;
  EntryLength := GetCompressedInteger(Stream);
  WriteSize := (Stream.Position - NewStart) + EntryLength;
  Move(Buffer[FHeaderSize], NewBuffer[0], WriteSize);
  Inc(WriteSize, WriteCompressedInteger(@NewBuffer[WriteSize], AIndex));

  Stream.Position := NewPos;

  if not FParentChunk.CanHold(WriteSize) then begin
    FinishBlock;
  end;
  
  FParentChunk.WriteEntry(WriteSize, @NewBuffer[0]);
  if Final then FinishBlock;
  //WriteLn(ChunkLevelCount);
end;

end.

