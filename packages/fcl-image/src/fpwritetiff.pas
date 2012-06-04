{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    Tiff writer for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Working:
    Grayscale 8,16bit (optional alpha),
    RGB 8,16bit (optional alpha),
    Orientation,

  ToDo:
    Compression: LZW, packbits, deflate, jpeg, ...
    thumbnail
    Planar
    ColorMap
    multiple images
    separate mask
    pages
    fillorder - not needed by baseline tiff reader
    bigtiff 64bit offsets
    endian - currently using system endianess
}
unit FPWriteTiff;

{$mode objfpc}{$H+}

{off $DEFINE VerboseTiffWriter}

interface

uses
  Math, Classes, SysUtils, FPimage, FPTiffCmn;

type

  { TTiffWriteEntry }

  TTiffWriteEntry = class
  public
    Tag: Word;
    EntryType: Word;
    Count: DWord;
    Data: Pointer;
    DataPos: DWord;
    Bytes: DWord;
    destructor Destroy; override;
  end;

  TTiffWriteChunk = record
    Data: Pointer;
    Bytes: DWord;
  end;
  PTiffWriteChunk = ^TTiffWriteChunk;

  { TTiffWriteChunkOffsets }

  TTiffWriteChunkOffsets = class(TTiffWriteEntry)
  public
    Chunks: PTiffWriteChunk;
    ChunkByteCounts: TTiffWriteEntry;
    constructor Create;
    destructor Destroy; override;
    procedure SetCount(NewCount: DWord);
  end;

  { TFPWriterTiff }

  TFPWriterTiff = class(TFPCustomImageWriter)
  private
    FSaveCMYKAsRGB: boolean;
    fStartPos: Int64;
    FEntries: TFPList; // list of TFPList of TTiffWriteEntry
    fStream: TStream;
    fPosition: DWord;
    procedure ClearEntries;
    procedure WriteTiff;
    procedure WriteHeader;
    procedure WriteIFDs;
    procedure WriteEntry(Entry: TTiffWriteEntry);
    procedure WriteData;
    procedure WriteEntryData(Entry: TTiffWriteEntry);
    procedure WriteBuf(var Buf; Count: DWord);
    procedure WriteWord(w: Word);
    procedure WriteDWord(d: DWord);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure AddEntryString(Tag: word; const s: string);
    procedure AddEntryShort(Tag: word; Value: Word);
    procedure AddEntryLong(Tag: word; Value: DWord);
    procedure AddEntryShortOrLong(Tag: word; Value: DWord);
    procedure AddEntryRational(Tag: word; const Value: TTiffRational);
    procedure AddEntry(Tag: Word; EntryType: Word; EntryCount: DWord;
                       Data: Pointer; Bytes: DWord;
                       CopyData: boolean = true);
    procedure AddEntry(Entry: TTiffWriteEntry);
    procedure TiffError(Msg: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddImage(Img: TFPCustomImage);
    procedure SaveToStream(Stream: TStream);
    property SaveCMYKAsRGB: boolean read FSaveCMYKAsRGB write FSaveCMYKAsRGB;
  end;

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;

implementation

function CompareTiffWriteEntries(Entry1, Entry2: Pointer): integer;
begin
  Result:=integer(TTiffWriteEntry(Entry1).Tag)-integer(TTiffWriteEntry(Entry2).Tag);
end;

{ TFPWriterTiff }

procedure TFPWriterTiff.WriteWord(w: Word);
begin
  if fStream<>nil then
    fStream.WriteWord(w);
  inc(fPosition,2);
end;

procedure TFPWriterTiff.WriteDWord(d: DWord);
begin
  if fStream<>nil then
    fStream.WriteDWord(d);
  inc(fPosition,4);
end;

procedure TFPWriterTiff.ClearEntries;
var
  i: Integer;
  List: TFPList;
  j: Integer;
begin
  for i:=FEntries.Count-1 downto 0 do begin
    List:=TFPList(FEntries[i]);
    for j:=List.Count-1 downto 0 do
      TObject(List[j]).Free;
    List.Free;
  end;
  FEntries.Clear;
end;

procedure TFPWriterTiff.WriteTiff;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TFPWriterTiff.WriteTiff fStream=',fStream<>nil);
  {$ENDIF}
  fPosition:=0;
  WriteHeader;
  WriteIFDs;
  WriteData;
end;

procedure TFPWriterTiff.WriteHeader;
var
  EndianMark: String;
begin
  EndianMark:={$IFDEF FPC_BIG_ENDIAN}'MM'{$ELSE}'II'{$ENDIF};
  WriteBuf(EndianMark[1],2);
  WriteWord(42);
  WriteDWord(8);
end;

procedure TFPWriterTiff.WriteIFDs;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriteEntry;
  NextIFDPos: DWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write count
    {$IFDEF FPC_Debug_Image}
    writeln('TFPWriterTiff.WriteIFDs List=',i,' Count=',List.Count);
    {$ENDIF}
    WriteWord(List.Count);
    // write array of entries
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriteEntry(List[j]);
      WriteEntry(Entry);
    end;
    // write position of next IFD
    if i<FEntries.Count-1 then
      NextIFDPos:=fPosition+4
    else
      NextIFDPos:=0;
    WriteDWord(NextIFDPos);
  end;
end;

procedure TFPWriterTiff.WriteEntry(Entry: TTiffWriteEntry);
var
  PadBytes: DWord;
begin
  {$IFDEF FPC_Debug_Image}
  //writeln('TFPWriterTiff.WriteEntry Tag=',Entry.Tag,' Type=',Entry.EntryType,' Count=',Entry.Count,' Bytes=',Entry.Bytes);
  {$ENDIF}
  WriteWord(Entry.Tag);
  WriteWord(Entry.EntryType);
  WriteDWord(Entry.Count);
  if Entry.Bytes<=4 then begin
    if Entry.Bytes>0 then
      WriteBuf(Entry.Data^,Entry.Bytes);
    PadBytes:=0;
    WriteBuf(PadBytes,4-Entry.Bytes);
  end else begin
    WriteDWord(Entry.DataPos);
  end;
end;

procedure TFPWriterTiff.WriteData;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriteEntry;
  Strips: TTiffWriteChunkOffsets;
  k: Integer;
  Bytes: DWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write entry data
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriteEntry(List[j]);
      WriteEntryData(Entry);
    end;
    // write strips
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriteEntry(List[j]);
      if Entry is TTiffWriteChunkOffsets then begin
        Strips:=TTiffWriteChunkOffsets(Entry);
        // write Strips
        for k:=0 to Strips.Count-1 do begin
          PDWord(Strips.Data)[k]:=fPosition;
          Bytes:=Strips.Chunks[k].Bytes;
          PDWord(Strips.ChunkByteCounts.Data)[k]:=Bytes;
          {$IFDEF FPC_Debug_Image}
          //writeln('TFPWriterTiff.WriteData Strip fPosition=',fPosition,' Bytes=',Bytes);
          {$ENDIF}
          if Bytes>0 then
            WriteBuf(Strips.Chunks[k].Data^,Bytes);
        end;
      end;
    end;
  end;
end;

procedure TFPWriterTiff.WriteEntryData(Entry: TTiffWriteEntry);
begin
  if Entry.Bytes>4 then begin
    Entry.DataPos:=fPosition;
    WriteBuf(Entry.Data^,Entry.Bytes);
  end;
end;

procedure TFPWriterTiff.WriteBuf(var Buf; Count: DWord);
begin
  if Count=0 then exit;
  if (fStream<>nil) then
    fStream.Write(Buf,Count);
  inc(fPosition,Count);
end;

procedure TFPWriterTiff.AddImage(Img: TFPCustomImage);
var
  IFD: TTiffIFD;
  GrayBits, RedBits, GreenBits, BlueBits, AlphaBits: Word;
  ImgWidth, ImgHeight: DWord;
  Compression: Word;
  BitsPerSample: array[0..3] of Word;
  SamplesPerPixel: Integer;
  BitsPerPixel: DWord;
  i: Integer;
  OrientedWidth, OrientedHeight: DWord;
  x, y: integer;
  Row: DWord;
  BytesPerLine: DWord;
  ChunkOffsets: TTiffWriteChunkOffsets;
  ChunkBytes: DWord;
  Chunk: PByte;
  ChunkIndex: DWord;
  ChunkCounts: TTiffWriteEntry;
  Run: PByte;
  Col: TFPColor;
  Value: Integer;
  dx, dy: Integer;
  CurEntries: TFPList;
  Shorts: array[0..3] of Word;
begin
  ChunkOffsets:=nil;
  Chunk:=nil;
  IFD:=TTiffIFD.Create;
  try
    // add new list of entries
    CurEntries:=TFPList.Create;
    FEntries.Add(CurEntries);

    if Img.Extra[TiffPhotoMetric]='' then
      IFD.PhotoMetricInterpretation:=2
    else begin
      IFD.PhotoMetricInterpretation:=
        StrToInt64Def(Img.Extra[TiffPhotoMetric],High(IFD.PhotoMetricInterpretation));
      if SaveCMYKAsRGB and (IFD.PhotoMetricInterpretation=5) then
        IFD.PhotoMetricInterpretation:=2;
    end;
    if not (IFD.PhotoMetricInterpretation in [0,1,2]) then
      TiffError('PhotoMetricInterpretation="'+Img.Extra[TiffPhotoMetric]+'" not supported');
    IFD.Artist:=Img.Extra[TiffArtist];
    IFD.Copyright:=Img.Extra[TiffCopyright];
    IFD.DocumentName:=Img.Extra[TiffDocumentName];
    IFD.DateAndTime:=Img.Extra[TiffDateTime];
    IFD.HostComputer:=Img.Extra[TiffHostComputer];
    IFD.Make_ScannerManufacturer:=Img.Extra[TiffMake_ScannerManufacturer];
    IFD.Model_Scanner:=Img.Extra[TiffModel_Scanner];
    IFD.ImageDescription:=Img.Extra[TiffImageDescription];
    IFD.Software:=Img.Extra[TiffSoftware];
    IFD.Orientation:=StrToIntDef(Img.Extra[TiffOrientation],1);
    if not (IFD.Orientation in [1..8]) then
      IFD.Orientation:=1;
    IFD.ResolutionUnit:=StrToIntDef(Img.Extra[TiffResolutionUnit],2);
    if not (IFD.ResolutionUnit in [1..3]) then
      IFD.ResolutionUnit:=2;
    IFD.XResolution:=StrToTiffRationalDef(Img.Extra[TiffXResolution],TiffRational72);
    IFD.YResolution:=StrToTiffRationalDef(Img.Extra[TiffYResolution],TiffRational72);
    IFD.PageNumber:=StrToIntDef(Img.Extra[TiffPageNumber],0);
    IFD.PageCount:=StrToIntDef(Img.Extra[TiffPageCount],0);

    GrayBits:=StrToIntDef(Img.Extra[TiffGrayBits],8);
    RedBits:=StrToIntDef(Img.Extra[TiffRedBits],8);
    GreenBits:=StrToIntDef(Img.Extra[TiffGreenBits],8);
    BlueBits:=StrToIntDef(Img.Extra[TiffBlueBits],8);
    AlphaBits:=StrToIntDef(Img.Extra[TiffAlphaBits],8);
    ImgWidth:=Img.Width;
    ImgHeight:=Img.Height;
    Compression:=1;

    if IFD.Orientation in [1..4] then begin
      OrientedWidth:=ImgWidth;
      OrientedHeight:=ImgHeight;
    end else begin
      OrientedWidth:=ImgHeight;
      OrientedHeight:=ImgWidth;
    end;

    {$IFDEF FPC_Debug_Image}
    writeln('TFPWriterTiff.AddImage PhotoMetricInterpretation=',IFD.PhotoMetricInterpretation);
    writeln('TFPWriterTiff.AddImage ImageWidth=',ImgWidth,' ImageHeight=',ImgHeight);
    writeln('TFPWriterTiff.AddImage Orientation=',IFD.Orientation);
    writeln('TFPWriterTiff.AddImage ResolutionUnit=',IFD.ResolutionUnit);
    writeln('TFPWriterTiff.AddImage XResolution=',TiffRationalToStr(IFD.XResolution));
    writeln('TFPWriterTiff.AddImage YResolution=',TiffRationalToStr(IFD.YResolution));
    writeln('TFPWriterTiff.AddImage GrayBits=',GrayBits,' RedBits=',RedBits,' GreenBits=',GreenBits,' BlueBits=',BlueBits,' AlphaBits=',AlphaBits);
    writeln('TFPWriterTiff.AddImage Compression=',Compression);
    writeln('TFPWriterTiff.AddImage Page=',IFD.PageNumber,'/',IFD.PageCount);
    {$ENDIF}

    // required meta entries
    AddEntryShort(262,IFD.PhotoMetricInterpretation);
    AddEntryLong(256,ImgWidth);
    AddEntryLong(257,ImgHeight);
    AddEntryShort(259,Compression);
    AddEntryShort(274,IFD.Orientation);
    AddEntryShort(296,IFD.ResolutionUnit);
    AddEntryRational(282,IFD.XResolution);
    AddEntryRational(283,IFD.YResolution);
    case IFD.PhotoMetricInterpretation of
    0,1:
      begin
        BitsPerSample[0]:=GrayBits;
        SamplesPerPixel:=1;
      end;
    2:
      begin
        BitsPerSample[0]:=RedBits;
        BitsPerSample[1]:=GreenBits;
        BitsPerSample[2]:=BlueBits;
        SamplesPerPixel:=3;
      end;
    end;
    if AlphaBits>0 then begin
      BitsPerSample[SamplesPerPixel]:=AlphaBits;
      inc(SamplesPerPixel);
      // ExtraSamples
      AddEntryShort(338,2);// 2=unassociated alpha
    end;
    // BitsPerSample (required)
    AddEntry(258,3,SamplesPerPixel,@BitsPerSample[0],SamplesPerPixel*2);
    AddEntryShort(277,SamplesPerPixel);

    // BitsPerPixel, BytesPerLine
    BitsPerPixel:=0;
    for i:=0 to SamplesPerPixel-1 do
      inc(BitsPerPixel,BitsPerSample[i]);
    BytesPerLine:=(BitsPerPixel*OrientedWidth+7) div 8;

    // RowsPerStrip (required)
    if OrientedWidth=0 then
      IFD.RowsPerStrip:=8
    else
      IFD.RowsPerStrip:=8192 div BytesPerLine;
    if IFD.RowsPerStrip<1 then
      IFD.RowsPerStrip:=1;
    {$IFDEF FPC_Debug_Image}
    writeln('TFPWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' BytesPerLine=',BytesPerLine,' RowsPerStrip=',IFD.RowsPerStrip);
    {$ENDIF}
    AddEntryLong(278,IFD.RowsPerStrip);

    // optional entries
    if IFD.DocumentName<>'' then
      AddEntryString(269,IFD.DocumentName);
    if IFD.ImageDescription<>'' then
      AddEntryString(270,IFD.ImageDescription);
    if IFD.Make_ScannerManufacturer<>'' then
      AddEntryString(271,IFD.Make_ScannerManufacturer);
    if IFD.Model_Scanner<>'' then
      AddEntryString(272,IFD.Model_Scanner);
    if IFD.Software<>'' then
      AddEntryString(305,IFD.Software);
    if IFD.DateAndTime<>'' then
      AddEntryString(306,IFD.DateAndTime);
    if IFD.Artist<>'' then
      AddEntryString(315,IFD.Artist);
    if IFD.HostComputer<>'' then
      AddEntryString(316,IFD.HostComputer);
    if IFD.PageCount>0 then begin
      Shorts[0]:=IFD.PageNumber;
      Shorts[1]:=IFD.PageCount;
      AddEntry(297,3,2,@Shorts[0],2*SizeOf(Word));
    end;
    if IFD.Copyright<>'' then
      AddEntryString(33432,IFD.Copyright);
    if IFD.TileWidth>0 then
      AddEntryShortOrLong(322,IFD.TileWidth);
    if IFD.TileLength>0 then
      AddEntryShortOrLong(323,IFD.TileLength);

    // ChunkOffsets: ChunkOffsets, StripByteCounts
    ChunkOffsets:=TTiffWriteChunkOffsets.Create;
    AddEntry(ChunkOffsets);
    ChunkCounts:=TTiffWriteEntry.Create;
    ChunkCounts.Tag:=279;
    ChunkCounts.EntryType:=4;
    ChunkOffsets.ChunkByteCounts:=ChunkCounts;
    AddEntry(ChunkCounts);
    if OrientedHeight>0 then begin
      ChunkOffsets.SetCount((OrientedHeight+IFD.RowsPerStrip-1) div IFD.RowsPerStrip);
      // compute ChunkOffsets
      Row:=0;
      ChunkIndex:=0;
      dx:=0;
      dy:=0;
      for y:=0 to OrientedHeight-1 do begin
        if Row=0 then begin
          // allocate Chunk for the next rows
          ChunkBytes:=Min(IFD.RowsPerStrip,OrientedHeight-y)*BytesPerLine;
          //writeln('TFPWriterTiff.AddImage StripIndex=',ChunkIndex,' StripBytes=',ChunkBytes);
          GetMem(Chunk,ChunkBytes);
          FillByte(Chunk^,ChunkBytes,0);
          ChunkOffsets.Chunks[ChunkIndex].Data:=Chunk;
          ChunkOffsets.Chunks[ChunkIndex].Bytes:=ChunkBytes;
          inc(ChunkIndex);
          Run:=Chunk;
        end;
        // write line
        for x:=0 to OrientedWidth-1 do begin
          // Orientation
          case IFD.Orientation of
          1: begin dx:=x; dy:=y; end;// 0,0 is left, top
          2: begin dx:=OrientedWidth-x-1; dy:=y; end;// 0,0 is right, top
          3: begin dx:=OrientedWidth-x-1; dy:=OrientedHeight-y-1; end;// 0,0 is right, bottom
          4: begin dx:=x; dy:=OrientedHeight-y; end;// 0,0 is left, bottom
          5: begin dx:=y; dy:=x; end;// 0,0 is top, left (rotated)
          6: begin dx:=OrientedHeight-y-1; dy:=x; end;// 0,0 is top, right (rotated)
          7: begin dx:=OrientedHeight-y-1; dy:=OrientedWidth-x-1; end;// 0,0 is bottom, right (rotated)
          8: begin dx:=y; dy:=OrientedWidth-x-1; end;// 0,0 is bottom, left (rotated)
          end;
          Col:=Img.Colors[dx,dy];
          case IFD.PhotoMetricInterpretation of
          0,1:
            begin
              // grayscale
              Value:=(DWord(Col.red)+Col.green+Col.blue) div 3;
              if IFD.PhotoMetricInterpretation=0 then
                Value:=$ffff-Value;// 0 is white
              if GrayBits=8 then begin
                Run^:=Value shr 8;
                inc(Run);
              end else if GrayBits=16 then begin
                PWord(Run)^:=Value;
                inc(Run,2);
              end;
              if AlphaBits=8 then begin
                Run^:=Col.alpha shr 8;
                inc(Run);
              end else if AlphaBits=16 then begin
                PWord(Run)^:=Col.alpha;
                inc(Run,2);
              end;
            end;
          2:
            begin
              // RGB
              if RedBits=8 then begin
                Run^:=Col.red shr 8;
                inc(Run);
              end else if RedBits=16 then begin
                PWord(Run)^:=Col.red;
                inc(Run,2);
              end;
              if GreenBits=8 then begin
                Run^:=Col.green shr 8;
                inc(Run);
              end else if GreenBits=16 then begin
                PWord(Run)^:=Col.green;
                inc(Run,2);
              end;
              if BlueBits=8 then begin
                Run^:=Col.blue shr 8;
                inc(Run);
              end else if BlueBits=16 then begin
                PWord(Run)^:=Col.blue;
                inc(Run,2);
              end;
              if AlphaBits=8 then begin
                Run^:=Col.alpha shr 8;
                inc(Run);
              end else if AlphaBits=16 then begin
                PWord(Run)^:=Col.alpha;
                inc(Run,2);
              end;
            end;
          end;
        end;
        // next row
        inc(Row);
        if (Row=IFD.RowsPerStrip) then
          Row:=0;
      end;
    end;

    CurEntries.Sort(@CompareTiffWriteEntries);
  finally
    IFD.Free;
  end;
end;

procedure TFPWriterTiff.SaveToStream(Stream: TStream);
begin
  fStartPos:=Stream.Position;
  // simulate write to compute offsets
  fStream:=nil;
  WriteTiff;
  // write to stream
  fStream:=Stream;
  WriteTiff;
  fStream:=nil;
end;

procedure TFPWriterTiff.InternalWrite(Stream: TStream; Img: TFPCustomImage);
begin
  AddImage(Img);
  SaveToStream(Stream);
end;

procedure TFPWriterTiff.AddEntryString(Tag: word; const s: string);
begin
  if s<>'' then
    AddEntry(Tag,2,length(s)+1,@s[1],length(s)+1)
  else
    AddEntry(Tag,2,0,nil,0);
end;

procedure TFPWriterTiff.AddEntryShort(Tag: word; Value: Word);
begin
  AddEntry(Tag,3,1,@Value,2);
end;

procedure TFPWriterTiff.AddEntryLong(Tag: word; Value: DWord);
begin
  AddEntry(Tag,4,1,@Value,4);
end;

procedure TFPWriterTiff.AddEntryShortOrLong(Tag: word; Value: DWord);
begin
  if Value<High(Word) then
    AddEntryShort(Tag,Value)
  else
    AddEntryLong(Tag,Value);
end;

procedure TFPWriterTiff.AddEntryRational(Tag: word; const Value: TTiffRational
  );
begin
  AddEntry(Tag,5,1,@Value,8);
end;

procedure TFPWriterTiff.AddEntry(Tag: Word; EntryType: Word; EntryCount: DWord;
  Data: Pointer; Bytes: DWord; CopyData: boolean);
var
  Entry: TTiffWriteEntry;
begin
  Entry:=TTiffWriteEntry.Create;
  Entry.Tag:=Tag;
  Entry.EntryType:=EntryType;
  Entry.Count:=EntryCount;
  if CopyData then begin
    if Bytes>0 then begin
      GetMem(Entry.Data,Bytes);
      System.Move(Data^,Entry.Data^,Bytes);
    end else begin
      Entry.Data:=nil;
    end;
  end else
    Entry.Data:=Data;
  Entry.Bytes:=Bytes;
  AddEntry(Entry);
end;

procedure TFPWriterTiff.AddEntry(Entry: TTiffWriteEntry);
var
  List: TFPList;
begin
  List:=TFPList(FEntries[FEntries.Count-1]);
  List.Add(Entry);
end;

procedure TFPWriterTiff.TiffError(Msg: string);
begin
  raise Exception.Create('TFPWriterTiff.TiffError: '+Msg);
end;

constructor TFPWriterTiff.Create;
begin
  inherited Create;
  FEntries:=TFPList.Create;
  FSaveCMYKAsRGB:=true;
end;

destructor TFPWriterTiff.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

procedure TFPWriterTiff.Clear;
begin
  ClearEntries;
end;

{ TTiffWriteEntry }

destructor TTiffWriteEntry.Destroy;
begin
  ReAllocMem(Data,0);
  inherited Destroy;
end;

{ TTiffWriteChunkOffsets }

constructor TTiffWriteChunkOffsets.Create;
begin
  Tag:=273;
  EntryType:=4;
end;

destructor TTiffWriteChunkOffsets.Destroy;
var
  i: Integer;
begin
  if Chunks<>nil then begin
    for i:=0 to Count-1 do
      ReAllocMem(Chunks[i].Data,0);
    ReAllocMem(Chunks,0);
  end;
  inherited Destroy;
end;

procedure TTiffWriteChunkOffsets.SetCount(NewCount: DWord);
var
  Size: DWord;
begin
  {$IFDEF FPC_Debug_Image}
  writeln('TTiffWriteStripOffsets.SetCount OldCount=',Count,' NewCount=',NewCount);
  {$ENDIF}
  Count:=NewCount;
  Size:=Count*SizeOf(TTiffWriteChunk);
  ReAllocMem(Chunks,Size);
  if Size>0 then FillByte(Chunks^,Size,0);
  Size:=Count*SizeOf(DWord);
  // StripOffsets
  ReAllocMem(Data,Size);
  if Size>0 then FillByte(Data^,Size,0);
  Bytes:=Size;
  // ChunkByteCounts
  ReAllocMem(ChunkByteCounts.Data,Size);
  if Size>0 then FillByte(ChunkByteCounts.Data^,Size,0);
  ChunkByteCounts.Count:=Count;
  ChunkByteCounts.Bytes:=Size;
end;

initialization
  if ImageHandlers.ImageWriter[TiffHandlerName]=nil then
    ImageHandlers.RegisterImageWriter (TiffHandlerName, 'tif;tiff', TFPWriterTiff);
end.

