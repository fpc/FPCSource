{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Tiff reader for fpImage.

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
    Compression: packbits, deflate, jpeg, ...
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
  Math, Classes, SysUtils, FPimage, FPTiffCmn, FPWriteTGA;

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

  TTiffWriteStrip = record
    Data: Pointer;
    Bytes: DWord;
  end;
  PTiffWriteStrip = ^TTiffWriteStrip;

  { TTiffWriteStripOffsets }

  TTiffWriteStripOffsets = class(TTiffWriteEntry)
  public
    Strips: PTiffWriteStrip;
    StripByteCounts: TTiffWriteEntry;
    constructor Create;
    destructor Destroy; override;
    procedure SetCount(NewCount: DWord);
  end;

  { TFPWriterTiff }

  TFPWriterTiff = class(TFPCustomImageWriter)
  private
    fStartPos: Int64;
    FEntries: TFPList; // list of TFPList of TTiffWriteEntry
    fStream: TStream;
    fPosition: DWord;
    procedure ClearEntries;
    procedure WriteTiff;
    procedure WriteHeader;
    procedure WriteIDFs;
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
  {$IFDEF VerboseTiffWriter}
  writeln('TFPWriterTiff.WriteTiff fStream=',fStream<>nil);
  {$ENDIF}
  fPosition:=0;
  WriteHeader;
  WriteIDFs;
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

procedure TFPWriterTiff.WriteIDFs;
var
  i: Integer;
  List: TFPList;
  j: Integer;
  Entry: TTiffWriteEntry;
  NextIDFPos: DWord;
begin
  for i:=0 to FEntries.Count-1 do begin
    List:=TFPList(FEntries[i]);
    // write count
    {$IFDEF VerboseTiffWriter}
    writeln('TFPWriterTiff.WriteIDFs Count=',List.Count);
    {$ENDIF}
    WriteWord(List.Count);
    // write array of entries
    for j:=0 to List.Count-1 do begin
      Entry:=TTiffWriteEntry(List[j]);
      WriteEntry(Entry);
    end;
    // write position of next IDF
    if i<FEntries.Count-1 then
      NextIDFPos:=fPosition+4
    else
      NextIDFPos:=0;
    WriteDWord(NextIDFPos);
  end;
end;

procedure TFPWriterTiff.WriteEntry(Entry: TTiffWriteEntry);
var
  PadBytes: DWord;
begin
  {$IFDEF VerboseTiffWriter}
  writeln('TFPWriterTiff.WriteEntry Tag=',Entry.Tag,' Type=',Entry.EntryType,' Count=',Entry.Count,' Bytes=',Entry.Bytes);
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
  Strips: TTiffWriteStripOffsets;
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
      if Entry is TTiffWriteStripOffsets then begin
        Strips:=TTiffWriteStripOffsets(Entry);
        // write Strips
        for k:=0 to Strips.Count-1 do begin
          PDWord(Strips.Data)[k]:=fPosition;
          Bytes:=Strips.Strips[k].Bytes;
          PDWord(Strips.StripByteCounts.Data)[k]:=Bytes;
          {$IFDEF VerboseTiffWriter}
          //writeln('TFPWriterTiff.WriteData Strip fPosition=',fPosition,' Bytes=',Bytes);
          {$ENDIF}
          if Bytes>0 then
            WriteBuf(Strips.Strips[k].Data^,Bytes);
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
  IDF: TTiffIDF;
  GrayBits: Word;
  RedBits: Word;
  GreenBits: Word;
  BlueBits: Word;
  AlphaBits: Word;
  ImgWidth: DWord;
  ImgHeight: DWord;
  Compression: Word;
  BitsPerSample: array[0..3] of Word;
  SamplesPerPixel: Integer;
  BitsPerPixel: DWord;
  i: Integer;
  OrientedWidth: DWord;
  OrientedHeight: DWord;
  y: integer;
  x: Integer;
  StripOffsets: TTiffWriteStripOffsets;
  Row: DWord;
  BytesPerLine: DWord;
  StripBytes: DWord;
  Strip: PByte;
  Run: PByte;
  StripIndex: DWord;
  Col: TFPColor;
  Value: Integer;
  dx: Integer;
  dy: Integer;
  CurEntries: TFPList;
  StripCounts: TTiffWriteEntry;
begin
  StripOffsets:=nil;
  Strip:=nil;
  IDF:=TTiffIDF.Create;
  try
    // add new list of entries
    CurEntries:=TFPList.Create;
    FEntries.Add(CurEntries);

    IDF.PhotoMetricInterpretation:=StrToInt64Def(Img.Extra[TiffPhotoMetric],High(IDF.PhotoMetricInterpretation));
    if not (IDF.PhotoMetricInterpretation in [0,1,2]) then
      TiffError('PhotoMetricInterpretation='+IntToStr(IDF.PhotometricInterpretation)+' not supported');
    IDF.Artist:=Img.Extra[TiffArtist];
    IDF.Copyright:=Img.Extra[TiffCopyright];
    IDF.DocumentName:=Img.Extra[TiffDocumentName];
    IDF.DateAndTime:=Img.Extra[TiffDateTime];
    IDF.ImageDescription:=Img.Extra[TiffImageDescription];
    IDF.Orientation:=StrToIntDef(Img.Extra[TiffOrientation],1);
    if not (IDF.Orientation in [1..8]) then
      IDF.Orientation:=1;
    IDF.ResolutionUnit:=StrToIntDef(Img.Extra[TiffResolutionUnit],2);
    if not (IDF.ResolutionUnit in [1..3]) then
      IDF.ResolutionUnit:=2;
    IDF.XResolution:=StrToTiffRationalDef(Img.Extra[TiffXResolution],TiffRational0);
    IDF.YResolution:=StrToTiffRationalDef(Img.Extra[TiffYResolution],TiffRational0);

    GrayBits:=StrToIntDef(Img.Extra[TiffGrayBits],0);
    RedBits:=StrToIntDef(Img.Extra[TiffRedBits],0);
    GreenBits:=StrToIntDef(Img.Extra[TiffGreenBits],0);
    BlueBits:=StrToIntDef(Img.Extra[TiffBlueBits],0);
    AlphaBits:=StrToIntDef(Img.Extra[TiffAlphaBits],0);
    ImgWidth:=Img.Width;
    ImgHeight:=Img.Height;
    Compression:=1;

    if IDF.Orientation in [1..4] then begin
      OrientedWidth:=ImgWidth;
      OrientedHeight:=ImgHeight;
    end else begin
      OrientedWidth:=ImgHeight;
      OrientedHeight:=ImgWidth;
    end;

    {$IFDEF VerboseTiffWriter}
    writeln('TFPWriterTiff.AddImage PhotoMetricInterpretation=',IDF.PhotoMetricInterpretation);
    writeln('TFPWriterTiff.AddImage ImageWidth=',ImgWidth,' ImageHeight=',ImgHeight);
    writeln('TFPWriterTiff.AddImage Orientation=',IDF.Orientation);
    writeln('TFPWriterTiff.AddImage ResolutionUnit=',IDF.ResolutionUnit);
    writeln('TFPWriterTiff.AddImage XResolution=',TiffRationalToStr(IDF.XResolution));
    writeln('TFPWriterTiff.AddImage YResolution=',TiffRationalToStr(IDF.YResolution));
    writeln('TFPWriterTiff.AddImage GrayBits=',GrayBits,' RedBits=',RedBits,' GreenBits=',GreenBits,' BlueBits=',BlueBits,' AlphaBits=',AlphaBits);
    writeln('TFPWriterTiff.AddImage Compression=',Compression);
    {$ENDIF}

    // required meta entries
    AddEntryShort(262,IDF.PhotoMetricInterpretation);
    AddEntryLong(256,ImgWidth);
    AddEntryLong(257,ImgHeight);
    AddEntryShort(259,Compression);
    AddEntryShort(274,IDF.Orientation);
    AddEntryShort(296,IDF.ResolutionUnit);
    AddEntryRational(282,IDF.XResolution);
    AddEntryRational(283,IDF.YResolution);
    case IDF.PhotoMetricInterpretation of
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

    // RowsPerStrip (required)
    BitsPerPixel:=0;
    for i:=0 to SamplesPerPixel-1 do
      inc(BitsPerPixel,BitsPerSample[i]);
    BytesPerLine:=(BitsPerPixel*OrientedWidth+7) div 8;
    if OrientedWidth=0 then
      IDF.RowsPerStrip:=8
    else
      IDF.RowsPerStrip:=8192 div BytesPerLine;
    if IDF.RowsPerStrip<1 then
      IDF.RowsPerStrip:=1;
    {$IFDEF VerboseTiffWriter}
    writeln('TFPWriterTiff.AddImage BitsPerPixel=',BitsPerPixel,' OrientedWidth=',OrientedWidth,' BytesPerLine=',BytesPerLine,' RowsPerStrip=',IDF.RowsPerStrip);
    {$ENDIF}
    AddEntryLong(278,IDF.RowsPerStrip);

    // optional entries
    if IDF.Artist<>'' then
      AddEntryString(315,IDF.Artist);
    if IDF.Copyright<>'' then
      AddEntryString(33432,IDF.Copyright);
    if IDF.DocumentName<>'' then
      AddEntryString(269,IDF.DocumentName);
    if IDF.DateAndTime<>'' then
      AddEntryString(306,IDF.DateAndTime);
    if IDF.ImageDescription<>'' then
      AddEntryString(270,IDF.ImageDescription);

    // StripOffsets: StripOffsets, StripByteCounts
    StripOffsets:=TTiffWriteStripOffsets.Create;
    AddEntry(StripOffsets);
    StripCounts:=TTiffWriteEntry.Create;
    StripCounts.Tag:=279;
    StripCounts.EntryType:=4;
    StripOffsets.StripByteCounts:=StripCounts;
    AddEntry(StripCounts);
    if OrientedHeight>0 then begin
      StripOffsets.SetCount((OrientedHeight+IDF.RowsPerStrip-1) div IDF.RowsPerStrip);
      // compute StripOffsets
      Row:=0;
      StripIndex:=0;
      dx:=0;
      dy:=0;
      for y:=0 to OrientedHeight-1 do begin
        if Row=0 then begin
          // allocate Strip for the next rows
          StripBytes:=Min(IDF.RowsPerStrip,OrientedHeight-y)*BytesPerLine;
          //writeln('TFPWriterTiff.AddImage StripIndex=',StripIndex,' StripBytes=',StripBytes);
          GetMem(Strip,StripBytes);
          FillByte(Strip^,StripBytes,0);
          StripOffsets.Strips[StripIndex].Data:=Strip;
          StripOffsets.Strips[StripIndex].Bytes:=StripBytes;
          inc(StripIndex);
          Run:=Strip;
        end;
        // write line
        for x:=0 to OrientedWidth-1 do begin
          // Orientation
          case IDF.Orientation of
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
          case IDF.PhotoMetricInterpretation of
          0,1:
            begin
              // grayscale
              Value:=(DWord(Col.red)+Col.green+Col.blue) div 3;
              if IDF.PhotoMetricInterpretation=0 then
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
        if (Row=IDF.RowsPerStrip) then
          Row:=0;
      end;
    end;

    CurEntries.Sort(@CompareTiffWriteEntries);
  finally
    IDF.Free;
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

{ TTiffWriteStripOffsets }

constructor TTiffWriteStripOffsets.Create;
begin
  Tag:=273;
  EntryType:=4;
end;

destructor TTiffWriteStripOffsets.Destroy;
var
  i: Integer;
begin
  if Strips<>nil then begin
    for i:=0 to Count-1 do
      ReAllocMem(Strips[i].Data,0);
    ReAllocMem(Strips,0);
  end;
  inherited Destroy;
end;

procedure TTiffWriteStripOffsets.SetCount(NewCount: DWord);
var
  Size: DWord;
begin
  {$IFDEF VerboseTiffWriter}
  writeln('TTiffWriteStripOffsets.SetCount OldCount=',Count,' NewCount=',NewCount);
  {$ENDIF}
  Count:=NewCount;
  Size:=Count*SizeOf(TTiffWriteStrip);
  ReAllocMem(Strips,Size);
  if Size>0 then FillByte(Strips^,Size,0);
  Size:=Count*SizeOf(DWord);
  // StripOffsets
  ReAllocMem(Data,Size);
  if Size>0 then FillByte(Data^,Size,0);
  Bytes:=Size;
  // StripByteCounts
  ReAllocMem(StripByteCounts.Data,Size);
  if Size>0 then FillByte(StripByteCounts.Data^,Size,0);
  StripByteCounts.Count:=Count;
  StripByteCounts.Bytes:=Size;
end;

end.

