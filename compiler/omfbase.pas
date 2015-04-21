{
    Copyright (c) 2015 by Nikolay Nikolov

    Contains Relocatable Object Module Format (OMF) definitions
    This is the object format used on the i8086-msdos platform.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit omfbase;

{$i fpcdefs.inc}

interface
{$H+}
  uses
    cclasses,
    owbase;

  const
    { OMF record types }
    RT_THEADR    = $80;  { Translator Header Record }
    RT_LHEADR    = $82;  { Library Module Header Record }
    RT_COMENT    = $88;  { Comment Record }
    RT_MODEND    = $8A;  { Module End Record }
    RT_MODEND32  = $8B;
    RT_EXTDEF    = $8C;  { External Names Definition Record }
    RT_PUBDEF    = $90;  { Public Names Definition Record }
    RT_PUBDEF32  = $91;
    RT_LINNUM    = $94;  { Line Numbers Record }
    RT_LINNUM32  = $95;
    RT_LNAMES    = $96;  { List of Names Record }
    RT_SEGDEF    = $98;  { Segment Definition Record }
    RT_SEGDEF32  = $99;
    RT_GRPDEF    = $9A;  { Group Definition Record }
    RT_FIXUPP    = $9C;  { Fixup Record }
    RT_FIXUPP32  = $9D;
    RT_LEDATA    = $A0;  { Logical Enumerated Data Record }
    RT_LEDATA32  = $A1;
    RT_LIDATA    = $A2;  { Logical Iterated Data Record }
    RT_LIDATA32  = $A3;
    RT_COMDEF    = $B0;  { Communal Names Definition Record }
    RT_BAKPAT    = $B2;  { Backpatch Record }
    RT_BAKPAT32  = $B3;
    RT_LEXTDEF   = $B4;  { Local External Names Definition Record }
    RT_LEXTDEF32 = $B5;
    RT_LPUBDEF   = $B6;  { Local Public Names Definition Record }
    RT_LPUBDEF32 = $B7;
    RT_LCOMDEF   = $B8;  { Local Communal Names Definition Record }
    RT_CEXTDEF   = $BC;  { COMDAT External Names Definition Record }
    RT_COMDAT    = $C2;  { Initialized Communal Data Record }
    RT_COMDAT32  = $C3;
    RT_LINSYM    = $C4;  { Symbol Line Numbers Record }
    RT_LINSYM32  = $C5;
    RT_ALIAS     = $C6;  { Alias Definition Record }
    RT_NBKPAT    = $C8;  { Named Backpatch Record }
    RT_NBKPAT32  = $C9;
    RT_LLNAMES   = $CA;  { Local Logical Names Definition Record }
    RT_VERNUM    = $CC;  { OMF Version Number Record }
    RT_VENDEXT   = $CE;  { Vendor-specific OMF Extension Record }
    RT_LIBHEAD   = $F0;  { Library Header Record }
    RT_LIBEND    = $F1;  { Library End Record (marks end of objects and beginning of dictionary) }

    { OMF comment class }
    CC_Translator               = $00; { language translator (compiler or assembler) name }
    CC_IntelCopyright           = $01;
    CC_IntelReservedRangeStart  = $02;
    CC_IntelReservedRangeEnd    = $9B;
    CC_LibrarySpecifierObsolete = $81;
    CC_MsDosVersionObsolete     = $9C;
    CC_MemoryModel              = $9D;
    CC_DOSSEG                   = $9E;
    CC_DefaultLibrarySearchName = $9F;
    CC_OmfExtension             = $A0;
    CC_NewOmfExtension          = $A1;
    CC_LinkPassSeparator        = $A2;
    CC_LIBMOD                   = $A3;
    CC_EXESTR                   = $A4;
    CC_INCERR                   = $A6;
    CC_NOPAD                    = $A7;
    CC_WKEXT                    = $A8;
    CC_LZEXT                    = $A9;
    CC_Comment                  = $DA;
    CC_Compiler                 = $DB;
    CC_Date                     = $DC;
    CC_Timestamp                = $DD;
    CC_User                     = $DF;
    CC_DependencyFileBorland    = $E9;
    CC_CommandLineMicrosoft     = $FF;

  type
    TOmfSegmentAlignment = (
      saAbsolute                = 0,
      saRelocatableByteAligned  = 1,
      saRelocatableWordAligned  = 2,
      saRelocatableParaAligned  = 3,
      saRelocatablePageAligned  = 4,  { 32-bit linkers extension }
      saRelocatableDWordAligned = 5,  { 32-bit linkers extension }
      saNotSupported            = 6,
      saNotDefined              = 7);
    TOmfSegmentCombination = (
      scPrivate   = 0,
      scReserved1 = 1,
      scPublic    = 2,
      scReserved3 = 3,
      scPublic4   = 4,  { same as scPublic }
      scStack     = 5,
      scCommon    = 6,
      scPublic7   = 7); { same as scPublic }
    TOmfSegmentUse = (suUse16, suUse32);

    TOmfFixupThread = (ftThread0, ftThread1, ftThread2, ftThread3);

    TOmfFixupMode = (fmSelfRelative, fmSegmentRelative);
    TOmfFixupLocationType = (
      fltLoByte                 = 0,  { low 8 bits of 16-bit offset }
      fltOffset                 = 1,  { 16-bit offset }
      fltBase                   = 2,  { 16-bit base (segment) }
      fltFarPointer             = 3,  { 16-bit base:16-bit offset }
      fltHiByte                 = 4,  { high 8 bits of 16-bit offset }
      fltLoaderResolvedOffset   = 5,  { PharLap: Offset32 }
      fltUndefined6             = 6,  { PharLap: Pointer48 }
      fltUndefined7             = 7,
      fltUndefined8             = 8,
      fltOffset32               = 9,  { 32-bit offset }
      fltUndefined10            = 10,
      fltFarPointer48           = 11, { 16-bit base:32-bit offset }
      fltUndefined12            = 12,
      fltLoaderResolvedOffset32 = 13,
      fltUndefined14            = 14,
      fltUndefined15            = 15);
    TOmfFixupFrameMethod = (
      ffmSegmentIndex  = 0,  { SI(<segment name>) - The frame is the canonic frame of the logical
                                                    segment segment defined by the index }
      ffmGroupIndex    = 1,  { GI(<group name>)   - The frame is the canonic frame of the group
                                                    (= the canonic frame of the logical segment from the group,
                                                    located at the lowest memory address) }
      ffmExternalIndex = 2,  { EI(<symbol name>)  - The frame is determined depending on the external's public definition:
                                                     * if the symbol is defined relative to a logical segment and no defined group,
                                                       the frame of the logical segment is used
                                                     * if the symbol is defined absolutely, without reference to a logical segment and
                                                       no defined group, the FRAME NUMBER from the symbol's PUBDEF record is used
                                                     * regardless of how the symbol is specified, if there's an associated group,
                                                       that group's canonic frame is used }
      ffmFrameNumber   = 3,  { <FRAME NUMBER> - The frame is a directly specified constant. }
      ffmLocation      = 4,  { LOCATION - The frame is determined by the location (i.e. the canonic frame of the logical
                                          segment where the fixup location is) }
      ffmTarget        = 5,  { TARGET - The frame is determined by the target. }
      ffmNone          = 6,  { NONE - There is no frame. Used for 8089 self-relative references. }
      ffmUndefined     = 7);
    TOmfFixupTargetMethod = (
      ftmSegmentIndex        = 0,  { SI(<segment name>),<displacement> }
      ftmGroupIndex          = 1,  { GI(<group name>),<displacement> }
      ftmExternalIndex       = 2,  { EI(<symbol name>),<displacement> }
      ftmFrameNumber         = 3,  { <FRAME NUMBER>,<displacement> }
      ftmSegmentIndexNoDisp  = 4,  { SI(<segment name>) }
      ftmGroupIndexNoDisp    = 5,  { GI(<group name>) }
      ftmExternalIndexNoDisp = 6,  { EI(<symbol name>) }
      ftmFrameNumberNoDisp   = 7); { <FRAME NUMBER> }

    { TOmfOrderedNameCollection }

    TOmfOrderedNameCollection = class
    private
      FStringList: array of string;
      function GetCount: Integer;
      function GetString(Index: Integer): string;
      procedure SetString(Index: Integer; AValue: string);
    public
      function Add(const S: string): Integer;
      procedure Clear;
      property Strings [Index: Integer]: string read GetString write SetString; default;
      property Count: Integer read GetCount;
    end;

    { TOmfRawRecord }

    TOmfRawRecord = class
    private
      function GetChecksumByte: Byte;
      function GetRecordLength: Word;
      function GetRecordType: Byte;
      procedure SetChecksumByte(AValue: Byte);
      procedure SetRecordLength(AValue: Word);
      procedure SetRecordType(AValue: Byte);
    public
      RawData: array [-3..65535] of Byte;
      property RecordType: Byte read GetRecordType write SetRecordType;
      property RecordLength: Word read GetRecordLength write SetRecordLength;

      function ReadStringAt(Offset: Integer; out s: string): Integer;
      function WriteStringAt(Offset: Integer; s: string): Integer;

      function ReadIndexedRef(Offset: Integer; out IndexedRef: Integer): Integer;
      function WriteIndexedRef(Offset: Integer; IndexedRef: Integer): Integer;

      procedure CalculateChecksumByte;
      function VerifyChecksumByte: boolean;
      property ChecksumByte: Byte read GetChecksumByte write SetChecksumByte;

      procedure ReadFrom(aReader: TObjectReader);
      procedure ReadFrom(aReader: TDynamicArray);
      procedure WriteTo(aWriter: TObjectWriter);
      procedure WriteTo(aWriter: TDynamicArray);
    end;

    { TOmfParsedRecord }

    TOmfParsedRecord = class
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);virtual;abstract;
      procedure EncodeTo(RawRecord: TOmfRawRecord);virtual;abstract;
    end;

    { TOmfRecord_THEADR }

    TOmfRecord_THEADR = class(TOmfParsedRecord)
    private
      FModuleName: string;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property ModuleName: string read FModuleName write FModuleName;
    end;

    { TOmfRecord_COMENT }

    TOmfRecord_COMENT = class(TOmfParsedRecord)
    private
      FCommentType: Byte;
      FCommentClass: Byte;
      FCommentString: string;
      function GetNoList: Boolean;
      function GetNoPurge: Boolean;
      procedure SetNoList(AValue: Boolean);
      procedure SetNoPurge(AValue: Boolean);
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property CommentType: Byte read FCommentType write FCommentType;
      property CommentClass: Byte read FCommentClass write FCommentClass;
      property CommentString: string read FCommentString write FCommentString;
      property NoPurge: Boolean read GetNoPurge write SetNoPurge;
      property NoList: Boolean read GetNoList write SetNoList;
    end;

    { TOmfRecord_LNAMES }

    TOmfRecord_LNAMES = class(TOmfParsedRecord)
    private
      FNames: TOmfOrderedNameCollection;
      FNextIndex: Integer;
    public
      constructor Create;

      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property Names: TOmfOrderedNameCollection read FNames write FNames;
      property NextIndex: Integer read FNextIndex write FNextIndex;
    end;

    { TOmfRecord_SEGDEF }

    TOmfRecord_SEGDEF = class(TOmfParsedRecord)
    private
      FAlignment: TOmfSegmentAlignment;
      FCombination: TOmfSegmentCombination;
      FUse: TOmfSegmentUse;
      FFrameNumber: Word;
      FOffset: Byte;
      FIs32Bit: Boolean;
      FSegmentLength: Int64;  { int64, because it can be 2**32 }
      FSegmentNameIndex: Integer;
      FClassNameIndex: Integer;
      FOverlayNameIndex: Integer;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property Alignment: TOmfSegmentAlignment read FAlignment write FAlignment;
      property Combination: TOmfSegmentCombination read FCombination write FCombination;
      property Use: TOmfSegmentUse read FUse write FUse;
      property FrameNumber: Word read FFrameNumber write FFrameNumber;
      property Offset: Byte read FOffset write FOffset;
      property Is32Bit: Boolean read FIs32Bit write FIs32Bit;
      property SegmentLength: Int64 read FSegmentLength write FSegmentLength;
      property SegmentNameIndex: Integer read FSegmentNameIndex write FSegmentNameIndex;
      property ClassNameIndex: Integer read FClassNameIndex write FClassNameIndex;
      property OverlayNameIndex: Integer read FOverlayNameIndex write FOverlayNameIndex;
    end;

    TSegmentList = array of Integer;

    { TOmfRecord_GRPDEF }

    TOmfRecord_GRPDEF = class(TOmfParsedRecord)
    private
      FGroupNameIndex: Integer;
      FSegmentList: TSegmentList;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property GroupNameIndex: Integer read FGroupNameIndex write FGroupNameIndex;
      property SegmentList: TSegmentList read FSegmentList write FSegmentList;
    end;

    { TOmfPublicNameElement }

    TOmfPublicNameElement = class(TFPHashObject)
    private
      FPublicOffset: DWord;
      FTypeIndex: Integer;
    public
      function GetLengthInFile(Is32Bit: Boolean): Integer;

      property PublicOffset: DWord read FPublicOffset write FPublicOffset;
      property TypeIndex: Integer read FTypeIndex write FTypeIndex;
    end;

    { TOmfRecord_PUBDEF }

    TOmfRecord_PUBDEF = class(TOmfParsedRecord)
    private
      FIs32Bit: Boolean;
      FBaseGroupIndex: Integer;
      FBaseSegmentIndex: Integer;
      FBaseFrame: Word;

      FPublicNames: TFPHashObjectList;
      FNextIndex: Integer;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property Is32Bit: Boolean read FIs32Bit write FIs32Bit;
      property BaseGroupIndex: Integer read FBaseGroupIndex write FBaseGroupIndex;
      property BaseSegmentIndex: Integer read FBaseSegmentIndex write FBaseSegmentIndex;
      property BaseFrame: Word read FBaseFrame write FBaseFrame;

      property PublicNames: TFPHashObjectList read FPublicNames write FPublicNames;
      property NextIndex: Integer read FNextIndex write FNextIndex;
    end;

    { TOmfExternalNameElement }

    TOmfExternalNameElement = class(TFPHashObject)
    private
      FTypeIndex: Integer;
    public
      function GetLengthInFile: Integer;

      property TypeIndex: Integer read FTypeIndex write FTypeIndex;
    end;

    { TOmfRecord_EXTDEF }

    TOmfRecord_EXTDEF = class(TOmfParsedRecord)
    private
      FExternalNames: TFPHashObjectList;
      FNextIndex: Integer;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property ExternalNames: TFPHashObjectList read FExternalNames write FExternalNames;
      property NextIndex: Integer read FNextIndex write FNextIndex;
    end;

    { TOmfRecord_MODEND }

    TOmfRecord_MODEND = class(TOmfParsedRecord)
    private
      FIs32Bit: Boolean;
      FIsMainModule: Boolean;
      FHasStartAddress: Boolean;
      FSegmentBit: Boolean;
      FLogicalStartAddress: Boolean;

      FFrameMethod: TOmfFixupFrameMethod;
      FFrameDatum: Integer;
      FTargetMethod: TOmfFixupTargetMethod;
      FTargetDatum: Integer;
      FTargetDisplacement: DWord;

      FPhysFrameNumber: Word;
      FPhysOffset: DWord;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property Is32Bit: Boolean read FIs32Bit write FIs32Bit;
      property IsMainModule: Boolean read FIsMainModule write FIsMainModule;
      property HasStartAddress: Boolean read FHasStartAddress write FHasStartAddress;
      property SegmentBit: Boolean read FSegmentBit write FSegmentBit;
      property LogicalStartAddress: Boolean read FLogicalStartAddress write FLogicalStartAddress;

      { properties, specifying a logical start address (used when LogicalStartAddress=true) }
      property FrameMethod: TOmfFixupFrameMethod read FFrameMethod write FFrameMethod;
      property FrameDatum: Integer read FFrameDatum write FFrameDatum;
      property TargetMethod: TOmfFixupTargetMethod read FTargetMethod write FTargetMethod;
      property TargetDatum: Integer read FTargetDatum write FTargetDatum;
      property TargetDisplacement: DWord read FTargetDisplacement write FTargetDisplacement;

      { properties, specifying a physical start address (used when LogicalStartAddress=false) }
      property PhysFrameNumber: Word read FPhysFrameNumber write FPhysFrameNumber;
      property PhysOffset: DWord read FPhysOffset write FPhysOffset;
    end;

    { TOmfSubRecord_FIXUP }

    TOmfSubRecord_FIXUP = class
    private
      FIs32Bit: Boolean;
      FMode: TOmfFixupMode;
      FLocationType: TOmfFixupLocationType;
      FLocationOffset: DWord;
      FDataRecordStartOffset: DWord;
      FTargetDeterminedByThread: Boolean;
      FTargetThread: TOmfFixupThread;
      FTargetThreadDisplacementPresent: Boolean;
      FTargetMethod: TOmfFixupTargetMethod;
      FTargetDatum: Integer;
      FTargetDisplacement: DWord;
      FFrameDeterminedByThread: Boolean;
      FFrameThread: TOmfFixupThread;
      FFrameMethod: TOmfFixupFrameMethod;
      FFrameDatum: Integer;
      function GetDataRecordOffset: Integer;
      function GetLocationSize: Integer;
      procedure SetDataRecordOffset(AValue: Integer);
    public
      function ReadAt(RawRecord: TOmfRawRecord; Offset: Integer): Integer;
      function WriteAt(RawRecord: TOmfRawRecord; Offset: Integer): Integer;

      property Is32Bit: Boolean read FIs32Bit write FIs32Bit;
      property Mode: TOmfFixupMode read FMode write FMode;
      property LocationType: TOmfFixupLocationType read FLocationType write FLocationType;
      property LocationOffset: DWord read FLocationOffset write FLocationOffset;
      property LocationSize: Integer read GetLocationSize;
      property DataRecordStartOffset: DWord read FDataRecordStartOffset write FDataRecordStartOffset;
      property DataRecordOffset: Integer read GetDataRecordOffset write SetDataRecordOffset;
      property TargetDeterminedByThread: Boolean read FTargetDeterminedByThread write FTargetDeterminedByThread;
      property TargetThread: TOmfFixupThread read FTargetThread write FTargetThread;
      property TargetThreadDisplacementPresent: Boolean read FTargetThreadDisplacementPresent write FTargetThreadDisplacementPresent;
      property TargetMethod: TOmfFixupTargetMethod read FTargetMethod write FTargetMethod;
      property TargetDatum: Integer read FTargetDatum write FTargetDatum;
      property TargetDisplacement: DWord read FTargetDisplacement write FTargetDisplacement;
      property FrameDeterminedByThread: Boolean read FFrameDeterminedByThread write FFrameDeterminedByThread;
      property FrameThread: TOmfFixupThread read FFrameThread write FFrameThread;
      property FrameMethod: TOmfFixupFrameMethod read FFrameMethod write FFrameMethod;
      property FrameDatum: Integer read FFrameDatum write FFrameDatum;
    end;

    { TOmfRecord_LIBHEAD }

    TOmfRecord_LIBHEAD = class(TOmfParsedRecord)
    private
      FPageSize: Integer;
      FDictionaryOffset: DWord;
      FDictionarySizeInBlocks: Word;
      FFlags: Byte;
      function IsCaseSensitive: Boolean;
      procedure SetCaseSensitive(AValue: Boolean);
      procedure SetPageSize(AValue: Integer);
    public
      constructor Create;
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      property PageSize: Integer read FPageSize write SetPageSize;
      property DictionaryOffset: DWord read FDictionaryOffset write FDictionaryOffset;
      property DictionarySizeInBlocks: Word read FDictionarySizeInBlocks write FDictionarySizeInBlocks;
      property Flags: Byte read FFlags write FFlags;
      property CaseSensitive: Boolean read IsCaseSensitive write SetCaseSensitive;
    end;

    { TOmfRecord_LIBEND }

    TOmfRecord_LIBEND = class(TOmfParsedRecord)
    private
      FPaddingBytes: Word;
    public
      procedure DecodeFrom(RawRecord: TOmfRawRecord);override;
      procedure EncodeTo(RawRecord: TOmfRawRecord);override;

      procedure CalculatePaddingBytes(RecordStartOffset: DWord);
      property PaddingBytes: Word read FPaddingBytes write FPaddingBytes;
    end;

    TOmfLibHash = record
      block_x: Integer;
      block_d: Integer;
      bucket_x: Integer;
      bucket_d: Integer;
    end;

  function compute_omf_lib_hash(const name: string; blocks: Integer): TOmfLibHash;

implementation

  uses
    cutils,
    verbose;

  { TOmfOrderedNameCollection }

  function TOmfOrderedNameCollection.GetString(Index: Integer): string;
    begin
      Result:=FStringList[Index-1];
    end;

  function TOmfOrderedNameCollection.GetCount: Integer;
    begin
      Result:=Length(FStringList);
    end;

  procedure TOmfOrderedNameCollection.SetString(Index: Integer; AValue: string);
    begin
      FStringList[Index-1]:=AValue;
    end;

  function TOmfOrderedNameCollection.Add(const S: string): Integer;
    begin
      Result:=Length(FStringList)+1;
      SetLength(FStringList,Result);
      FStringList[Result-1]:=S;
    end;

  procedure TOmfOrderedNameCollection.Clear;
    begin
      SetLength(FStringList,0);
    end;

  { TOmfRawRecord }

  function TOmfRawRecord.GetRecordType: Byte;
    begin
      Result:=RawData[-3];
    end;

  procedure TOmfRawRecord.SetRecordType(AValue: Byte);
    begin
      RawData[-3]:=AValue;
    end;

  function TOmfRawRecord.GetRecordLength: Word;
    begin
      Result:=RawData[-2] or (RawData[-1] shl 8);
    end;

  procedure TOmfRawRecord.SetRecordLength(AValue: Word);
    begin
      RawData[-2]:=Byte(AValue);
      RawData[-1]:=Byte(AValue shr 8);
    end;

  function TOmfRawRecord.ReadStringAt(Offset: Integer; out s: string): Integer;
    var
      len: Byte;
    begin
      len:=RawData[Offset];
      Result:=Offset+len+1;
      if result>RecordLength then
        internalerror(2015033103);
      SetLength(s, len);
      UniqueString(s);
      Move(RawData[Offset+1],s[1],len);
    end;

  function TOmfRawRecord.WriteStringAt(Offset: Integer; s: string): Integer;
    begin
      if Length(s)>255 then
        internalerror(2015033101);
      result:=Offset+Length(s)+1;
      if result>High(RawData) then
        internalerror(2015033102);
      RawData[Offset]:=Length(s);
      Move(s[1], RawData[Offset+1], Length(s));
    end;

  function TOmfRawRecord.ReadIndexedRef(Offset: Integer; out IndexedRef: Integer): Integer;
    begin
      Result:=Offset+1;
      if result>RecordLength then
        internalerror(2015033103);
      IndexedRef:=RawData[Offset];
      if IndexedRef<=$7f then
        exit;
      Result:=Offset+2;
      if result>RecordLength then
        internalerror(2015033103);
      IndexedRef:=((IndexedRef and $7f) shl 8)+RawData[Offset+1];
    end;

  function TOmfRawRecord.WriteIndexedRef(Offset: Integer; IndexedRef: Integer): Integer;
    begin
      if (IndexedRef<0) or (IndexedRef>$7FFF) then
        internalerror(2015040303);
      if IndexedRef<=$7f then
        begin
          Result:=Offset+1;
          if Result>High(RawData) then
            internalerror(2015033102);
          RawData[Offset]:=IndexedRef;
        end
      else
        begin
          Result:=Offset+2;
          if Result>High(RawData) then
            internalerror(2015033102);
          RawData[Offset]:=$80+(IndexedRef shr 8);
          RawData[Offset+1]:=Byte(IndexedRef);
        end;
    end;

  function TOmfRawRecord.GetChecksumByte: Byte;
    begin
      if RecordLength>0 then
        Result:=RawData[RecordLength-1]
      else
        Result:=0;
    end;

  procedure TOmfRawRecord.SetChecksumByte(AValue: Byte);
    begin
      if RecordLength>0 then
        RawData[RecordLength-1]:=AValue;
    end;

  procedure TOmfRawRecord.CalculateChecksumByte;
    var
      I: Integer;
      b: Byte;
    begin
      b:=0;
      for I:=-3 to RecordLength-2 do
        b:=byte(b+RawData[I]);
      SetChecksumByte($100-b);
    end;

  function TOmfRawRecord.VerifyChecksumByte: boolean;
    var
      I: Integer;
      b: Byte;
    begin
      { according to the OMF spec, some tools always write a 0 rather than
        computing the checksum, so it should also be accepted as correct }
      if ChecksumByte=0 then
        exit(true);
      b:=0;
      for I:=-3 to RecordLength-1 do
        b:=byte(b+RawData[I]);
      Result:=(b=0);
    end;

  procedure TOmfRawRecord.ReadFrom(aReader: TObjectReader);
    begin
      aReader.read(RawData, 3);
      aReader.read(RawData[0], RecordLength);
    end;

  procedure TOmfRawRecord.ReadFrom(aReader: TDynamicArray);
    begin
      aReader.read(RawData, 3);
      aReader.read(RawData[0], RecordLength);
    end;

  procedure TOmfRawRecord.WriteTo(aWriter: TObjectWriter);
    begin
      aWriter.write(RawData, RecordLength+3);
    end;

  procedure TOmfRawRecord.WriteTo(aWriter: TDynamicArray);
    begin
      aWriter.write(RawData, RecordLength+3);
    end;

  { TOmfRecord_THEADR }

  procedure TOmfRecord_THEADR.DecodeFrom(RawRecord: TOmfRawRecord);
    begin
      if RawRecord.RecordType<>RT_THEADR then
        internalerror(2015040301);
      RawRecord.ReadStringAt(0,FModuleName);
    end;

  procedure TOmfRecord_THEADR.EncodeTo(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
    begin
      RawRecord.RecordType:=RT_THEADR;
      NextOfs:=RawRecord.WriteStringAt(0,ModuleName);
      RawRecord.RecordLength:=NextOfs+1;
      RawRecord.CalculateChecksumByte;
    end;

  { TOmfRecord_COMENT }

  function TOmfRecord_COMENT.GetNoList: Boolean;
    begin
      Result:=(CommentType and $40)<>0;
    end;

  function TOmfRecord_COMENT.GetNoPurge: Boolean;
    begin
      Result:=(CommentType and $80)<>0;
    end;

  procedure TOmfRecord_COMENT.SetNoList(AValue: Boolean);
    begin
      if AValue then
        CommentType:=CommentType or $40
      else
        CommentType:=CommentType and $BF;
    end;

  procedure TOmfRecord_COMENT.SetNoPurge(AValue: Boolean);
    begin
      if AValue then
        CommentType:=CommentType or $80
      else
        CommentType:=CommentType and $7F;
    end;

  procedure TOmfRecord_COMENT.DecodeFrom(RawRecord: TOmfRawRecord);
    begin
      if RawRecord.RecordType<>RT_COMENT then
        internalerror(2015040301);
      if RawRecord.RecordLength<3 then
        internalerror(2015033104);
      CommentType:=RawRecord.RawData[0];
      CommentClass:=RawRecord.RawData[1];
      SetLength(FCommentString,RawRecord.RecordLength-3);
      UniqueString(FCommentString);
      Move(RawRecord.RawData[2],FCommentString[1],Length(FCommentString));
    end;

  procedure TOmfRecord_COMENT.EncodeTo(RawRecord: TOmfRawRecord);
    begin
      RawRecord.RecordType:=RT_COMENT;
      if (Length(FCommentString)+3)>High(RawRecord.RawData) then
        internalerror(2015033105);
      RawRecord.RecordLength:=Length(FCommentString)+3;
      RawRecord.RawData[0]:=CommentType;
      RawRecord.RawData[1]:=CommentClass;
      Move(FCommentString[1],RawRecord.RawData[2],Length(FCommentString));
      RawRecord.CalculateChecksumByte;
    end;

  { TOmfRecord_LNAMES }

  constructor TOmfRecord_LNAMES.Create;
    begin
      FNextIndex:=1;
    end;

  procedure TOmfRecord_LNAMES.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
      Name: string;
    begin
      if RawRecord.RecordType<>RT_LNAMES then
        internalerror(2015040301);
      NextOfs:=0;
      while NextOfs<(RawRecord.RecordLength-1) do
        begin
          NextOfs:=RawRecord.ReadStringAt(NextOfs,Name);
          Names.Add(Name);
        end;
    end;

  procedure TOmfRecord_LNAMES.EncodeTo(RawRecord: TOmfRawRecord);
    const
      RecordLengthLimit = 1024;
    var
      Len,LastIncludedIndex,NextOfs,I: Integer;
    begin
      RawRecord.RecordType:=RT_LNAMES;

      { find out how many strings can we include until we reach the length limit }
      Len:=1;
      LastIncludedIndex:=NextIndex-1;
      repeat
        Inc(LastIncludedIndex);
        Inc(Len,Length(Names[LastIncludedIndex])+1);
      until (LastIncludedIndex>=Names.Count) or ((Len+Length(Names[LastIncludedIndex+1])+1)>=RecordLengthLimit);

      { write the strings... }
      NextOfs:=0;
      for I:=NextIndex to LastIncludedIndex do
        NextOfs:=RawRecord.WriteStringAt(NextOfs,Names[I]);
      RawRecord.RecordLength:=Len;
      RawRecord.CalculateChecksumByte;

      { update NextIndex }
      NextIndex:=LastIncludedIndex+1;
    end;

  { TOmfRecord_SEGDEF }

  procedure TOmfRecord_SEGDEF.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      B: Byte;
      Big: Boolean;
      NextOfs: Integer;
      MinLen: Integer;
    begin
      if not (RawRecord.RecordType in [RT_SEGDEF,RT_SEGDEF32]) then
        internalerror(2015040301);
      Is32Bit:=RawRecord.RecordType=RT_SEGDEF32;

      MinLen:=7; { b(1)+seglength(2..4)+segnameindex(1..2)+classnameindex(1..2)+overlaynameindex(1..2)+checksum }
      if Is32Bit then
        inc(MinLen,2);
      if RawRecord.RecordLength<MinLen then
        internalerror(2015040305);
      B:=RawRecord.RawData[0];
      Alignment:=TOmfSegmentAlignment(B shr 5);
      Combination:=TOmfSegmentCombination((B shr 2) and 7);
      Big:=(B and 2)<>0;
      Use:=TOmfSegmentUse(B and 1);
      NextOfs:=1;
      if Alignment=saAbsolute then
        begin
          inc(MinLen,3);
          if RawRecord.RecordLength<MinLen then
            internalerror(2015040305);
          FrameNumber:=RawRecord.RawData[1]+(RawRecord.RawData[2] shl 8);
          Offset:=RawRecord.RawData[3];
          NextOfs:=4;
        end
      else
        begin
          FrameNumber:=0;
          Offset:=0;
        end;
      if Is32Bit then
        begin
          SegmentLength:=RawRecord.RawData[NextOfs]+
            (RawRecord.RawData[NextOfs+1] shl 8)+
            (RawRecord.RawData[NextOfs+2] shl 16)+
            (RawRecord.RawData[NextOfs+3] shl 24);
          if Big then
            if SegmentLength=0 then
              SegmentLength:=4294967296
            else
              internalerror(2015040306);
          Inc(NextOfs,4);
        end
      else
        begin
          SegmentLength:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8);
          if Big then
            if SegmentLength=0 then
              SegmentLength:=65536
            else
              internalerror(2015040306);
          Inc(NextOfs,2);
        end;
      NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FSegmentNameIndex);
      NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FClassNameIndex);
      NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FOverlayNameIndex);
    end;

  procedure TOmfRecord_SEGDEF.EncodeTo(RawRecord: TOmfRawRecord);
    var
      Big: Boolean;
      NextOfs: Integer;
    begin
      if Is32Bit then
        begin
          RawRecord.RecordType:=RT_SEGDEF32;
          if SegmentLength>4294967296 then
            internalerror(2015040302);
          Big:=SegmentLength=4294967296;
        end
      else
        begin
          RawRecord.RecordType:=RT_SEGDEF;
          if SegmentLength>65536 then
            internalerror(2015040302);
          Big:=SegmentLength=65536;
        end;
      RawRecord.RawData[0]:=(Ord(Alignment) shl 5) or (Ord(Combination) shl 2) or (Ord(Big) shl 1) or Ord(Use);
      NextOfs:=1;
      if Alignment=saAbsolute then
        begin
          RawRecord.RawData[1]:=Byte(FrameNumber);
          RawRecord.RawData[2]:=Byte(FrameNumber shr 8);
          RawRecord.RawData[3]:=Offset;
          NextOfs:=4;
        end;
      if Is32Bit then
        begin
          RawRecord.RawData[NextOfs]:=Byte(SegmentLength);
          RawRecord.RawData[NextOfs+1]:=Byte(SegmentLength shr 8);
          RawRecord.RawData[NextOfs+2]:=Byte(SegmentLength shr 16);
          RawRecord.RawData[NextOfs+3]:=Byte(SegmentLength shr 24);
          Inc(NextOfs,4);
        end
      else
        begin
          RawRecord.RawData[NextOfs]:=Byte(SegmentLength);
          RawRecord.RawData[NextOfs+1]:=Byte(SegmentLength shr 8);
          Inc(NextOfs,2);
        end;
      NextOfs:=RawRecord.WriteIndexedRef(NextOfs,SegmentNameIndex);
      NextOfs:=RawRecord.WriteIndexedRef(NextOfs,ClassNameIndex);
      NextOfs:=RawRecord.WriteIndexedRef(NextOfs,OverlayNameIndex);
      RawRecord.RecordLength:=NextOfs+1;
      RawRecord.CalculateChecksumByte;
    end;

  { TOmfRecord_GRPDEF }

  procedure TOmfRecord_GRPDEF.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
      Segment: Integer;
    begin
      if RawRecord.RecordType<>RT_GRPDEF then
        internalerror(2015040301);
      NextOfs:=RawRecord.ReadIndexedRef(0,FGroupNameIndex);
      SetLength(FSegmentList,0);
      while NextOfs<RawRecord.RecordLength-1 do
        begin
          if RawRecord.RawData[NextOfs]<>$ff then
            internalerror(2015040901);
          NextOfs:=RawRecord.ReadIndexedRef(NextOfs+1,Segment);
          SetLength(FSegmentList,Length(FSegmentList)+1);
          FSegmentList[High(FSegmentList)]:=Segment;
        end;
    end;

  procedure TOmfRecord_GRPDEF.EncodeTo(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
      Segment: Integer;
    begin
      RawRecord.RecordType:=RT_GRPDEF;
      NextOfs:=RawRecord.WriteIndexedRef(0,GroupNameIndex);
      for Segment in SegmentList do
        begin
          if NextOfs>High(RawRecord.RawData) then
            internalerror(2015040401);
          RawRecord.RawData[NextOfs]:=$ff;
          NextOfs:=RawRecord.WriteIndexedRef(NextOfs+1,Segment);
        end;
      RawRecord.RecordLength:=NextOfs+1;
      RawRecord.CalculateChecksumByte;
    end;

  { TOmfPublicNameElement }

    function TOmfPublicNameElement.GetLengthInFile(Is32Bit: Boolean): Integer;
    begin
      Result:=1+Length(Name)+2+1;
      if Is32Bit then
        Inc(Result,2);
      if TypeIndex>=$80 then
        Inc(Result);
    end;

  { TOmfRecord_PUBDEF }

  procedure TOmfRecord_PUBDEF.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
      Name: string;
      TypeIndex: Integer;
      PublicOffset: DWord;
      PubName: TOmfPublicNameElement;
    begin
      if not (RawRecord.RecordType in [RT_PUBDEF,RT_PUBDEF32]) then
        internalerror(2015040301);
      Is32Bit:=RawRecord.RecordType=RT_PUBDEF32;

      NextOfs:=RawRecord.ReadIndexedRef(0,FBaseGroupIndex);
      NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FBaseSegmentIndex);
      if BaseSegmentIndex=0 then
        begin
          if (NextOfs+1)>=RawRecord.RecordLength then
            internalerror(2015041401);
          BaseFrame:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8);
          Inc(NextOfs,2);
        end
      else
        BaseFrame:=0;

      while NextOfs<(RawRecord.RecordLength-1) do
        begin
          NextOfs:=RawRecord.ReadStringAt(NextOfs,Name);
          if Is32Bit then
            begin
              if (NextOfs+3)>=RawRecord.RecordLength then
                internalerror(2015041401);
              PublicOffset:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8)+
                (RawRecord.RawData[NextOfs+2] shl 16)+(RawRecord.RawData[NextOfs+3] shl 24);
              Inc(NextOfs,4);
            end
          else
            begin
              if (NextOfs+1)>=RawRecord.RecordLength then
                internalerror(2015041401);
              PublicOffset:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8);
              Inc(NextOfs,2);
            end;
          NextOfs:=RawRecord.ReadIndexedRef(NextOfs,TypeIndex);
          PubName:=TOmfPublicNameElement.Create(PublicNames,Name);
          PubName.PublicOffset:=PublicOffset;
          PubName.TypeIndex:=TypeIndex;
        end;
    end;

  procedure TOmfRecord_PUBDEF.EncodeTo(RawRecord: TOmfRawRecord);
    const
      RecordLengthLimit = 1024;
    var
      Len,LastIncludedIndex,NextOfs,I: Integer;
      PubName: TOmfPublicNameElement;
    begin
      if Is32Bit then
        RawRecord.RecordType:=RT_PUBDEF32
      else
        RawRecord.RecordType:=RT_PUBDEF;

      NextOfs:=RawRecord.WriteIndexedRef(0,BaseGroupIndex);
      NextOfs:=RawRecord.WriteIndexedRef(NextOfs,BaseSegmentIndex);
      if BaseSegmentIndex=0 then
        begin
          RawRecord.RawData[NextOfs]:=Byte(BaseFrame);
          RawRecord.RawData[NextOfs+1]:=Byte(BaseFrame shr 8);
          Inc(NextOfs,2);
        end;

      { find out how many public names can we include until we reach the length limit }
      Len:=NextOfs;
      LastIncludedIndex:=NextIndex-1;
      repeat
        Inc(LastIncludedIndex);
        Inc(Len,TOmfPublicNameElement(PublicNames[LastIncludedIndex]).GetLengthInFile(Is32Bit));
      until (LastIncludedIndex>=(PublicNames.Count-1)) or ((Len+TOmfPublicNameElement(PublicNames[LastIncludedIndex+1]).GetLengthInFile(Is32Bit))>=RecordLengthLimit);

      { write the public names... }
      for I:=NextIndex to LastIncludedIndex do
        begin
          PubName:=TOmfPublicNameElement(PublicNames[I]);
          NextOfs:=RawRecord.WriteStringAt(NextOfs,PubName.Name);
          if Is32Bit then
            begin
              RawRecord.RawData[NextOfs]:=Byte(PubName.PublicOffset);
              RawRecord.RawData[NextOfs+1]:=Byte(PubName.PublicOffset shr 8);
              RawRecord.RawData[NextOfs+2]:=Byte(PubName.PublicOffset shr 16);
              RawRecord.RawData[NextOfs+3]:=Byte(PubName.PublicOffset shr 24);
              Inc(NextOfs,4);
            end
          else
            begin
              if PubName.PublicOffset>$ffff then
                internalerror(2015041403);
              RawRecord.RawData[NextOfs]:=Byte(PubName.PublicOffset);
              RawRecord.RawData[NextOfs+1]:=Byte(PubName.PublicOffset shr 8);
              Inc(NextOfs,2);
            end;
          NextOfs:=RawRecord.WriteIndexedRef(NextOfs,PubName.TypeIndex);
        end;
      RawRecord.RecordLength:=Len+1;
      RawRecord.CalculateChecksumByte;

      { update NextIndex }
      NextIndex:=LastIncludedIndex+1;
    end;

  { TOmfExternalNameElement }

  function TOmfExternalNameElement.GetLengthInFile: Integer;
    begin
      Result:=1+Length(Name)+1;
      if TypeIndex>=$80 then
        Inc(Result);
    end;

  { TOmfRecord_EXTDEF }

  procedure TOmfRecord_EXTDEF.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      NextOfs: Integer;
      Name: string;
      TypeIndex: Integer;
      ExtName: TOmfExternalNameElement;
    begin
      if RawRecord.RecordType<>RT_EXTDEF then
        internalerror(2015040301);
      NextOfs:=0;
      while NextOfs<(RawRecord.RecordLength-1) do
        begin
          NextOfs:=RawRecord.ReadStringAt(NextOfs,Name);
          NextOfs:=RawRecord.ReadIndexedRef(NextOfs,TypeIndex);
          ExtName:=TOmfExternalNameElement.Create(ExternalNames,Name);
          ExtName.TypeIndex:=TypeIndex;
        end;
    end;

  procedure TOmfRecord_EXTDEF.EncodeTo(RawRecord: TOmfRawRecord);
    const
      RecordLengthLimit = 1024;
    var
      Len,LastIncludedIndex,NextOfs,I: Integer;
      ExtName: TOmfExternalNameElement;
    begin
      RawRecord.RecordType:=RT_EXTDEF;
      NextOfs:=0;

      { find out how many external names can we include until we reach the length limit }
      Len:=NextOfs;
      LastIncludedIndex:=NextIndex-1;
      repeat
        Inc(LastIncludedIndex);
        Inc(Len,TOmfExternalNameElement(ExternalNames[LastIncludedIndex]).GetLengthInFile);
      until (LastIncludedIndex>=(ExternalNames.Count-1)) or ((Len+TOmfExternalNameElement(ExternalNames[LastIncludedIndex+1]).GetLengthInFile)>=RecordLengthLimit);

      { write the external names... }
      for I:=NextIndex to LastIncludedIndex do
        begin
          ExtName:=TOmfExternalNameElement(ExternalNames[I]);
          NextOfs:=RawRecord.WriteStringAt(NextOfs,ExtName.Name);
          NextOfs:=RawRecord.WriteIndexedRef(NextOfs,ExtName.TypeIndex);
        end;
      RawRecord.RecordLength:=Len+1;
      RawRecord.CalculateChecksumByte;

      { update NextIndex }
      NextIndex:=LastIncludedIndex+1;
    end;

  { TOmfRecord_MODEND }

  procedure TOmfRecord_MODEND.DecodeFrom(RawRecord: TOmfRawRecord);
    var
      ModTyp: Byte;
      NextOfs: Integer;
      EndData: Byte;
    begin
      if not (RawRecord.RecordType in [RT_MODEND,RT_MODEND32]) then
        internalerror(2015040301);
      Is32Bit:=RawRecord.RecordType=RT_MODEND32;

      if RawRecord.RecordLength<2 then
        internalerror(2015040305);
      ModTyp:=RawRecord.RawData[0];
      IsMainModule:=(ModTyp and $80)<>0;
      HasStartAddress:=(ModTyp and $40)<>0;
      SegmentBit:=(ModTyp and $20)<>0;
      LogicalStartAddress:=(ModTyp and $01)<>0;
      if (ModTyp and $1E)<>0 then
        internalerror(2015041404);
      NextOfs:=1;

      { clear all the start address properties first }
      FrameMethod:=Low(FrameMethod);
      FrameDatum:=0;
      TargetMethod:=Low(TargetMethod);
      TargetDatum:=0;
      TargetDisplacement:=0;
      PhysFrameNumber:=0;
      PhysOffset:=0;

      if HasStartAddress then
        begin
          if LogicalStartAddress then
            begin
              if NextOfs>=RawRecord.RecordLength then
                internalerror(2015040305);
              EndData:=RawRecord.RawData[NextOfs];
              Inc(NextOfs);
              { frame and target method determined by thread is not allowed in MODEND records }
              if (EndData and $88)<>0 then
                internalerror(2015041406);
              FrameMethod:=TOmfFixupFrameMethod((EndData shr 4) and 7);
              TargetMethod:=TOmfFixupTargetMethod(EndData and 7);
              { frame method ffmLocation is not allowed in an MODEND record }
              if FrameMethod=ffmLocation then
                internalerror(2015041402);
              { read Frame Datum? }
              if FrameMethod in [ffmSegmentIndex,ffmGroupIndex,ffmExternalIndex,ffmFrameNumber] then
                NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FFrameDatum);
              { read Target Datum? }
              NextOfs:=RawRecord.ReadIndexedRef(NextOfs,FTargetDatum);
              { read Target Displacement? }
              if TargetMethod in [ftmSegmentIndex,ftmGroupIndex,ftmExternalIndex,ftmFrameNumber] then
                begin
                  if Is32Bit then
                    begin
                      if (NextOfs+3)>=RawRecord.RecordLength then
                        internalerror(2015040504);
                      TargetDisplacement := RawRecord.RawData[NextOfs]+
                                           (RawRecord.RawData[NextOfs+1] shl 8)+
                                           (RawRecord.RawData[NextOfs+2] shl 16)+
                                           (RawRecord.RawData[NextOfs+3] shl 24);
                      Inc(NextOfs,4);
                    end
                  else
                    begin
                      if (NextOfs+1)>=RawRecord.RecordLength then
                        internalerror(2015040504);
                      TargetDisplacement := RawRecord.RawData[NextOfs]+
                                           (RawRecord.RawData[NextOfs+1] shl 8);
                      Inc(NextOfs,2);
                    end;
                end;
            end
          else
            begin
              { physical start address }
              if (NextOfs+1)>=RawRecord.RecordLength then
                internalerror(2015040305);
              PhysFrameNumber:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8);
              Inc(NextOfs,2);
              if Is32Bit then
                begin
                  if (NextOfs+3)>=RawRecord.RecordLength then
                    internalerror(2015040305);
                  PhysOffset:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8)+
                    (RawRecord.RawData[NextOfs+2] shl 16)+(RawRecord.RawData[NextOfs+3] shl 24);
                  Inc(NextOfs,4);
                end
              else
                begin
                  if (NextOfs+1)>=RawRecord.RecordLength then
                    internalerror(2015040305);
                  PhysOffset:=RawRecord.RawData[NextOfs]+(RawRecord.RawData[NextOfs+1] shl 8);
                  Inc(NextOfs,2);
                end;
            end;
        end;
    end;

  procedure TOmfRecord_MODEND.EncodeTo(RawRecord: TOmfRawRecord);
    var
      ModTyp: Byte;
      NextOfs: Integer;
      EndData: Byte;
    begin
      if Is32Bit then
        RawRecord.RecordType:=RT_MODEND32
      else
        RawRecord.RecordType:=RT_MODEND;
      ModTyp:=(Ord(IsMainModule) shl 7)+(Ord(HasStartAddress) shl 6)+(Ord(SegmentBit) shl 5)+Ord(LogicalStartAddress);
      RawRecord.RawData[0]:=ModTyp;
      NextOfs:=1;
      if HasStartAddress then
        begin
          if LogicalStartAddress then
            begin
              { frame method ffmLocation is not allowed in an MODEND record }
              if FrameMethod=ffmLocation then
                internalerror(2015041402);
              EndData:=(Ord(FrameMethod) shl 4)+Ord(TargetMethod);
              RawRecord.RawData[NextOfs]:=EndData;
              Inc(NextOfs);
              { save Frame Datum? }
              if FrameMethod in [ffmSegmentIndex,ffmGroupIndex,ffmExternalIndex,ffmFrameNumber] then
                NextOfs:=RawRecord.WriteIndexedRef(NextOfs,FrameDatum);
              { save Target Datum? }
              NextOfs:=RawRecord.WriteIndexedRef(NextOfs,TargetDatum);
              { save Target Displacement? }
              if TargetMethod in [ftmSegmentIndex,ftmGroupIndex,ftmExternalIndex,ftmFrameNumber] then
                begin
                  if Is32Bit then
                    begin
                      RawRecord.RawData[NextOfs]:=Byte(TargetDisplacement);
                      RawRecord.RawData[NextOfs+1]:=Byte(TargetDisplacement shr 8);
                      RawRecord.RawData[NextOfs+2]:=Byte(TargetDisplacement shr 16);
                      RawRecord.RawData[NextOfs+3]:=Byte(TargetDisplacement shr 24);
                      Inc(NextOfs,4);
                    end
                  else
                    begin
                      if TargetDisplacement>$ffff then
                        internalerror(2015040502);
                      RawRecord.RawData[NextOfs]:=Byte(TargetDisplacement);
                      RawRecord.RawData[NextOfs+1]:=Byte(TargetDisplacement shr 8);
                      Inc(NextOfs,2);
                    end;
                end;
            end
          else
            begin
              { physical start address }
              RawRecord.RawData[NextOfs]:=Byte(PhysFrameNumber);
              RawRecord.RawData[NextOfs+1]:=Byte(PhysFrameNumber shr 8);
              Inc(NextOfs,2);
              if Is32Bit then
                begin
                  RawRecord.RawData[NextOfs]:=Byte(PhysOffset);
                  RawRecord.RawData[NextOfs+1]:=Byte(PhysOffset shr 8);
                  RawRecord.RawData[NextOfs+2]:=Byte(PhysOffset shr 16);
                  RawRecord.RawData[NextOfs+3]:=Byte(PhysOffset shr 24);
                  Inc(NextOfs,4);
                end
              else
                begin
                  if PhysOffset>$ffff then
                    internalerror(2015040502);
                  RawRecord.RawData[NextOfs]:=Byte(PhysOffset);
                  RawRecord.RawData[NextOfs+1]:=Byte(PhysOffset shr 8);
                  Inc(NextOfs,2);
                end;
            end;
        end;
      RawRecord.RecordLength:=NextOfs+1;
      RawRecord.CalculateChecksumByte;
    end;

  { TOmfSubRecord_FIXUP }

  function TOmfSubRecord_FIXUP.GetDataRecordOffset: Integer;
    begin
      Result:=FLocationOffset-FDataRecordStartOffset;
    end;

  function TOmfSubRecord_FIXUP.GetLocationSize: Integer;
    const
      OmfLocationType2Size: array [TOmfFixupLocationType] of Integer=
        (1,  // fltLoByte
         2,  // fltOffset
         2,  // fltBase
         4,  // fltFarPointer
         1,  // fltHiByte
         2,  // fltLoaderResolvedOffset  (PharLap: Offset32)
         0,  // fltUndefined6            (PharLap: Pointer48)
         0,  // fltUndefined7
         0,  // fltUndefined8
         4,  // fltOffset32
         0,  // fltUndefined10
         6,  // fltFarPointer48
         0,  // fltUndefined12
         4,  // fltLoaderResolvedOffset32
         0,  // fltUndefined14
         0); // fltUndefined15
    begin
      Result:=OmfLocationType2Size[LocationType];
    end;

  procedure TOmfSubRecord_FIXUP.SetDataRecordOffset(AValue: Integer);
    begin
      FLocationOffset:=AValue+FDataRecordStartOffset;
    end;

  function TOmfSubRecord_FIXUP.ReadAt(RawRecord: TOmfRawRecord; Offset: Integer): Integer;
    var
      Locat: Word;
      FixData: Byte;
    begin
      if (Offset+2)>=RawRecord.RecordLength then
        internalerror(2015040504);
      { unlike other fields in the OMF format, this one is big endian }
      Locat:=(RawRecord.RawData[Offset] shl 8) or RawRecord.RawData[Offset+1];
      FixData:=RawRecord.RawData[Offset+2];
      Inc(Offset,3);
      if (Locat and $8000)=0 then
        internalerror(2015040503);
      DataRecordOffset:=Locat and $3FF;
      LocationType:=TOmfFixupLocationType((Locat shr 10) and 15);
      Mode:=TOmfFixupMode((Locat shr 14) and 1);
      FrameDeterminedByThread:=(FixData and $80)<>0;
      TargetDeterminedByThread:=(FixData and $08)<>0;
      if FrameDeterminedByThread then
        FrameThread:=TOmfFixupThread((FixData shr 4) and 3)
      else
        FrameMethod:=TOmfFixupFrameMethod((FixData shr 4) and 7);
      if TargetDeterminedByThread then
        begin
          TargetThread:=TOmfFixupThread(FixData and 3);
          TargetThreadDisplacementPresent:=(FixData and $40)=0;
        end
      else
        TargetMethod:=TOmfFixupTargetMethod(FixData and 7);
      { read Frame Datum? }
      if not FrameDeterminedByThread and (FrameMethod in [ffmSegmentIndex,ffmGroupIndex,ffmExternalIndex,ffmFrameNumber]) then
        Offset:=RawRecord.ReadIndexedRef(Offset,FFrameDatum)
      else
        FrameDatum:=0;
      { read Target Datum? }
      if not TargetDeterminedByThread then
        Offset:=RawRecord.ReadIndexedRef(Offset,FTargetDatum)
      else
        TargetDatum:=0;
      { read Target Displacement? }
      if (TargetDeterminedByThread and TargetThreadDisplacementPresent) or
         (TargetMethod in [ftmSegmentIndex,ftmGroupIndex,ftmExternalIndex,ftmFrameNumber]) then
        begin
          if Is32Bit then
            begin
              if (Offset+3)>=RawRecord.RecordLength then
                internalerror(2015040504);
              TargetDisplacement := RawRecord.RawData[Offset]+
                                   (RawRecord.RawData[Offset+1] shl 8)+
                                   (RawRecord.RawData[Offset+2] shl 16)+
                                   (RawRecord.RawData[Offset+3] shl 24);
              Inc(Offset,4);
            end
          else
            begin
              if (Offset+1)>=RawRecord.RecordLength then
                internalerror(2015040504);
              TargetDisplacement := RawRecord.RawData[Offset]+
                                   (RawRecord.RawData[Offset+1] shl 8);
              Inc(Offset,2);
            end;
        end;
      Result:=Offset;
    end;

  function TOmfSubRecord_FIXUP.WriteAt(RawRecord: TOmfRawRecord; Offset: Integer): Integer;
    var
      Locat: Word;
      FixData: Byte;
    begin
      if (DataRecordOffset<0) or (DataRecordOffset>1023) then
        internalerror(2015040501);
      Locat:=$8000+(Ord(Mode) shl 14)+(Ord(LocationType) shl 10)+DataRecordOffset;
      { unlike other fields in the OMF format, this one is big endian }
      RawRecord.RawData[Offset]:=Byte(Locat shr 8);
      RawRecord.RawData[Offset+1]:=Byte(Locat);
      Inc(Offset, 2);
      FixData:=(Ord(FrameDeterminedByThread) shl 7)+(Ord(TargetDeterminedByThread) shl 3);
      if FrameDeterminedByThread then
        FixData:=FixData+(Ord(FrameThread) shl 4)
      else
        FixData:=FixData+(Ord(FrameMethod) shl 4);
      if TargetDeterminedByThread then
        FixData:=FixData+Ord(TargetThread)+(Ord(not TargetThreadDisplacementPresent) shl 2)
      else
        FixData:=FixData+Ord(TargetMethod);
      RawRecord.RawData[Offset]:=FixData;
      Inc(Offset);
      { save Frame Datum? }
      if not FrameDeterminedByThread and (FrameMethod in [ffmSegmentIndex,ffmGroupIndex,ffmExternalIndex,ffmFrameNumber]) then
        Offset:=RawRecord.WriteIndexedRef(Offset,FrameDatum);
      { save Target Datum? }
      if not TargetDeterminedByThread then
        Offset:=RawRecord.WriteIndexedRef(Offset,TargetDatum);
      { save Target Displacement? }
      if (TargetDeterminedByThread and TargetThreadDisplacementPresent) or
         (TargetMethod in [ftmSegmentIndex,ftmGroupIndex,ftmExternalIndex,ftmFrameNumber]) then
        begin
          if Is32Bit then
            begin
              RawRecord.RawData[Offset]:=Byte(TargetDisplacement);
              RawRecord.RawData[Offset+1]:=Byte(TargetDisplacement shr 8);
              RawRecord.RawData[Offset+2]:=Byte(TargetDisplacement shr 16);
              RawRecord.RawData[Offset+3]:=Byte(TargetDisplacement shr 24);
              Inc(Offset,4);
            end
          else
            begin
              if TargetDisplacement>$ffff then
                internalerror(2015040502);
              RawRecord.RawData[Offset]:=Byte(TargetDisplacement);
              RawRecord.RawData[Offset+1]:=Byte(TargetDisplacement shr 8);
              Inc(Offset,2);
            end;
        end;
      Result:=Offset;
    end;


  { TOmfRecord_LIBHEAD }

  constructor TOmfRecord_LIBHEAD.Create;
    begin
      PageSize:=512;
      DictionarySizeInBlocks:=2;
      CaseSensitive:=true;
    end;

  procedure TOmfRecord_LIBHEAD.SetPageSize(AValue: Integer);
    var
      p: longint;
    begin
      { valid library page sizes are powers of two, between 2**4 and 2**15 }
      if not ispowerof2(AValue,p) then
        internalerror(2015041802);
      if (p<4) or (p>15) then
        internalerror(2015041802);
      FPageSize:=AValue;
    end;

  procedure TOmfRecord_LIBHEAD.DecodeFrom(RawRecord: TOmfRawRecord);
    begin
      if RawRecord.RecordType<>RT_LIBHEAD then
        internalerror(2015040301);
      { this will also range check PageSize and will ensure that RecordLength>=13 }
      PageSize:=RawRecord.RecordLength+3;
      DictionaryOffset:=RawRecord.RawData[0]+
                       (RawRecord.RawData[1] shl 8)+
                       (RawRecord.RawData[2] shl 16)+
                       (RawRecord.RawData[3] shl 24);
      DictionarySizeInBlocks:=RawRecord.RawData[4]+
                             (RawRecord.RawData[5] shl 8);
      Flags:=RawRecord.RawData[6];
    end;

  procedure TOmfRecord_LIBHEAD.EncodeTo(RawRecord: TOmfRawRecord);
    begin
      { make sure the LIBHEAD record is padded with zeros at the end }
      FillChar(RawRecord.RawData,SizeOf(RawRecord.RawData),0);
      RawRecord.RecordType:=RT_LIBHEAD;
      RawRecord.RecordLength:=PageSize-3;
      RawRecord.RawData[0]:=Byte(DictionaryOffset);
      RawRecord.RawData[1]:=Byte(DictionaryOffset shr 8);
      RawRecord.RawData[2]:=Byte(DictionaryOffset shr 16);
      RawRecord.RawData[3]:=Byte(DictionaryOffset shr 24);
      RawRecord.RawData[4]:=Byte(DictionarySizeInBlocks);
      RawRecord.RawData[5]:=Byte(DictionarySizeInBlocks shr 8);
      RawRecord.RawData[6]:=Flags;
      { the LIBHEAD record contains no checksum byte, so no need to call
        RawRecord.CalculateChecksumByte }
    end;

  function TOmfRecord_LIBHEAD.IsCaseSensitive: Boolean;
    begin
      Result:=(FFlags and 1)<>0;
    end;

  procedure TOmfRecord_LIBHEAD.SetCaseSensitive(AValue: Boolean);
    begin
      FFlags:=(FFlags and $FE) or Ord(AValue);
    end;

  { TOmfRecord_LIBEND }

  procedure TOmfRecord_LIBEND.DecodeFrom(RawRecord: TOmfRawRecord);
    begin
      if RawRecord.RecordType<>RT_LIBEND then
        internalerror(2015040301);
      FPaddingBytes:=RawRecord.RecordLength;
    end;

  procedure TOmfRecord_LIBEND.EncodeTo(RawRecord: TOmfRawRecord);
    begin
      { make sure the LIBEND record is padded with zeros at the end }
      FillChar(RawRecord.RawData,SizeOf(RawRecord.RawData),0);
      RawRecord.RecordType:=RT_LIBEND;
      RawRecord.RecordLength:=FPaddingBytes;
      { the LIBEND record contains no checksum byte, so no need to call
        RawRecord.CalculateChecksumByte }
    end;

  procedure TOmfRecord_LIBEND.CalculatePaddingBytes(RecordStartOffset: DWord);
    var
      DictionaryStartOffset: Integer;
    begin
      { padding must be calculated, so that the dictionary begins on a 512-byte boundary }
      Inc(RecordStartOffset,3);  // padding begins _after_ the record header (3 bytes)
      DictionaryStartOffset:=(RecordStartOffset+511) and $fffffe00;
      PaddingBytes:=DictionaryStartOffset-RecordStartOffset;
    end;

  function compute_omf_lib_hash(const name: string; blocks: Integer): TOmfLibHash;
    const
      blank=$20;  // ASCII blank
      nbuckets=37;
    var
      block_x: Integer;
      block_d: Integer;
      bucket_x: Integer;
      bucket_d: Integer;
      len: Integer;
      pbidx,peidx: Integer;
      cback,cfront: Byte;
    begin
      len:=Length(name);
      if len=0 then
        internalerror(2015041801);
      pbidx:=1;
      peidx:=len+1;
      { left to right scan }
      block_x:=len or blank;
      bucket_d:=block_x;
      { right to left scan }
      block_d:=0;
      bucket_x:=0;
      while true do
        begin
          { blank -> convert to LC }
          Dec(peidx);
          cback:=Byte(name[peidx]) or blank;
          bucket_x:=RorWord(bucket_x,2) xor cback;
          block_d:=RolWord(block_d,2) xor cback;
          Dec(len);
          if len=0 then
            break;
          cfront:=Byte(name[pbidx]) or blank;
          Inc(pbidx);
          block_x:=RolWord(block_x,2) xor cfront;
          bucket_d:=RorWord(bucket_d,2) xor cfront;
        end;
      Result.block_x:=block_x mod blocks;
      Result.block_d:=max(block_d mod blocks,1);
      Result.bucket_x:=bucket_x mod nbuckets;
      Result.bucket_d:=max(bucket_d mod nbuckets,1);
    end;

end.
