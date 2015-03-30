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

      procedure CalculateChecksumByte;
      function VerifyChecksumByte: boolean;
      property ChecksumByte: Byte read GetChecksumByte write SetChecksumByte;

      procedure ReadFrom(aReader: TObjectReader);
      procedure WriteTo(aWriter: TObjectWriter);
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

implementation

  uses
    verbose;

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

  procedure TOmfRawRecord.WriteTo(aWriter: TObjectWriter);
    begin
      aWriter.write(RawData, RecordLength+3);
    end;

  { TOmfRecord_THEADR }

  procedure TOmfRecord_THEADR.DecodeFrom(RawRecord: TOmfRawRecord);
    begin
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

end.
