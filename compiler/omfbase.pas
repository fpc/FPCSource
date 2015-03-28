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

  type

    { TOmfRawRecord }

    TOmfRawRecord = class
    private
      function GetRecordLength: Word;
      function GetRecordType: Byte;
      procedure SetRecordLength(AValue: Word);
      procedure SetRecordType(AValue: Byte);
    public
      RawData: array [-3..65535] of Byte;
      property RecordType: Byte read GetRecordType write SetRecordType;
      property RecordLength: Word read GetRecordLength write SetRecordLength;

      procedure ReadFrom(aReader: TObjectReader);
      procedure WriteTo(aWriter: TObjectWriter);
    end;

implementation

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

  procedure TOmfRawRecord.ReadFrom(aReader: TObjectReader);
    begin
      aReader.read(RawData, 3);
      aReader.read(RawData[0], RecordLength);
    end;

  procedure TOmfRawRecord.WriteTo(aWriter: TObjectWriter);
    begin
      aWriter.write(RawData, RecordLength+3);
    end;

end.
