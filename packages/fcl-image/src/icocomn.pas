{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Windows icon (ICO) and cursor (CUR) file structures shared by the reader and the writer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit icocomn;
{$ENDIF FPC_DOTTEDUNITS}

interface

const
  IcoTypeIcon = 1;
  IcoTypeCursor = 2;
  // Extra keys of the hotspot of a cursor.
  IcoExtraHotSpotX = 'HotSpotX';
  IcoExtraHotSpotY = 'HotSpotY';
  // The directory can hold at most this many entries.
  IcoMaxEntries = 65535;

type
  { The file header. }
  TIconDir = packed record
    Reserved: Word;
    IconType: Word;
    Count: Word;
  end;

  { One entry of the directory. For a cursor, Planes and BitCount are the hotspot. }
  TIconDirEntry = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
    Planes: Word;
    BitCount: Word;
    BytesInRes: LongWord;
    ImageOffset: LongWord;
  end;

  { How the image of an entry is stored. }
  TIconEntryFormat = (iefBMP, iefPNG);

  { An entry as found in a file, with the size and depth of its image data. }
  TIconEntry = record
    Width, Height: Integer;
    BitCount: Word;
    HotSpotX, HotSpotY: Word;
    Offset, Size: LongWord;
    Format: TIconEntryFormat;
  end;

const
  IcoPNGSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

// Converts the fields of aDir between little-endian and the byte order of the machine.
procedure SwapIconDir(var aDir: TIconDir);
// Converts the fields of aEntry between little-endian and the byte order of the machine.
procedure SwapIconDirEntry(var aEntry: TIconDirEntry);

implementation

procedure SwapIconDir(var aDir: TIconDir);

begin
  aDir.Reserved := LEtoN(aDir.Reserved);
  aDir.IconType := LEtoN(aDir.IconType);
  aDir.Count := LEtoN(aDir.Count);
end;


procedure SwapIconDirEntry(var aEntry: TIconDirEntry);

begin
  aEntry.Planes := LEtoN(aEntry.Planes);
  aEntry.BitCount := LEtoN(aEntry.BitCount);
  aEntry.BytesInRes := LEtoN(aEntry.BytesInRes);
  aEntry.ImageOffset := LEtoN(aEntry.ImageOffset);
end;


end.
