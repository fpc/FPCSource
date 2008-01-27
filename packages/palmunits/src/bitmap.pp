(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Bitmap.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines bitmap structures and routines.
 *
 * History:
 *    September, 1999   Created by Bertrand Simon
 *       Name  Date     Description
 *       ----  ----     -----------
 *       BS    9/99     Create
 *       jmp   12/23/99 Fix <> vs. "" problem.
 *
 *****************************************************************************)

unit bitmap;

interface

uses palmos, coretraps;

//-----------------------------------------------
// The Bitmap Structure.
//-----------------------------------------------

// bitmap version numbers
const
  BitmapVersionZero = 0;
  BitmapVersionOne = 1;
  BitmapVersionTwo = 2;

// Compression Types for BitmapVersionTwo.
type
  BitmapCompressionType = Enum;

const
 BitmapCompressionTypeScanLine = 0;
 BitmapCompressionTypeRLE = Succ(BitmapCompressionTypeScanLine);

 BitmapCompressionTypePackBits = Succ(BitmapCompressionTypeRLE);
 BitmapCompressionTypeEnd = Succ(BitmapCompressionTypePackBits);
 // must follow last compression algorithm

 BitmapCompressionTypeBest = $64;
 BitmapCompressionTypeNone = $FF;

type
  BitmapFlagsType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_BITMAPS}  // These fields will not be available in the next OS release!
    Bits: UInt16;
 {
 UInt16 compressed:1;      // Data format:  0=raw; 1=compressed
 UInt16 hasColorTable:1;   // if true, color table stored before bits[]
 UInt16 hasTransparency:1; // true if transparency is used
 UInt16 indirect:1;        // true if bits are stored indirectly
 UInt16 forScreen:1;       // system use only
 UInt16 directColor:1;     // direct color bitmap
 UInt16 reserved:10
 }
{$endif}
  end;

// this definition correspond to the 'Tbmp' and 'tAIB' resource types
  BitmapType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_BITMAPS}  // These fields will not be available in the next OS release!
    width: Int16;
    height: Int16;
    rowBytes: UInt16;
    flags: BitmapFlagsType;
    pixelSize: UInt8;        // bits/pixel
    version: UInt8;          // version of bitmap. This is vers 2
    nextDepthOffset: UInt16; // # of DWords to next BitmapType
                             //  from beginnning of this one
    transparentIndex: UInt8; // v2 only, if flags.hasTransparency is true,
                             // index number of transparent color
    compressionType: UInt8;  // v2 only, if flags.compressed is true, this is
                             // the type, see BitmapCompressionType

    reserved: UInt16;        // for future use, must be zero!

    // if (flags.hasColorTable)
    //   ColorTableType  colorTable     // NOTE: Could have 0 entries (2 bytes long)
    //
    // if (flags.directColor)
    //   BitmapDirectInfoType  directInfo;
    //
    // if (flags.indirect)
    //   void*    bitsP;                // pointer to actual bits
    // else
    //    UInt8   bits[];               // or actual bits
    //
{$endif}
  end;

  BitmapPtr = ^BitmapType;

//-----------------------------------------------
// This is the structure of a color table. It maps pixel values into
//  RGB colors. Each element in the table corresponds to the next
//  index, starting at 0.
//-----------------------------------------------

  RGBColorType = record
    index: UInt8; // index of color or best match to cur CLUT or unused.
    r: UInt8;     // amount of red, 0->255
    g: UInt8;     // amount of green, 0->255
    b: UInt8;     // amount of blue, 0->255
  end;
  RGBColorPtr = ^RGBColorType;

// -----------------------------------------------
// For direct color bitmaps (flags.directColor set), this structure follows
//  the color table if one is present, or immediately follows the BitmapType if a
//  color table is not present.
// The only type of direct color bitmap that is currently supported in version 3
//  of the Window Manager (feature: sysFtrCreator, #sysFtrNumWinVersion) are
//  16 bits/pixel with redBits=5, greenBits=6, blueBits=5.
// -----------------------------------------------

 BitmapDirectInfoType = record
   redBits: UInt8;   // # of red bits in each pixel
   greenBits: UInt8; // # of green bits in each pixel
   blueBits: UInt8;  // # of blue bits in each pixel
   reserved: UInt8;  // must be zero
   transparentColor: RGBColorType; // transparent color (index field ignored)
  end;

// -----------------------------------------------
// Color Table
// -----------------------------------------------

  ColorTableType = record
    // high bits (numEntries > 256) reserved
    numEntries: UInt16;  // number of entries in table
    // RGBColorType   entry[];   // array 0..numEntries-1 of colors
                                 // starts immediately after numEntries
  end;
  ColorTablePtr = ^ColorTableType;

// get start of color table entries aray given pointer to ColorTableType

function ColorTableEntries(ctP: ColorTablePtr): RGBColorPtr;

//-----------------------------------------------
// Routines relating to bitmap management
//-----------------------------------------------

function BmpCreate(width, height: Coord; depth: UInt8; var colortableP: ColorTableType; var error: UInt16): BitmapPtr; syscall sysTrapBmpCreate;

function BmpDelete(bitmapP: BitmapPtr): Err; syscall sysTrapBmpDelete;

function BmpCompress(bitmapP: BitmapPtr; compType: BitmapCompressionType): Err; syscall sysTrapBmpCompress;

function BmpGetBits(bitmapP: BitmapPtr): Pointer; syscall sysTrapBmpGetBits;

function BmpGetColortable(bitmapP: BitmapPtr): ColorTablePtr; syscall sysTrapBmpGetColortable;

function BmpSize(bitmapP: BitmapPtr): UInt16; syscall sysTrapBmpSize;

function BmpBitsSize(bitmapP: BitmapPtr): UInt16; syscall sysTrapBmpBitsSize;

procedure BmpGetSizes(bitmapP: BitmapPtr; var dataSizeP: UInt32; var headerSizeP: UInt32); syscall sysTrapBmpGetSizes;

function BmpColortableSize(bitmapP: BitmapPtr): UInt16; syscall sysTrapBmpColortableSize;

procedure BmpGetDimensions(bitmapP: BitmapPtr; var widthP, heightP: Coord; var rowBytesP: UInt16); syscall sysTrapBmpGetDimensions;

function BmpGetBitDepth(bitmapP: BitmapPtr): UInt8; syscall sysTrapBmpGetBitDepth;

function BmpGetNextBitmap(bitmapP: BitmapPtr): BitmapPtr; syscall sysTrapBmpGetNextBitmap;

implementation

function ColorTableEntries(ctP: ColorTablePtr): RGBColorPtr;
begin
  ColorTableEntries := RGBColorPtr(PChar(ctP) + SizeOf(ctP^));
end;

end.
