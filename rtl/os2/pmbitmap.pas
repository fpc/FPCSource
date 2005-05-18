{****************************************************************************


    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by the Free Pascal development team.
    Copyright (c) 1999-2000 by Ramon Bosque

    Types and constants for bitmap images manipulation
    plus functions implemented in PMPIC.DLL.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}
unit PMBitmap;

interface

{$PACKRECORDS 1}

type    TBitmapInfoHeader=record
            cbFix:cardinal;
            cx:word;
            cy:word;
            cPlanes:word;
            cBitCount:word;
        end;
        PBitmapInfoHeader=^TBitmapInfoHeader;
        BitmapInfoHeader=TBitmapInfoHeader;

        TRgb=record
            bBlue,
            bGreen,
            bRed:byte;
        end;
        PRgb=^TRgb;
        Rgb=TRgb;

        TBitmapInfo=record
            cbFix:cardinal;
            cx:word;
            cy:word;
            cPlanes:word;
            cBitCount:word;
            aRgbColor:array[0..0] of TRgb;
        end;
        PBitmapInfo=^TBitmapInfo;
        BitmapInfo=TBitmapInfo;

        TBitmapInfoHeader2=record
            cbFix:cardinal;         { Length of structure }
            cx:cardinal;            { Bitmap width in pels }
            cy:cardinal;            { Bitmap height in pels }
            cPlanes:word;           { Number of bit planes }
            cBitCount:word;         { Number of bits per pel within a plane }
            ulCompression:cardinal; { Compression scheme used
                                      to store the bitmap }
            cbImage:cardinal;       { Length of bitmap storage data in bytes }
            cxResolution:cardinal;  { X resolution of target device }
            cyResolution:cardinal;  { Y resolution of target device }
            cClrUsed:cardinal;      { Number of color indices used }
            cClrImportant:cardinal; { Number of important color indices }
            usUnits:word;           { Units of measure }
            usReserved:word;
            usRecording:word;       { Recording algorithm }
            usRendering:word;       { Halftoning algorithm }
            cSize1:cardinal;        { Size value 1 }
            cSize2:cardinal;        { Size value 2 }
            ulColorEncoding:cardinal;   { Color encoding }
            ulIdentifier:cardinal;  { Reserved for application use }
        end;
        PBitmapInfoHeader2=^TBitmapInfoHeader2;
        BitmapInfoHeader2=TBitmapInfoHeader2;

        TRgb2=record
            bBlue,
            bGreen,
            bRed,
            fcOptions:byte; { Reserved, must be zero }
        end;
        PRgb2=^TRgb2;
        Rgb2=TRgb2;

        TBitmapInfo2=record
            cbFix:cardinal;
            cx:cardinal;
            cy:cardinal;
            cPlanes:word;
            cBitCount:word;
            ulCompression:cardinal;
            cbImage:cardinal;
            cxResolution:cardinal;
            cyResolution:cardinal;
            cClrUsed:cardinal;
            cClrImportant:cardinal;
            usUnits:word;
            usReserved:word;
            usRecording:word;
            usRendering:word;
            cSize1:cardinal;
            cSize2:cardinal;
            ulColorEncoding:cardinal;
            ulIdentifier:cardinal;
            aRgbColor:array[0..0] of TRgb2;
        end;
        PBitmapInfo2=^TBitmapInfo2;
        BitmapInfo2=TBitmapInfo2;

        TBitmapFileHeader=record
            usType:word;
            cbSize:cardinal;
            xHotspot:integer;
            yHotspot:integer;
            offBits:cardinal;
            bmp:TBitmapInfoHeader;
        end;
        PBitmapFileHeader=^TBitmapFileHeader;
        BitmapFileHeader=TBitmapFileHeader;

        TBitmapArrayFileHeader=record
            usType:word;
            cbSize:cardinal;
            offNext:cardinal;
            cxDisplay:word;
            cyDisplay:word;
            bfh:TBitmapFileHeader;
        end;
        PBitmapArrayFileHeader=^TBitmapArrayFileHeader;
        BitmapArrayFileHeader=TBitmapArrayFileHeader;

        TBitmapFileHeader2=record
            usType:word;
            cbSize:cardinal;
            xHotspot:integer;
            yHotspot:integer;
            offBits:cardinal;
            bmp2:TBitmapInfoHeader2;
        end;
        PBitmapFileHeader2=^TBitmapFileHeader2;
        BitmapFileHeader2=TBitmapFileHeader2;

        TBitmapArrayFileHeader2=record
            usType:word;
            cbSize:cardinal;
            offNext:cardinal;
            cxDisplay:word;
            cyDisplay:word;
            bfh2:TBitmapFileHeader2;
        end;
        PBitmapArrayFileHeader2=^TBitmapArrayFileHeader2;
        BitmapArrayFileHeader2=TBitmapArrayFileHeader2;

{ Constants for compression/decompression command }
const   CBD_COMPRESSION         =       1;
        CBD_DECOMPRESSION       =       2;
        CBD_BITS                =       0;

{ Flags for compression/decompression option }
        CBD_COLOR_CONVERSION    =$0000001;

{ Compression scheme in the ulCompression field of the bitmapinfo structure }
        BCA_UNCOMP              =       0;
        BCA_HUFFMAN1D           =       3;
        BCA_RLE4                =       2;
        BCA_RLE8                =       1;
        BCA_RLE24               =       4;

        BRU_METRIC              =       0;

        BRA_BOTTOMUP            =       0;

        BRH_NOTHALFTONED        =       0;
        BRH_ERRORDIFFUSION      =       1;
        BRH_PANDA               =       2;
        BRH_SUPERCIRCLE         =       3;

        BCE_PALETTE             =      -1;
        BCE_RGB                 =       0;

{ Values identifying bitmap types used in usType field
  of BITMAPFILEHEADER(2) and BITMAPARRAYFILEHEADER(2).
  (BFT_ => Bitmap File Type) }
        BFT_ICON                =   $4349;
        BFT_BMAP                =   $4d42;
        BFT_POINTER             =   $5450;
        BFT_COLORICON           =   $4943;
        BFT_COLORPOINTER        =   $5043;
        BFT_BITMAPARRAY         =   $4142;

{ type of picture to print }
const   PIP_MF       = 1;
        PIP_PIF      = 2;

{ type of conversion required }
        PIC_PIFTOMET = 0;
        PIC_SSTOFONT = 2;

function PicPrint (ahab: longint; var pszFilename: PChar; lType: longint;
                                        var pszParams: PChar): Longbool; cdecl;

function PicIchg (ahab: longint; var pszFilename1, pszFilename2: PChar;
                                              lType: longint): Longbool; cdecl;


implementation

function PicPrint (ahab: longint; var pszFilename: PChar; lType: longint;
             var pszParams: PChar): Longbool; cdecl; external 'PMPIC' index 11;

function PicIchg (ahab: longint; var pszFilename1, pszFilename2: PChar;
                   lType: longint): Longbool; cdecl; external 'PMPIC' index 12;

end.
