{****************************************************************************

    $Id$

                   Copyright (c) 1993,94,99 by FK, RB
                  
 ****************************************************************************}
unit pmbitmap;

  interface

{$PACKRECORDS 1}

    type
       BITMAPINFOHEADER = record
          cbFix : cardinal;
          cx : word;
          cy : word;
          cPlanes : word;
          cBitCount : word;
       end;

       PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

       RGB = record
          bBlue : BYTE;
          bGreen : BYTE;
          bRed : BYTE;
       end;

       BITMAPINFO = record
          cbFix : cardinal;
          cx : word;
          cy : word;
          cPlanes : word;
          cBitCount : word;
          argbColor : array[0..1-1] of RGB;
       end;

       PBITMAPINFO = ^BITMAPINFO;

       BITMAPINFOHEADER2 = record
          cbFix : cardinal;
          cx : cardinal;
          cy : cardinal;
          cPlanes : word;
          cBitCount : word;
          ulCompression : cardinal;
          cbImage : cardinal;
          cxResolution : cardinal;
          cyResolution : cardinal;
          cclrUsed : cardinal;
          cclrImportant : cardinal;
          usUnits : word;
          usReserved : word;
          usRecording : word;
          usRendering : word;
          cSize1 : cardinal;
          cSize2 : cardinal;
          ulColorEncoding : cardinal;
          ulIdentifier : cardinal;
       end;

       PBITMAPINFOHEADER2 = ^BITMAPINFOHEADER2;

       RGB2 = record
          bBlue : BYTE;
          bGreen : BYTE;
          bRed : BYTE;
          fcOptions : BYTE;
       end;

       PRGB2 = ^RGB2;

       BITMAPINFO2 = record
          cbFix : cardinal;
          cx : cardinal;
          cy : cardinal;
          cPlanes : word;
          cBitCount : word;
          ulCompression : cardinal;
          cbImage : cardinal;
          cxResolution : cardinal;
          cyResolution : cardinal;
          cclrUsed : cardinal;
          cclrImportant : cardinal;
          usUnits : word;
          usReserved : word;
          usRecording : word;
          usRendering : word;
          cSize1 : cardinal;
          cSize2 : cardinal;
          ulColorEncoding : cardinal;
          ulIdentifier : cardinal;
          argbColor : array[0..1-1] of RGB2;
       end;

       PBITMAPINFO2 = ^BITMAPINFO2;

       BITMAPFILEHEADER = record
          usType : word;
          cbSize : cardinal;
          xHotspot : integer;
          yHotspot : integer;
          offBits : cardinal;
          bmp : BITMAPINFOHEADER;
       end;

       PBITMAPFILEHEADER = ^BITMAPFILEHEADER;

       BITMAPARRAYFILEHEADER = record
          usType : word;
          cbSize : cardinal;
          offNext : cardinal;
          cxDisplay : word;
          cyDisplay : word;
          bfh : BITMAPFILEHEADER;
       end;

       PBITMAPARRAYFILEHEADER = ^BITMAPARRAYFILEHEADER;

       BITMAPFILEHEADER2 = record
          usType : word;
          cbSize : cardinal;
          xHotspot : integer;
          yHotspot : integer;
          offBits : cardinal;
          bmp2 : BITMAPINFOHEADER2;
       end;

       PBITMAPFILEHEADER2 = ^BITMAPFILEHEADER2;

       BITMAPARRAYFILEHEADER2 = record
          usType : word;
          cbSize : cardinal;
          offNext : cardinal;
          cxDisplay : word;
          cyDisplay : word;
          bfh2 : BITMAPFILEHEADER2;
       end;

       PBITMAPARRAYFILEHEADER2 = ^BITMAPARRAYFILEHEADER2;

{$PACKRECORDS NORMAL}


    const
       CBD_COMPRESSION = 1;
       CBD_DECOMPRESSION = 2;
       CBD_BITS = 0;
       CBD_COLOR_CONVERSION = $00000001;
       BCA_UNCOMP = 0;
       BCA_HUFFMAN1D = 3;
       BCA_RLE4 = 2;
       BCA_RLE8 = 1;
       BCA_RLE24 = 4;
       BRU_METRIC = 0;
       BRA_BOTTOMUP = 0;
       BRH_NOTHALFTONED = 0;
       BRH_ERRORDIFFUSION = 1;
       BRH_PANDA = 2;
       BRH_SUPERCIRCLE = 3;
       BCE_PALETTE = (-1);
       BCE_RGB = 0;

       BFT_ICON = $4349;
       BFT_BMAP = $4d42;
       BFT_POINTER = $5450;
       BFT_COLORICON = $4943;
       BFT_COLORPOINTER = $5043;
       BFT_BITMAPARRAY = $4142;


  implementation

end.
{
  $Log$
  Revision 1.3  1999-06-02 16:01:31  hajny
    * changes by Ramon Bosque

}
