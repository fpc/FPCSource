{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit pmbitmap;

  interface
  
    uses
       os2def;

{$PACKRECORDS 1}

    type
       BITMAPINFOHEADER = record
          cbFix : ULONG;
          cx : USHORT;
          cy : USHORT;
          cPlanes : USHORT;
          cBitCount : USHORT;
       end;

       PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

       RGB = record
          bBlue : BYTE;
          bGreen : BYTE;
          bRed : BYTE;
       end;

       BITMAPINFO = record
          cbFix : ULONG;
          cx : USHORT;
          cy : USHORT;
          cPlanes : USHORT;
          cBitCount : USHORT;
          argbColor : array[0..1-1] of RGB;
       end;

       PBITMAPINFO = ^BITMAPINFO;

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

    type
       BITMAPINFOHEADER2 = record
          cbFix : ULONG;
          cx : ULONG;
          cy : ULONG;
          cPlanes : USHORT;
          cBitCount : USHORT;
          ulCompression : ULONG;
          cbImage : ULONG;
          cxResolution : ULONG;
          cyResolution : ULONG;
          cclrUsed : ULONG;
          cclrImportant : ULONG;
          usUnits : USHORT;
          usReserved : USHORT;
          usRecording : USHORT;
          usRendering : USHORT;
          cSize1 : ULONG;
          cSize2 : ULONG;
          ulColorEncoding : ULONG;
          ulIdentifier : ULONG;
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
          cbFix : ULONG;
          cx : ULONG;
          cy : ULONG;
          cPlanes : USHORT;
          cBitCount : USHORT;
          ulCompression : ULONG;
          cbImage : ULONG;
          cxResolution : ULONG;
          cyResolution : ULONG;
          cclrUsed : ULONG;
          cclrImportant : ULONG;
          usUnits : USHORT;
          usReserved : USHORT;
          usRecording : USHORT;
          usRendering : USHORT;
          cSize1 : ULONG;
          cSize2 : ULONG;
          ulColorEncoding : ULONG;
          ulIdentifier : ULONG;
          argbColor : array[0..1-1] of RGB2;
       end;

       PBITMAPINFO2 = ^BITMAPINFO2;

       BITMAPFILEHEADER = record
          usType : USHORT;
          cbSize : ULONG;
          xHotspot : SHORT;
          yHotspot : SHORT;
          offBits : ULONG;
          bmp : BITMAPINFOHEADER;
       end;

       PBITMAPFILEHEADER = ^BITMAPFILEHEADER;

       BITMAPARRAYFILEHEADER = record
          usType : USHORT;
          cbSize : ULONG;
          offNext : ULONG;
          cxDisplay : USHORT;
          cyDisplay : USHORT;
          bfh : BITMAPFILEHEADER;
       end;

       PBITMAPARRAYFILEHEADER = ^BITMAPARRAYFILEHEADER;

       BITMAPFILEHEADER2 = record
          usType : USHORT;
          cbSize : ULONG;
          xHotspot : SHORT;
          yHotspot : SHORT;
          offBits : ULONG;
          bmp2 : BITMAPINFOHEADER2;
       end;

       PBITMAPFILEHEADER2 = ^BITMAPFILEHEADER2;

       BITMAPARRAYFILEHEADER2 = record
          usType : USHORT;
          cbSize : ULONG;
          offNext : ULONG;
          cxDisplay : USHORT;
          cyDisplay : USHORT;
          bfh2 : BITMAPFILEHEADER2;
       end;

       PBITMAPARRAYFILEHEADER2 = ^BITMAPARRAYFILEHEADER2;

    const
       BFT_ICON = $4349;
       BFT_BMAP = $4d42;
       BFT_POINTER = $5450;
       BFT_COLORICON = $4943;
       BFT_COLORPOINTER = $5043;
       BFT_BITMAPARRAY = $4142;
{$PACKRECORDS NORMAL}

  implementation

end.
