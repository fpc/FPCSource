{****************************************************************************

    $Id$

              Copyright (c) 1999-2000 by Florian Klaempfl
                  Copyright (c) 1999-2000 by Ramon Bosque

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

 ****************************************************************************}
unit pmbitmap;

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

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
            aRgbColor:array[0..1-1] of TRgb;
        end;
        PBitmapInfo=^TBitmapInfo;
        BitmapInfo=TBitmapInfo;

        TBitmapInfoHeader2=record
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
        end;
        PBitmapInfoHeader2=^TBitmapInfoHeader2;
        BitmapInfoHeader2=TBitmapInfoHeader2;

        TRgb2=record
            bBlue,
            bGreen,
            bRed,
            fcOptions:byte;
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
            aRgbColor:array[0..1-1] of TRgb2;
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

const   CBD_COMPRESSION         =       1;
        CBD_DECOMPRESSION       =       2;
        CBD_BITS                =       0;
        CBD_COLOR_CONVERSION    =$0000001;
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
        BCE_PALETTE             =    (-1);
        BCE_RGB                 =       0;

        BFT_ICON                =   $4349;
        BFT_BMAP                =   $4d42;
        BFT_POINTER             =   $5450;
        BFT_COLORICON           =   $4943;
        BFT_COLORPOINTER        =   $5043;
        BFT_BITMAPARRAY         =   $4142;


implementation

end.
{
  $Log$
  Revision 1.3  2002-09-07 16:01:25  peter
    * old logs removed and tabs fixed

}
