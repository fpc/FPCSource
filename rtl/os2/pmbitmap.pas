{****************************************************************************

    $Id$

              Copyright (c) 1993,94 by Florian Klaempfl
                  Copyright (c) 1999 by Ramon Bosque

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified FPK Pascal source code file.>

 Send us your modified files, we can work together if you want!

 FPK-Pascal is distributed in the hope that it will be useful,
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

type    Tbitmapinfoheader=record
            cbFix:cardinal;
            cx:word;
            cy:word;
            cPlanes:word;
            cBitCount:word;
        end;
        Pbitmapinfoheader = ^Tbitmapinfoheader;

        Trgb=record
            bBlue,
            bGreen,
            bRed:BYTE;
        end;

        Tbitmapinfo=record
            cbFix:cardinal;
            cx:word;
            cy:word;
            cPlanes:word;
            cBitCount:word;
            argbColor:array[0..1-1] of Trgb;
        end;
        Pbitmapinfo=^Tbitmapinfo;

        Tbitmapinfoheader2=record
            cbFix:cardinal;
            cx:cardinal;
            cy:cardinal;
            cPlanes:word;
            cBitCount:word;
            ulCompression:cardinal;
            cbImage:cardinal;
            cxResolution:cardinal;
            cyResolution:cardinal;
            cclrUsed:cardinal;
            cclrImportant:cardinal;
            usUnits:word;
            usReserved:word;
            usRecording:word;
            usRendering:word;
            cSize1:cardinal;
            cSize2:cardinal;
            ulColorEncoding:cardinal;
            ulIdentifier:cardinal;
        end;
        Pbitmapinfoheader2=^Tbitmapinfoheader2;

        Trgb2=record
            bBlue,
            bGreen,
            bRed,
            fcOptions:byte;
        end;
        Prgb2=^Trgb2;

        Tbitmapinfo2=record
            cbFix:cardinal;
            cx:cardinal;
            cy:cardinal;
            cPlanes:word;
            cBitCount:word;
            ulCompression:cardinal;
            cbImage:cardinal;
            cxResolution:cardinal;
            cyResolution:cardinal;
            cclrUsed:cardinal;
            cclrImportant:cardinal;
            usUnits:word;
            usReserved:word;
            usRecording:word;
            usRendering:word;
            cSize1:cardinal;
            cSize2:cardinal;
            ulColorEncoding:cardinal;
            ulIdentifier:cardinal;
            argbColor:array[0..1-1] of Trgb2;
        end;
        Pbitmapinfo2=^Tbitmapinfo2;

        Tbitmapfileheader=record
            usType:word;
            cbSize:cardinal;
            xHotspot:integer;
            yHotspot:integer;
            offBits:cardinal;
            bmp:Tbitmapinfoheader;
        end;

        Pbitmapfileheader=^Tbitmapfileheader;

        Tbitmaparrayfileheader=record
            usType:word;
            cbSize:cardinal;
            offNext:cardinal;
            cxDisplay:word;
            cyDisplay:word;
            bfh:Tbitmapfileheader;
        end;
        Pbitmaparrayfileheader=^Tbitmaparrayfileheader;

        Tbitmapfileheader2=record
            usType:word;
            cbSize:cardinal;
            xHotspot:integer;
            yHotspot:integer;
            offBits:cardinal;
            bmp2:Tbitmapinfoheader2;
        end;
        Pbitmapfileheader2=^Tbitmapfileheader2;

        Tbitmaparrayfileheader2=record
            usType:word;
            cbSize:cardinal;
            offNext:cardinal;
            cxDisplay:word;
            cyDisplay:word;
            bfh2:Tbitmapfileheader2;
        end;

        Pbitmaparrayfileheader2=^Tbitmaparrayfileheader2;

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
  Revision 1.4  1999-06-11 13:16:21  daniel
  * Layout and copyright updates.

  Revision 1.3  1999/06/02 16:01:31  hajny
    * changes by Ramon Bosque

}
