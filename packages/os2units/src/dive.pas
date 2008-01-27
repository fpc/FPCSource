{****************************************************************************


                             DIVE interface unit
                     FPC Pascal Runtime Library for OS/2
             Copyright (c) 1999-2000 by Karoly Balogh (aka Charlie/INQ)

 The FPC Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
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

Unit DIVE;

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

Interface

Uses OS2Def, PMWin, MMBase;

Const Max_Dive_Instances = 64;

      DIVE_Success                       = $00000000;
      DIVE_Err_Invalid_Instance          = $00001000;
      DIVE_Err_Source_Format             = $00001001;
      DIVE_Err_Destination_Format        = $00001002;
      DIVE_Err_Blitter_Not_Setup         = $00001003;
      DIVE_Err_Insufficient_Length       = $00001004;
      DIVE_Err_Too_Many_Instances        = $00001005;
      DIVE_Err_No_Direct_Access          = $00001006;
      DIVE_Err_Not_Bank_Switched         = $00001007;
      DIVE_Err_Invalid_Bank_Number       = $00001008;
      DIVE_Err_FB_Not_Acquired           = $00001009;
      DIVE_Err_FB_Already_Acquired       = $0000100A;
      DIVE_Err_Acquire_Failed            = $0000100B;
      DIVE_Err_Bank_Switch_Failed        = $0000100C;
      DIVE_Err_Deacquire_Failed          = $0000100D;
      DIVE_Err_Invalid_Palette           = $0000100E;
      DIVE_Err_Invalid_Destination_RECTL = $0000100F;
      DIVE_Err_Invalid_Buffer_Number     = $00001010;
      DIVE_Err_SSMDD_Not_Installed       = $00001011;
      DIVE_Err_Buffer_Already_Accessed   = $00001012;
      DIVE_Err_Buffer_Not_Accessed       = $00001013;
      DIVE_Err_Too_Many_Bufffers         = $00001014;
      DIVE_Err_Allocation_Error          = $00001015;
      DIVE_Err_Invalid_Linesize          = $00001016;
      DIVE_Err_Fatal_Exception           = $00001017;
      DIVE_Err_Invalid_Conversion        = $00001018;
      DIVE_Err_VSD_Error                 = $00001019;
      DIVE_Err_Color_Support             = $0000101A;
      DIVE_Err_Out_Of_Range              = $0000101B;
      DIVE_Warn_No_Size                  = $00001100;

      DIVE_Buffer_Screen                 = $00000000;
      DIVE_Buffer_Graphics_Plane         = $00000001;
      DIVE_Buffer_Alternate_Plane        = $00000002;

      DIVE_Fully_Visible                 = $FFFFFFFF;

      { * Use either of the two defines as the bRGB2Entries pointer to have DIVE  * }
      { * query and set the physical or default palette as source or destination. * }

      DIVE_Palette_Physical = $00000000;
      DIVE_Palette_Default  = $FFFFFFFF;

Type  HDIVE  = cardinal;

      { * Blitter setup structures * }
      TSetup_Blitter = Record
        ulStructLen : cardinal; { * ulStructLen tells how much of the structure is used.     * }
                             { * Comments here show appropriate values, so don't count ;) * }

        fInvert     : cardinal; { * Image is inverted on blit * }
                             { * fInvert use: * }
                             { * %0001 = 01 = $01 horizontal flip * }
                             { * %0010 = 02 = $02 vertical flip   * }

        { * This is the mark for 8 bytes * }

        fccSrcColorFormat : FourCC;   { * Source data format * }
        ulSrcWidth        : cardinal;    { * Width in pels  * }
        ulSrcHeight       : cardinal;    { * Height in pels * }
                                      { * The following are for displaying a sub-portion of the image. * }
        ulSrcPosX         : cardinal;    { * X Position of source data * }
        ulSrcPosY         : cardinal;    { * Y Position of source data * }

        { * This is the mark for 28 bytes * }

        ulDitherType      : cardinal;    { * Dither type * }

        { * 32 byte mark * }

        fccDstColorFormat : FourCC;   { * Destination color format   * }
        ulDstWidth        : cardinal;    { * Destination width in pels  * }
        ulDstHeight       : cardinal;    { * Destination height in pels * }
        lDstPosX          : LongInt;
        lDstPosY          : LongInt;

        { * 52 byte mark * }

        lScreenPosX       : LongInt;
        lScreenPosY       : LongInt;

        { * 60 byte mark * }

        ulNumDstRects     : cardinal;
        pVisDstRects      : PRectl; { * This is a pointer to an array of visible rectangles. * }

        { * 68 bytes = fully used * }
       End;

      PSetup_Blitter = ^TSetup_Blitter;

      { * Stuff for DiveQueryCaps() * }
      TDIVE_CAPS = Record
        ulStructLen   : cardinal;  { * SizeOf(TDIVE_CAPS)       * }
        ulPlaneCount  : cardinal;  { * Number of defined planes * }

        { * Following info applies to ulPlaneID * }
        fScreenDirect : Boolean;  { * Direct screen access (was type BOOL in C) * }
        fBankSwitched : Boolean;  { * VRAM bank-switched? (was type BOOL in C) * }
        ulDepth       : cardinal;  { * Number of bits per pixel * }
        ulHorizontalResolution : cardinal;
        ulVerticalResolution   : cardinal;
        ulScanLineBytes        : cardinal;
        fccColorEncoding       : FourCC;
        ulApertureSize         : cardinal;

        ulInputFormats   : cardinal;   { * Number of input color formats * }
        ulOutputFormats  : cardinal;
        ulFormatLength   : cardinal;   { * Length of format buffer * }
        pFormatData      : Pointer; { * Pointer to format buffer of FOURCC's * }
       End;

      PDIVE_CAPS = ^TDIVE_CAPS;

Function DiveQueryCaps(DiveCaps: PDIVE_CAPS; ulPlaneBufNum : cardinal) : cardinal; cdecl;

Function DiveOpen(Var phDiveInst : cardinal; fNonScreenInstance : cardinal;
                  Var ppFrameBuffer : Pointer) : cardinal; cdecl;
Function DiveClose(hDiveInst : cardinal) : cardinal; cdecl;

Function DiveSetupBlitter(hDiveInst : cardinal; pSetupBlitter : PSetup_Blitter) : cardinal; cdecl;
Function DiveBlitImage(hDiveInst : cardinal; ulSrcBufNumber : cardinal; ulDstBufNumber : cardinal) : cardinal; cdecl;

Function DiveAcquireFrameBuffer(hDiveInst : cardinal; prectlDst : PRectl) : cardinal; cdecl;
Function DiveDeacquireFrameBuffer(hDiveInst : cardinal) : cardinal; cdecl;
Function DiveCalcFrameBufferAddress(hDiveInst           : cardinal;
                                    prectlDest          : PRectl;
                                Var pDestinationAddress : Pointer;
                                Var ulBankNumber        : cardinal;
                                Var ulRemLinesInBank    : cardinal) : cardinal; cdecl;
Function DiveSwitchBank(hDiveInst : cardinal; ulBankNumber : cardinal) : cardinal; cdecl;

{ Notes on DiveAllocImageBuffer:
  If pbImageBuffer is not NULL, the buffer is associated rather than
  allocated.  If pbImageBuffer is not NULL and the buffer number
  pointed to by pulBufferNumber is non-zero, a new buffer pointer is
  associated with the buffer number.  Even though no memory is
  allocated by DiveAllocImageBuffer when user-allocated buffers are
  associated, DiveFreeImageBuffer should be called to release the
  buffer association to avoid using up available buffer indexes.
  The specified line size will be used if a buffer is allocated in
  system memory, or if a user buffer is associated.  If the
  specified line size is zero, the allocated line size is rounded up
  to the nearest DWORD boundry. }

Function DiveAllocImageBuffer(hDiveInst       : cardinal;
                          Var ulBufferNumber  : cardinal;
                              fccColorSpace   : FourCC;
                              ulWidth         : cardinal;
                              ulHeight        : cardinal;
                              ulLineSizeBytes : cardinal;
                          Var bImageBuffer    : Pointer) : cardinal; cdecl;
Function DiveFreeImageBuffer(hDiveInst : cardinal; ulBufferNumber : cardinal) : cardinal; cdecl;

Function DiveBeginImageBufferAccess(hDiveInst              : cardinal;
                                    ulBufferNumber         : cardinal;
                                Var pbImageBuffer          : Pointer;
                                Var ulBufferScanLineBytes  : cardinal;
                                Var ulBufferScanLines      : cardinal) : cardinal; cdecl;
Function DiveEndImageBufferAccess(hDiveInst : cardinal; ulBufferNumber : cardinal) : cardinal; cdecl;


{/* Notes on palettes:
      Neither DiveSetSourcePalette nor DiveSetDestinationPalette API's will set
      the physical palette.  If your application MUST set the PHYSICAL palette,
      try using no more than 236 entries (the middle 236: 10-245, thus leaving
      the top and bottom 10 entries for the Workplace Shell).  If your
      application MUST use ALL 256 entries, it must do so as a full-screen
      (i.e. maximized) application.  Remember, No WM_REALIZEPALETTE message
      will be sent to other running applications, meaning they will not redraw
      and their colors will be all wrong.  It is not recommended that a
      developer use these commands:

   To set physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            GpiCreateLogColorTable ( hps, LCOL_PURECOLOR | LCOL_REALIZABLE,
                           LCOLF_CONSECRGB, 0, 256, (PLONG)plRGB2Entries );
            Gre32EntrY3 ( hdc, 0L, 0x000060C6L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );

   To reset physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            Gre32EntrY3 ( hdc, 0L, 0x000060C7L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );
*/}

Function DiveSetDestinationPalette(hDiveInst    : cardinal;
                                   ulStartIndex : cardinal;
                                   ulNumEntries : cardinal;
                               Var bRGB2Entries : Pointer) : cardinal; cdecl;

Function DiveSetSourcePalette(hDiveInst    : cardinal;
                              ulStartIndex : cardinal;
                              ulNumEntries : cardinal;
                          Var bRGB2Entries : Pointer) : cardinal; cdecl;

Function DiveSetTransparentBlitMode(hDiveInst    : cardinal;
                                    ulStartIndex : cardinal;
                                    ulValue1     : cardinal;
                                    ulValue2     : cardinal) : cardinal; cdecl;

Implementation

Function DiveQueryCaps(DiveCaps : PDIVE_CAPS; ulPlaneBufNum : cardinal) : cardinal; cdecl;
External 'DIVE' Index 1;

Function DiveOpen(Var phDiveInst : cardinal; fNonScreenInstance : cardinal; Var ppFrameBuffer : Pointer) : cardinal; cdecl;
External 'DIVE' Index 2;

Function DiveClose(hDiveInst : cardinal) : cardinal; cdecl;
External 'DIVE' Index 3;

Function DiveSetupBlitter(hDiveInst : cardinal; pSetupBlitter : PSetup_Blitter) : cardinal; cdecl;
External 'DIVE' Index 4;

Function DiveBlitImage(hDiveInst : cardinal; ulSrcBufNumber : cardinal; ulDstBufNumber : cardinal) : cardinal; cdecl;
External 'DIVE' Index 5;

Function DiveAcquireFrameBuffer(hDiveInst : cardinal; prectlDst : PRectl) : cardinal; cdecl;
External 'DIVE' Index 6;

Function DiveDeacquireFrameBuffer(hDiveInst : cardinal) : cardinal; cdecl;
External 'DIVE' Index 8;

Function DiveCalcFrameBufferAddress(hDiveInst           : cardinal;
                                    prectlDest          : PRectl;
                                Var pDestinationAddress : Pointer;
                                Var ulBankNumber        : cardinal;
                                Var ulRemLinesInBank    : cardinal) : cardinal; cdecl;
External 'DIVE' Index 11;

Function DiveSwitchBank(hDiveInst : cardinal; ulBankNumber : cardinal) : cardinal; cdecl;
External 'DIVE' Index 7;


Function DiveAllocImageBuffer(hDiveInst       : cardinal;
                          Var ulBufferNumber  : cardinal;
                              fccColorSpace   : FourCC;
                              ulWidth         : cardinal;
                              ulHeight        : cardinal;
                              ulLineSizeBytes : cardinal;
                          Var bImageBuffer    : Pointer) : cardinal; cdecl;
External 'DIVE' Index 12;

Function DiveFreeImageBuffer(hDiveInst      : cardinal;
                             ulBufferNumber : cardinal) : cardinal; cdecl;
External 'DIVE' Index 13;

Function DiveBeginImageBufferAccess(hDiveInst              : cardinal;
                                    ulBufferNumber         : cardinal;
                                Var pbImageBuffer          : Pointer;
                                Var ulBufferScanLineBytes  : cardinal;
                                Var ulBufferScanLines      : cardinal) : cardinal; cdecl;
External 'DIVE' Index 14;

Function DiveEndImageBufferAccess(hDiveInst : cardinal; ulBufferNumber : cardinal) : cardinal; cdecl;
External 'DIVE' Index 15;

Function DiveSetDestinationPalette(hDiveInst    : cardinal;
                                   ulStartIndex : cardinal;
                                   ulNumEntries : cardinal;
                               Var bRGB2Entries : Pointer) : cardinal; cdecl;
External 'DIVE' Index 9;

Function DiveSetSourcePalette(hDiveInst    : cardinal;
                              ulStartIndex : cardinal;
                              ulNumEntries : cardinal;
                          Var bRGB2Entries : Pointer) : cardinal; cdecl;
External 'DIVE' Index 10;

Function DiveSetTransparentBlitMode(hDiveInst    : cardinal;
                                    ulStartIndex : cardinal;
                                    ulValue1     : cardinal;
                                    ulValue2     : cardinal) : cardinal; cdecl;
External 'DIVE' Index 18;

End.
