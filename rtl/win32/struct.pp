{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1993,97 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef windows_include_files}
{$define read_interface}
{$define read_implementation}
{$endif not windows_include_files}


{$ifndef windows_include_files}

unit struct;

{  Automatically converted by H2PAS.EXE from structures.h
   Utility made by Florian Klaempfl 25th-28th september 96
   Improvements made by Mark A. Malakanov 22nd-25th may 97
   Further improvements by Michael Van Canneyt, April 1998
   define handling and error recovery by Pierre Muller, June 1998 }


  interface

   uses
      base,defines;

{$endif not windows_include_files}

{$ifdef read_interface}

  { C default packing is dword }

{$PACKRECORDS 4}
  {
     Structures.h

     Declarations for all the Windows32 API Structures

     Copyright (C) 1996 Free Software Foundation, Inc.

     Author:  Scott Christley <scottc@net-community.com>
     Date: 1996

     This file is part of the Windows32 API Library.

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License as published by the Free Software Foundation; either
     version 2 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Library General Public License for more details.

     If you are interested in a warranty or support for this source code,
     contact Scott Christley <scottc@net-community.com> for more information.

     You should have received a copy of the GNU Library General Public
     License along with this library; see the file COPYING.LIB.
     If not, write to the Free Software Foundation,
     59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   }
{$ifndef _GNU_H_WINDOWS32_STRUCTURES}
{$define _GNU_H_WINDOWS32_STRUCTURES}
{ C++ extern C conditionnal removed }
  { __cplusplus  }

  type

      { WARNING
      the variable argument list
      is not implemented for FPC
      va_list is just a dummy record }
      va_list = record
                end;
     ABC = record
          abcA : longint;
          abcB : UINT;
          abcC : longint;
       end;

     LPABC = ^ABC;

     _ABC = ABC;

     ABCFLOAT = record
          abcfA : FLOAT;
          abcfB : FLOAT;
          abcfC : FLOAT;
       end;

     LPABCFLOAT = ^ABCFLOAT;

     _ABCFLOAT = ABCFLOAT;

     ACCEL = record
          fVirt : BYTE;
          key : WORD;
          cmd : WORD;
       end;

     LPACCEL = ^ACCEL;

     tagACCEL = ACCEL;

     ACE_HEADER = record
          AceType : BYTE;
          AceFlags : BYTE;
          AceSize : WORD;
       end;

     _ACE_HEADER = ACE_HEADER;

     ACCESS_MASK = DWORD;

     REGSAM = ACCESS_MASK;

     ACCESS_ALLOWED_ACE = record
          Header : ACE_HEADER;
          Mask : ACCESS_MASK;
          SidStart : DWORD;
       end;

     _ACCESS_ALLOWED_ACE = ACCESS_ALLOWED_ACE;

     ACCESS_DENIED_ACE = record
          Header : ACE_HEADER;
          Mask : ACCESS_MASK;
          SidStart : DWORD;
       end;

     _ACCESS_DENIED_ACE = ACCESS_DENIED_ACE;

     ACCESSTIMEOUT = record
          cbSize : UINT;
          dwFlags : DWORD;
          iTimeOutMSec : DWORD;
       end;

     tagACCESSTIMEOUT = ACCESSTIMEOUT;

     ACL = record
          AclRevision : BYTE;
          Sbz1 : BYTE;
          AclSize : WORD;
          AceCount : WORD;
          Sbz2 : WORD;
       end;

     PACL = ^ACL;

     _ACL = ACL;

     ACL_REVISION_INFORMATION = record
          AclRevision : DWORD;
       end;

     _ACL_REVISION_INFORMATION = ACL_REVISION_INFORMATION;

     ACL_SIZE_INFORMATION = record
          AceCount : DWORD;
          AclBytesInUse : DWORD;
          AclBytesFree : DWORD;
       end;

     _ACL_SIZE_INFORMATION = ACL_SIZE_INFORMATION;

     ACTION_HEADER = record
          transport_id : ULONG;
          action_code : USHORT;
          reserved : USHORT;
       end;

     _ACTION_HEADER = ACTION_HEADER;

     ADAPTER_STATUS = record
          adapter_address : array[0..5] of UCHAR;
          rev_major : UCHAR;
          reserved0 : UCHAR;
          adapter_type : UCHAR;
          rev_minor : UCHAR;
          duration : WORD;
          frmr_recv : WORD;
          frmr_xmit : WORD;
          iframe_recv_err : WORD;
          xmit_aborts : WORD;
          xmit_success : DWORD;
          recv_success : DWORD;
          iframe_xmit_err : WORD;
          recv_buff_unavail : WORD;
          t1_timeouts : WORD;
          ti_timeouts : WORD;
          reserved1 : DWORD;
          free_ncbs : WORD;
          max_cfg_ncbs : WORD;
          max_ncbs : WORD;
          xmit_buf_unavail : WORD;
          max_dgram_size : WORD;
          pending_sess : WORD;
          max_cfg_sess : WORD;
          max_sess : WORD;
          max_sess_pkt_size : WORD;
          name_count : WORD;
       end;

     _ADAPTER_STATUS = ADAPTER_STATUS;

     ADDJOB_INFO_1 = record
          Path : LPTSTR;
          JobId : DWORD;
       end;

     _ADDJOB_INFO_1 = ADDJOB_INFO_1;

     ANIMATIONINFO = record
          cbSize : UINT;
          iMinAnimate : longint;
       end;

     LPANIMATIONINFO = ^ANIMATIONINFO;

     tagANIMATIONINFO = ANIMATIONINFO;

     RECT = record
          left : LONG;
          top : LONG;
          right : LONG;
          bottom : LONG;
       end;

     LPRECT = ^RECT;

     PRECT = ^RECT;

     _RECT = RECT;

     RECTL = record
          left : LONG;
          top : LONG;
          right : LONG;
          bottom : LONG;
       end;

     _RECTL = RECTL;

     APPBARDATA = record
          cbSize : DWORD;
          hWnd : HWND;
          uCallbackMessage : UINT;
          uEdge : UINT;
          rc : RECT;
          lParam : LPARAM;
       end;

     PAPPBARDATA = ^APPBARDATA;

     _AppBarData = APPBARDATA;

     BITMAP = record
          bmType : LONG;
          bmWidth : LONG;
          bmHeight : LONG;
          bmWidthBytes : LONG;
          bmPlanes : WORD;
          bmBitsPixel : WORD;
          bmBits : LPVOID;
       end;

     PBITMAP = ^BITMAP;

     NPBITMAP = ^BITMAP;

     LPBITMAP = ^BITMAP;

     tagBITMAP = BITMAP;

     BITMAPCOREHEADER = record
          bcSize : DWORD;
          bcWidth : WORD;
          bcHeight : WORD;
          bcPlanes : WORD;
          bcBitCount : WORD;
       end;

     tagBITMAPCOREHEADER = BITMAPCOREHEADER;

     RGBTRIPLE = record
          rgbtBlue : BYTE;
          rgbtGreen : BYTE;
          rgbtRed : BYTE;
       end;

     tagRGBTRIPLE = RGBTRIPLE;

     BITMAPCOREINFO = record
          bmciHeader : BITMAPCOREHEADER;
          bmciColors : array[0..0] of RGBTRIPLE;
       end;

     PBITMAPCOREINFO = ^BITMAPCOREINFO;

     LPBITMAPCOREINFO = ^BITMAPCOREINFO;

     _BITMAPCOREINFO = BITMAPCOREINFO;
(* error
  WORD    bfReserved1;
  WORD    bfReserved2;
 in declarator_list *)

     BITMAPINFOHEADER = record
          biSize : DWORD;
          biWidth : LONG;
          biHeight : LONG;
          biPlanes : WORD;
          biBitCount : WORD;
          biCompression : DWORD;
          biSizeImage : DWORD;
          biXPelsPerMeter : LONG;
          biYPelsPerMeter : LONG;
          biClrUsed : DWORD;
          biClrImportant : DWORD;
       end;

     LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;

     PBITMAPINFO = ^BITMAPINFOHEADER;

     tagBITMAPINFOHEADER = BITMAPINFOHEADER;

     RGBQUAD = record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;

     tagRGBQUAD = RGBQUAD;

     BITMAPINFO = record
          bmiHeader : BITMAPINFOHEADER;
          bmiColors : array[0..0] of RGBQUAD;
       end;

     LPBITMAPINFO = ^BITMAPINFO;

     tagBITMAPINFO = BITMAPINFO;

     FXPT2DOT30 = longint;

     LPFXPT2DOT30 = ^FXPT2DOT30;

     CIEXYZ = record
          ciexyzX : FXPT2DOT30;
          ciexyzY : FXPT2DOT30;
          ciexyzZ : FXPT2DOT30;
       end;

     tagCIEXYZ = CIEXYZ;

     LPCIEXYZ = ^CIEXYZ;

     CIEXYZTRIPLE = record
          ciexyzRed : CIEXYZ;
          ciexyzGreen : CIEXYZ;
          ciexyzBlue : CIEXYZ;
       end;

     tagCIEXYZTRIPLE = CIEXYZTRIPLE;

     LPCIEXYZTRIPLE = ^CIEXYZTRIPLE;

     BITMAPV4HEADER = record
          bV4Size : DWORD;
          bV4Width : LONG;
          bV4Height : LONG;
          bV4Planes : WORD;
          bV4BitCount : WORD;
          bV4V4Compression : DWORD;
          bV4SizeImage : DWORD;
          bV4XPelsPerMeter : LONG;
          bV4YPelsPerMeter : LONG;
          bV4ClrUsed : DWORD;
          bV4ClrImportant : DWORD;
          bV4RedMask : DWORD;
          bV4GreenMask : DWORD;
          bV4BlueMask : DWORD;
          bV4AlphaMask : DWORD;
          bV4CSType : DWORD;
          bV4Endpoints : CIEXYZTRIPLE;
          bV4GammaRed : DWORD;
          bV4GammaGreen : DWORD;
          bV4GammaBlue : DWORD;
       end;

     LPBITMAPV4HEADER = ^BITMAPV4HEADER;

     PBITMAPV4HEADER = ^BITMAPV4HEADER;

     BLOB = record
          cbSize : ULONG;
          pBlobData : ^BYTE;
       end;

     _BLOB = BLOB;

     SHITEMID = record
          cb : USHORT;
          abID : array[0..0] of BYTE;
       end;

     LPSHITEMID = ^SHITEMID;

     _SHITEMID = SHITEMID;
(* Const before type ignored *)

     LPCSHITEMID = ^SHITEMID;

     ITEMIDLIST = record
          mkid : SHITEMID;
       end;

     LPITEMIDLIST = ^ITEMIDLIST;

     _ITEMIDLIST = ITEMIDLIST;
(* Const before type ignored *)

     LPCITEMIDLIST = ^ITEMIDLIST;

     BROWSEINFO = record
          hwndOwner : HWND;
          pidlRoot : LPCITEMIDLIST;
          pszDisplayName : LPSTR;
          lpszTitle : LPCSTR;
          ulFlags : UINT;
          lpfn : BFFCALLBACK;
          lParam : LPARAM;
          iImage : longint;
       end;

     PBROWSEINFO = ^BROWSEINFO;

     LPBROWSEINFO = ^BROWSEINFO;

     _browseinfo = BROWSEINFO;

     FILETIME = record
          dwLowDateTime : DWORD;
          dwHighDateTime : DWORD;
       end;

     LPFILETIME = ^FILETIME;

     PFILETIME = ^FILETIME;

     _FILETIME = FILETIME;

     BY_HANDLE_FILE_INFORMATION = record
          dwFileAttributes : DWORD;
          ftCreationTime : FILETIME;
          ftLastAccessTime : FILETIME;
          ftLastWriteTime : FILETIME;
          dwVolumeSerialNumber : DWORD;
          nFileSizeHigh : DWORD;
          nFileSizeLow : DWORD;
          nNumberOfLinks : DWORD;
          nFileIndexHigh : DWORD;
          nFileIndexLow : DWORD;
       end;

     LPBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;

     _BY_HANDLE_FILE_INFORMATION = BY_HANDLE_FILE_INFORMATION;

     FIXED = record
          fract : WORD;
          value : integer;
       end;

     _FIXED = FIXED;

     POINT = record
          x : LONG;
          y : LONG;
       end;

     LPPOINT = ^POINT;

     PPOINT = ^POINT;

     tagPOINT = POINT;

     POINTFX = record
          x : FIXED;
          y : FIXED;
       end;

     tagPOINTFX = POINTFX;

     POINTL = record
          x : LONG;
          y : LONG;
       end;

     _POINTL = POINTL;

     POINTS = record
          x : SHORT;
          y : SHORT;
       end;

     tagPOINTS = POINTS;

     CANDIDATEFORM = record
          dwIndex : DWORD;
          dwStyle : DWORD;
          ptCurrentPos : POINT;
          rcArea : RECT;
       end;

     LPCANDIDATEFORM = ^CANDIDATEFORM;

     _tagCANDIDATEFORM = CANDIDATEFORM;

     CANDIDATELIST = record
          dwSize : DWORD;
          dwStyle : DWORD;
          dwCount : DWORD;
          dwSelection : DWORD;
          dwPageStart : DWORD;
          dwPageSize : DWORD;
          dwOffset : array[0..0] of DWORD;
       end;

     LPCANDIDATELIST = ^CANDIDATELIST;

     _tagCANDIDATELIST = CANDIDATELIST;

     CREATESTRUCT = record
          lpCreateParams : LPVOID;
          hInstance : HINST;
          hMenu : HMENU;
          hwndParent : HWND;
          cy : longint;
          cx : longint;
          y : longint;
          x : longint;
          style : LONG;
          lpszName : LPCTSTR;
          lpszClass : LPCTSTR;
          dwExStyle : DWORD;
       end;

     LPCREATESTRUCT = ^CREATESTRUCT;

     tagCREATESTRUCT = CREATESTRUCT;

     CBT_CREATEWND = record
          lpcs : LPCREATESTRUCT;
          hwndInsertAfter : HWND;
       end;

     tagCBT_CREATEWND = CBT_CREATEWND;

     CBTACTIVATESTRUCT = record
          fMouse : WINBOOL;
          hWndActive : HWND;
       end;

     tagCBTACTIVATESTRUCT = CBTACTIVATESTRUCT;

     CHAR_INFO = record
          Char : record
              case longint of
                 0 : ( UnicodeChar : WCHAR );
                 1 : ( AsciiChar : CHAR );
              end;
          Attributes : WORD;
       end;

     PCHAR_INFO = ^CHAR_INFO;

     _CHAR_INFO = CHAR_INFO;

     CHARFORMAT = record
          cbSize : UINT;
          dwMask : DWORD;
          dwEffects : DWORD;
          yHeight : LONG;
          yOffset : LONG;
          crTextColor : COLORREF;
          bCharSet : BYTE;
          bPitchAndFamily : BYTE;
          szFaceName : array[0..(LF_FACESIZE)-1] of TCHAR;
       end;

     _charformat = CHARFORMAT;

     CHARRANGE = record
          cpMin : LONG;
          cpMax : LONG;
       end;

     _charrange = CHARRANGE;

     CHARSET = record
          aflBlock : array[0..2] of DWORD;
          flLang : DWORD;
       end;

     tagCHARSET = CHARSET;

     FONTSIGNATURE = record
          fsUsb : array[0..3] of DWORD;
          fsCsb : array[0..1] of DWORD;
       end;

     LPFONTSIGNATURE = ^FONTSIGNATURE;

     tagFONTSIGNATURE = FONTSIGNATURE;

     CHARSETINFO = record
          ciCharset : UINT;
          ciACP : UINT;
          fs : FONTSIGNATURE;
       end;

     LPCHARSETINFO = ^CHARSETINFO;

     {CHOOSECOLOR = record confilcts with function ChooseColor }
     TCHOOSECOLOR = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hInstance : HWND;
          rgbResult : COLORREF;
          lpCustColors : ^COLORREF;
          Flags : DWORD;
          lCustData : LPARAM;
          lpfnHook : LPCCHOOKPROC;
          lpTemplateName : LPCTSTR;
       end;

     LPCHOOSECOLOR = ^TCHOOSECOLOR;

     LOGFONT = record
          lfHeight : LONG;
          lfWidth : LONG;
          lfEscapement : LONG;
          lfOrientation : LONG;
          lfWeight : LONG;
          lfItalic : BYTE;
          lfUnderline : BYTE;
          lfStrikeOut : BYTE;
          lfCharSet : BYTE;
          lfOutPrecision : BYTE;
          lfClipPrecision : BYTE;
          lfQuality : BYTE;
          lfPitchAndFamily : BYTE;
          lfFaceName : array[0..(LF_FACESIZE)-1] of TCHAR;
       end;

     LPLOGFONT = ^LOGFONT;

     PLOGFONT = ^LOGFONT;

     tagLOGFONT = LOGFONT;

     {CHOOSEFONT = record conflicts with ChosseFont function }
     TCHOOSEFONT = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hDC : HDC;
          lpLogFont : LPLOGFONT;
          iPointSize : INT;
          Flags : DWORD;
          rgbColors : DWORD;
          lCustData : LPARAM;
          lpfnHook : LPCFHOOKPROC;
          lpTemplateName : LPCTSTR;
          hInstance : HINST;
          lpszStyle : LPTSTR;
          nFontType : WORD;
          ___MISSING_ALIGNMENT__ : WORD;
          nSizeMin : INT;
          nSizeMax : INT;
       end;

     LPCHOOSEFONT = ^TCHOOSEFONT;

     CIDA = record
          cidl : UINT;
          aoffset : array[0..0] of UINT;
       end;

     LPIDA = ^CIDA;

     _IDA = CIDA;

     CLIENTCREATESTRUCT = record
          hWindowMenu : HANDLE;
          idFirstChild : UINT;
       end;

     tagCLIENTCREATESTRUCT = CLIENTCREATESTRUCT;

     LPCLIENTCREATESTRUCT = ^CLIENTCREATESTRUCT;

     CMINVOKECOMMANDINFO = record
          cbSize : DWORD;
          fMask : DWORD;
          hwnd : HWND;
          lpVerb : LPCSTR;
          lpParameters : LPCSTR;
          lpDirectory : LPCSTR;
          nShow : longint;
          dwHotKey : DWORD;
          hIcon : HANDLE;
       end;

     LPCMINVOKECOMMANDINFO = ^CMINVOKECOMMANDINFO;

     _CMInvokeCommandInfo = CMINVOKECOMMANDINFO;

     COLORADJUSTMENT = record
          caSize : WORD;
          caFlags : WORD;
          caIlluminantIndex : WORD;
          caRedGamma : WORD;
          caGreenGamma : WORD;
          caBlueGamma : WORD;
          caReferenceBlack : WORD;
          caReferenceWhite : WORD;
          caContrast : SHORT;
          caBrightness : SHORT;
          caColorfulness : SHORT;
          caRedGreenTint : SHORT;
       end;

     LPCOLORADJUSTMENT = ^COLORADJUSTMENT;

     tagCOLORADJUSTMENT = COLORADJUSTMENT;

     COLORMAP = record
          from : COLORREF;
          _to : COLORREF;
       end;

     LPCOLORMAP = ^COLORMAP;

     _COLORMAP = COLORMAP;

     DCB = record
          DCBlength : DWORD;
          BaudRate : DWORD;
          flag0 : longint;
          wReserved : WORD;
          XonLim : WORD;
          XoffLim : WORD;
          ByteSize : BYTE;
          Parity : BYTE;
          StopBits : BYTE;
          XonChar : char;
          XoffChar : char;
          ErrorChar : char;
          EofChar : char;
          EvtChar : char;
          wReserved1 : WORD;
       end;

     LPDCB = ^DCB;

     _DCB = DCB;
  const
     bm_DCB_fBinary = $1;
     bp_DCB_fBinary = 0;
     bm_DCB_fParity = $2;
     bp_DCB_fParity = 1;
     bm_DCB_fOutxCtsFlow = $4;
     bp_DCB_fOutxCtsFlow = 2;
     bm_DCB_fOutxDsrFlow = $8;
     bp_DCB_fOutxDsrFlow = 3;
     bm_DCB_fDtrControl = $30;
     bp_DCB_fDtrControl = 4;
     bm_DCB_fDsrSensitivity = $40;
     bp_DCB_fDsrSensitivity = 6;
     bm_DCB_fTXContinueOnXoff = $80;
     bp_DCB_fTXContinueOnXoff = 7;
     bm_DCB_fOutX = $100;
     bp_DCB_fOutX = 8;
     bm_DCB_fInX = $200;
     bp_DCB_fInX = 9;
     bm_DCB_fErrorChar = $400;
     bp_DCB_fErrorChar = 10;
     bm_DCB_fNull = $800;
     bp_DCB_fNull = 11;
     bm_DCB_fRtsControl = $3000;
     bp_DCB_fRtsControl = 12;
     bm_DCB_fAbortOnError = $4000;
     bp_DCB_fAbortOnError = 14;
     bm_DCB_fDummy2 = $FFFF8000;
     bp_DCB_fDummy2 = 15;
  function fBinary(var a : DCB) : DWORD;
  procedure set_fBinary(var a : DCB; __fBinary : DWORD);
  function fParity(var a : DCB) : DWORD;
  procedure set_fParity(var a : DCB; __fParity : DWORD);
  function fOutxCtsFlow(var a : DCB) : DWORD;
  procedure set_fOutxCtsFlow(var a : DCB; __fOutxCtsFlow : DWORD);
  function fOutxDsrFlow(var a : DCB) : DWORD;
  procedure set_fOutxDsrFlow(var a : DCB; __fOutxDsrFlow : DWORD);
  function fDtrControl(var a : DCB) : DWORD;
  procedure set_fDtrControl(var a : DCB; __fDtrControl : DWORD);
  function fDsrSensitivity(var a : DCB) : DWORD;
  procedure set_fDsrSensitivity(var a : DCB; __fDsrSensitivity : DWORD);
  function fTXContinueOnXoff(var a : DCB) : DWORD;
  procedure set_fTXContinueOnXoff(var a : DCB; __fTXContinueOnXoff : DWORD);
  function fOutX(var a : DCB) : DWORD;
  procedure set_fOutX(var a : DCB; __fOutX : DWORD);
  function fInX(var a : DCB) : DWORD;
  procedure set_fInX(var a : DCB; __fInX : DWORD);
  function fErrorChar(var a : DCB) : DWORD;
  procedure set_fErrorChar(var a : DCB; __fErrorChar : DWORD);
  function fNull(var a : DCB) : DWORD;
  procedure set_fNull(var a : DCB; __fNull : DWORD);
  function fRtsControl(var a : DCB) : DWORD;
  procedure set_fRtsControl(var a : DCB; __fRtsControl : DWORD);
  function fAbortOnError(var a : DCB) : DWORD;
  procedure set_fAbortOnError(var a : DCB; __fAbortOnError : DWORD);
  function fDummy2(var a : DCB) : DWORD;
  procedure set_fDummy2(var a : DCB; __fDummy2 : DWORD);

  type

     COMMCONFIG = record
          dwSize : DWORD;
          wVersion : WORD;
          wReserved : WORD;
          dcb : DCB;
          dwProviderSubType : DWORD;
          dwProviderOffset : DWORD;
          dwProviderSize : DWORD;
          wcProviderData : array[0..0] of WCHAR;
       end;

     LPCOMMCONFIG = ^COMMCONFIG;

     _COMM_CONFIG = COMMCONFIG;

     COMMPROP = record
          wPacketLength : WORD;
          wPacketVersion : WORD;
          dwServiceMask : DWORD;
          dwReserved1 : DWORD;
          dwMaxTxQueue : DWORD;
          dwMaxRxQueue : DWORD;
          dwMaxBaud : DWORD;
          dwProvSubType : DWORD;
          dwProvCapabilities : DWORD;
          dwSettableParams : DWORD;
          dwSettableBaud : DWORD;
          wSettableData : WORD;
          wSettableStopParity : WORD;
          dwCurrentTxQueue : DWORD;
          dwCurrentRxQueue : DWORD;
          dwProvSpec1 : DWORD;
          dwProvSpec2 : DWORD;
          wcProvChar : array[0..0] of WCHAR;
       end;

     LPCOMMPROP = ^COMMPROP;

     _COMMPROP = COMMPROP;

     COMMTIMEOUTS = record
          ReadIntervalTimeout : DWORD;
          ReadTotalTimeoutMultiplier : DWORD;
          ReadTotalTimeoutConstant : DWORD;
          WriteTotalTimeoutMultiplier : DWORD;
          WriteTotalTimeoutConstant : DWORD;
       end;

     LPCOMMTIMEOUTS = ^COMMTIMEOUTS;

     _COMMTIMEOUTS = COMMTIMEOUTS;

     COMPAREITEMSTRUCT = record
          CtlType : UINT;
          CtlID : UINT;
          hwndItem : HWND;
          itemID1 : UINT;
          itemData1 : DWORD;
          itemID2 : UINT;
          itemData2 : DWORD;
       end;

     tagCOMPAREITEMSTRUCT = COMPAREITEMSTRUCT;

     COMPCOLOR = record
          crText : COLORREF;
          crBackground : COLORREF;
          dwEffects : DWORD;
       end;

     COMPOSITIONFORM = record
          dwStyle : DWORD;
          ptCurrentPos : POINT;
          rcArea : RECT;
       end;

     LPCOMPOSITIONFORM = ^COMPOSITIONFORM;

     _tagCOMPOSITIONFORM = COMPOSITIONFORM;

     COMSTAT = record
          flag0 : longint;
          cbInQue : DWORD;
          cbOutQue : DWORD;
       end;

     LPCOMSTAT = ^COMSTAT;

     _COMSTAT = COMSTAT;
  const
     bm_COMSTAT_fCtsHold = $1;
     bp_COMSTAT_fCtsHold = 0;
     bm_COMSTAT_fDsrHold = $2;
     bp_COMSTAT_fDsrHold = 1;
     bm_COMSTAT_fRlsdHold = $4;
     bp_COMSTAT_fRlsdHold = 2;
     bm_COMSTAT_fXoffHold = $8;
     bp_COMSTAT_fXoffHold = 3;
     bm_COMSTAT_fXoffSent = $10;
     bp_COMSTAT_fXoffSent = 4;
     bm_COMSTAT_fEof = $20;
     bp_COMSTAT_fEof = 5;
     bm_COMSTAT_fTxim = $40;
     bp_COMSTAT_fTxim = 6;
     bm_COMSTAT_fReserved = $FFFFFF80;
     bp_COMSTAT_fReserved = 7;
  function fCtsHold(var a : COMSTAT) : DWORD;
  procedure set_fCtsHold(var a : COMSTAT; __fCtsHold : DWORD);
  function fDsrHold(var a : COMSTAT) : DWORD;
  procedure set_fDsrHold(var a : COMSTAT; __fDsrHold : DWORD);
  function fRlsdHold(var a : COMSTAT) : DWORD;
  procedure set_fRlsdHold(var a : COMSTAT; __fRlsdHold : DWORD);
  function fXoffHold(var a : COMSTAT) : DWORD;
  procedure set_fXoffHold(var a : COMSTAT; __fXoffHold : DWORD);
  function fXoffSent(var a : COMSTAT) : DWORD;
  procedure set_fXoffSent(var a : COMSTAT; __fXoffSent : DWORD);
  function fEof(var a : COMSTAT) : DWORD;
  procedure set_fEof(var a : COMSTAT; __fEof : DWORD);
  function fTxim(var a : COMSTAT) : DWORD;
  procedure set_fTxim(var a : COMSTAT; __fTxim : DWORD);
  function fReserved(var a : COMSTAT) : DWORD;
  procedure set_fReserved(var a : COMSTAT; __fReserved : DWORD);

  type

     CONSOLE_CURSOR_INFO = record
          dwSize : DWORD;
          bVisible : WINBOOL;
       end;

     PCONSOLE_CURSOR_INFO = ^CONSOLE_CURSOR_INFO;

     _CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO;

     COORD = record
          X : SHORT;
          Y : SHORT;
       end;

     _COORD = COORD;

     SMALL_RECT = record
          Left : SHORT;
          Top : SHORT;
          Right : SHORT;
          Bottom : SHORT;
       end;

     PSMALL_RECT = ^SMALL_RECT;

     _SMALL_RECT = SMALL_RECT;

     CONSOLE_SCREEN_BUFFER_INFO = record
          dwSize : COORD;
          dwCursorPosition : COORD;
          wAttributes : WORD;
          srWindow : SMALL_RECT;
          dwMaximumWindowSize : COORD;
       end;

     PCONSOLE_SCREEN_BUFFER_INFO = ^CONSOLE_SCREEN_BUFFER_INFO;

     _CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO;
{$ifdef __i386__}

  type

     FLOATING_SAVE_AREA = record
          ControlWord : DWORD;
          StatusWord : DWORD;
          TagWord : DWORD;
          ErrorOffset : DWORD;
          ErrorSelector : DWORD;
          DataOffset : DWORD;
          DataSelector : DWORD;
          RegisterArea : array[0..79] of BYTE;
          Cr0NpxState : DWORD;
       end;

     _FLOATING_SAVE_AREA = FLOATING_SAVE_AREA;

     CONTEXT = record
          ContextFlags : DWORD;
          Dr0 : DWORD;
          Dr1 : DWORD;
          Dr2 : DWORD;
          Dr3 : DWORD;
          Dr6 : DWORD;
          Dr7 : DWORD;
          FloatSave : FLOATING_SAVE_AREA;
          SegGs : DWORD;
          SegFs : DWORD;
          SegEs : DWORD;
          SegDs : DWORD;
          Edi : DWORD;
          Esi : DWORD;
          Ebx : DWORD;
          Edx : DWORD;
          Ecx : DWORD;
          Eax : DWORD;
          Ebp : DWORD;
          Eip : DWORD;
          SegCs : DWORD;
          EFlags : DWORD;
          Esp : DWORD;
          SegSs : DWORD;
       end;

     PCONTEXT = ^CONTEXT;

     LPCONTEXT = ^CONTEXT;

     _CONTEXT = CONTEXT;
{$else}
  { __ppc__  }
  { Floating point registers returned when CONTEXT_FLOATING_POINT is set  }
  { Integer registers returned when CONTEXT_INTEGER is set.   }
  { Condition register  }
  { Fixed point exception register  }
  { The following are set when CONTEXT_CONTROL is set.   }
  { Machine status register  }
  { Instruction address register  }
  { Link register  }
  { Control register  }
  { Control which context values are returned  }
  { Registers returned if CONTEXT_DEBUG_REGISTERS is set.   }
  { Breakpoint Register 1  }
  { Breakpoint Register 2  }
  { Breakpoint Register 3  }
  { Breakpoint Register 4  }
  { Breakpoint Register 5  }
  { Breakpoint Register 6  }
  { Debug Status Register  }
  { Debug Control Register  }

  type

     CONTEXT = record
          Fpr0 : double;
          Fpr1 : double;
          Fpr2 : double;
          Fpr3 : double;
          Fpr4 : double;
          Fpr5 : double;
          Fpr6 : double;
          Fpr7 : double;
          Fpr8 : double;
          Fpr9 : double;
          Fpr10 : double;
          Fpr11 : double;
          Fpr12 : double;
          Fpr13 : double;
          Fpr14 : double;
          Fpr15 : double;
          Fpr16 : double;
          Fpr17 : double;
          Fpr18 : double;
          Fpr19 : double;
          Fpr20 : double;
          Fpr21 : double;
          Fpr22 : double;
          Fpr23 : double;
          Fpr24 : double;
          Fpr25 : double;
          Fpr26 : double;
          Fpr27 : double;
          Fpr28 : double;
          Fpr29 : double;
          Fpr30 : double;
          Fpr31 : double;
          Fpscr : double;
          Gpr0 : DWORD;
          Gpr1 : DWORD;
          Gpr2 : DWORD;
          Gpr3 : DWORD;
          Gpr4 : DWORD;
          Gpr5 : DWORD;
          Gpr6 : DWORD;
          Gpr7 : DWORD;
          Gpr8 : DWORD;
          Gpr9 : DWORD;
          Gpr10 : DWORD;
          Gpr11 : DWORD;
          Gpr12 : DWORD;
          Gpr13 : DWORD;
          Gpr14 : DWORD;
          Gpr15 : DWORD;
          Gpr16 : DWORD;
          Gpr17 : DWORD;
          Gpr18 : DWORD;
          Gpr19 : DWORD;
          Gpr20 : DWORD;
          Gpr21 : DWORD;
          Gpr22 : DWORD;
          Gpr23 : DWORD;
          Gpr24 : DWORD;
          Gpr25 : DWORD;
          Gpr26 : DWORD;
          Gpr27 : DWORD;
          Gpr28 : DWORD;
          Gpr29 : DWORD;
          Gpr30 : DWORD;
          Gpr31 : DWORD;
          Cr : DWORD;
          Xer : DWORD;
          Msr : DWORD;
          Iar : DWORD;
          Lr : DWORD;
          Ctr : DWORD;
          ContextFlags : DWORD;
          Fill : array[0..2] of DWORD;
          Dr0 : DWORD;
          Dr1 : DWORD;
          Dr2 : DWORD;
          Dr3 : DWORD;
          Dr4 : DWORD;
          Dr5 : DWORD;
          Dr6 : DWORD;
          Dr7 : DWORD;
       end;

     PCONTEXT = ^CONTEXT;

     LPCONTEXT = ^CONTEXT;
{$endif}

  type

     LIST_ENTRY = record
          Flink : ^_LIST_ENTRY;
          Blink : ^_LIST_ENTRY;
       end;

     PLIST_ENTRY = ^LIST_ENTRY;

     _LIST_ENTRY = LIST_ENTRY;

     CRITICAL_SECTION_DEBUG = record
          _Type : WORD;
          CreatorBackTraceIndex : WORD;
          CriticalSection : ^_CRITICAL_SECTION;
          ProcessLocksList : LIST_ENTRY;
          EntryCount : DWORD;
          ContentionCount : DWORD;
          Depth : DWORD;
          OwnerBackTrace : array[0..4] of PVOID;
       end;

     PCRITICAL_SECTION_DEBUG = ^CRITICAL_SECTION_DEBUG;

     _CRITICAL_SECTION_DEBUG = CRITICAL_SECTION_DEBUG;

     CRITICAL_SECTION = record
          DebugInfo : PCRITICAL_SECTION_DEBUG;
          LockCount : LONG;
          RecursionCount : LONG;
          OwningThread : HANDLE;
          LockSemaphore : HANDLE;
          Reserved : DWORD;
       end;

     PCRITICAL_SECTION = ^CRITICAL_SECTION;

     LPCRITICAL_SECTION = ^CRITICAL_SECTION;

     _CRITICAL_SECTION = CRITICAL_SECTION;
  { SECURITY_CONTEXT_TRACKING_MODE ContextTrackingMode;  }

     SECURITY_QUALITY_OF_SERVICE = record
          Length : DWORD;
          ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
          ContextTrackingMode : WINBOOL;
          EffectiveOnly : BOOLEAN;
       end;

     _SECURITY_QUALITY_OF_SERVICE = SECURITY_QUALITY_OF_SERVICE;

     CONVCONTEXT = record
          cb : UINT;
          wFlags : UINT;
          wCountryID : UINT;
          iCodePage : longint;
          dwLangID : DWORD;
          dwSecurity : DWORD;
          qos : SECURITY_QUALITY_OF_SERVICE;
       end;

     tagCONVCONTEXT = CONVCONTEXT;

     PCONVCONTEXT = ^CONVCONTEXT;

     CONVINFO = record
          cb : DWORD;
          hUser : DWORD;
          hConvPartner : HCONV;
          hszSvcPartner : HSZ;
          hszServiceReq : HSZ;
          hszTopic : HSZ;
          hszItem : HSZ;
          wFmt : UINT;
          wType : UINT;
          wStatus : UINT;
          wConvst : UINT;
          wLastError : UINT;
          hConvList : HCONVLIST;
          ConvCtxt : CONVCONTEXT;
          _hwnd : HWND;
          hwndPartner : HWND;
       end;

     tagCONVINFO = CONVINFO;

     COPYDATASTRUCT = record
          dwData : DWORD;
          cbData : DWORD;
          lpData : PVOID;
       end;

     tagCOPYDATASTRUCT = COPYDATASTRUCT;

     CPINFO = record
          MaxCharSize : UINT;
          DefaultChar : array[0..(MAX_DEFAULTCHAR)-1] of BYTE;
          LeadByte : array[0..(MAX_LEADBYTES)-1] of BYTE;
       end;

     LPCPINFO = ^CPINFO;

     _cpinfo = CPINFO;

     CPLINFO = record
          idIcon : longint;
          idName : longint;
          idInfo : longint;
          lData : LONG;
       end;

     tagCPLINFO = CPLINFO;

     CREATE_PROCESS_DEBUG_INFO = record
          hFile : HANDLE;
          hProcess : HANDLE;
          hThread : HANDLE;
          lpBaseOfImage : LPVOID;
          dwDebugInfoFileOffset : DWORD;
          nDebugInfoSize : DWORD;
          lpThreadLocalBase : LPVOID;
          lpStartAddress : LPTHREAD_START_ROUTINE;
          lpImageName : LPVOID;
          fUnicode : WORD;
       end;

     _CREATE_PROCESS_DEBUG_INFO = CREATE_PROCESS_DEBUG_INFO;

     CREATE_THREAD_DEBUG_INFO = record
          hThread : HANDLE;
          lpThreadLocalBase : LPVOID;
          lpStartAddress : LPTHREAD_START_ROUTINE;
       end;

     _CREATE_THREAD_DEBUG_INFO = CREATE_THREAD_DEBUG_INFO;
  (*
   TODO: sockets
  typedef struct _SOCKET_ADDRESS {
    LPSOCKADDR lpSockaddr ;
    INT iSockaddrLength ;
  } SOCKET_ADDRESS,  PSOCKET_ADDRESS,  LPSOCKET_ADDRESS;
   }
  {
  typedef struct _CSADDR_INFO {
    SOCKET_ADDRESS  LocalAddr;
    SOCKET_ADDRESS  RemoteAddr;
    INT             iSocketType;
    INT             iProtocol;
  } CSADDR_INFO;
    *)

     CURRENCYFMT = record
          NumDigits : UINT;
          LeadingZero : UINT;
          Grouping : UINT;
          lpDecimalSep : LPTSTR;
          lpThousandSep : LPTSTR;
          NegativeOrder : UINT;
          PositiveOrder : UINT;
          lpCurrencySymbol : LPTSTR;
       end;

     _currencyfmt = CURRENCYFMT;

     CURSORSHAPE = record
          xHotSpot : longint;
          yHotSpot : longint;
          cx : longint;
          cy : longint;
          cbWidth : longint;
          Planes : BYTE;
          BitsPixel : BYTE;
       end;

     LPCURSORSHAPE = ^CURSORSHAPE;

     tagCURSORSHAPE = CURSORSHAPE;

     CWPRETSTRUCT = record
          lResult : LRESULT;
          lParam : LPARAM;
          wParam : WPARAM;
          message : DWORD;
          hwnd : HWND;
       end;

     tagCWPRETSTRUCT = CWPRETSTRUCT;

     CWPSTRUCT = record
          lParam : LPARAM;
          wParam : WPARAM;
          message : UINT;
          hwnd : HWND;
       end;

     tagCWPSTRUCT = CWPSTRUCT;

     DATATYPES_INFO_1 = record
          pName : LPTSTR;
       end;

     _DATATYPES_INFO_1 = DATATYPES_INFO_1;

     DDEACK = record
          flag0 : word;
       end;
  const
     bm_DDEACK_bAppReturnCode = $FF;
     bp_DDEACK_bAppReturnCode = 0;
     bm_DDEACK_reserved = $3F00;
     bp_DDEACK_reserved = 8;
     bm_DDEACK_fBusy = $4000;
     bp_DDEACK_fBusy = 14;
     bm_DDEACK_fAck = $8000;
     bp_DDEACK_fAck = 15;
  function bAppReturnCode(var a : DDEACK) : word;
  procedure set_bAppReturnCode(var a : DDEACK; __bAppReturnCode : word);
  function reserved(var a : DDEACK) : word;
  procedure set_reserved(var a : DDEACK; __reserved : word);
  function fBusy(var a : DDEACK) : word;
  procedure set_fBusy(var a : DDEACK; __fBusy : word);
  function fAck(var a : DDEACK) : word;
  procedure set_fAck(var a : DDEACK; __fAck : word);

  type

     DDEADVISE = record
          flag0 : word;
          cfFormat : integer;
       end;
  const
     bm_DDEADVISE_reserved = $3FFF;
     bp_DDEADVISE_reserved = 0;
     bm_DDEADVISE_fDeferUpd = $4000;
     bp_DDEADVISE_fDeferUpd = 14;
     bm_DDEADVISE_fAckReq = $8000;
     bp_DDEADVISE_fAckReq = 15;
  function reserved(var a : DDEADVISE) : word;
  procedure set_reserved(var a : DDEADVISE; __reserved : word);
  function fDeferUpd(var a : DDEADVISE) : word;
  procedure set_fDeferUpd(var a : DDEADVISE; __fDeferUpd : word);
  function fAckReq(var a : DDEADVISE) : word;
  procedure set_fAckReq(var a : DDEADVISE; __fAckReq : word);

  type

     DDEDATA = record
          flag0 : word;
          cfFormat : integer;
          Value : array[0..0] of BYTE;
       end;
  const
     bm_DDEDATA_unused = $FFF;
     bp_DDEDATA_unused = 0;
     bm_DDEDATA_fResponse = $1000;
     bp_DDEDATA_fResponse = 12;
     bm_DDEDATA_fRelease = $2000;
     bp_DDEDATA_fRelease = 13;
     bm_DDEDATA_reserved = $4000;
     bp_DDEDATA_reserved = 14;
     bm_DDEDATA_fAckReq = $8000;
     bp_DDEDATA_fAckReq = 15;
  function unused(var a : DDEDATA) : word;
  procedure set_unused(var a : DDEDATA; __unused : word);
  function fResponse(var a : DDEDATA) : word;
  procedure set_fResponse(var a : DDEDATA; __fResponse : word);
  function fRelease(var a : DDEDATA) : word;
  procedure set_fRelease(var a : DDEDATA; __fRelease : word);
  function reserved(var a : DDEDATA) : word;
  procedure set_reserved(var a : DDEDATA; __reserved : word);
  function fAckReq(var a : DDEDATA) : word;
  procedure set_fAckReq(var a : DDEDATA; __fAckReq : word);

  type

     DDELN = record
          flag0 : word;
          cfFormat : integer;
       end;
  const
     bm_DDELN_unused = $1FFF;
     bp_DDELN_unused = 0;
     bm_DDELN_fRelease = $2000;
     bp_DDELN_fRelease = 13;
     bm_DDELN_fDeferUpd = $4000;
     bp_DDELN_fDeferUpd = 14;
     bm_DDELN_fAckReq = $8000;
     bp_DDELN_fAckReq = 15;
  function unused(var a : DDELN) : word;
  procedure set_unused(var a : DDELN; __unused : word);
  function fRelease(var a : DDELN) : word;
  procedure set_fRelease(var a : DDELN; __fRelease : word);
  function fDeferUpd(var a : DDELN) : word;
  procedure set_fDeferUpd(var a : DDELN; __fDeferUpd : word);
  function fAckReq(var a : DDELN) : word;
  procedure set_fAckReq(var a : DDELN; __fAckReq : word);

  type

     DDEML_MSG_HOOK_DATA = record
          uiLo : UINT;
          uiHi : UINT;
          cbData : DWORD;
          Data : array[0..7] of DWORD;
       end;

     tagDDEML_MSG_HOOK_DATA = DDEML_MSG_HOOK_DATA;

     DDEPOKE = record
          flag0 : word;
          cfFormat : integer;
          Value : array[0..0] of BYTE;
       end;
  const
     bm_DDEPOKE_unused = $1FFF;
     bp_DDEPOKE_unused = 0;
     bm_DDEPOKE_fRelease = $2000;
     bp_DDEPOKE_fRelease = 13;
     bm_DDEPOKE_fReserved = $C000;
     bp_DDEPOKE_fReserved = 14;
  function unused(var a : DDEPOKE) : word;
  procedure set_unused(var a : DDEPOKE; __unused : word);
  function fRelease(var a : DDEPOKE) : word;
  procedure set_fRelease(var a : DDEPOKE; __fRelease : word);
  function fReserved(var a : DDEPOKE) : word;
  procedure set_fReserved(var a : DDEPOKE; __fReserved : word);

  type

     DDEUP = record
          flag0 : word;
          cfFormat : integer;
          rgb : array[0..0] of BYTE;
       end;
  const
     bm_DDEUP_unused = $FFF;
     bp_DDEUP_unused = 0;
     bm_DDEUP_fAck = $1000;
     bp_DDEUP_fAck = 12;
     bm_DDEUP_fRelease = $2000;
     bp_DDEUP_fRelease = 13;
     bm_DDEUP_fReserved = $4000;
     bp_DDEUP_fReserved = 14;
     bm_DDEUP_fAckReq = $8000;
     bp_DDEUP_fAckReq = 15;
  function unused(var a : DDEUP) : word;
  procedure set_unused(var a : DDEUP; __unused : word);
  function fAck(var a : DDEUP) : word;
  procedure set_fAck(var a : DDEUP; __fAck : word);
  function fRelease(var a : DDEUP) : word;
  procedure set_fRelease(var a : DDEUP; __fRelease : word);
  function fReserved(var a : DDEUP) : word;
  procedure set_fReserved(var a : DDEUP; __fReserved : word);
  function fAckReq(var a : DDEUP) : word;
  procedure set_fAckReq(var a : DDEUP; __fAckReq : word);

  type

     EXCEPTION_RECORD = record
          ExceptionCode : DWORD;
          ExceptionFlags : DWORD;
          ExceptionRecord : ^_EXCEPTION_RECORD;
          ExceptionAddress : PVOID;
          NumberParameters : DWORD;
          ExceptionInformation : array[0..(EXCEPTION_MAXIMUM_PARAMETERS)-1] of DWORD;
       end;

     PEXCEPTION_RECORD = ^EXCEPTION_RECORD;

     LPEXCEPTION_RECORD = ^EXCEPTION_RECORD;

     _EXCEPTION_RECORD = EXCEPTION_RECORD;

     EXCEPTION_DEBUG_INFO = record
          ExceptionRecord : EXCEPTION_RECORD;
          dwFirstChance : DWORD;
       end;

     _EXCEPTION_DEBUG_INFO = EXCEPTION_DEBUG_INFO;

     EXIT_PROCESS_DEBUG_INFO = record
          dwExitCode : DWORD;
       end;

     _EXIT_PROCESS_DEBUG_INFO = EXIT_PROCESS_DEBUG_INFO;

     EXIT_THREAD_DEBUG_INFO = record
          dwExitCode : DWORD;
       end;

     _EXIT_THREAD_DEBUG_INFO = EXIT_THREAD_DEBUG_INFO;

     LOAD_DLL_DEBUG_INFO = record
          hFile : HANDLE;
          lpBaseOfDll : LPVOID;
          dwDebugInfoFileOffset : DWORD;
          nDebugInfoSize : DWORD;
          lpImageName : LPVOID;
          fUnicode : WORD;
       end;

     _LOAD_DLL_DEBUG_INFO = LOAD_DLL_DEBUG_INFO;

     UNLOAD_DLL_DEBUG_INFO = record
          lpBaseOfDll : LPVOID;
       end;

     _UNLOAD_DLL_DEBUG_INFO = UNLOAD_DLL_DEBUG_INFO;

     OUTPUT_DEBUG_STRING_INFO = record
          lpDebugStringData : LPSTR;
          fUnicode : WORD;
          nDebugStringLength : WORD;
       end;

     _OUTPUT_DEBUG_STRING_INFO = OUTPUT_DEBUG_STRING_INFO;

     RIP_INFO = record
          dwError : DWORD;
          dwType : DWORD;
       end;

     _RIP_INFO = RIP_INFO;

     DEBUG_EVENT = record
          dwDebugEventCode : DWORD;
          dwProcessId : DWORD;
          dwThreadId : DWORD;
          u : record
              case longint of
                 0 : ( Exception : EXCEPTION_DEBUG_INFO );
                 1 : ( CreateThread : CREATE_THREAD_DEBUG_INFO );
                 2 : ( CreateProcessInfo : CREATE_PROCESS_DEBUG_INFO );
                 3 : ( ExitThread : EXIT_THREAD_DEBUG_INFO );
                 4 : ( ExitProcess : EXIT_PROCESS_DEBUG_INFO );
                 5 : ( LoadDll : LOAD_DLL_DEBUG_INFO );
                 6 : ( UnloadDll : UNLOAD_DLL_DEBUG_INFO );
                 7 : ( DebugString : OUTPUT_DEBUG_STRING_INFO );
                 8 : ( RipInfo : RIP_INFO );
              end;
       end;

     LPDEBUG_EVENT = ^DEBUG_EVENT;

     _DEBUG_EVENT = DEBUG_EVENT;

     DEBUGHOOKINFO = record
          idThread : DWORD;
          idThreadInstaller : DWORD;
          lParam : LPARAM;
          wParam : WPARAM;
          code : longint;
       end;

     tagDEBUGHOOKINFO = DEBUGHOOKINFO;

     DELETEITEMSTRUCT = record
          CtlType : UINT;
          CtlID : UINT;
          itemID : UINT;
          hwndItem : HWND;
          itemData : UINT;
       end;

     tagDELETEITEMSTRUCT = DELETEITEMSTRUCT;

     DEV_BROADCAST_HDR = record
          dbch_size : ULONG;
          dbch_devicetype : ULONG;
          dbch_reserved : ULONG;
       end;

     _DEV_BROADCAST_HDR = DEV_BROADCAST_HDR;

     PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR;

     DEV_BROADCAST_OEM = record
          dbco_size : ULONG;
          dbco_devicetype : ULONG;
          dbco_reserved : ULONG;
          dbco_identifier : ULONG;
          dbco_suppfunc : ULONG;
       end;

     _DEV_BROADCAST_OEM = DEV_BROADCAST_OEM;

     PDEV_BROADCAST_OEM = ^DEV_BROADCAST_OEM;

     DEV_BROADCAST_PORT = record
          dbcp_size : ULONG;
          dbcp_devicetype : ULONG;
          dbcp_reserved : ULONG;
          dbcp_name : array[0..0] of char;
       end;

     _DEV_BROADCAST_PORT = DEV_BROADCAST_PORT;

     PDEV_BROADCAST_PORT = ^DEV_BROADCAST_PORT;
     _DEV_BROADCAST_USERDEFINED = record
          dbud_dbh : _DEV_BROADCAST_HDR;
          dbud_szName : array[0..0] of char;
          dbud_rgbUserDefined : array[0..0] of BYTE;
       end;


     DEV_BROADCAST_VOLUME = record
          dbcv_size : ULONG;
          dbcv_devicetype : ULONG;
          dbcv_reserved : ULONG;
          dbcv_unitmask : ULONG;
          dbcv_flags : USHORT;
       end;

     _DEV_BROADCAST_VOLUME = DEV_BROADCAST_VOLUME;

     PDEV_BROADCAST_VOLUME = ^DEV_BROADCAST_VOLUME;

     DEVMODE = record
          dmDeviceName : array[0..(CCHDEVICENAME)-1] of BCHAR;
          dmSpecVersion : WORD;
          dmDriverVersion : WORD;
          dmSize : WORD;
          dmDriverExtra : WORD;
          dmFields : DWORD;
          dmOrientation : integer;
          dmPaperSize : integer;
          dmPaperLength : integer;
          dmPaperWidth : integer;
          dmScale : integer;
          dmCopies : integer;
          dmDefaultSource : integer;
          dmPrintQuality : integer;
          dmColor : integer;
          dmDuplex : integer;
          dmYResolution : integer;
          dmTTOption : integer;
          dmCollate : integer;
          dmFormName : array[0..(CCHFORMNAME)-1] of BCHAR;
          dmLogPixels : WORD;
          dmBitsPerPel : DWORD;
          dmPelsWidth : DWORD;
          dmPelsHeight : DWORD;
          dmDisplayFlags : DWORD;
          dmDisplayFrequency : DWORD;
          dmICMMethod : DWORD;
          dmICMIntent : DWORD;
          dmMediaType : DWORD;
          dmDitherType : DWORD;
          dmICCManufacturer : DWORD;
          dmICCModel : DWORD;
       end;

     LPDEVMODE = ^DEVMODE;

     _devicemode = DEVMODE;

     DEVNAMES = record
          wDriverOffset : WORD;
          wDeviceOffset : WORD;
          wOutputOffset : WORD;
          wDefault : WORD;
       end;

     LPDEVNAMES = ^DEVNAMES;

     tagDEVNAMES = DEVNAMES;

     DIBSECTION = record
          dsBm : BITMAP;
          dsBmih : BITMAPINFOHEADER;
          dsBitfields : array[0..2] of DWORD;
          dshSection : HANDLE;
          dsOffset : DWORD;
       end;

     tagDIBSECTION = DIBSECTION;

     LARGE_INTEGER = record
          LowPart : DWORD;
          HighPart : LONG;
       end;

     PLARGE_INTEGER = ^LARGE_INTEGER;

     _LARGE_INTEGER = LARGE_INTEGER;

     DISK_GEOMETRY = record
          Cylinders : LARGE_INTEGER;
          MediaType : MEDIA_TYPE;
          TracksPerCylinder : DWORD;
          SectorsPerTrack : DWORD;
          BytesPerSector : DWORD;
       end;

     _DISK_GEOMETRY = DISK_GEOMETRY;

     DISK_PERFORMANCE = record
          BytesRead : LARGE_INTEGER;
          BytesWritten : LARGE_INTEGER;
          ReadTime : LARGE_INTEGER;
          WriteTime : LARGE_INTEGER;
          ReadCount : DWORD;
          WriteCount : DWORD;
          QueueDepth : DWORD;
       end;

     _DISK_PERFORMANCE = DISK_PERFORMANCE;
{$PACKRECORDS 1}

     DLGITEMTEMPLATE = record
          style : DWORD;
          dwExtendedStyle : DWORD;
          x : integer;
          y : integer;
          cx : integer;
          cy : integer;
          id : WORD;
       end;

     LPDLGITEMTEMPLATE = ^DLGITEMTEMPLATE;

     PDLGITEMTEMPLATE = ^DLGITEMTEMPLATE;

     DLGTEMPLATE = record
          style : DWORD;
          dwExtendedStyle : DWORD;
          cdit : WORD;
          x : integer;
          y : integer;
          cx : integer;
          cy : integer;
       end;

     LPDLGTEMPLATE = ^DLGTEMPLATE;
(* Const before type ignored *)

     LPCDLGTEMPLATE = ^DLGTEMPLATE;
{$PACKRECORDS 4}

     DOC_INFO_1 = record
          pDocName : LPTSTR;
          pOutputFile : LPTSTR;
          pDatatype : LPTSTR;
       end;

     _DOC_INFO_1 = DOC_INFO_1;

     DOC_INFO_2 = record
          pDocName : LPTSTR;
          pOutputFile : LPTSTR;
          pDatatype : LPTSTR;
          dwMode : DWORD;
          JobId : DWORD;
       end;

     _DOC_INFO_2 = DOC_INFO_2;

     DOCINFO = record
          cbSize : longint;
          lpszDocName : LPCTSTR;
          lpszOutput : LPCTSTR;
          lpszDatatype : LPCTSTR;
          fwType : DWORD;
       end;

     DRAGLISTINFO = record
          uNotification : UINT;
          hWnd : HWND;
          ptCursor : POINT;
       end;

     LPDRAGLISTINFO = ^DRAGLISTINFO;

     DRAWITEMSTRUCT = record
          CtlType : UINT;
          CtlID : UINT;
          itemID : UINT;
          itemAction : UINT;
          itemState : UINT;
          hwndItem : HWND;
          hDC : HDC;
          rcItem : RECT;
          itemData : DWORD;
       end;

     LPDRAWITEMSTRUCT = ^DRAWITEMSTRUCT;

     PDRAWITEMSTRUCT = ^DRAWITEMSTRUCT;

     tagDRAWITEMSTRUCT = DRAWITEMSTRUCT;

     DRAWTEXTPARAMS = record
          cbSize : UINT;
          iTabLength : longint;
          iLeftMargin : longint;
          iRightMargin : longint;
          uiLengthDrawn : UINT;
       end;

     LPDRAWTEXTPARAMS = ^DRAWTEXTPARAMS;

     PARTITION_INFORMATION = record
          PartitionType : BYTE;
          BootIndicator : BOOLEAN;
          RecognizedPartition : BOOLEAN;
          RewritePartition : BOOLEAN;
          StartingOffset : LARGE_INTEGER;
          PartitionLength : LARGE_INTEGER;
          HiddenSectors : LARGE_INTEGER;
       end;

     _PARTITION_INFORMATION = PARTITION_INFORMATION;

     DRIVE_LAYOUT_INFORMATION = record
          PartitionCount : DWORD;
          Signature : DWORD;
          PartitionEntry : array[0..0] of PARTITION_INFORMATION;
       end;

     _DRIVE_LAYOUT_INFORMATION = DRIVE_LAYOUT_INFORMATION;

     DRIVER_INFO_1 = record
          pName : LPTSTR;
       end;

     _DRIVER_INFO_1 = DRIVER_INFO_1;

     DRIVER_INFO_2 = record
          cVersion : DWORD;
          pName : LPTSTR;
          pEnvironment : LPTSTR;
          pDriverPath : LPTSTR;
          pDataFile : LPTSTR;
          pConfigFile : LPTSTR;
       end;

     _DRIVER_INFO_2 = DRIVER_INFO_2;

     DRIVER_INFO_3 = record
          cVersion : DWORD;
          pName : LPTSTR;
          pEnvironment : LPTSTR;
          pDriverPath : LPTSTR;
          pDataFile : LPTSTR;
          pConfigFile : LPTSTR;
          pHelpFile : LPTSTR;
          pDependentFiles : LPTSTR;
          pMonitorName : LPTSTR;
          pDefaultDataType : LPTSTR;
       end;

     _DRIVER_INFO_3 = DRIVER_INFO_3;

     EDITSTREAM = record
          dwCookie : DWORD;
          dwError : DWORD;
          pfnCallback : EDITSTREAMCALLBACK;
       end;

     _editstream = EDITSTREAM;

     EMR = record
          iType : DWORD;
          nSize : DWORD;
       end;

     PEMR = ^EMR;

     tagEMR = EMR;

     EMRANGLEARC = record
          emr : EMR;
          ptlCenter : POINTL;
          nRadius : DWORD;
          eStartAngle : FLOAT;
          eSweepAngle : FLOAT;
       end;

     PEMRANGLEARC = ^EMRANGLEARC;

     tagEMRANGLEARC = EMRANGLEARC;

     EMRARC = record
          emr : EMR;
          rclBox : RECTL;
          ptlStart : POINTL;
          ptlEnd : POINTL;
       end;

     PEMRARC = ^EMRARC;

     EMRARCTO = EMRARC;

     PEMRARCTO = ^EMRARC;

     EMRCHORD = EMRARC;

     PEMRCHORD = ^EMRARC;

     EMRPIE = EMRARC;

     PEMRPIE = ^EMRARC;

     tagEMRARC = EMRARC;

     XFORM = record
          eM11 : FLOAT;
          eM12 : FLOAT;
          eM21 : FLOAT;
          eM22 : FLOAT;
          eDx : FLOAT;
          eDy : FLOAT;
       end;

     PXFORM = ^XFORM;

     LPXFORM = ^XFORM;

     _XFORM = XFORM;

     EMRBITBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
       end;

     PEMRBITBLT = ^EMRBITBLT;

     tagEMRBITBLT = EMRBITBLT;

     LOGBRUSH = record
          lbStyle : UINT;
          lbColor : COLORREF;
          lbHatch : LONG;
       end;

     tagLOGBRUSH = LOGBRUSH;

     EMRCREATEBRUSHINDIRECT = record
          emr : EMR;
          ihBrush : DWORD;
          lb : LOGBRUSH;
       end;

     PEMRCREATEBRUSHINDIRECT = ^EMRCREATEBRUSHINDIRECT;

     tagEMRCREATEBRUSHINDIRECT = EMRCREATEBRUSHINDIRECT;

     LCSCSTYPE = LONG;

     LCSGAMUTMATCH = LONG;

     LOGCOLORSPACE = record
          lcsSignature : DWORD;
          lcsVersion : DWORD;
          lcsSize : DWORD;
          lcsCSType : LCSCSTYPE;
          lcsIntent : LCSGAMUTMATCH;
          lcsEndpoints : CIEXYZTRIPLE;
          lcsGammaRed : DWORD;
          lcsGammaGreen : DWORD;
          lcsGammaBlue : DWORD;
          lcsFilename : array[0..(MAX_PATH)-1] of TCHAR;
       end;

     LPLOGCOLORSPACE = ^LOGCOLORSPACE;

     tagLOGCOLORSPACE = LOGCOLORSPACE;

     EMRCREATECOLORSPACE = record
          emr : EMR;
          ihCS : DWORD;
          lcs : LOGCOLORSPACE;
       end;

     PEMRCREATECOLORSPACE = ^EMRCREATECOLORSPACE;

     tagEMRCREATECOLORSPACE = EMRCREATECOLORSPACE;

     EMRCREATEDIBPATTERNBRUSHPT = record
          emr : EMR;
          ihBrush : DWORD;
          iUsage : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
       end;

     PEMRCREATEDIBPATTERNBRUSHPT = EMRCREATEDIBPATTERNBRUSHPT;

     tagEMRCREATEDIBPATTERNBRUSHPT = EMRCREATEDIBPATTERNBRUSHPT;

     EMRCREATEMONOBRUSH = record
          emr : EMR;
          ihBrush : DWORD;
          iUsage : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
       end;

     PEMRCREATEMONOBRUSH = ^EMRCREATEMONOBRUSH;

     tagEMRCREATEMONOBRUSH = EMRCREATEMONOBRUSH;

     PALETTEENTRY = record
          peRed : BYTE;
          peGreen : BYTE;
          peBlue : BYTE;
          peFlags : BYTE;
       end;

     LPPALETTEENTRY = ^PALETTEENTRY;

     PPALETTEENTRY = ^PALETTEENTRY;

     tagPALETTEENTRY = PALETTEENTRY;

     LOGPALETTE = record
          palVersion : WORD;
          palNumEntries : WORD;
          palPalEntry : array[0..0] of PALETTEENTRY;
       end;

     LPLOGPALETTE = ^LOGPALETTE;

     PLOGPALETTE = ^LOGPALETTE;

     tagLOGPALETTE = LOGPALETTE;

     EMRCREATEPALETTE = record
          emr : EMR;
          ihPal : DWORD;
          lgpl : LOGPALETTE;
       end;

     PEMRCREATEPALETTE = ^EMRCREATEPALETTE;

     tagEMRCREATEPALETTE = EMRCREATEPALETTE;

     LOGPEN = record
          lopnStyle : UINT;
          lopnWidth : POINT;
          lopnColor : COLORREF;
       end;

     tagLOGPEN = LOGPEN;

     EMRCREATEPEN = record
          emr : EMR;
          ihPen : DWORD;
          lopn : LOGPEN;
       end;

     PEMRCREATEPEN = ^EMRCREATEPEN;

     tagEMRCREATEPEN = EMRCREATEPEN;

     EMRELLIPSE = record
          emr : EMR;
          rclBox : RECTL;
       end;

     PEMRELLIPSE = ^EMRELLIPSE;

     EMRRECTANGLE = EMRELLIPSE;

     PEMRRECTANGLE = ^EMRELLIPSE;

     tagEMRELLIPSE = EMRELLIPSE;

     EMREOF = record
          emr : EMR;
          nPalEntries : DWORD;
          offPalEntries : DWORD;
          nSizeLast : DWORD;
       end;

     PEMREOF = ^EMREOF;

     tagEMREOF = EMREOF;

     EMREXCLUDECLIPRECT = record
          emr : EMR;
          rclClip : RECTL;
       end;

     PEMREXCLUDECLIPRECT = ^EMREXCLUDECLIPRECT;

     EMRINTERSECTCLIPRECT = EMREXCLUDECLIPRECT;

     PEMRINTERSECTCLIPRECT = ^EMREXCLUDECLIPRECT;

     tagEMREXCLUDECLIPRECT = EMREXCLUDECLIPRECT;

     PANOSE = record
          bFamilyType : BYTE;
          bSerifStyle : BYTE;
          bWeight : BYTE;
          bProportion : BYTE;
          bContrast : BYTE;
          bStrokeVariation : BYTE;
          bArmStyle : BYTE;
          bLetterform : BYTE;
          bMidline : BYTE;
          bXHeight : BYTE;
       end;

     tagPANOSE = PANOSE;

     EXTLOGFONT = record
          elfLogFont : LOGFONT;
          elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
          elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
          elfVersion : DWORD;
          elfStyleSize : DWORD;
          elfMatch : DWORD;
          elfReserved : DWORD;
          elfVendorId : array[0..(ELF_VENDOR_SIZE)-1] of BYTE;
          elfCulture : DWORD;
          elfPanose : PANOSE;
       end;

     tagEXTLOGFONT = EXTLOGFONT;

     EMREXTCREATEFONTINDIRECTW = record
          emr : EMR;
          ihFont : DWORD;
          elfw : EXTLOGFONT;
       end;

     PEMREXTCREATEFONTINDIRECTW = EMREXTCREATEFONTINDIRECTW;

     tagEMREXTCREATEFONTINDIRECTW = EMREXTCREATEFONTINDIRECTW;

     EXTLOGPEN = record
          elpPenStyle : UINT;
          elpWidth : UINT;
          elpBrushStyle : UINT;
          elpColor : COLORREF;
          elpHatch : LONG;
          elpNumEntries : DWORD;
          elpStyleEntry : array[0..0] of DWORD;
       end;

     tagEXTLOGPEN = EXTLOGPEN;

     EMREXTCREATEPEN = record
          emr : EMR;
          ihPen : DWORD;
          offBmi : DWORD;
          cbBmi : DWORD;
          offBits : DWORD;
          cbBits : DWORD;
          elp : EXTLOGPEN;
       end;

     PEMREXTCREATEPEN = ^EMREXTCREATEPEN;

     tagEMREXTCREATEPEN = EMREXTCREATEPEN;

     EMREXTFLOODFILL = record
          emr : EMR;
          ptlStart : POINTL;
          crColor : COLORREF;
          iMode : DWORD;
       end;

     PEMREXTFLOODFILL = ^EMREXTFLOODFILL;

     tagEMREXTFLOODFILL = EMREXTFLOODFILL;

     EMREXTSELECTCLIPRGN = record
          emr : EMR;
          cbRgnData : DWORD;
          iMode : DWORD;
          RgnData : array[0..0] of BYTE;
       end;

     PEMREXTSELECTCLIPRGN = ^EMREXTSELECTCLIPRGN;

     tagEMREXTSELECTCLIPRGN = EMREXTSELECTCLIPRGN;

     EMRTEXT = record
          ptlReference : POINTL;
          nChars : DWORD;
          offString : DWORD;
          fOptions : DWORD;
          rcl : RECTL;
          offDx : DWORD;
       end;

     PEMRTEXT = ^EMRTEXT;

     tagEMRTEXT = EMRTEXT;

     EMREXTTEXTOUTA = record
          emr : EMR;
          rclBounds : RECTL;
          iGraphicsMode : DWORD;
          exScale : FLOAT;
          eyScale : FLOAT;
          emrtext : EMRTEXT;
       end;

     PEMREXTTEXTOUTA = ^EMREXTTEXTOUTA;

     EMREXTTEXTOUTW = EMREXTTEXTOUTA;

     PEMREXTTEXTOUTW = ^EMREXTTEXTOUTA;

     tagEMREXTTEXTOUTA = EMREXTTEXTOUTA;

     EMRFILLPATH = record
          emr : EMR;
          rclBounds : RECTL;
       end;

     PEMRFILLPATH = ^EMRFILLPATH;

     EMRSTROKEANDFILLPATH = EMRFILLPATH;

     PEMRSTROKEANDFILLPATH = ^EMRFILLPATH;

     EMRSTROKEPATH = EMRFILLPATH;

     PEMRSTROKEPATH = ^EMRFILLPATH;

     tagEMRFILLPATH = EMRFILLPATH;

     EMRFILLRGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          ihBrush : DWORD;
          RgnData : array[0..0] of BYTE;
       end;

     PEMRFILLRGN = ^EMRFILLRGN;

     tagEMRFILLRGN = EMRFILLRGN;

     EMRFORMAT = record
          dSignature : DWORD;
          nVersion : DWORD;
          cbData : DWORD;
          offData : DWORD;
       end;

     tagEMRFORMAT = EMRFORMAT;

     SIZE = record
          cx : LONG;
          cy : LONG;
       end;

     PSIZE = ^SIZE;

     LPSIZE = ^SIZE;

     SIZEL = SIZE;

     PSIZEL = ^SIZE;

     LPSIZEL = ^SIZE;

     tagSIZE = SIZE;

     EMRFRAMERGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          ihBrush : DWORD;
          szlStroke : SIZEL;
          RgnData : array[0..0] of BYTE;
       end;

     PEMRFRAMERGN = ^EMRFRAMERGN;

     tagEMRFRAMERGN = EMRFRAMERGN;

     EMRGDICOMMENT = record
          emr : EMR;
          cbData : DWORD;
          Data : array[0..0] of BYTE;
       end;

     PEMRGDICOMMENT = ^EMRGDICOMMENT;

     tagEMRGDICOMMENT = EMRGDICOMMENT;

     EMRINVERTRGN = record
          emr : EMR;
          rclBounds : RECTL;
          cbRgnData : DWORD;
          RgnData : array[0..0] of BYTE;
       end;

     PEMRINVERTRGN = ^EMRINVERTRGN;

     EMRPAINTRGN = EMRINVERTRGN;

     PEMRPAINTRGN = ^EMRINVERTRGN;

     tagEMRINVERTRGN = EMRINVERTRGN;

     EMRLINETO = record
          emr : EMR;
          ptl : POINTL;
       end;

     PEMRLINETO = ^EMRLINETO;

     EMRMOVETOEX = EMRLINETO;

     PEMRMOVETOEX = ^EMRLINETO;

     tagEMRLINETO = EMRLINETO;

     EMRMASKBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          xMask : LONG;
          yMask : LONG;
          iUsageMask : DWORD;
          offBmiMask : DWORD;
          cbBmiMask : DWORD;
          offBitsMask : DWORD;
          cbBitsMask : DWORD;
       end;

     PEMRMASKBLT = ^EMRMASKBLT;

     tagEMRMASKBLT = EMRMASKBLT;

     EMRMODIFYWORLDTRANSFORM = record
          emr : EMR;
          xform : XFORM;
          iMode : DWORD;
       end;

     PEMRMODIFYWORLDTRANSFORM = EMRMODIFYWORLDTRANSFORM;

     tagEMRMODIFYWORLDTRANSFORM = EMRMODIFYWORLDTRANSFORM;

     EMROFFSETCLIPRGN = record
          emr : EMR;
          ptlOffset : POINTL;
       end;

     PEMROFFSETCLIPRGN = ^EMROFFSETCLIPRGN;

     tagEMROFFSETCLIPRGN = EMROFFSETCLIPRGN;

     EMRPLGBLT = record
          emr : EMR;
          rclBounds : RECTL;
          aptlDest : array[0..2] of POINTL;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          xMask : LONG;
          yMask : LONG;
          iUsageMask : DWORD;
          offBmiMask : DWORD;
          cbBmiMask : DWORD;
          offBitsMask : DWORD;
          cbBitsMask : DWORD;
       end;

     PEMRPLGBLT = ^EMRPLGBLT;

     tagEMRPLGBLT = EMRPLGBLT;

     EMRPOLYDRAW = record
          emr : EMR;
          rclBounds : RECTL;
          cptl : DWORD;
          aptl : array[0..0] of POINTL;
          abTypes : array[0..0] of BYTE;
       end;

     PEMRPOLYDRAW = ^EMRPOLYDRAW;

     tagEMRPOLYDRAW = EMRPOLYDRAW;

     EMRPOLYDRAW16 = record
          emr : EMR;
          rclBounds : RECTL;
          cpts : DWORD;
          apts : array[0..0] of POINTS;
          abTypes : array[0..0] of BYTE;
       end;

     PEMRPOLYDRAW16 = ^EMRPOLYDRAW16;

     tagEMRPOLYDRAW16 = EMRPOLYDRAW16;

     EMRPOLYLINE = record
          emr : EMR;
          rclBounds : RECTL;
          cptl : DWORD;
          aptl : array[0..0] of POINTL;
       end;

     PEMRPOLYLINE = ^EMRPOLYLINE;

     EMRPOLYBEZIER = EMRPOLYLINE;

     PEMRPOLYBEZIER = ^EMRPOLYLINE;

     EMRPOLYGON = EMRPOLYLINE;

     PEMRPOLYGON = ^EMRPOLYLINE;

     EMRPOLYBEZIERTO = EMRPOLYLINE;

     PEMRPOLYBEZIERTO = ^EMRPOLYLINE;

     EMRPOLYLINETO = EMRPOLYLINE;

     PEMRPOLYLINETO = ^EMRPOLYLINE;

     tagEMRPOLYLINE = EMRPOLYLINE;

     EMRPOLYLINE16 = record
          emr : EMR;
          rclBounds : RECTL;
          cpts : DWORD;
          apts : array[0..0] of POINTL;
       end;

     PEMRPOLYLINE16 = ^EMRPOLYLINE16;

     EMRPOLYBEZIER16 = EMRPOLYLINE16;

     PEMRPOLYBEZIER16 = ^EMRPOLYLINE16;

     EMRPOLYGON16 = EMRPOLYLINE16;

     PEMRPOLYGON16 = ^EMRPOLYLINE16;

     EMRPOLYBEZIERTO16 = EMRPOLYLINE16;

     PEMRPOLYBEZIERTO16 = ^EMRPOLYLINE16;

     EMRPOLYLINETO16 = EMRPOLYLINE16;

     PEMRPOLYLINETO16 = ^EMRPOLYLINE16;

     tagEMRPOLYLINE16 = EMRPOLYLINE16;

     EMRPOLYPOLYLINE = record
          emr : EMR;
          rclBounds : RECTL;
          nPolys : DWORD;
          cptl : DWORD;
          aPolyCounts : array[0..0] of DWORD;
          aptl : array[0..0] of POINTL;
       end;

     PEMRPOLYPOLYLINE = ^EMRPOLYPOLYLINE;

     EMRPOLYPOLYGON = EMRPOLYPOLYLINE;

     PEMRPOLYPOLYGON = ^EMRPOLYPOLYLINE;

     tagEMRPOLYPOLYLINE = EMRPOLYPOLYLINE;

     EMRPOLYPOLYLINE16 = record
          emr : EMR;
          rclBounds : RECTL;
          nPolys : DWORD;
          cpts : DWORD;
          aPolyCounts : array[0..0] of DWORD;
          apts : array[0..0] of POINTS;
       end;

     PEMRPOLYPOLYLINE16 = ^EMRPOLYPOLYLINE16;

     EMRPOLYPOLYGON16 = EMRPOLYPOLYLINE16;

     PEMRPOLYPOLYGON16 = ^EMRPOLYPOLYLINE16;

     tagEMRPOLYPOLYLINE16 = EMRPOLYPOLYLINE16;

     EMRPOLYTEXTOUTA = record
          emr : EMR;
          rclBounds : RECTL;
          iGraphicsMode : DWORD;
          exScale : FLOAT;
          eyScale : FLOAT;
          cStrings : LONG;
          aemrtext : array[0..0] of EMRTEXT;
       end;

     PEMRPOLYTEXTOUTA = ^EMRPOLYTEXTOUTA;

     EMRPOLYTEXTOUTW = EMRPOLYTEXTOUTA;

     PEMRPOLYTEXTOUTW = ^EMRPOLYTEXTOUTA;

     tagEMRPOLYTEXTOUTA = EMRPOLYTEXTOUTA;

     EMRRESIZEPALETTE = record
          emr : EMR;
          ihPal : DWORD;
          cEntries : DWORD;
       end;

     PEMRRESIZEPALETTE = ^EMRRESIZEPALETTE;

     tagEMRRESIZEPALETTE = EMRRESIZEPALETTE;

     EMRRESTOREDC = record
          emr : EMR;
          iRelative : LONG;
       end;

     PEMRRESTOREDC = ^EMRRESTOREDC;

     tagEMRRESTOREDC = EMRRESTOREDC;

     EMRROUNDRECT = record
          emr : EMR;
          rclBox : RECTL;
          szlCorner : SIZEL;
       end;

     PEMRROUNDRECT = ^EMRROUNDRECT;

     tagEMRROUNDRECT = EMRROUNDRECT;

     EMRSCALEVIEWPORTEXTEX = record
          emr : EMR;
          xNum : LONG;
          xDenom : LONG;
          yNum : LONG;
          yDenom : LONG;
       end;

     PEMRSCALEVIEWPORTEXTEX = ^EMRSCALEVIEWPORTEXTEX;

     EMRSCALEWINDOWEXTEX = EMRSCALEVIEWPORTEXTEX;

     PEMRSCALEWINDOWEXTEX = ^EMRSCALEVIEWPORTEXTEX;

     tagEMRSCALEVIEWPORTEXTEX = EMRSCALEVIEWPORTEXTEX;

     EMRSELECTCOLORSPACE = record
          emr : EMR;
          ihCS : DWORD;
       end;

     PEMRSELECTCOLORSPACE = ^EMRSELECTCOLORSPACE;

     EMRDELETECOLORSPACE = EMRSELECTCOLORSPACE;

     PEMRDELETECOLORSPACE = ^EMRSELECTCOLORSPACE;

     tagEMRSELECTCOLORSPACE = EMRSELECTCOLORSPACE;

     EMRSELECTOBJECT = record
          emr : EMR;
          ihObject : DWORD;
       end;

     PEMRSELECTOBJECT = ^EMRSELECTOBJECT;

     EMRDELETEOBJECT = EMRSELECTOBJECT;

     PEMRDELETEOBJECT = ^EMRSELECTOBJECT;

     tagEMRSELECTOBJECT = EMRSELECTOBJECT;

     EMRSELECTPALETTE = record
          emr : EMR;
          ihPal : DWORD;
       end;

     PEMRSELECTPALETTE = ^EMRSELECTPALETTE;

     tagEMRSELECTPALETTE = EMRSELECTPALETTE;

     EMRSETARCDIRECTION = record
          emr : EMR;
          iArcDirection : DWORD;
       end;

     PEMRSETARCDIRECTION = ^EMRSETARCDIRECTION;

     tagEMRSETARCDIRECTION = EMRSETARCDIRECTION;

     EMRSETBKCOLOR = record
          emr : EMR;
          crColor : COLORREF;
       end;

     PEMRSETBKCOLOR = ^EMRSETBKCOLOR;

     EMRSETTEXTCOLOR = EMRSETBKCOLOR;

     PEMRSETTEXTCOLOR = ^EMRSETBKCOLOR;

     tagEMRSETTEXTCOLOR = EMRSETBKCOLOR;

     EMRSETCOLORADJUSTMENT = record
          emr : EMR;
          ColorAdjustment : COLORADJUSTMENT;
       end;

     PEMRSETCOLORADJUSTMENT = ^EMRSETCOLORADJUSTMENT;

     tagEMRSETCOLORADJUSTMENT = EMRSETCOLORADJUSTMENT;

     EMRSETDIBITSTODEVICE = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          iUsageSrc : DWORD;
          iStartScan : DWORD;
          cScans : DWORD;
       end;

     PEMRSETDIBITSTODEVICE = ^EMRSETDIBITSTODEVICE;

     tagEMRSETDIBITSTODEVICE = EMRSETDIBITSTODEVICE;

     EMRSETMAPPERFLAGS = record
          emr : EMR;
          dwFlags : DWORD;
       end;

     PEMRSETMAPPERFLAGS = ^EMRSETMAPPERFLAGS;

     tagEMRSETMAPPERFLAGS = EMRSETMAPPERFLAGS;

     EMRSETMITERLIMIT = record
          emr : EMR;
          eMiterLimit : FLOAT;
       end;

     PEMRSETMITERLIMIT = ^EMRSETMITERLIMIT;

     tagEMRSETMITERLIMIT = EMRSETMITERLIMIT;

     EMRSETPALETTEENTRIES = record
          emr : EMR;
          ihPal : DWORD;
          iStart : DWORD;
          cEntries : DWORD;
          aPalEntries : array[0..0] of PALETTEENTRY;
       end;

     PEMRSETPALETTEENTRIES = ^EMRSETPALETTEENTRIES;

     tagEMRSETPALETTEENTRIES = EMRSETPALETTEENTRIES;

     EMRSETPIXELV = record
          emr : EMR;
          ptlPixel : POINTL;
          crColor : COLORREF;
       end;

     PEMRSETPIXELV = ^EMRSETPIXELV;

     tagEMRSETPIXELV = EMRSETPIXELV;

     EMRSETVIEWPORTEXTEX = record
          emr : EMR;
          szlExtent : SIZEL;
       end;

     PEMRSETVIEWPORTEXTEX = ^EMRSETVIEWPORTEXTEX;

     EMRSETWINDOWEXTEX = EMRSETVIEWPORTEXTEX;

     PEMRSETWINDOWEXTEX = ^EMRSETVIEWPORTEXTEX;

     tagEMRSETVIEWPORTEXTEX = EMRSETVIEWPORTEXTEX;

     EMRSETVIEWPORTORGEX = record
          emr : EMR;
          ptlOrigin : POINTL;
       end;

     PEMRSETVIEWPORTORGEX = ^EMRSETVIEWPORTORGEX;

     EMRSETWINDOWORGEX = EMRSETVIEWPORTORGEX;

     PEMRSETWINDOWORGEX = ^EMRSETVIEWPORTORGEX;

     EMRSETBRUSHORGEX = EMRSETVIEWPORTORGEX;

     PEMRSETBRUSHORGEX = ^EMRSETVIEWPORTORGEX;

     tagEMRSETVIEWPORTORGEX = EMRSETVIEWPORTORGEX;

     EMRSETWORLDTRANSFORM = record
          emr : EMR;
          xform : XFORM;
       end;

     PEMRSETWORLDTRANSFORM = ^EMRSETWORLDTRANSFORM;

     tagEMRSETWORLDTRANSFORM = EMRSETWORLDTRANSFORM;

     EMRSTRETCHBLT = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          cxDest : LONG;
          cyDest : LONG;
          dwRop : DWORD;
          xSrc : LONG;
          ySrc : LONG;
          xformSrc : XFORM;
          crBkColorSrc : COLORREF;
          iUsageSrc : DWORD;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          cxSrc : LONG;
          cySrc : LONG;
       end;

     PEMRSTRETCHBLT = ^EMRSTRETCHBLT;

     tagEMRSTRETCHBLT = EMRSTRETCHBLT;

     EMRSTRETCHDIBITS = record
          emr : EMR;
          rclBounds : RECTL;
          xDest : LONG;
          yDest : LONG;
          xSrc : LONG;
          ySrc : LONG;
          cxSrc : LONG;
          cySrc : LONG;
          offBmiSrc : DWORD;
          cbBmiSrc : DWORD;
          offBitsSrc : DWORD;
          cbBitsSrc : DWORD;
          iUsageSrc : DWORD;
          dwRop : DWORD;
          cxDest : LONG;
          cyDest : LONG;
       end;

     PEMRSTRETCHDIBITS = ^EMRSTRETCHDIBITS;

     tagEMRSTRETCHDIBITS = EMRSTRETCHDIBITS;

     EMRABORTPATH = record
          emr : EMR;
       end;

     PEMRABORTPATH = ^EMRABORTPATH;

     EMRBEGINPATH = EMRABORTPATH;

     PEMRBEGINPATH = ^EMRABORTPATH;

     EMRENDPATH = EMRABORTPATH;

     PEMRENDPATH = ^EMRABORTPATH;

     EMRCLOSEFIGURE = EMRABORTPATH;

     PEMRCLOSEFIGURE = ^EMRABORTPATH;

     EMRFLATTENPATH = EMRABORTPATH;

     PEMRFLATTENPATH = ^EMRABORTPATH;

     EMRWIDENPATH = EMRABORTPATH;

     PEMRWIDENPATH = ^EMRABORTPATH;

     EMRSETMETARGN = EMRABORTPATH;

     PEMRSETMETARGN = ^EMRABORTPATH;

     EMRSAVEDC = EMRABORTPATH;

     PEMRSAVEDC = ^EMRABORTPATH;

     EMRREALIZEPALETTE = EMRABORTPATH;

     PEMRREALIZEPALETTE = ^EMRABORTPATH;

     tagABORTPATH = EMRABORTPATH;

     EMRSELECTCLIPPATH = record
          emr : EMR;
          iMode : DWORD;
       end;

     PEMRSELECTCLIPPATH = ^EMRSELECTCLIPPATH;

     EMRSETBKMODE = EMRSELECTCLIPPATH;

     PEMRSETBKMODE = ^EMRSELECTCLIPPATH;

     EMRSETMAPMODE = EMRSELECTCLIPPATH;

     PEMRSETMAPMODE = ^EMRSELECTCLIPPATH;

     EMRSETPOLYFILLMODE = EMRSELECTCLIPPATH;

     PEMRSETPOLYFILLMODE = ^EMRSELECTCLIPPATH;

     EMRSETROP2 = EMRSELECTCLIPPATH;

     PEMRSETROP2 = ^EMRSELECTCLIPPATH;

     EMRSETSTRETCHBLTMODE = EMRSELECTCLIPPATH;

     PEMRSETSTRETCHBLTMODE = ^EMRSELECTCLIPPATH;

     EMRSETTEXTALIGN = EMRSELECTCLIPPATH;

     PEMRSETTEXTALIGN = ^EMRSELECTCLIPPATH;

     EMRENABLEICM = EMRSELECTCLIPPATH;

     PEMRENABLEICM = ^EMRSELECTCLIPPATH;

     tagEMRSELECTCLIPPATH = EMRSELECTCLIPPATH;

     NMHDR = record
          hwndFrom : HWND;
          idFrom : UINT;
          code : UINT;
       end;

     tagNMHDR = NMHDR;

     ENCORRECTTEXT = record
          nmhdr : NMHDR;
          chrg : CHARRANGE;
          seltyp : WORD;
       end;

     _encorrecttext = ENCORRECTTEXT;

     ENDROPFILES = record
          nmhdr : NMHDR;
          hDrop : HANDLE;
          cp : LONG;
          fProtected : WINBOOL;
       end;

     _endropfiles = ENDROPFILES;

     ENSAVECLIPBOARD = record
          nmhdr : NMHDR;
          cObjectCount : LONG;
          cch : LONG;
       end;

     ENOLEOPFAILED = record
          nmhdr : NMHDR;
          iob : LONG;
          lOper : LONG;
          hr : HRESULT;
       end;

     ENHMETAHEADER = record
          iType : DWORD;
          nSize : DWORD;
          rclBounds : RECTL;
          rclFrame : RECTL;
          dSignature : DWORD;
          nVersion : DWORD;
          nBytes : DWORD;
          nRecords : DWORD;
          nHandles : WORD;
          sReserved : WORD;
          nDescription : DWORD;
          offDescription : DWORD;
          nPalEntries : DWORD;
          szlDevice : SIZEL;
          szlMillimeters : SIZEL;
       end;

     LPENHMETAHEADER = ^ENHMETAHEADER;

     tagENHMETAHEADER = ENHMETAHEADER;

     ENHMETARECORD = record
          iType : DWORD;
          nSize : DWORD;
          dParm : array[0..0] of DWORD;
       end;

     PENHMETARECORD = ^ENHMETARECORD;

     LPENHMETARECORD = ^ENHMETARECORD;

     tagENHMETARECORD = ENHMETARECORD;

     ENPROTECTED = record
          nmhdr : NMHDR;
          msg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
          chrg : CHARRANGE;
       end;

     _enprotected = ENPROTECTED;

     SERVICE_STATUS = record
          dwServiceType : DWORD;
          dwCurrentState : DWORD;
          dwControlsAccepted : DWORD;
          dwWin32ExitCode : DWORD;
          dwServiceSpecificExitCode : DWORD;
          dwCheckPoint : DWORD;
          dwWaitHint : DWORD;
       end;

     LPSERVICE_STATUS = ^SERVICE_STATUS;

     _SERVICE_STATUS = SERVICE_STATUS;

     ENUM_SERVICE_STATUS = record
          lpServiceName : LPTSTR;
          lpDisplayName : LPTSTR;
          ServiceStatus : SERVICE_STATUS;
       end;

     LPENUM_SERVICE_STATUS = ^ENUM_SERVICE_STATUS;

     _ENUM_SERVICE_STATUS = ENUM_SERVICE_STATUS;

     ENUMLOGFONT = record
          elfLogFont : LOGFONT;
          elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
          elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
       end;

     tagENUMLOGFONT = ENUMLOGFONT;

     ENUMLOGFONTEX = record
          elfLogFont : LOGFONT;
          elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
          elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
          elfScript : array[0..(LF_FACESIZE)-1] of BCHAR;
       end;

     tagENUMLOGFONTEX = ENUMLOGFONTEX;
  {
    Then follow:

    TCHAR SourceName[]
    TCHAR Computername[]
    SID   UserSid
    TCHAR Strings[]
    BYTE  Data[]
    CHAR  Pad[]
    DWORD Length;
   }

     EVENTLOGRECORD = record
          Length : DWORD;
          Reserved : DWORD;
          RecordNumber : DWORD;
          TimeGenerated : DWORD;
          TimeWritten : DWORD;
          EventID : DWORD;
          EventType : WORD;
          NumStrings : WORD;
          EventCategory : WORD;
          ReservedFlags : WORD;
          ClosingRecordNumber : DWORD;
          StringOffset : DWORD;
          UserSidLength : DWORD;
          UserSidOffset : DWORD;
          DataLength : DWORD;
          DataOffset : DWORD;
       end;

     _EVENTLOGRECORD = EVENTLOGRECORD;

     EVENTMSG = record
          message : UINT;
          paramL : UINT;
          paramH : UINT;
          time : DWORD;
          hwnd : HWND;
       end;

     tagEVENTMSG = EVENTMSG;

     EXCEPTION_POINTERS = record
          ExceptionRecord : PEXCEPTION_RECORD;
          ContextRecord : PCONTEXT;
       end;

     PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;

     LPEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;

     _EXCEPTION_POINTERS = EXCEPTION_POINTERS;

     EXT_BUTTON = record
          idCommand : WORD;
          idsHelp : WORD;
          fsStyle : WORD;
       end;

     LPEXT_BUTTON = ^EXT_BUTTON;

     _EXT_BUTTON = EXT_BUTTON;

     FILTERKEYS = record
          cbSize : UINT;
          dwFlags : DWORD;
          iWaitMSec : DWORD;
          iDelayMSec : DWORD;
          iRepeatMSec : DWORD;
          iBounceMSec : DWORD;
       end;

     tagFILTERKEYS = FILTERKEYS;

     FIND_NAME_BUFFER = record
          length : UCHAR;
          access_control : UCHAR;
          frame_control : UCHAR;
          destination_addr : array[0..5] of UCHAR;
          source_addr : array[0..5] of UCHAR;
          routing_info : array[0..17] of UCHAR;
       end;

     _FIND_NAME_BUFFER = FIND_NAME_BUFFER;

     FIND_NAME_HEADER = record
          node_count : WORD;
          reserved : UCHAR;
          unique_group : UCHAR;
       end;

     _FIND_NAME_HEADER = FIND_NAME_HEADER;

     FINDREPLACE = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hInstance : HINST;
          Flags : DWORD;
          lpstrFindWhat : LPTSTR;
          lpstrReplaceWith : LPTSTR;
          wFindWhatLen : WORD;
          wReplaceWithLen : WORD;
          lCustData : LPARAM;
          lpfnHook : LPFRHOOKPROC;
          lpTemplateName : LPCTSTR;
       end;

     LPFINDREPLACE = ^FINDREPLACE;

     {FINDTEXT = record conflicts with FindText function }
     TFINDTEXT = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
       end;

     _findtext = TFINDTEXT;

     FINDTEXTEX = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
          chrgText : CHARRANGE;
       end;

     _findtextex = FINDTEXTEX;

     FMS_GETDRIVEINFO = record
          dwTotalSpace : DWORD;
          dwFreeSpace : DWORD;
          szPath : array[0..259] of TCHAR;
          szVolume : array[0..13] of TCHAR;
          szShare : array[0..127] of TCHAR;
       end;

     _FMS_GETDRIVEINFO = FMS_GETDRIVEINFO;

     FMS_GETFILESEL = record
          ftTime : FILETIME;
          dwSize : DWORD;
          bAttr : BYTE;
          szName : array[0..259] of TCHAR;
       end;

     _FMS_GETFILESEL = FMS_GETFILESEL;

     FMS_LOAD = record
          dwSize : DWORD;
          szMenuName : array[0..(MENU_TEXT_LEN)-1] of TCHAR;
          hMenu : HMENU;
          wMenuDelta : UINT;
       end;

     _FMS_LOAD = FMS_LOAD;

     FMS_TOOLBARLOAD = record
          dwSize : DWORD;
          lpButtons : LPEXT_BUTTON;
          cButtons : WORD;
          cBitmaps : WORD;
          idBitmap : WORD;
          hBitmap : HBITMAP;
       end;

     _FMS_TOOLBARLOAD = FMS_TOOLBARLOAD;

     FOCUS_EVENT_RECORD = record
          bSetFocus : WINBOOL;
       end;

     _FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD;

     FORM_INFO_1 = record
          Flags : DWORD;
          pName : LPTSTR;
          Size : SIZEL;
          ImageableArea : RECTL;
       end;

     _FORM_INFO_1 = FORM_INFO_1;

     FORMAT_PARAMETERS = record
          MediaType : MEDIA_TYPE;
          StartCylinderNumber : DWORD;
          EndCylinderNumber : DWORD;
          StartHeadNumber : DWORD;
          EndHeadNumber : DWORD;
       end;

     _FORMAT_PARAMETERS = FORMAT_PARAMETERS;

     FORMATRANGE = record
          _hdc : HDC;
          hdcTarget : HDC;
          rc : RECT;
          rcPage : RECT;
          chrg : CHARRANGE;
       end;

     _formatrange = FORMATRANGE;

     GCP_RESULTS = record
          lStructSize : DWORD;
          lpOutString : LPTSTR;
          lpOrder : ^UINT;
          lpDx : ^INT;
          lpCaretPos : ^INT;
          lpClass : LPTSTR;
          lpGlyphs : ^UINT;
          nGlyphs : UINT;
          nMaxFit : UINT;
       end;

     LPGCP_RESULTS = ^GCP_RESULTS;

     tagGCP_RESULTS = GCP_RESULTS;

     GENERIC_MAPPING = record
          GenericRead : ACCESS_MASK;
          GenericWrite : ACCESS_MASK;
          GenericExecute : ACCESS_MASK;
          GenericAll : ACCESS_MASK;
       end;

     PGENERIC_MAPPING = ^GENERIC_MAPPING;

     _GENERIC_MAPPING = GENERIC_MAPPING;

     GLYPHMETRICS = record
          gmBlackBoxX : UINT;
          gmBlackBoxY : UINT;
          gmptGlyphOrigin : POINT;
          gmCellIncX : integer;
          gmCellIncY : integer;
       end;

     LPGLYPHMETRICS = ^GLYPHMETRICS;

     _GLYPHMETRICS = GLYPHMETRICS;

     HANDLETABLE = record
          objectHandle : array[0..0] of HGDIOBJ;
       end;

     LPHANDLETABLE = ^HANDLETABLE;

     tagHANDLETABLE = HANDLETABLE;

     HD_HITTESTINFO = record
          pt : POINT;
          flags : UINT;
          iItem : longint;
       end;

     _HD_HITTESTINFO = HD_HITTESTINFO;

     HD_ITEM = record
          mask : UINT;
          cxy : longint;
          pszText : LPTSTR;
          hbm : HBITMAP;
          cchTextMax : longint;
          fmt : longint;
          lParam : LPARAM;
       end;

     _HD_ITEM = HD_ITEM;

     WINDOWPOS = record
          _hwnd : HWND;
          hwndInsertAfter : HWND;
          x : longint;
          y : longint;
          cx : longint;
          cy : longint;
          flags : UINT;
       end;

     PWINDOWPOS = ^WINDOWPOS;

     LPWINDOWPOS = ^WINDOWPOS;

     _WINDOWPOS = WINDOWPOS;

     HD_LAYOUT = record
          prc : ^RECT;
          pwpos : ^WINDOWPOS;
       end;

     _HD_LAYOUT = HD_LAYOUT;

     HD_NOTIFY = record
          hdr : NMHDR;
          iItem : longint;
          iButton : longint;
          pitem : ^HD_ITEM;
       end;

     _HD_NOTIFY = HD_NOTIFY;

     HELPINFO = record
          cbSize : UINT;
          iContextType : longint;
          iCtrlId : longint;
          hItemHandle : HANDLE;
          dwContextId : DWORD;
          MousePos : POINT;
       end;

     LPHELPINFO = ^HELPINFO;

     tagHELPINFO = HELPINFO;

     HELPWININFO = record
          wStructSize : longint;
          x : longint;
          y : longint;
          dx : longint;
          dy : longint;
          wMax : longint;
          rgchMember : array[0..1] of TCHAR;
       end;

     HIGHCONTRAST = record
          cbSize : UINT;
          dwFlags : DWORD;
          lpszDefaultScheme : LPTSTR;
       end;

     LPHIGHCONTRAST = ^HIGHCONTRAST;

     tagHIGHCONTRAST = HIGHCONTRAST;

     HSZPAIR = record
          hszSvc : HSZ;
          hszTopic : HSZ;
       end;

     tagHSZPAIR = HSZPAIR;

     ICONINFO = record
          fIcon : WINBOOL;
          xHotspot : DWORD;
          yHotspot : DWORD;
          hbmMask : HBITMAP;
          hbmColor : HBITMAP;
       end;

     PICONINFO = ^ICONINFO;

     _ICONINFO = ICONINFO;

     ICONMETRICS = record
          cbSize : UINT;
          iHorzSpacing : longint;
          iVertSpacing : longint;
          iTitleWrap : longint;
          lfFont : LOGFONT;
       end;

     LPICONMETRICS = ^ICONMETRICS;

     tagICONMETRICS = ICONMETRICS;

     IMAGEINFO = record
          hbmImage : HBITMAP;
          hbmMask : HBITMAP;
          Unused1 : longint;
          Unused2 : longint;
          rcImage : RECT;
       end;

     _IMAGEINFO = IMAGEINFO;

{$PACKRECORDS 1}

  type

     KEY_EVENT_RECORD = record
          bKeyDown : WINBOOL;
          wRepeatCount : WORD;
          wVirtualKeyCode : WORD;
          wVirtualScanCode : WORD;
          AsciiChar : char;
          pad : char;
          uChar : record
              case longint of
                 0 : ( UnicodeChar : WCHAR );
                 1 : ( AsciiChar : CHAR );
              end;
          dwControlKeyState : DWORD;
       end;

     _KEY_EVENT_RECORD = KEY_EVENT_RECORD;
{$PACKRECORDS 4}

     MOUSE_EVENT_RECORD = record
          dwMousePosition : COORD;
          dwButtonState : DWORD;
          dwControlKeyState : DWORD;
          dwEventFlags : DWORD;
       end;

     _MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD;

     WINDOW_BUFFER_SIZE_RECORD = record
          dwSize : COORD;
       end;

     _WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD;

     MENU_EVENT_RECORD = record
          dwCommandId : UINT;
       end;

     PMENU_EVENT_RECORD = ^MENU_EVENT_RECORD;

     _MENU_EVENT_RECORD = MENU_EVENT_RECORD;
{$ifndef __cplus_plus}
  { this will be the wrong size in c++  }
{$endif}

  type

     INPUT_RECORD = record
          EventType : WORD;
          Event : record
              case longint of
                 0 : ( KeyEvent : KEY_EVENT_RECORD );
                 1 : ( MouseEvent : MOUSE_EVENT_RECORD );
                 2 : ( WindowBufferSizeEvent : WINDOW_BUFFER_SIZE_RECORD );
                 3 : ( MenuEvent : MENU_EVENT_RECORD );
                 4 : ( FocusEvent : FOCUS_EVENT_RECORD );
              end;
       end;

     PINPUT_RECORD = ^INPUT_RECORD;

     _INPUT_RECORD = INPUT_RECORD;

     SYSTEMTIME = record
          wYear : WORD;
          wMonth : WORD;
          wDayOfWeek : WORD;
          wDay : WORD;
          wHour : WORD;
          wMinute : WORD;
          wSecond : WORD;
          wMilliseconds : WORD;
       end;

     LPSYSTEMTIME = ^SYSTEMTIME;

     _SYSTEMTIME = SYSTEMTIME;

     JOB_INFO_1 = record
          JobId : DWORD;
          pPrinterName : LPTSTR;
          pMachineName : LPTSTR;
          pUserName : LPTSTR;
          pDocument : LPTSTR;
          pDatatype : LPTSTR;
          pStatus : LPTSTR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          TotalPages : DWORD;
          PagesPrinted : DWORD;
          Submitted : SYSTEMTIME;
       end;

     _JOB_INFO_1 = JOB_INFO_1;

     SID_IDENTIFIER_AUTHORITY = record
          Value : array[0..5] of BYTE;
       end;

     PSID_IDENTIFIER_AUTHORITY = ^SID_IDENTIFIER_AUTHORITY;

     LPSID_IDENTIFIER_AUTHORITY = ^SID_IDENTIFIER_AUTHORITY;

     _SID_IDENTIFIER_AUTHORITY = SID_IDENTIFIER_AUTHORITY;

     SID = record
          Revision : BYTE;
          SubAuthorityCount : BYTE;
          IdentifierAuthority : SID_IDENTIFIER_AUTHORITY;
          SubAuthority : array[0..(ANYSIZE_ARRAY)-1] of DWORD;
       end;

     PSID = ^SID;

     _SID = SID;

     SECURITY_DESCRIPTOR_CONTROL = WORD;

     PSECURITY_DESCRIPTOR_CONTROL = ^SECURITY_DESCRIPTOR_CONTROL;

     SECURITY_DESCRIPTOR = record
          Revision : BYTE;
          Sbz1 : BYTE;
          Control : SECURITY_DESCRIPTOR_CONTROL;
          Owner : PSID;
          Group : PSID;
          Sacl : PACL;
          Dacl : PACL;
       end;

     PSECURITY_DESCRIPTOR = ^SECURITY_DESCRIPTOR;

     _SECURITY_DESCRIPTOR = SECURITY_DESCRIPTOR;

     JOB_INFO_2 = record
          JobId : DWORD;
          pPrinterName : LPTSTR;
          pMachineName : LPTSTR;
          pUserName : LPTSTR;
          pDocument : LPTSTR;
          pNotifyName : LPTSTR;
          pDatatype : LPTSTR;
          pPrintProcessor : LPTSTR;
          pParameters : LPTSTR;
          pDriverName : LPTSTR;
          pDevMode : LPDEVMODE;
          pStatus : LPTSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          TotalPages : DWORD;
          Size : DWORD;
          Submitted : SYSTEMTIME;
          Time : DWORD;
          PagesPrinted : DWORD;
       end;

     _JOB_INFO_2 = JOB_INFO_2;

     KERNINGPAIR = record
          wFirst : WORD;
          wSecond : WORD;
          iKernAmount : longint;
       end;

     LPKERNINGPAIR = ^KERNINGPAIR;

     tagKERNINGPAIR = KERNINGPAIR;

     LANA_ENUM = record
          length : UCHAR;
          lana : array[0..(MAX_LANA)-1] of UCHAR;
       end;

     _LANA_ENUM = LANA_ENUM;

     LDT_ENTRY = record
          LimitLow : WORD;
          BaseLow : WORD;
          HighWord : record
              case longint of
                 0 : ( Bytes : record
                      BaseMid : BYTE;
                      Flags1 : BYTE;
                      Flags2 : BYTE;
                      BaseHi : BYTE;
                   end );
                 1 : ( Bits : record
                      flag0 : longint;
                   end );
              end;
       end;

     PLDT_ENTRY = ^LDT_ENTRY;

     LPLDT_ENTRY = ^LDT_ENTRY;

     _LDT_ENTRY = LDT_ENTRY;
  const
     bm_LDT_ENTRY_BaseMid = $FF;
     bp_LDT_ENTRY_BaseMid = 0;
     bm_LDT_ENTRY_Type = $1F00;
     bp_LDT_ENTRY_Type = 8;
     bm_LDT_ENTRY_Dpl = $6000;
     bp_LDT_ENTRY_Dpl = 13;
     bm_LDT_ENTRY_Pres = $8000;
     bp_LDT_ENTRY_Pres = 15;
     bm_LDT_ENTRY_LimitHi = $F0000;
     bp_LDT_ENTRY_LimitHi = 16;
     bm_LDT_ENTRY_Sys = $100000;
     bp_LDT_ENTRY_Sys = 20;
     bm_LDT_ENTRY_Reserved_0 = $200000;
     bp_LDT_ENTRY_Reserved_0 = 21;
     bm_LDT_ENTRY_Default_Big = $400000;
     bp_LDT_ENTRY_Default_Big = 22;
     bm_LDT_ENTRY_Granularity = $800000;
     bp_LDT_ENTRY_Granularity = 23;
     bm_LDT_ENTRY_BaseHi = $FF000000;
     bp_LDT_ENTRY_BaseHi = 24;

  type

     LOCALESIGNATURE = record
          lsUsb : array[0..3] of DWORD;
          lsCsbDefault : array[0..1] of DWORD;
          lsCsbSupported : array[0..1] of DWORD;
       end;

     tagLOCALESIGNATURE = LOCALESIGNATURE;

     LOCALGROUP_MEMBERS_INFO_0 = record
          lgrmi0_sid : PSID;
       end;

     _LOCALGROUP_MEMBERS_INFO_0 = LOCALGROUP_MEMBERS_INFO_0;

     LOCALGROUP_MEMBERS_INFO_3 = record
          lgrmi3_domainandname : LPWSTR;
       end;

     _LOCALGROUP_MEMBERS_INFO_3 = LOCALGROUP_MEMBERS_INFO_3;

     FXPT16DOT16 = longint;

     LPFXPT16DOT16 = ^FXPT16DOT16;

     LUID = LARGE_INTEGER;

     PLUID = ^LUID;

     LUID_AND_ATTRIBUTES = record
          Luid : LUID;
          Attributes : DWORD;
       end;

     _LUID_AND_ATTRIBUTES = LUID_AND_ATTRIBUTES;

     LUID_AND_ATTRIBUTES_ARRAY = array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;

     PLUID_AND_ATTRIBUTES_ARRAY = ^LUID_AND_ATTRIBUTES_ARRAY;

     LV_COLUMN = record
          mask : UINT;
          fmt : longint;
          cx : longint;
          pszText : LPTSTR;
          cchTextMax : longint;
          iSubItem : longint;
       end;

     _LV_COLUMN = LV_COLUMN;

     LV_ITEM = record
          mask : UINT;
          iItem : longint;
          iSubItem : longint;
          state : UINT;
          stateMask : UINT;
          pszText : LPTSTR;
          cchTextMax : longint;
          iImage : longint;
          lParam : LPARAM;
       end;

     _LV_ITEM = LV_ITEM;

     LV_DISPINFO = record
          hdr : NMHDR;
          item : LV_ITEM;
       end;

     tagLV_DISPINFO = LV_DISPINFO;

     LV_FINDINFO = record
          flags : UINT;
          psz : LPCTSTR;
          lParam : LPARAM;
          pt : POINT;
          vkDirection : UINT;
       end;

     _LV_FINDINFO = LV_FINDINFO;

     LV_HITTESTINFO = record
          pt : POINT;
          flags : UINT;
          iItem : longint;
       end;

     _LV_HITTESTINFO = LV_HITTESTINFO;

     LV_KEYDOWN = record
          hdr : NMHDR;
          wVKey : WORD;
          flags : UINT;
       end;

     tagLV_KEYDOWN = LV_KEYDOWN;

     MAT2 = record
          eM11 : FIXED;
          eM12 : FIXED;
          eM21 : FIXED;
          eM22 : FIXED;
       end;

     _MAT2 = MAT2;

     MDICREATESTRUCT = record
          szClass : LPCTSTR;
          szTitle : LPCTSTR;
          hOwner : HANDLE;
          x : longint;
          y : longint;
          cx : longint;
          cy : longint;
          style : DWORD;
          lParam : LPARAM;
       end;

     tagMDICREATESTRUCT = MDICREATESTRUCT;

     LPMDICREATESTRUCT = ^MDICREATESTRUCT;

     MEASUREITEMSTRUCT = record
          CtlType : UINT;
          CtlID : UINT;
          itemID : UINT;
          itemWidth : UINT;
          itemHeight : UINT;
          itemData : DWORD;
       end;

     LPMEASUREITEMSTRUCT = ^MEASUREITEMSTRUCT;

     tagMEASUREITEMSTRUCT = MEASUREITEMSTRUCT;

     MEMORY_BASIC_INFORMATION = record
          BaseAddress : PVOID;
          AllocationBase : PVOID;
          AllocationProtect : DWORD;
          RegionSize : DWORD;
          State : DWORD;
          Protect : DWORD;
          _Type : DWORD;
       end;

     _MEMORY_BASIC_INFORMATION = MEMORY_BASIC_INFORMATION;

     PMEMORY_BASIC_INFORMATION = ^MEMORY_BASIC_INFORMATION;

     MEMORYSTATUS = record
          dwLength : DWORD;
          dwMemoryLoad : DWORD;
          dwTotalPhys : DWORD;
          dwAvailPhys : DWORD;
          dwTotalPageFile : DWORD;
          dwAvailPageFile : DWORD;
          dwTotalVirtual : DWORD;
          dwAvailVirtual : DWORD;
       end;

     LPMEMORYSTATUS = ^MEMORYSTATUS;

     _MEMORYSTATUS = MEMORYSTATUS;

     MENUEX_TEMPLATE_HEADER = record
          wVersion : WORD;
          wOffset : WORD;
          dwHelpId : DWORD;
       end;

     MENUEX_TEMPLATE_ITEM = record
          dwType : DWORD;
          dwState : DWORD;
          uId : UINT;
          bResInfo : BYTE;
          szText : array[0..0] of WCHAR;
          dwHelpId : DWORD;
       end;

     MENUITEMINFO = record
          cbSize : UINT;
          fMask : UINT;
          fType : UINT;
          fState : UINT;
          wID : UINT;
          hSubMenu : HMENU;
          hbmpChecked : HBITMAP;
          hbmpUnchecked : HBITMAP;
          dwItemData : DWORD;
          dwTypeData : LPTSTR;
          cch : UINT;
       end;

     LPMENUITEMINFO = ^MENUITEMINFO;

     tagMENUITEMINFO = MENUITEMINFO;
(* Const before declarator ignored *)

     LPCMENUITEMINFO = ^MENUITEMINFO;

     MENUITEMTEMPLATE = record
          mtOption : WORD;
          mtID : WORD;
          mtString : array[0..0] of WCHAR;
       end;

     MENUITEMTEMPLATEHEADER = record
          versionNumber : WORD;
          offset : WORD;
       end;

     MENUTEMPLATE = record
                    end;

     LPMENUTEMPLATE = ^MENUTEMPLATE;

     METAFILEPICT = record
          mm : LONG;
          xExt : LONG;
          yExt : LONG;
          hMF : HMETAFILE;
       end;

     PMETAFILEPICT = ^METAFILEPICT;

     LPMETAFILEPICT = ^METAFILEPICT;

     tagMETAFILEPICT = METAFILEPICT;
{$PACKRECORDS 1}

     METAHEADER = record
          mtType : WORD;
          mtHeaderSize : WORD;
          mtVersion : WORD;
          mtSize : DWORD;
          mtNoObjects : WORD;
          mtMaxRecord : DWORD;
          mtNoParameters : WORD;
       end;

     tagMETAHEADER = METAHEADER;
{$PACKRECORDS 4}

     METARECORD = record
          rdSize : DWORD;
          rdFunction : WORD;
          rdParm : array[0..0] of WORD;
       end;

     LPMETARECORD = ^METARECORD;

     tagMETARECORD = METARECORD;

     MINIMIZEDMETRICS = record
          cbSize : UINT;
          iWidth : longint;
          iHorzGap : longint;
          iVertGap : longint;
          iArrange : longint;
       end;

     LPMINIMIZEDMETRICS = ^MINIMIZEDMETRICS;

     tagMINIMIZEDMETRICS = MINIMIZEDMETRICS;

     MINMAXINFO = record
          ptReserved : POINT;
          ptMaxSize : POINT;
          ptMaxPosition : POINT;
          ptMinTrackSize : POINT;
          ptMaxTrackSize : POINT;
       end;

     tagMINMAXINFO = MINMAXINFO;

     MODEMDEVCAPS = record
          dwActualSize : DWORD;
          dwRequiredSize : DWORD;
          dwDevSpecificOffset : DWORD;
          dwDevSpecificSize : DWORD;
          dwModemProviderVersion : DWORD;
          dwModemManufacturerOffset : DWORD;
          dwModemManufacturerSize : DWORD;
          dwModemModelOffset : DWORD;
          dwModemModelSize : DWORD;
          dwModemVersionOffset : DWORD;
          dwModemVersionSize : DWORD;
          dwDialOptions : DWORD;
          dwCallSetupFailTimer : DWORD;
          dwInactivityTimeout : DWORD;
          dwSpeakerVolume : DWORD;
          dwSpeakerMode : DWORD;
          dwModemOptions : DWORD;
          dwMaxDTERate : DWORD;
          dwMaxDCERate : DWORD;
          abVariablePortion : array[0..0] of BYTE;
       end;

     PMODEMDEVCAPS = ^MODEMDEVCAPS;

     LPMODEMDEVCAPS = ^MODEMDEVCAPS;

     modemdevcaps_tag = MODEMDEVCAPS;

     MODEMSETTINGS = record
          dwActualSize : DWORD;
          dwRequiredSize : DWORD;
          dwDevSpecificOffset : DWORD;
          dwDevSpecificSize : DWORD;
          dwCallSetupFailTimer : DWORD;
          dwInactivityTimeout : DWORD;
          dwSpeakerVolume : DWORD;
          dwSpeakerMode : DWORD;
          dwPreferredModemOptions : DWORD;
          dwNegotiatedModemOptions : DWORD;
          dwNegotiatedDCERate : DWORD;
          abVariablePortion : array[0..0] of BYTE;
       end;

     PMODEMSETTINGS = ^MODEMSETTINGS;

     LPMODEMSETTINGS = ^MODEMSETTINGS;

     modemsettings_tag = MODEMSETTINGS;

     MONCBSTRUCT = record
          cb : UINT;
          dwTime : DWORD;
          hTask : HANDLE;
          dwRet : DWORD;
          wType : UINT;
          wFmt : UINT;
          hConv : HCONV;
          hsz1 : HSZ;
          hsz2 : HSZ;
          hData : HDDEDATA;
          dwData1 : DWORD;
          dwData2 : DWORD;
          cc : CONVCONTEXT;
          cbData : DWORD;
          Data : array[0..7] of DWORD;
       end;

     tagMONCBSTRUCT = MONCBSTRUCT;

     MONCONVSTRUCT = record
          cb : UINT;
          fConnect : WINBOOL;
          dwTime : DWORD;
          hTask : HANDLE;
          hszSvc : HSZ;
          hszTopic : HSZ;
          hConvClient : HCONV;
          hConvServer : HCONV;
       end;

     tagMONCONVSTRUCT = MONCONVSTRUCT;

     MONERRSTRUCT = record
          cb : UINT;
          wLastError : UINT;
          dwTime : DWORD;
          hTask : HANDLE;
       end;

     tagMONERRSTRUCT = MONERRSTRUCT;

     MONHSZSTRUCT = record
          cb : UINT;
          fsAction : WINBOOL;
          dwTime : DWORD;
          hsz : HSZ;
          hTask : HANDLE;
          str : array[0..0] of TCHAR;
       end;

     tagMONHSZSTRUCT = MONHSZSTRUCT;

     MONITOR_INFO_1 = record
          pName : LPTSTR;
       end;

     _MONITOR_INFO_1 = MONITOR_INFO_1;

     MONITOR_INFO_2 = record
          pName : LPTSTR;
          pEnvironment : LPTSTR;
          pDLLName : LPTSTR;
       end;

     _MONITOR_INFO_2 = MONITOR_INFO_2;

     MONLINKSTRUCT = record
          cb : UINT;
          dwTime : DWORD;
          hTask : HANDLE;
          fEstablished : WINBOOL;
          fNoData : WINBOOL;
          hszSvc : HSZ;
          hszTopic : HSZ;
          hszItem : HSZ;
          wFmt : UINT;
          fServer : WINBOOL;
          hConvServer : HCONV;
          hConvClient : HCONV;
       end;

     tagMONLINKSTRUCT = MONLINKSTRUCT;

     MONMSGSTRUCT = record
          cb : UINT;
          hwndTo : HWND;
          dwTime : DWORD;
          hTask : HANDLE;
          wMsg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
          dmhd : DDEML_MSG_HOOK_DATA;
       end;

     tagMONMSGSTRUCT = MONMSGSTRUCT;

     MOUSEHOOKSTRUCT = record
          pt : POINT;
          hwnd : HWND;
          wHitTestCode : UINT;
          dwExtraInfo : DWORD;
       end;

     PMOUSEHOOKSTRUCT = ^MOUSEHOOKSTRUCT;

     LPMOUSEHOOKSTRUCT = ^MOUSEHOOKSTRUCT;

     tagMOUSEHOOKSTRUCT = MOUSEHOOKSTRUCT;

     MOUSEKEYS = record
          cbSize : DWORD;
          dwFlags : DWORD;
          iMaxSpeed : DWORD;
          iTimeToMaxSpeed : DWORD;
          iCtrlSpeed : DWORD;
          dwReserved1 : DWORD;
          dwReserved2 : DWORD;
       end;

     _MOUSEKEYS = MOUSEKEYS;

     MSG = record
          hwnd : HWND;
          message : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
          time : DWORD;
          pt : POINT;
       end;

     LPMSG = ^MSG;

     tagMSG = MSG;

     MSGBOXCALLBACK = procedure (lpHelpInfo:LPHELPINFO);

     MSGBOXPARAMS = record
          cbSize : UINT;
          hwndOwner : HWND;
          hInstance : HINST;
          lpszText : LPCSTR;
          lpszCaption : LPCSTR;
          dwStyle : DWORD;
          lpszIcon : LPCSTR;
          dwContextHelpId : DWORD;
          lpfnMsgBoxCallback : MSGBOXCALLBACK;
          dwLanguageId : DWORD;
       end;

     PMSGBOXPARAMS = ^MSGBOXPARAMS;

     LPMSGBOXPARAMS = ^MSGBOXPARAMS;

     MSGFILTER = record
          nmhdr : NMHDR;
          msg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
       end;

     _msgfilter = MSGFILTER;

     MULTIKEYHELP = record
          mkSize : DWORD;
          mkKeylist : TCHAR;
          szKeyphrase : array[0..0] of TCHAR;
       end;

     tagMULTIKEYHELP = MULTIKEYHELP;

     NAME_BUFFER = record
          name : array[0..(NCBNAMSZ)-1] of UCHAR;
          name_num : UCHAR;
          name_flags : UCHAR;
       end;

     _NAME_BUFFER = NAME_BUFFER;

     p_NCB = ^_NCB;
     NCB = record
          ncb_command : UCHAR;
          ncb_retcode : UCHAR;
          ncb_lsn : UCHAR;
          ncb_num : UCHAR;
          ncb_buffer : PUCHAR;
          ncb_length : WORD;
          ncb_callname : array[0..(NCBNAMSZ)-1] of UCHAR;
          ncb_name : array[0..(NCBNAMSZ)-1] of UCHAR;
          ncb_rto : UCHAR;
          ncb_sto : UCHAR;
          ncb_post : procedure (_para1:p_NCB);CDECL;
          ncb_lana_num : UCHAR;
          ncb_cmd_cplt : UCHAR;
          ncb_reserve : array[0..9] of UCHAR;
          ncb_event : HANDLE;
       end;

     _NCB = NCB;

     NCCALCSIZE_PARAMS = record
          rgrc : array[0..2] of RECT;
          lppos : PWINDOWPOS;
       end;

     _NCCALCSIZE_PARAMS = NCCALCSIZE_PARAMS;

     NDDESHAREINFO = record
          lRevision : LONG;
          lpszShareName : LPTSTR;
          lShareType : LONG;
          lpszAppTopicList : LPTSTR;
          fSharedFlag : LONG;
          fService : LONG;
          fStartAppFlag : LONG;
          nCmdShow : LONG;
          qModifyId : array[0..1] of LONG;
          cNumItems : LONG;
          lpszItemList : LPTSTR;
       end;

     _NDDESHAREINFO = NDDESHAREINFO;

     NETRESOURCE = record
          dwScope : DWORD;
          dwType : DWORD;
          dwDisplayType : DWORD;
          dwUsage : DWORD;
          lpLocalName : LPTSTR;
          lpRemoteName : LPTSTR;
          lpComment : LPTSTR;
          lpProvider : LPTSTR;
       end;

     LPNETRESOURCE = ^NETRESOURCE;

     _NETRESOURCE = NETRESOURCE;

     NEWCPLINFO = record
          dwSize : DWORD;
          dwFlags : DWORD;
          dwHelpContext : DWORD;
          lData : LONG;
          hIcon : HICON;
          szName : array[0..31] of TCHAR;
          szInfo : array[0..63] of TCHAR;
          szHelpFile : array[0..127] of TCHAR;
       end;

     tagNEWCPLINFO = NEWCPLINFO;

     NEWTEXTMETRIC = record
          tmHeight : LONG;
          tmAscent : LONG;
          tmDescent : LONG;
          tmInternalLeading : LONG;
          tmExternalLeading : LONG;
          tmAveCharWidth : LONG;
          tmMaxCharWidth : LONG;
          tmWeight : LONG;
          tmOverhang : LONG;
          tmDigitizedAspectX : LONG;
          tmDigitizedAspectY : LONG;
          tmFirstChar : BCHAR;
          tmLastChar : BCHAR;
          tmDefaultChar : BCHAR;
          tmBreakChar : BCHAR;
          tmItalic : BYTE;
          tmUnderlined : BYTE;
          tmStruckOut : BYTE;
          tmPitchAndFamily : BYTE;
          tmCharSet : BYTE;
          ntmFlags : DWORD;
          ntmSizeEM : UINT;
          ntmCellHeight : UINT;
          ntmAvgWidth : UINT;
       end;

     tagNEWTEXTMETRIC = NEWTEXTMETRIC;

     NEWTEXTMETRICEX = record
          ntmentm : NEWTEXTMETRIC;
          ntmeFontSignature : FONTSIGNATURE;
       end;

     tagNEWTEXTMETRICEX = NEWTEXTMETRICEX;

     NM_LISTVIEW = record
          hdr : NMHDR;
          iItem : longint;
          iSubItem : longint;
          uNewState : UINT;
          uOldState : UINT;
          uChanged : UINT;
          ptAction : POINT;
          lParam : LPARAM;
       end;

     tagNM_LISTVIEW = NM_LISTVIEW;

{$ifndef windows_include_files}
{ already in defines.pp file }
      TREEITEM = record
                 end;
     HTREEITEM = ^TREEITEM;
{$endif windows_include_files}

     TV_ITEM = record
          mask : UINT;
          hItem : HTREEITEM;
          state : UINT;
          stateMask : UINT;
          pszText : LPTSTR;
          cchTextMax : longint;
          iImage : longint;
          iSelectedImage : longint;
          cChildren : longint;
          lParam : LPARAM;
       end;

     LPTV_ITEM = ^TV_ITEM;

     _TV_ITEM = TV_ITEM;

     NM_TREEVIEW = record
          hdr : NMHDR;
          action : UINT;
          itemOld : TV_ITEM;
          itemNew : TV_ITEM;
          ptDrag : POINT;
       end;

     _NM_TREEVIEW = NM_TREEVIEW;

     LPNM_TREEVIEW = ^NM_TREEVIEW;

     NM_UPDOWNW = record
          hdr : NMHDR;
          iPos : longint;
          iDelta : longint;
       end;

     _NM_UPDOWN = NM_UPDOWNW;

     NONCLIENTMETRICS = record
          cbSize : UINT;
          iBorderWidth : longint;
          iScrollWidth : longint;
          iScrollHeight : longint;
          iCaptionWidth : longint;
          iCaptionHeight : longint;
          lfCaptionFont : LOGFONT;
          iSmCaptionWidth : longint;
          iSmCaptionHeight : longint;
          lfSmCaptionFont : LOGFONT;
          iMenuWidth : longint;
          iMenuHeight : longint;
          lfMenuFont : LOGFONT;
          lfStatusFont : LOGFONT;
          lfMessageFont : LOGFONT;
       end;

     LPNONCLIENTMETRICS = ^NONCLIENTMETRICS;

     tagNONCLIENTMETRICS = NONCLIENTMETRICS;

     SERVICE_ADDRESS = record
          dwAddressType : DWORD;
          dwAddressFlags : DWORD;
          dwAddressLength : DWORD;
          dwPrincipalLength : DWORD;
          lpAddress : ^BYTE;
          lpPrincipal : ^BYTE;
       end;

     _SERVICE_ADDRESS = SERVICE_ADDRESS;

     SERVICE_ADDRESSES = record
          dwAddressCount : DWORD;
          Addresses : array[0..0] of SERVICE_ADDRESS;
       end;

     LPSERVICE_ADDRESSES = ^SERVICE_ADDRESSES;

     _SERVICE_ADDRESSES = SERVICE_ADDRESSES;

     GUID = record
          Data1 : cardinal;
          Data2 : word;
          Data3 : word;
          Data4 : array[0..7] of byte;
       end;

     LPGUID = ^GUID;

     _GUID = GUID;

     CLSID = GUID;

     LPCLSID = ^CLSID;

     SERVICE_INFO = record
          lpServiceType : LPGUID;
          lpServiceName : LPTSTR;
          lpComment : LPTSTR;
          lpLocale : LPTSTR;
          dwDisplayHint : DWORD;
          dwVersion : DWORD;
          dwTime : DWORD;
          lpMachineName : LPTSTR;
          lpServiceAddress : LPSERVICE_ADDRESSES;
          ServiceSpecificInfo : BLOB;
       end;

     _SERVICE_INFO = SERVICE_INFO;

     NS_SERVICE_INFO = record
          dwNameSpace : DWORD;
          ServiceInfo : SERVICE_INFO;
       end;

     _NS_SERVICE_INFO = NS_SERVICE_INFO;

     NUMBERFMT = record
          NumDigits : UINT;
          LeadingZero : UINT;
          Grouping : UINT;
          lpDecimalSep : LPTSTR;
          lpThousandSep : LPTSTR;
          NegativeOrder : UINT;
       end;

     _numberfmt = NUMBERFMT;

     OFSTRUCT = record
          cBytes : BYTE;
          fFixedDisk : BYTE;
          nErrCode : WORD;
          Reserved1 : WORD;
          Reserved2 : WORD;
          szPathName : array[0..(OFS_MAXPATHNAME)-1] of CHAR;
       end;

     LPOFSTRUCT = ^OFSTRUCT;

     _OFSTRUCT = OFSTRUCT;

     OPENFILENAME = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hInstance : HINST;
          lpstrFilter : LPCTSTR;
          lpstrCustomFilter : LPTSTR;
          nMaxCustFilter : DWORD;
          nFilterIndex : DWORD;
          lpstrFile : LPTSTR;
          nMaxFile : DWORD;
          lpstrFileTitle : LPTSTR;
          nMaxFileTitle : DWORD;
          lpstrInitialDir : LPCTSTR;
          lpstrTitle : LPCTSTR;
          Flags : DWORD;
          nFileOffset : WORD;
          nFileExtension : WORD;
          lpstrDefExt : LPCTSTR;
          lCustData : DWORD;
          lpfnHook : LPOFNHOOKPROC;
          lpTemplateName : LPCTSTR;
       end;

     LPOPENFILENAME = ^OPENFILENAME;

     tagOFN = OPENFILENAME;

     OFNOTIFY = record
          hdr : NMHDR;
          lpOFN : LPOPENFILENAME;
          pszFile : LPTSTR;
       end;

     LPOFNOTIFY = ^OFNOTIFY;

     _OFNOTIFY = OFNOTIFY;

     OSVERSIONINFO = record
          dwOSVersionInfoSize : DWORD;
          dwMajorVersion : DWORD;
          dwMinorVersion : DWORD;
          dwBuildNumber : DWORD;
          dwPlatformId : DWORD;
          szCSDVersion : array[0..127] of TCHAR;
       end;

     POSVERSIONINFO = ^OSVERSIONINFO;

     LPOSVERSIONINFO = ^OSVERSIONINFO;

     _OSVERSIONINFO = OSVERSIONINFO;

     TEXTMETRIC = record
          tmHeight : LONG;
          tmAscent : LONG;
          tmDescent : LONG;
          tmInternalLeading : LONG;
          tmExternalLeading : LONG;
          tmAveCharWidth : LONG;
          tmMaxCharWidth : LONG;
          tmWeight : LONG;
          tmOverhang : LONG;
          tmDigitizedAspectX : LONG;
          tmDigitizedAspectY : LONG;
          tmFirstChar : BCHAR;
          tmLastChar : BCHAR;
          tmDefaultChar : BCHAR;
          tmBreakChar : BCHAR;
          tmItalic : BYTE;
          tmUnderlined : BYTE;
          tmStruckOut : BYTE;
          tmPitchAndFamily : BYTE;
          tmCharSet : BYTE;
       end;

     LPTEXTMETRIC = ^TEXTMETRIC;

     tagTEXTMETRIC = TEXTMETRIC;

     OUTLINETEXTMETRIC = record
          otmSize : UINT;
          otmTextMetrics : TEXTMETRIC;
          otmFiller : BYTE;
          otmPanoseNumber : PANOSE;
          otmfsSelection : UINT;
          otmfsType : UINT;
          otmsCharSlopeRise : longint;
          otmsCharSlopeRun : longint;
          otmItalicAngle : longint;
          otmEMSquare : UINT;
          otmAscent : longint;
          otmDescent : longint;
          otmLineGap : UINT;
          otmsCapEmHeight : UINT;
          otmsXHeight : UINT;
          otmrcFontBox : RECT;
          otmMacAscent : longint;
          otmMacDescent : longint;
          otmMacLineGap : UINT;
          otmusMinimumPPEM : UINT;
          otmptSubscriptSize : POINT;
          otmptSubscriptOffset : POINT;
          otmptSuperscriptSize : POINT;
          otmptSuperscriptOffset : POINT;
          otmsStrikeoutSize : UINT;
          otmsStrikeoutPosition : longint;
          otmsUnderscoreSize : longint;
          otmsUnderscorePosition : longint;
          otmpFamilyName : PSTR;
          otmpFaceName : PSTR;
          otmpStyleName : PSTR;
          otmpFullName : PSTR;
       end;

     LPOUTLINETEXTMETRIC = ^OUTLINETEXTMETRIC;

     _OUTLINETEXTMETRIC = OUTLINETEXTMETRIC;

     OVERLAPPED = record
          Internal : DWORD;
          InternalHigh : DWORD;
          Offset : DWORD;
          OffsetHigh : DWORD;
          hEvent : HANDLE;
       end;

     LPOVERLAPPED = ^OVERLAPPED;

     _OVERLAPPED = OVERLAPPED;

     {PAGESETUPDLG = record conflicts with function PageSetupDlg }
     TPAGESETUPDLG = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hDevMode : HGLOBAL;
          hDevNames : HGLOBAL;
          Flags : DWORD;
          ptPaperSize : POINT;
          rtMinMargin : RECT;
          rtMargin : RECT;
          hInstance : HINST;
          lCustData : LPARAM;
          lpfnPageSetupHook : LPPAGESETUPHOOK;
          lpfnPagePaintHook : LPPAGEPAINTHOOK;
          lpPageSetupTemplateName : LPCTSTR;
          hPageSetupTemplate : HGLOBAL;
       end;

     LPPAGESETUPDLG = ^TPAGESETUPDLG;

     tagPSD = TPAGESETUPDLG;

     PAINTSTRUCT = record
          hdc : HDC;
          fErase : WINBOOL;
          rcPaint : RECT;
          fRestore : WINBOOL;
          fIncUpdate : WINBOOL;
          rgbReserved : array[0..31] of BYTE;
       end;

     LPPAINTSTRUCT = ^PAINTSTRUCT;

     tagPAINTSTRUCT = PAINTSTRUCT;

     PARAFORMAT = record
          cbSize : UINT;
          dwMask : DWORD;
          wNumbering : WORD;
          wReserved : WORD;
          dxStartIndent : LONG;
          dxRightIndent : LONG;
          dxOffset : LONG;
          wAlignment : WORD;
          cTabCount : SHORT;
          rgxTabs : array[0..(MAX_TAB_STOPS)-1] of LONG;
       end;

     _paraformat = PARAFORMAT;

     PERF_COUNTER_BLOCK = record
          ByteLength : DWORD;
       end;

     _PERF_COUNTER_BLOCK = PERF_COUNTER_BLOCK;

     PERF_COUNTER_DEFINITION = record
          ByteLength : DWORD;
          CounterNameTitleIndex : DWORD;
          CounterNameTitle : LPWSTR;
          CounterHelpTitleIndex : DWORD;
          CounterHelpTitle : LPWSTR;
          DefaultScale : DWORD;
          DetailLevel : DWORD;
          CounterType : DWORD;
          CounterSize : DWORD;
          CounterOffset : DWORD;
       end;

     _PERF_COUNTER_DEFINITION = PERF_COUNTER_DEFINITION;

     PERF_DATA_BLOCK = record
          Signature : array[0..3] of WCHAR;
          LittleEndian : DWORD;
          Version : DWORD;
          Revision : DWORD;
          TotalByteLength : DWORD;
          HeaderLength : DWORD;
          NumObjectTypes : DWORD;
          DefaultObject : DWORD;
          SystemTime : SYSTEMTIME;
          PerfTime : LARGE_INTEGER;
          PerfFreq : LARGE_INTEGER;
          PerfTime100nSec : LARGE_INTEGER;
          SystemNameLength : DWORD;
          SystemNameOffset : DWORD;
       end;

     _PERF_DATA_BLOCK = PERF_DATA_BLOCK;

     PERF_INSTANCE_DEFINITION = record
          ByteLength : DWORD;
          ParentObjectTitleIndex : DWORD;
          ParentObjectInstance : DWORD;
          UniqueID : DWORD;
          NameOffset : DWORD;
          NameLength : DWORD;
       end;

     _PERF_INSTANCE_DEFINITION = PERF_INSTANCE_DEFINITION;

     PERF_OBJECT_TYPE = record
          TotalByteLength : DWORD;
          DefinitionLength : DWORD;
          HeaderLength : DWORD;
          ObjectNameTitleIndex : DWORD;
          ObjectNameTitle : LPWSTR;
          ObjectHelpTitleIndex : DWORD;
          ObjectHelpTitle : LPWSTR;
          DetailLevel : DWORD;
          NumCounters : DWORD;
          DefaultCounter : DWORD;
          NumInstances : DWORD;
          CodePage : DWORD;
          PerfTime : LARGE_INTEGER;
          PerfFreq : LARGE_INTEGER;
       end;

     _PERF_OBJECT_TYPE = PERF_OBJECT_TYPE;

     POLYTEXT = record
          x : longint;
          y : longint;
          n : UINT;
          lpstr : LPCTSTR;
          uiFlags : UINT;
          rcl : RECT;
          pdx : ^longint;
       end;

     _POLYTEXT = POLYTEXT;

     PORT_INFO_1 = record
          pName : LPTSTR;
       end;

     _PORT_INFO_1 = PORT_INFO_1;

     PORT_INFO_2 = record
          pPortName : LPSTR;
          pMonitorName : LPSTR;
          pDescription : LPSTR;
          fPortType : DWORD;
          Reserved : DWORD;
       end;

     _PORT_INFO_2 = PORT_INFO_2;

     PREVENT_MEDIA_REMOVAL = record
          PreventMediaRemoval : BOOLEAN;
       end;

     _PREVENT_MEDIA_REMOVAL = PREVENT_MEDIA_REMOVAL;
{$PACKRECORDS 1}

     {PRINTDLG = record conflicts with PrintDlg function }
     TPRINTDLG = record
          lStructSize : DWORD;
          hwndOwner : HWND;
          hDevMode : HANDLE;
          hDevNames : HANDLE;
          hDC : HDC;
          Flags : DWORD;
          nFromPage : WORD;
          nToPage : WORD;
          nMinPage : WORD;
          nMaxPage : WORD;
          nCopies : WORD;
          hInstance : HINST;
          lCustData : DWORD;
          lpfnPrintHook : LPPRINTHOOKPROC;
          lpfnSetupHook : LPSETUPHOOKPROC;
          lpPrintTemplateName : LPCTSTR;
          lpSetupTemplateName : LPCTSTR;
          hPrintTemplate : HANDLE;
          hSetupTemplate : HANDLE;
       end;

     LPPRINTDLG = ^TPRINTDLG;

     tagPD = TPRINTDLG;
{$PACKRECORDS 4}

     PRINTER_DEFAULTS = record
          pDatatype : LPTSTR;
          pDevMode : LPDEVMODE;
          DesiredAccess : ACCESS_MASK;
       end;

     _PRINTER_DEFAULTS = PRINTER_DEFAULTS;

     PRINTER_INFO_1 = record
          Flags : DWORD;
          pDescription : LPTSTR;
          pName : LPTSTR;
          pComment : LPTSTR;
       end;

     PPRINTER_INFO_1 = ^PRINTER_INFO_1;

     LPPRINTER_INFO_1 = ^PRINTER_INFO_1;

     _PRINTER_INFO_1 = PRINTER_INFO_1;

     PRINTER_INFO_2 = record
          pServerName : LPTSTR;
          pPrinterName : LPTSTR;
          pShareName : LPTSTR;
          pPortName : LPTSTR;
          pDriverName : LPTSTR;
          pComment : LPTSTR;
          pLocation : LPTSTR;
          pDevMode : LPDEVMODE;
          pSepFile : LPTSTR;
          pPrintProcessor : LPTSTR;
          pDatatype : LPTSTR;
          pParameters : LPTSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Attributes : DWORD;
          Priority : DWORD;
          DefaultPriority : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          Status : DWORD;
          cJobs : DWORD;
          AveragePPM : DWORD;
       end;

     _PRINTER_INFO_2 = PRINTER_INFO_2;

     PRINTER_INFO_3 = record
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
       end;

     _PRINTER_INFO_3 = PRINTER_INFO_3;

     PRINTER_INFO_4 = record
          pPrinterName : LPTSTR;
          pServerName : LPTSTR;
          Attributes : DWORD;
       end;

     _PRINTER_INFO_4 = PRINTER_INFO_4;

     PRINTER_INFO_5 = record
          pPrinterName : LPTSTR;
          pPortName : LPTSTR;
          Attributes : DWORD;
          DeviceNotSelectedTimeout : DWORD;
          TransmissionRetryTimeout : DWORD;
       end;

     _PRINTER_INFO_5 = PRINTER_INFO_5;

     PRINTER_NOTIFY_INFO_DATA = record
          _Type : WORD;
          Field : WORD;
          Reserved : DWORD;
          Id : DWORD;
          NotifyData : record
              case longint of
                 0 : ( adwData : array[0..1] of DWORD );
                 1 : ( Data : record
                      cbBuf : DWORD;
                      pBuf : LPVOID;
                   end );
              end;
       end;

     _PRINTER_NOTIFY_INFO_DATA = PRINTER_NOTIFY_INFO_DATA;

     PRINTER_NOTIFY_INFO = record
          Version : DWORD;
          Flags : DWORD;
          Count : DWORD;
          aData : array[0..0] of PRINTER_NOTIFY_INFO_DATA;
       end;

     _PRINTER_NOTIFY_INFO = PRINTER_NOTIFY_INFO;

     PRINTER_NOTIFY_OPTIONS_TYPE = record
          _Type : WORD;
          Reserved0 : WORD;
          Reserved1 : DWORD;
          Reserved2 : DWORD;
          Count : DWORD;
          pFields : PWORD;
       end;

     PPRINTER_NOTIFY_OPTIONS_TYPE = ^PRINTER_NOTIFY_OPTIONS_TYPE;

     _PRINTER_NOTIFY_OPTIONS_TYPE = PRINTER_NOTIFY_OPTIONS_TYPE;

     PRINTER_NOTIFY_OPTIONS = record
          Version : DWORD;
          Flags : DWORD;
          Count : DWORD;
          pTypes : PPRINTER_NOTIFY_OPTIONS_TYPE;
       end;

     _PRINTER_NOTIFY_OPTIONS = PRINTER_NOTIFY_OPTIONS;

     PRINTPROCESSOR_INFO_1 = record
          pName : LPTSTR;
       end;

     _PRINTPROCESSOR_INFO_1 = PRINTPROCESSOR_INFO_1;

     PRIVILEGE_SET = record
          PrivilegeCount : DWORD;
          Control : DWORD;
          Privilege : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
       end;

     PPRIVILEGE_SET = ^PRIVILEGE_SET;

     LPPRIVILEGE_SET = ^PRIVILEGE_SET;

     _PRIVILEGE_SET = PRIVILEGE_SET;

     PROCESS_HEAPENTRY = record
          lpData : PVOID;
          cbData : DWORD;
          cbOverhead : BYTE;
          iRegionIndex : BYTE;
          wFlags : WORD;
          dwCommittedSize : DWORD;
          dwUnCommittedSize : DWORD;
          lpFirstBlock : LPVOID;
          lpLastBlock : LPVOID;
          hMem : HANDLE;
       end;

     LPPROCESS_HEAP_ENTRY = ^PROCESS_HEAPENTRY;

     _PROCESS_HEAP_ENTRY = PROCESS_HEAPENTRY;

     PROCESS_INFORMATION = record
          hProcess : HANDLE;
          hThread : HANDLE;
          dwProcessId : DWORD;
          dwThreadId : DWORD;
       end;

     LPPROCESS_INFORMATION = ^PROCESS_INFORMATION;

     _PROCESS_INFORMATION = PROCESS_INFORMATION;

     LPFNPSPCALLBACK = function (_para1:HWND; _para2:UINT; _para3:LPVOID):UINT;

     PROPSHEETPAGE = record
          dwSize : DWORD;
          dwFlags : DWORD;
          hInstance : HINST;
          u1 : record
              case longint of
                 0 : ( pszTemplate : LPCTSTR );
                 1 : ( pResource : LPCDLGTEMPLATE );
              end;
          u2 : record
              case longint of
                 0 : ( hIcon : HICON );
                 1 : ( pszIcon : LPCTSTR );
              end;
          pszTitle : LPCTSTR;
          pfnDlgProc : DLGPROC;
          lParam : LPARAM;
          pfnCallback : LPFNPSPCALLBACK;
          pcRefParent : ^UINT;
       end;

     LPPROPSHEETPAGE = ^PROPSHEETPAGE;

     _PROPSHEETPAGE = PROPSHEETPAGE;
(* Const before type ignored *)

     LPCPROPSHEETPAGE = ^PROPSHEETPAGE;

   emptyrecord = record
                 end;
   HPROPSHEETPAGE = ^emptyrecord;

     PROPSHEETHEADER = record
          dwSize : DWORD;
          dwFlags : DWORD;
          hwndParent : HWND;
          hInstance : HINST;
          u1 : record
              case longint of
                 0 : ( hIcon : HICON );
                 1 : ( pszIcon : LPCTSTR );
              end;
          pszCaption : LPCTSTR;
          nPages : UINT;
          u2 : record
              case longint of
                 0 : ( nStartPage : UINT );
                 1 : ( pStartPage : LPCTSTR );
              end;
          u3 : record
              case longint of
                 0 : ( ppsp : LPCPROPSHEETPAGE );
                 1 : ( phpage : ^HPROPSHEETPAGE );
              end;
          pfnCallback : PFNPROPSHEETCALLBACK;
       end;

     LPPROPSHEETHEADER = ^PROPSHEETHEADER;

     _PROPSHEETHEADER = PROPSHEETHEADER;
(* Const before type ignored *)

     LPCPROPSHEETHEADER = ^PROPSHEETHEADER;
  { PropertySheet callbacks  }

     LPFNADDPROPSHEETPAGE = function (_para1:HPROPSHEETPAGE; _para2:LPARAM):WINBOOL;

     LPFNADDPROPSHEETPAGES = function (_para1:LPVOID; _para2:LPFNADDPROPSHEETPAGE; _para3:LPARAM):WINBOOL;

     PROTOCOL_INFO = record
          dwServiceFlags : DWORD;
          iAddressFamily : INT;
          iMaxSockAddr : INT;
          iMinSockAddr : INT;
          iSocketType : INT;
          iProtocol : INT;
          dwMessageSize : DWORD;
          lpProtocol : LPTSTR;
       end;

     _PROTOCOL_INFO = PROTOCOL_INFO;

     PROVIDOR_INFO_1 = record
          pName : LPTSTR;
          pEnvironment : LPTSTR;
          pDLLName : LPTSTR;
       end;

     _PROVIDOR_INFO_1 = PROVIDOR_INFO_1;

     PSHNOTIFY = record
          hdr : NMHDR;
          lParam : LPARAM;
       end;

     LPPSHNOTIFY = ^PSHNOTIFY;

     _PSHNOTIFY = PSHNOTIFY;

     PUNCTUATION = record
          iSize : UINT;
          szPunctuation : LPSTR;
       end;

     _punctuation = PUNCTUATION;

     QUERY_SERVICE_CONFIG = record
          dwServiceType : DWORD;
          dwStartType : DWORD;
          dwErrorControl : DWORD;
          lpBinaryPathName : LPTSTR;
          lpLoadOrderGroup : LPTSTR;
          dwTagId : DWORD;
          lpDependencies : LPTSTR;
          lpServiceStartName : LPTSTR;
          lpDisplayName : LPTSTR;
       end;

     LPQUERY_SERVICE_CONFIG = ^QUERY_SERVICE_CONFIG;

     _QUERY_SERVICE_CONFIG = QUERY_SERVICE_CONFIG;

     QUERY_SERVICE_LOCK_STATUS = record
          fIsLocked : DWORD;
          lpLockOwner : LPTSTR;
          dwLockDuration : DWORD;
       end;

     LPQUERY_SERVICE_LOCK_STATUS = ^QUERY_SERVICE_LOCK_STATUS;

     _QUERY_SERVICE_LOCK_STATUS = QUERY_SERVICE_LOCK_STATUS;

     RASAMB = record
          dwSize : DWORD;
          dwError : DWORD;
          szNetBiosError : array[0..(NETBIOS_NAME_LEN + 1)-1] of TCHAR;
          bLana : BYTE;
       end;

     _RASAMB = RASAMB;

     RASCONN = record
          dwSize : DWORD;
          hrasconn : HRASCONN;
          szEntryName : array[0..(RAS_MaxEntryName + 1)-1] of TCHAR;
          szDeviceType : array[0..(RAS_MaxDeviceType + 1)-1] of CHAR;
          szDeviceName : array[0..(RAS_MaxDeviceName + 1)-1] of CHAR;
       end;

     _RASCONN = RASCONN;

     RASCONNSTATUS = record
          dwSize : DWORD;
          rasconnstate : RASCONNSTATE;
          dwError : DWORD;
          szDeviceType : array[0..(RAS_MaxDeviceType + 1)-1] of TCHAR;
          szDeviceName : array[0..(RAS_MaxDeviceName + 1)-1] of TCHAR;
       end;

     _RASCONNSTATUS = RASCONNSTATUS;

     RASDIALEXTENSIONS = record
          dwSize : DWORD;
          dwfOptions : DWORD;
          hwndParent : HWND;
          reserved : DWORD;
       end;

     _RASDIALEXTENSIONS = RASDIALEXTENSIONS;

     RASDIALPARAMS = record
          dwSize : DWORD;
          szEntryName : array[0..(RAS_MaxEntryName + 1)-1] of TCHAR;
          szPhoneNumber : array[0..(RAS_MaxPhoneNumber + 1)-1] of TCHAR;
          szCallbackNumber : array[0..(RAS_MaxCallbackNumber + 1)-1] of TCHAR;
          szUserName : array[0..(UNLEN + 1)-1] of TCHAR;
          szPassword : array[0..(PWLEN + 1)-1] of TCHAR;
          szDomain : array[0..(DNLEN + 1)-1] of TCHAR;
       end;

     _RASDIALPARAMS = RASDIALPARAMS;

     RASENTRYNAME = record
          dwSize : DWORD;
          szEntryName : array[0..(RAS_MaxEntryName + 1)-1] of TCHAR;
       end;

     _RASENTRYNAME = RASENTRYNAME;

     RASPPPIP = record
          dwSize : DWORD;
          dwError : DWORD;
          szIpAddress : array[0..(RAS_MaxIpAddress + 1)-1] of TCHAR;
       end;

     _RASPPPIP = RASPPPIP;

     RASPPPIPX = record
          dwSize : DWORD;
          dwError : DWORD;
          szIpxAddress : array[0..(RAS_MaxIpxAddress + 1)-1] of TCHAR;
       end;

     _RASPPPIPX = RASPPPIPX;

     RASPPPNBF = record
          dwSize : DWORD;
          dwError : DWORD;
          dwNetBiosError : DWORD;
          szNetBiosError : array[0..(NETBIOS_NAME_LEN + 1)-1] of TCHAR;
          szWorkstationName : array[0..(NETBIOS_NAME_LEN + 1)-1] of TCHAR;
          bLana : BYTE;
       end;

     _RASPPPNBF = RASPPPNBF;

     RASTERIZER_STATUS = record
          nSize : integer;
          wFlags : integer;
          nLanguageID : integer;
       end;

     LPRASTERIZER_STATUS = ^RASTERIZER_STATUS;

     _RASTERIZER_STATUS = RASTERIZER_STATUS;

     REASSIGN_BLOCKS = record
          Reserved : WORD;
          Count : WORD;
          BlockNumber : array[0..0] of DWORD;
       end;

     _REASSIGN_BLOCKS = REASSIGN_BLOCKS;

     REMOTE_NAME_INFO = record
          lpUniversalName : LPTSTR;
          lpConnectionName : LPTSTR;
          lpRemainingPath : LPTSTR;
       end;

     _REMOTE_NAME_INFO = REMOTE_NAME_INFO;
  (*
   TODO: OLE
  typedef struct _reobject {
    DWORD  cbStruct;
    LONG   cp;
    CLSID  clsid;
    LPOLEOBJECT      poleobj;
    LPSTORAGE        pstg;
    LPOLECLIENTSITE  polesite;
    SIZEL  sizel;
    DWORD  dvaspect;
    DWORD  dwFlags;
    DWORD  dwUser;
  } REOBJECT;
   *)

     REPASTESPECIAL = record
          dwAspect : DWORD;
          dwParam : DWORD;
       end;

     _repastespecial = REPASTESPECIAL;

     REQRESIZE = record
          nmhdr : NMHDR;
          rc : RECT;
       end;

     _reqresize = REQRESIZE;

     RGNDATAHEADER = record
          dwSize : DWORD;
          iType : DWORD;
          nCount : DWORD;
          nRgnSize : DWORD;
          rcBound : RECT;
       end;

     _RGNDATAHEADER = RGNDATAHEADER;

     RGNDATA = record
          rdh : RGNDATAHEADER;
          Buffer : array[0..0] of char;
       end;

     LPRGNDATA = ^RGNDATA;

     _RGNDATA = RGNDATA;

     SCROLLINFO = record
          cbSize : UINT;
          fMask : UINT;
          nMin : longint;
          nMax : longint;
          nPage : UINT;
          nPos : longint;
          nTrackPos : longint;
       end;

     LPSCROLLINFO = ^SCROLLINFO;

     tagSCROLLINFO = SCROLLINFO;
(* Const before declarator ignored *)

     LPCSCROLLINFO = ^SCROLLINFO;

     SECURITY_ATTRIBUTES = record
          nLength : DWORD;
          lpSecurityDescriptor : LPVOID;
          bInheritHandle : WINBOOL;
       end;

     LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;

     _SECURITY_ATTRIBUTES = SECURITY_ATTRIBUTES;

     SECURITY_INFORMATION = DWORD;

     PSECURITY_INFORMATION = ^SECURITY_INFORMATION;

     SELCHANGE = record
          nmhdr : NMHDR;
          chrg : CHARRANGE;
          seltyp : WORD;
       end;

     _selchange = SELCHANGE;

     SERIALKEYS = record
          cbSize : DWORD;
          dwFlags : DWORD;
          lpszActivePort : LPSTR;
          lpszPort : LPSTR;
          iBaudRate : DWORD;
          iPortState : DWORD;
       end;

     LPSERIALKEYS = ^SERIALKEYS;

     tagSERIALKEYS = SERIALKEYS;

     SERVICE_TABLE_ENTRY = record
          lpServiceName : LPTSTR;
          lpServiceProc : LPSERVICE_MAIN_FUNCTION;
       end;

     LPSERVICE_TABLE_ENTRY = ^SERVICE_TABLE_ENTRY;

     _SERVICE_TABLE_ENTRY = SERVICE_TABLE_ENTRY;

     SERVICE_TYPE_VALUE_ABS = record
          dwNameSpace : DWORD;
          dwValueType : DWORD;
          dwValueSize : DWORD;
          lpValueName : LPTSTR;
          lpValue : PVOID;
       end;

     _SERVICE_TYPE_VALUE_ABS = SERVICE_TYPE_VALUE_ABS;

     SERVICE_TYPE_INFO_ABS = record
          lpTypeName : LPTSTR;
          dwValueCount : DWORD;
          Values : array[0..0] of SERVICE_TYPE_VALUE_ABS;
       end;

     _SERVICE_TYPE_INFO_ABS = SERVICE_TYPE_INFO_ABS;

     SESSION_BUFFER = record
          lsn : UCHAR;
          state : UCHAR;
          local_name : array[0..(NCBNAMSZ)-1] of UCHAR;
          remote_name : array[0..(NCBNAMSZ)-1] of UCHAR;
          rcvs_outstanding : UCHAR;
          sends_outstanding : UCHAR;
       end;

     _SESSION_BUFFER = SESSION_BUFFER;

     SESSION_HEADER = record
          sess_name : UCHAR;
          num_sess : UCHAR;
          rcv_dg_outstanding : UCHAR;
          rcv_any_outstanding : UCHAR;
       end;

     _SESSION_HEADER = SESSION_HEADER;

     SET_PARTITION_INFORMATION = record
          PartitionType : BYTE;
       end;

     _SET_PARTITION_INFORMATION = SET_PARTITION_INFORMATION;

     SHCONTF = (SHCONTF_FOLDERS := 32,SHCONTF_NONFOLDERS := 64,
       SHCONTF_INCLUDEHIDDEN := 128);

     tagSHCONTF = SHCONTF;

     SHFILEINFO = record
          hIcon : HICON;
          iIcon : longint;
          dwAttributes : DWORD;
          szDisplayName : array[0..(MAX_PATH)-1] of char;
          szTypeName : array[0..79] of char;
       end;

     _SHFILEINFO = SHFILEINFO;

     FILEOP_FLAGS = WORD;

     SHFILEOPSTRUCT = record
          hwnd : HWND;
          wFunc : UINT;
          pFrom : LPCSTR;
          pTo : LPCSTR;
          fFlags : FILEOP_FLAGS;
          fAnyOperationsAborted : WINBOOL;
          hNameMappings : LPVOID;
          lpszProgressTitle : LPCSTR;
       end;

     LPSHFILEOPSTRUCT = ^SHFILEOPSTRUCT;

     _SHFILEOPSTRUCT = SHFILEOPSTRUCT;

     SHGNO = (SHGDN_NORMAL := 0,SHGDN_INFOLDER := 1,
       SHGDN_FORPARSING := $8000);

     tagSHGDN = SHGNO;

     SHNAMEMAPPING = record
          pszOldPath : LPSTR;
          pszNewPath : LPSTR;
          cchOldPath : longint;
          cchNewPath : longint;
       end;

     LPSHNAMEMAPPING = ^SHNAMEMAPPING;

     _SHNAMEMAPPING = SHNAMEMAPPING;

     SID_AND_ATTRIBUTES = record
          Sid : PSID;
          Attributes : DWORD;
       end;

     _SID_AND_ATTRIBUTES = SID_AND_ATTRIBUTES;

     SID_AND_ATTRIBUTES_ARRAY = array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;

     PSID_AND_ATTRIBUTES_ARRAY = ^SID_AND_ATTRIBUTES_ARRAY;

     SINGLE_LIST_ENTRY = record
          Next : ^_SINGLE_LIST_ENTRY;
       end;

     _SINGLE_LIST_ENTRY = SINGLE_LIST_ENTRY;

     SOUNDSENTRY = record
          cbSize : UINT;
          dwFlags : DWORD;
          iFSTextEffect : DWORD;
          iFSTextEffectMSec : DWORD;
          iFSTextEffectColorBits : DWORD;
          iFSGrafEffect : DWORD;
          iFSGrafEffectMSec : DWORD;
          iFSGrafEffectColor : DWORD;
          iWindowsEffect : DWORD;
          iWindowsEffectMSec : DWORD;
          lpszWindowsEffectDLL : LPTSTR;
          iWindowsEffectOrdinal : DWORD;
       end;

     LPSOUNDSENTRY = ^SOUNDSENTRY;

     tagSOUNDSENTRY = SOUNDSENTRY;

     STARTUPINFO = record
          cb : DWORD;
          lpReserved : LPTSTR;
          lpDesktop : LPTSTR;
          lpTitle : LPTSTR;
          dwX : DWORD;
          dwY : DWORD;
          dwXSize : DWORD;
          dwYSize : DWORD;
          dwXCountChars : DWORD;
          dwYCountChars : DWORD;
          dwFillAttribute : DWORD;
          dwFlags : DWORD;
          wShowWindow : WORD;
          cbReserved2 : WORD;
          lpReserved2 : LPBYTE;
          hStdInput : HANDLE;
          hStdOutput : HANDLE;
          hStdError : HANDLE;
       end;

     LPSTARTUPINFO = ^STARTUPINFO;

     _STARTUPINFO = STARTUPINFO;

     STICKYKEYS = record
          cbSize : DWORD;
          dwFlags : DWORD;
       end;

     LPSTICKYKEYS = ^STICKYKEYS;

     tagSTICKYKEYS = STICKYKEYS;

     STRRET = record
          uType : UINT;
          DUMMYUNIONNAME : record
              case longint of
                 0 : ( pOleStr : LPWSTR );
                 1 : ( uOffset : UINT );
                 2 : ( cStr : array[0..(MAX_PATH)-1] of char );
              end;
       end;

     LPSTRRET = ^STRRET;

     _STRRET = STRRET;

     STYLEBUF = record
          dwStyle : DWORD;
          szDescription : array[0..31] of CHAR;
       end;

     LPSTYLEBUF = ^STYLEBUF;

     _tagSTYLEBUF = STYLEBUF;

     STYLESTRUCT = record
          styleOld : DWORD;
          styleNew : DWORD;
       end;

     LPSTYLESTRUCT = ^STYLESTRUCT;

     tagSTYLESTRUCT = STYLESTRUCT;

     SYSTEM_AUDIT_ACE = record
          Header : ACE_HEADER;
          Mask : ACCESS_MASK;
          SidStart : DWORD;
       end;

     _SYSTEM_AUDIT_ACE = SYSTEM_AUDIT_ACE;

     SYSTEM_INFO = record
          u : record
              case longint of
                 0 : ( dwOemId : DWORD );
                 1 : ( s : record
                      wProcessorArchitecture : WORD;
                      wReserved : WORD;
                   end );
              end;
          dwPageSize : DWORD;
          lpMinimumApplicationAddress : LPVOID;
          lpMaximumApplicationAddress : LPVOID;
          dwActiveProcessorMask : DWORD;
          dwNumberOfProcessors : DWORD;
          dwProcessorType : DWORD;
          dwAllocationGranularity : DWORD;
          wProcessorLevel : WORD;
          wProcessorRevision : WORD;
       end;

     LPSYSTEM_INFO = ^SYSTEM_INFO;

     _SYSTEM_INFO = SYSTEM_INFO;

     SYSTEM_POWER_STATUS = record
          ACLineStatus : BYTE;
          BatteryFlag : BYTE;
          BatteryLifePercent : BYTE;
          Reserved1 : BYTE;
          BatteryLifeTime : DWORD;
          BatteryFullLifeTime : DWORD;
       end;

     _SYSTEM_POWER_STATUS = SYSTEM_POWER_STATUS;

     LPSYSTEM_POWER_STATUS = ^emptyrecord;

     TAPE_ERASE = record
          _Type : ULONG;
       end;

     _TAPE_ERASE = TAPE_ERASE;

     TAPE_GET_DRIVE_PARAMETERS = record
          ECC : BOOLEAN;
          Compression : BOOLEAN;
          DataPadding : BOOLEAN;
          ReportSetmarks : BOOLEAN;
          DefaultBlockSize : ULONG;
          MaximumBlockSize : ULONG;
          MinimumBlockSize : ULONG;
          MaximumPartitionCount : ULONG;
          FeaturesLow : ULONG;
          FeaturesHigh : ULONG;
          EOTWarningZoneSize : ULONG;
       end;

     _TAPE_GET_DRIVE_PARAMETERS = TAPE_GET_DRIVE_PARAMETERS;

     TAPE_GET_MEDIA_PARAMETERS = record
          Capacity : LARGE_INTEGER;
          Remaining : LARGE_INTEGER;
          BlockSize : DWORD;
          PartitionCount : DWORD;
          WriteProtected : BOOLEAN;
       end;

     _TAPE_GET_MEDIA_PARAMETERS = TAPE_GET_MEDIA_PARAMETERS;

     TAPE_GET_POSITION = record
          _Type : ULONG;
          Partition : ULONG;
          OffsetLow : ULONG;
          OffsetHigh : ULONG;
       end;

     _TAPE_GET_POSITION = TAPE_GET_POSITION;

     TAPE_PREPARE = record
          Operation : ULONG;
       end;

     _TAPE_PREPARE = TAPE_PREPARE;

     TAPE_SET_DRIVE_PARAMETERS = record
          ECC : BOOLEAN;
          Compression : BOOLEAN;
          DataPadding : BOOLEAN;
          ReportSetmarks : BOOLEAN;
          EOTWarningZoneSize : ULONG;
       end;

     _TAPE_SET_DRIVE_PARAMETERS = TAPE_SET_DRIVE_PARAMETERS;

     TAPE_SET_MEDIA_PARAMETERS = record
          BlockSize : ULONG;
       end;

     _TAPE_SET_MEDIA_PARAMETERS = TAPE_SET_MEDIA_PARAMETERS;

     TAPE_SET_POSITION = record
          Method : ULONG;
          Partition : ULONG;
          OffsetLow : ULONG;
          OffsetHigh : ULONG;
       end;

     _TAPE_SET_POSITION = TAPE_SET_POSITION;

     TAPE_WRITE_MARKS = record
          _Type : ULONG;
          Count : ULONG;
       end;

     _TAPE_WRITE_MARKS = TAPE_WRITE_MARKS;

     TBADDBITMAP = record
          hInst : HINST;
          nID : UINT;
       end;

     LPTBADDBITMAP = ^TBADDBITMAP;

     TBBUTTON = record
          iBitmap : longint;
          idCommand : longint;
          fsState : BYTE;
          fsStyle : BYTE;
          dwData : DWORD;
          iString : longint;
       end;

     PTBBUTTON = ^TBBUTTON;

     LPTBBUTTON = ^TBBUTTON;

     _TBBUTTON = TBBUTTON;
(* Const before type ignored *)

     LPCTBBUTTON = ^TBBUTTON;

     TBNOTIFY = record
          hdr : NMHDR;
          iItem : longint;
          tbButton : TBBUTTON;
          cchText : longint;
          pszText : LPTSTR;
       end;

     LPTBNOTIFY = ^TBNOTIFY;

     TBSAVEPARAMS = record
          hkr : HKEY;
          pszSubKey : LPCTSTR;
          pszValueName : LPCTSTR;
       end;

     TC_HITTESTINFO = record
          pt : POINT;
          flags : UINT;
       end;

     _TC_HITTESTINFO = TC_HITTESTINFO;

     TC_ITEM = record
          mask : UINT;
          lpReserved1 : UINT;
          lpReserved2 : UINT;
          pszText : LPTSTR;
          cchTextMax : longint;
          iImage : longint;
          lParam : LPARAM;
       end;

     _TC_ITEM = TC_ITEM;

     TC_ITEMHEADER = record
          mask : UINT;
          lpReserved1 : UINT;
          lpReserved2 : UINT;
          pszText : LPTSTR;
          cchTextMax : longint;
          iImage : longint;
       end;

     _TC_ITEMHEADER = TC_ITEMHEADER;

     TC_KEYDOWN = record
          hdr : NMHDR;
          wVKey : WORD;
          flags : UINT;
       end;

     _TC_KEYDOWN = TC_KEYDOWN;

     TEXTRANGE = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
       end;

     _textrange = TEXTRANGE;

     TIME_ZONE_INFORMATION = record
          Bias : LONG;
          StandardName : array[0..31] of WCHAR;
          StandardDate : SYSTEMTIME;
          StandardBias : LONG;
          DaylightName : array[0..31] of WCHAR;
          DaylightDate : SYSTEMTIME;
          DaylightBias : LONG;
       end;

     LPTIME_ZONE_INFORMATION = ^TIME_ZONE_INFORMATION;

     _TIME_ZONE_INFORMATION = TIME_ZONE_INFORMATION;

     TOGGLEKEYS = record
          cbSize : DWORD;
          dwFlags : DWORD;
       end;

     tagTOGGLEKEYS = TOGGLEKEYS;

     TOKEN_SOURCE = record
          SourceName : array[0..7] of CHAR;
          SourceIdentifier : LUID;
       end;

     _TOKEN_SOURCE = TOKEN_SOURCE;

     TOKEN_CONTROL = record
          TokenId : LUID;
          AuthenticationId : LUID;
          ModifiedId : LUID;
          TokenSource : TOKEN_SOURCE;
       end;

     _TOKEN_CONTROL = TOKEN_CONTROL;

     TOKEN_DEFAULT_DACL = record
          DefaultDacl : PACL;
       end;

     _TOKEN_DEFAULT_DACL = TOKEN_DEFAULT_DACL;

     TOKEN_GROUPS = record
          GroupCount : DWORD;
          Groups : array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;
       end;

     PTOKEN_GROUPS = ^TOKEN_GROUPS;

     LPTOKEN_GROUPS = ^TOKEN_GROUPS;

     _TOKEN_GROUPS = TOKEN_GROUPS;

     TOKEN_OWNER = record
          Owner : PSID;
       end;

     _TOKEN_OWNER = TOKEN_OWNER;

     TOKEN_PRIMARY_GROUP = record
          PrimaryGroup : PSID;
       end;

     _TOKEN_PRIMARY_GROUP = TOKEN_PRIMARY_GROUP;

     TOKEN_PRIVILEGES = record
          PrivilegeCount : DWORD;
          Privileges : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
       end;

     PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;

     LPTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;

     _TOKEN_PRIVILEGES = TOKEN_PRIVILEGES;

     TOKEN_STATISTICS = record
          TokenId : LUID;
          AuthenticationId : LUID;
          ExpirationTime : LARGE_INTEGER;
          TokenType : TOKEN_TYPE;
          ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
          DynamicCharged : DWORD;
          DynamicAvailable : DWORD;
          GroupCount : DWORD;
          PrivilegeCount : DWORD;
          ModifiedId : LUID;
       end;

     _TOKEN_STATISTICS = TOKEN_STATISTICS;

     TOKEN_USER = record
          User : SID_AND_ATTRIBUTES;
       end;

     _TOKEN_USER = TOKEN_USER;

     TOOLINFO = record
          cbSize : UINT;
          uFlags : UINT;
          hwnd : HWND;
          uId : UINT;
          rect : RECT;
          hinst : HINST;
          lpszText : LPTSTR;
       end;

     PTOOLINFO = ^TOOLINFO;

     LPTOOLINFO = ^TOOLINFO;

     TOOLTIPTEXT = record
          hdr : NMHDR;
          lpszText : LPTSTR;
          szText : array[0..79] of char;
          hinst : HINST;
          uFlags : UINT;
       end;

     LPTOOLTIPTEXT = ^TOOLTIPTEXT;

     TPMPARAMS = record
          cbSize : UINT;
          rcExclude : RECT;
       end;

     LPTPMPARAMS = ^TPMPARAMS;

     tagTPMPARAMS = TPMPARAMS;

     TRANSMIT_FILE_BUFFERS = record
          Head : PVOID;
          HeadLength : DWORD;
          Tail : PVOID;
          TailLength : DWORD;
       end;

     _TRANSMIT_FILE_BUFFERS = TRANSMIT_FILE_BUFFERS;

     TTHITTESTINFO = record
          hwnd : HWND;
          pt : POINT;
          ti : TOOLINFO;
       end;

     LPHITTESTINFO = ^TTHITTESTINFO;

     _TT_HITTESTINFO = TTHITTESTINFO;

     TTPOLYCURVE = record
          wType : WORD;
          cpfx : WORD;
          apfx : array[0..0] of POINTFX;
       end;

     LPTTPOLYCURVE = ^TTPOLYCURVE;

     tagTTPOLYCURVE = TTPOLYCURVE;

     TTPOLYGONHEADER = record
          cb : DWORD;
          dwType : DWORD;
          pfxStart : POINTFX;
       end;

     LPTTPOLYGONHEADER = ^TTPOLYGONHEADER;

     _TTPOLYGONHEADER = TTPOLYGONHEADER;

     TV_DISPINFO = record
          hdr : NMHDR;
          item : TV_ITEM;
       end;

     _TV_DISPINFO = TV_DISPINFO;

     TV_HITTESTINFO = record
          pt : POINT;
          flags : UINT;
          hItem : HTREEITEM;
       end;

     LPTV_HITTESTINFO = ^TV_HITTESTINFO;

     _TVHITTESTINFO = TV_HITTESTINFO;

     TV_INSERTSTRUCT = record
          hParent : HTREEITEM;
          hInsertAfter : HTREEITEM;
          item : TV_ITEM;
       end;

     LPTV_INSERTSTRUCT = ^TV_INSERTSTRUCT;

     _TV_INSERTSTRUCT = TV_INSERTSTRUCT;

     TV_KEYDOWN = record
          hdr : NMHDR;
          wVKey : WORD;
          flags : UINT;
       end;

     _TV_KEYDOWN = TV_KEYDOWN;

     TV_SORTCB = record
          hParent : HTREEITEM;
          lpfnCompare : PFNTVCOMPARE;
          lParam : LPARAM;
       end;

     LPTV_SORTCB = ^TV_SORTCB;

     _TV_SORTCB = TV_SORTCB;

     UDACCEL = record
          nSec : UINT;
          nInc : UINT;
       end;

     ULARGE_INTEGER = record
          LowPart : DWORD;
          HighPart : DWORD;
       end;

     PULARGE_INTEGER = ^ULARGE_INTEGER;

     _ULARGE_INTEGER = ULARGE_INTEGER;

     UNIVERSAL_NAME_INFO = record
          lpUniversalName : LPTSTR;
       end;

     _UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFO;

     USEROBJECTFLAGS = record
          fInherit : WINBOOL;
          fReserved : WINBOOL;
          dwFlags : DWORD;
       end;

     tagUSEROBJECTFLAGS = USEROBJECTFLAGS;

     VALENT = record
          ve_valuename : LPTSTR;
          ve_valuelen : DWORD;
          ve_valueptr : DWORD;
          ve_type : DWORD;
       end;

     PVALENT = ^VALENT;

     value_ent = VALENT;

     VERIFY_INFORMATION = record
          StartingOffset : LARGE_INTEGER;
          Length : DWORD;
       end;

     _VERIFY_INFORMATION = VERIFY_INFORMATION;

     VS_FIXEDFILEINFO = record
          dwSignature : DWORD;
          dwStrucVersion : DWORD;
          dwFileVersionMS : DWORD;
          dwFileVersionLS : DWORD;
          dwProductVersionMS : DWORD;
          dwProductVersionLS : DWORD;
          dwFileFlagsMask : DWORD;
          dwFileFlags : DWORD;
          dwFileOS : DWORD;
          dwFileType : DWORD;
          dwFileSubtype : DWORD;
          dwFileDateMS : DWORD;
          dwFileDateLS : DWORD;
       end;

     _VS_FIXEDFILEINFO = VS_FIXEDFILEINFO;

     WIN32_FIND_DATA = record
          dwFileAttributes : DWORD;
          ftCreationTime : FILETIME;
          ftLastAccessTime : FILETIME;
          ftLastWriteTime : FILETIME;
          nFileSizeHigh : DWORD;
          nFileSizeLow : DWORD;
          dwReserved0 : DWORD;
          dwReserved1 : DWORD;
          cFileName : array[0..(MAX_PATH)-1] of TCHAR;
          cAlternateFileName : array[0..13] of TCHAR;
       end;

     LPWIN32_FIND_DATA = ^WIN32_FIND_DATA;

     PWIN32_FIND_DATA = ^WIN32_FIND_DATA;

     _WIN32_FIND_DATA = WIN32_FIND_DATA;

     WIN32_STREAM_ID = record
          dwStreamId : DWORD;
          dwStreamAttributes : DWORD;
          Size : LARGE_INTEGER;
          dwStreamNameSize : DWORD;
          cStreamName : ^WCHAR;
       end;

     _WIN32_STREAM_ID = WIN32_STREAM_ID;

     WINDOWPLACEMENT = record
          length : UINT;
          flags : UINT;
          showCmd : UINT;
          ptMinPosition : POINT;
          ptMaxPosition : POINT;
          rcNormalPosition : RECT;
       end;

     _WINDOWPLACEMENT = WINDOWPLACEMENT;

     WNDCLASS = record
          style : UINT;
          lpfnWndProc : WNDPROC;
          cbClsExtra : longint;
          cbWndExtra : longint;
          hInstance : HANDLE;
          hIcon : HICON;
          hCursor : HCURSOR;
          hbrBackground : HBRUSH;
          lpszMenuName : LPCTSTR;
          lpszClassName : LPCTSTR;
       end;

     LPWNDCLASS = ^WNDCLASS;

     _WNDCLASS = WNDCLASS;

     WNDCLASSEX = record
          cbSize : UINT;
          style : UINT;
          lpfnWndProc : WNDPROC;
          cbClsExtra : longint;
          cbWndExtra : longint;
          hInstance : HANDLE;
          _hIcon : HICON;
          hCursor : HCURSOR;
          hbrBackground : HBRUSH;
          lpszMenuName : LPCTSTR;
          lpszClassName : LPCTSTR;
          hIconSm : HICON;
       end;

     LPWNDCLASSEX = ^WNDCLASSEX;

     _WNDCLASSEX = WNDCLASSEX;

     CONNECTDLGSTRUCT = record
          cbStructure : DWORD;
          hwndOwner : HWND;
          lpConnRes : LPNETRESOURCE;
          dwFlags : DWORD;
          dwDevNum : DWORD;
       end;

     LPCONNECTDLGSTRUCT = ^CONNECTDLGSTRUCT;

     _CONNECTDLGSTRUCT = CONNECTDLGSTRUCT;

     DISCDLGSTRUCT = record
          cbStructure : DWORD;
          hwndOwner : HWND;
          lpLocalName : LPTSTR;
          lpRemoteName : LPTSTR;
          dwFlags : DWORD;
       end;

     LPDISCDLGSTRUCT = ^DISCDLGSTRUCT;

     _DISCDLGSTRUCT = DISCDLGSTRUCT;

     NETINFOSTRUCT = record
          cbStructure : DWORD;
          dwProviderVersion : DWORD;
          dwStatus : DWORD;
          dwCharacteristics : DWORD;
          dwHandle : DWORD;
          wNetType : WORD;
          dwPrinters : DWORD;
          dwDrives : DWORD;
       end;

     LPNETINFOSTRUCT = ^NETINFOSTRUCT;

     _NETINFOSTRUCT = NETINFOSTRUCT;

     NETCONNECTINFOSTRUCT = record
          cbStructure : DWORD;
          dwFlags : DWORD;
          dwSpeed : DWORD;
          dwDelay : DWORD;
          dwOptDataSize : DWORD;
       end;

     LPNETCONNECTINFOSTRUCT = ^NETCONNECTINFOSTRUCT;

     _NETCONNECTINFOSTRUCT = NETCONNECTINFOSTRUCT;

     ENUMMETAFILEPROC = function (_para1:HDC; _para2:HANDLETABLE; _para3:METARECORD; _para4:longint; _para5:LPARAM):longint;

     ENHMETAFILEPROC = function (_para1:HDC; _para2:HANDLETABLE; _para3:ENHMETARECORD; _para4:longint; _para5:LPARAM):longint;

     ENUMFONTSPROC = function (_para1:LPLOGFONT; _para2:LPTEXTMETRIC; _para3:DWORD; _para4:LPARAM):longint;

     FONTENUMPROC = function (var _para1:ENUMLOGFONT; var _para2:NEWTEXTMETRIC; _para3:longint; _para4:LPARAM):longint;

     FONTENUMEXPROC = function (var _para1:ENUMLOGFONTEX;var _para2:NEWTEXTMETRICEX; _para3:longint; _para4:LPARAM):longint;

     LPOVERLAPPED_COMPLETION_ROUTINE = procedure (_para1:DWORD; _para2:DWORD; _para3:LPOVERLAPPED);
  {
    Structures for the extensions to OpenGL
     }

     POINTFLOAT = record
          x : FLOAT;
          y : FLOAT;
       end;

     PPOINTFLOAT = ^POINTFLOAT;

     _POINTFLOAT = POINTFLOAT;

     GLYPHMETRICSFLOAT = record
          gmfBlackBoxX : FLOAT;
          gmfBlackBoxY : FLOAT;
          gmfptGlyphOrigin : POINTFLOAT;
          gmfCellIncX : FLOAT;
          gmfCellIncY : FLOAT;
       end;

     PGLYPHMETRICSFLOAT = ^GLYPHMETRICSFLOAT;

     LPGLYPHMETRICSFLOAT = ^GLYPHMETRICSFLOAT;

     _GLYPHMETRICSFLOAT = GLYPHMETRICSFLOAT;

     LAYERPLANEDESCRIPTOR = record
          nSize : WORD;
          nVersion : WORD;
          dwFlags : DWORD;
          iPixelType : BYTE;
          cColorBits : BYTE;
          cRedBits : BYTE;
          cRedShift : BYTE;
          cGreenBits : BYTE;
          cGreenShift : BYTE;
          cBlueBits : BYTE;
          cBlueShift : BYTE;
          cAlphaBits : BYTE;
          cAlphaShift : BYTE;
          cAccumBits : BYTE;
          cAccumRedBits : BYTE;
          cAccumGreenBits : BYTE;
          cAccumBlueBits : BYTE;
          cAccumAlphaBits : BYTE;
          cDepthBits : BYTE;
          cStencilBits : BYTE;
          cAuxBuffers : BYTE;
          iLayerPlane : BYTE;
          bReserved : BYTE;
          crTransparent : COLORREF;
       end;

     PLAYERPLANEDESCRIPTOR = ^LAYERPLANEDESCRIPTOR;

     LPLAYERPLANEDESCRIPTOR = ^LAYERPLANEDESCRIPTOR;

     tagLAYERPLANEDESCRIPTOR = LAYERPLANEDESCRIPTOR;

     PIXELFORMATDESCRIPTOR = record
          nSize : WORD;
          nVersion : WORD;
          dwFlags : DWORD;
          iPixelType : BYTE;
          cColorBits : BYTE;
          cRedBits : BYTE;
          cRedShift : BYTE;
          cGreenBits : BYTE;
          cGreenShift : BYTE;
          cBlueBits : BYTE;
          cBlueShift : BYTE;
          cAlphaBits : BYTE;
          cAlphaShift : BYTE;
          cAccumBits : BYTE;
          cAccumRedBits : BYTE;
          cAccumGreenBits : BYTE;
          cAccumBlueBits : BYTE;
          cAccumAlphaBits : BYTE;
          cDepthBits : BYTE;
          cStencilBits : BYTE;
          cAuxBuffers : BYTE;
          iLayerType : BYTE;
          bReserved : BYTE;
          dwLayerMask : DWORD;
          dwVisibleMask : DWORD;
          dwDamageMask : DWORD;
       end;

     PPIXELFORMATDESCRIPTOR = ^PIXELFORMATDESCRIPTOR;

     LPPIXELFORMATDESCRIPTOR = ^PIXELFORMATDESCRIPTOR;

     tagPIXELFORMATDESCRIPTOR = PIXELFORMATDESCRIPTOR;

     USER_INFO_2 = record
          usri2_name : LPWSTR;
          usri2_password : LPWSTR;
          usri2_password_age : DWORD;
          usri2_priv : DWORD;
          usri2_home_dir : LPWSTR;
          usri2_comment : LPWSTR;
          usri2_flags : DWORD;
          usri2_script_path : LPWSTR;
          usri2_auth_flags : DWORD;
          usri2_full_name : LPWSTR;
          usri2_usr_comment : LPWSTR;
          usri2_parms : LPWSTR;
          usri2_workstations : LPWSTR;
          usri2_last_logon : DWORD;
          usri2_last_logoff : DWORD;
          usri2_acct_expires : DWORD;
          usri2_max_storage : DWORD;
          usri2_units_per_week : DWORD;
          usri2_logon_hours : PBYTE;
          usri2_bad_pw_count : DWORD;
          usri2_num_logons : DWORD;
          usri2_logon_server : LPWSTR;
          usri2_country_code : DWORD;
          usri2_code_page : DWORD;
       end;

     PUSER_INFO_2 = ^USER_INFO_2;

     LPUSER_INFO_2 = ^USER_INFO_2;

     USER_INFO_0 = record
          usri0_name : LPWSTR;
       end;

     PUSER_INFO_0 = ^USER_INFO_0;

     LPUSER_INFO_0 = ^USER_INFO_0;

     USER_INFO_3 = record
          usri3_name : LPWSTR;
          usri3_password : LPWSTR;
          usri3_password_age : DWORD;
          usri3_priv : DWORD;
          usri3_home_dir : LPWSTR;
          usri3_comment : LPWSTR;
          usri3_flags : DWORD;
          usri3_script_path : LPWSTR;
          usri3_auth_flags : DWORD;
          usri3_full_name : LPWSTR;
          usri3_usr_comment : LPWSTR;
          usri3_parms : LPWSTR;
          usri3_workstations : LPWSTR;
          usri3_last_logon : DWORD;
          usri3_last_logoff : DWORD;
          usri3_acct_expires : DWORD;
          usri3_max_storage : DWORD;
          usri3_units_per_week : DWORD;
          usri3_logon_hours : PBYTE;
          usri3_bad_pw_count : DWORD;
          usri3_num_logons : DWORD;
          usri3_logon_server : LPWSTR;
          usri3_country_code : DWORD;
          usri3_code_page : DWORD;
          usri3_user_id : DWORD;
          usri3_primary_group_id : DWORD;
          usri3_profile : LPWSTR;
          usri3_home_dir_drive : LPWSTR;
          usri3_password_expired : DWORD;
       end;

     PUSER_INFO_3 = ^USER_INFO_3;

     LPUSER_INFO_3 = ^USER_INFO_3;

     GROUP_INFO_2 = record
          grpi2_name : LPWSTR;
          grpi2_comment : LPWSTR;
          grpi2_group_id : DWORD;
          grpi2_attributes : DWORD;
       end;

     PGROUP_INFO_2 = ^GROUP_INFO_2;

     LOCALGROUP_INFO_0 = record
          lgrpi0_name : LPWSTR;
       end;

     PLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;

     LPLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;
  { PE executable header.   }
  { Magic number, 0x5a4d  }
  { Bytes on last page of file, 0x90  }
  { Pages in file, 0x3  }
  { Relocations, 0x0  }
  { Size of header in paragraphs, 0x4  }
  { Minimum extra paragraphs needed, 0x0  }
  { Maximum extra paragraphs needed, 0xFFFF  }
  { Initial (relative) SS value, 0x0  }
  { Initial SP value, 0xb8  }
  { Checksum, 0x0  }
  { Initial IP value, 0x0  }
  { Initial (relative) CS value, 0x0  }
  { File address of relocation table, 0x40  }
  { Overlay number, 0x0  }
  { Reserved words, all 0x0  }
  { OEM identifier (for e_oeminfo), 0x0  }
  { OEM information; e_oemid specific, 0x0  }
  { Reserved words, all 0x0  }
  { File address of new exe header, 0x80  }
  { We leave out the next two fields, since they aren't in the
       Windows header file.   }
  { DWORD dos_message[16];   text which always follows dos header  }
  { DWORD nt_signature;      required NT signature, 0x4550  }

     IMAGE_DOS_HEADER = record
          e_magic : WORD;
          e_cblp : WORD;
          e_cp : WORD;
          e_crlc : WORD;
          e_cparhdr : WORD;
          e_minalloc : WORD;
          e_maxalloc : WORD;
          e_ss : WORD;
          e_sp : WORD;
          e_csum : WORD;
          e_ip : WORD;
          e_cs : WORD;
          e_lfarlc : WORD;
          e_ovno : WORD;
          e_res : array[0..3] of WORD;
          e_oemid : WORD;
          e_oeminfo : WORD;
          e_res2 : array[0..9] of WORD;
          e_lfanew : LONG;
       end;

     PIMAGE_DOS_HEADER = ^IMAGE_DOS_HEADER;
{ C++ end of extern C conditionnal removed }
  { __cplusplus  }
{$endif}
  { _GNU_H_WINDOWS32_STRUCTURES  }


{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function fBinary(var a : DCB) : DWORD;
    begin
       fBinary:=(a.flag0 and bm_DCB_fBinary) shr bp_DCB_fBinary;
    end;

  procedure set_fBinary(var a : DCB; __fBinary : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fBinary shl bp_DCB_fBinary) and bm_DCB_fBinary);
    end;

  function fParity(var a : DCB) : DWORD;
    begin
       fParity:=(a.flag0 and bm_DCB_fParity) shr bp_DCB_fParity;
    end;

  procedure set_fParity(var a : DCB; __fParity : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fParity shl bp_DCB_fParity) and bm_DCB_fParity);
    end;

  function fOutxCtsFlow(var a : DCB) : DWORD;
    begin
       fOutxCtsFlow:=(a.flag0 and bm_DCB_fOutxCtsFlow) shr bp_DCB_fOutxCtsFlow;
    end;

  procedure set_fOutxCtsFlow(var a : DCB; __fOutxCtsFlow : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fOutxCtsFlow shl bp_DCB_fOutxCtsFlow) and bm_DCB_fOutxCtsFlow);
    end;

  function fOutxDsrFlow(var a : DCB) : DWORD;
    begin
       fOutxDsrFlow:=(a.flag0 and bm_DCB_fOutxDsrFlow) shr bp_DCB_fOutxDsrFlow;
    end;

  procedure set_fOutxDsrFlow(var a : DCB; __fOutxDsrFlow : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fOutxDsrFlow shl bp_DCB_fOutxDsrFlow) and bm_DCB_fOutxDsrFlow);
    end;

  function fDtrControl(var a : DCB) : DWORD;
    begin
       fDtrControl:=(a.flag0 and bm_DCB_fDtrControl) shr bp_DCB_fDtrControl;
    end;

  procedure set_fDtrControl(var a : DCB; __fDtrControl : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fDtrControl shl bp_DCB_fDtrControl) and bm_DCB_fDtrControl);
    end;

  function fDsrSensitivity(var a : DCB) : DWORD;
    begin
       fDsrSensitivity:=(a.flag0 and bm_DCB_fDsrSensitivity) shr bp_DCB_fDsrSensitivity;
    end;

  procedure set_fDsrSensitivity(var a : DCB; __fDsrSensitivity : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fDsrSensitivity shl bp_DCB_fDsrSensitivity) and bm_DCB_fDsrSensitivity);
    end;

  function fTXContinueOnXoff(var a : DCB) : DWORD;
    begin
       fTXContinueOnXoff:=(a.flag0 and bm_DCB_fTXContinueOnXoff) shr bp_DCB_fTXContinueOnXoff;
    end;

  procedure set_fTXContinueOnXoff(var a : DCB; __fTXContinueOnXoff : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fTXContinueOnXoff shl bp_DCB_fTXContinueOnXoff) and bm_DCB_fTXContinueOnXoff);
    end;

  function fOutX(var a : DCB) : DWORD;
    begin
       fOutX:=(a.flag0 and bm_DCB_fOutX) shr bp_DCB_fOutX;
    end;

  procedure set_fOutX(var a : DCB; __fOutX : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fOutX shl bp_DCB_fOutX) and bm_DCB_fOutX);
    end;

  function fInX(var a : DCB) : DWORD;
    begin
       fInX:=(a.flag0 and bm_DCB_fInX) shr bp_DCB_fInX;
    end;

  procedure set_fInX(var a : DCB; __fInX : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fInX shl bp_DCB_fInX) and bm_DCB_fInX);
    end;

  function fErrorChar(var a : DCB) : DWORD;
    begin
       fErrorChar:=(a.flag0 and bm_DCB_fErrorChar) shr bp_DCB_fErrorChar;
    end;

  procedure set_fErrorChar(var a : DCB; __fErrorChar : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fErrorChar shl bp_DCB_fErrorChar) and bm_DCB_fErrorChar);
    end;

  function fNull(var a : DCB) : DWORD;
    begin
       fNull:=(a.flag0 and bm_DCB_fNull) shr bp_DCB_fNull;
    end;

  procedure set_fNull(var a : DCB; __fNull : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fNull shl bp_DCB_fNull) and bm_DCB_fNull);
    end;

  function fRtsControl(var a : DCB) : DWORD;
    begin
       fRtsControl:=(a.flag0 and bm_DCB_fRtsControl) shr bp_DCB_fRtsControl;
    end;

  procedure set_fRtsControl(var a : DCB; __fRtsControl : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fRtsControl shl bp_DCB_fRtsControl) and bm_DCB_fRtsControl);
    end;

  function fAbortOnError(var a : DCB) : DWORD;
    begin
       fAbortOnError:=(a.flag0 and bm_DCB_fAbortOnError) shr bp_DCB_fAbortOnError;
    end;

  procedure set_fAbortOnError(var a : DCB; __fAbortOnError : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fAbortOnError shl bp_DCB_fAbortOnError) and bm_DCB_fAbortOnError);
    end;

  function fDummy2(var a : DCB) : DWORD;
    begin
       fDummy2:=(a.flag0 and bm_DCB_fDummy2) shr bp_DCB_fDummy2;
    end;

  procedure set_fDummy2(var a : DCB; __fDummy2 : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fDummy2 shl bp_DCB_fDummy2) and bm_DCB_fDummy2);
    end;

  function fCtsHold(var a : COMSTAT) : DWORD;
    begin
       fCtsHold:=(a.flag0 and bm_COMSTAT_fCtsHold) shr bp_COMSTAT_fCtsHold;
    end;

  procedure set_fCtsHold(var a : COMSTAT; __fCtsHold : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fCtsHold shl bp_COMSTAT_fCtsHold) and bm_COMSTAT_fCtsHold);
    end;

  function fDsrHold(var a : COMSTAT) : DWORD;
    begin
       fDsrHold:=(a.flag0 and bm_COMSTAT_fDsrHold) shr bp_COMSTAT_fDsrHold;
    end;

  procedure set_fDsrHold(var a : COMSTAT; __fDsrHold : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fDsrHold shl bp_COMSTAT_fDsrHold) and bm_COMSTAT_fDsrHold);
    end;

  function fRlsdHold(var a : COMSTAT) : DWORD;
    begin
       fRlsdHold:=(a.flag0 and bm_COMSTAT_fRlsdHold) shr bp_COMSTAT_fRlsdHold;
    end;

  procedure set_fRlsdHold(var a : COMSTAT; __fRlsdHold : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fRlsdHold shl bp_COMSTAT_fRlsdHold) and bm_COMSTAT_fRlsdHold);
    end;

  function fXoffHold(var a : COMSTAT) : DWORD;
    begin
       fXoffHold:=(a.flag0 and bm_COMSTAT_fXoffHold) shr bp_COMSTAT_fXoffHold;
    end;

  procedure set_fXoffHold(var a : COMSTAT; __fXoffHold : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fXoffHold shl bp_COMSTAT_fXoffHold) and bm_COMSTAT_fXoffHold);
    end;

  function fXoffSent(var a : COMSTAT) : DWORD;
    begin
       fXoffSent:=(a.flag0 and bm_COMSTAT_fXoffSent) shr bp_COMSTAT_fXoffSent;
    end;

  procedure set_fXoffSent(var a : COMSTAT; __fXoffSent : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fXoffSent shl bp_COMSTAT_fXoffSent) and bm_COMSTAT_fXoffSent);
    end;

  function fEof(var a : COMSTAT) : DWORD;
    begin
       fEof:=(a.flag0 and bm_COMSTAT_fEof) shr bp_COMSTAT_fEof;
    end;

  procedure set_fEof(var a : COMSTAT; __fEof : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fEof shl bp_COMSTAT_fEof) and bm_COMSTAT_fEof);
    end;

  function fTxim(var a : COMSTAT) : DWORD;
    begin
       fTxim:=(a.flag0 and bm_COMSTAT_fTxim) shr bp_COMSTAT_fTxim;
    end;

  procedure set_fTxim(var a : COMSTAT; __fTxim : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fTxim shl bp_COMSTAT_fTxim) and bm_COMSTAT_fTxim);
    end;

  function fReserved(var a : COMSTAT) : DWORD;
    begin
       fReserved:=(a.flag0 and bm_COMSTAT_fReserved) shr bp_COMSTAT_fReserved;
    end;

  procedure set_fReserved(var a : COMSTAT; __fReserved : DWORD);
    begin
       a.flag0:=a.flag0 or ((__fReserved shl bp_COMSTAT_fReserved) and bm_COMSTAT_fReserved);
    end;

  function bAppReturnCode(var a : DDEACK) : word;
    begin
       bAppReturnCode:=(a.flag0 and bm_DDEACK_bAppReturnCode) shr bp_DDEACK_bAppReturnCode;
    end;

  procedure set_bAppReturnCode(var a : DDEACK; __bAppReturnCode : word);
    begin
       a.flag0:=a.flag0 or ((__bAppReturnCode shl bp_DDEACK_bAppReturnCode) and bm_DDEACK_bAppReturnCode);
    end;

  function reserved(var a : DDEACK) : word;
    begin
       reserved:=(a.flag0 and bm_DDEACK_reserved) shr bp_DDEACK_reserved;
    end;

  procedure set_reserved(var a : DDEACK; __reserved : word);
    begin
       a.flag0:=a.flag0 or ((__reserved shl bp_DDEACK_reserved) and bm_DDEACK_reserved);
    end;

  function fBusy(var a : DDEACK) : word;
    begin
       fBusy:=(a.flag0 and bm_DDEACK_fBusy) shr bp_DDEACK_fBusy;
    end;

  procedure set_fBusy(var a : DDEACK; __fBusy : word);
    begin
       a.flag0:=a.flag0 or ((__fBusy shl bp_DDEACK_fBusy) and bm_DDEACK_fBusy);
    end;

  function fAck(var a : DDEACK) : word;
    begin
       fAck:=(a.flag0 and bm_DDEACK_fAck) shr bp_DDEACK_fAck;
    end;

  procedure set_fAck(var a : DDEACK; __fAck : word);
    begin
       a.flag0:=a.flag0 or ((__fAck shl bp_DDEACK_fAck) and bm_DDEACK_fAck);
    end;

  function reserved(var a : DDEADVISE) : word;
    begin
       reserved:=(a.flag0 and bm_DDEADVISE_reserved) shr bp_DDEADVISE_reserved;
    end;

  procedure set_reserved(var a : DDEADVISE; __reserved : word);
    begin
       a.flag0:=a.flag0 or ((__reserved shl bp_DDEADVISE_reserved) and bm_DDEADVISE_reserved);
    end;

  function fDeferUpd(var a : DDEADVISE) : word;
    begin
       fDeferUpd:=(a.flag0 and bm_DDEADVISE_fDeferUpd) shr bp_DDEADVISE_fDeferUpd;
    end;

  procedure set_fDeferUpd(var a : DDEADVISE; __fDeferUpd : word);
    begin
       a.flag0:=a.flag0 or ((__fDeferUpd shl bp_DDEADVISE_fDeferUpd) and bm_DDEADVISE_fDeferUpd);
    end;

  function fAckReq(var a : DDEADVISE) : word;
    begin
       fAckReq:=(a.flag0 and bm_DDEADVISE_fAckReq) shr bp_DDEADVISE_fAckReq;
    end;

  procedure set_fAckReq(var a : DDEADVISE; __fAckReq : word);
    begin
       a.flag0:=a.flag0 or ((__fAckReq shl bp_DDEADVISE_fAckReq) and bm_DDEADVISE_fAckReq);
    end;

  function unused(var a : DDEDATA) : word;
    begin
       unused:=(a.flag0 and bm_DDEDATA_unused) shr bp_DDEDATA_unused;
    end;

  procedure set_unused(var a : DDEDATA; __unused : word);
    begin
       a.flag0:=a.flag0 or ((__unused shl bp_DDEDATA_unused) and bm_DDEDATA_unused);
    end;

  function fResponse(var a : DDEDATA) : word;
    begin
       fResponse:=(a.flag0 and bm_DDEDATA_fResponse) shr bp_DDEDATA_fResponse;
    end;

  procedure set_fResponse(var a : DDEDATA; __fResponse : word);
    begin
       a.flag0:=a.flag0 or ((__fResponse shl bp_DDEDATA_fResponse) and bm_DDEDATA_fResponse);
    end;

  function fRelease(var a : DDEDATA) : word;
    begin
       fRelease:=(a.flag0 and bm_DDEDATA_fRelease) shr bp_DDEDATA_fRelease;
    end;

  procedure set_fRelease(var a : DDEDATA; __fRelease : word);
    begin
       a.flag0:=a.flag0 or ((__fRelease shl bp_DDEDATA_fRelease) and bm_DDEDATA_fRelease);
    end;

  function reserved(var a : DDEDATA) : word;
    begin
       reserved:=(a.flag0 and bm_DDEDATA_reserved) shr bp_DDEDATA_reserved;
    end;

  procedure set_reserved(var a : DDEDATA; __reserved : word);
    begin
       a.flag0:=a.flag0 or ((__reserved shl bp_DDEDATA_reserved) and bm_DDEDATA_reserved);
    end;

  function fAckReq(var a : DDEDATA) : word;
    begin
       fAckReq:=(a.flag0 and bm_DDEDATA_fAckReq) shr bp_DDEDATA_fAckReq;
    end;

  procedure set_fAckReq(var a : DDEDATA; __fAckReq : word);
    begin
       a.flag0:=a.flag0 or ((__fAckReq shl bp_DDEDATA_fAckReq) and bm_DDEDATA_fAckReq);
    end;

  function unused(var a : DDELN) : word;
    begin
       unused:=(a.flag0 and bm_DDELN_unused) shr bp_DDELN_unused;
    end;

  procedure set_unused(var a : DDELN; __unused : word);
    begin
       a.flag0:=a.flag0 or ((__unused shl bp_DDELN_unused) and bm_DDELN_unused);
    end;

  function fRelease(var a : DDELN) : word;
    begin
       fRelease:=(a.flag0 and bm_DDELN_fRelease) shr bp_DDELN_fRelease;
    end;

  procedure set_fRelease(var a : DDELN; __fRelease : word);
    begin
       a.flag0:=a.flag0 or ((__fRelease shl bp_DDELN_fRelease) and bm_DDELN_fRelease);
    end;

  function fDeferUpd(var a : DDELN) : word;
    begin
       fDeferUpd:=(a.flag0 and bm_DDELN_fDeferUpd) shr bp_DDELN_fDeferUpd;
    end;

  procedure set_fDeferUpd(var a : DDELN; __fDeferUpd : word);
    begin
       a.flag0:=a.flag0 or ((__fDeferUpd shl bp_DDELN_fDeferUpd) and bm_DDELN_fDeferUpd);
    end;

  function fAckReq(var a : DDELN) : word;
    begin
       fAckReq:=(a.flag0 and bm_DDELN_fAckReq) shr bp_DDELN_fAckReq;
    end;

  procedure set_fAckReq(var a : DDELN; __fAckReq : word);
    begin
       a.flag0:=a.flag0 or ((__fAckReq shl bp_DDELN_fAckReq) and bm_DDELN_fAckReq);
    end;

  function unused(var a : DDEPOKE) : word;
    begin
       unused:=(a.flag0 and bm_DDEPOKE_unused) shr bp_DDEPOKE_unused;
    end;

  procedure set_unused(var a : DDEPOKE; __unused : word);
    begin
       a.flag0:=a.flag0 or ((__unused shl bp_DDEPOKE_unused) and bm_DDEPOKE_unused);
    end;

  function fRelease(var a : DDEPOKE) : word;
    begin
       fRelease:=(a.flag0 and bm_DDEPOKE_fRelease) shr bp_DDEPOKE_fRelease;
    end;

  procedure set_fRelease(var a : DDEPOKE; __fRelease : word);
    begin
       a.flag0:=a.flag0 or ((__fRelease shl bp_DDEPOKE_fRelease) and bm_DDEPOKE_fRelease);
    end;

  function fReserved(var a : DDEPOKE) : word;
    begin
       fReserved:=(a.flag0 and bm_DDEPOKE_fReserved) shr bp_DDEPOKE_fReserved;
    end;

  procedure set_fReserved(var a : DDEPOKE; __fReserved : word);
    begin
       a.flag0:=a.flag0 or ((__fReserved shl bp_DDEPOKE_fReserved) and bm_DDEPOKE_fReserved);
    end;

  function unused(var a : DDEUP) : word;
    begin
       unused:=(a.flag0 and bm_DDEUP_unused) shr bp_DDEUP_unused;
    end;

  procedure set_unused(var a : DDEUP; __unused : word);
    begin
       a.flag0:=a.flag0 or ((__unused shl bp_DDEUP_unused) and bm_DDEUP_unused);
    end;

  function fAck(var a : DDEUP) : word;
    begin
       fAck:=(a.flag0 and bm_DDEUP_fAck) shr bp_DDEUP_fAck;
    end;

  procedure set_fAck(var a : DDEUP; __fAck : word);
    begin
       a.flag0:=a.flag0 or ((__fAck shl bp_DDEUP_fAck) and bm_DDEUP_fAck);
    end;

  function fRelease(var a : DDEUP) : word;
    begin
       fRelease:=(a.flag0 and bm_DDEUP_fRelease) shr bp_DDEUP_fRelease;
    end;

  procedure set_fRelease(var a : DDEUP; __fRelease : word);
    begin
       a.flag0:=a.flag0 or ((__fRelease shl bp_DDEUP_fRelease) and bm_DDEUP_fRelease);
    end;

  function fReserved(var a : DDEUP) : word;
    begin
       fReserved:=(a.flag0 and bm_DDEUP_fReserved) shr bp_DDEUP_fReserved;
    end;

  procedure set_fReserved(var a : DDEUP; __fReserved : word);
    begin
       a.flag0:=a.flag0 or ((__fReserved shl bp_DDEUP_fReserved) and bm_DDEUP_fReserved);
    end;

  function fAckReq(var a : DDEUP) : word;
    begin
       fAckReq:=(a.flag0 and bm_DDEUP_fAckReq) shr bp_DDEUP_fAckReq;
    end;

  procedure set_fAckReq(var a : DDEUP; __fAckReq : word);
    begin
       a.flag0:=a.flag0 or ((__fAckReq shl bp_DDEUP_fAckReq) and bm_DDEUP_fAckReq);
    end;


{$endif read_implementation}


{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.5  1998-10-27 11:17:17  peter
    * type HINSTANCE -> HINST

  Revision 1.4  1998/08/31 11:53:59  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

  Revision 1.3  1998/06/25 08:41:48  florian
    * better rtti

  Revision 1.2  1998/05/06 12:36:50  michael
  + Removed log from before restored version.

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version
}
