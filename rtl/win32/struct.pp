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

{
    This file is generated using h2pas written by Florian Klaempfl,
    but some modifications are done.

    The C header file was structure.h of the
    GNU Windows32 API Library Version 0.1.2

 ****************************************************************************

  Possible defines:
      UNICODE    makes an unicode version
      I386       makes a version for Intel processors

  FK     Florian Klaempfl
  +      added
  -      removed
  *      modified

  History (started with version 0.1.0):
      19th november 1997 version:
         + started (FK)

  ToDo:

  Not recommended to use:

  Unimplemented:
      typedef struct _DCB {
        DWORD DCBlength;
        DWORD BaudRate;
        DWORD fBinary: 1;
        DWORD fParity: 1;
        DWORD fOutxCtsFlow:1;
        DWORD fOutxDsrFlow:1;
        DWORD fDtrControl:2;
        DWORD fDsrSensitivity:1;
        DWORD fTXContinueOnXoff:1;
        DWORD fOutX: 1;
        DWORD fInX: 1;
        DWORD fErrorChar: 1;
        DWORD fNull: 1;
        DWORD fRtsControl:2;
        DWORD fAbortOnError:1;
        DWORD fDummy2:17;
        WORD wReserved;
        WORD XonLim;
        WORD XoffLim;
        BYTE ByteSize;
        BYTE Parity;
        BYTE StopBits;
        char XonChar;
        char XoffChar;
        char ErrorChar;
        char EofChar;
        char EvtChar;
        WORD wReserved1;
      } DCB, *LPDCB;

      typedef struct _COMSTAT {
        DWORD fCtsHold : 1;
        DWORD fDsrHold : 1;
        DWORD fRlsdHold : 1;
        DWORD fXoffHold : 1;
        DWORD fXoffSent : 1;
        DWORD fEof : 1;
        DWORD fTxim : 1;
        DWORD fReserved : 25;
        DWORD cbInQue;
        DWORD cbOutQue;
      } COMSTAT, *LPCOMSTAT;

      typedef struct {
        unsigned short bAppReturnCode:8,
          reserved:6,
          fBusy:1,
          fAck:1;
      } DDEACK;

      typedef struct {
        unsigned short reserved:14,
          fDeferUpd:1,
          fAckReq:1;
        short cfFormat;
      } DDEADVISE;

      typedef struct {
        unsigned short unused:12,
          fResponse:1,
          fRelease:1,
          reserved:1,
          fAckReq:1;
        short cfFormat;
        BYTE  Value[1];
      } DDEDATA;

      typedef struct {
        unsigned short unused:13,
          fRelease:1,
          fDeferUpd:1,
          fAckReq:1;
        short cfFormat;
      } DDELN;

      typedef struct {
        unsigned short unused:13,
          fRelease:1,
          fReserved:2;
        short cfFormat;
        BYTE  Value[1];
      } DDEPOKE;

      typedef struct {
        unsigned short unused:12,
          fAck:1,
          fRelease:1,
          fReserved:1,
          fAckReq:1;
        short cfFormat;
        BYTE rgb[1];
      } DDEUP;

      typedef struct _LDT_ENTRY {
        WORD LimitLow;
        WORD BaseLow;
        union {
          struct {
            BYTE BaseMid;
            BYTE Flags1;
            BYTE Flags2;
            BYTE BaseHi;
          } Bytes;
          struct {
            DWORD BaseMid : 8;
            DWORD Type : 5;
            DWORD Dpl : 2;
            DWORD Pres : 1;
            DWORD LimitHi : 4;
            DWORD Sys : 1;
            DWORD Reserved_0 : 1;
            DWORD Default_Big : 1;
            DWORD Granularity : 1;
            DWORD BaseHi : 8;
          } Bits;
        } HighWord;
      } LDT_ENTRY, *PLDT_ENTRY, *LPLDT_ENTRY;

      typedef enum tagSHCONTF {
        SHCONTF_FOLDERS = 32,
        SHCONTF_NONFOLDERS = 64,
        SHCONTF_INCLUDEHIDDEN = 128,
      } SHCONTF;
      typedef enum tagSHGDN {
        SHGDN_NORMAL = 0,
        SHGDN_INFOLDER = 1,
        SHGDN_FORPARSING = 0x8000,
      } SHGNO;

 ****************************************************************************}

unit struct;

  interface

    uses
       base;

    type
       ABC = record
            abcA : longint;
            abcB : UINT;
            abcC : longint;
         end;
       LPABC = ^ABC;
       ABCFLOAT = record
            abcfA : FLOAT;
            abcfB : FLOAT;
            abcfC : FLOAT;
         end;
       LPABCFLOAT = ^ABCFLOAT;
       ACCEL = record
            fVirt : BYTE;
            key : WORD;
            cmd : WORD;
         end;
       LPACCEL = ^ACCEL;
       ACE_HEADER = record
            AceType : BYTE;
            AceFlags : BYTE;
            AceSize : WORD;
         end;
       ACCESS_MASK = DWORD;
       REGSAM = ACCESS_MASK;
       ACCESS_ALLOWED_ACE = record
            Header : ACE_HEADER;
            Mask : ACCESS_MASK;
            SidStart : DWORD;
         end;
       ACCESS_DENIED_ACE = record
            Header : ACE_HEADER;
            Mask : ACCESS_MASK;
            SidStart : DWORD;
         end;
       ACCESSTIMEOUT = record
            cbSize : UINT;
            dwFlags : DWORD;
            iTimeOutMSec : DWORD;
         end;
       ACL = record
            AclRevision : BYTE;
            Sbz1 : BYTE;
            AclSize : WORD;
            AceCount : WORD;
            Sbz2 : WORD;
         end;
       PACL = ^ACL;
       ACL_REVISION_INFORMATION = record
            AclRevision : DWORD;
         end;
       ACL_SIZE_INFORMATION = record
            AceCount : DWORD;
            AclBytesInUse : DWORD;
            AclBytesFree : DWORD;
         end;
       ACTION_HEADER = record
            transport_id : ULONG;
            action_code : USHORT;
            reserved : USHORT;
         end;
       ADAPTER_STATUS = record
            adapter_address : array[0..(6)-1] of UCHAR;
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
       ADDJOB_INFO_1 = record
            Path : LPTSTR;
            JobId : DWORD;
         end;
       ANIMATIONINFO = record
            cbSize : UINT;
            iMinAnimate : longint;
         end;
       LPANIMATIONINFO = ^ANIMATIONINFO;
       RECT = record
            left : LONG;
            top : LONG;
            right : LONG;
            bottom : LONG;
         end;
       LPRECT = ^RECT;
       RECTL = record
            left : LONG;
            top : LONG;
            right : LONG;
            bottom : LONG;
         end;
       APPBARDATA = record
            cbSize : DWORD;
            hWnd : HWND;
            uCallbackMessage : UINT;
            uEdge : UINT;
            rc : RECT;
            lParam : LPARAM;
         end;
       PAPPBARDATA = ^APPBARDATA;
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
       BITMAPCOREHEADER = record
            bcSize : DWORD;
            bcWidth : WORD;
            bcHeight : WORD;
            bcPlanes : WORD;
            bcBitCount : WORD;
         end;
       RGBTRIPLE = record
            rgbtBlue : BYTE;
            rgbtGreen : BYTE;
            rgbtRed : BYTE;
         end;
       BITMAPCOREINFO = record
            bmciHeader : BITMAPCOREHEADER;
            bmciColors : array[0..(1)-1] of RGBTRIPLE;
         end;
       BITMAPFILEHEADER = record
            bfType : WORD;
            bfSize : DWORD;
            bfReserved1 : WORD;
            bfReserved2 : WORD;
            bfOffBits : DWORD;
         end;
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
       RGBQUAD = record
            rgbBlue : BYTE;
            rgbGreen : BYTE;
            rgbRed : BYTE;
            rgbReserved : BYTE;
         end;
       BITMAPINFO = record
            bmiHeader : BITMAPINFOHEADER;
            bmiColors : array[0..0] of RGBQUAD;
         end;
       LPBITMAPINFO = ^BITMAPINFO;
       FXPT2DOT30 = longint;
       LPFXPT2DOT30 = ^longint;
       CIEXYZ = record
            ciexyzX : FXPT2DOT30;
            ciexyzY : FXPT2DOT30;
            ciexyzZ : FXPT2DOT30;
         end;
       LPCIEXYZ = ^CIEXYZ;
       CIEXYZTRIPLE = record
            ciexyzRed : CIEXYZ;
            ciexyzGreen : CIEXYZ;
            ciexyzBlue : CIEXYZ;
         end;
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
       SHITEMID = record
            cb : USHORT;
            abID : array[0..0] of BYTE;
         end;
       LPSHITEMID = ^SHITEMID;
       LPCSHITEMID = ^SHITEMID;
       ITEMIDLIST = record
            mkid : SHITEMID;
         end;
       LPITEMIDLIST = ITEMIDLIST;
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
       FILETIME = record
            dwLowDateTime : DWORD;
            dwHighDateTime : DWORD;
         end;
       LPFILETIME = ^FILETIME;
       PFILETIME = ^FILETIME;
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
       FIXED = record
            fract : WORD;
            value : integer;
         end;
       POINT = record
            x : LONG;
            y : LONG;
         end;
       LPPOINT = ^POINT;
       POINTFX = record
            x : FIXED;
            y : FIXED;
         end;
       POINTL = record
            x : LONG;
            y : LONG;
         end;
       POINTS = record
            x : SHORT;
            y : SHORT;
         end;
       CANDIDATEFORM = record
            dwIndex : DWORD;
            dwStyle : DWORD;
            ptCurrentPos : POINT;
            rcArea : RECT;
         end;
       LPCANDIDATEFORM = ^CANDIDATEFORM;
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
       CREATESTRUCT = record
            lpCreateParams : LPVOID;
            hInstance : HINSTANCE;
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
       CBT_CREATEWND = record
            lpcs : LPCREATESTRUCT;
            hwndInsertAfter : HWND;
         end;
       CBTACTIVATESTRUCT = record
            fMouse : WINBOOL;
            hWndActive : HWND;
         end;
       CHAR_INFO = record
            Char : record
                case longint of
                   0 : ( UnicodeChar:WCHAR );
                   1 : ( AsciiChar:CHAR );
              end;
            Attributes : WORD;
         end;
       PCHAR_INFO = ^CHAR_INFO;
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
       CHARRANGE = record
            cpMin : LONG;
            cpMax : LONG;
         end;
       CHARSET = record
            aflBlock : array[0..(3)-1] of DWORD;
            flLang : DWORD;
         end;
       FONTSIGNATURE = record
            fsUsb : array[0..(4)-1] of DWORD;
            fsCsb : array[0..(2)-1] of DWORD;
         end;
       LPFONTSIGNATURE = ^FONTSIGNATURE;
       CHARSETINFO = record
            ciCharset : UINT;
            ciACP : UINT;
            fs : FONTSIGNATURE;
         end;
       LPCHARSETINFO = ^CHARSETINFO;
       CHOOSECOLOR = record
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
       LPCHOOSECOLOR = ^CHOOSECOLOR;
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
       CHOOSEFONT = record
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
            hInstance : HINSTANCE;
            lpszStyle : LPTSTR;
            nFontType : WORD;
            ___MISSING_ALIGNMENT__ : WORD;
            nSizeMin : INT;
            nSizeMax : INT;
         end;
       LPCHOOSEFONT = ^CHOOSEFONT;
       CIDA = record
            cidl : UINT;
            aoffset : array[0..(1)-1] of UINT;
         end;
       LPIDA = ^CIDA;
       CLIENTCREATESTRUCT = record
            hWindowMenu : HANDLE;
            idFirstChild : UINT;
         end;
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
       COLORMAP = record
            from : COLORREF;
            _to : COLORREF;
         end;
       LPCOLORMAP = ^COLORMAP;
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
       COMMTIMEOUTS = record
            ReadIntervalTimeout : DWORD;
            ReadTotalTimeoutMultiplier : DWORD;
            ReadTotalTimeoutConstant : DWORD;
            WriteTotalTimeoutMultiplier : DWORD;
            WriteTotalTimeoutConstant : DWORD;
         end;
       LPCOMMTIMEOUTS = ^COMMTIMEOUTS;
       COMPAREITEMSTRUCT = record
            CtlType : UINT;
            CtlID : UINT;
            hwndItem : HWND;
            itemID1 : UINT;
            itemData1 : DWORD;
            itemID2 : UINT;
            itemData2 : DWORD;
         end;
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
       CONSOLE_CURSOR_INFO = record
            dwSize : DWORD;
            bVisible : WINBOOL;
         end;
       PCONSOLE_CURSOR_INFO = ^CONSOLE_CURSOR_INFO;
       COORD = record
            X : SHORT;
            Y : SHORT;
         end;
       SMALL_RECT = record
            Left : SHORT;
            Top : SHORT;
            Right : SHORT;
            Bottom : SHORT;
         end;
       PSMALL_RECT = ^SMALL_RECT;
       CONSOLE_SCREEN_BUFFER_INFO = record
            dwSize : COORD;
            dwCursorPosition : COORD;
            wAttributes : WORD;
            srWindow : SMALL_RECT;
            dwMaximumWindowSize : COORD;
         end;
       PCONSOLE_SCREEN_BUFFER_INFO = ^CONSOLE_SCREEN_BUFFER_INFO;
{$ifdef I386}
       FLOATING_SAVE_AREA = record
            ControlWord : DWORD;
            StatusWord : DWORD;
            TagWord : DWORD;
            ErrorOffset : DWORD;
            ErrorSelector : DWORD;
            DataOffset : DWORD;
            DataSelector : DWORD;
            RegisterArea : array[0..(80)-1] of BYTE;
            Cr0NpxState : DWORD;
         end;
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
{$endif I386}
       LIST_ENTRY = record
            Flink : ^*;
            Blink : ^*;
         end;
       PLIST_ENTRY = ^LIST_ENTRY;
       CRITICAL_SECTION_DEBUG = record
            Type : WORD;
            CreatorBackTraceIndex : WORD;
            CriticalSection : ^*;
            ProcessLocksList : LIST_ENTRY;
            EntryCount : DWORD;
            ContentionCount : DWORD;
            Depth : DWORD;
            OwnerBackTrace : array[0..(5)-1] of PVOID;
         end;
       PCRITICAL_SECTION_DEBUG = ^CRITICAL_SECTION_DEBUG;
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
    { SECURITY_CONTEXT_TRACKING_MODE ContextTrackingMode; }
       SECURITY_QUALITY_OF_SERVICE = record
            Length : DWORD;
            ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
            ContextTrackingMode : WINBOOL;
            EffectiveOnly : BOOLEAN;
         end;
       CONVCONTEXT = record
            cb : UINT;
            wFlags : UINT;
            wCountryID : UINT;
            iCodePage : longint;
            dwLangID : DWORD;
            dwSecurity : DWORD;
            qos : SECURITY_QUALITY_OF_SERVICE;
         end;
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
            hwnd : HWND;
            hwndPartner : HWND;
         end;
       COPYDATASTRUCT = record
            dwData : DWORD;
            cbData : DWORD;
            lpData : PVOID;
         end;
       CPINFO = record
            MaxCharSize : UINT;
            DefaultChar : array[0..(MAX_DEFAULTCHAR)-1] of BYTE;
            LeadByte : array[0..(MAX_LEADBYTES)-1] of BYTE;
         end;
       LPCPINFO = ^CPINFO;
       CPLINFO = record
            idIcon : longint;
            idName : longint;
            idInfo : longint;
            lData : LONG;
         end;
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
       CREATE_THREAD_DEBUG_INFO = record
            hThread : HANDLE;
            lpThreadLocalBase : LPVOID;
            lpStartAddress : LPTHREAD_START_ROUTINE;
         end;
    { TODO: sockets}
    {typedef struct _SOCKET_ADDRESS {}
    {  LPSOCKADDR lpSockaddr ;}
    {  INT iSockaddrLength ;}
    {} SOCKET_ADDRESS, *PSOCKET_ADDRESS, *LPSOCKET_ADDRESS;}
    {typedef struct _CSADDR_INFO { }
    {  SOCKET_ADDRESS  LocalAddr; }
    {  SOCKET_ADDRESS  RemoteAddr; }
    {  INT             iSocketType; }
    {  INT             iProtocol; }
    {} CSADDR_INFO; }
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
       CWPRETSTRUCT = record
            lResult : LRESULT;
            lParam : LPARAM;
            wParam : WPARAM;
            message : DWORD;
            hwnd : HWND;
         end;
       CWPSTRUCT = record
            lParam : LPARAM;
            wParam : WPARAM;
            message : UINT;
            hwnd : HWND;
         end;
       DATATYPES_INFO_1 = record
            pName : LPTSTR;
         end;
       DDEML_MSG_HOOK_DATA = record
            uiLo : UINT;
            uiHi : UINT;
            cbData : DWORD;
            Data : array[0..(8)-1] of DWORD;
         end;
       EXCEPTION_RECORD = record
            ExceptionCode : DWORD;
            ExceptionFlags : DWORD;
            ExceptionRecord : ^*;
            ExceptionAddress : PVOID;
            NumberParameters : DWORD;
            ExceptionInformation : array[0..(EXCEPTION_MAXIMUM_PARAMETERS)-1] of DWORD;
         end;
       PEXCEPTION_RECORD = ^EXCEPTION_RECORD;
       LPEXCEPTION_RECORD = ^EXCEPTION_RECORD;
       EXCEPTION_DEBUG_INFO = record
            ExceptionRecord : EXCEPTION_RECORD;
            dwFirstChance : DWORD;
         end;
       EXIT_PROCESS_DEBUG_INFO = record
            dwExitCode : DWORD;
         end;
       EXIT_THREAD_DEBUG_INFO = record
            dwExitCode : DWORD;
         end;
       LOAD_DLL_DEBUG_INFO = record
            hFile : HANDLE;
            lpBaseOfDll : LPVOID;
            dwDebugInfoFileOffset : DWORD;
            nDebugInfoSize : DWORD;
            lpImageName : LPVOID;
            fUnicode : WORD;
         end;
       UNLOAD_DLL_DEBUG_INFO = record
            lpBaseOfDll : LPVOID;
         end;
       OUTPUT_DEBUG_STRING_INFO = record
            lpDebugStringData : LPSTR;
            fUnicode : WORD;
            nDebugStringLength : WORD;
         end;
       RIP_INFO = record
            dwError : DWORD;
            dwType : DWORD;
         end;
       DEBUG_EVENT = record
            dwDebugEventCode : DWORD;
            dwProcessId : DWORD;
            dwThreadId : DWORD;
            u : record
                case longint of
                   0 : ( Exception:EXCEPTION_DEBUG_INFO );
                   1 : ( CreateThread:CREATE_THREAD_DEBUG_INFO );
                   2 : ( CreateProcessInfo:CREATE_PROCESS_DEBUG_INFO );
                   3 : ( ExitThread:EXIT_THREAD_DEBUG_INFO );
                   4 : ( ExitProcess:EXIT_PROCESS_DEBUG_INFO );
                   5 : ( LoadDll:LOAD_DLL_DEBUG_INFO );
                   6 : ( UnloadDll:UNLOAD_DLL_DEBUG_INFO );
                   7 : ( DebugString:OUTPUT_DEBUG_STRING_INFO );
                   8 : ( RipInfo:RIP_INFO );
              end;
         end;
       LPDEBUG_EVENT = ^DEBUG_EVENT;
       DEBUGHOOKINFO = record
            idThread : DWORD;
            idThreadInstaller : DWORD;
            lParam : LPARAM;
            wParam : WPARAM;
            code : longint;
         end;
       DELETEITEMSTRUCT = record
            CtlType : UINT;
            CtlID : UINT;
            itemID : UINT;
            hwndItem : HWND;
            itemData : UINT;
         end;
       DEV_BROADCAST_HDR = record
            dbch_size : ULONG;
            dbch_devicetype : ULONG;
            dbch_reserved : ULONG;
         end;
       PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR;
       DEV_BROADCAST_OEM = record
            dbco_size : ULONG;
            dbco_devicetype : ULONG;
            dbco_reserved : ULONG;
            dbco_identifier : ULONG;
            dbco_suppfunc : ULONG;
         end;
       PDEV_BROADCAST_OEM = ^DEV_BROADCAST_OEM;
       DEV_BROADCAST_PORT = record
            dbcp_size : ULONG;
            dbcp_devicetype : ULONG;
            dbcp_reserved : ULONG;
            dbcp_name : array[0..(1)-1] of char;
         end;
       PDEV_BROADCAST_PORT = ^DEV_BROADCAST_PORT;
       DEV_BROADCAST_USERDEFINED = record
            dbud_dbh : _DEV_BROADCAST_HDR;
            dbud_szName : array[0..(1)-1] of char;
            dbud_rgbUserDefined : array[0..(1)-1] of BYTE;
         end;
       DEV_BROADCAST_VOLUME = record
            dbcv_size : ULONG;
            dbcv_devicetype : ULONG;
            dbcv_reserved : ULONG;
            dbcv_unitmask : ULONG;
            dbcv_flags : USHORT;
         end;
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
       DEVNAMES = record
            wDriverOffset : WORD;
            wDeviceOffset : WORD;
            wOutputOffset : WORD;
            wDefault : WORD;
         end;
       LPDEVNAMES = ^DEVNAMES;
       DIBSECTION = record
            dsBm : BITMAP;
            dsBmih : BITMAPINFOHEADER;
            dsBitfields : array[0..(3)-1] of DWORD;
            dshSection : HANDLE;
            dsOffset : DWORD;
         end;
       LARGE_INTEGER = record
            LowPart : DWORD;
            HighPart : LONG;
         end;
       PLARGE_INTEGER = ^LARGE_INTEGER;
       DISK_GEOMETRY = record
            Cylinders : LARGE_INTEGER;
            MediaType : MEDIA_TYPE;
            TracksPerCylinder : DWORD;
            SectorsPerTrack : DWORD;
            BytesPerSector : DWORD;
         end;
       DISK_PERFORMANCE = record
            BytesRead : LARGE_INTEGER;
            BytesWritten : LARGE_INTEGER;
            ReadTime : LARGE_INTEGER;
            WriteTime : LARGE_INTEGER;
            ReadCount : DWORD;
            WriteCount : DWORD;
            QueueDepth : DWORD;
         end;
       DLGITEMTEMPLATE = record
            style : DWORD;
            dwExtendedStyle : DWORD;
            x : integer;
            y : integer;
            cx : integer;
            cy : integer;
            id : WORD;
         end;
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
       LPCDLGTEMPLATE = ^DLGTEMPLATE;
       DOC_INFO_1 = record
            pDocName : LPTSTR;
            pOutputFile : LPTSTR;
            pDatatype : LPTSTR;
         end;
       DOC_INFO_2 = record
            pDocName : LPTSTR;
            pOutputFile : LPTSTR;
            pDatatype : LPTSTR;
            dwMode : DWORD;
            JobId : DWORD;
         end;
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
       DRIVE_LAYOUT_INFORMATION = record
            PartitionCount : DWORD;
            Signature : DWORD;
            PartitionEntry : array[0..(1)-1] of PARTITION_INFORMATION;
         end;
       DRIVER_INFO_1 = record
            pName : LPTSTR;
         end;
       DRIVER_INFO_2 = record
            cVersion : DWORD;
            pName : LPTSTR;
            pEnvironment : LPTSTR;
            pDriverPath : LPTSTR;
            pDataFile : LPTSTR;
            pConfigFile : LPTSTR;
         end;
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
       EDITSTREAM = record
            dwCookie : DWORD;
            dwError : DWORD;
            pfnCallback : EDITSTREAMCALLBACK;
         end;
       EMR = record
            iType : DWORD;
            nSize : DWORD;
         end;
       PEMR = ^EMR;
       EMRANGLEARC = record
            emr : EMR;
            ptlCenter : POINTL;
            nRadius : DWORD;
            eStartAngle : FLOAT;
            eSweepAngle : FLOAT;
         end;
       PEMRANGLEARC = ^EMRANGLEARC;
       EMRARC = record
            emr : EMR;
            rclBox : RECTL;
            ptlStart : POINTL;
            ptlEnd : POINTL;
         end;
       PEMRARC = ^EMRARC;
       EMRARCTO = record
            emr : EMR;
            rclBox : RECTL;
            ptlStart : POINTL;
            ptlEnd : POINTL;
         end;
       PEMRARCTO = ^EMRARCTO;
       EMRCHORD = record
            emr : EMR;
            rclBox : RECTL;
            ptlStart : POINTL;
            ptlEnd : POINTL;
         end;
       PEMRCHORD = ^EMRCHORD;
       EMRPIE = record
            emr : EMR;
            rclBox : RECTL;
            ptlStart : POINTL;
            ptlEnd : POINTL;
         end;
       PEMRPIE = ^EMRPIE;
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
       LOGBRUSH = record
            lbStyle : UINT;
            lbColor : COLORREF;
            lbHatch : LONG;
         end;
       EMRCREATEBRUSHINDIRECT = record
            emr : EMR;
            ihBrush : DWORD;
            lb : LOGBRUSH;
         end;
       PEMRCREATEBRUSHINDIRECT = ^EMRCREATEBRUSHINDIRECT;
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
       EMRCREATECOLORSPACE = record
            emr : EMR;
            ihCS : DWORD;
            lcs : LOGCOLORSPACE;
         end;
       PEMRCREATECOLORSPACE = ^EMRCREATECOLORSPACE;
       EMRCREATEDIBPATTERNBRUSHPT = record
            emr : EMR;
            ihBrush : DWORD;
            iUsage : DWORD;
            offBmi : DWORD;
            cbBmi : DWORD;
            offBits : DWORD;
            cbBits : DWORD;
         end;
       PEMRCREATEDIBPATTERNBRUSHPT = record
            emr : EMR;
            ihBrush : DWORD;
            iUsage : DWORD;
            offBmi : DWORD;
            cbBmi : DWORD;
            offBits : DWORD;
            cbBits : DWORD;
         end;
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
       PALETTEENTRY = record
            peRed : BYTE;
            peGreen : BYTE;
            peBlue : BYTE;
            peFlags : BYTE;
         end;
       LPPALETTEENTRY = ^PALETTEENTRY;
       LOGPALETTE = record
            palVersion : WORD;
            palNumEntries : WORD;
            palPalEntry : array[0..(1)-1] of PALETTEENTRY;
         end;
       EMRCREATEPALETTE = record
            emr : EMR;
            ihPal : DWORD;
            lgpl : LOGPALETTE;
         end;
       PEMRCREATEPALETTE = ^EMRCREATEPALETTE;
       LOGPEN = record
            lopnStyle : UINT;
            lopnWidth : POINT;
            lopnColor : COLORREF;
         end;
       EMRCREATEPEN = record
            emr : EMR;
            ihPen : DWORD;
            lopn : LOGPEN;
         end;
       PEMRCREATEPEN = ^EMRCREATEPEN;
       EMRELLIPSE = record
            emr : EMR;
            rclBox : RECTL;
         end;
       PEMRELLIPSE = ^EMRELLIPSE;
       EMRRECTANGLE = record
            emr : EMR;
            rclBox : RECTL;
         end;
       PEMRRECTANGLE = ^EMRRECTANGLE;
       EMREOF = record
            emr : EMR;
            nPalEntries : DWORD;
            offPalEntries : DWORD;
            nSizeLast : DWORD;
         end;
       PEMREOF = ^EMREOF;
       EMREXCLUDECLIPRECT = record
            emr : EMR;
            rclClip : RECTL;
         end;
       PEMREXCLUDECLIPRECT = ^EMREXCLUDECLIPRECT;
       EMRINTERSECTCLIPRECT = record
            emr : EMR;
            rclClip : RECTL;
         end;
       PEMRINTERSECTCLIPRECT = ^EMRINTERSECTCLIPRECT;
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
       EMREXTCREATEFONTINDIRECTW = record
            emr : EMR;
            ihFont : DWORD;
            elfw : EXTLOGFONT;
         end;
       PEMREXTCREATEFONTINDIRECTW = record
            emr : EMR;
            ihFont : DWORD;
            elfw : EXTLOGFONT;
         end;
       EXTLOGPEN = record
            elpPenStyle : UINT;
            elpWidth : UINT;
            elpBrushStyle : UINT;
            elpColor : COLORREF;
            elpHatch : LONG;
            elpNumEntries : DWORD;
            elpStyleEntry : array[0..(1)-1] of DWORD;
         end;
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
       EMREXTFLOODFILL = record
            emr : EMR;
            ptlStart : POINTL;
            crColor : COLORREF;
            iMode : DWORD;
         end;
       PEMREXTFLOODFILL = ^EMREXTFLOODFILL;
       EMREXTSELECTCLIPRGN = record
            emr : EMR;
            cbRgnData : DWORD;
            iMode : DWORD;
            RgnData : array[0..(1)-1] of BYTE;
         end;
       PEMREXTSELECTCLIPRGN = ^EMREXTSELECTCLIPRGN;
       EMRTEXT = record
            ptlReference : POINTL;
            nChars : DWORD;
            offString : DWORD;
            fOptions : DWORD;
            rcl : RECTL;
            offDx : DWORD;
         end;
       PEMRTEXT = ^EMRTEXT;
       EMREXTTEXTOUTA = record
            emr : EMR;
            rclBounds : RECTL;
            iGraphicsMode : DWORD;
            exScale : FLOAT;
            eyScale : FLOAT;
            emrtext : EMRTEXT;
         end;
       PEMREXTTEXTOUTA = ^EMREXTTEXTOUTA;
       EMREXTTEXTOUTW = record
            emr : EMR;
            rclBounds : RECTL;
            iGraphicsMode : DWORD;
            exScale : FLOAT;
            eyScale : FLOAT;
            emrtext : EMRTEXT;
         end;
       PEMREXTTEXTOUTW = ^EMREXTTEXTOUTW;
       EMRFILLPATH = record
            emr : EMR;
            rclBounds : RECTL;
         end;
       PEMRFILLPATH = ^EMRFILLPATH;
       EMRSTROKEANDFILLPATH = record
            emr : EMR;
            rclBounds : RECTL;
         end;
       PEMRSTROKEANDFILLPATH = ^EMRSTROKEANDFILLPATH;
       EMRSTROKEPATH = record
            emr : EMR;
            rclBounds : RECTL;
         end;
       PEMRSTROKEPATH = ^EMRSTROKEPATH;
       EMRFILLRGN = record
            emr : EMR;
            rclBounds : RECTL;
            cbRgnData : DWORD;
            ihBrush : DWORD;
            RgnData : array[0..(1)-1] of BYTE;
         end;
       PEMRFILLRGN = ^EMRFILLRGN;
       EMRFORMAT = record
            dSignature : DWORD;
            nVersion : DWORD;
            cbData : DWORD;
            offData : DWORD;
         end;
       SIZE = record
            cx : LONG;
            cy : LONG;
         end;
       PSIZE = ^SIZE;
       LPSIZE = ^SIZE;
       SIZEL = record
            cx : LONG;
            cy : LONG;
         end;
       PSIZEL = ^SIZEL;
       LPSIZEL = ^SIZEL;
       EMRFRAMERGN = record
            emr : EMR;
            rclBounds : RECTL;
            cbRgnData : DWORD;
            ihBrush : DWORD;
            szlStroke : SIZEL;
            RgnData : array[0..0] of BYTE;
         end;
       PEMRFRAMERGN = ^EMRFRAMERGN;
       EMRGDICOMMENT = record
            emr : EMR;
            cbData : DWORD;
            Data : array[0..0] of BYTE;
         end;
       PEMRGDICOMMENT = ^EMRGDICOMMENT;
       EMRINVERTRGN = record
            emr : EMR;
            rclBounds : RECTL;
            cbRgnData : DWORD;
            RgnData : array[0..(1)-1] of BYTE;
         end;
       PEMRINVERTRGN = ^EMRINVERTRGN;
       EMRPAINTRGN = record
            emr : EMR;
            rclBounds : RECTL;
            cbRgnData : DWORD;
            RgnData : array[0..(1)-1] of BYTE;
         end;
       PEMRPAINTRGN = ^EMRPAINTRGN;
       EMRLINETO = record
            emr : EMR;
            ptl : POINTL;
         end;
       PEMRLINETO = ^EMRLINETO;
       EMRMOVETOEX = record
            emr : EMR;
            ptl : POINTL;
         end;
       PEMRMOVETOEX = ^EMRMOVETOEX;
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
       EMRMODIFYWORLDTRANSFORM = record
            emr : EMR;
            xform : XFORM;
            iMode : DWORD;
         end;
       PEMRMODIFYWORLDTRANSFORM = ^EMRMODIFYWORLDTRANSFORM;
       EMROFFSETCLIPRGN = record
            emr : EMR;
            ptlOffset : POINTL;
         end;
       PEMROFFSETCLIPRGN = ^EMROFFSETCLIPRGN;
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
       EMRPOLYDRAW = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
            abTypes : array[0..0] of BYTE;
         end;
       PEMRPOLYDRAW = ^EMRPOLYDRAW;
       EMRPOLYDRAW16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTS;
            abTypes : array[0..0] of BYTE;
         end;
       PEMRPOLYDRAW16 = ^EMRPOLYDRAW16;
       EMRPOLYLINE = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYLINE = ^EMRPOLYLINE;
       EMRPOLYBEZIER = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYBEZIER = ^EMRPOLYBEZIER;
       EMRPOLYGON = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYGON = ^EMRPOLYGON;
       EMRPOLYBEZIERTO = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYBEZIERTO = ^EMRPOLYBEZIERTO;
       EMRPOLYLINETO = record
            emr : EMR;
            rclBounds : RECTL;
            cptl : DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYLINETO = ^EMRPOLYLINETO;
       EMRPOLYLINE16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTL;
         end;
       PEMRPOLYLINE16 = ^EMRPOLYLINE16;
       EMRPOLYBEZIER16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTL;
         end;
       PEMRPOLYBEZIER16 = ^EMRPOLYBEZIER16;
       EMRPOLYGON16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTL;
         end;
       PEMRPOLYGON16 = ^EMRPOLYGON16;
       EMRPOLYBEZIERTO16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTL;
         end;
       PEMRPOLYBEZIERTO16 = ^EMRPOLYBEZIERTO16;
       EMRPOLYLINETO16 = record
            emr : EMR;
            rclBounds : RECTL;
            cpts : DWORD;
            apts : array[0..0] of POINTL;
         end;
       PEMRPOLYLINETO16 = ^EMRPOLYLINETO16;
       EMRPOLYPOLYLINE = record
            emr : EMR;
            rclBounds : RECTL;
            nPolys : DWORD;
            cptl : DWORD;
            aPolyCounts : array[0..0] of DWORD;
            aptl : array[0..0] of POINTL;
         end;
       PEMRPOLYPOLYLINE = ^EMRPOLYPOLYLINE;
       EMRPOLYPOLYGON = record
            emr : EMR;
            rclBounds : RECTL;
            nPolys : DWORD;
            cptl : DWORD;
            aPolyCounts : array[0..(1)-1] of DWORD;
            aptl : array[0..(1)-1] of POINTL;
         end;
       PEMRPOLYPOLYGON = ^EMRPOLYPOLYGON;
       EMRPOLYPOLYLINE16 = record
            emr : EMR;
            rclBounds : RECTL;
            nPolys : DWORD;
            cpts : DWORD;
            aPolyCounts : array[0..(1)-1] of DWORD;
            apts : array[0..(1)-1] of POINTS;
         end;
       PEMRPOLYPOLYLINE16 = ^EMRPOLYPOLYLINE16;
       EMRPOLYPOLYGON16 = record
            emr : EMR;
            rclBounds : RECTL;
            nPolys : DWORD;
            cpts : DWORD;
            aPolyCounts : array[0..(1)-1] of DWORD;
            apts : array[0..(1)-1] of POINTS;
         end;
       PEMRPOLYPOLYGON16 = ^EMRPOLYPOLYGON16;
       EMRPOLYTEXTOUTA = record
            emr : EMR;
            rclBounds : RECTL;
            iGraphicsMode : DWORD;
            exScale : FLOAT;
            eyScale : FLOAT;
            cStrings : LONG;
            aemrtext : array[0..(1)-1] of EMRTEXT;
         end;
       PEMRPOLYTEXTOUTA = ^EMRPOLYTEXTOUTA;
       EMRPOLYTEXTOUTW = record
            emr : EMR;
            rclBounds : RECTL;
            iGraphicsMode : DWORD;
            exScale : FLOAT;
            eyScale : FLOAT;
            cStrings : LONG;
            aemrtext : array[0..(1)-1] of EMRTEXT;
         end;
       PEMRPOLYTEXTOUTW = ^EMRPOLYTEXTOUTW;
       EMRRESIZEPALETTE = record
            emr : EMR;
            ihPal : DWORD;
            cEntries : DWORD;
         end;
       PEMRRESIZEPALETTE = ^EMRRESIZEPALETTE;
       EMRRESTOREDC = record
            emr : EMR;
            iRelative : LONG;
         end;
       PEMRRESTOREDC = ^EMRRESTOREDC;
       EMRROUNDRECT = record
            emr : EMR;
            rclBox : RECTL;
            szlCorner : SIZEL;
         end;
       PEMRROUNDRECT = ^EMRROUNDRECT;
       EMRSCALEVIEWPORTEXTEX = record
            emr : EMR;
            xNum : LONG;
            xDenom : LONG;
            yNum : LONG;
            yDenom : LONG;
         end;
       PEMRSCALEVIEWPORTEXTEX = ^EMRSCALEVIEWPORTEXTEX;
       EMRSCALEWINDOWEXTEX = record
            emr : EMR;
            xNum : LONG;
            xDenom : LONG;
            yNum : LONG;
            yDenom : LONG;
         end;
       PEMRSCALEWINDOWEXTEX = ^EMRSCALEWINDOWEXTEX;
       EMRSELECTCOLORSPACE = record
            emr : EMR;
            ihCS : DWORD;
         end;
       PEMRSELECTCOLORSPACE = ^EMRSELECTCOLORSPACE;
       EMRDELETECOLORSPACE = record
            emr : EMR;
            ihCS : DWORD;
         end;
       PEMRDELETECOLORSPACE = ^EMRDELETECOLORSPACE;
       EMRSELECTOBJECT = record
            emr : EMR;
            ihObject : DWORD;
         end;
       PEMRSELECTOBJECT = ^EMRSELECTOBJECT;
       EMRDELETEOBJECT = record
            emr : EMR;
            ihObject : DWORD;
         end;
       PEMRDELETEOBJECT = ^EMRDELETEOBJECT;
       EMRSELECTPALETTE = record
            emr : EMR;
            ihPal : DWORD;
         end;
       PEMRSELECTPALETTE = ^EMRSELECTPALETTE;
       EMRSETARCDIRECTION = record
            emr : EMR;
            iArcDirection : DWORD;
         end;
       PEMRSETARCDIRECTION = ^EMRSETARCDIRECTION;
            emr : EMR;
            iArcDirection : DWORD;
         end;
       EMRSETBKCOLOR = record
            emr : EMR;
            crColor : COLORREF;
         end;
       PEMRSETBKCOLOR = ^EMRSETBKCOLOR;
       EMRSETTEXTCOLOR = record
            emr : EMR;
            crColor : COLORREF;
         end;
       PEMRSETTEXTCOLOR = ^EMRSETTEXTCOLOR;
       EMRSETCOLORADJUSTMENT = record
            emr : EMR;
            ColorAdjustment : COLORADJUSTMENT;
         end;
       PEMRSETCOLORADJUSTMENT = ^EMRSETCOLORADJUSTMENT;
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
       EMRSETMAPPERFLAGS = record
            emr : EMR;
            dwFlags : DWORD;
         end;
       PEMRSETMAPPERFLAGS = ^EMRSETMAPPERFLAGS;
       EMRSETMITERLIMIT = record
            emr : EMR;
            eMiterLimit : FLOAT;
         end;
       PEMRSETMITERLIMIT = ^EMRSETMITERLIMIT;
       EMRSETPALETTEENTRIES = record
            emr : EMR;
            ihPal : DWORD;
            iStart : DWORD;
            cEntries : DWORD;
            aPalEntries : array[0..0] of PALETTEENTRY;
         end;
       PEMRSETPALETTEENTRIES = ^EMRSETPALETTEENTRIES;
       EMRSETPIXELV = record
            emr : EMR;
            ptlPixel : POINTL;
            crColor : COLORREF;
         end;
       PEMRSETPIXELV = ^record
            emr : EMR;
            ptlPixel : POINTL;
            crColor : COLORREF;
         end;
       EMRSETVIEWPORTEXTEX = record
            emr : EMR;
            szlExtent : SIZEL;
         end;
       PEMRSETVIEWPORTEXTEX = ^EMRSETVIEWPORTEXTEX;
       EMRSETWINDOWEXTEX = record
            emr : EMR;
            szlExtent : SIZEL;
         end;
       PEMRSETWINDOWEXTEX = ^EMRSETWINDOWEXTEX;
       EMRSETVIEWPORTORGEX = record
            emr : EMR;
            ptlOrigin : POINTL;
         end;
       PEMRSETVIEWPORTORGEX = ^EMRSETVIEWPORTORGEX;
       EMRSETWINDOWORGEX = record
            emr : EMR;
            ptlOrigin : POINTL;
         end;
       PEMRSETWINDOWORGEX = ^EMRSETWINDOWORGEX;
       EMRSETBRUSHORGEX = record
            emr : EMR;
            ptlOrigin : POINTL;
         end;
       PEMRSETBRUSHORGEX = ^EMRSETBRUSHORGEX;
       EMRSETWORLDTRANSFORM = record
            emr : EMR;
            xform : XFORM;
         end;
       PEMRSETWORLDTRANSFORM = ^EMRSETWORLDTRANSFORM;
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
       EMRABORTPATH = record
            emr : EMR;
         end;
       PEMRABORTPATH = ^EMRABORTPATH;
       EMRBEGINPATH = record
            emr : EMR;
         end;
       PEMRBEGINPATH = ^EMRBEGINPATH;
       EMRENDPATH = record
            emr : EMR;
         end;
       PEMRENDPATH = ^EMRENDPATH;
       EMRCLOSEFIGURE = record
            emr : EMR;
         end;
       PEMRCLOSEFIGURE = ^EMRCLOSEFIGURE;
       EMRFLATTENPATH = record
            emr : EMR;
         end;
       PEMRFLATTENPATH = ^EMRFLATTENPATH;
       EMRWIDENPATH = record
            emr : EMR;
         end;
       PEMRWIDENPATH = ^EMRWIDENPATH;
       EMRSETMETARGN = record
            emr : EMR;
         end;
       PEMRSETMETARGN = ^EMRSETMETARGN;
       EMRSAVEDC = record
            emr : EMR;
         end;
       PEMRSAVEDC = ^EMRSAVEDC;
       EMRREALIZEPALETTE = record
            emr : EMR;
         end;
       PEMRREALIZEPALETTE = ^EMRREALIZEPALETTE;
       EMRSELECTCLIPPATH = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSELECTCLIPPATH = ^EMRSELECTCLIPPATH;
       EMRSETBKMODE = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETBKMODE = ^EMRSETBKMODE;
       EMRSETMAPMODE = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETMAPMODE = ^EMRSETMAPMODE;
       EMRSETPOLYFILLMODE = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETPOLYFILLMODE = ^EMRSETPOLYFILLMODE;
       EMRSETROP2 = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETROP2 = ^EMRSETROP2;
       EMRSETSTRETCHBLTMODE = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETSTRETCHBLTMODE = ^EMRSETSTRETCHBLTMODE;
       EMRSETTEXTALIGN = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRSETTEXTALIGN = ^EMRSETTEXTALIGN;
       EMRENABLEICM = record
            emr : EMR;
            iMode : DWORD;
         end;
       PEMRENABLEICM = ^EMRENABLEICM;
       NMHDR = record
            hwndFrom : HWND;
            idFrom : UINT;
            code : UINT;
         end;
       ENCORRECTTEXT = record
            nmhdr : NMHDR;
            chrg : CHARRANGE;
            seltyp : WORD;
         end;
       ENDROPFILES = record
            nmhdr : NMHDR;
            hDrop : HANDLE;
            cp : LONG;
            fProtected : WINBOOL;
         end;
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
       ENHMETARECORD = record
            iType : DWORD;
            nSize : DWORD;
            dParm : array[0..0] of DWORD;
         end;
       ENPROTECTED = record
            nmhdr : NMHDR;
            msg : UINT;
            wParam : WPARAM;
            lParam : LPARAM;
            chrg : CHARRANGE;
         end;
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
       ENUM_SERVICE_STATUS = record
            lpServiceName : LPTSTR;
            lpDisplayName : LPTSTR;
            ServiceStatus : SERVICE_STATUS;
         end;
       LPENUM_SERVICE_STATUS = ^ENUM_SERVICE_STATUS;
       ENUMLOGFONT = record
            elfLogFont : LOGFONT;
            elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
            elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
         end;
       ENUMLOGFONTEX = record
            elfLogFont : LOGFONT;
            elfFullName : array[0..(LF_FULLFACESIZE)-1] of BCHAR;
            elfStyle : array[0..(LF_FACESIZE)-1] of BCHAR;
            elfScript : array[0..(LF_FACESIZE)-1] of BCHAR;
         end;
    { }
    { Then follow:}
    { }
    { TCHAR SourceName[] }
    { TCHAR Computername[] }
    { SID   UserSid }
    { TCHAR Strings[] }
    { BYTE  Data[] }
    { CHAR  Pad[] }
    { DWORD Length; }
    { }
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
       EVENTMSG = record
            message : UINT;
            paramL : UINT;
            paramH : UINT;
            time : DWORD;
            hwnd : HWND;
         end;
       EXCEPTION_POINTERS = record
            ExceptionRecord : PEXCEPTION_RECORD;
            ContextRecord : PCONTEXT;
         end;
       PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
       LPEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
       EXT_BUTTON = record
            idCommand : WORD;
            idsHelp : WORD;
            fsStyle : WORD;
         end;
       LPEXT_BUTTON = ^EXT_BUTTON;
       FILTERKEYS = record
            cbSize : UINT;
            dwFlags : DWORD;
            iWaitMSec : DWORD;
            iDelayMSec : DWORD;
            iRepeatMSec : DWORD;
            iBounceMSec : DWORD;
         end;
       FIND_NAME_BUFFER = record
            length : UCHAR;
            access_control : UCHAR;
            frame_control : UCHAR;
            destination_addr : array[0..5] of UCHAR;
            source_addr : array[0..5] of UCHAR;
            routing_info : array[0..17] of UCHAR;
         end;
       FIND_NAME_HEADER = record
            node_count : WORD;
            reserved : UCHAR;
            unique_group : UCHAR;
         end;
       FINDREPLACE = record
            lStructSize : DWORD;
            hwndOwner : HWND;
            hInstance : HINSTANCE;
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
       FINDTEXT = record
            chrg : CHARRANGE;
            lpstrText : LPSTR;
         end;
       FINDTEXTEX = record
            chrg : CHARRANGE;
            lpstrText : LPSTR;
            chrgText : CHARRANGE;
         end;
       FMS_GETDRIVEINFO = record
            dwTotalSpace : DWORD;
            dwFreeSpace : DWORD;
            szPath : array[0..(260)-1] of TCHAR;
            szVolume : array[0..(14)-1] of TCHAR;
            szShare : array[0..(128)-1] of TCHAR;
         end;
       FMS_GETFILESEL = record
            ftTime : FILETIME;
            dwSize : DWORD;
            bAttr : BYTE;
            szName : array[0..(260)-1] of TCHAR;
         end;
       FMS_LOAD = record
            dwSize : DWORD;
            szMenuName : array[0..(MENU_TEXT_LEN)-1] of TCHAR;
            hMenu : HMENU;
            wMenuDelta : UINT;
         end;
       FMS_TOOLBARLOAD = record
            dwSize : DWORD;
            lpButtons : LPEXT_BUTTON;
            cButtons : WORD;
            cBitmaps : WORD;
            idBitmap : WORD;
            hBitmap : HBITMAP;
         end;
       FOCUS_EVENT_RECORD = record
            bSetFocus : WINBOOL;
         end;
       FORM_INFO_1 = record
            Flags : DWORD;
            pName : LPTSTR;
            Size : SIZEL;
            ImageableArea : RECTL;
         end;
       FORMAT_PARAMETERS = record
            MediaType : MEDIA_TYPE;
            StartCylinderNumber : DWORD;
            EndCylinderNumber : DWORD;
            StartHeadNumber : DWORD;
            EndHeadNumber : DWORD;
         end;
       FORMATRANGE = record
            hdc : HDC;
            hdcTarget : HDC;
            rc : RECT;
            rcPage : RECT;
            chrg : CHARRANGE;
         end;
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
       GENERIC_MAPPING = record
            GenericRead : ACCESS_MASK;
            GenericWrite : ACCESS_MASK;
            GenericExecute : ACCESS_MASK;
            GenericAll : ACCESS_MASK;
         end;
       PGENERIC_MAPPING = ^GENERIC_MAPPING;
       GLYPHMETRICS = record
            gmBlackBoxX : UINT;
            gmBlackBoxY : UINT;
            gmptGlyphOrigin : POINT;
            gmCellIncX : integer;
            gmCellIncY : integer;
         end;
       LPGLYPHMETRICS = ^GLYPHMETRICS;
       HANDLETABLE = record
            objectHandle : array[0..0] of HGDIOBJ;
         end;
       LPHANDLETABLE = ^HANDLETABLE;
       HD_HITTESTINFO = record
            pt : POINT;
            flags : UINT;
            iItem : longint;
         end;
       HD_ITEM = record
            mask : UINT;
            cxy : longint;
            pszText : LPTSTR;
            hbm : HBITMAP;
            cchTextMax : longint;
            fmt : longint;
            lParam : LPARAM;
         end;
       WINDOWPOS = record
            hwnd : HWND;
            hwndInsertAfter : HWND;
            x : longint;
            y : longint;
            cx : longint;
            cy : longint;
            flags : UINT;
         end;
       PWINDOWPOS = ^WINDOWPOS;
       LPWINDOWPOS = ^WINDOWPOS;
       HD_LAYOUT = record
            prc : ^RECT;
            pwpos : ^WINDOWPOS;
         end;
       HD_NOTIFY = record
            hdr : NMHDR;
            iItem : longint;
            iButton : longint;
            pitem : ^HD_ITEM;
         end;
       HELPINFO = record
            cbSize : UINT;
            iContextType : longint;
            iCtrlId : longint;
            hItemHandle : HANDLE;
            dwContextId : DWORD;
            MousePos : POINT;
         end;
       LPHELPINFO = ^HELPINFO;
       HELPWININFO = record
            wStructSize : longint;
            x : longint;
            y : longint;
            dx : longint;
            dy : longint;
            wMax : longint;
            rgchMember : array[0..(2)-1] of TCHAR;
         end;
       HIGHCONTRAST = record
            cbSize : UINT;
            dwFlags : DWORD;
            lpszDefaultScheme : LPTSTR;
         end;
       LPHIGHCONTRAST = ^HIGHCONTRAST;
       HSZPAIR = record
            hszSvc : HSZ;
            hszTopic : HSZ;
         end;
       ICONINFO = record
            fIcon : WINBOOL;
            xHotspot : DWORD;
            yHotspot : DWORD;
            hbmMask : HBITMAP;
            hbmColor : HBITMAP;
         end;
       PICONINFO = ^ICONINFO;
       ICONMETRICS = record
            cbSize : UINT;
            iHorzSpacing : longint;
            iVertSpacing : longint;
            iTitleWrap : longint;
            lfFont : LOGFONT;
         end;
       LPICONMETRICS = ^ICONMETRICS;
       IMAGEINFO = record
            hbmImage : HBITMAP;
            hbmMask : HBITMAP;
            Unused1 : longint;
            Unused2 : longint;
            rcImage : RECT;
         end;
       KEY_EVENT_RECORD = record
            bKeyDown : WINBOOL;
            wRepeatCount : WORD;
            wVirtualKeyCode : WORD;
            wVirtualScanCode : WORD;
            uChar : record
                case longint of
                   0 : ( UnicodeChar:WCHAR );
                   1 : ( AsciiChar:CHAR );
              end;
            dwControlKeyState : DWORD;
         end;
       MOUSE_EVENT_RECORD = record
            dwMousePosition : COORD;
            dwButtonState : DWORD;
            dwControlKeyState : DWORD;
            dwEventFlags : DWORD;
         end;
       WINDOW_BUFFER_SIZE_RECORD = record
            dwSize : COORD;
         end;
       MENU_EVENT_RECORD = record
            dwCommandId : UINT;
         end;
       PMENU_EVENT_RECORD = ^MENU_EVENT_RECORD;
       INPUT_RECORD = record
            EventType : WORD;
            Event : record
                case longint of
                   0 : ( KeyEvent:KEY_EVENT_RECORD );
                   1 : ( MouseEvent:MOUSE_EVENT_RECORD );
                   2 : ( WindowBufferSizeEvent:WINDOW_BUFFER_SIZE_RECORD );
                   3 : ( MenuEvent:MENU_EVENT_RECORD );
                   4 : ( FocusEvent:FOCUS_EVENT_RECORD );
              end;
         end;
       PINPUT_RECORD = ^INPUT_RECORD;
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
       SID_IDENTIFIER_AUTHORITY = record
            Value : array[0..5] of BYTE;
         end;
       PSID_IDENTIFIER_AUTHORITY = ^SID_IDENTIFIER_AUTHORITY;
       LPSID_IDENTIFIER_AUTHORITY = ^SID_IDENTIFIER_AUTHORITY;
       SID = record
            Revision : BYTE;
            SubAuthorityCount : BYTE;
            IdentifierAuthority : SID_IDENTIFIER_AUTHORITY;
            SubAuthority : array[0..(ANYSIZE_ARRAY)-1] of DWORD;
         end;
       PSID = ^SID;
       SECURITY_DESCRIPTOR_CONTROL = WORD;
       PSECURITY_DESCRIPTOR_CONTROL = ^WORD;
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
       KERNINGPAIR = record
            wFirst : WORD;
            wSecond : WORD;
            iKernAmount : longint;
         end;
       LPKERNINGPAIR = ^KERNINGPAIR;
       LANA_ENUM = record
            length : UCHAR;
            lana : array[0..(MAX_LANA)-1] of UCHAR;
         end;
       LOCALESIGNATURE = record
            lsUsb : array[0..(4)-1] of DWORD;
            lsCsbDefault : array[0..(2)-1] of DWORD;
            lsCsbSupported : array[0..(2)-1] of DWORD;
         end;
       LOCALGROUP_MEMBERS_INFO_0 = record
            lgrmi0_sid : PSID;
         end;
       LOCALGROUP_MEMBERS_INFO_3 = record
            lgrmi3_domainandname : LPWSTR;
         end;
       FXPT16DOT16 = longint;
       LPFXPT16DOT16 = ^longint;
       LUID = LARGE_INTEGER;
       PLUID = ^LARGE_INTEGER;
       LUID_AND_ATTRIBUTES = record
            Luid : LUID;
            Attributes : DWORD;
         end;
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
       LV_DISPINFO = record
            hdr : NMHDR;
            item : LV_ITEM;
         end;
       LV_FINDINFO = record
            flags : UINT;
            psz : LPCTSTR;
            lParam : LPARAM;
            pt : POINT;
            vkDirection : UINT;
         end;
       LV_HITTESTINFO = record
            pt : POINT;
            flags : UINT;
            iItem : longint;
         end;
       LV_KEYDOWN = record
            hdr : NMHDR;
            wVKey : WORD;
            flags : UINT;
         end;
       MAT2 = record
            eM11 : FIXED;
            eM12 : FIXED;
            eM21 : FIXED;
            eM22 : FIXED;
         end;
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
       MEASUREITEMSTRUCT = record
            CtlType : UINT;
            CtlID : UINT;
            itemID : UINT;
            itemWidth : UINT;
            itemHeight : UINT;
            itemData : DWORD;
         end;
       MEMORY_BASIC_INFORMATION = record
            BaseAddress : PVOID;
            AllocationBase : PVOID;
            AllocationProtect : DWORD;
            RegionSize : DWORD;
            State : DWORD;
            Protect : DWORD;
            Type : DWORD;
         end;
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
            szText : array[0..(1)-1] of WCHAR;
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
       MENUTEMPLATE = VOID;
       LPMENUTEMPLATE = ^VOID;
       METAFILEPICT = record
            mm : LONG;
            xExt : LONG;
            yExt : LONG;
            hMF : HMETAFILE;
         end;
       METAHEADER = record
            mtType : WORD;
            mtHeaderSize : WORD;
            mtVersion : WORD;
            mtSize : DWORD;
            mtNoObjects : WORD;
            mtMaxRecord : DWORD;
            mtNoParameters : WORD;
         end;
       METARECORD = record
            rdSize : DWORD;
            rdFunction : WORD;
            rdParm : array[0..0] of WORD;
         end;
       LPMETARECORD = ^METARECORD;
       MINIMIZEDMETRICS = record
            cbSize : UINT;
            iWidth : longint;
            iHorzGap : longint;
            iVertGap : longint;
            iArrange : longint;
         end;
       LPMINIMIZEDMETRICS = ^MINIMIZEDMETRICS;
       MINMAXINFO = record
            ptReserved : POINT;
            ptMaxSize : POINT;
            ptMaxPosition : POINT;
            ptMinTrackSize : POINT;
            ptMaxTrackSize : POINT;
         end;
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
       MONERRSTRUCT = record
            cb : UINT;
            wLastError : UINT;
            dwTime : DWORD;
            hTask : HANDLE;
         end;
       MONHSZSTRUCT = record
            cb : UINT;
            fsAction : WINBOOL;
            dwTime : DWORD;
            hsz : HSZ;
            hTask : HANDLE;
            str : array[0..(1)-1] of TCHAR;
         end;
       MONITOR_INFO_1 = record
            pName : LPTSTR;
         end;
       MONITOR_INFO_2 = record
            pName : LPTSTR;
            pEnvironment : LPTSTR;
            pDLLName : LPTSTR;
         end;
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
       MOUSEHOOKSTRUCT = record
            pt : POINT;
            hwnd : HWND;
            wHitTestCode : UINT;
            dwExtraInfo : DWORD;
         end;
       MOUSEKEYS = record
            cbSize : DWORD;
            dwFlags : DWORD;
            iMaxSpeed : DWORD;
            iTimeToMaxSpeed : DWORD;
            iCtrlSpeed : DWORD;
            dwReserved1 : DWORD;
            dwReserved2 : DWORD;
         end;
       MSG = record
            hwnd : HWND;
            message : UINT;
            wParam : WPARAM;
            lParam : LPARAM;
            time : DWORD;
            pt : POINT;
         end;
       LPMSG = ^MSG;
       MSGBOXCALLBACK = procedure(lpHelpInfo:LPHELPINFO);
       MSGBOXPARAMS = record
            cbSize : UINT;
            hwndOwner : HWND;
            hInstance : HINSTANCE;
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
       MULTIKEYHELP = record
            mkSize : DWORD;
            mkKeylist : TCHAR;
            szKeyphrase : array[0..(1)-1] of TCHAR;
         end;
       NAME_BUFFER = record
            name : array[0..(NCBNAMSZ)-1] of UCHAR;
            name_num : UCHAR;
            name_flags : UCHAR;
         end;
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
            ncb_post : procedure(_para1:^*);
            ncb_lana_num : UCHAR;
            ncb_cmd_cplt : UCHAR;
            ncb_reserve : array[0..(10)-1] of UCHAR;
            ncb_event : HANDLE;
         end;
       NCCALCSIZE_PARAMS = record
            rgrc : array[0..(3)-1] of RECT;
            lppos : PWINDOWPOS;
         end;
       NDDESHAREINFO = record
            lRevision : LONG;
            lpszShareName : LPTSTR;
            lShareType : LONG;
            lpszAppTopicList : LPTSTR;
            fSharedFlag : LONG;
            fService : LONG;
            fStartAppFlag : LONG;
            nCmdShow : LONG;
            qModifyId : array[0..(2)-1] of LONG;
            cNumItems : LONG;
            lpszItemList : LPTSTR;
         end;
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
       NEWCPLINFO = record
            dwSize : DWORD;
            dwFlags : DWORD;
            dwHelpContext : DWORD;
            lData : LONG;
            hIcon : HICON;
            szName : array[0..31] of TCHAR;
            szInfo : array[0..(63] of TCHAR;
            szHelpFile : array[0..127] of TCHAR;
         end;
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
       NEWTEXTMETRICEX = record
            ntmentm : NEWTEXTMETRIC;
            ntmeFontSignature : FONTSIGNATURE;
         end;
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
       HTREEITEM = ^*;
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
       NM_TREEVIEW = record
            hdr : NMHDR;
            action : UINT;
            itemOld : TV_ITEM;
            itemNew : TV_ITEM;
            ptDrag : POINT;
         end;
       LPNM_TREEVIEW = ^NM_TREEVIEW;
       NM_UPDOWNW = record
            hdr : NMHDR;
            iPos : longint;
            iDelta : longint;
         end;
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
       SERVICE_ADDRESS = record
            dwAddressType : DWORD;
            dwAddressFlags : DWORD;
            dwAddressLength : DWORD;
            dwPrincipalLength : DWORD;
            lpAddress : ^BYTE;
            lpPrincipal : ^BYTE;
         end;
       SERVICE_ADDRESSES = record
            dwAddressCount : DWORD;
            Addresses : array[0..0] of SERVICE_ADDRESS;
         end;
       LPSERVICE_ADDRESSES = ^SERVICE_ADDRESSES;
       GUID = record
            Data1 : cardinal;
            Data2 : word;
            Data3 : word;
            Data4 : array[0..7] of char;
         end;
       LPGUID = ^GUID;
       CLSID = GUID;
       LPCLSID = ^GUID;
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
       NS_SERVICE_INFO = record
            dwNameSpace : DWORD;
            ServiceInfo : SERVICE_INFO;
         end;
       NUMBERFMT = record
            NumDigits : UINT;
            LeadingZero : UINT;
            Grouping : UINT;
            lpDecimalSep : LPTSTR;
            lpThousandSep : LPTSTR;
            NegativeOrder : UINT;
         end;
       OFSTRUCT = record
            cBytes : BYTE;
            fFixedDisk : BYTE;
            nErrCode : WORD;
            Reserved1 : WORD;
            Reserved2 : WORD;
            szPathName : array[0..(OFS_MAXPATHNAME)-1] of CHAR;
         end;
       LPOFSTRUCT = ^OFSTRUCT;
       OPENFILENAME = record
            lStructSize : DWORD;
            hwndOwner : HWND;
            hInstance : HINSTANCE;
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
       OFNOTIFY = record
            hdr : NMHDR;
            lpOFN : LPOPENFILENAME;
            pszFile : LPTSTR;
         end;
       LPOFNOTIFY = ^OFNOTIFY;
       OSVERSIONINFO = record
            dwOSVersionInfoSize : DWORD;
            dwMajorVersion : DWORD;
            dwMinorVersion : DWORD;
            dwBuildNumber : DWORD;
            dwPlatformId : DWORD;
            szCSDVersion : array[0..(128)-1] of TCHAR;
         end;
       POSVERSIONINFO = ^OSVERSIONINFO;
       LPOSVERSIONINFO = ^OSVERSIONINFO;
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
       OVERLAPPED = record
            Internal : DWORD;
            InternalHigh : DWORD;
            Offset : DWORD;
            OffsetHigh : DWORD;
            hEvent : HANDLE;
         end;
       LPOVERLAPPED = ^OVERLAPPED;
       PAGESETUPDLG = record
            lStructSize : DWORD;
            hwndOwner : HWND;
            hDevMode : HGLOBAL;
            hDevNames : HGLOBAL;
            Flags : DWORD;
            ptPaperSize : POINT;
            rtMinMargin : RECT;
            rtMargin : RECT;
            hInstance : HINSTANCE;
            lCustData : LPARAM;
            lpfnPageSetupHook : LPPAGESETUPHOOK;
            lpfnPagePaintHook : LPPAGEPAINTHOOK;
            lpPageSetupTemplateName : LPCTSTR;
            hPageSetupTemplate : HGLOBAL;
         end;
       LPPAGESETUPDLG = ^PAGESETUPDLG;
       PAINTSTRUCT = record
            hdc : HDC;
            fErase : WINBOOL;
            rcPaint : RECT;
            fRestore : WINBOOL;
            fIncUpdate : WINBOOL;
            rgbReserved : array[0..(32)-1] of BYTE;
         end;
       LPPAINTSTRUCT = ^PAINTSTRUCT;
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
       PERF_COUNTER_BLOCK = record
            ByteLength : DWORD;
         end;
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
       PERF_DATA_BLOCK = record
            Signature : array[0..(4)-1] of WCHAR;
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
       PERF_INSTANCE_DEFINITION = record
            ByteLength : DWORD;
            ParentObjectTitleIndex : DWORD;
            ParentObjectInstance : DWORD;
            UniqueID : DWORD;
            NameOffset : DWORD;
            NameLength : DWORD;
         end;
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
       POLYTEXT = record
            x : longint;
            y : longint;
            n : UINT;
            lpstr : LPCTSTR;
            uiFlags : UINT;
            rcl : RECT;
            pdx : ^longint;
         end;
       PORT_INFO_1 = record
            pName : LPTSTR;
         end;
       PORT_INFO_2 = record
            pPortName : LPSTR;
            pMonitorName : LPSTR;
            pDescription : LPSTR;
            fPortType : DWORD;
            Reserved : DWORD;
         end;
       PREVENT_MEDIA_REMOVAL = record
            PreventMediaRemoval : BOOLEAN;
         end;
       PRINTDLG = record
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
            hInstance : HINSTANCE;
            lCustData : DWORD;
            lpfnPrintHook : LPPRINTHOOKPROC;
            lpfnSetupHook : LPSETUPHOOKPROC;
            lpPrintTemplateName : LPCTSTR;
            lpSetupTemplateName : LPCTSTR;
            hPrintTemplate : HANDLE;
            hSetupTemplate : HANDLE;
         end;
       LPPRINTDLG = ^PRINTDLG;
       PRINTER_DEFAULTS = record
            pDatatype : LPTSTR;
            pDevMode : LPDEVMODE;
            DesiredAccess : ACCESS_MASK;
         end;
       PRINTER_INFO_1 = record
            Flags : DWORD;
            pDescription : LPTSTR;
            pName : LPTSTR;
            pComment : LPTSTR;
         end;
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
       PRINTER_INFO_3 = record
            pSecurityDescriptor : PSECURITY_DESCRIPTOR;
         end;
       PRINTER_INFO_4 = record
            pPrinterName : LPTSTR;
            pServerName : LPTSTR;
            Attributes : DWORD;
         end;
       PRINTER_INFO_5 = record
            pPrinterName : LPTSTR;
            pPortName : LPTSTR;
            Attributes : DWORD;
            DeviceNotSelectedTimeout : DWORD;
            TransmissionRetryTimeout : DWORD;
         end;
       PRINTER_NOTIFY_INFO_DATA = record
            Type : WORD;
            Field : WORD;
            Reserved : DWORD;
            Id : DWORD;
            NotifyData : record
                case longint of
                   0 : ( adwData:array[0..(2)-1] of DWORD );
                   1 : ( Data:record
                        cbBuf : DWORD;
                        pBuf : LPVOID;
                     end );
              end;
         end;
       PRINTER_NOTIFY_INFO = record
            Version : DWORD;
            Flags : DWORD;
            Count : DWORD;
            aData : array[0..(1)-1] of PRINTER_NOTIFY_INFO_DATA;
         end;
       PRINTER_NOTIFY_OPTIONS_TYPE = record
            Type : WORD;
            Reserved0 : WORD;
            Reserved1 : DWORD;
            Reserved2 : DWORD;
            Count : DWORD;
            pFields : PWORD;
         end;
       PPRINTER_NOTIFY_OPTIONS_TYPE = ^PRINTER_NOTIFY_OPTIONS_TYPE;
       PRINTER_NOTIFY_OPTIONS = record
            Version : DWORD;
            Flags : DWORD;
            Count : DWORD;
            pTypes : PPRINTER_NOTIFY_OPTIONS_TYPE;
         end;
       PRINTPROCESSOR_INFO_1 = record
            pName : LPTSTR;
         end;
       PRIVILEGE_SET = record
            PrivilegeCount : DWORD;
            Control : DWORD;
            Privilege : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
         end;
       PPRIVILEGE_SET = ^PRIVILEGE_SET;
       LPPRIVILEGE_SET = ^PRIVILEGE_SET;
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
       LPPROCESS_HEAP_ENTRY = ^PROCESS_HEAP_ENTRY;
       PROCESS_INFORMATION = record
            hProcess : HANDLE;
            hThread : HANDLE;
            dwProcessId : DWORD;
            dwThreadId : DWORD;
         end;
       LPPROCESS_INFORMATION = ^PROCESS_INFORMATION;
       LPFNPSPCALLBACK = function(_para1:HWND; _para2:UINT; _para3:LPVOID):UINT;
       PROPSHEETPAGE = record
            dwSize : DWORD;
            dwFlags : DWORD;
            hInstance : HINSTANCE;
            u1 : record
                case longint of
                   0 : ( pszTemplate:LPCTSTR );
                   1 : ( pResource:LPCDLGTEMPLATE );
              end;
            u2 : record
                case longint of
                   0 : ( hIcon:HICON );
                   1 : ( pszIcon:LPCTSTR );
              end;
            pszTitle : LPCTSTR;
            pfnDlgProc : DLGPROC;
            lParam : LPARAM;
            pfnCallback : LPFNPSPCALLBACK;
            pcRefParent : ^UINT;
         end;
       LPPROPSHEETPAGE = ^PROPSHEETPAGE;
       LPCPROPSHEETPAGE = ^PROPSHEETPAGE;
       HPROPSHEETPAGE = ^*;
       PROPSHEETHEADER = record
            dwSize : DWORD;
            dwFlags : DWORD;
            hwndParent : HWND;
            hInstance : HINSTANCE;
            u1 : record
                case longint of
                   0 : ( hIcon:HICON );
                   1 : ( pszIcon:LPCTSTR );
              end;
            pszCaption : LPCTSTR;
            nPages : UINT;
            u2 : record
                case longint of
                   0 : ( nStartPage:UINT );
                   1 : ( pStartPage:LPCTSTR );
              end;
            u3 : record
                case longint of
                   0 : ( ppsp:LPCPROPSHEETPAGE );
                   1 : ( phpage:^HPROPSHEETPAGE );
              end;
            pfnCallback : PFNPROPSHEETCALLBACK;
         end;
       LPPROPSHEETHEADER = ^PROPSHEETHEADER;
       LPCPROPSHEETHEADER = ^PROPSHEETHEADER;
    { PropertySheet callbacks}
       LPFNADDPROPSHEETPAGE = function(_para1:HPROPSHEETPAGE; _para2:LPARAM):WINBOOL;
       LPFNADDPROPSHEETPAGES = function(_para1:LPVOID; _para2:LPFNADDPROPSHEETPAGE; _para3:LPARAM):WINBOOL;
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
       PROVIDOR_INFO_1 = record
            pName : LPTSTR;
            pEnvironment : LPTSTR;
            pDLLName : LPTSTR;
         end;
       PSHNOTIFY = record
            hdr : NMHDR;
            lParam : LPARAM;
         end;
       LPPSHNOTIFY = ^PSHNOTIFY;
       PUNCTUATION = record
            iSize : UINT;
            szPunctuation : LPSTR;
         end;
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
       QUERY_SERVICE_LOCK_STATUS = record
            fIsLocked : DWORD;
            lpLockOwner : LPTSTR;
            dwLockDuration : DWORD;
         end;
       LPQUERY_SERVICE_LOCK_STATUS = ^QUERY_SERVICE_LOCK_STATUS;
       RASAMB = record
            dwSize : DWORD;
            dwError : DWORD;
            szNetBiosError : array[0..((NETBIOS_NAME_LEN) + (1))-1] of TCHAR;
            bLana : BYTE;
         end;
       RASCONN = record
            dwSize : DWORD;
            hrasconn : HRASCONN;
            szEntryName : array[0..((RAS_MaxEntryName) + (1))-1] of TCHAR;
            szDeviceType : array[0..((RAS_MaxDeviceType) + (1))-1] of CHAR;
            szDeviceName : array[0..((RAS_MaxDeviceName) + (1))-1] of CHAR;
         end;
       RASCONNSTATUS = record
            dwSize : DWORD;
            rasconnstate : RASCONNSTATE;
            dwError : DWORD;
            szDeviceType : array[0..((RAS_MaxDeviceType) + (1))-1] of TCHAR;
            szDeviceName : array[0..((RAS_MaxDeviceName) + (1))-1] of TCHAR;
         end;
       RASDIALEXTENSIONS = record
            dwSize : DWORD;
            dwfOptions : DWORD;
            hwndParent : HWND;
            reserved : DWORD;
         end;
       RASDIALPARAMS = record
            dwSize : DWORD;
            szEntryName : array[0..((RAS_MaxEntryName) + (1))-1] of TCHAR;
            szPhoneNumber : array[0..((RAS_MaxPhoneNumber) + (1))-1] of TCHAR;
            szCallbackNumber : array[0..((RAS_MaxCallbackNumber) + (1))-1] of TCHAR;
            szUserName : array[0..((UNLEN) + (1))-1] of TCHAR;
            szPassword : array[0..((PWLEN) + (1))-1] of TCHAR;
            szDomain : array[0..((DNLEN) + (1))-1] of TCHAR;
         end;
       RASENTRYNAME = record
            dwSize : DWORD;
            szEntryName : array[0..((RAS_MaxEntryName) + (1))-1] of TCHAR;
         end;
       RASPPPIP = record
            dwSize : DWORD;
            dwError : DWORD;
            szIpAddress : array[0..((RAS_MaxIpAddress) + (1))-1] of TCHAR;
         end;
       RASPPPIPX = record
            dwSize : DWORD;
            dwError : DWORD;
            szIpxAddress : array[0..((RAS_MaxIpxAddress) + (1))-1] of TCHAR;
         end;
       RASPPPNBF = record
            dwSize : DWORD;
            dwError : DWORD;
            dwNetBiosError : DWORD;
            szNetBiosError : array[0..((NETBIOS_NAME_LEN) + (1))-1] of TCHAR;
            szWorkstationName : array[0..((NETBIOS_NAME_LEN) + (1))-1] of TCHAR;
            bLana : BYTE;
         end;
       RASTERIZER_STATUS = record
            nSize : integer;
            wFlags : integer;
            nLanguageID : integer;
         end;
       LPRASTERIZER_STATUS = ^RASTERIZER_STATUS;
       REASSIGN_BLOCKS = record
            Reserved : WORD;
            Count : WORD;
            BlockNumber : array[0..(1)-1] of DWORD;
         end;
       REMOTE_NAME_INFO = record
            lpUniversalName : LPTSTR;
            lpConnectionName : LPTSTR;
            lpRemainingPath : LPTSTR;
         end;
    { TODO: OLE}
    {typedef struct _reobject { }
    {  DWORD  cbStruct;           }
    {  LONG   cp;                 }
    {  CLSID  clsid;              }
    {  LPOLEOBJECT      poleobj;  }
    {  LPSTORAGE        pstg;     }
    {  LPOLECLIENTSITE  polesite; }
    {  SIZEL  sizel;              }
    {  DWORD  dvaspect;           }
    {  DWORD  dwFlags;            }
    {  DWORD  dwUser;             }
    {} REOBJECT; }
       REPASTESPECIAL = record
            dwAspect : DWORD;
            dwParam : DWORD;
         end;
       REQRESIZE = record
            nmhdr : NMHDR;
            rc : RECT;
         end;
       RGNDATAHEADER = record
            dwSize : DWORD;
            iType : DWORD;
            nCount : DWORD;
            nRgnSize : DWORD;
            rcBound : RECT;
         end;
       RGNDATA = record
            rdh : RGNDATAHEADER;
            Buffer : array[0..(1)-1] of char;
         end;
       LPRGNDATA = ^RGNDATA;
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
       LPCSCROLLINFO = ^SCROLLINFO;
       SECURITY_ATTRIBUTES = record
            nLength : DWORD;
            lpSecurityDescriptor : LPVOID;
            bInheritHandle : WINBOOL;
         end;
       LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
       SECURITY_INFORMATION = DWORD;
       PSECURITY_INFORMATION = ^DWORD;
       SELCHANGE = record
            nmhdr : NMHDR;
            chrg : CHARRANGE;
            seltyp : WORD;
         end;
       SERIALKEYS = record
            cbSize : DWORD;
            dwFlags : DWORD;
            lpszActivePort : LPSTR;
            lpszPort : LPSTR;
            iBaudRate : DWORD;
            iPortState : DWORD;
         end;
       LPSERIALKEYS = ^SERIALKEYS;
       SERVICE_TABLE_ENTRY = record
            lpServiceName : LPTSTR;
            lpServiceProc : LPSERVICE_MAIN_FUNCTION;
         end;
       LPSERVICE_TABLE_ENTRY = ^SERVICE_TABLE_ENTRY;
       SERVICE_TYPE_VALUE_ABS = record
            dwNameSpace : DWORD;
            dwValueType : DWORD;
            dwValueSize : DWORD;
            lpValueName : LPTSTR;
            lpValue : PVOID;
         end;
       SERVICE_TYPE_INFO_ABS = record
            lpTypeName : LPTSTR;
            dwValueCount : DWORD;
            Values : array[0..(1)-1] of SERVICE_TYPE_VALUE_ABS;
         end;
       SESSION_BUFFER = record
            lsn : UCHAR;
            state : UCHAR;
            local_name : array[0..(NCBNAMSZ)-1] of UCHAR;
            remote_name : array[0..(NCBNAMSZ)-1] of UCHAR;
            rcvs_outstanding : UCHAR;
            sends_outstanding : UCHAR;
         end;
       SESSION_HEADER = record
            sess_name : UCHAR;
            num_sess : UCHAR;
            rcv_dg_outstanding : UCHAR;
            rcv_any_outstanding : UCHAR;
         end;
       SET_PARTITION_INFORMATION = record
            PartitionType : BYTE;
         end;
       SHFILEINFO = record
            hIcon : HICON;
            iIcon : longint;
            dwAttributes : DWORD;
            szDisplayName : array[0..(MAX_PATH)-1] of char;
            szTypeName : array[0..(80)-1] of char;
         end;
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
       SHNAMEMAPPING = record
            pszOldPath : LPSTR;
            pszNewPath : LPSTR;
            cchOldPath : longint;
            cchNewPath : longint;
         end;
       LPSHNAMEMAPPING = ^SHNAMEMAPPING;
       SID_AND_ATTRIBUTES = record
            Sid : PSID;
            Attributes : DWORD;
         end;
       SID_AND_ATTRIBUTES_ARRAY = array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;
       PSID_AND_ATTRIBUTES_ARRAY = ^SID_AND_ATTRIBUTES_ARRAY;
       SINGLE_LIST_ENTRY = record
            Next : ^*;
         end;
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
       STICKYKEYS = record
            cbSize : DWORD;
            dwFlags : DWORD;
         end;
       LPSTICKYKEYS = ^STICKYKEYS;
       STRRET = record
            uType : UINT;
            DUMMYUNIONNAME : record
                case longint of
                   0 : ( pOleStr:LPWSTR );
                   1 : ( uOffset:UINT );
                   2 : ( cStr:array[0..(MAX_PATH)-1] of char );
              end;
         end;
       LPSTRRET = ^STRRET;
       STYLEBUF = record
            dwStyle : DWORD;
            szDescription : array[0..(32)-1] of CHAR;
         end;
       LPSTYLEBUF = ^record
            dwStyle : DWORD;
            szDescription : array[0..(32)-1] of CHAR;
         end;
       STYLESTRUCT = record
            styleOld : DWORD;
            styleNew : DWORD;
         end;
       LPSTYLESTRUCT = ^record
            styleOld : DWORD;
            styleNew : DWORD;
         end;
       SYSTEM_AUDIT_ACE = record
            Header : ACE_HEADER;
            Mask : ACCESS_MASK;
            SidStart : DWORD;
         end;
       SYSTEM_INFO = record
            wProcessorArchitecture : WORD;
            wReserved : WORD;
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
       LPSYSTEM_INFO = ^record
            wProcessorArchitecture : WORD;
            wReserved : WORD;
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
       SYSTEM_POWER_STATUS = record
            ACLineStatus : BYTE;
            BatteryFlag : BYTE;
            BatteryLifePercent : BYTE;
            Reserved1 : BYTE;
            BatteryLifeTime : DWORD;
            BatteryFullLifeTime : DWORD;
         end;
       LPSYSTEM_POWER_STATUS = ^*;
       TAPE_ERASE = record
            Type : ULONG;
         end;
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
       TAPE_GET_MEDIA_PARAMETERS = record
            Capacity : LARGE_INTEGER;
            Remaining : LARGE_INTEGER;
            BlockSize : DWORD;
            PartitionCount : DWORD;
            WriteProtected : BOOLEAN;
         end;
       TAPE_GET_POSITION = record
            Type : ULONG;
            Partition : ULONG;
            OffsetLow : ULONG;
            OffsetHigh : ULONG;
         end;
       TAPE_PREPARE = record
            Operation : ULONG;
         end;
       TAPE_SET_DRIVE_PARAMETERS = record
            ECC : BOOLEAN;
            Compression : BOOLEAN;
            DataPadding : BOOLEAN;
            ReportSetmarks : BOOLEAN;
            EOTWarningZoneSize : ULONG;
         end;
       TAPE_SET_MEDIA_PARAMETERS = record
            BlockSize : ULONG;
         end;
       TAPE_SET_POSITION = record
            Method : ULONG;
            Partition : ULONG;
            OffsetLow : ULONG;
            OffsetHigh : ULONG;
         end;
       TAPE_WRITE_MARKS = record
            Type : ULONG;
            Count : ULONG;
         end;
       TBADDBITMAP = record
            hInst : HINSTANCE;
            nID : UINT;
         end;
       LPTBADDBITMAP = ^record
            hInst : HINSTANCE;
            nID : UINT;
         end;
       TBBUTTON = record
            iBitmap : longint;
            idCommand : longint;
            fsState : BYTE;
            fsStyle : BYTE;
            dwData : DWORD;
            iString : longint;
         end;
       PTBBUTTON = ^record
            iBitmap : longint;
            idCommand : longint;
            fsState : BYTE;
            fsStyle : BYTE;
            dwData : DWORD;
            iString : longint;
         end;
       LPTBBUTTON = ^record
            iBitmap : longint;
            idCommand : longint;
            fsState : BYTE;
            fsStyle : BYTE;
            dwData : DWORD;
            iString : longint;
         end;
       LPCTBBUTTON = ^TBBUTTON;
       TBNOTIFY = record
            hdr : NMHDR;
            iItem : longint;
            tbButton : TBBUTTON;
            cchText : longint;
            pszText : LPTSTR;
         end;
       LPTBNOTIFY = ^record
            hdr : NMHDR;
            iItem : longint;
            tbButton : TBBUTTON;
            cchText : longint;
            pszText : LPTSTR;
         end;
       TBSAVEPARAMS = record
            hkr : HKEY;
            pszSubKey : LPCTSTR;
            pszValueName : LPCTSTR;
         end;
       TC_HITTESTINFO = record
            pt : POINT;
            flags : UINT;
         end;
       TC_ITEM = record
            mask : UINT;
            lpReserved1 : UINT;
            lpReserved2 : UINT;
            pszText : LPTSTR;
            cchTextMax : longint;
            iImage : longint;
            lParam : LPARAM;
         end;
       TC_ITEMHEADER = record
            mask : UINT;
            lpReserved1 : UINT;
            lpReserved2 : UINT;
            pszText : LPTSTR;
            cchTextMax : longint;
            iImage : longint;
         end;
       TC_KEYDOWN = record
            hdr : NMHDR;
            wVKey : WORD;
            flags : UINT;
         end;
       TEXTRANGE = record
            chrg : CHARRANGE;
            lpstrText : LPSTR;
         end;
       TIME_ZONE_INFORMATION = record
            Bias : LONG;
            StandardName : array[0..(32)-1] of WCHAR;
            StandardDate : SYSTEMTIME;
            StandardBias : LONG;
            DaylightName : array[0..(32)-1] of WCHAR;
            DaylightDate : SYSTEMTIME;
            DaylightBias : LONG;
         end;
       LPTIME_ZONE_INFORMATION = ^record
            Bias : LONG;
            StandardName : array[0..(32)-1] of WCHAR;
            StandardDate : SYSTEMTIME;
            StandardBias : LONG;
            DaylightName : array[0..(32)-1] of WCHAR;
            DaylightDate : SYSTEMTIME;
            DaylightBias : LONG;
         end;
       TOGGLEKEYS = record
            cbSize : DWORD;
            dwFlags : DWORD;
         end;
       TOKEN_SOURCE = record
            SourceName : array[0..(8)-1] of CHAR;
            SourceIdentifier : LUID;
         end;
       TOKEN_CONTROL = record
            TokenId : LUID;
            AuthenticationId : LUID;
            ModifiedId : LUID;
            TokenSource : TOKEN_SOURCE;
         end;
       TOKEN_DEFAULT_DACL = record
            DefaultDacl : PACL;
         end;
       TOKEN_GROUPS = record
            GroupCount : DWORD;
            Groups : array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;
         end;
       PTOKEN_GROUPS = ^record
            GroupCount : DWORD;
            Groups : array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;
         end;
       LPTOKEN_GROUPS = ^record
            GroupCount : DWORD;
            Groups : array[0..(ANYSIZE_ARRAY)-1] of SID_AND_ATTRIBUTES;
         end;
       TOKEN_OWNER = record
            Owner : PSID;
         end;
       TOKEN_PRIMARY_GROUP = record
            PrimaryGroup : PSID;
         end;
       TOKEN_PRIVILEGES = record
            PrivilegeCount : DWORD;
            Privileges : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
         end;
       PTOKEN_PRIVILEGES = ^record
            PrivilegeCount : DWORD;
            Privileges : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
         end;
       LPTOKEN_PRIVILEGES = ^record
            PrivilegeCount : DWORD;
            Privileges : array[0..(ANYSIZE_ARRAY)-1] of LUID_AND_ATTRIBUTES;
         end;
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
       TOKEN_USER = record
            User : SID_AND_ATTRIBUTES;
         end;
       TOOLINFO = record
            cbSize : UINT;
            uFlags : UINT;
            hwnd : HWND;
            uId : UINT;
            rect : RECT;
            hinst : HINSTANCE;
            lpszText : LPTSTR;
         end;
       PTOOLINFO = ^record
            cbSize : UINT;
            uFlags : UINT;
            hwnd : HWND;
            uId : UINT;
            rect : RECT;
            hinst : HINSTANCE;
            lpszText : LPTSTR;
         end;
       LPTOOLINFO = ^record
            cbSize : UINT;
            uFlags : UINT;
            hwnd : HWND;
            uId : UINT;
            rect : RECT;
            hinst : HINSTANCE;
            lpszText : LPTSTR;
         end;
       TOOLTIPTEXT = record
            hdr : NMHDR;
            lpszText : LPTSTR;
            szText : array[0..(80)-1] of char;
            hinst : HINSTANCE;
            uFlags : UINT;
         end;
       LPTOOLTIPTEXT = ^record
            hdr : NMHDR;
            lpszText : LPTSTR;
            szText : array[0..(80)-1] of char;
            hinst : HINSTANCE;
            uFlags : UINT;
         end;
       TPMPARAMS = record
            cbSize : UINT;
            rcExclude : RECT;
         end;
       LPTPMPARAMS = ^record
            cbSize : UINT;
            rcExclude : RECT;
         end;
       TRANSMIT_FILE_BUFFERS = record
            Head : PVOID;
            HeadLength : DWORD;
            Tail : PVOID;
            TailLength : DWORD;
         end;
       TTHITTESTINFO = record
            hwnd : HWND;
            pt : POINT;
            ti : TOOLINFO;
         end;
       LPHITTESTINFO = ^record
            hwnd : HWND;
            pt : POINT;
            ti : TOOLINFO;
         end;
       TTPOLYCURVE = record
            wType : WORD;
            cpfx : WORD;
            apfx : array[0..(1)-1] of POINTFX;
         end;
       LPTTPOLYCURVE = ^record
            wType : WORD;
            cpfx : WORD;
            apfx : array[0..(1)-1] of POINTFX;
         end;
       TTPOLYGONHEADER = record
            cb : DWORD;
            dwType : DWORD;
            pfxStart : POINTFX;
         end;
       LPTTPOLYGONHEADER = ^record
            cb : DWORD;
            dwType : DWORD;
            pfxStart : POINTFX;
         end;
       TV_DISPINFO = record
            hdr : NMHDR;
            item : TV_ITEM;
         end;
       TV_HITTESTINFO = record
            pt : POINT;
            flags : UINT;
            hItem : HTREEITEM;
         end;
       LPTV_HITTESTINFO = ^record
            pt : POINT;
            flags : UINT;
            hItem : HTREEITEM;
         end;
       TV_INSERTSTRUCT = record
            hParent : HTREEITEM;
            hInsertAfter : HTREEITEM;
            item : TV_ITEM;
         end;
       LPTV_INSERTSTRUCT = ^record
            hParent : HTREEITEM;
            hInsertAfter : HTREEITEM;
            item : TV_ITEM;
         end;
       TV_KEYDOWN = record
            hdr : NMHDR;
            wVKey : WORD;
            flags : UINT;
         end;
       TV_SORTCB = record
            hParent : HTREEITEM;
            lpfnCompare : PFNTVCOMPARE;
            lParam : LPARAM;
         end;
       LPTV_SORTCB = ^record
            hParent : HTREEITEM;
            lpfnCompare : PFNTVCOMPARE;
            lParam : LPARAM;
         end;
       UDACCEL = record
            nSec : UINT;
            nInc : UINT;
         end;
       ULARGE_INTEGER = record
            LowPart : DWORD;
            HighPart : DWORD;
         end;
       PULARGE_INTEGER = ^record
            LowPart : DWORD;
            HighPart : DWORD;
         end;
       UNIVERSAL_NAME_INFO = record
            lpUniversalName : LPTSTR;
         end;
       USEROBJECTFLAGS = record
            fInherit : WINBOOL;
            fReserved : WINBOOL;
            dwFlags : DWORD;
         end;
       VALENT = record
            ve_valuename : LPTSTR;
            ve_valuelen : DWORD;
            ve_valueptr : DWORD;
            ve_type : DWORD;
         end;
       PVALENT = ^record
            ve_valuename : LPTSTR;
            ve_valuelen : DWORD;
            ve_valueptr : DWORD;
            ve_type : DWORD;
         end;
       VERIFY_INFORMATION = record
            StartingOffset : LARGE_INTEGER;
            Length : DWORD;
         end;
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
            cAlternateFileName : array[0..(14)-1] of TCHAR;
         end;
       LPWIN32_FIND_DATA = ^record
            dwFileAttributes : DWORD;
            ftCreationTime : FILETIME;
            ftLastAccessTime : FILETIME;
            ftLastWriteTime : FILETIME;
            nFileSizeHigh : DWORD;
            nFileSizeLow : DWORD;
            dwReserved0 : DWORD;
            dwReserved1 : DWORD;
            cFileName : array[0..(MAX_PATH)-1] of TCHAR;
            cAlternateFileName : array[0..(14)-1] of TCHAR;
         end;
       WIN32_STREAM_ID = record
            dwStreamId : DWORD;
            dwStreamAttributes : DWORD;
            Size : LARGE_INTEGER;
            dwStreamNameSize : DWORD;
            cStreamName : ^WCHAR;
         end;
       WINDOWPLACEMENT = record
            length : UINT;
            flags : UINT;
            showCmd : UINT;
            ptMinPosition : POINT;
            ptMaxPosition : POINT;
            rcNormalPosition : RECT;
         end;
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
       LPWNDCLASS = ^record
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
       WNDCLASSEX = record
            cbSize : UINT;
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
            hIconSm : HICON;
         end;
       LPWNDCLASSEX = ^record
            cbSize : UINT;
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
            hIconSm : HICON;
         end;
       CONNECTDLGSTRUCT = record
            cbStructure : DWORD;
            hwndOwner : HWND;
            lpConnRes : LPNETRESOURCE;
            dwFlags : DWORD;
            dwDevNum : DWORD;
         end;
       LPCONNECTDLGSTRUCT = ^record
            cbStructure : DWORD;
            hwndOwner : HWND;
            lpConnRes : LPNETRESOURCE;
            dwFlags : DWORD;
            dwDevNum : DWORD;
         end;
       DISCDLGSTRUCT = record
            cbStructure : DWORD;
            hwndOwner : HWND;
            lpLocalName : LPTSTR;
            lpRemoteName : LPTSTR;
            dwFlags : DWORD;
         end;
       LPDISCDLGSTRUCT = ^record
            cbStructure : DWORD;
            hwndOwner : HWND;
            lpLocalName : LPTSTR;
            lpRemoteName : LPTSTR;
            dwFlags : DWORD;
         end;
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
       LPNETINFOSTRUCT = ^record
            cbStructure : DWORD;
            dwProviderVersion : DWORD;
            dwStatus : DWORD;
            dwCharacteristics : DWORD;
            dwHandle : DWORD;
            wNetType : WORD;
            dwPrinters : DWORD;
            dwDrives : DWORD;
         end;
       NETCONNECTINFOSTRUCT = record
            cbStructure : DWORD;
            dwFlags : DWORD;
            dwSpeed : DWORD;
            dwDelay : DWORD;
            dwOptDataSize : DWORD;
         end;
       LPNETCONNECTINFOSTRUCT = ^record
            cbStructure : DWORD;
            dwFlags : DWORD;
            dwSpeed : DWORD;
            dwDelay : DWORD;
            dwOptDataSize : DWORD;
         end;
       ENUMMETAFILEPROC = function(_para1:HDC; _para2:HANDLETABLE; _para3:METARECORD; _para4:longint; _para5:LPARAM):longint;
       ENHMETAFILEPROC = function(_para1:HDC; _para2:HANDLETABLE; _para3:ENHMETARECORD; _para4:longint; _para5:LPARAM):longint;
       ENUMFONTSPROC = function(_para1:LPLOGFONT; _para2:LPTEXTMETRIC; _para3:DWORD; _para4:LPARAM):longint;
       FONTENUMPROC = function(_para1:^ENUMLOGFONT; _para2:^NEWTEXTMETRIC; _para3:longint; _para4:LPARAM):longint;
       FONTENUMEXPROC = function(_para1:^ENUMLOGFONTEX; _para2:^NEWTEXTMETRICEX; _para3:longint; _para4:LPARAM):longint;
       LPOVERLAPPED_COMPLETION_ROUTINE = function(_para1:DWORD; _para2:DWORD; _para3:LPOVERLAPPED):VOID;
    {
      Structures for the extensions to OpenGL
      }
       POINTFLOAT = record
            x : FLOAT;
            y : FLOAT;
         end;
       PPOINTFLOAT = ^record
            x : FLOAT;
            y : FLOAT;
         end;
       GLYPHMETRICSFLOAT = record
            gmfBlackBoxX : FLOAT;
            gmfBlackBoxY : FLOAT;
            gmfptGlyphOrigin : POINTFLOAT;
            gmfCellIncX : FLOAT;
            gmfCellIncY : FLOAT;
         end;
       PGLYPHMETRICSFLOAT = ^record
            gmfBlackBoxX : FLOAT;
            gmfBlackBoxY : FLOAT;
            gmfptGlyphOrigin : POINTFLOAT;
            gmfCellIncX : FLOAT;
            gmfCellIncY : FLOAT;
         end;
       LPGLYPHMETRICSFLOAT = ^record
            gmfBlackBoxX : FLOAT;
            gmfBlackBoxY : FLOAT;
            gmfptGlyphOrigin : POINTFLOAT;
            gmfCellIncX : FLOAT;
            gmfCellIncY : FLOAT;
         end;
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
       PLAYERPLANEDESCRIPTOR = ^record
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
       LPLAYERPLANEDESCRIPTOR = ^record
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
       PPIXELFORMATDESCRIPTOR = ^record
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
       LPPIXELFORMATDESCRIPTOR = ^record
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

  implementation

const __ExtrnlLibrary='system.dll'; {Setup as you need!}


end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:47  root
  Initial revision

  Revision 1.3  1998/01/26 12:02:36  michael
  + Added log at the end


  
  Working file: rtl/win32/struct.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:42:49;  author: michael;  state: Exp;  lines: +10 -16
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 23:28:32;  author: florian;  state: Exp;
  - Win32: base.pp compilable, but there is a compiler bug, so wrong assembler
           is created
  - Win32: API interface units renamed to *.pp
  =============================================================================
}
