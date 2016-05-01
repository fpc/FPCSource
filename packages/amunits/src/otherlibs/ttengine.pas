{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.

  This is a unit for ttengine.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{
  History:

  First version of this unit.
  16 Jan 2003.

  Changed cardinal > longword.
  Changed startcode for unit.
  12 Feb 2003.

  nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}


UNIT TTENGINE;

INTERFACE
USES Exec,utility,agraphics;

VAR TTEngineBase : pLibrary = nil;

const
    TTENGINENAME : PChar = 'ttengine.library';


  { $VER: ttengine.h 6.0 (3.1.2003) (c) by Grzegorz Kraszewski 2002.  }

  const

     TTENGINEVERSION = 6;
     TTENGINEMINVERSION = 3;
  { Tags  }
  { Tags applicability legend:  }
  { O - TT_OpenFont()  }
  { G - TT_GetAttrs()  }
  { S - TT_SetAttrs()  }
  { P - TT_GetPixmap()  }
  { ---- name -------------------- value ----- applicability  }
  { OG..  }
     TT_FontFile = $6EDA0000;
  { OG..  }
     TT_FontStyle = $6EDA0001;
     TT_FontStyle_Regular = 0;
     TT_FontStyle_Italic = 1;
  { O...  }
     TT_FamilyTable = $6EDA0002;
  { OG..  }
     TT_FontSize = $6EDA0003;
  { OG..  }
     TT_FontWeight = $6EDA0004;
     TT_FontWeight_Normal = 400;
     TT_FontWeight_Bold = 700;
  { O...  }
     TT_ColorMap = $6EDA0005;
  { O...  }
     TT_Screen = $6EDA0006;
  { O...  }
     TT_Window = $6EDA0007;
  { .G..  }
     TT_FontAscender = $6EDA0008;
  { .G..  }
     TT_FontDescender = $6EDA0009;
  { .GSP  }
     TT_Antialias = $6EDA000F;
     TT_Antialias_Auto = 0;
     TT_Antialias_Off = 1;
     TT_Antialias_On = 2;
  { .GSP  }
     TT_Encoding = $6EDA0010;
  { supported  }
  { use ENV:ttfcodepage or ISO-8859-1 if not found  }
     TT_Encoding_Default = 0;
  { Western Europe and US  }
     TT_Encoding_ISO8859_1 = 4;
  { Eastern Europe  }
     TT_Encoding_ISO8859_2 = 5;
     TT_Encoding_ISO8859_3 = 6;
     TT_Encoding_ISO8859_4 = 7;
     TT_Encoding_ISO8859_5 = 8;
     TT_Encoding_ISO8859_6 = 9;
     TT_Encoding_ISO8859_7 = 10;
     TT_Encoding_ISO8859_8 = 11;
     TT_Encoding_ISO8859_9 = 12;
     TT_Encoding_ISO8859_10 = 13;
     TT_Encoding_ISO8859_11 = 14;
     TT_Encoding_ISO8859_13 = 109;
     TT_Encoding_ISO8859_14 = 110;
     TT_Encoding_ISO8859_15 = 111;
     TT_Encoding_ISO8859_16 = 112;
     TT_Encoding_UTF16_BE = 1013;
     TT_Encoding_UTF32_BE = 1018;
     TT_Encoding_UTF8 = 106;
     TT_Encoding_UTF16_LE = 1014;
     TT_Encoding_UTF32_LE = 1019;
     TT_Encoding_UTF16 = 1015;
     TT_Encoding_UTF32 = 1017;
  { .G..  }
     TT_FontName = $6EDA0011;
  { .G..  }
     TT_FamilyName = $6EDA0012;
  { .G..  }
     TT_SubfamilyName = $6EDA0013;
  { .GS.  from 0 to 255  }
     TT_Transparency = $6EDA0014;
  { O.SP  single precision floating point +- 0.01 to 100  }
     TT_ScaleX = $6EDA0015;
  { O.SP  single precision floating point +- 0.01 to 100  }
     TT_ScaleY = $6EDA0016;
  { ..SP (V5)  }
     TT_SoftStyle = $6EDA0017;
     TT_SoftStyle_None = $0000;
     TT_SoftStyle_Underlined = $0001;
     TT_SoftStyle_DblUnderlined = $0002;
     TT_SoftStyle_Overstriked = $0004;
     TT_SoftStyle_DblOverstriked = $0008;
  { ..S.  foreground RGB value }
     TT_Foreground = $6EDA0018;
     TT_Foreground_UseRastPort = -(1);
  { ..S.  background RGB value }
     TT_Background = $6EDA0019;
     TT_Background_UseRastPort = -(1);
  { .G..  }
     TT_FontMaxTop = $6EDA001E;
  { .G..  }
     TT_FontMaxBottom = $6EDA001F;
  { .G..  }
     TT_FontDesignHeight = $6EDA0020;
  { .G..  }
     TT_FontRealAscender = $6EDA0021;
  { .G..  }
     TT_FontRealDescender = $6EDA0022;
  { .G..  }
     TT_FontAccentedAscender = $6EDA0023;
  { ..SP  }
     TT_CustomEncoding = $6EDA0024;


  { Structure returned by TT_GetPixmap() (V5) }
  type
     PTT_Pixmap = ^tTT_Pixmap;
     tTT_Pixmap = record
          ttp_Size : ULONG;      { size of the structure inculdung this field  }
          ttp_Width : ULONG;     { also equal to bytes per row  }
          ttp_Height : ULONG;    { number of rows  }
          ttp_Data : Pointer;    { grayscale pixmap data  }
       end;

  { font requester attributes (V6)  }

  const
  { struct Window ,   NULL               }
     TTRQ_Window = $6EDA2000;
  { STRPTR,           NULL [Workbench]   }
     TTRQ_PubScreenName = $6EDA2001;
  { struct Screen ,   NULL               }
     TTRQ_Screen = $6EDA2002;
  { BOOL,             FALSE              }
     TTRQ_SleepWindow = $6EDA2003;
  { STRPTR,           "Select TrueType font" or localized  }
     TTRQ_TitleText = $6EDA2004;
  { STRPTR,           "OK" or localized  }
     TTRQ_PositiveText = $6EDA2005;
  { STRPTR,           "Cancel" or localized  }
     TTRQ_NegativeText = $6EDA2006;
  { WORD,             centered on screen  }
     TTRQ_InitialLeftEdge = $6EDA2007;
  { WORD,             centered on screen  }
     TTRQ_InitialTopEdge = $6EDA2008;
  { WORD,             max(200, 25% of sceeen width)  }
     TTRQ_InitialWidth = $6EDA2009;
  { WORD,             max(200, 50% of screen height)  }
     TTRQ_InitialHeight = $6EDA200A;
  { BOOL,             TRUE               }
     TTRQ_DoSizes = $6EDA2000;

FUNCTION TT_AllocRequest : POINTER; syscall TTEngineBase 102;
PROCEDURE TT_CloseFont(font : POINTER location 'a0'); syscall TTEngineBase 42;
PROCEDURE TT_DoneRastPort(rp : pRastPort location 'a1'); syscall TTEngineBase 96;
PROCEDURE TT_FreePixmap(pixmap : pTT_Pixmap location 'a0'); syscall TTEngineBase 90;
PROCEDURE TT_FreeRequest(request : POINTER location 'a0'); syscall TTEngineBase 114;
FUNCTION TT_GetAttrsA(rp : pRastPort location 'a1'; taglist : pTagItem location 'a0') : longword; syscall TTEngineBase 60;
FUNCTION TT_GetPixmapA(font : POINTER location 'a1'; _string : POINTER location 'a2'; count : longword location 'd0'; taglist : pTagItem location 'a0') : pTT_Pixmap; syscall TTEngineBase 84;
FUNCTION TT_OpenFontA(taglist : pTagItem location 'a0') : POINTER; syscall TTEngineBase 30;
FUNCTION TT_RequestA(request : POINTER location 'a0'; taglist : pTagItem location 'a1') : pTagItem; syscall TTEngineBase 108;
FUNCTION TT_SetAttrsA(rp : pRastPort location 'a1'; taglist : pTagItem location 'a0') : longword; syscall TTEngineBase 54;
FUNCTION TT_SetFont(rp : pRastPort location 'a1'; font : POINTER location 'a0') : BOOLEAN; syscall TTEngineBase 36;
PROCEDURE TT_Text(rp : pRastPort location 'a1'; _string : POINTER location 'a0'; count : longword location 'd0'); syscall TTEngineBase 48;
PROCEDURE TT_TextExtent(rp : pRastPort location 'a1'; _string : POINTER location 'a0'; count : LONGINT location 'd0'; te : pTextExtent location 'a2'); syscall TTEngineBase 72;
FUNCTION TT_TextFit(rp : pRastPort location 'a1'; _string : POINTER location 'a0'; count : longword location 'd0'; te : pTextExtent location 'a2'; tec : pTextExtent location 'a3'; dir : LONGINT location 'd1'; cwidth : longword location 'd2'; cheight : longword location 'd3') : longword; syscall TTEngineBase 78;
FUNCTION TT_TextLength(rp : pRastPort location 'a1'; _string : POINTER location 'a0'; count : longword location 'd0') : longword; syscall TTEngineBase 66;
{
 Functions and procedures with array of PtrUInt go here
}
FUNCTION TT_GetAttrs(rp : pRastPort; const taglist : array of PtrUInt) : longword;
FUNCTION TT_GetPixmap(font : POINTER; _string : POINTER; count : longword; const taglist : array of PtrUInt) : pTT_Pixmap;
FUNCTION TT_OpenFont(const taglist : array of PtrUInt) : POINTER;
FUNCTION TT_Request(request : POINTER; const taglist : array of PtrUInt) : pTagItem;
FUNCTION TT_SetAttrs(rp : pRastPort; const taglist : array of PtrUInt) : longword;

IMPLEMENTATION

{
 Functions and procedures with array of PtrUInt go here
}
FUNCTION TT_GetAttrs(rp : pRastPort; const taglist : array of PtrUInt) : longword;
begin
    TT_GetAttrs := TT_GetAttrsA(rp , @taglist);
end;

FUNCTION TT_GetPixmap(font : POINTER; _string : POINTER; count : longword; const taglist : array of PtrUInt) : pTT_Pixmap;
begin
    TT_GetPixmap := TT_GetPixmapA(font , _string , count , @taglist);
end;

FUNCTION TT_OpenFont(const taglist : array of PtrUInt) : POINTER;
begin
    TT_OpenFont := TT_OpenFontA(@taglist);
end;

FUNCTION TT_Request(request : POINTER; const taglist : array of PtrUInt) : pTagItem;
begin
    TT_Request := TT_RequestA(request , @taglist);
end;

FUNCTION TT_SetAttrs(rp : pRastPort; const taglist : array of PtrUInt) : longword;
begin
    TT_SetAttrs := TT_SetAttrsA(rp , @taglist);
end;

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  TTEngineBase := OpenLibrary(TTENGINENAME,LIBVERSION);
finalization
  if Assigned(TTEngineBase) then
    CloseLibrary(TTEngineBase);
END. (* UNIT TTENGINE *)



