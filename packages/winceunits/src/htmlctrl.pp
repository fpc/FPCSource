{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Automatically converted by H2Pas 1.0.0 from htmlctrl.h
  The following command line parameters were used:
    -d
    -c
    -w
    htmlctrl.h
}
unit htmlctrl;

interface

uses Windows;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
   DTM_ADDTEXT = WM_USER+101;   
   DTM_ADDTEXTW = WM_USER+102;   
   DTM_SETIMAGE = WM_USER+103;   
   DTM_ENDOFSOURCE = WM_USER+104;   
   DTM_ANCHOR = WM_USER+105;   
   DTM_ANCHORW = WM_USER+106;   
   DTM_ENABLESHRINK = WM_USER+107;   
   DTM_FITTOWINDOW = WM_USER+107;   
   DTM_SCROLLINTOVIEW = WM_USER+108;   
   DTM_IMAGEFAIL = WM_USER+109;   
   DTM_ENABLECONTEXTMENU = WM_USER+110;   
   DTM_SELECTALL = WM_USER+111;   
   DTM_ISSELECTION = WM_USER+112;   
   DTM_CLEAR = WM_USER+113;   
   DTM_ENABLECLEARTYPE = WM_USER+114;   
   DTM_ENABLESCRIPTING = WM_USER+115;   
   DTM_ZOOMLEVEL = WM_USER+116;   
   DTM_LAYOUTWIDTH = WM_USER+117;   
   DTM_LAYOUTHEIGHT = WM_USER+118;   
   DTM_COPYSELECTIONTONEWISTREAM = WM_USER+119;   
   DTM_NAVIGATE = WM_USER+120;   
   DTM_INTEGRALPAGING = WM_USER+121;   
   DTM_SCRIPTDISPATCH = WM_USER+122;   
   DTM_DOCUMENTDISPATCH = WM_USER+123;   
   DTM_BROWSERDISPATCH = WM_USER+124;   
   DTM_STOP = WM_USER+125;   
   DTM_ADDSTYLE = WM_USER+126;   
   NM_HOTSPOT = WM_USER+101;   
   NM_INLINE_IMAGE = WM_USER+102;   
   NM_INLINE_SOUND = WM_USER+103;   
   NM_TITLE = WM_USER+104;   
   NM_META = WM_USER+105;   
   NM_BASE = WM_USER+106;   
   NM_CONTEXTMENU = WM_USER+107;   
   NM_INLINE_XML = WM_USER+108;   
   NM_BEFORENAVIGATE = WM_USER+109;   
   NM_DOCUMENTCOMPLETE = WM_USER+110;   
   NM_NAVIGATECOMPLETE = WM_USER+111;   
   NM_TITLECHANGE = WM_USER+112;   
   NM_INLINE_STYLE = WM_USER+113;   
   DISPLAYCLASS = 'DISPLAYCLASS';
   WC_HTML = DISPLAYCLASS;   
   HS_NOFITTOWINDOW = $0001;   
   HS_CONTEXTMENU = $0002;   
   HS_CLEARTYPE = $0004;   
   HS_NOSCRIPTING = $0008;   
   HS_INTEGRALPAGING = $0010;   
   HS_NOSCROLL = $0020;   
   HS_NOIMAGES = $0040;   
   HS_NOSOUNDS = $0080;   
   HS_NOACTIVEX = $0100;   
   HS_NOSELECTION = $0200;   
   HS_NOFOCUSRECT = $0400;   
   FRAME_SCROLLING_AUTO = 1;   
   FRAME_SCROLLING_YES = 2;   
   FRAME_SCROLLING_NO = 3;   
{ DTM_NAVIGATE flags }
   NAVIGATEFLAG_REFRESH = $0020;   
   NAVIGATEFLAG_RELATIVE = $0040;   
   NAVIGATEFLAG_ENTERED = $0080;   
   NAVIGATEFLAG_IGNORETARGET = $0200;   
   NAVIGATEFLAG_GETFROMCACHE = $0400;   
   NAVIGATEFLAG_NOCACHE = $1000;   
   NAVIGATEFLAG_RESYNCHRONIZE = $2000;   
   NAVIGATEFLAG_RELOAD = $4000;   

type
   tagNM_HTMLVIEWW = record
        hdr : NMHDR;
        szTarget : LPCWSTR;
        szData : LPCWSTR;
        dwCookieFlags : DWORD;
        szExInfo : LPCWSTR;
     end;
   NM_HTMLVIEWW = tagNM_HTMLVIEWW;
   NM_HTMLVIEW = NM_HTMLVIEWW;
   
   tagINLINEIMAGEINFO = record
        dwCookie : DWORD;
        iOrigHeight : longint;
        iOrigWidth : longint;
        hbm : HBITMAP;
        bOwnBitmap : BOOL;
     end;
   INLINEIMAGEINFO = tagINLINEIMAGEINFO;

const
   HTMLCONTEXT_BACKGROUND = $00;   
   HTMLCONTEXT_LINK = $01;   
   HTMLCONTEXT_IMAGE = $02;   
   HTMLCONTEXT_IMAGENOTLOADED = $04;   
   HTMLCONTEXT_TEXT = $08;   

type
   tagNM_HTMLCONTEXT = record
        hdr : NMHDR;
        pt : POINT;
        uTypeFlags : UINT;
        szLinkHREF : LPTSTR;
        szLinkName : LPTSTR;
        dwReserved1 : DWORD;
        dwImageCookie : DWORD;
        dwReserved2 : DWORD;
     end;
   NM_HTMLCONTEXT = tagNM_HTMLCONTEXT;

function InitHTMLControl(hinst:THandle):BOOL;cdecl;external 'htmlview.dll' name 'InitHTMLControl';

implementation

end.
