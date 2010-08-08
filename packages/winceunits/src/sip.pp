{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }



//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit sip;

interface

{$MODE OBJFPC}

uses Windows, Types, SIPAPI;

const
      IID_IIMCallback:TIID    = '{42429669-AE04-11D0-A4F8-00AA00A749B9}';
      IID_IIMCallback2:TIID   = '{0576F2E0-AA6B-11D2-A146-0000F8757270}';
      IID_IIMCallbackEx:TIID  = '{9DDB3920-3606-11D2-A2EB-0000F8757270}';
      IID_IInputMethod:TIID   = '{42429666-AE04-11D0-A4F8-00AA00A749B9}';
      IID_IInputMethodEx:TIID = '{9DDB3921-3606-11D2-A2EB-0000F8757270}';
      IID_IInputMethod2:TIID  = '{0576F2E1-AA6B-11D2-A146-0000F8757270}';

const
      CLSID_CMSQwertyIm:CLSID = '{42429667-AE04-11D0-A4f8-00AA00A749B9}';

type
     _tagImInfo = record
       cbSize:DWORD;
       hImageNarrow:HANDLE;
       hImageWide:HANDLE;
       iNarrow:longint;
       iWide:longint;
       fdwFlags:DWORD;
       rcSipRect:Windows.RECT;
     end;
     IMINFO = _tagImInfo;
     LPIMINFO = ^_tagImInfo;

type
     _tagLMDATA = record
       dwVersion:DWORD;
       flags:DWORD;
       cnt:DWORD;
       dwOffsetSymbols:DWORD;
       dwOffsetSkip:DWORD;
       dwOffsetScore:DWORD;
       ab:array[0..0] of byte; //* [max_is] */
     end;
     LMDATA = _tagLMDATA;
     LPLMDATA = ^_tagLMDATA;

type
    IIMCallback = interface(IUnknown)
     ['{42429669-AE04-11D0-A4F8-00AA00A749B9}']
      function SetImInfo(pimi:LPIMINFO):HRESULT; stdcall;
      function SendVirtualKey(bVK:byte; dwFlags:DWORD):HRESULT; stdcall;
      function SendCharEvents(uVK:UINT; uKeyFlags:UINT; uChars:UINT; puShift:PUINT; puChars:PUINT):HRESULT; stdcall;
      function SendString(ptszStr:POleStr{BSTR}; dwChars:DWORD):HRESULT; stdcall;
    end;

    IIMCallback2 = interface(IIMCallback)
     ['{0576F2E0-AA6B-11D2-A146-0000F8757270}']
      function SendAlternatives2(plmd:LMDATA):HRESULT; stdcall;
    end;

    IIMCallbackEx = interface(IIMCallback)
     ['{9DDB3920-3606-11D2-A2EB-0000F8757270}']
      function SendAlternatives(plmd:LPLMDATA):HRESULT; stdcall;
    end;

    IInputMethod = interface(IUnknown)
     ['{42429666-AE04-11D0-A4F8-00AA00A749B9}']
      function Select(hwndSip:HWND):HRESULT; stdcall;
      function Deselect:HRESULT; stdcall;
      function Showing:HRESULT; stdcall;
      function Hiding:HRESULT; stdcall;
      function GetInfo(pimi:LPIMINFO):HRESULT; stdcall;
      function ReceiveSipInfo(psi:LPSIPINFO):HRESULT; stdcall;
      function RegisterCallback(const lpIMCallback:IIMCallback):HRESULT; stdcall;
      function GetImData(dwSize:DWORD; pvImData:LPVOID):HRESULT; stdcall;
      function SetImData(dwSize:DWORD; pvImData:LPVOID):HRESULT; stdcall;
      function UserOptionsDlg(hwndParent:HWND):HRESULT; stdcall;
    end;

    IInputMethodEx = interface(IInputMethod)
     ['{9DDB3921-3606-11D2-A2EB-0000F8757270}']
      function SetIMMActiveContext(_hwnd:HWND;
                                   bOpen:BOOL;
                                   dwConversion:DWORD;
                                   dwSentence:DWORD;
                                   hkl:DWORD):HRESULT; stdcall;
      function RegisterCallbackEx(const lpIMCallback:IIMCallbackEx):HRESULT; stdcall;
    end;


    IInputMethod2 = interface(IInputMethod)
     ['{0576F2E1-AA6B-11D2-A146-0000F8757270}']
      function SetIMMActiveContext(_hwnd:HWND;
                                   bOpen:BOOL;
                                   dwConversion:DWORD;
                                   dwSentence:DWORD;
                                   hkl:DWORD):HRESULT; stdcall;
      function RegisterCallback2(const lpIMCallback:IIMCallback2):HRESULT; stdcall;
    end;



implementation

end.