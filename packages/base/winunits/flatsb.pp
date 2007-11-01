{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by the Free Pascal development team
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$inline on}
unit FlatSB;

  interface

    uses
      ctypes,Windows;

    function InitializeFlatSB(hWnd: HWND): Bool; stdcall;
    procedure UninitializeFlatSB(hWnd: HWND); stdcall;
    function FlatSB_GetScrollProp(hwnd:HWND;propIndex : cint;p3 : LPINT):BOOL; stdcall;
    function FlatSB_SetScrollProp(p1: HWND; index : UINT; newValue: INT_PTR; p4: BOOL):BOOL; stdcall;

    var
      FlatSB_EnableScrollBar: function(hwnd:HWND;code : cint;p3 : UINT):BOOL; stdcall;
      FlatSB_ShowScrollBar: function(hwnd:HWND;code : cint;p3 : BOOL):BOOL; stdcall;
      FlatSB_GetScrollRange: function(hwnd:HWND;code : cint;var p3,p4 : cint):BOOL; stdcall;
      FlatSB_GetScrollInfo: function(hwnd:HWND;code : cint;var ScrollInfo : TSCROLLINFO):BOOL; stdcall;
      FlatSB_GetScrollPos: function(hwnd:HWND;code : cint):cint; stdcall;
      FlatSB_SetScrollPos: function(hWnd:HWND;nBar,nPos:cint;bRedraw:BOOL):cint; stdcall;
      FlatSB_SetScrollInfo: function(hWnd:HWND;BarFlag:cint;const ScrollInfo:TScrollInfo;Redraw:BOOL):cint; stdcall;
      FlatSB_SetScrollRange: function(hWnd: HWND; nBar,nMinPos,nMaxPos: cint; bRedraw: BOOL):cint; stdcall;

  implementation

    var
      Internal_FlatSB_GetScrollProp: function(hwnd:HWND;propIndex : cint;p3 : LPINT):BOOL; stdcall;
      Internal_FlatSB_SetScrollProp: function(p1: HWND; index : UINT; newValue: INT_PTR; p4: BOOL):BOOL; stdcall;
      Internal_InitializeFlatSB: function(hWnd: HWND): Bool; stdcall;
      Internal_UninitializeFlatSB: procedure(hWnd: HWND); stdcall;


    function FlatSB_GetScrollProp(hwnd:HWND;propIndex : cint;p3 : LPINT):BOOL; stdcall;
      begin
        Result:=Assigned(Internal_FlatSB_GetScrollProp) and Internal_FlatSB_GetScrollProp(hwnd,propIndex, p3);
      end;


    function FlatSB_SetScrollProp(p1: HWND; index : UINT; newValue: INT_PTR; p4: BOOL):BOOL; stdcall;
      begin
        Result:=Assigned(Internal_FlatSB_SetScrollProp) and Internal_FlatSB_SetScrollProp(p1,index,newValue,p4);
      end;


    function InitializeFlatSB(hWnd: HWND): Bool; stdcall;
      begin
        Result:=Assigned(Internal_InitializeFlatSB) and Internal_InitializeFlatSB(hWnd);
      end;


    procedure UninitializeFlatSB(hWnd: HWND); stdcall;
      begin
        if Assigned(Internal_UninitializeFlatSB) then
          Internal_UninitializeFlatSB(hWnd);
      end;

    var
      handle : THandle;
    begin
      handle:=GetModuleHandle('comctrl32.dll');
      if handle<>0 then
        begin
          pointer(Internal_InitializeFlatSB):=GetProcAddress(handle,'InitializeFlatSB');
          pointer(Internal_UninitializeFlatSB):=GetProcAddress(handle,'UninitializeFlatSB');
          pointer(Internal_FlatSB_GetScrollProp):=GetProcAddress(handle,'FlatSB_GetScrollProp');
          pointer(Internal_FlatSB_SetScrollProp):=GetProcAddress(handle,'FlatSB_SetScrollProp');

          pointer(FlatSB_EnableScrollBar):=GetProcAddress(handle,'FlatSB_EnableScrollBar');
          if not(assigned(FlatSB_EnableScrollBar)) then
            pointer(FlatSB_EnableScrollBar):=pointer(@EnableScrollBar);

          pointer(FlatSB_ShowScrollBar):=GetProcAddress(handle,'FlatSB_ShowScrollBar');
          if not(assigned(FlatSB_ShowScrollBar)) then
            pointer(FlatSB_ShowScrollBar):=pointer(@ShowScrollBar);

          pointer(FlatSB_GetScrollRange):=GetProcAddress(handle,'FlatSB_GetScrollRange');
          if not(assigned(FlatSB_GetScrollRange)) then
            pointer(FlatSB_GetScrollRange):=pointer(@GetScrollRange);

          pointer(FlatSB_GetScrollInfo):=GetProcAddress(handle,'FlatSB_GetScrollInfo');
          if not(assigned(FlatSB_GetScrollInfo)) then
            pointer(FlatSB_GetScrollInfo):=pointer(@GetScrollInfo);

          pointer(FlatSB_GetScrollPos):=GetProcAddress(handle,'FlatSB_GetScrollPos');
          if not(assigned(FlatSB_GetScrollPos)) then
            pointer(FlatSB_GetScrollPos):=pointer(@GetScrollPos);

          pointer(FlatSB_SetScrollPos):=GetProcAddress(handle,'FlatSB_SetScrollPos');
          if not(assigned(FlatSB_SetScrollPos)) then
            pointer(FlatSB_SetScrollPos):=pointer(@SetScrollPos);

          pointer(FlatSB_SetScrollInfo):=GetProcAddress(handle,'FlatSB_SetScrollInfo');
          if not(assigned(FlatSB_SetScrollInfo)) then
            pointer(FlatSB_SetScrollInfo):=pointer(@SetScrollInfo);

          pointer(FlatSB_SetScrollRange):=GetProcAddress(handle,'FlatSB_SetScrollRange');
          if not(assigned(FlatSB_SetScrollRange)) then
            pointer(FlatSB_SetScrollRange):=pointer(@SetScrollRange);
        end;
    end.
