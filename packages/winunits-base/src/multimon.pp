{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by the Free Pascal development team
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
//=============================================================================
//
// multimon.h -- Stub module that fakes multiple monitor apis on Win32 OSes
//               without them.
//
// By using this header your code will get back default values from
// GetSystemMetrics() for new metrics, and the new multimonitor APIs
// will act like only one display is present on a Win32 OS without
// multimonitor APIs.
//
// Exactly one source must include this with COMPILE_MULTIMON_STUBS defined.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
//=============================================================================
unit MultiMon;

{$mode objfpc}{$H+}

{$ifdef FPC_OS_UNICODE}
  {$define UNICODE}
{$endif}

interface

uses
  Windows;

//
// If we are building with Win95/NT4 headers, we need to declare
// the multimonitor-related metrics and APIs ourselves.
//
const
  SM_XVIRTUALSCREEN    = 76;
  SM_YVIRTUALSCREEN    = 77;
  SM_CXVIRTUALSCREEN   = 78;
  SM_CYVIRTUALSCREEN   = 79;
  SM_CMONITORS         = 80;
  SM_SAMEDISPLAYFORMAT = 81;

// HMONITOR is already declared if WINVER >= 0x0500 in windef.h
// This is for components built with an older version number.
type
  HMONITOR = Windows.HMonitor;

const
  MONITOR_DEFAULTTONULL    = $00000000;
  MONITOR_DEFAULTTOPRIMARY = $00000001;
  MONITOR_DEFAULTTONEAREST = $00000002;

  MONITORINFOF_PRIMARY     = $00000001;

type
  tagMONITORINFO = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
  MONITORINFO = tagMONITORINFO;
  LPMONITORINFO = ^tagMONITORINFO;
  TMonitorInfo = MONITORINFO;
  PMonitorInfo = LPMONITORINFO;

const
  CCHDEVICENAME = 32;

type
  tagMONITORINFOEXA = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
    szDevice: array[0..CCHDEVICENAME - 1] of Char;
  end;
  MONITORINFOEXA = tagMONITORINFOEXA;
  LPMONITORINFOEXA = ^tagMONITORINFOEXA;
  TMonitorInfoExA = MONITORINFOEXA;
  PMonitorInfoExA = LPMONITORINFOEXA;

  tagMONITORINFOEXW = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
    szDevice: array[0..CCHDEVICENAME - 1] of WideChar;
  end;
  MONITORINFOEXW = tagMONITORINFOEXW;
  LPMONITORINFOEXW = ^tagMONITORINFOEXW;
  TMonitorInfoExW = MONITORINFOEXW;
  PMonitorInfoExW = LPMONITORINFOEXW;

{$ifdef UNICODE}
  MONITORINFOEX = MONITORINFOEXW;
  LPMONITORINFOEX = LPMONITORINFOEXW;
  TMonitorInfoEx = MONITORINFOEXW;
  PMonitorInfoEx = LPMONITORINFOEXW;
{$else}
  MONITORINFOEX = MONITORINFOEXA;
  LPMONITORINFOEX = LPMONITORINFOEXA;
  TMonitorInfoEx = MONITORINFOEXA;
  PMonitorInfoEx = LPMONITORINFOEXA;
{$endif}

type
  TMonitorEnumProc = function(hMonitor: HMONITOR; hdcMonitor: HDC; lprcMonitor: PRect;
    dwData: LPARAM): BOOL; stdcall;

// ifndef DISPLAY_DEVICE_ATTACHED_TO_DESKTOP
type
  _DISPLAY_DEVICEA = record
    cb: DWORD;
    DeviceName: array[0..31] of Char;
    DeviceString: array[0..127] of Char;
    StateFlags: DWORD;
    DeviceID: array[0..127] of Char;
    DeviceKey: array[0..127] of Char;
  end;
  DISPLAY_DEVICEA = _DISPLAY_DEVICEA;
  PDISPLAY_DEVICEA = ^_DISPLAY_DEVICEA;
  LPDISPLAY_DEVICEA = ^_DISPLAY_DEVICEA;
  TDisplayDeviceA = DISPLAY_DEVICEA;
  PDisplayDeviceA = PDISPLAY_DEVICEA;

  _DISPLAY_DEVICEW = record
    cb: DWORD;
    DeviceName: array[0..31] of WideChar;
    DeviceString: array[0..127] of WideChar;
    StateFlags: DWORD;
    DeviceID: array[0..127] of WideChar;
    DeviceKey: array[0..127] of WideChar;
  end;
  DISPLAY_DEVICEW = _DISPLAY_DEVICEW;
  PDISPLAY_DEVICEW = ^_DISPLAY_DEVICEW;
  LPDISPLAY_DEVICEW = ^_DISPLAY_DEVICEW;
  TDisplayDeviceW = DISPLAY_DEVICEW;
  PDisplayDeviceW = PDISPLAY_DEVICEW;

{$ifdef UNICODE}
  DISPLAY_DEVICE = DISPLAY_DEVICEW;
  PDISPLAY_DEVICE = PDISPLAY_DEVICEW;
  LPDISPLAY_DEVICE = LPDISPLAY_DEVICEW;
  TDisplayDevice = TDisplayDeviceW;
  PDisplayDevice = PDisplayDeviceW;
{$else}
  DISPLAY_DEVICE = DISPLAY_DEVICEA;
  PDISPLAY_DEVICE = PDISPLAY_DEVICEA;
  LPDISPLAY_DEVICE = LPDISPLAY_DEVICEA;
  TDisplayDevice = TDisplayDeviceA;
  PDisplayDevice = PDisplayDeviceA;
{$endif} // UNICODE

const
  DISPLAY_DEVICE_ATTACHED_TO_DESKTOP = $00000001;
  DISPLAY_DEVICE_MULTI_DRIVER        = $00000002;
  DISPLAY_DEVICE_PRIMARY_DEVICE      = $00000004;
  DISPLAY_DEVICE_MIRRORING_DRIVER    = $00000008;
  DISPLAY_DEVICE_VGA_COMPATIBLE      = $00000010;

type
  TGetSystemMetrics = function(nIndex: longint): longint; stdcall;
  TMonitorFromWindow = function(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
  TMonitorFromRect = function(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; stdcall;
  TMonitorFromPoint = function(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; stdcall;
  TGetMonitorInfo = function(hMonitor: HMONITOR; lpmi: PMonitorInfo): BOOL; stdcall;
  TEnumDisplayMonitors = function(hdc: HDC; lprcClip: PRect; lpfnEnum: TMonitorEnumProc; dwData: LPARAM): BOOL; stdcall;
  TEnumDisplayDevices = function(lpDevice: Pointer; iDevNum: DWORD; lpDisplayDevice: PDisplayDevice; dwFlags: DWORD): BOOL; stdcall;

var
  GetSystemMetrics: TGetSystemMetrics;
  MonitorFromWindow: TMonitorFromWindow;
  MonitorFromRect: TMonitorFromRect;
  MonitorFromPoint: TMonitorFromPoint;
  GetMonitorInfo: TGetMonitorInfo;
  EnumDisplayMonitors: TEnumDisplayMonitors;
  EnumDisplayDevices: TEnumDisplayDevices;

implementation

var
  g_fMultiMonInitDone: Boolean = False;
  g_pfnGetSystemMetrics: TGetSystemMetrics = nil;
  g_pfnMonitorFromWindow: TMonitorFromWindow = nil;
  g_pfnMonitorFromRect: TMonitorFromRect = nil;
  g_pfnMonitorFromPoint: TMonitorFromPoint = nil;
  g_pfnGetMonitorInfo: TGetMonitorInfo = nil;
  g_pfnEnumDisplayMonitors: TEnumDisplayMonitors = nil;
  g_pfnEnumDisplayDevices: TEnumDisplayDevices = nil;

function IsPlatformNT: Boolean;
var
  osvi: TOSVersionInfo;
begin
{$HINTS OFF}
  FillChar(osvi, SizeOf(osvi), 0);
{$HINTS ON}
  osvi.dwOSVersionInfoSize := sizeof(osvi);
  GetVersionExA(@osvi);
  Result := VER_PLATFORM_WIN32_NT = osvi.dwPlatformId;
end;

function InitMultipleMonitorStubs: Boolean;
var
  hUser32: HMODULE;
begin
  if g_fMultiMonInitDone then
    Exit(@g_pfnGetMonitorInfo <> nil);

  hUser32 := GetModuleHandle('USER32');
  if hUser32 <> 0 then
  begin
    Pointer(g_pfnGetSystemMetrics)    := GetProcAddress(hUser32, 'GetSystemMetrics');
    Pointer(g_pfnMonitorFromWindow)   := GetProcAddress(hUser32, 'MonitorFromWindow');
    Pointer(g_pfnMonitorFromRect)     := GetProcAddress(hUser32, 'MonitorFromRect');
    Pointer(g_pfnMonitorFromPoint)    := GetProcAddress(hUser32, 'MonitorFromPoint');
    Pointer(g_pfnEnumDisplayMonitors) := GetProcAddress(hUser32, 'EnumDisplayMonitors');
{$ifdef UNICODE}
    Pointer(g_pfnEnumDisplayDevices)  := GetProcAddress(hUser32, 'EnumDisplayDevicesW');
    if IsPlatformNT then
      Pointer(g_pfnGetMonitorInfo)    := GetProcAddress(hUser32, 'GetMonitorInfoW')
    else
      Pointer(g_pfnGetMonitorInfo)    := GetProcAddress(hUser32, 'GetMonitorInfoA');
{$else}
    Pointer(g_pfnGetMonitorInfo)      := GetProcAddress(hUser32, 'GetMonitorInfoA');
    Pointer(g_pfnEnumDisplayDevices)  := GetProcAddress(hUser32, 'EnumDisplayDevicesA');
{$endif}
    g_fMultiMonInitDone := True;
    Result := True;
  end
  else
  begin
    Pointer(g_pfnGetSystemMetrics)    := nil;
    Pointer(g_pfnMonitorFromWindow)   := nil;
    Pointer(g_pfnMonitorFromRect)     := nil;
    Pointer(g_pfnMonitorFromPoint)    := nil;
    Pointer(g_pfnEnumDisplayMonitors) := nil;
    Pointer(g_pfnGetMonitorInfo)      := nil;
    Pointer(g_pfnEnumDisplayDevices)  := nil;

    g_fMultiMonInitDone := True;
    Result := False;
  end;
end;

//-----------------------------------------------------------------------------
//
// fake implementations of Monitor APIs that work with the primary display
// no special parameter validation is made since these run in client code
//
//-----------------------------------------------------------------------------

function xGetSystemMetrics(nIndex: Integer): Integer; stdcall;
begin
  if (InitMultipleMonitorStubs()) then
    Exit(g_pfnGetSystemMetrics(nIndex));

  case nIndex of
    SM_CMONITORS,
    SM_SAMEDISPLAYFORMAT:
      Exit(1);

    SM_XVIRTUALSCREEN,
    SM_YVIRTUALSCREEN:
      Exit(0);

    SM_CXVIRTUALSCREEN:
      nIndex := SM_CXSCREEN;

    SM_CYVIRTUALSCREEN:
      nIndex := SM_CYSCREEN;
  end;
  Result := GetSystemMetrics(nIndex);
end;

const
  xPRIMARY_MONITOR = HMONITOR($12340042);

function xMonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWORD): HMONITOR; stdcall;
begin
  if (InitMultipleMonitorStubs()) then
      Exit(g_pfnMonitorFromPoint(ptScreenCoords, dwFlags));

  if ((dwFlags and (MONITOR_DEFAULTTOPRIMARY or MONITOR_DEFAULTTONEAREST) <> 0 ) or
      ((ptScreenCoords.x >= 0) and
      (ptScreenCoords.x < GetSystemMetrics(SM_CXSCREEN)) and
      (ptScreenCoords.y >= 0) and
      (ptScreenCoords.y < GetSystemMetrics(SM_CYSCREEN)))) then
    Result := xPRIMARY_MONITOR
  else
    Result := 0;
end;

function xMonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWORD): HMONITOR; stdcall;
begin
  if (InitMultipleMonitorStubs()) then
    Exit(g_pfnMonitorFromRect(lprcScreenCoords, dwFlags));

  if ((dwFlags and (MONITOR_DEFAULTTOPRIMARY or MONITOR_DEFAULTTONEAREST) <> 0) or
      ((lprcScreenCoords^.right > 0) and
      (lprcScreenCoords^.bottom > 0) and
      (lprcScreenCoords^.left < GetSystemMetrics(SM_CXSCREEN)) and
      (lprcScreenCoords^.top < GetSystemMetrics(SM_CYSCREEN)))) then
    Result := xPRIMARY_MONITOR
  else
    Result := 0;
end;

function xMonitorFromWindow(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
var
  wp: TWindowPlacement;
  B: Boolean;
begin
  if (InitMultipleMonitorStubs()) then
    Exit(g_pfnMonitorFromWindow(hWnd, dwFlags));

  if (dwFlags and (MONITOR_DEFAULTTOPRIMARY or MONITOR_DEFAULTTONEAREST) <> 0) then
    Exit(xPRIMARY_MONITOR);

  if IsIconic(hWnd) then
    B := GetWindowPlacement(hWnd, @wp)
  else
    B := GetWindowRect(hWnd, @wp.rcNormalPosition);

  if B then
    Result := xMonitorFromRect(@wp.rcNormalPosition, dwFlags)
  else
    Result := 0;
end;

function xGetMonitorInfo(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfo): BOOL; stdcall;
var
  rcWork: TRect;
  f: BOOL;
begin
  if (InitMultipleMonitorStubs()) then
  begin
    f := g_pfnGetMonitorInfo(hMonitor, lpMonitorInfo);
{$ifdef UNICODE}
    if (f and not IsPlatformNT and (lpMonitorInfo^.cbSize >= sizeof(TMonitorInfoEx))) then
    begin
      MultiByteToWideChar(CP_ACP, 0,
          LPCSTR(@PMonitorInfoEx(lpMonitorInfo)^.szDevice[0]), -1,
          @PMonitorInfoEx(lpMonitorInfo)^.szDevice[0], CCHDEVICENAME);
    end;
{$endif}
    Exit(f);
  end;

  if ((hMonitor = xPRIMARY_MONITOR) and
      (lpMonitorInfo <> nil) and
      (lpMonitorInfo^.cbSize >= sizeof(TMonitorInfo)) and
      SystemParametersInfo(SPI_GETWORKAREA, 0, @rcWork, 0)) then
  begin
    lpMonitorInfo^.rcMonitor.left := 0;
    lpMonitorInfo^.rcMonitor.top  := 0;
    lpMonitorInfo^.rcMonitor.right  := GetSystemMetrics(SM_CXSCREEN);
    lpMonitorInfo^.rcMonitor.bottom := GetSystemMetrics(SM_CYSCREEN);
    lpMonitorInfo^.rcWork := rcWork;
    lpMonitorInfo^.dwFlags := MONITORINFOF_PRIMARY;

    if (lpMonitorInfo^.cbSize >= sizeof(TMonitorInfoEx)) then
      PMonitorInfoEx(lpMonitorInfo)^.szDevice := 'DISPLAY';

    Result := True;
  end
  else
    Result := False;
end;

function xEnumDisplayMonitors(hdcOptionalForPainting: HDC; lprcEnumMonitorsThatIntersect: PRect;
   lpfnEnumProc: TMonitorEnumProc; dwData: LPARAM): BOOL; stdcall;
var
  rcLimit, rcClip: TRect;
  ptOrg: TPoint;
  Cb: Integer;
begin
  if (InitMultipleMonitorStubs()) then
    Exit(g_pfnEnumDisplayMonitors(
           hdcOptionalForPainting,
           lprcEnumMonitorsThatIntersect,
           lpfnEnumProc,
           dwData));

  if (lpfnEnumProc = nil) then
    Exit(False);

  rcLimit.left   := 0;
  rcLimit.top    := 0;
  rcLimit.right  := GetSystemMetrics(SM_CXSCREEN);
  rcLimit.bottom := GetSystemMetrics(SM_CYSCREEN);

  if (hdcOptionalForPainting <> 0) then
  begin
    Cb := GetClipBox(hdcOptionalForPainting, @rcClip);
    if not GetDCOrgEx(hdcOptionalForPainting, @ptOrg) then
      Exit(False);

    OffsetRect(rcLimit, -ptOrg.x, -ptOrg.y);
    if (IntersectRect(rcLimit, rcLimit, rcClip) and
       ((lprcEnumMonitorsThatIntersect = nil) or
         IntersectRect(rcLimit, rcLimit, lprcEnumMonitorsThatIntersect^))) then
    begin
      if Cb =  NULLREGION then
        Exit(True)
      else
      if Cb = ERROR then
        Exit(False);
    end
  end
  else
  if ((lprcEnumMonitorsThatIntersect <> nil) and
      not IntersectRect(rcLimit, rcLimit, lprcEnumMonitorsThatIntersect^)) then
    Exit(True);

  Result := lpfnEnumProc(
              xPRIMARY_MONITOR,
              hdcOptionalForPainting,
              @rcLimit,
              dwData);
end;

function xEnumDisplayDevices(Unused: Pointer; iDevNum: DWORD; lpDisplayDevice: PDisplayDevice;
  dwFlags: DWORD): BOOL; stdcall;
begin
  if (InitMultipleMonitorStubs()) then
    Exit(g_pfnEnumDisplayDevices(Unused, iDevNum, lpDisplayDevice, dwFlags));

  if (Unused <> nil) then
    Exit(False);

  if (iDevNum <> 0) then
    Exit(False);

  if (lpDisplayDevice = nil) or (lpDisplayDevice^.cb < sizeof(TDisplayDevice)) then
    Exit(False);

  lpDisplayDevice^.DeviceName := 'DISPLAY';
  lpDisplayDevice^.DeviceString := 'DISPLAY';

  lpDisplayDevice^.StateFlags := DISPLAY_DEVICE_ATTACHED_TO_DESKTOP or DISPLAY_DEVICE_PRIMARY_DEVICE;

  Result := True;
end;


initialization
  Pointer(GetSystemMetrics) := @xGetSystemMetrics;
  Pointer(MonitorFromWindow) := @xMonitorFromWindow;
  Pointer(MonitorFromRect) := @xMonitorFromRect;
  Pointer(MonitorFromPoint) := @xMonitorFromPoint;
  Pointer(GetMonitorInfo) := @xGetMonitorInfo;
  Pointer(EnumDisplayMonitors) := @xEnumDisplayMonitors;
  Pointer(EnumDisplayDevices) := @xEnumDisplayDevices;
end.

