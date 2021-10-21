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
{*=========================================================================*

    Copyright (c) Microsoft Corporation.  All rights reserved.

    File: dwmapi.h

    Module Name: dwmapi

    Description: DWM API declarations

 *=========================================================================*}
unit DwmApi;
{$mode objfpc}{$H+}

interface

uses
  Windows, UxTheme;

procedure FreeDwmLibrary;
function InitDwmLibrary: Boolean;
function DwmCompositionEnabled: Boolean;

// Blur behind data structures
const
  DWM_BB_ENABLE                = $00000001; // fEnable has been specified
  DWM_BB_BLURREGION            = $00000002; // hRgnBlur has been specified
  DWM_BB_TRANSITIONONMAXIMIZED = $00000004; // fTransitionOnMaximized has been specified

type
  _DWM_BLURBEHIND = record
    dwFlags: DWORD;
    fEnable: BOOL;
    hRgnBlur: HRGN;
    fTransitionOnMaximized: BOOL;
  end;
  DWM_BLURBEHIND = _DWM_BLURBEHIND;
  PDWM_BLURBEHIND = ^_DWM_BLURBEHIND;
  TDWMBlurBehind = DWM_BLURBEHIND;
  PDWMBlurBehind = PDWM_BLURBEHIND;

// Window attributes
const
  DWMWA_NCRENDERING_ENABLED         = 01; // [get] Is non-client rendering enabled/disabled
  DWMWA_NCRENDERING_POLICY          = 02; // [set] Non-client rendering policy
  DWMWA_TRANSITIONS_FORCEDISABLED   = 03; // [set] Potentially enable/forcibly disable transitions
  DWMWA_ALLOW_NCPAINT               = 04; // [set] Allow contents rendered in the non-client area to be visible on the DWM-drawn frame.
  DWMWA_CAPTION_BUTTON_BOUNDS       = 05; // [get] Bounds of the caption button area in window-relative space.
  DWMWA_NONCLIENT_RTL_LAYOUT        = 06; // [set] Is non-client content RTL mirrored
  DWMWA_FORCE_ICONIC_REPRESENTATION = 07; // [set] Force this window to display iconic thumbnails.
  DWMWA_FLIP3D_POLICY               = 08; // [set] Designates how Flip3D will treat the window.
  DWMWA_EXTENDED_FRAME_BOUNDS       = 09; // [get] Gets the extended frame bounds rectangle in screen space
  DWMWA_HAS_ICONIC_BITMAP           = 10; // [set] Indicates an available bitmap when there is no better thumbnail representation.
  DWMWA_DISALLOW_PEEK               = 11; // [set] Don't invoke Peek on the window.
  DWMWA_EXCLUDED_FROM_PEEK          = 12; // [set] LivePreview exclusion information
  DWMWA_CLOAK                           = 13; // [set] Cloak or uncloak the window
  DWMWA_CLOAKED                         = 14; // [get] Gets the cloaked state of the window
  DWMWA_FREEZE_REPRESENTATION           = 15; // [set] BOOL, Force this window to freeze the thumbnail without live update
  DWMWA_PASSIVE_UPDATE_MODE             = 16; // [set] BOOL, Updates the window only when desktop composition runs for other reasons
  DWMWA_USE_HOSTBACKDROPBRUSH           = 17; // [set] BOOL, Allows the use of host backdrop brushes for the window.
  DWMWA_USE_IMMERSIVE_DARK_MODE         = 20; // [set] BOOL, Allows a window to either use the accent color, or dark, according to the user Color Mode preferences.
  DWMWA_WINDOW_CORNER_PREFERENCE        = 33; // [set] WINDOW_CORNER_PREFERENCE, Controls the policy that rounds top-level window corners
  DWMWA_BORDER_COLOR                    = 34; // [set] COLORREF, The color of the thin border around a top-level window
  DWMWA_CAPTION_COLOR                   = 35; // [set] COLORREF, The color of the caption
  DWMWA_TEXT_COLOR                      = 36; // [set] COLORREF, The color of the caption text
  DWMWA_VISIBLE_FRAME_BORDER_THICKNESS  = 37; // [get] UINT, width of the visible border around a thick frame window
  DWMWA_LAST                            = 38;
 
// Apply rounded corners in desktop apps for Windows 11
// DWM_WINDOW_CORNER_PREFERENCE

   DWMWCP_DEFAULT                                 = 0; // Let the system decide whether or not to round window corners
   DWMWCP_DONOTROUND                              = 1; // Never round window corners
   DWMWCP_ROUND                                   = 2; // Round the corners if appropriate
   DWMWCP_ROUNDSMALL                              = 3; // Round the corners if appropriate, with a small radius


// Use this constant to reset any window part colors to the system default behavior
   DWMWA_COLOR_DEFAULT = $FFFFFFFF;

// Use this constant to specify that a window part should not be rendered
   DWMWA_COLOR_NONE    = $FFFFFFFE;

// Non-client rendering policy attribute values
const
  DWMNCRP_USEWINDOWSTYLE = 0; // Enable/disable non-client rendering based on window style
  DWMNCRP_DISABLED       = 1; // Disabled non-client rendering; window style is ignored
  DWMNCRP_ENABLED        = 2; // Enabled non-client rendering; window style is ignored
  DWMNCRP_LAST           = 3;

// Values designating how Flip3D treats a given window.
const
  DWMFLIP3D_DEFAULT      = 0; // Hide or include the window in Flip3D based on window style and visibility.
  DWMFLIP3D_EXCLUDEBELOW = 1; // Display the window under Flip3D and disabled.
  DWMFLIP3D_EXCLUDEABOVE = 2; // Display the window above Flip3D and enabled.
  DWMFLIP3D_LAST         = 3;

// Cloaked flags describing why a window is cloaked.
const
  DWM_CLOAKED_APP         = $00000001;
  DWM_CLOAKED_SHELL       = $00000002;
  DWM_CLOAKED_INHERITED   = $00000004;

// Thumbnails
type
  HTHUMBNAIL = HANDLE;
  PHTHUMBNAIL = ^HTHUMBNAIL;

const
  DWM_TNP_RECTDESTINATION      = $00000001;
  DWM_TNP_RECTSOURCE           = $00000002;
  DWM_TNP_OPACITY              = $00000004;
  DWM_TNP_VISIBLE              = $00000008;
  DWM_TNP_SOURCECLIENTAREAONLY = $00000010;

type
  _DWM_THUMBNAIL_PROPERTIES = record
    dwFlags: DWORD;
    rcDestination: TRect;
    rcSource: TRect;
    opacity: Byte;
    fVisible: BOOL;
    fSourceClientAreaOnly: BOOL;
  end;
  DWM_THUMBNAIL_PROPERTIES = _DWM_THUMBNAIL_PROPERTIES;
  PDWM_THUMBNAIL_PROPERTIES = ^_DWM_THUMBNAIL_PROPERTIES;
  TDWMThumbnailProperties = DWM_THUMBNAIL_PROPERTIES;
  PDWMThumbnailProperties = PDWM_THUMBNAIL_PROPERTIES;

// Video enabling apis

type
  DWM_FRAME_COUNT = ULONGLONG;
  QPC_TIME = ULONGLONG;

type
  _UNSIGNED_RATIO = record
    uiNumerator: LongWord;
    uiDenominator: LongWord;
  end;
  UNSIGNED_RATIO = _UNSIGNED_RATIO;
  TUnsignedRatio = UNSIGNED_RATIO;

type
  _DWM_TIMING_INFO = record
    cbSize: LongWord;

    // Data on DWM composition overall

    // Monitor refresh rate
    rateRefresh: UNSIGNED_RATIO;

    // Actual period
    qpcRefreshPeriod: QPC_TIME;

    // composition rate
    rateCompose: UNSIGNED_RATIO;

    // QPC time at a VSync interupt
    qpcVBlank: QPC_TIME;

    // DWM refresh count of the last vsync
    // DWM refresh count is a 64bit number where zero is
    // the first refresh the DWM woke up to process
    cRefresh: DWM_FRAME_COUNT;

    // DX refresh count at the last Vsync Interupt
    // DX refresh count is a 32bit number with zero
    // being the first refresh after the card was initialized
    // DX increments a counter when ever a VSync ISR is processed
    // It is possible for DX to miss VSyncs
    //
    // There is not a fixed mapping between DX and DWM refresh counts
    // because the DX will rollover and may miss VSync interupts
    cDXRefresh: UINT;

    // QPC time at a compose time.
    qpcCompose: QPC_TIME;

    // Frame number that was composed at qpcCompose
    cFrame: DWM_FRAME_COUNT;

    // The present number DX uses to identify renderer frames
    cDXPresent: UINT;

    // Refresh count of the frame that was composed at qpcCompose
    cRefreshFrame: DWM_FRAME_COUNT;


    // DWM frame number that was last submitted
    cFrameSubmitted: DWM_FRAME_COUNT;

    // DX Present number that was last submitted
    cDXPresentSubmitted: UINT;

    // DWM frame number that was last confirmed presented
    cFrameConfirmed: DWM_FRAME_COUNT;

    // DX Present number that was last confirmed presented
    cDXPresentConfirmed: UINT;

    // The target refresh count of the last
    // frame confirmed completed by the GPU
    cRefreshConfirmed: DWM_FRAME_COUNT;

    // DX refresh count when the frame was confirmed presented
    cDXRefreshConfirmed: UINT;

    // Number of frames the DWM presented late
    // AKA Glitches
    cFramesLate: DWM_FRAME_COUNT;

    // the number of composition frames that
    // have been issued but not confirmed completed
    cFramesOutstanding: UINT;


    // Following fields are only relavent when an HWND is specified
    // Display frame


    // Last frame displayed
    cFrameDisplayed: DWM_FRAME_COUNT;

    // QPC time of the composition pass when the frame was displayed
    qpcFrameDisplayed: QPC_TIME;

    // Count of the VSync when the frame should have become visible
    cRefreshFrameDisplayed: DWM_FRAME_COUNT;

    // Complete frames: DX has notified the DWM that the frame is done rendering

    // ID of the the last frame marked complete (starts at 0)
    cFrameComplete: DWM_FRAME_COUNT;

    // QPC time when the last frame was marked complete
    qpcFrameComplete: QPC_TIME;

    // Pending frames:
    // The application has been submitted to DX but not completed by the GPU

    // ID of the the last frame marked pending (starts at 0)
    cFramePending: DWM_FRAME_COUNT;

    // QPC time when the last frame was marked pending
    qpcFramePending: QPC_TIME;

    // number of unique frames displayed
    cFramesDisplayed: DWM_FRAME_COUNT;

    // number of new completed frames that have been received
    cFramesComplete: DWM_FRAME_COUNT;

     // number of new frames submitted to DX but not yet complete
    cFramesPending: DWM_FRAME_COUNT;

    // number of frames available but not displayed, used or dropped
    cFramesAvailable: DWM_FRAME_COUNT;

    // number of rendered frames that were never
    // displayed because composition occurred too late
    cFramesDropped: DWM_FRAME_COUNT;

    // number of times an old frame was composed
    // when a new frame should have been used
    // but was not available
    cFramesMissed: DWM_FRAME_COUNT;

    // the refresh at which the next frame is
    // scheduled to be displayed
    cRefreshNextDisplayed: DWM_FRAME_COUNT;

    // the refresh at which the next DX present is
    // scheduled to be displayed
    cRefreshNextPresented: DWM_FRAME_COUNT;

    // The total number of refreshes worth of content
    // for this HWND that have been displayed by the DWM
    // since DwmSetPresentParameters was called
    cRefreshesDisplayed: DWM_FRAME_COUNT;

    // The total number of refreshes worth of content
    // that have been presented by the application
    // since DwmSetPresentParameters was called
    cRefreshesPresented: DWM_FRAME_COUNT;


    // The actual refresh # when content for this
    // window started to be displayed
    // it may be different than that requested
    // DwmSetPresentParameters
    cRefreshStarted: DWM_FRAME_COUNT;

    // Total number of pixels DX redirected
    // to the DWM.
    // If Queueing is used the full buffer
    // is transfered on each present.
    // If not queuing it is possible only
    // a dirty region is updated
    cPixelsReceived: ULONGLONG;

    // Total number of pixels drawn.
    // Does not take into account if
    // if the window is only partial drawn
    // do to clipping or dirty rect management
    cPixelsDrawn: ULONGLONG;

    // The number of buffers in the flipchain
    // that are empty.   An application can
    // present that number of times and guarantee
    // it won't be blocked waiting for a buffer to
    // become empty to present to
    cBuffersEmpty: DWM_FRAME_COUNT;

  end;
  DWM_TIMING_INFO = _DWM_TIMING_INFO;
  TDWMTimingInfo = DWM_TIMING_INFO;

const
  // Use the first source frame that
  // includes the first refresh of the output frame
  DWM_SOURCE_FRAME_SAMPLING_POINT = 0;

  // use the source frame that includes the most
  // refreshes of out the output frame
  // in case of multiple source frames with the
  // same coverage the last will be used
  DWM_SOURCE_FRAME_SAMPLING_COVERAGE = 1;

     // Sentinel value
  DWM_SOURCE_FRAME_SAMPLING_LAST = 2;

const
  c_DwmMaxQueuedBuffers = 8;
  c_DwmMaxMonitors = 16;
  c_DwmMaxAdapters = 16;

type
  _DWM_PRESENT_PARAMETERS = record
    cbSize: LongWord;
    fQueue: BOOL;
    cRefreshStart: DWM_FRAME_COUNT;
    cBuffer: UINT;
    fUseSourceRate: BOOL;
    rateSource: UNSIGNED_RATIO;
    cRefreshesPerFrame: UINT;
    eSampling: LongWord;
  end;
  DWM_PRESENT_PARAMETERS = _DWM_PRESENT_PARAMETERS;
  TDWMPresentParameters = DWM_PRESENT_PARAMETERS;

const
  DWM_FRAME_DURATION_DEFAULT = -1;

var
  DwmDefWindowProc: function(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; out plResult: LRESULT): BOOL; stdcall;

  DwmEnableBlurBehindWindow: function(hWnd: HWND; pBlurBehind: PDWM_BLURBEHIND): HRESULT; stdcall;

const
  DWM_EC_DISABLECOMPOSITION = 0;
  DWM_EC_ENABLECOMPOSITION  = 1;

var
  DwmEnableComposition: function(uCompositionAction: UINT): HRESULT; stdcall;

  DwmEnableMMCSS: function(fEnableMMCSS: BOOL): HRESULT; stdcall;

  DwmExtendFrameIntoClientArea: function(hWnd: HWND; pMarInset: PMARGINS): HRESULT; stdcall;

  DwmGetColorizationColor: function(out pcrColorization: DWORD; out pfOpaqueBlend: BOOL): HRESULT; stdcall;

  DwmGetCompositionTimingInfo: function(hwnd: HWND; out pTimingInfo: DWM_TIMING_INFO): HRESULT; stdcall;

  DwmGetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;

  DwmIsCompositionEnabled: function(out pfEnabled: BOOL): HRESULT; stdcall;

  DwmModifyPreviousDxFrameDuration: function(hwnd: HWND; cRefreshes: Integer; fRelative: BOOL): HRESULT; stdcall;

  DwmQueryThumbnailSourceSize: function(hThumbnail: HTHUMBNAIL; out pSize: TSIZE): HRESULT; stdcall;

  DwmRegisterThumbnail: function(hwndDestination: HWND; hwndSource: HWND; out phThumbnailId: HTHUMBNAIL): HRESULT; stdcall;

  DwmSetDxFrameDuration: function(hwnd: HWND; cRefreshes: Integer): HRESULT; stdcall;

  DwmSetPresentParameters: function(hwnd: HWND; var pPresentParams: DWM_PRESENT_PARAMETERS): HRESULT; stdcall;

  DwmSetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;

  DwmUnregisterThumbnail: function(hThumbnailId: HTHUMBNAIL): HRESULT; stdcall;

  DwmUpdateThumbnailProperties: function(hThumbnailId: HTHUMBNAIL; ptnProperties: PDWM_THUMBNAIL_PROPERTIES): HRESULT; stdcall;

// if(_WIN32_WINNT >= 0x0601)
const
  DWM_SIT_DISPLAYFRAME = $00000001; // Display a window frame around the provided bitmap

var
  DwmSetIconicThumbnail: function(hwnd: HWND; hbmp: HBITMAP; dwSITFlags: DWORD): HRESULT; stdcall;

  DwmSetIconicLivePreviewBitmap: function(hwnd: HWND; hbmp: HBITMAP; var ptClient: PPOINT; dwSITFlags: DWORD): HRESULT; stdcall;

  DwmInvalidateIconicBitmaps: function(hwnd: HWND): HRESULT; stdcall;

// endif /* _WIN32_WINNT >= 0x0601 */

var
  DwmAttachMilContent: function(hwnd: HWND): HRESULT; stdcall;

  DwmDetachMilContent: function(hwnd: HWND): HRESULT; stdcall;

  DwmFlush: function(): HRESULT; stdcall;

type
  _MilMatrix3x2D = record
    S_11: DOUBLE;
    S_12: DOUBLE;
    S_21: DOUBLE;
    S_22: DOUBLE;
    DX: DOUBLE;
    DY: DOUBLE;
  end;
  MilMatrix3x2D = _MilMatrix3x2D;
  TMilMatrix3x2D = MilMatrix3x2D;

var
  DwmGetGraphicsStreamTransformHint: function(uIndex: UINT; out pTransform: MilMatrix3x2D): HRESULT; stdcall;

  DwmGetGraphicsStreamClient: function(uIndex: UINT; out pClientUuid: TGUID): HRESULT; stdcall;

  DwmGetTransportAttributes: function(out pfIsRemoting: BOOL; out pfIsConnected: BOOL; out pDwGeneration: DWORD): HRESULT; stdcall;

implementation

const
  dwmlib = 'dwmapi.dll';

var
  DwmLibrary: THandle;
  ReferenceCount: Integer;  // We have to keep track of several load/unload calls.

procedure FreeDwmLibrary;
begin
  if ReferenceCount > 0 then
    Dec(ReferenceCount);

  if (DwmLibrary <> 0) and (ReferenceCount = 0) then
  begin
    FreeLibrary(DwmLibrary);
    DwmLibrary := 0;

    DwmDefWindowProc := nil;
    DwmEnableBlurBehindWindow := nil;
    DwmEnableComposition := nil;
    DwmEnableMMCSS := nil;
    DwmExtendFrameIntoClientArea := nil;
    DwmGetColorizationColor := nil;
    DwmGetCompositionTimingInfo := nil;
    DwmGetWindowAttribute := nil;
    DwmIsCompositionEnabled := nil;
    DwmModifyPreviousDxFrameDuration := nil;
    DwmQueryThumbnailSourceSize := nil;
    DwmRegisterThumbnail := nil;
    DwmSetDxFrameDuration := nil;
    DwmSetPresentParameters := nil;
    DwmSetWindowAttribute := nil;
    DwmUnregisterThumbnail := nil;
    DwmUpdateThumbnailProperties := nil;
    DwmAttachMilContent := nil;
    DwmDetachMilContent := nil;
    DwmFlush := nil;
    DwmGetGraphicsStreamTransformHint := nil;
    DwmGetGraphicsStreamClient := nil;
    DwmGetTransportAttributes := nil;

    DwmSetIconicThumbnail := nil;
    DwmSetIconicLivePreviewBitmap := nil;
    DwmInvalidateIconicBitmaps := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitDwmLibrary: Boolean;
begin
  Inc(ReferenceCount);

  if DwmLibrary = 0 then
  begin
    DwmLibrary := LoadLibrary(dwmlib);
    if DwmLibrary > 0 then
    begin
      // windows vista
      Pointer(DwmDefWindowProc) := GetProcAddress(DwmLibrary, 'DwmDefWindowProc');
      Pointer(DwmEnableBlurBehindWindow) := GetProcAddress(DwmLibrary, 'DwmEnableBlurBehindWindow');
      Pointer(DwmEnableComposition) := GetProcAddress(DwmLibrary, 'DwmEnableComposition');
      Pointer(DwmEnableMMCSS) := GetProcAddress(DwmLibrary, 'DwmEnableMMCSS');
      Pointer(DwmExtendFrameIntoClientArea) := GetProcAddress(DwmLibrary, 'DwmExtendFrameIntoClientArea');
      Pointer(DwmGetColorizationColor) := GetProcAddress(DwmLibrary, 'DwmGetColorizationColor');
      Pointer(DwmGetCompositionTimingInfo) := GetProcAddress(DwmLibrary, 'DwmGetCompositionTimingInfo');
      Pointer(DwmGetWindowAttribute) := GetProcAddress(DwmLibrary, 'DwmGetWindowAttribute');
      Pointer(DwmIsCompositionEnabled) := GetProcAddress(DwmLibrary, 'DwmIsCompositionEnabled');
      Pointer(DwmModifyPreviousDxFrameDuration) := GetProcAddress(DwmLibrary, 'DwmModifyPreviousDxFrameDuration');
      Pointer(DwmQueryThumbnailSourceSize) := GetProcAddress(DwmLibrary, 'DwmQueryThumbnailSourceSize');
      Pointer(DwmRegisterThumbnail) := GetProcAddress(DwmLibrary, 'DwmRegisterThumbnail');
      Pointer(DwmSetDxFrameDuration) := GetProcAddress(DwmLibrary, 'DwmSetDxFrameDuration');
      Pointer(DwmSetPresentParameters) := GetProcAddress(DwmLibrary, 'DwmSetPresentParameters');
      Pointer(DwmSetWindowAttribute) := GetProcAddress(DwmLibrary, 'DwmSetWindowAttribute');
      Pointer(DwmUnregisterThumbnail) := GetProcAddress(DwmLibrary, 'DwmUnregisterThumbnail');
      Pointer(DwmUpdateThumbnailProperties) := GetProcAddress(DwmLibrary, 'DwmUpdateThumbnailProperties');
      Pointer(DwmAttachMilContent) := GetProcAddress(DwmLibrary, 'DwmAttachMilContent');
      Pointer(DwmDetachMilContent) := GetProcAddress(DwmLibrary, 'DwmDetachMilContent');
      Pointer(DwmFlush) := GetProcAddress(DwmLibrary, 'DwmFlush');
      Pointer(DwmGetGraphicsStreamTransformHint) := GetProcAddress(DwmLibrary, 'DwmGetGraphicsStreamTransformHint');
      Pointer(DwmGetGraphicsStreamClient) := GetProcAddress(DwmLibrary, 'DwmGetGraphicsStreamClient');
      Pointer(DwmGetTransportAttributes) := GetProcAddress(DwmLibrary, 'DwmGetTransportAttributes');

      // windows 7
      Pointer(DwmSetIconicThumbnail) := GetProcAddress(DwmLibrary, 'DwmSetIconicThumbnail');
      Pointer(DwmSetIconicLivePreviewBitmap) := GetProcAddress(DwmLibrary, 'DwmSetIconicLivePreviewBitmap');
      Pointer(DwmInvalidateIconicBitmaps) := GetProcAddress(DwmLibrary, 'DwmInvalidateIconicBitmaps');
    end;
  end;
  Result := DwmLibrary > 0;
end;

function DwmCompositionEnabled: Boolean;
var
  B: BOOL;
begin
  Result := DwmLibrary > 0;
  if Result then
    Result := (DwmIsCompositionEnabled(B) = S_OK) and B;
end;

initialization
  ReferenceCount := 0;

finalization
  while ReferenceCount > 0 do
    FreeDwmLibrary;

end.
