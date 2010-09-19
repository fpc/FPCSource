//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
//
// Use of this sample source code is subject to the terms of the Microsoft
// license agreement under which you licensed this sample source code. If
// you did not accept the terms of the license agreement, you are not
// authorized to use this sample source code. For the terms of the license,
// please see the license agreement between you and Microsoft or, if applicable,
// see the LICENSE.RTF on your install media or the root of your tools installation.
// THE SAMPLE SOURCE CODE IS PROVIDED "AS IS", WITH NO WARRANTIES OR INDEMNITIES.
//
(*==========================================================================;
 *
 *  File:       ddraw.h
 *  Content:    DirectDraw include file
 *
 ***************************************************************************)
unit ddraw;

interface

{$MODE objfpc}{$H+}
{$PACKRECORDS c}

uses
  Windows;

//#pragma once

//#include <windows.h>

//#ifdef __cplusplus
//extern "C" {
//#endif


// Other components use this define to tell if they have the DirectDraw
// defintitions presents or if they have to re-define stuff.

//#define __DDRAW_INCLUDED__

(*
 * GUIDS used by DirectDraw objects
 *)

const
  IID_IDirectDraw: TGuid             = '{9c59509a-39bd-11d1-8c4a-00c04fd930c5}';
  IID_IDirectDrawSurface: TGuid      = '{0b0e83e4-f37f-11d2-8b15-00c04f689292}';
  IID_IDirectDrawPalette: TGuid      = '{6C14DB84-A733-11CE-A521-0020AF0BE560}';
  IID_IDirectDrawClipper: TGuid      = '{6C14DB85-A733-11CE-A521-0020AF0BE560}';
  IID_IDirectDrawColorControl: TGuid = '{4B9F0EE0-0D7E-11D0-9B06-00A0C903A3B8}';
  IID_IDirectDrawGammaControl: TGuid = '{69C11C3E-B46B-11D1-AD7A-00C04FC29B4E}';

(*
 * Forward interface declerations
 *)

type
  IDirectDraw             = interface;
  IDirectDrawSurface      = interface;
  IDirectDrawPalette      = interface;
  IDirectDrawClipper      = interface;
  IDirectDrawColorControl = interface;
  IDirectDrawGammaControl = interface;

{typedef struct IDirectDraw              *LPDIRECTDRAW;
typedef struct IDirectDrawSurface       *LPDIRECTDRAWSURFACE;
typedef struct IDirectDrawPalette		*LPDIRECTDRAWPALETTE;
typedef struct IDirectDrawClipper		*LPDIRECTDRAWCLIPPER;
typedef struct IDirectDrawColorControl	*LPDIRECTDRAWCOLORCONTROL;
typedef struct IDirectDrawGammaControl  *LPDIRECTDRAWGAMMACONTROL;}

(*
 * DirectDraw Structures
 *)

(*
 * DDCOLORKEY
 *)

  PDDCOLORKEY = ^TDDCOLORKEY;
  TDDCOLORKEY = record
    dwColorSpaceLowValue: DWORD;	// low boundary of color space that is to
			                // be treated as Color Key, inclusive
    dwColorSpaceHighValue: DWORD;	// high boundary of color space that is
			                // to be treated as Color Key, inclusive
  end;

(*
 * DDARGB
 * Generic pixel format with 8-bit RGB and alpha components
 *)

  PDDARGB = ^TDDARGB;
  TDDARGB = record
    blue: BYTE;
    green: BYTE;
    red: BYTE;
    alpha: BYTE;

  end;

(*
 * DDBLTFX
 *)

  PDDBLTFX = ^TDDBLTFX;
  TDDBLTFX = record
    dwSize: DWORD;                 // size of structure
    dwROP: DWORD;                  // Win32 raster operations
    dwFillColor: DWORD;            // color in RGB or Palettized (Brush value for Win32 ROPs)
    ddckDestColorkey: TDDCOLORKEY;		// DestColorkey override
    ddckSrcColorkey: TDDCOLORKEY;		// SrcColorkey override

  end;

(*
 * DDALPHABLTFX
 *)

  PDDALPHABLTFX = ^TDDALPHABLTFX;
  TDDALPHABLTFX = record
    dwSize: DWORD;                 // size of structure
    ddargbScaleFactors: TDDARGB;   // Constant scaling factors
    dwFillColor: DWORD;            // color in ARGB or Palettized

  end;

(*
 * DDSCAPS
 * Caps bits defined below.
 *)

  PDDSCAPS = ^TDDSCAPS;
  TDDSCAPS = record
    dwCaps: DWORD;

  end;

(*
 * DDCAPS
 *)

const
  DD_ROP_SPACE = 256 div 32;       // space required to store ROP array

type
  PDDCAPS = ^TDDCAPS;
  TDDCAPS = record
    dwSize: DWORD;			        // size of the DDCAPS structure

    // Surface capabilities

    dwVidMemTotal: DWORD;          // total amount of video memory
    dwVidMemFree: DWORD;           // amount of free video memory
    dwVidMemStride: DWORD;         // video memory stride (0 if linear)

    ddsCaps: TDDSCAPS;                // surface caps

    dwNumFourCCCodes: DWORD;       // number of four cc codes

    // Palette capabilities

    dwPalCaps: DWORD;              // palette capabilities

    // Hardware blitting capabilities

    dwBltCaps: DWORD;              // driver specific capabilities
    dwCKeyCaps: DWORD;		        // color key blitting capabilities
    dwAlphaCaps: DWORD;	        // alpha blitting capabilities
    dwRops: array [0..DD_ROP_SPACE-1] of DWORD;	// ROPS supported

    // Overlay capabilities

    dwOverlayCaps: DWORD;          // general overlay capabilities.

    dwMaxVisibleOverlays: DWORD;	// maximum number of visible overlays
    dwCurrVisibleOverlays: DWORD;	// current number of visible overlays

    dwAlignBoundarySrc: DWORD;	    // source rectangle alignment
    dwAlignSizeSrc: DWORD;		    // source rectangle byte size
    dwAlignBoundaryDest: DWORD;	// dest rectangle alignment
    dwAlignSizeDest: DWORD;	    // dest rectangle byte size

    dwMinOverlayStretch: DWORD;	// minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxOverlayStretch: DWORD;	// maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3

    // Miscalenous capabilies

    dwMiscCaps: DWORD;

  end;

(*
 * DDPIXELFORMAT
 *)

  PDDPIXELFORMAT = ^TDDPIXELFORMAT;
  TDDPIXELFORMAT = record
    dwSize: DWORD;			// size of structure
    dwFlags: DWORD;		// pixel format flags
    dwFourCC: DWORD;		// (FOURCC code)
    case Integer of
    0: (
      dwRGBBitCount: DWORD;		// how many bits per pixel
    );
    1: (
      dwYUVBitCount: DWORD;		// how many bits per pixel
    );
    2: (
      dwAlphaBitDepth: DWORD;	// how many bits for alpha channels

      case Integer of
      0: (
        dwRBitMask: DWORD;		// mask for red bit
      );
      1: (
        dwYBitMask: DWORD;		// mask for Y bits

        case Integer of
        0: (
          dwGBitMask: DWORD;		// mask for green bits
        );
        1: (
          dwUBitMask: DWORD;		// mask for U bits

          case Integer of
          0: (
            dwBBitMask: DWORD;		// mask for blue bits
          );
          1: (
            dwVBitMask: DWORD;		// mask for V bits

            dwRGBAlphaBitMask: DWORD;	// mask for alpha channel
          );
        );
      );
    );
  end;

(*
 * DDPIXELFORMAT dwFlags values.
 *)

const
  DDPF_ALPHAPIXELS    = $00000001;
  DDPF_ALPHA          = $00000002;
  DDPF_FOURCC         = $00000004;
  DDPF_PALETTEINDEXED = $00000020;
  DDPF_RGB            = $00000040;
  DDPF_ALPHAPREMULT   = $00008000;

  DDPF_VALID          = DDPF_ALPHAPIXELS or
                        DDPF_ALPHA or
                        DDPF_FOURCC or
                        DDPF_PALETTEINDEXED or
                        DDPF_RGB or
                        DDPF_ALPHAPREMULT;

(*
 * DDSURFACEDESC
 *)

type
  PDDSURFACEDESC = ^TDDSURFACEDESC;
  TDDSURFACEDESC = record
    dwSize: DWORD;                    // size of the DDSURFACEDESC structure
    dwFlags: DWORD;                   // determines what fields are valid
    dwHeight: DWORD;                  // height of surface to be created
    dwWidth: DWORD;                   // width of input surface
    lPitch: LONG;                     // bytes to next line down (return value only)
    lXPitch: LONG;                    // bytes to next pixel right (return value only)
    dwBackBufferCount: DWORD;         // number of back buffers requested
    dwRefreshRate: DWORD;             // refresh rate (used when display mode is described)
    lpSurface: LPVOID;                // pointer to the associated surface memory
    ddckCKDestOverlay: TDDCOLORKEY;   // color key for destination overlay use
    ddckCKDestBlt: TDDCOLORKEY;       // color key for destination blt use
    ddckCKSrcOverlay: TDDCOLORKEY;    // color key for source overlay use
    ddckCKSrcBlt: TDDCOLORKEY;        // color key for source blt use
    ddpfPixelFormat: TDDPIXELFORMAT;  // pixel format description of the surface
    ddsCaps: TDDSCAPS;                // direct draw surface capabilities
    dwSurfaceSize: DWORD;             // Surface size, in bytes

  end;

(*
 * DDSURFACEDESC dwFlags values.
 *)

const
  DDSD_CAPS            = $00000001;
  DDSD_HEIGHT          = $00000002;
  DDSD_WIDTH           = $00000004;
  DDSD_PITCH           = $00000008;
  DDSD_XPITCH          = $00000010;
  DDSD_BACKBUFFERCOUNT = $00000020;
  DDSD_LPSURFACE       = $00000800;
  DDSD_PIXELFORMAT     = $00001000;
  DDSD_CKDESTOVERLAY   = $00002000;
  DDSD_CKDESTBLT       = $00004000;
  DDSD_CKSRCOVERLAY    = $00008000;
  DDSD_CKSRCBLT        = $00010000;
  DDSD_REFRESHRATE     = $00040000;
  DDSD_SURFACESIZE     = $00080000;

  DDSD_VALID           = DDSD_CAPS or
                         DDSD_HEIGHT or
                         DDSD_WIDTH or
                         DDSD_PITCH or
                         DDSD_XPITCH or
                         DDSD_BACKBUFFERCOUNT or
                         DDSD_LPSURFACE or
                         DDSD_PIXELFORMAT or
                         DDSD_CKDESTOVERLAY or
                         DDSD_CKDESTBLT or
                         DDSD_CKSRCOVERLAY or
                         DDSD_CKSRCBLT or
                         DDSD_REFRESHRATE or
                         DDSD_SURFACESIZE;

  DDSD_ENUM_VALID      = DDSD_CAPS or
                         DDSD_HEIGHT or
                         DDSD_WIDTH or
                         DDSD_PIXELFORMAT;

(*
 * DDOVERLAYFX
 *)

type
  PDDOVERLAYFX = ^TDDOVERLAYFX;
  TDDOVERLAYFX = record
    dwSize: DWORD;                  // size of structure

    dwAlphaConstBitDepth: DWORD;    // Bit depth used to specify alpha constant.
    dwAlphaConst: DWORD;            // Constant to use as alpha channel.

    dckDestColorkey: TDDCOLORKEY;         // DestColorkey override
    dckSrcColorkey: TDDCOLORKEY;          // DestColorkey override

  end;

(*
 * DDGAMMARAMP
 *)

  PDDGAMMARAMP = ^TDDGAMMARAMP;
  TDDGAMMARAMP = record
    red: array [0..256-1] of WORD;
    green: array [0..256-1] of WORD;
    blue: array [0..256-1] of WORD;

  end;

(*
 *  This is the structure within which DirectDraw returns data about the current graphics driver and chipset
 *)

const
  MAX_DDDEVICEID_STRING = 512;

type
  PDDDEVICEIDENTIFIER = ^TDDDEVICEIDENTIFIER;
  TDDDEVICEIDENTIFIER = record
    (*
     * These elements are for presentation to the user only. They should not be used to identify particular
     * drivers, since this is unreliable and many different strings may be associated with the same
     * device, and the same driver from different vendors.
     *)

    szDriver: array [0..MAX_DDDEVICEID_STRING-1] of wchar_t;
    szDescription: array [0..MAX_DDDEVICEID_STRING-1] of wchar_t;

    (*
     * This element is the version of the DirectDraw/3D driver. It is legal to do <, > comparisons
     * on the whole 64 bits. Caution should be exercised if you use this element to identify problematic
     * drivers. It is recommended that guidDeviceIdentifier is used for this purpose.
     *
     * This version has the form:
     *  wProduct = HIWORD(liDriverVersion.HighPart)
     *  wVersion = LOWORD(liDriverVersion.HighPart)
     *  wSubVersion = HIWORD(liDriverVersion.LowPart)
     *  wBuild = LOWORD(liDriverVersion.LowPart)
     *)

    liDriverVersion: LARGE_INTEGER;


    (*
     * These elements can be used to identify particular chipsets. Use with extreme caution. 
     *   dwVendorId     Identifies the manufacturer. May be zero if unknown.
     *   dwDeviceId     Identifies the type of chipset. May be zero if unknown.
     *   dwSubSysId     Identifies the subsystem, typically this means the particular board. May be zero if unknown.
     *   dwRevision     Identifies the revision level of the chipset. May be zero if unknown.
     *)

    dwVendorId: DWORD;
    dwDeviceId: DWORD;
    dwSubSysId: DWORD;
    dwRevision: DWORD;

    (*
     * This element can be used to check changes in driver/chipset. This GUID is a unique identifier for the
     * driver/chipset pair. Use this element if you wish to track changes to the driver/chipset in order to
     * reprofile the graphics subsystem.
     * This element can also be used to identify particular problematic drivers.
     *)

    guidDeviceIdentifier: TGuid;

  end;

(*
 * DDCOLORCONTROL
 *)

  PDDCOLORCONTROL = ^TDDCOLORCONTROL;
  TDDCOLORCONTROL = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lBrightness: LONG;
    lContrast: LONG;
    lHue: LONG;
    lSaturation: LONG;
    lSharpness: LONG;
    lGamma: LONG;
    lColorEnable: LONG;

  end;

(*
 * DDCOLORCONTROL dwFlags values.
 *)

const
  DDCOLOR_BRIGHTNESS  = $00000001;
  DDCOLOR_CONTRAST    = $00000002;
  DDCOLOR_HUE         = $00000004;
  DDCOLOR_SATURATION  = $00000008;
  DDCOLOR_SHARPNESS   = $00000010;
  DDCOLOR_GAMMA       = $00000020;
  DDCOLOR_COLORENABLE = $00000040;

  DDCOLOR_VALID       = DDCOLOR_BRIGHTNESS or
                        DDCOLOR_CONTRAST or
                        DDCOLOR_HUE or
                        DDCOLOR_SATURATION or
                        DDCOLOR_SHARPNESS or
                        DDCOLOR_GAMMA or
                        DDCOLOR_COLORENABLE;

(*
 * API's
 *)

//typedef BOOL (PASCAL * LPDDENUMCALLBACKEX)(LPGUID, LPWSTR, LPWSTR, LPVOID, HMONITOR);
type
  LPDDENUMCALLBACKEX = function(lpGUID: PGUID; lpDriverDescription: LPWSTR; lpDriverName: LPWSTR; lpContext: LPVOID; hm: HMONITOR): BOOL; cdecl;


(*
 * DirectDrawEnumerateEx Flags
 *)

(*
 * This flag causes enumeration of any GDI display devices which are part of
 * the Windows Desktop
 *)
const
  DDENUM_ATTACHEDSECONDARYDEVICES     = $00000001;

(*
 * This flag causes enumeration of any GDI display devices which are not
 * part of the Windows Desktop
 *)
  DDENUM_DETACHEDSECONDARYDEVICES     = $00000002;

  DDENUM_VALID                        = DDENUM_ATTACHEDSECONDARYDEVICES or
                                        DDENUM_DETACHEDSECONDARYDEVICES;

(*
 * Enumeration function pointer types
 *)

//typedef HRESULT (FAR PASCAL * LPDDENUMMODESCALLBACK)(LPDDSURFACEDESC, LPVOID);
type
  LPDDENUMMODESCALLBACK = function(lpDDSurfaceDesc: PDDSURFACEDESC; lpContext: LPVOID): HRESULT; cdecl;
//typedef HRESULT (FAR PASCAL * LPDDENUMSURFACESCALLBACK)(LPDIRECTDRAWSURFACE, LPDDSURFACEDESC, LPVOID);
  LPDDENUMSURFACESCALLBACK = function(lpDDSurface: IDirectDrawSurface; lpDDSurfaceDesc: PDDSURFACEDESC; lpContext: LPVOID): HRESULT; cdecl;

(*
 * IDirectDraw
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDraw
//DECLARE_INTERFACE_(IDirectDraw, IUnknown)
  IDirectDraw = interface(IUnknown)
    ['{9c59509a-39bd-11d1-8c4a-00c04fd930c5}']
    (*** IDirectDraw methods ***)
    function CreateClipper(dwFlags: DWORD; out lplpDDClipper: IDirectDrawClipper; pUnkOuter: IUnknown): HRESULT; cdecl;
    function CreatePalette(dwFlags: DWORD; lpDDColorArray: PPALETTEENTRY; out lplpDDPalette: IDirectDrawPalette; pUnkOuter: IUnknown): HRESULT; cdecl;
    function CreateSurface(lpDDSurfaceDesc: PDDSURFACEDESC; out lplpDDSurface: IDirectDrawSurface; pUnkOuter: IUnknown): HRESULT; cdecl;
    function EnumDisplayModes(dwFlags: DWORD; lpDDSurfaceDesc: PDDSURFACEDESC; lpContext: LPVOID; lpEnumModesCallback: LPDDENUMMODESCALLBACK): HRESULT; cdecl;
    function EnumSurfaces(dwFlags: DWORD; lpDDSD: PDDSURFACEDESC; lpContext: LPVOID; lpEnumSurfacesCallback: LPDDENUMSURFACESCALLBACK): HRESULT; cdecl;
    function FlipToGDISurface: HRESULT; cdecl;
    function GetCaps(lpDDDriverCaps: PDDCAPS; lpDDEmulCaps: PDDCAPS): HRESULT; cdecl;
    function GetDisplayMode(lpDDSurfaceDesc: PDDSURFACEDESC): HRESULT; cdecl;
    function GetFourCCCodes(lpNumCodes: LPDWORD; lpCodes: LPDWORD): HRESULT; cdecl;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HRESULT; cdecl;
    function GetMonitorFrequency(lpdwFrequency: LPDWORD): HRESULT; cdecl;
    function GetScanLine(lpdwScanLine: LPDWORD): HRESULT; cdecl;
    function GetVerticalBlankStatus(lpbIsInVB: LPBOOL): HRESULT; cdecl;
    function RestoreDisplayMode: HRESULT; cdecl;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: DWORD): HRESULT; cdecl;
    function SetDisplayMode(dwWidth: DWORD; dwHeight: DWORD; dwBPP: DWORD; dwRefreshRate: DWORD; dwFlags: DWORD): HRESULT; cdecl;
    function WaitForVerticalBlank(dwFlags: DWORD; hEvent: HANDLE): HRESULT; cdecl;
    function GetAvailableVidMem(lpDDSCaps: PDDSCAPS; lpdwTotal: LPDWORD; lpdwFree: LPDWORD): HRESULT; cdecl;
    function GetSurfaceFromDC(hdc: HDC; out lpDDS: IDirectDrawSurface): HRESULT; cdecl;
    function RestoreAllSurfaces: HRESULT; cdecl;
    function TestCooperativeLevel: HRESULT; cdecl;
    function GetDeviceIdentifier(lpDDDeviceIdentifier: PDDDEVICEIDENTIFIER; dwFlags: DWORD): HRESULT; cdecl;
  end;

(*
 * IDirectDrawPalette
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDrawPalette
//DECLARE_INTERFACE_(IDirectDrawPalette, IUnknown)
  IDirectDrawPalette = interface(IUnknown)
    ['{6C14DB84-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawPalette methods ***)
    function GetCaps(lpdwCaps: LPDWORD): HRESULT; cdecl;
    function GetEntries(dwFlags: DWORD; dwBase: DWORD; dwNumEntries: DWORD; lpEntries: LPPALETTEENTRY): HRESULT; cdecl;
    function SetEntries(dwFlags: DWORD; dwStartingEntry: DWORD; dwCount: DWORD; lpEntries: LPPALETTEENTRY): HRESULT; cdecl;
  end;

(*
 * IDirectDrawClipper
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDrawClipper
//DECLARE_INTERFACE_(IDirectDrawClipper, IUnknown)
{$WARNING PHWND is missing from windows unit}
  PHWND = ^HWND;
  IDirectDrawClipper = interface(IUnknown)
    ['{6C14DB85-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawClipper methods ***)
    function GetClipList(lpRect: LPRECT; lpClipList: LPRGNDATA; lpdwSize: LPDWORD): HRESULT; cdecl;
    function GetHWnd(lphWnd: PHWND): HRESULT; cdecl;
    function IsClipListChanged(lpbChanged: LPBOOL): HRESULT; cdecl;
    function SetClipList(lpClipList: LPRGNDATA; dwFlags: DWORD): HRESULT; cdecl;
    function SetHWnd(dwFlags: DWORD; hWnd: HWND): HRESULT; cdecl;
  end;

(*
 * IDirectDrawSurface
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDrawSurface
//DECLARE_INTERFACE_(IDirectDrawSurface, IUnknown)
{$WARNING PHDC is missing from windows unit}
  PHDC = ^HDC;
  IDirectDrawSurface = interface(IUnknown)
    ['{0b0e83e4-f37f-11d2-8b15-00c04f689292}']
    (*** IDirectDrawSurface methods ***)
    function AddOverlayDirtyRect(lpRect: LPRECT): HRESULT; cdecl;
    function Blt(lpDestRect: LPRECT; lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: LPRECT; dwFlags: DWORD; lpDDBltFx: PDDBLTFX): HRESULT; cdecl;
    function EnumAttachedSurfaces(lpContext: LPVOID; lpEnumSurfacesCallback: LPDDENUMSURFACESCALLBACK): HRESULT; cdecl;
    function EnumOverlayZOrders(dwFlags: DWORD; lpContext: LPVOID; lpfnCallback: LPDDENUMSURFACESCALLBACK): HRESULT; cdecl;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface; dwFlags: DWORD): HRESULT; cdecl;
    function GetBltStatus(dwFlags: DWORD): HRESULT; cdecl;
    function GetCaps(lpDDSCaps: PDDSCAPS): HRESULT; cdecl;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HRESULT; cdecl;
    function GetColorKey(dwFlags: DWORD; lpDDColorKey: PDDCOLORKEY): HRESULT; cdecl;
    function GetDC(lphDC: PHDC): HRESULT; cdecl;
    function GetFlipStatus(dwFlags: DWORD): HRESULT; cdecl;
    function GetOverlayPosition(lplX: LPLONG; lplY: LPLONG): HRESULT; cdecl;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HRESULT; cdecl;
    function GetPixelFormat(lpDDPixelFormat: PDDPIXELFORMAT): HRESULT; cdecl;
    function GetSurfaceDesc(lpDDSurfaceDesc: PDDSURFACEDESC): HRESULT; cdecl;
    function IsLost: HRESULT; cdecl;
    function Lock(lpDestRect: LPRECT; lpDDSurfaceDesc: PDDSURFACEDESC; dwFlags: DWORD; hEvent: HANDLE): HRESULT; cdecl;
    function ReleaseDC(hDC: HDC): HRESULT; cdecl;
    function Restore: HRESULT; cdecl;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HRESULT; cdecl;
    function SetColorKey(dwFlags: DWORD; lpDDColorKey: PDDCOLORKEY): HRESULT; cdecl;
    function SetOverlayPosition(lX: LONG; lY: LONG): HRESULT; cdecl;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HRESULT; cdecl;
    function Unlock(lpRect: LPRECT): HRESULT; cdecl;
    function UpdateOverlay(lpSrcRect: LPRECT; lpDDDestSurface: IDirectDrawSurface; lpDestRect: LPRECT; dwFlags: DWORD; lpDDOverlayFx: PDDOVERLAYFX): HRESULT; cdecl;
    function UpdateOverlayZOrder(dwFlags: DWORD; lpDDSReference: IDirectDrawSurface): HRESULT; cdecl;
    function GetDDInterface(out lplpDD: IDirectDraw): HRESULT; cdecl;
    function AlphaBlt(lpDestRect: LPRECT; lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: LPRECT; dwFlags: DWORD; lpDDAlphaBltFX: PDDALPHABLTFX): HRESULT; cdecl;
  end;

(*
 * IDirectDrawColorControl
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDrawColorControl
//DECLARE_INTERFACE_(IDirectDrawColorControl, IUnknown)
  IDirectDrawColorControl = interface(IUnknown)
    ['{4B9F0EE0-0D7E-11D0-9B06-00A0C903A3B8}']
    (*** IDirectDrawColorControl methods ***)
    function GetColorControls(lpColorControl: PDDCOLORCONTROL): HRESULT; cdecl;
    function SetColorControls(lpColorControl: PDDCOLORCONTROL): HRESULT; cdecl;
  end;

(*
 * IDirectDrawGammaControl
 *)

//#undef INTERFACE
//#define INTERFACE IDirectDrawGammaControl
//DECLARE_INTERFACE_(IDirectDrawGammaControl, IUnknown)
  IDirectDrawGammaControl = interface(IUnknown)
    ['{69C11C3E-B46B-11D1-AD7A-00C04FC29B4E}']
    (*** IDirectDrawColorControl methods ***)
    function GetGammaRamp(dwFlags: DWORD; lpGammaRamp: PDDGAMMARAMP): HRESULT; cdecl;
    function SetGammaRamp(dwFlags: DWORD; lpGammaRamp: PDDGAMMARAMP): HRESULT; cdecl;
  end;

(*
 * DirectDraw Macros
 *)


(*
 * Direct Draw Capability Flags
 *)

(*
 * Surface Capability Flags
 *)

const
  DDSCAPS_ALPHA           = $00000001;
  DDSCAPS_BACKBUFFER      = $00000002;
  DDSCAPS_FLIP            = $00000004;
  DDSCAPS_FRONTBUFFER     = $00000008;
  DDSCAPS_OVERLAY         = $00000010;
  DDSCAPS_PALETTE         = $00000020;
  DDSCAPS_PRIMARYSURFACE  = $00000040;
  DDSCAPS_SYSTEMMEMORY    = $00000080;
  DDSCAPS_VIDEOMEMORY     = $00000100;
  DDSCAPS_WRITEONLY       = $00000200;
  DDSCAPS_READONLY        = $00000800;
  DDSCAPS_NOTUSERLOCKABLE = $00002000;
  DDSCAPS_DYNAMIC         = $00004000;

  DDSCAPS_VALID           = DDSCAPS_ALPHA or
                            DDSCAPS_BACKBUFFER or
                            DDSCAPS_FLIP or
                            DDSCAPS_FRONTBUFFER or
                            DDSCAPS_OVERLAY or
                            DDSCAPS_PALETTE or
                            DDSCAPS_PRIMARYSURFACE or
                            DDSCAPS_SYSTEMMEMORY or
                            DDSCAPS_VIDEOMEMORY or
                            DDSCAPS_WRITEONLY or
                            DDSCAPS_READONLY or
                            DDSCAPS_NOTUSERLOCKABLE or
                            DDSCAPS_DYNAMIC;

(*
 * Palette Capability Flags
 *)

  DDPCAPS_PRIMARYSURFACE = $00000010;
  DDPCAPS_ALPHA          = $00000400;

  DDPCAPS_VALID          = DDPCAPS_PRIMARYSURFACE or
                           DDPCAPS_ALPHA;

(*
 * DirectDraw Capability Flags
 *)

(*
 * General hardware blitting capabilities (For DDCAPS dwBltCaps field)
 *)

  DDBLTCAPS_READSYSMEM  = $00000001;
  DDBLTCAPS_WRITESYSMEM = $00000002;
  DDBLTCAPS_FOURCCTORGB = $00000004;
  DDBLTCAPS_COPYFOURCC  = $00000008;
  DDBLTCAPS_FILLFOURCC  = $00000010;

  DDBLTCAPS_VALID       = DDBLTCAPS_READSYSMEM or
                          DDBLTCAPS_WRITESYSMEM or
                          DDBLTCAPS_FOURCCTORGB or
                          DDBLTCAPS_COPYFOURCC or
                          DDBLTCAPS_FILLFOURCC;

(*
 * Hardware color key blitting capabilities (For DDCAPS dwCKeyCaps field)
 *)

  DDCKEYCAPS_DESTBLT            = $00000001;
  DDCKEYCAPS_DESTBLTCLRSPACE    = $00000002;
  DDCKEYCAPS_DESTBLTCLRSPACEYUV = $00000004;
  DDCKEYCAPS_SRCBLT             = $00000200;
  DDCKEYCAPS_SRCBLTCLRSPACE     = $00000400;
  DDCKEYCAPS_SRCBLTCLRSPACEYUV  = $00000800;
  DDCKEYCAPS_BOTHBLT            = $00001000;

  DDCKEYCAPS_VALID              = DDCKEYCAPS_DESTBLT or
                                  DDCKEYCAPS_DESTBLTCLRSPACE or
                                  DDCKEYCAPS_DESTBLTCLRSPACEYUV or
                                  DDCKEYCAPS_SRCBLT or
                                  DDCKEYCAPS_SRCBLTCLRSPACE or
                                  DDCKEYCAPS_SRCBLTCLRSPACEYUV or
                                  DDCKEYCAPS_BOTHBLT;

(*
 * Hardware alpha blitting capabilities (For DDCAPS dwAlphaCaps field)
 *)

  DDALPHACAPS_ALPHAPIXELS       = $00000001;
  DDALPHACAPS_ALPHASURFACE      = $00000002;
  DDALPHACAPS_ALPHAPALETTE      = $00000004;
  DDALPHACAPS_ALPHACONSTANT     = $00000008;
  DDALPHACAPS_ARGBSCALE         = $00000010;
  DDALPHACAPS_SATURATE          = $00000020;
  DDALPHACAPS_PREMULT           = $00000040;
  DDALPHACAPS_NONPREMULT        = $00000080;
  DDALPHACAPS_ALPHAFILL         = $00000800;
  DDALPHACAPS_ALPHANEG          = $00000100;

  DDALPHACAPS_VALID             = DDALPHACAPS_ALPHAPIXELS or
                                  DDALPHACAPS_ALPHASURFACE or
                                  DDALPHACAPS_ALPHAPALETTE or
                                  DDALPHACAPS_ALPHACONSTANT or
                                  DDALPHACAPS_ARGBSCALE or
                                  DDALPHACAPS_SATURATE or
                                  DDALPHACAPS_PREMULT or
                                  DDALPHACAPS_NONPREMULT or
                                  DDALPHACAPS_ALPHAFILL or
                                  DDALPHACAPS_ALPHANEG;

(*
 * Overlay capabilities ( For DDCAPS dwOverlayCaps field)
 *)

  DDOVERLAYCAPS_FLIP                 = $00000001;
  DDOVERLAYCAPS_FOURCC               = $00000004;
  DDOVERLAYCAPS_ZORDER               = $00000008;
  DDOVERLAYCAPS_MIRRORLEFTRIGHT      = $00000010;
  DDOVERLAYCAPS_MIRRORUPDOWN         = $00000020;
  DDOVERLAYCAPS_CKEYSRC              = $00000040;
  DDOVERLAYCAPS_CKEYSRCCLRSPACE      = $00000080;
  DDOVERLAYCAPS_CKEYSRCCLRSPACEYUV   = $00000100;
  DDOVERLAYCAPS_CKEYDEST             = $00000200;
  DDOVERLAYCAPS_CKEYDESTCLRSPACE     = $00000400;
  DDOVERLAYCAPS_CKEYDESTCLRSPACEYUV  = $00000800;
  DDOVERLAYCAPS_CKEYBOTH             = $00001000;
  DDOVERLAYCAPS_ALPHADEST            = $00002000;
  DDOVERLAYCAPS_ALPHASRC             = $00008000;
  DDOVERLAYCAPS_ALPHADESTNEG         = $00002000;
  DDOVERLAYCAPS_ALPHASRCNEG          = $00008000;
  DDOVERLAYCAPS_ALPHACONSTANT        = $00010000;
  DDOVERLAYCAPS_ALPHAPREMULT         = $00040000;
  DDOVERLAYCAPS_ALPHANONPREMULT      = $00080000;
  DDOVERLAYCAPS_ALPHAANDKEYDEST      = $00100000;
  DDOVERLAYCAPS_OVERLAYSUPPORT       = $80000000;

  DDOVERLAYCAPS_VALID                = DDOVERLAYCAPS_FLIP or
                                       DDOVERLAYCAPS_FOURCC or
                                       DDOVERLAYCAPS_ZORDER or
                                       DDOVERLAYCAPS_MIRRORLEFTRIGHT or
                                       DDOVERLAYCAPS_MIRRORUPDOWN or
                                       DDOVERLAYCAPS_CKEYSRC or
                                       DDOVERLAYCAPS_CKEYSRCCLRSPACE or
                                       DDOVERLAYCAPS_CKEYSRCCLRSPACEYUV or
                                       DDOVERLAYCAPS_CKEYDEST or
                                       DDOVERLAYCAPS_CKEYDESTCLRSPACE or
                                       DDOVERLAYCAPS_CKEYDESTCLRSPACEYUV or
                                       DDOVERLAYCAPS_CKEYBOTH or
                                       DDOVERLAYCAPS_ALPHADEST or
                                       DDOVERLAYCAPS_ALPHASRC or
                                       DDOVERLAYCAPS_ALPHADESTNEG or
                                       DDOVERLAYCAPS_ALPHASRCNEG or
                                       DDOVERLAYCAPS_ALPHACONSTANT or
                                       DDOVERLAYCAPS_ALPHAPREMULT or
                                       DDOVERLAYCAPS_ALPHANONPREMULT or
                                       DDOVERLAYCAPS_ALPHAANDKEYDEST or
                                       DDOVERLAYCAPS_OVERLAYSUPPORT;

(*
 * Miscellaneous Capability Flags (For DDCAPS dwMiscCaps member)
 *)

  DDMISCCAPS_READSCANLINE        = $00000001;
  DDMISCCAPS_READMONITORFREQ     = $00000002;
  DDMISCCAPS_READVBLANKSTATUS    = $00000004;
  DDMISCCAPS_FLIPINTERVAL        = $00000008;
  DDMISCCAPS_FLIPODDEVEN         = $00000010;
  DDMISCCAPS_FLIPVSYNCWITHVBI    = $00000020;
  DDMISCCAPS_COLORCONTROLOVERLAY = $00000040;
  DDMISCCAPS_COLORCONTROLPRIMARY = $00000080;
  DDMISCCAPS_GAMMACONTROLOVERLAY = $00000100;
  DDMISCCAPS_GAMMACONTROLPRIMARY = $00000200;

  DDMISCCAPS_VALID               = DDMISCCAPS_READSCANLINE or
                                   DDMISCCAPS_READMONITORFREQ or
                                   DDMISCCAPS_READVBLANKSTATUS or
                                   DDMISCCAPS_FLIPINTERVAL or
                                   DDMISCCAPS_FLIPODDEVEN or
                                   DDMISCCAPS_FLIPVSYNCWITHVBI or
                                   DDMISCCAPS_COLORCONTROLOVERLAY or
                                   DDMISCCAPS_COLORCONTROLPRIMARY or
                                   DDMISCCAPS_GAMMACONTROLOVERLAY or
                                   DDMISCCAPS_GAMMACONTROLPRIMARY;

(*
 * DirectDraw method flags
 *)

(*
 * Flags for IDirectDraw::EnumSurfaces
 *)

  DDENUMSURFACES_ALL          = $00000001;
  DDENUMSURFACES_MATCH        = $00000002;
  DDENUMSURFACES_NOMATCH      = $00000004;
  DDENUMSURFACES_CANBECREATED = $00000008;
  DDENUMSURFACES_DOESEXIST    = $00000010;

  DDENUMSURFACES_VALID        = DDENUMSURFACES_ALL or
                                DDENUMSURFACES_MATCH or
                                DDENUMSURFACES_NOMATCH or
                                DDENUMSURFACES_CANBECREATED or
                                DDENUMSURFACES_DOESEXIST;

(*
 * Flags for IDirectDraw::SetCooperativeLevel
 *)

  DDSCL_NORMAL     = $00000000;
  DDSCL_FULLSCREEN = $00000001;

  DDSCL_VALID      = DDSCL_FULLSCREEN or
                     DDSCL_NORMAL;

(*
 * Flags for IDirectDraw::WaitForVerticalBlank
 *)

  DDWAITVB_BLOCKBEGIN = $00000001;
  DDWAITVB_BLOCKEND   = $00000004;

  DDWAITVB_VALID      = DDWAITVB_BLOCKBEGIN or
                        DDWAITVB_BLOCKEND;

(*
 * Flags for IDirectDrawSurface::GetColorKey and IDirectDrawSurface::SetColorKey
 *)

  DDCKEY_COLORSPACE  = $00000001;
  DDCKEY_DESTBLT     = $00000002;
  DDCKEY_DESTOVERLAY = $00000004;
  DDCKEY_SRCBLT      = $00000008;
  DDCKEY_SRCOVERLAY  = $00000010;

  DDCKEY_VALID       = DDCKEY_DESTBLT or
                       DDCKEY_DESTOVERLAY or
                       DDCKEY_SRCBLT or
                       DDCKEY_SRCOVERLAY;

(*
 * Flags for IDirectDrawSurface::Blt
 *)

  DDBLT_COLORFILL       = $00000400;
  DDBLT_KEYDEST         = $00002000;
  DDBLT_KEYDESTOVERRIDE = $00004000;
  DDBLT_KEYSRC          = $00008000;
  DDBLT_KEYSRCOVERRIDE  = $00010000;
  DDBLT_ROP             = $00020000;
  DDBLT_WAITNOTBUSY     = $01000000;
  DDBLT_WAITVSYNC       = $00000001;

  DDBLT_VALID           = DDBLT_COLORFILL or
                          DDBLT_KEYDEST or
                          DDBLT_KEYDESTOVERRIDE or
                          DDBLT_KEYSRC or
                          DDBLT_KEYSRCOVERRIDE or
                          DDBLT_ROP or
                          DDBLT_WAITNOTBUSY or
                          DDBLT_WAITVSYNC;

(*
 * Flags for IDirectDrawSurface::AlphaBlt
 *)

  DDABLT_NOBLEND      = $02000000;
  DDABLT_COLORFILL    = $00100000;
  DDABLT_ALPHADESTNEG = $00000004;
  DDABLT_ALPHASRCNEG  = $00000080;
  DDABLT_WAITNOTBUSY  = $01000000;
  DDABLT_WAITVSYNC    = $00000001;

  DDABLT_VALID        = DDABLT_NOBLEND or
                        DDABLT_COLORFILL or
                        DDABLT_ALPHADESTNEG or
                        DDABLT_ALPHASRCNEG or
                        DDABLT_WAITNOTBUSY or
                        DDABLT_WAITVSYNC;

(*
 * Flags for IDirectDrawSurface::Flip
 *)

  DDFLIP_EVEN                         = $00000002;
  DDFLIP_ODD                          = $00000004;
  DDFLIP_INTERVAL1                    = $01000000;
  DDFLIP_INTERVAL2                    = $02000000;
  DDFLIP_INTERVAL4                    = $04000000;
  DDFLIP_WAITNOTBUSY                  = $00000008;
  DDFLIP_WAITVSYNC                    = $00000001;

  DDFLIP_VALID                        = DDFLIP_EVEN or
                                        DDFLIP_ODD or
                                        DDFLIP_INTERVAL1 or
                                        DDFLIP_INTERVAL2 or
                                        DDFLIP_INTERVAL4 or
                                        DDFLIP_WAITNOTBUSY or
                                        DDFLIP_WAITVSYNC;

  DDFLIP_INTERVAL3                    = DDFLIP_INTERVAL1 or
                                        DDFLIP_INTERVAL2;

  DDFLIP_INTERVALMASK                 = DDFLIP_INTERVAL1 or
                                        DDFLIP_INTERVAL2 or
                                        DDFLIP_INTERVAL4;

(*
 * Flag values for IDirectDrawSurface::UpdateOverlay
 *)

  DDOVER_ALPHADEST          = $00000001;
  DDOVER_ALPHADESTNEG       = $00000002;
  DDOVER_ALPHASRC           = $00000004;
  DDOVER_ALPHASRCNEG        = $00000008;
  DDOVER_ALPHACONSTOVERRIDE = $00000010;
  DDOVER_HIDE               = $00000020;
  DDOVER_KEYDEST            = $00000040;
  DDOVER_KEYDESTOVERRIDE    = $00000080;
  DDOVER_KEYSRC             = $00000100;
  DDOVER_KEYSRCOVERRIDE     = $00000200;
  DDOVER_SHOW               = $00000400;
  DDOVER_MIRRORLEFTRIGHT    = $00001000;
  DDOVER_MIRRORUPDOWN       = $00002000;
  DDOVER_WAITNOTBUSY        = $00004000;
  DDOVER_WAITVSYNC          = $00008000;

  DDOVER_VALID              = DDOVER_ALPHADEST or
                              DDOVER_ALPHADESTNEG or
                              DDOVER_ALPHASRC or
                              DDOVER_ALPHASRCNEG or
                              DDOVER_ALPHACONSTOVERRIDE or
                              DDOVER_HIDE or
                              DDOVER_KEYDEST or
                              DDOVER_KEYDESTOVERRIDE or
                              DDOVER_KEYSRC or
                              DDOVER_KEYSRCOVERRIDE or
                              DDOVER_SHOW or
                              DDOVER_MIRRORLEFTRIGHT or
                              DDOVER_MIRRORUPDOWN or
                              DDOVER_WAITNOTBUSY or
                              DDOVER_WAITVSYNC;

(*
 * Flags for IDirectDrawSurface::Lock
 *)

  DDLOCK_READONLY            = $00000001;
  DDLOCK_WRITEONLY           = $00000002;
  DDLOCK_DISCARD             = $00000004;
  DDLOCK_WAITNOTBUSY         = $00000008;

  DDLOCK_VALID               = DDLOCK_READONLY or
                               DDLOCK_WRITEONLY or
                               DDLOCK_DISCARD or
                               DDLOCK_WAITNOTBUSY;

(*
 * Flags for IDirectDrawSurface::GetFlipStatus
 *)

  DDGFS_CANFLIP    = $00000001;
  DDGFS_ISFLIPDONE = $00000002;

  DDGFS_VALID      = DDGFS_CANFLIP or
                     DDGFS_ISFLIPDONE;


(*
 * Flags for IDirectDrawSurface::GetBltStatus
 *)

  DDGBS_CANBLT    = $00000001;
  DDGBS_ISBLTDONE = $00000002;

  DDGBS_VALID     = DDGBS_CANBLT or
                    DDGBS_ISBLTDONE;

(*
 * Flags for IDirectDrawSurface::EnumOverlayZOrders
 *)

  DDENUMOVERLAYZ_FRONTTOBACK = $00000001;

  DDENUMOVERLAYZ_VALID       = DDENUMOVERLAYZ_FRONTTOBACK;

(*
 * Flags for IDirectDrawSurface::UpdateOverlayZOrder
 *)

  DDOVERZ_SENDTOFRONT     = $00000000;
  DDOVERZ_SENDTOBACK      = $00000001;
  DDOVERZ_MOVEFORWARD     = $00000002;
  DDOVERZ_MOVEBACKWARD    = $00000003;
  DDOVERZ_INSERTINFRONTOF = $00000004;
  DDOVERZ_INSERTINBACKOF  = $00000005;

(*
 * DirectDraw Return Codes
 *)

(*
 * Enumeration function return values.
 *)

  DDENUMRET_CANCEL = HRESULT(0);
  DDENUMRET_OK     = HRESULT(1);

(*
 * DirectDraw error codes.
 *)

//#define _FACDD	0x876
//#define MAKE_DDHRESULT( code )	MAKE_HRESULT( 1, _FACDD, code )
  _FACDD = $876;

{$WARNING NOERROR is missing from windows unit}
{  DD_OK                               = NOERROR;}
  DD_OK                               = HResult(0);

  DDERR_CURRENTLYNOTAVAIL		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 40);
  DDERR_GENERIC				        = E_FAIL;
  DDERR_HEIGHTALIGN			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 90);
  DDERR_INCOMPATIBLEPRIMARY		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 95);
  DDERR_INVALIDCAPS			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 100);
  DDERR_INVALIDCLIPLIST			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 110);
  DDERR_INVALIDMODE			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 120);
  DDERR_INVALIDOBJECT			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 130);
  DDERR_INVALIDPARAMS			        = E_INVALIDARG;
  DDERR_INVALIDPIXELFORMAT		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 145);
  DDERR_INVALIDRECT			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 150);
  DDERR_LOCKEDSURFACES			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 160);
  DDERR_NOCLIPLIST			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 205);
  DDERR_NOALPHAHW				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 180);
  DDERR_NOCOLORCONVHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 210);
  DDERR_NOCOOPERATIVELEVELSET		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 212);
  DDERR_NOCOLORKEYHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 215);
  DDERR_NOFLIPHW				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 230);
  DDERR_NOTFOUND				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 255);
  DDERR_NOOVERLAYHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 260);
  DDERR_OVERLAPPINGRECTS			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 270);
  DDERR_NORASTEROPHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 280);
  DDERR_NOSTRETCHHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 310);
  DDERR_NOVSYNCHW				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 335);
  DDERR_NOZOVERLAYHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 350);
  DDERR_OUTOFCAPS				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 360);
  DDERR_OUTOFMEMORY			        = E_OUTOFMEMORY;
  DDERR_OUTOFVIDEOMEMORY			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 380);
  DDERR_PALETTEBUSY			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 387);
  DDERR_COLORKEYNOTSET			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 400);
  DDERR_SURFACEBUSY			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 430);
  DDERR_CANTLOCKSURFACE                         = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 435);
  DDERR_SURFACELOST			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 450);
  DDERR_TOOBIGHEIGHT			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 470);
  DDERR_TOOBIGSIZE			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 480);
  DDERR_TOOBIGWIDTH			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 490);
  DDERR_UNSUPPORTED			        = E_NOTIMPL;
  DDERR_UNSUPPORTEDFORMAT                       = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 536);
  DDERR_VERTICALBLANKINPROGRESS		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 537);
  DDERR_WASSTILLDRAWING			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 540);
  DDERR_DIRECTDRAWALREADYCREATED		= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 562);
  DDERR_PRIMARYSURFACEALREADYEXISTS	        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 564);
  DDERR_REGIONTOOSMALL			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 566);
  DDERR_CLIPPERISUSINGHWND		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 567);
  DDERR_NOCLIPPERATTACHED			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 568);
  DDERR_NOPALETTEATTACHED			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 572);
  DDERR_NOPALETTEHW			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 573);
  DDERR_NOBLTHW				        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 575);
  DDERR_OVERLAYNOTVISIBLE			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 577);
  DDERR_NOOVERLAYDEST			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 578);
  DDERR_INVALIDPOSITION			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 579);
  DDERR_NOTAOVERLAYSURFACE		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 580);
  DDERR_EXCLUSIVEMODEALREADYSET		        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 581);
  DDERR_NOTFLIPPABLE			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 582);
  DDERR_NOTLOCKED				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 584);
  DDERR_CANTCREATEDC			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 585);
  DDERR_NODC				        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 586);
  DDERR_WRONGMODE				= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 587);
  DDERR_IMPLICITLYCREATED			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 588);
  DDERR_NOTPALETTIZED			        = HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 589);
  DDERR_DCALREADYCREATED			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 620);
  DDERR_MOREDATA         			= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 690);
  DDERR_VIDEONOTACTIVE   		    	= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 695);
  DDERR_DEVICEDOESNTOWNSURFACE   		= HRESULT((DWORD(1) shl 31) or (_FACDD shl 16) or 699);

//#ifdef __cplusplus
//};
//#endif


//extern HRESULT WINAPI DirectDrawEnumerateEx(LPDDENUMCALLBACKEX lpCallback, LPVOID lpContext, DWORD dwFlags);

//extern HRESULT WINAPI DirectDrawCreate(LPGUID lpGUID, LPDIRECTDRAW *lplpDD, IUnknown *pUnkOuter);
//extern HRESULT WINAPI DirectDrawCreateClipper(DWORD dwFlags, LPDIRECTDRAWCLIPPER *lplpDDClipper, IUnknown *pUnkOuter);

function DirectDrawEnumerateEx(lpCallback: LPDDENUMCALLBACKEX; lpContext: LPVOID; dwFlags: DWORD): HRESULT; cdecl; external 'ddraw.dll';
function DirectDrawCreate(lpGUID: PGUID; out lplpDD: IDirectDraw; pUnkOuter: IUnknown): HRESULT; cdecl; external 'ddraw.dll';
function DirectDrawCreateClipper(dwFlags: DWORD; out lplpDDClipper: IDirectDrawClipper; pUnkOuter: IUnknown): HRESULT; cdecl; external 'ddraw.dll';

//#ifndef MAKEFOURCC
//    #define MAKEFOURCC(ch0, ch1, ch2, ch3)                                \
//                ((DWORD)(BYTE)(ch0) | ((DWORD)(BYTE)(ch1) << 8) |         \
//                ((DWORD)(BYTE)(ch2) << 16) | ((DWORD)(BYTE)(ch3) << 24 ))
//#endif //defined(MAKEFOURCC)

function MAKEFOURCC(ch0, ch1, ch2, ch3: BYTE): DWORD; inline;

implementation

function MAKEFOURCC(ch0, ch1, ch2, ch3: BYTE): DWORD; inline;
begin
  Result := DWORD(ch0) or
           (DWORD(ch1) shl 8) or
           (DWORD(ch2) shl 16) or
	   (DWORD(ch3) shl 24);
end;

end.
