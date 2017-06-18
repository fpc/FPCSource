(****************************************************************************
 *
 *  Copyright (C) 1996-2002 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dinput.h
 *  Content:    DirectInput include file
 *
 ****************************************************************************)

unit p_dinput;

{$MODE objfpc}{$H+}
{$MACRO on}
{$PACKRECORDS c}

{$MODESWITCH NESTEDCOMMENTS-}

interface

uses
  windows;

//#ifndef __DINPUT_INCLUDED__
//#define __DINPUT_INCLUDED__
//
//#include <winapifamily.h>
//
//#ifndef DIJ_RINGZERO
//
//#ifdef _WIN32
//#define COM_NO_WINDOWS_H
//#include <objbase.h>
//#endif
//
//#endif /* DIJ_RINGZERO */
//
//#ifdef __cplusplus
//extern "C" {
//#endif
//
//
//#pragma region Desktop Family
//#if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)





(*
 *  To build applications for older versions of DirectInput
 *
 *  #define DIRECTINPUT_VERSION [ 0x0300 | 0x0500 | 0x0700 ]
 *
 *  before #include <dinput.h>.  By default, #include <dinput.h>
 *  will produce a DirectX 8-compatible header file.
 *
 *)

{$DEFINE DIRECTINPUT_HEADER_VERSION:=$0800}
{$IFNDEF DIRECTINPUT_VERSION}
{$DEFINE DIRECTINPUT_VERSION:=DIRECTINPUT_HEADER_VERSION}
//#pragma message(__FILE__ ": DIRECTINPUT_VERSION undefined. Defaulting to version 0x0800")
{$ENDIF}

//#ifndef DIJ_RINGZERO

(****************************************************************************
 *
 *      Class IDs
 *
 ****************************************************************************)

const
  CLSID_DirectInput       : TGuid = '{25E609E0-B259-11CF-BFC7-444553540000}';
  CLSID_DirectInputDevice : TGuid = '{25E609E1-B259-11CF-BFC7-444553540000}';

  CLSID_DirectInput8      : TGuid = '{25E609E4-B259-11CF-BFC7-444553540000}';
  CLSID_DirectInputDevice8: TGuid = '{25E609E5-B259-11CF-BFC7-444553540000}';

(****************************************************************************
 *
 *      Interfaces
 *
 ****************************************************************************)

  IID_IDirectInputA       : TIID = '{89521360-AA8A-11CF-BFC7-444553540000}';
  IID_IDirectInputW       : TIID = '{89521361-AA8A-11CF-BFC7-444553540000}';
  IID_IDirectInput2A      : TIID = '{5944E662-AA8A-11CF-BFC7-444553540000}';
  IID_IDirectInput2W      : TIID = '{5944E663-AA8A-11CF-BFC7-444553540000}';
  IID_IDirectInput7A      : TIID = '{9A4CB684-236D-11D3-8E9D-00C04F6844AE}';
  IID_IDirectInput7W      : TIID = '{9A4CB685-236D-11D3-8E9D-00C04F6844AE}';
  IID_IDirectInput8A      : TIID = '{BF798030-483A-4DA2-AA99-5D64ED369700}';
  IID_IDirectInput8W      : TIID = '{BF798031-483A-4DA2-AA99-5D64ED369700}';
  IID_IDirectInputDeviceA : TIID = '{5944E680-C92E-11CF-BFC7-444553540000}';
  IID_IDirectInputDeviceW : TIID = '{5944E681-C92E-11CF-BFC7-444553540000}';
  IID_IDirectInputDevice2A: TIID = '{5944E682-C92E-11CF-BFC7-444553540000}';
  IID_IDirectInputDevice2W: TIID = '{5944E683-C92E-11CF-BFC7-444553540000}';
  IID_IDirectInputDevice7A: TIID = '{57D7C6BC-2356-11D3-8E9D-00C04F6844AE}';
  IID_IDirectInputDevice7W: TIID = '{57D7C6BD-2356-11D3-8E9D-00C04F6844AE}';
  IID_IDirectInputDevice8A: TIID = '{54D41080-DC15-4833-A41B-748F73A38179}';
  IID_IDirectInputDevice8W: TIID = '{54D41081-DC15-4833-A41B-748F73A38179}';
  IID_IDirectInputEffect  : TIID = '{E7E1F7C0-88D2-11D0-9AD0-00A0C9A06E35}';

(****************************************************************************
 *
 *      Predefined object types
 *
 ****************************************************************************)

  GUID_XAxis  : TGuid = '{A36D02E0-C9F3-11CF-BFC7-444553540000}';
  GUID_YAxis  : TGuid = '{A36D02E1-C9F3-11CF-BFC7-444553540000}';
  GUID_ZAxis  : TGuid = '{A36D02E2-C9F3-11CF-BFC7-444553540000}';
  GUID_RxAxis : TGuid = '{A36D02F4-C9F3-11CF-BFC7-444553540000}';
  GUID_RyAxis : TGuid = '{A36D02F5-C9F3-11CF-BFC7-444553540000}';
  GUID_RzAxis : TGuid = '{A36D02E3-C9F3-11CF-BFC7-444553540000}';
  GUID_Slider : TGuid = '{A36D02E4-C9F3-11CF-BFC7-444553540000}';

  GUID_Button : TGuid = '{A36D02F0-C9F3-11CF-BFC7-444553540000}';
  GUID_Key    : TGuid = '{55728220-D33C-11CF-BFC7-444553540000}';

  GUID_POV    : TGuid = '{A36D02F2-C9F3-11CF-BFC7-444553540000}';

  GUID_Unknown: TGuid = '{A36D02F3-C9F3-11CF-BFC7-444553540000}';

(****************************************************************************
 *
 *      Predefined product GUIDs
 *
 ****************************************************************************)

  GUID_SysMouse      : TGuid = '{6F1D2B60-D5A0-11CF-BFC7-444553540000}';
  GUID_SysKeyboard   : TGuid = '{6F1D2B61-D5A0-11CF-BFC7-444553540000}';
  GUID_Joystick      : TGuid = '{6F1D2B70-D5A0-11CF-BFC7-444553540000}';
  GUID_SysMouseEm    : TGuid = '{6F1D2B80-D5A0-11CF-BFC7-444553540000}';
  GUID_SysMouseEm2   : TGuid = '{6F1D2B81-D5A0-11CF-BFC7-444553540000}';
  GUID_SysKeyboardEm : TGuid = '{6F1D2B82-D5A0-11CF-BFC7-444553540000}';
  GUID_SysKeyboardEm2: TGuid = '{6F1D2B83-D5A0-11CF-BFC7-444553540000}';

(****************************************************************************
 *
 *      Predefined force feedback effects
 *
 ****************************************************************************)

  GUID_ConstantForce: TGuid = '{13541C20-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_RampForce    : TGuid = '{13541C21-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Square       : TGuid = '{13541C22-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Sine         : TGuid = '{13541C23-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Triangle     : TGuid = '{13541C24-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_SawtoothUp   : TGuid = '{13541C25-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_SawtoothDown : TGuid = '{13541C26-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Spring       : TGuid = '{13541C27-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Damper       : TGuid = '{13541C28-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Inertia      : TGuid = '{13541C29-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_Friction     : TGuid = '{13541C2A-8E33-11D0-9AD0-00A0C9A06E35}';
  GUID_CustomForce  : TGuid = '{13541C2B-8E33-11D0-9AD0-00A0C9A06E35}';

//#endif /* DIJ_RINGZERO */

(****************************************************************************
 *
 *      Interfaces and Structures...
 *
 ****************************************************************************)

{$IF DIRECTINPUT_VERSION >= $0500}

(****************************************************************************
 *
 *      IDirectInputEffect
 *
 ****************************************************************************)

  DIEFT_ALL                = $00000000;

  DIEFT_CONSTANTFORCE      = $00000001;
  DIEFT_RAMPFORCE          = $00000002;
  DIEFT_PERIODIC           = $00000003;
  DIEFT_CONDITION          = $00000004;
  DIEFT_CUSTOMFORCE        = $00000005;
  DIEFT_HARDWARE           = $000000FF;
  DIEFT_FFATTACK           = $00000200;
  DIEFT_FFFADE             = $00000400;
  DIEFT_SATURATION         = $00000800;
  DIEFT_POSNEGCOEFFICIENTS = $00001000;
  DIEFT_POSNEGSATURATION   = $00002000;
  DIEFT_DEADBAND           = $00004000;
  DIEFT_STARTDELAY         = $00008000;
//#define DIEFT_GETTYPE(n)            LOBYTE(n)
type
  DIEFT_GETTYPE            = byte;

const
  DI_DEGREES               = 100;
  DI_FFNOMINALMAX          = 10000;
  DI_SECONDS               = 1000000;

type
  LPCDICONSTANTFORCE = ^TDICONSTANTFORCE;
  PCDICONSTANTFORCE = ^TDICONSTANTFORCE;
  LPDICONSTANTFORCE = ^TDICONSTANTFORCE;
  PDICONSTANTFORCE = ^TDICONSTANTFORCE;
  TDICONSTANTFORCE = record
    lMagnitude: LONG;
  end;

  LPCDIRAMPFORCE = ^TDIRAMPFORCE;
  PCDIRAMPFORCE = ^TDIRAMPFORCE;
  LPDIRAMPFORCE = ^TDIRAMPFORCE;
  PDIRAMPFORCE = ^TDIRAMPFORCE;
  TDIRAMPFORCE = record
    lStart: LONG;
    lEnd  : LONG;
  end;

  LPCDIPERIODIC = ^TDIPERIODIC;
  PCDIPERIODIC = ^TDIPERIODIC;
  LPDIPERIODIC = ^TDIPERIODIC;
  PDIPERIODIC = ^TDIPERIODIC;
  TDIPERIODIC = record
    dwMagnitude: DWORD;
    lOffset: LONG;
    dwPhase: DWORD;
    dwPeriod: DWORD;
  end;

  LPCDICONDITION = ^TDICONDITION;
  PCDICONDITION = ^TDICONDITION;
  LPDICONDITION = ^TDICONDITION;
  PDICONDITION = ^TDICONDITION;
  TDICONDITION = record
    lOffset: LONG;
    lPositiveCoefficient: LONG;
    lNegativeCoefficient: LONG;
    dwPositiveSaturation: DWORD;
    dwNegativeSaturation: DWORD;
    lDeadBand: LONG;
  end;

  LPCDICUSTOMFORCE = ^TDICUSTOMFORCE;
  PCDICUSTOMFORCE = ^TDICUSTOMFORCE;
  LPDICUSTOMFORCE = ^TDICUSTOMFORCE;
  PDICUSTOMFORCE = ^TDICUSTOMFORCE;
  TDICUSTOMFORCE = record
    cChannels: DWORD;
    dwSamplePeriod: DWORD;
    cSamples: DWORD;
    rglForceData: LPLONG
  end;


  LPCDIENVELOPE = ^TDIENVELOPE;
  PCDIENVELOPE = ^TDIENVELOPE;
  LPDIENVELOPE = ^TDIENVELOPE;
  PDIENVELOPE = ^TDIENVELOPE;
  TDIENVELOPE = record
    dwSize: DWORD;            { sizeof(DIENVELOPE)   }
    dwAttackLevel: DWORD;
    dwAttackTime: DWORD;      { Microseconds         }
    dwFadeLevel: DWORD;
    dwFadeTime: DWORD;        { Microseconds         }
  end;


{ This structure is defined for DirectX 5.0 compatibility }
  LPCDIEFFECT_DX5 = ^TDIEFFECT_DX5;
  PCDIEFFECT_DX5 = ^TDIEFFECT_DX5;
  LPDIEFFECT_DX5 = ^TDIEFFECT_DX5;
  PDIEFFECT_DX5 = ^TDIEFFECT_DX5;
  TDIEFFECT_DX5 = record
    dwSize: DWORD;                   { sizeof(DIEFFECT_DX5) }
    dwFlags: DWORD;                  { DIEFF_*              }
    dwDuration: DWORD;               { Microseconds         }
    dwSamplePeriod: DWORD;           { Microseconds         }
    dwGain: DWORD;
    dwTriggerButton: DWORD;          { or DIEB_NOTRIGGER    }
    dwTriggerRepeatInterval: DWORD;  { Microseconds         }
    cAxes: DWORD;                    { Number of axes       }
    rgdwAxes: LPDWORD;               { Array of axes        }
    rglDirection: LPLONG;            { Array of directions  }
    lpEnvelope: LPDIENVELOPE;        { Optional             }
    cbTypeSpecificParams: DWORD;     { Size of params       }
    lpvTypeSpecificParams: LPVOID;   { Pointer to params    }
  end;

  LPCDIEFFECT = ^TDIEFFECT;
  PCDIEFFECT = ^TDIEFFECT;
  LPDIEFFECT = ^TDIEFFECT;
  PDIEFFECT = ^TDIEFFECT;
  TDIEFFECT = record
    dwSize: DWORD;                   { sizeof(DIEFFECT)     }
    dwFlags: DWORD;                  { DIEFF_*              }
    dwDuration: DWORD;               { Microseconds         }
    dwSamplePeriod: DWORD;           { Microseconds         }
    dwGain: DWORD;
    dwTriggerButton: DWORD;          { or DIEB_NOTRIGGER    }
    dwTriggerRepeatInterval: DWORD;  { Microseconds         }
    cAxes: DWORD;                    { Number of axes       }
    rgdwAxes: LPDWORD;               { Array of axes        }
    rglDirection: LPLONG;            { Array of directions  }
    lpEnvelope: LPDIENVELOPE;        { Optional             }
    cbTypeSpecificParams: DWORD;     { Size of params       }
    lpvTypeSpecificParams: LPVOID;   { Pointer to params    }
{$IF DIRECTINPUT_VERSION >= $0600}
    dwStartDelay: DWORD;             { Microseconds         }
{$ENDIF} { DIRECTINPUT_VERSION >= $0600 }
  end;
  LPDIEFFECT_DX6 = LPDIEFFECT;
  TDIEFFECT_DX6 = TDIEFFECT;


{$IF DIRECTINPUT_VERSION >= $0700}
//#ifndef DIJ_RINGZERO
  LPCDIFILEEFFECT= ^TDIFILEEFFECT;
  PCDIFILEEFFECT= ^TDIFILEEFFECT;
  LPDIFILEEFFECT = ^TDIFILEEFFECT;
  PDIFILEEFFECT = ^TDIFILEEFFECT;
  TDIFILEEFFECT = record
    dwSize: DWORD;
    GuidEffect: GUID;
    lpDiEffect: LPCDIEFFECT;
    szFriendlyName: array [0..MAX_PATH-1] of CHAR;
  end;
//typedef BOOL (FAR PASCAL * LPDIENUMEFFECTSINFILECALLBACK)(LPCDIFILEEFFECT , LPVOID);
  LPDIENUMEFFECTSINFILECALLBACK = function(lpDiFileEf: LPCDIFILEEFFECT; pvRef: LPVOID): BOOL; stdcall;
//#endif /* DIJ_RINGZERO */
{$ENDIF} { DIRECTINPUT_VERSION >= $0700 }

const
  DIEFF_OBJECTIDS             = $00000001;
  DIEFF_OBJECTOFFSETS         = $00000002;
  DIEFF_CARTESIAN             = $00000010;
  DIEFF_POLAR                 = $00000020;
  DIEFF_SPHERICAL             = $00000040;

  DIEP_DURATION               = $00000001;
  DIEP_SAMPLEPERIOD           = $00000002;
  DIEP_GAIN                   = $00000004;
  DIEP_TRIGGERBUTTON          = $00000008;
  DIEP_TRIGGERREPEATINTERVAL  = $00000010;
  DIEP_AXES                   = $00000020;
  DIEP_DIRECTION              = $00000040;
  DIEP_ENVELOPE               = $00000080;
  DIEP_TYPESPECIFICPARAMS     = $00000100;
{$IF DIRECTINPUT_VERSION >= 0x0600}
  DIEP_STARTDELAY             = $00000200;
  DIEP_ALLPARAMS_DX5          = $000001FF;
  DIEP_ALLPARAMS              = $000003FF;
{$ELSE} { DIRECTINPUT_VERSION < 0x0600 }
  DIEP_ALLPARAMS              = $000001FF;
{$ENDIF} { DIRECTINPUT_VERSION < 0x0600 }
  DIEP_START                  = $20000000;
  DIEP_NORESTART              = $40000000;
  DIEP_NODOWNLOAD             = $80000000;
  DIEB_NOTRIGGER              = $FFFFFFFF;

  DIES_SOLO                   = $00000001;
  DIES_NODOWNLOAD             = $80000000;

  DIEGES_PLAYING              = $00000001;
  DIEGES_EMULATED             = $00000002;

type
  LPDIEFFESCAPE = ^TDIEFFESCAPE;
  PDIEFFESCAPE = ^TDIEFFESCAPE;
  TDIEFFESCAPE = record
    dwSize: DWORD;
    dwCommand: DWORD;
    lpvInBuffer: LPVOID;
    cbInBuffer: DWORD;
    lpvOutBuffer: LPVOID;
    cbOutBuffer: DWORD;
  end;

//#ifndef DIJ_RINGZERO

//#undef INTERFACE
//#define INTERFACE IDirectInputEffect

//DECLARE_INTERFACE_(IDirectInputEffect, IUnknown)
type
  IDirectInputEffect = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputEffect methods ***)
    function Initialize(hinst: HINST; dwVersion: DWORD; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function GetEffectGuid(pguid: LPGUID): HRESULT; stdcall;
    function GetParameters(peff: LPDIEFFECT; dwFlags: DWORD): HRESULT; stdcall;
    function SetParameters(peff: LPCDIEFFECT; dwFlags: DWORD): HRESULT; stdcall;
    function Start(dwIterations, dwFlags: DWORD): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function GetEffectStatus(pdwFlags: LPDWORD): HRESULT; stdcall;
    function Download: HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
    function Escape(pesc: LPDIEFFESCAPE): HRESULT; stdcall;
  end;

//typedef struct IDirectInputEffect *LPDIRECTINPUTEFFECT;
  LPDIRECTINPUTEFFECT = IDirectInputEffect;

{#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectInputEffect_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectInputEffect_AddRef(p) (p)->lpVtbl->AddRef(p)
#define IDirectInputEffect_Release(p) (p)->lpVtbl->Release(p)
#define IDirectInputEffect_Initialize(p,a,b,c) (p)->lpVtbl->Initialize(p,a,b,c)
#define IDirectInputEffect_GetEffectGuid(p,a) (p)->lpVtbl->GetEffectGuid(p,a)
#define IDirectInputEffect_GetParameters(p,a,b) (p)->lpVtbl->GetParameters(p,a,b)
#define IDirectInputEffect_SetParameters(p,a,b) (p)->lpVtbl->SetParameters(p,a,b)
#define IDirectInputEffect_Start(p,a,b) (p)->lpVtbl->Start(p,a,b)
#define IDirectInputEffect_Stop(p) (p)->lpVtbl->Stop(p)
#define IDirectInputEffect_GetEffectStatus(p,a) (p)->lpVtbl->GetEffectStatus(p,a)
#define IDirectInputEffect_Download(p) (p)->lpVtbl->Download(p)
#define IDirectInputEffect_Unload(p) (p)->lpVtbl->Unload(p)
#define IDirectInputEffect_Escape(p,a) (p)->lpVtbl->Escape(p,a)
#else
#define IDirectInputEffect_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
#define IDirectInputEffect_AddRef(p) (p)->AddRef()
#define IDirectInputEffect_Release(p) (p)->Release()
#define IDirectInputEffect_Initialize(p,a,b,c) (p)->Initialize(a,b,c)
#define IDirectInputEffect_GetEffectGuid(p,a) (p)->GetEffectGuid(a)
#define IDirectInputEffect_GetParameters(p,a,b) (p)->GetParameters(a,b)
#define IDirectInputEffect_SetParameters(p,a,b) (p)->SetParameters(a,b)
#define IDirectInputEffect_Start(p,a,b) (p)->Start(a,b)
#define IDirectInputEffect_Stop(p) (p)->Stop()
#define IDirectInputEffect_GetEffectStatus(p,a) (p)->GetEffectStatus(a)
#define IDirectInputEffect_Download(p) (p)->Download()
#define IDirectInputEffect_Unload(p) (p)->Unload()
#define IDirectInputEffect_Escape(p,a) (p)->Escape(a)
#endif}

//#endif /* DIJ_RINGZERO */

{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }

(****************************************************************************
 *
 *      IDirectInputDevice
 *
 ****************************************************************************)

{$IF DIRECTINPUT_VERSION <= $700}
const
  DIDEVTYPE_DEVICE   = 1;
  DIDEVTYPE_MOUSE    = 2;
  DIDEVTYPE_KEYBOARD = 3;
  DIDEVTYPE_JOYSTICK = 4;

{$ELSE}
const
  DI8DEVCLASS_ALL          = 0;
  DI8DEVCLASS_DEVICE       = 1;
  DI8DEVCLASS_POINTER      = 2;
  DI8DEVCLASS_KEYBOARD     = 3;
  DI8DEVCLASS_GAMECTRL     = 4;

  DI8DEVTYPE_DEVICE        = $11;
  DI8DEVTYPE_MOUSE         = $12;
  DI8DEVTYPE_KEYBOARD      = $13;
  DI8DEVTYPE_JOYSTICK      = $14;
  DI8DEVTYPE_GAMEPAD       = $15;
  DI8DEVTYPE_DRIVING       = $16;
  DI8DEVTYPE_FLIGHT        = $17;
  DI8DEVTYPE_1STPERSON     = $18;
  DI8DEVTYPE_DEVICECTRL    = $19;
  DI8DEVTYPE_SCREENPOINTER = $1A;
  DI8DEVTYPE_REMOTE        = $1B;
  DI8DEVTYPE_SUPPLEMENTAL  = $1C;
{$ENDIF} { DIRECTINPUT_VERSION <= 0x700 }

const
  DIDEVTYPE_HID            = $00010000;

{$IF DIRECTINPUT_VERSION <= $700}
const
  DIDEVTYPEMOUSE_UNKNOWN        = 1;
  DIDEVTYPEMOUSE_TRADITIONAL    = 2;
  DIDEVTYPEMOUSE_FINGERSTICK    = 3;
  DIDEVTYPEMOUSE_TOUCHPAD       = 4;
  DIDEVTYPEMOUSE_TRACKBALL      = 5;

  DIDEVTYPEKEYBOARD_UNKNOWN     =  0;
  DIDEVTYPEKEYBOARD_PCXT        =  1;
  DIDEVTYPEKEYBOARD_OLIVETTI    =  2;
  DIDEVTYPEKEYBOARD_PCAT        =  3;
  DIDEVTYPEKEYBOARD_PCENH       =  4;
  DIDEVTYPEKEYBOARD_NOKIA1050   =  5;
  DIDEVTYPEKEYBOARD_NOKIA9140   =  6;
  DIDEVTYPEKEYBOARD_NEC98       =  7;
  DIDEVTYPEKEYBOARD_NEC98LAPTOP =  8;
  DIDEVTYPEKEYBOARD_NEC98106    =  9;
  DIDEVTYPEKEYBOARD_JAPAN106    = 10;
  DIDEVTYPEKEYBOARD_JAPANAX     = 11;
  DIDEVTYPEKEYBOARD_J3100       = 12;

  DIDEVTYPEJOYSTICK_UNKNOWN     = 1;
  DIDEVTYPEJOYSTICK_TRADITIONAL = 2;
  DIDEVTYPEJOYSTICK_FLIGHTSTICK = 3;
  DIDEVTYPEJOYSTICK_GAMEPAD     = 4;
  DIDEVTYPEJOYSTICK_RUDDER      = 5;
  DIDEVTYPEJOYSTICK_WHEEL       = 6;
  DIDEVTYPEJOYSTICK_HEADTRACKER = 7;

{$ELSE}
const
  DI8DEVTYPEMOUSE_UNKNOWN                       = 1;
  DI8DEVTYPEMOUSE_TRADITIONAL                   = 2;
  DI8DEVTYPEMOUSE_FINGERSTICK                   = 3;
  DI8DEVTYPEMOUSE_TOUCHPAD                      = 4;
  DI8DEVTYPEMOUSE_TRACKBALL                     = 5;
  DI8DEVTYPEMOUSE_ABSOLUTE                      = 6;

  DI8DEVTYPEKEYBOARD_UNKNOWN                    =  0;
  DI8DEVTYPEKEYBOARD_PCXT                       =  1;
  DI8DEVTYPEKEYBOARD_OLIVETTI                   =  2;
  DI8DEVTYPEKEYBOARD_PCAT                       =  3;
  DI8DEVTYPEKEYBOARD_PCENH                      =  4;
  DI8DEVTYPEKEYBOARD_NOKIA1050                  =  5;
  DI8DEVTYPEKEYBOARD_NOKIA9140                  =  6;
  DI8DEVTYPEKEYBOARD_NEC98                      =  7;
  DI8DEVTYPEKEYBOARD_NEC98LAPTOP                =  8;
  DI8DEVTYPEKEYBOARD_NEC98106                   =  9;
  DI8DEVTYPEKEYBOARD_JAPAN106                   = 10;
  DI8DEVTYPEKEYBOARD_JAPANAX                    = 11;
  DI8DEVTYPEKEYBOARD_J3100                      = 12;

  DI8DEVTYPE_LIMITEDGAMESUBTYPE                 = 1;

  DI8DEVTYPEJOYSTICK_LIMITED                    = DI8DEVTYPE_LIMITEDGAMESUBTYPE;
  DI8DEVTYPEJOYSTICK_STANDARD                   = 2;

  DI8DEVTYPEGAMEPAD_LIMITED                     = DI8DEVTYPE_LIMITEDGAMESUBTYPE;
  DI8DEVTYPEGAMEPAD_STANDARD                    = 2;
  DI8DEVTYPEGAMEPAD_TILT                        = 3;

  DI8DEVTYPEDRIVING_LIMITED                     = DI8DEVTYPE_LIMITEDGAMESUBTYPE;
  DI8DEVTYPEDRIVING_COMBINEDPEDALS              = 2;
  DI8DEVTYPEDRIVING_DUALPEDALS                  = 3;
  DI8DEVTYPEDRIVING_THREEPEDALS                 = 4;
  DI8DEVTYPEDRIVING_HANDHELD                    = 5;

  DI8DEVTYPEFLIGHT_LIMITED                      = DI8DEVTYPE_LIMITEDGAMESUBTYPE;
  DI8DEVTYPEFLIGHT_STICK                        = 2;
  DI8DEVTYPEFLIGHT_YOKE                         = 3;
  DI8DEVTYPEFLIGHT_RC                           = 4;

  DI8DEVTYPE1STPERSON_LIMITED                   = DI8DEVTYPE_LIMITEDGAMESUBTYPE;
  DI8DEVTYPE1STPERSON_UNKNOWN                   = 2;
  DI8DEVTYPE1STPERSON_SIXDOF                    = 3;
  DI8DEVTYPE1STPERSON_SHOOTER                   = 4;

  DI8DEVTYPESCREENPTR_UNKNOWN                   = 2;
  DI8DEVTYPESCREENPTR_LIGHTGUN                  = 3;
  DI8DEVTYPESCREENPTR_LIGHTPEN                  = 4;
  DI8DEVTYPESCREENPTR_TOUCH                     = 5;

  DI8DEVTYPEREMOTE_UNKNOWN                      = 2;

  DI8DEVTYPEDEVICECTRL_UNKNOWN                  = 2;
  DI8DEVTYPEDEVICECTRL_COMMSSELECTION           = 3;
  DI8DEVTYPEDEVICECTRL_COMMSSELECTION_HARDWIRED = 4;

  DI8DEVTYPESUPPLEMENTAL_UNKNOWN                =  2;
  DI8DEVTYPESUPPLEMENTAL_2NDHANDCONTROLLER      =  3;
  DI8DEVTYPESUPPLEMENTAL_HEADTRACKER            =  4;
  DI8DEVTYPESUPPLEMENTAL_HANDTRACKER            =  5;
  DI8DEVTYPESUPPLEMENTAL_SHIFTSTICKGATE         =  6;
  DI8DEVTYPESUPPLEMENTAL_SHIFTER                =  7;
  DI8DEVTYPESUPPLEMENTAL_THROTTLE               =  8;
  DI8DEVTYPESUPPLEMENTAL_SPLITTHROTTLE          =  9;
  DI8DEVTYPESUPPLEMENTAL_COMBINEDPEDALS         = 10;
  DI8DEVTYPESUPPLEMENTAL_DUALPEDALS             = 11;
  DI8DEVTYPESUPPLEMENTAL_THREEPEDALS            = 12;
  DI8DEVTYPESUPPLEMENTAL_RUDDERPEDALS           = 13;
{$ENDIF} { DIRECTINPUT_VERSION <= $700 }

//#define GET_DIDEVICE_TYPE(dwDevType)    LOBYTE(dwDevType)
type
  GET_DIDEVICE_TYPE = byte;
//#define GET_DIDEVICE_SUBTYPE(dwDevType) HIBYTE(dwDevType)

{$IF DIRECTINPUT_VERSION >= $0500}
{ This structure is defined for DirectX 3.0 compatibility }
type
  LPDIDEVCAPS_DX3 = ^TDIDEVCAPS_DX3;
  PDIDEVCAPS_DX3 = ^TDIDEVCAPS_DX3;
  TDIDEVCAPS_DX3 = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDevType: DWORD;
    dwAxes: DWORD;
    dwButtons: DWORD;
    dwPOVs: DWORD;
  end;
{$ENDIF} { DIRECTINPUT_VERSION >= 0x0500 }

  LPDIDEVCAPS = ^TDIDEVCAPS;
  PDIDEVCAPS = ^TDIDEVCAPS;
  TDIDEVCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDevType: DWORD;
    dwAxes: DWORD;
    dwButtons: DWORD;
    dwPOVs: DWORD;
{$IF DIRECTINPUT_VERSION >= $0500}
    dwFFSamplePeriod: DWORD;
    dwFFMinTimeResolution: DWORD;
    dwFirmwareRevision: DWORD;
    dwHardwareRevision: DWORD;
    dwFFDriverVersion: DWORD;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  end;

const
  DIDC_ATTACHED           = $00000001;
  DIDC_POLLEDDEVICE       = $00000002;
  DIDC_EMULATED           = $00000004;
  DIDC_POLLEDDATAFORMAT   = $00000008;
{$IF DIRECTINPUT_VERSION >= $0500}
  DIDC_FORCEFEEDBACK      = $00000100;
  DIDC_FFATTACK           = $00000200;
  DIDC_FFFADE             = $00000400;
  DIDC_SATURATION         = $00000800;
  DIDC_POSNEGCOEFFICIENTS = $00001000;
  DIDC_POSNEGSATURATION   = $00002000;
  DIDC_DEADBAND           = $00004000;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  DIDC_STARTDELAY         = $00008000;
{$IF DIRECTINPUT_VERSION >= $050a}
  DIDC_ALIAS              = $00010000;
  DIDC_PHANTOM            = $00020000;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }
{$IF DIRECTINPUT_VERSION >= $0800}
  DIDC_HIDDEN             = $00040000;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

  DIDFT_ALL             = $00000000;

  DIDFT_RELAXIS         = $00000001;
  DIDFT_ABSAXIS         = $00000002;
  DIDFT_AXIS            = $00000003;

  DIDFT_PSHBUTTON       = $00000004;
  DIDFT_TGLBUTTON       = $00000008;
  DIDFT_BUTTON          = $0000000C;

  DIDFT_POV             = $00000010;
  DIDFT_COLLECTION      = $00000040;
  DIDFT_NODATA          = $00000080;

  DIDFT_ANYINSTANCE     = $00FFFF00;
  DIDFT_INSTANCEMASK    = DIDFT_ANYINSTANCE;
  //DIDFT_MAKEINSTANCE(n) ((WORD)(n) << 8)
  //DIDFT_GETTYPE(n)     LOBYTE(n)
type
  DIDFT_GETTYPE = byte;
  //DIDFT_GETINSTANCE(n) LOWORD((n) >> 8)
const
  DIDFT_FFACTUATOR      = $01000000;
  DIDFT_FFEFFECTTRIGGER = $02000000;
{$IF DIRECTINPUT_VERSION >= $050a}
  DIDFT_OUTPUT          = $10000000;
  DIDFT_VENDORDEFINED   = $04000000;
  DIDFT_ALIAS           = $08000000;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

//  DIDFT_ENUMCOLLECTION(n) ((WORD)(n) << 8)
  DIDFT_NOCOLLECTION      = $00FFFF00;

//#ifndef DIJ_RINGZERO

type
  LPCDIOBJECTDATAFORMAT = ^TDIOBJECTDATAFORMAT;
  PCDIOBJECTDATAFORMAT = ^TDIOBJECTDATAFORMAT;
  LPDIOBJECTDATAFORMAT = ^TDIOBJECTDATAFORMAT;
  PDIOBJECTDATAFORMAT = ^TDIOBJECTDATAFORMAT;
  TDIOBJECTDATAFORMAT = record
    pguid: PGuid; {const GUID *}
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
  end;

  LPCDIDATAFORMAT = ^TDIDATAFORMAT;
  PCDIDATAFORMAT = ^TDIDATAFORMAT;
  LPDIDATAFORMAT = ^TDIDATAFORMAT;
  PDIDATAFORMAT = ^TDIDATAFORMAT;
  TDIDATAFORMAT = record
    dwSize: DWORD;
    dwObjSize: DWORD;
    dwFlags: DWORD;
    dwDataSize: DWORD;
    dwNumObjs: DWORD;
    rgodf: LPDIOBJECTDATAFORMAT;
  end;

const
  DIDF_ABSAXIS = $00000001;
  DIDF_RELAXIS = $00000002;

//#ifdef __cplusplus
//extern "C" {
//#endif
//extern const DIDATAFORMAT c_dfDIMouse;
var
  c_dfDIMouse: TDIDATAFORMAT; cvar; external;

{$IF DIRECTINPUT_VERSION >= $0700}
//extern const DIDATAFORMAT c_dfDIMouse2;
  c_dfDIMouse2: TDIDATAFORMAT; cvar; external;
{$ENDIF} { DIRECTINPUT_VERSION >= $0700 }

//extern const DIDATAFORMAT c_dfDIKeyboard;
  c_dfDIKeyboard: TDIDATAFORMAT; cvar; external;

{$IF DIRECTINPUT_VERSION >= $0500}
//extern const DIDATAFORMAT c_dfDIJoystick;
  c_dfDIJoystick: TDIDATAFORMAT; cvar; external;
//extern const DIDATAFORMAT c_dfDIJoystick2;
  c_dfDIJoystick2: TDIDATAFORMAT; cvar; external;
{$ENDIF} { DIRECTINPUT_VERSION >= 0x0500 }

//#ifdef __cplusplus
//};
//#endif


{$IF DIRECTINPUT_VERSION > $0700}

//#pragma warning(push)
//#pragma warning(disable:4201)   // Nameless union / struct when compiled for C.

type
  LPDIACTIONA = ^TDIACTIONA;
  PDIACTIONA = ^TDIACTIONA;
  TDIACTIONA = record
    uAppData: UINT_PTR;
    dwSemantic: DWORD;
    dwFlags: DWORD;  {OPTIONAL}
    case Integer of
      0: (
        lptszActionName: LPCSTR;  {OPTIONAL}
        guidInstance: GUID;  {OPTIONAL}
        dwObjID: DWORD;  {OPTIONAL}
        dwHow: DWORD; {OPTIONAL}
      );
      1: (
        uResIdString: UINT  {OPTIONAL}
      );
  end;
  LPDIACTIONW = ^TDIACTIONW;
  PDIACTIONW = ^TDIACTIONW;
  TDIACTIONW = record
    uAppData: UINT_PTR;
    dwSemantic: DWORD;
    dwFlags: DWORD;  {OPTIONAL}
    case Integer of
      0: (
        lptszActionName: LPCWSTR;  {OPTIONAL}
        guidInstance: GUID;  {OPTIONAL}
        dwObjID: DWORD;  {OPTIONAL}
        dwHow: DWORD; {OPTIONAL}
      );
      1: (
        uResIdString: UINT;  {OPTIONAL}
      );
  end;
{$IFDEF UNICODE}
  TDIACTION = TDIACTIONW;
  LPDIACTION = LPDIACTIONW;
  PDIACTION = PDIACTIONW;
{$ELSE}
  TDIACTION = TDIACTIONA;
  LPDIACTION = LPDIACTIONA;
  PDIACTION = PDIACTIONA;
{$ENDIF} // UNICODE

  LPCDIACTIONA = ^TDIACTIONA;
  PCDIACTIONA = ^TDIACTIONA;
  LPCDIACTIONW = ^TDIACTIONW;
  PCDIACTIONW = ^TDIACTIONW;
{$IFDEF UNICODE}
  LPCDIACTION = LPCDIACTIONW;
  PCDIACTION = PCDIACTIONW;
{$ELSE}
  LPCDIACTION = LPCDIACTIONA;
  PCDIACTION = PCDIACTIONA;
{$ENDIF} // UNICODE

//#pragma warning(pop)

const
  DIA_FORCEFEEDBACK = $00000001;
  DIA_APPMAPPED     = $00000002;
  DIA_APPNOMAP      = $00000004;
  DIA_NORANGE       = $00000008;
  DIA_APPFIXED      = $00000010;

  DIAH_UNMAPPED     = $00000000;
  DIAH_USERCONFIG   = $00000001;
  DIAH_APPREQUESTED = $00000002;
  DIAH_HWAPP        = $00000004;
  DIAH_HWDEFAULT    = $00000008;
  DIAH_DEFAULT      = $00000020;
  DIAH_ERROR        = $80000000;

type
  LPDIACTIONFORMATA = ^TDIACTIONFORMATA;
  PDIACTIONFORMATA = ^TDIACTIONFORMATA;
  TDIACTIONFORMATA = record
    dwSize: DWORD;
    dwActionSize: DWORD;
    dwDataSize: DWORD;
    dwNumActions: DWORD;
    rgoAction: LPDIACTIONA;
    guidActionMap: GUID;
    dwGenre: DWORD;
    dwBufferSize: DWORD;
    lAxisMin: LONG;  {OPTIONAL}
    lAxisMax: LONG;  {OPTIONAL}
    hInstString: HINST;  {OPTIONAL}
    ftTimeStamp: FILETIME;
    dwCRC: DWORD;
    tszActionMap: array [0..MAX_PATH-1] of CHAR;
  end;
  LPDIACTIONFORMATW = ^TDIACTIONFORMATW;
  PDIACTIONFORMATW = ^TDIACTIONFORMATW;
  TDIACTIONFORMATW = record
    dwSize: DWORD;
    dwActionSize: DWORD;
    dwDataSize: DWORD;
    dwNumActions: DWORD;
    rgoAction: LPDIACTIONW;
    guidActionMap: GUID;
    dwGenre: DWORD;
    dwBufferSize: DWORD;
    lAxisMin: LONG;  {OPTIONAL}
    lAxisMax: LONG;  {OPTIONAL}
    hInstString: HINST;  {OPTIONAL}
    ftTimeStamp: FILETIME;
    dwCRC: DWORD;
    tszActionMap: array [0..MAX_PATH-1] of WCHAR;
  end;
{$IFDEF UNICODE}
  TDIACTIONFORMAT = TDIACTIONFORMATW;
  LPDIACTIONFORMAT = LPDIACTIONFORMATW;
  PDIACTIONFORMAT = PDIACTIONFORMATW;
{$ELSE}
  TDIACTIONFORMAT = TDIACTIONFORMATA;
  LPDIACTIONFORMAT = LPDIACTIONFORMATA;
  PDIACTIONFORMAT = PDIACTIONFORMATA;
{$ENDIF} // UNICODE
  LPCDIACTIONFORMATA = ^TDIACTIONFORMATA;
  PCDIACTIONFORMATA = ^TDIACTIONFORMATA;
  LPCDIACTIONFORMATW = ^TDIACTIONFORMATW;
  PCDIACTIONFORMATW = ^TDIACTIONFORMATW;
{$IFDEF UNICODE}
  LPCDIACTIONFORMAT = LPCDIACTIONFORMATW;
  PCDIACTIONFORMAT = PCDIACTIONFORMATW;
{$ELSE}
  LPCDIACTIONFORMAT = LPCDIACTIONFORMATA;
  PCDIACTIONFORMAT = PCDIACTIONFORMATA;
{$ENDIF} // UNICODE

const
  DIAFTS_NEWDEVICELOW     = $FFFFFFFF;
  DIAFTS_NEWDEVICEHIGH    = $FFFFFFFF;
  DIAFTS_UNUSEDDEVICELOW  = $00000000;
  DIAFTS_UNUSEDDEVICEHIGH = $00000000;

  DIDBAM_DEFAULT          = $00000000;
  DIDBAM_PRESERVE         = $00000001;
  DIDBAM_INITIALIZE       = $00000002;
  DIDBAM_HWDEFAULTS       = $00000004;

  DIDSAM_DEFAULT          = $00000000;
  DIDSAM_NOUSER           = $00000001;
  DIDSAM_FORCESAVE        = $00000002;

  DICD_DEFAULT            = $00000000;
  DICD_EDIT               = $00000001;

(*
 * The following definition is normally defined in d3dtypes.h
 *)
{$IFNDEF D3DCOLOR_DEFINED}
type
  D3DCOLOR = DWORD;
{$DEFINE D3DCOLOR_DEFINED}
{$ENDIF}

  LPCDICOLORSET = ^TDICOLORSET;
  PCDICOLORSET = ^TDICOLORSET;
  LPDICOLORSET = ^TDICOLORSET;
  PDICOLORSET = ^TDICOLORSET;
  TDICOLORSET = record
    dwSize: DWORD;
    cTextFore: D3DCOLOR;
    cTextHighlight: D3DCOLOR;
    cCalloutLine: D3DCOLOR;
    cCalloutHighlight: D3DCOLOR;
    cBorder: D3DCOLOR;
    cControlFill: D3DCOLOR;
    cHighlightFill: D3DCOLOR;
    cAreaFill: D3DCOLOR;
  end;


  LPDICONFIGUREDEVICESPARAMSA = ^TDICONFIGUREDEVICESPARAMSA;
  PDICONFIGUREDEVICESPARAMSA = ^TDICONFIGUREDEVICESPARAMSA;
  TDICONFIGUREDEVICESPARAMSA = record
    dwSize: DWORD;
    dwcUsers: DWORD;
    lptszUserNames: LPSTR;
    dwcFormats: DWORD;
    lprgFormats: LPDIACTIONFORMATA;
    hwnd: HWND;
    dics: TDICOLORSET;
    lpUnkDDSTarget: {IUnknown FAR *}IUnknown;
  end;
  LPDICONFIGUREDEVICESPARAMSW = ^TDICONFIGUREDEVICESPARAMSW;
  PDICONFIGUREDEVICESPARAMSW = ^TDICONFIGUREDEVICESPARAMSW;
  TDICONFIGUREDEVICESPARAMSW = record
    dwSize: DWORD;
    dwcUsers: DWORD;
    lptszUserNames: LPWSTR;
    dwcFormats: DWORD;
    lprgFormats: LPDIACTIONFORMATW;
    hwnd: HWND;
    dics: TDICOLORSET;
    lpUnkDDSTarget: {IUnknown FAR *}IUnknown;
  end;
{$IFDEF UNICODE}
  TDICONFIGUREDEVICESPARAMS = TDICONFIGUREDEVICESPARAMSW;
  LPDICONFIGUREDEVICESPARAMS = LPDICONFIGUREDEVICESPARAMSW;
  PDICONFIGUREDEVICESPARAMS = PDICONFIGUREDEVICESPARAMSW;
{$ELSE}
  TDICONFIGUREDEVICESPARAMS = TDICONFIGUREDEVICESPARAMSA;
  LPDICONFIGUREDEVICESPARAMS = LPDICONFIGUREDEVICESPARAMSA;
  PDICONFIGUREDEVICESPARAMS = PDICONFIGUREDEVICESPARAMSA;
{$ENDIF} // UNICODE
  LPCDICONFIGUREDEVICESPARAMSA = ^TDICONFIGUREDEVICESPARAMSA;
  PCDICONFIGUREDEVICESPARAMSA = ^TDICONFIGUREDEVICESPARAMSA;
  LPCDICONFIGUREDEVICESPARAMSW = ^TDICONFIGUREDEVICESPARAMSW;
  PCDICONFIGUREDEVICESPARAMSW = ^TDICONFIGUREDEVICESPARAMSW;
{$IFDEF UNICODE}
  LPCDICONFIGUREDEVICESPARAMS = LPCDICONFIGUREDEVICESPARAMSW;
  PCDICONFIGUREDEVICESPARAMS = PCDICONFIGUREDEVICESPARAMSW;
{$ELSE}
  LPCDICONFIGUREDEVICESPARAMS = LPCDICONFIGUREDEVICESPARAMSA;
  PCDICONFIGUREDEVICESPARAMS = PCDICONFIGUREDEVICESPARAMSA;
{$ENDIF} // UNICODE


const
  DIDIFT_CONFIGURATION = $00000001;
  DIDIFT_OVERLAY       = $00000002;

  DIDAL_CENTERED      = $00000000;
  DIDAL_LEFTALIGNED   = $00000001;
  DIDAL_RIGHTALIGNED  = $00000002;
  DIDAL_MIDDLE        = $00000000;
  DIDAL_TOPALIGNED    = $00000004;
  DIDAL_BOTTOMALIGNED = $00000008;

type
  LPDIDEVICEIMAGEINFOA = ^TDIDEVICEIMAGEINFOA;
  PDIDEVICEIMAGEINFOA = ^TDIDEVICEIMAGEINFOA;
  TDIDEVICEIMAGEINFOA = record
    tszImagePath: array [0..MAX_PATH-1] of CHAR;
    dwFlags: DWORD;
    // These are valid if DIDIFT_OVERLAY is present in dwFlags.
    dwViewID: DWORD;
    rcOverlay: RECT;
    dwObjID: DWORD;
    dwcValidPts: DWORD;
    rgptCalloutLine: array [0..4] of POINT;
    rcCalloutRect: RECT;
    dwTextAlign: DWORD;
  end;
  LPDIDEVICEIMAGEINFOW = ^TDIDEVICEIMAGEINFOW;
  PDIDEVICEIMAGEINFOW = ^TDIDEVICEIMAGEINFOW;
  TDIDEVICEIMAGEINFOW = record
    tszImagePath: array [0..MAX_PATH-1] of WCHAR;
    dwFlags: DWORD;
    // These are valid if DIDIFT_OVERLAY is present in dwFlags.
    dwViewID: DWORD;
    rcOverlay: RECT;
    dwObjID: DWORD;
    dwcValidPts: DWORD;
    rgptCalloutLine: array [0..4] of POINT;
    rcCalloutRect: RECT;
    dwTextAlign: DWORD;
  end;
{$IFDEF UNICODE}
  TDIDEVICEIMAGEINFO = TDIDEVICEIMAGEINFOW;
  LPDIDEVICEIMAGEINFO = LPDIDEVICEIMAGEINFOW;
  PDIDEVICEIMAGEINFO = PDIDEVICEIMAGEINFOW;
{$ELSE}
  TDIDEVICEIMAGEINFO = TDIDEVICEIMAGEINFOA;
  LPDIDEVICEIMAGEINFO = LPDIDEVICEIMAGEINFOA;
  PDIDEVICEIMAGEINFO = PDIDEVICEIMAGEINFOA;
{$ENDIF} // UNICODE
  LPCDIDEVICEIMAGEINFOA = ^TDIDEVICEIMAGEINFOA;
  PCDIDEVICEIMAGEINFOA = ^TDIDEVICEIMAGEINFOA;
  LPCDIDEVICEIMAGEINFOW = ^TDIDEVICEIMAGEINFOW;
  PCDIDEVICEIMAGEINFOW = ^TDIDEVICEIMAGEINFOW;
{$IFDEF UNICODE}
  LPCDIDEVICEIMAGEINFO = LPCDIDEVICEIMAGEINFOW;
  PCDIDEVICEIMAGEINFO = PCDIDEVICEIMAGEINFOW;
{$ELSE}
  LPCDIDEVICEIMAGEINFO = LPCDIDEVICEIMAGEINFOA;
  PCDIDEVICEIMAGEINFO = PCDIDEVICEIMAGEINFOA;
{$ENDIF} // UNICODE

  LPDIDEVICEIMAGEINFOHEADERA = ^TDIDEVICEIMAGEINFOHEADERA;
  PDIDEVICEIMAGEINFOHEADERA = ^TDIDEVICEIMAGEINFOHEADERA;
  TDIDEVICEIMAGEINFOHEADERA = record
    dwSize: DWORD;
    dwSizeImageInfo: DWORD;
    dwcViews: DWORD;
    dwcButtons: DWORD;
    dwcAxes: DWORD;
    dwcPOVs: DWORD;
    dwBufferSize: DWORD;
    dwBufferUsed: DWORD;
    lprgImageInfoArray: LPDIDEVICEIMAGEINFOA;
  end;
  LPDIDEVICEIMAGEINFOHEADERW = ^TDIDEVICEIMAGEINFOHEADERW;
  PDIDEVICEIMAGEINFOHEADERW = ^TDIDEVICEIMAGEINFOHEADERW;
  TDIDEVICEIMAGEINFOHEADERW = record
    dwSize: DWORD;
    dwSizeImageInfo: DWORD;
    dwcViews: DWORD;
    dwcButtons: DWORD;
    dwcAxes: DWORD;
    dwcPOVs: DWORD;
    dwBufferSize: DWORD;
    dwBufferUsed: DWORD;
    lprgImageInfoArray: LPDIDEVICEIMAGEINFOW;
  end;
{$IFDEF UNICODE}
  TDIDEVICEIMAGEINFOHEADER = TDIDEVICEIMAGEINFOHEADERW;
  LPDIDEVICEIMAGEINFOHEADER = LPDIDEVICEIMAGEINFOHEADERW;
  PDIDEVICEIMAGEINFOHEADER = PDIDEVICEIMAGEINFOHEADERW;
{$ELSE}
  TDIDEVICEIMAGEINFOHEADER = TDIDEVICEIMAGEINFOHEADERA;
  LPDIDEVICEIMAGEINFOHEADER = LPDIDEVICEIMAGEINFOHEADERA;
  PDIDEVICEIMAGEINFOHEADER = PDIDEVICEIMAGEINFOHEADERA;
{$ENDIF} // UNICODE
  LPCDIDEVICEIMAGEINFOHEADERA = ^TDIDEVICEIMAGEINFOHEADERA;
  PCDIDEVICEIMAGEINFOHEADERA = ^TDIDEVICEIMAGEINFOHEADERA;
  LPCDIDEVICEIMAGEINFOHEADERW = ^TDIDEVICEIMAGEINFOHEADERW;
  PCDIDEVICEIMAGEINFOHEADERW = ^TDIDEVICEIMAGEINFOHEADERW;
{$IFDEF UNICODE}
  LPCDIDEVICEIMAGEINFOHEADER = LPCDIDEVICEIMAGEINFOHEADERW;
  PCDIDEVICEIMAGEINFOHEADER = PCDIDEVICEIMAGEINFOHEADERW;
{$ELSE}
  LPCDIDEVICEIMAGEINFOHEADER = LPCDIDEVICEIMAGEINFOHEADERA;
  PCDIDEVICEIMAGEINFOHEADER = PCDIDEVICEIMAGEINFOHEADERA;
{$ENDIF} // UNICODE

{$ENDIF} { DIRECTINPUT_VERSION > $0700 }

{$IF DIRECTINPUT_VERSION >= $0500}
{ These structures are defined for DirectX 3.0 compatibility }

type
  LPDIDEVICEOBJECTINSTANCE_DX3A = ^TDIDEVICEOBJECTINSTANCE_DX3A;
  PDIDEVICEOBJECTINSTANCE_DX3A = ^TDIDEVICEOBJECTINSTANCE_DX3A;
  TDIDEVICEOBJECTINSTANCE_DX3A = record
    dwSize: DWORD;
    guidType: GUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array [0..MAX_PATH-1] of CHAR;
  end;
  LPDIDEVICEOBJECTINSTANCE_DX3W = ^TDIDEVICEOBJECTINSTANCE_DX3W;
  PDIDEVICEOBJECTINSTANCE_DX3W = ^TDIDEVICEOBJECTINSTANCE_DX3W;
  TDIDEVICEOBJECTINSTANCE_DX3W = record
    dwSize: DWORD;
    guidType: GUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array [0..MAX_PATH-1] of WCHAR;
  end;
{$IFDEF UNICODE}
  TDIDEVICEOBJECTINSTANCE_DX3 = TDIDEVICEOBJECTINSTANCE_DX3W;
  LPDIDEVICEOBJECTINSTANCE_DX3 = LPDIDEVICEOBJECTINSTANCE_DX3W;
  PDIDEVICEOBJECTINSTANCE_DX3 = PDIDEVICEOBJECTINSTANCE_DX3W;
{$ELSE}
  TDIDEVICEOBJECTINSTANCE_DX3 = TDIDEVICEOBJECTINSTANCE_DX3A;
  LPDIDEVICEOBJECTINSTANCE_DX3 = LPDIDEVICEOBJECTINSTANCE_DX3A;
  PDIDEVICEOBJECTINSTANCE_DX3 = PDIDEVICEOBJECTINSTANCE_DX3A;
{$ENDIF} // UNICODE
  LPCDIDEVICEOBJECTINSTANCE_DX3A = ^TDIDEVICEOBJECTINSTANCE_DX3A;
  LPCDIDEVICEOBJECTINSTANCE_DX3W = ^TDIDEVICEOBJECTINSTANCE_DX3W;
  LPCDIDEVICEOBJECTINSTANCE_DX3 = ^TDIDEVICEOBJECTINSTANCE_DX3;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }

  LPDIDEVICEOBJECTINSTANCEA = ^TDIDEVICEOBJECTINSTANCEA;
  PDIDEVICEOBJECTINSTANCEA = ^TDIDEVICEOBJECTINSTANCEA;
  TDIDEVICEOBJECTINSTANCEA = record
    dwSize: DWORD;
    guidType: GUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array [0..MAX_PATH-1] of CHAR;
{$IF DIRECTINPUT_VERSION >= $0500}
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReportId: WORD;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  end;
  LPDIDEVICEOBJECTINSTANCEW = ^TDIDEVICEOBJECTINSTANCEW;
  PDIDEVICEOBJECTINSTANCEW = ^TDIDEVICEOBJECTINSTANCEW;
  TDIDEVICEOBJECTINSTANCEW = record
    dwSize: DWORD;
    guidType: GUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array [0..MAX_PATH-1] of WCHAR;
{$IF DIRECTINPUT_VERSION >= $0500}
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReportId: WORD;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  end;
{$IFDEF UNICODE}
  TDIDEVICEOBJECTINSTANCE = TDIDEVICEOBJECTINSTANCEW;
  LPDIDEVICEOBJECTINSTANCE = LPDIDEVICEOBJECTINSTANCEW;
  PDIDEVICEOBJECTINSTANCE = PDIDEVICEOBJECTINSTANCEW;
{$ELSE}
  TDIDEVICEOBJECTINSTANCE = TDIDEVICEOBJECTINSTANCEA;
  LPDIDEVICEOBJECTINSTANCE = LPDIDEVICEOBJECTINSTANCEA;
  PDIDEVICEOBJECTINSTANCE = PDIDEVICEOBJECTINSTANCEA;
{$ENDIF} // UNICODE
  LPCDIDEVICEOBJECTINSTANCEA = ^TDIDEVICEOBJECTINSTANCEA;
  PCDIDEVICEOBJECTINSTANCEA = ^TDIDEVICEOBJECTINSTANCEA;
  LPCDIDEVICEOBJECTINSTANCEW = ^TDIDEVICEOBJECTINSTANCEW;
  PCDIDEVICEOBJECTINSTANCEW = ^TDIDEVICEOBJECTINSTANCEW;
  LPCDIDEVICEOBJECTINSTANCE = ^TDIDEVICEOBJECTINSTANCE;
  PCDIDEVICEOBJECTINSTANCE = ^TDIDEVICEOBJECTINSTANCE;

  LPDIENUMDEVICEOBJECTSCALLBACKA = function(lpddoi: LPCDIDEVICEOBJECTINSTANCEA; pvRef: LPVOID): BOOL; stdcall;
  LPDIENUMDEVICEOBJECTSCALLBACKW = function(lpddoi: LPCDIDEVICEOBJECTINSTANCEW; pvRef: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
  LPDIENUMDEVICEOBJECTSCALLBACK = LPDIENUMDEVICEOBJECTSCALLBACKW;
{$ELSE}
  LPDIENUMDEVICEOBJECTSCALLBACK = LPDIENUMDEVICEOBJECTSCALLBACKA;
{$ENDIF} // !UNICODE

{$IF DIRECTINPUT_VERSION >= $0500}
const
  DIDOI_FFACTUATOR      = $00000001;
  DIDOI_FFEFFECTTRIGGER = $00000002;
  DIDOI_POLLED          = $00008000;
  DIDOI_ASPECTPOSITION  = $00000100;
  DIDOI_ASPECTVELOCITY  = $00000200;
  DIDOI_ASPECTACCEL     = $00000300;
  DIDOI_ASPECTFORCE     = $00000400;
  DIDOI_ASPECTMASK      = $00000F00;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
{$IF DIRECTINPUT_VERSION >= $050a}
  DIDOI_GUIDISUSAGE     = $00010000;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

type
  LPCDIPROPHEADER = ^TDIPROPHEADER;
  PCDIPROPHEADER = ^TDIPROPHEADER;
  LPDIPROPHEADER = ^TDIPROPHEADER;
  PDIPROPHEADER = ^TDIPROPHEADER;
  TDIPROPHEADER = record
    dwSize: DWORD;
    dwHeaderSize: DWORD;
    dwObj: DWORD;
    dwHow: DWORD;
  end;

const
  DIPH_DEVICE           = 0;
  DIPH_BYOFFSET         = 1;
  DIPH_BYID             = 2;
{$IF DIRECTINPUT_VERSION >= $050a}
  DIPH_BYUSAGE          = 3;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

//#if(DIRECTINPUT_VERSION >= 0x050a)
//#define DIMAKEUSAGEDWORD(UsagePage, Usage) \
//                                (DWORD)MAKELONG(Usage, UsagePage)
//#endif /* DIRECTINPUT_VERSION >= 0x050a */

type
  LPCDIPROPDWORD = ^TDIPROPDWORD;
  PCDIPROPDWORD = ^TDIPROPDWORD;
  LPDIPROPDWORD = ^TDIPROPDWORD;
  PDIPROPDWORD = ^TDIPROPDWORD;
  TDIPROPDWORD = record
    diph: TDIPROPHEADER;
    dwData: DWORD;
  end;

{$IF DIRECTINPUT_VERSION >= $0800}
  LPCDIPROPPOINTER = ^TDIPROPPOINTER;
  PCDIPROPPOINTER = ^TDIPROPPOINTER;
  LPDIPROPPOINTER = ^TDIPROPPOINTER;
  PDIPROPPOINTER = ^TDIPROPPOINTER;
  TDIPROPPOINTER = record
    diph: TDIPROPHEADER;
    uData: UINT_PTR;
  end;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

  LPCDIPROPRANGE = ^TDIPROPRANGE;
  PCDIPROPRANGE = ^TDIPROPRANGE;
  LPDIPROPRANGE = ^TDIPROPRANGE;
  PDIPROPRANGE = ^TDIPROPRANGE;
  TDIPROPRANGE = record
    diph: TDIPROPHEADER;
    lMin: LONG;
    lMax: LONG;
  end;

const
  DIPROPRANGE_NOMIN     = LONG($80000000);
  DIPROPRANGE_NOMAX     = LONG($7FFFFFFF);

{$IF DIRECTINPUT_VERSION >= $050a}
type
  LPCDIPROPCAL = ^TDIPROPCAL;
  PCDIPROPCAL = ^TDIPROPCAL;
  LPDIPROPCAL = ^TDIPROPCAL;
  PDIPROPCAL = ^TDIPROPCAL;
  TDIPROPCAL = record
    diph: TDIPROPHEADER;
    lMin: LONG;
    lCenter: LONG;
    lMax: LONG;
  end;

  LPCDIPROPCALPOV = ^TDIPROPCALPOV;
  PCDIPROPCALPOV = ^TDIPROPCALPOV;
  LPDIPROPCALPOV = ^TDIPROPCALPOV;
  PDIPROPCALPOV = ^TDIPROPCALPOV;
  TDIPROPCALPOV = record
    diph: TDIPROPHEADER;
    lMin: array [0..4] of LONG;
    lMax: array [0..4] of LONG;
  end;

  LPCDIPROPGUIDANDPATH = ^TDIPROPGUIDANDPATH;
  PCDIPROPGUIDANDPATH = ^TDIPROPGUIDANDPATH;
  LPDIPROPGUIDANDPATH = ^TDIPROPGUIDANDPATH;
  PDIPROPGUIDANDPATH = ^TDIPROPGUIDANDPATH;
  TDIPROPGUIDANDPATH = record
    diph: TDIPROPHEADER;
    guidClass: GUID;
    wszPath: array [0..MAX_PATH-1] of WCHAR;
  end;

  LPCDIPROPSTRING = ^TDIPROPSTRING;
  PCDIPROPSTRING = ^TDIPROPSTRING;
  LPDIPROPSTRING = ^TDIPROPSTRING;
  PDIPROPSTRING = ^TDIPROPSTRING;
  TDIPROPSTRING = record
    diph: TDIPROPHEADER;
    wsz: array [0..MAX_PATH-1] of WCHAR;
  end;

{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

{$IF DIRECTINPUT_VERSION >= $0800}
const
  MAXCPOINTSNUM        = 8;

type
  PCPOINT = ^TCPOINT;
  TCPOINT = record
    lP: LONG;      // raw value
    dwLog: DWORD;  // logical_value / max_logical_value * 10000
  end;

  LPCDIPROPCPOINTS = ^TDIPROPCPOINTS;
  PCDIPROPCPOINTS = ^TDIPROPCPOINTS;
  LPDIPROPCPOINTS = ^TDIPROPCPOINTS;
  PDIPROPCPOINTS = ^TDIPROPCPOINTS;
  TDIPROPCPOINTS = record
    diph: TDIPROPHEADER;
    dwCPointsNum: DWORD;
    cp: array [0..MAXCPOINTSNUM-1] of TCPOINT;
  end;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }


//#ifdef __cplusplus
//#define MAKEDIPROP(prop)    (*(const GUID * )(prop))
//#else
//#define MAKEDIPROP(prop)    ((REFGUID)(prop))
//#endif

var
//#define DIPROP_BUFFERSIZE       MAKEDIPROP(1)
  DIPROP_BUFFERSIZE: TGuid absolute 1;

//#define DIPROP_AXISMODE         MAKEDIPROP(2)
  DIPROP_AXISMODE: TGuid absolute 2;

const
  DIPROPAXISMODE_ABS    = 0;
  DIPROPAXISMODE_REL    = 1;

var
//#define DIPROP_GRANULARITY      MAKEDIPROP(3)
  DIPROP_GRANULARITY: TGuid absolute 3;

//#define DIPROP_RANGE            MAKEDIPROP(4)
  DIPROP_RANGE: TGuid absolute 4;

//#define DIPROP_DEADZONE         MAKEDIPROP(5)
  DIPROP_DEADZONE: TGuid absolute 5;

//#define DIPROP_SATURATION       MAKEDIPROP(6)
  DIPROP_SATURATION: TGuid absolute 6;

//#define DIPROP_FFGAIN           MAKEDIPROP(7)
  DIPROP_FFGAIN: TGuid absolute 7;

//#define DIPROP_FFLOAD           MAKEDIPROP(8)
  DIPROP_FFLOAD: TGuid absolute 8;

//#define DIPROP_AUTOCENTER       MAKEDIPROP(9)
  DIPROP_AUTOCENTER: TGuid absolute 9;

const
  DIPROPAUTOCENTER_OFF  = 0;
  DIPROPAUTOCENTER_ON   = 1;

var
//#define DIPROP_CALIBRATIONMODE  MAKEDIPROP(10)
  DIPROP_CALIBRATIONMODE: TGuid absolute 10;

const
  DIPROPCALIBRATIONMODE_COOKED  = 0;
  DIPROPCALIBRATIONMODE_RAW     = 1;

{$IF DIRECTINPUT_VERSION >= $050a}
var
//#define DIPROP_CALIBRATION      MAKEDIPROP(11)
  DIPROP_CALIBRATION: TGuid absolute 11;

//#define DIPROP_GUIDANDPATH      MAKEDIPROP(12)
  DIPROP_GUIDANDPATH: TGuid absolute 12;

//#define DIPROP_INSTANCENAME     MAKEDIPROP(13)
  DIPROP_INSTANCENAME: TGuid absolute 13;

//#define DIPROP_PRODUCTNAME      MAKEDIPROP(14)
  DIPROP_PRODUCTNAME: TGuid absolute 14;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

{$IF DIRECTINPUT_VERSION >= $05b2}
var
//#define DIPROP_JOYSTICKID       MAKEDIPROP(15)
  DIPROP_JOYSTICKID: TGuid absolute 15;

//#define DIPROP_GETPORTDISPLAYNAME       MAKEDIPROP(16)
  DIPROP_GETPORTDISPLAYNAME: TGuid absolute 16;

{$ENDIF} { DIRECTINPUT_VERSION >= $05b2 }

{$IF DIRECTINPUT_VERSION >= $0700}
var
//#define DIPROP_PHYSICALRANGE            MAKEDIPROP(18)
  DIPROP_PHYSICALRANGE: TGuid absolute 18;

//#define DIPROP_LOGICALRANGE             MAKEDIPROP(19)
  DIPROP_LOGICALRANGE: TGuid absolute 19;
{$ENDIF} { DIRECTINPUT_VERSION >= $0700 }

{$IF DIRECTINPUT_VERSION >= $0800}
//#define DIPROP_KEYNAME                     MAKEDIPROP(20)
  DIPROP_KEYNAME: TGuid absolute 20;

//#define DIPROP_CPOINTS                 MAKEDIPROP(21)
  DIPROP_CPOINTS: TGuid absolute 21;

//#define DIPROP_APPDATA       MAKEDIPROP(22)
  DIPROP_APPDATA: TGuid absolute 22;

//#define DIPROP_SCANCODE      MAKEDIPROP(23)
  DIPROP_SCANCODE: TGuid absolute 23;

//#define DIPROP_VIDPID           MAKEDIPROP(24)
  DIPROP_VIDPID: TGuid absolute 24;

//#define DIPROP_USERNAME         MAKEDIPROP(25)
  DIPROP_USERNAME: TGuid absolute 25;

//#define DIPROP_TYPENAME         MAKEDIPROP(26)
  DIPROP_TYPENAME: TGuid absolute 26;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }


type
  LPCDIDEVICEOBJECTDATA_DX = ^TDIDEVICEOBJECTDATA_DX3;
  PCDIDEVICEOBJECTDATA_DX = ^TDIDEVICEOBJECTDATA_DX3;
  LPDIDEVICEOBJECTDATA_DX3 = ^TDIDEVICEOBJECTDATA_DX3;
  PDIDEVICEOBJECTDATA_DX3 = ^TDIDEVICEOBJECTDATA_DX3;
  TDIDEVICEOBJECTDATA_DX3 = record
    dwOfs: DWORD;
    dwData: DWORD;
    dwTimeStamp: DWORD;
    dwSequence: DWORD;
  end;

  LPCDIDEVICEOBJECTDATA = ^TDIDEVICEOBJECTDATA;
  PCDIDEVICEOBJECTDATA = ^TDIDEVICEOBJECTDATA;
  LPDIDEVICEOBJECTDATA = ^TDIDEVICEOBJECTDATA;
  PDIDEVICEOBJECTDATA = ^TDIDEVICEOBJECTDATA;
  TDIDEVICEOBJECTDATA = record
    dwOfs: DWORD;
    dwData: DWORD;
    dwTimeStamp: DWORD;
    dwSequence: DWORD;
{$IF DIRECTINPUT_VERSION >= $0800}
    uAppData: UINT_PTR;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }
  end;

const
  DIGDD_PEEK        = $00000001;

//#define DISEQUENCE_COMPARE(dwSequence1, cmp, dwSequence2) \
//                        ((int)((dwSequence1) - (dwSequence2)) cmp 0)
const
  DISCL_EXCLUSIVE    = $00000001;
  DISCL_NONEXCLUSIVE = $00000002;
  DISCL_FOREGROUND   = $00000004;
  DISCL_BACKGROUND   = $00000008;
  DISCL_NOWINKEY     = $00000010;

{$IF DIRECTINPUT_VERSION >= $0500}
{ These structures are defined for DirectX 3.0 compatibility }

type
  LPDIDEVICEINSTANCE_DX3A = ^TDIDEVICEINSTANCE_DX3A;
  PDIDEVICEINSTANCE_DX3A = ^TDIDEVICEINSTANCE_DX3A;
  TDIDEVICEINSTANCE_DX3A = record
    dwSize: DWORD;
    guidInstance: GUID;
    guidProduct: GUID;
    dwDevType: DWORD;
    tszInstanceName: array [0..MAX_PATH-1] of CHAR;
    tszProductName: array [0..MAX_PATH-1] of CHAR;
  end;
  LPDIDEVICEINSTANCE_DX3W = ^TDIDEVICEINSTANCE_DX3W;
  PDIDEVICEINSTANCE_DX3W = ^TDIDEVICEINSTANCE_DX3W;
  TDIDEVICEINSTANCE_DX3W = record
    dwSize: DWORD;
    guidInstance: GUID;
    guidProduct: GUID;
    dwDevType: DWORD;
    tszInstanceName: array [0..MAX_PATH-1] of WCHAR;
    tszProductName: array [0..MAX_PATH-1] of WCHAR;
  end;
{$IFDEF UNICODE}
  TDIDEVICEINSTANCE_DX3 = TDIDEVICEINSTANCE_DX3W;
  LPDIDEVICEINSTANCE_DX3 = LPDIDEVICEINSTANCE_DX3W;
  PDIDEVICEINSTANCE_DX3 = PDIDEVICEINSTANCE_DX3W;
{$ELSE}
  TDIDEVICEINSTANCE_DX3 = TDIDEVICEINSTANCE_DX3A;
  LPDIDEVICEINSTANCE_DX3 = LPDIDEVICEINSTANCE_DX3A;
  PDIDEVICEINSTANCE_DX3 = PDIDEVICEINSTANCE_DX3A;
{$ENDIF} // UNICODE
  LPCDIDEVICEINSTANCE_DX3A = ^TDIDEVICEINSTANCE_DX3A;
  PCDIDEVICEINSTANCE_DX3A = ^TDIDEVICEINSTANCE_DX3A;
  LPCDIDEVICEINSTANCE_DX3W = ^TDIDEVICEINSTANCE_DX3W;
  PCDIDEVICEINSTANCE_DX3W = ^TDIDEVICEINSTANCE_DX3W;
  LPCDIDEVICEINSTANCE_DX3 = ^TDIDEVICEINSTANCE_DX3;
  PCDIDEVICEINSTANCE_DX3 = ^TDIDEVICEINSTANCE_DX3;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }

  LPDIDEVICEINSTANCEA = ^TDIDEVICEINSTANCEA;
  PDIDEVICEINSTANCEA = ^TDIDEVICEINSTANCEA;
  TDIDEVICEINSTANCEA = record
    dwSize: DWORD;
    guidInstance: GUID;
    guidProduct: GUID;
    dwDevType: DWORD;
    tszInstanceName: array [0..MAX_PATH-1] of CHAR;
    tszProductName: array [0..MAX_PATH-1] of CHAR;
{$IF DIRECTINPUT_VERSION >= $0500}
    guidFFDriver: GUID;
    wUsagePage: WORD;
    wUsage: WORD;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  end;
  LPDIDEVICEINSTANCEW = ^TDIDEVICEINSTANCEW;
  PDIDEVICEINSTANCEW = ^TDIDEVICEINSTANCEW;
  TDIDEVICEINSTANCEW = record
    dwSize: DWORD;
    guidInstance: GUID;
    guidProduct: GUID;
    dwDevType: DWORD;
    tszInstanceName: array [0..MAX_PATH-1] of WCHAR;
    tszProductName: array [0..MAX_PATH-1] of WCHAR;
{$IF DIRECTINPUT_VERSION >= $0500}
    guidFFDriver: GUID;
    wUsagePage: WORD;
    wUsage: WORD;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
  end;
{$IFDEF UNICODE}
  TDIDEVICEINSTANCE = TDIDEVICEINSTANCEW;
  LPDIDEVICEINSTANCE = LPDIDEVICEINSTANCEW;
  PDIDEVICEINSTANCE = PDIDEVICEINSTANCEW;
{$ELSE}
  TDIDEVICEINSTANCE = TDIDEVICEINSTANCEA;
  LPDIDEVICEINSTANCE = LPDIDEVICEINSTANCEA;
  PDIDEVICEINSTANCE = PDIDEVICEINSTANCEA;
{$ENDIF} // UNICODE

  LPCDIDEVICEINSTANCEA = ^TDIDEVICEINSTANCEA;
  PCDIDEVICEINSTANCEA = ^TDIDEVICEINSTANCEA;
  LPCDIDEVICEINSTANCEW = ^TDIDEVICEINSTANCEW;
  PCDIDEVICEINSTANCEW = ^TDIDEVICEINSTANCEW;
  LPCDIDEVICEINSTANCE = ^TDIDEVICEINSTANCE;
  PCDIDEVICEINSTANCE = ^TDIDEVICEINSTANCE;

//#undef INTERFACE
//#define INTERFACE IDirectInputDeviceW

  IDirectInputDeviceW = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDeviceW methods ***)
    function GetCapabilities(lpDIDevCaps: LPDIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKW; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPDIPROPHEADER): HRESULT; stdcall;
    function SetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPCDIPROPHEADER): HRESULT; stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: LPVOID): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: LPDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(lpdf: LPCDIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: HANDLE): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(pdidoi: LPDIDEVICEOBJECTINSTANCEW; dwObj: DWORD; dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(pdidi: LPDIDEVICEINSTANCEW): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDeviceW *LPDIRECTINPUTDEVICEW;
  LPDIRECTINPUTDEVICEW = IDirectInputDeviceW;

//#undef INTERFACE
//#define INTERFACE IDirectInputDeviceA

  IDirectInputDeviceA = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDeviceA methods ***)
    function GetCapabilities(lpDIDevCaps: LPDIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKA; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPDIPROPHEADER): HRESULT; stdcall;
    function SetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPCDIPROPHEADER): HRESULT; stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: LPVOID): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: LPDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(lpdf: LPCDIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: HANDLE): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(pdidoi: LPDIDEVICEOBJECTINSTANCEA; dwObj: DWORD; dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(pdidi: LPDIDEVICEINSTANCEA): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDeviceA *LPDIRECTINPUTDEVICEA;
  LPDIRECTINPUTDEVICEA = IDirectInputDeviceA;

{$IFDEF UNICODE}
const
  IID_IDirectInputDevice: TIID = '{5944E681-C92E-11CF-BFC7-444553540000}'{IID_IDirectInputDeviceW};
type
  IDirectInputDevice = IDirectInputDeviceW;
{$ELSE}
const
  IID_IDirectInputDevice: TIID = '{5944E680-C92E-11CF-BFC7-444553540000}'{IID_IDirectInputDeviceA};
type
  IDirectInputDevice = IDirectInputDeviceA;
{$ENDIF}
//typedef struct IDirectInputDevice *LPDIRECTINPUTDEVICE;
  LPDIRECTINPUTDEVICE = IDirectInputDevice;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInputDevice_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInputDevice_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInputDevice_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInputDevice_GetCapabilities(p,a) (p)->lpVtbl->GetCapabilities(p,a)
//#define IDirectInputDevice_EnumObjects(p,a,b,c) (p)->lpVtbl->EnumObjects(p,a,b,c)
//#define IDirectInputDevice_GetProperty(p,a,b) (p)->lpVtbl->GetProperty(p,a,b)
//#define IDirectInputDevice_SetProperty(p,a,b) (p)->lpVtbl->SetProperty(p,a,b)
//#define IDirectInputDevice_Acquire(p) (p)->lpVtbl->Acquire(p)
//#define IDirectInputDevice_Unacquire(p) (p)->lpVtbl->Unacquire(p)
//#define IDirectInputDevice_GetDeviceState(p,a,b) (p)->lpVtbl->GetDeviceState(p,a,b)
//#define IDirectInputDevice_GetDeviceData(p,a,b,c,d) (p)->lpVtbl->GetDeviceData(p,a,b,c,d)
//#define IDirectInputDevice_SetDataFormat(p,a) (p)->lpVtbl->SetDataFormat(p,a)
//#define IDirectInputDevice_SetEventNotification(p,a) (p)->lpVtbl->SetEventNotification(p,a)
//#define IDirectInputDevice_SetCooperativeLevel(p,a,b) (p)->lpVtbl->SetCooperativeLevel(p,a,b)
//#define IDirectInputDevice_GetObjectInfo(p,a,b,c) (p)->lpVtbl->GetObjectInfo(p,a,b,c)
//#define IDirectInputDevice_GetDeviceInfo(p,a) (p)->lpVtbl->GetDeviceInfo(p,a)
//#define IDirectInputDevice_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInputDevice_Initialize(p,a,b,c) (p)->lpVtbl->Initialize(p,a,b,c)
//#else
//#define IDirectInputDevice_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInputDevice_AddRef(p) (p)->AddRef()
//#define IDirectInputDevice_Release(p) (p)->Release()
//#define IDirectInputDevice_GetCapabilities(p,a) (p)->GetCapabilities(a)
//#define IDirectInputDevice_EnumObjects(p,a,b,c) (p)->EnumObjects(a,b,c)
//#define IDirectInputDevice_GetProperty(p,a,b) (p)->GetProperty(a,b)
//#define IDirectInputDevice_SetProperty(p,a,b) (p)->SetProperty(a,b)
//#define IDirectInputDevice_Acquire(p) (p)->Acquire()
//#define IDirectInputDevice_Unacquire(p) (p)->Unacquire()
//#define IDirectInputDevice_GetDeviceState(p,a,b) (p)->GetDeviceState(a,b)
//#define IDirectInputDevice_GetDeviceData(p,a,b,c,d) (p)->GetDeviceData(a,b,c,d)
//#define IDirectInputDevice_SetDataFormat(p,a) (p)->SetDataFormat(a)
//#define IDirectInputDevice_SetEventNotification(p,a) (p)->SetEventNotification(a)
//#define IDirectInputDevice_SetCooperativeLevel(p,a,b) (p)->SetCooperativeLevel(a,b)
//#define IDirectInputDevice_GetObjectInfo(p,a,b,c) (p)->GetObjectInfo(a,b,c)
//#define IDirectInputDevice_GetDeviceInfo(p,a) (p)->GetDeviceInfo(a)
//#define IDirectInputDevice_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInputDevice_Initialize(p,a,b,c) (p)->Initialize(a,b,c)
//#endif

//#endif /* DIJ_RINGZERO */


{$IF DIRECTINPUT_VERSION >= $0500}

const
  DISFFC_RESET           = $00000001;
  DISFFC_STOPALL         = $00000002;
  DISFFC_PAUSE           = $00000004;
  DISFFC_CONTINUE        = $00000008;
  DISFFC_SETACTUATORSON  = $00000010;
  DISFFC_SETACTUATORSOFF = $00000020;

  DIGFFS_EMPTY           = $00000001;
  DIGFFS_STOPPED         = $00000002;
  DIGFFS_PAUSED          = $00000004;
  DIGFFS_ACTUATORSON     = $00000010;
  DIGFFS_ACTUATORSOFF    = $00000020;
  DIGFFS_POWERON         = $00000040;
  DIGFFS_POWEROFF        = $00000080;
  DIGFFS_SAFETYSWITCHON  = $00000100;
  DIGFFS_SAFETYSWITCHOFF = $00000200;
  DIGFFS_USERFFSWITCHON  = $00000400;
  DIGFFS_USERFFSWITCHOFF = $00000800;
  DIGFFS_DEVICELOST      = $80000000;

//#ifndef DIJ_RINGZERO

type
  LPDIEFFECTINFOA = ^TDIEFFECTINFOA;
  PDIEFFECTINFOA = ^TDIEFFECTINFOA;
  TDIEFFECTINFOA = record
    dwSize: DWORD;
    guid: GUID;
    dwEffType: DWORD;
    dwStaticParams: DWORD;
    dwDynamicParams: DWORD;
    tszName: array [0..MAX_PATH-1] of CHAR;
  end;
  LPDIEFFECTINFOW = ^TDIEFFECTINFOW;
  PDIEFFECTINFOW = ^TDIEFFECTINFOW;
  TDIEFFECTINFOW = record
    dwSize: DWORD;
    guid: GUID;
    dwEffType: DWORD;
    dwStaticParams: DWORD;
    dwDynamicParams: DWORD;
    tszName: array [0..MAX_PATH-1] of WCHAR;
  end;
{$IFDEF UNICODE}
  TDIEFFECTINFO = TDIEFFECTINFOW;
  LPDIEFFECTINFO = LPDIEFFECTINFOW;
  PDIEFFECTINFO = PDIEFFECTINFOW;
{$ELSE}
  TDIEFFECTINFO = TDIEFFECTINFOA;
  LPDIEFFECTINFO = LPDIEFFECTINFOA;
  PDIEFFECTINFO = PDIEFFECTINFOA;
{$ENDIF} // UNICODE
  LPCDIEFFECTINFOA = ^TDIEFFECTINFOA;
  PCDIEFFECTINFOA = ^TDIEFFECTINFOA;
  LPCDIEFFECTINFOW = ^TDIEFFECTINFOW;
  PCDIEFFECTINFOW = ^TDIEFFECTINFOW;
  LPCDIEFFECTINFO = ^TDIEFFECTINFO;
  PCDIEFFECTINFO = ^TDIEFFECTINFO;

const
  DISDD_CONTINUE        = $00000001;

type
  LPDIENUMEFFECTSCALLBACKA = function(pdei: LPCDIEFFECTINFOA; pvRef: LPVOID): BOOL; stdcall;
  LPDIENUMEFFECTSCALLBACKW = function(pdei: LPCDIEFFECTINFOW; pvRef: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
  LPDIENUMEFFECTSCALLBACK = LPDIENUMEFFECTSCALLBACKW;
{$ELSE}
  LPDIENUMEFFECTSCALLBACK = LPDIENUMEFFECTSCALLBACKA;
{$ENDIF} // !UNICODE
  LPDIENUMCREATEDEFFECTOBJECTSCALLBACK = function(peff: LPDIRECTINPUTEFFECT; pvRef: LPVOID): BOOL; stdcall;

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice2W

  IDirectInputDevice2W = interface(IDirectInputDeviceW)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDeviceW methods ***)
    {STDMETHOD(GetCapabilities)(THIS_ LPDIDEVCAPS) PURE;
    STDMETHOD(EnumObjects)(THIS_ LPDIENUMDEVICEOBJECTSCALLBACKW,LPVOID,DWORD) PURE;
    STDMETHOD(GetProperty)(THIS_ REFGUID,LPDIPROPHEADER) PURE;
    STDMETHOD(SetProperty)(THIS_ REFGUID,LPCDIPROPHEADER) PURE;
    STDMETHOD(Acquire)(THIS) PURE;
    STDMETHOD(Unacquire)(THIS) PURE;
    STDMETHOD(GetDeviceState)(THIS_ DWORD,LPVOID) PURE;
    STDMETHOD(GetDeviceData)(THIS_ DWORD,LPDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;
    STDMETHOD(SetDataFormat)(THIS_ LPCDIDATAFORMAT) PURE;
    STDMETHOD(SetEventNotification)(THIS_ HANDLE) PURE;
    STDMETHOD(SetCooperativeLevel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(GetObjectInfo)(THIS_ LPDIDEVICEOBJECTINSTANCEW,DWORD,DWORD) PURE;
    STDMETHOD(GetDeviceInfo)(THIS_ LPDIDEVICEINSTANCEW) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD,REFGUID) PURE;}

    (*** IDirectInputDevice2W methods ***)
    function CreateEffect(const rguid: TGuid {REFGUID}; lpeff: LPCDIEFFECT; out ppdeff: IDirectInputEffect {LPDIRECTINPUTEFFECT *}; punkOuter: {LPUNKNOWN}IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKW; pvRef: LPVOID; dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(pdei: LPDIEFFECTINFOW; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function GetForceFeedbackState(pdwOut: LPDWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback: LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: LPVOID; fl: DWORD): HRESULT; stdcall;
    function Escape(pesc: LPDIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(cbObjectData: DWORD; rgdod: LPCDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; fl: DWORD): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice2W *LPDIRECTINPUTDEVICE2W;
  LPDIRECTINPUTDEVICE2W = IDirectInputDevice2W;

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice2A

  IDirectInputDevice2A = interface(IDirectInputDeviceA)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDeviceA methods ***)
    {STDMETHOD(GetCapabilities)(THIS_ LPDIDEVCAPS) PURE;
    STDMETHOD(EnumObjects)(THIS_ LPDIENUMDEVICEOBJECTSCALLBACKA,LPVOID,DWORD) PURE;
    STDMETHOD(GetProperty)(THIS_ REFGUID,LPDIPROPHEADER) PURE;
    STDMETHOD(SetProperty)(THIS_ REFGUID,LPCDIPROPHEADER) PURE;
    STDMETHOD(Acquire)(THIS) PURE;
    STDMETHOD(Unacquire)(THIS) PURE;
    STDMETHOD(GetDeviceState)(THIS_ DWORD,LPVOID) PURE;
    STDMETHOD(GetDeviceData)(THIS_ DWORD,LPDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;
    STDMETHOD(SetDataFormat)(THIS_ LPCDIDATAFORMAT) PURE;
    STDMETHOD(SetEventNotification)(THIS_ HANDLE) PURE;
    STDMETHOD(SetCooperativeLevel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(GetObjectInfo)(THIS_ LPDIDEVICEOBJECTINSTANCEA,DWORD,DWORD) PURE;
    STDMETHOD(GetDeviceInfo)(THIS_ LPDIDEVICEINSTANCEA) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD,REFGUID) PURE;}

    (*** IDirectInputDevice2A methods ***)
    function CreateEffect(const rguid: TGuid {REFGUID}; lpeff: LPCDIEFFECT; out ppdeff: IDirectInputEffect {LPDIRECTINPUTEFFECT *}; punkOuter: {LPUNKNOWN}IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKA; pvRef: LPVOID; dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(pdei: LPDIEFFECTINFOA; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function GetForceFeedbackState(pdwOut: LPDWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback: LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: LPVOID; fl: DWORD): HRESULT; stdcall;
    function Escape(pesc: LPDIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(cbObjectData: DWORD; rgdod: LPCDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; fl: DWORD): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice2A *LPDIRECTINPUTDEVICE2A;
  LPDIRECTINPUTDEVICE2A = IDirectInputDevice2A;

{$IFDEF UNICODE}
const
  IID_IDirectInputDevice2: TIID = '{5944E683-C92E-11CF-BFC7-444553540000}'{IID_IDirectInputDevice2W};
type
  IDirectInputDevice2 = IDirectInputDevice2W;
{$ELSE}
const
  IID_IDirectInputDevice2: TIID = '{5944E682-C92E-11CF-BFC7-444553540000}'{IID_IDirectInputDevice2A};
type
  IDirectInputDevice2 = IDirectInputDevice2A;
{$ENDIF}
//typedef struct IDirectInputDevice2 *LPDIRECTINPUTDEVICE2;
  LPDIRECTINPUTDEVICE2 = IDirectInputDevice2;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInputDevice2_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInputDevice2_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInputDevice2_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInputDevice2_GetCapabilities(p,a) (p)->lpVtbl->GetCapabilities(p,a)
//#define IDirectInputDevice2_EnumObjects(p,a,b,c) (p)->lpVtbl->EnumObjects(p,a,b,c)
//#define IDirectInputDevice2_GetProperty(p,a,b) (p)->lpVtbl->GetProperty(p,a,b)
//#define IDirectInputDevice2_SetProperty(p,a,b) (p)->lpVtbl->SetProperty(p,a,b)
//#define IDirectInputDevice2_Acquire(p) (p)->lpVtbl->Acquire(p)
//#define IDirectInputDevice2_Unacquire(p) (p)->lpVtbl->Unacquire(p)
//#define IDirectInputDevice2_GetDeviceState(p,a,b) (p)->lpVtbl->GetDeviceState(p,a,b)
//#define IDirectInputDevice2_GetDeviceData(p,a,b,c,d) (p)->lpVtbl->GetDeviceData(p,a,b,c,d)
//#define IDirectInputDevice2_SetDataFormat(p,a) (p)->lpVtbl->SetDataFormat(p,a)
//#define IDirectInputDevice2_SetEventNotification(p,a) (p)->lpVtbl->SetEventNotification(p,a)
//#define IDirectInputDevice2_SetCooperativeLevel(p,a,b) (p)->lpVtbl->SetCooperativeLevel(p,a,b)
//#define IDirectInputDevice2_GetObjectInfo(p,a,b,c) (p)->lpVtbl->GetObjectInfo(p,a,b,c)
//#define IDirectInputDevice2_GetDeviceInfo(p,a) (p)->lpVtbl->GetDeviceInfo(p,a)
//#define IDirectInputDevice2_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInputDevice2_Initialize(p,a,b,c) (p)->lpVtbl->Initialize(p,a,b,c)
//#define IDirectInputDevice2_CreateEffect(p,a,b,c,d) (p)->lpVtbl->CreateEffect(p,a,b,c,d)
//#define IDirectInputDevice2_EnumEffects(p,a,b,c) (p)->lpVtbl->EnumEffects(p,a,b,c)
//#define IDirectInputDevice2_GetEffectInfo(p,a,b) (p)->lpVtbl->GetEffectInfo(p,a,b)
//#define IDirectInputDevice2_GetForceFeedbackState(p,a) (p)->lpVtbl->GetForceFeedbackState(p,a)
//#define IDirectInputDevice2_SendForceFeedbackCommand(p,a) (p)->lpVtbl->SendForceFeedbackCommand(p,a)
//#define IDirectInputDevice2_EnumCreatedEffectObjects(p,a,b,c) (p)->lpVtbl->EnumCreatedEffectObjects(p,a,b,c)
//#define IDirectInputDevice2_Escape(p,a) (p)->lpVtbl->Escape(p,a)
//#define IDirectInputDevice2_Poll(p) (p)->lpVtbl->Poll(p)
//#define IDirectInputDevice2_SendDeviceData(p,a,b,c,d) (p)->lpVtbl->SendDeviceData(p,a,b,c,d)
//#else
//#define IDirectInputDevice2_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInputDevice2_AddRef(p) (p)->AddRef()
//#define IDirectInputDevice2_Release(p) (p)->Release()
//#define IDirectInputDevice2_GetCapabilities(p,a) (p)->GetCapabilities(a)
//#define IDirectInputDevice2_EnumObjects(p,a,b,c) (p)->EnumObjects(a,b,c)
//#define IDirectInputDevice2_GetProperty(p,a,b) (p)->GetProperty(a,b)
//#define IDirectInputDevice2_SetProperty(p,a,b) (p)->SetProperty(a,b)
//#define IDirectInputDevice2_Acquire(p) (p)->Acquire()
//#define IDirectInputDevice2_Unacquire(p) (p)->Unacquire()
//#define IDirectInputDevice2_GetDeviceState(p,a,b) (p)->GetDeviceState(a,b)
//#define IDirectInputDevice2_GetDeviceData(p,a,b,c,d) (p)->GetDeviceData(a,b,c,d)
//#define IDirectInputDevice2_SetDataFormat(p,a) (p)->SetDataFormat(a)
//#define IDirectInputDevice2_SetEventNotification(p,a) (p)->SetEventNotification(a)
//#define IDirectInputDevice2_SetCooperativeLevel(p,a,b) (p)->SetCooperativeLevel(a,b)
//#define IDirectInputDevice2_GetObjectInfo(p,a,b,c) (p)->GetObjectInfo(a,b,c)
//#define IDirectInputDevice2_GetDeviceInfo(p,a) (p)->GetDeviceInfo(a)
//#define IDirectInputDevice2_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInputDevice2_Initialize(p,a,b,c) (p)->Initialize(a,b,c)
//#define IDirectInputDevice2_CreateEffect(p,a,b,c,d) (p)->CreateEffect(a,b,c,d)
//#define IDirectInputDevice2_EnumEffects(p,a,b,c) (p)->EnumEffects(a,b,c)
//#define IDirectInputDevice2_GetEffectInfo(p,a,b) (p)->GetEffectInfo(a,b)
//#define IDirectInputDevice2_GetForceFeedbackState(p,a) (p)->GetForceFeedbackState(a)
//#define IDirectInputDevice2_SendForceFeedbackCommand(p,a) (p)->SendForceFeedbackCommand(a)
//#define IDirectInputDevice2_EnumCreatedEffectObjects(p,a,b,c) (p)->EnumCreatedEffectObjects(a,b,c)
//#define IDirectInputDevice2_Escape(p,a) (p)->Escape(a)
//#define IDirectInputDevice2_Poll(p) (p)->Poll()
//#define IDirectInputDevice2_SendDeviceData(p,a,b,c,d) (p)->SendDeviceData(a,b,c,d)
//#endif

//#endif /* DIJ_RINGZERO */

{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }

{$IF DIRECTINPUT_VERSION >= $0700}
const
  DIFEF_DEFAULT             = $00000000;
  DIFEF_INCLUDENONSTANDARD  = $00000001;
  DIFEF_MODIFYIFNEEDED      = $00000010;

//#ifndef DIJ_RINGZERO

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice7W

type
  IDirectInputDevice7W = interface(IDirectInputDevice2W)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDevice2W methods ***)
    {STDMETHOD(GetCapabilities)(THIS_ LPDIDEVCAPS) PURE;
    STDMETHOD(EnumObjects)(THIS_ LPDIENUMDEVICEOBJECTSCALLBACKW,LPVOID,DWORD) PURE;
    STDMETHOD(GetProperty)(THIS_ REFGUID,LPDIPROPHEADER) PURE;
    STDMETHOD(SetProperty)(THIS_ REFGUID,LPCDIPROPHEADER) PURE;
    STDMETHOD(Acquire)(THIS) PURE;
    STDMETHOD(Unacquire)(THIS) PURE;
    STDMETHOD(GetDeviceState)(THIS_ DWORD,LPVOID) PURE;
    STDMETHOD(GetDeviceData)(THIS_ DWORD,LPDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;
    STDMETHOD(SetDataFormat)(THIS_ LPCDIDATAFORMAT) PURE;
    STDMETHOD(SetEventNotification)(THIS_ HANDLE) PURE;
    STDMETHOD(SetCooperativeLevel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(GetObjectInfo)(THIS_ LPDIDEVICEOBJECTINSTANCEW,DWORD,DWORD) PURE;
    STDMETHOD(GetDeviceInfo)(THIS_ LPDIDEVICEINSTANCEW) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD,REFGUID) PURE;
    STDMETHOD(CreateEffect)(THIS_ REFGUID,LPCDIEFFECT,LPDIRECTINPUTEFFECT *,LPUNKNOWN) PURE;
    STDMETHOD(EnumEffects)(THIS_ LPDIENUMEFFECTSCALLBACKW,LPVOID,DWORD) PURE;
    STDMETHOD(GetEffectInfo)(THIS_ LPDIEFFECTINFOW,REFGUID) PURE;
    STDMETHOD(GetForceFeedbackState)(THIS_ LPDWORD) PURE;
    STDMETHOD(SendForceFeedbackCommand)(THIS_ DWORD) PURE;
    STDMETHOD(EnumCreatedEffectObjects)(THIS_ LPDIENUMCREATEDEFFECTOBJECTSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(Escape)(THIS_ LPDIEFFESCAPE) PURE;
    STDMETHOD(Poll)(THIS) PURE;
    STDMETHOD(SendDeviceData)(THIS_ DWORD,LPCDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;}

    (*** IDirectInputDevice7W methods ***)
    function EnumEffectsInFile(lpszFileName: LPCWSTR; pec: LPDIENUMEFFECTSINFILECALLBACK; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function WriteEffectToFile(lpszFileName: LPCWSTR; dwEntries: DWORD; rgDiFileEft: LPDIFILEEFFECT; dwFlags: DWORD): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice7W *LPDIRECTINPUTDEVICE7W;
  LPDIRECTINPUTDEVICE7W = IDirectInputDevice7W;

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice7A

  IDirectInputDevice7A = interface(IDirectInputDevice2A)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDevice2A methods ***)
    {STDMETHOD(GetCapabilities)(THIS_ LPDIDEVCAPS) PURE;
    STDMETHOD(EnumObjects)(THIS_ LPDIENUMDEVICEOBJECTSCALLBACKA,LPVOID,DWORD) PURE;
    STDMETHOD(GetProperty)(THIS_ REFGUID,LPDIPROPHEADER) PURE;
    STDMETHOD(SetProperty)(THIS_ REFGUID,LPCDIPROPHEADER) PURE;
    STDMETHOD(Acquire)(THIS) PURE;
    STDMETHOD(Unacquire)(THIS) PURE;
    STDMETHOD(GetDeviceState)(THIS_ DWORD,LPVOID) PURE;
    STDMETHOD(GetDeviceData)(THIS_ DWORD,LPDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;
    STDMETHOD(SetDataFormat)(THIS_ LPCDIDATAFORMAT) PURE;
    STDMETHOD(SetEventNotification)(THIS_ HANDLE) PURE;
    STDMETHOD(SetCooperativeLevel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(GetObjectInfo)(THIS_ LPDIDEVICEOBJECTINSTANCEA,DWORD,DWORD) PURE;
    STDMETHOD(GetDeviceInfo)(THIS_ LPDIDEVICEINSTANCEA) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD,REFGUID) PURE;
    STDMETHOD(CreateEffect)(THIS_ REFGUID,LPCDIEFFECT,LPDIRECTINPUTEFFECT *,LPUNKNOWN) PURE;
    STDMETHOD(EnumEffects)(THIS_ LPDIENUMEFFECTSCALLBACKA,LPVOID,DWORD) PURE;
    STDMETHOD(GetEffectInfo)(THIS_ LPDIEFFECTINFOA,REFGUID) PURE;
    STDMETHOD(GetForceFeedbackState)(THIS_ LPDWORD) PURE;
    STDMETHOD(SendForceFeedbackCommand)(THIS_ DWORD) PURE;
    STDMETHOD(EnumCreatedEffectObjects)(THIS_ LPDIENUMCREATEDEFFECTOBJECTSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(Escape)(THIS_ LPDIEFFESCAPE) PURE;
    STDMETHOD(Poll)(THIS) PURE;
    STDMETHOD(SendDeviceData)(THIS_ DWORD,LPCDIDEVICEOBJECTDATA,LPDWORD,DWORD) PURE;}

    (*** IDirectInputDevice7A methods ***)
    function EnumEffectsInFile(lpszFileName: LPCSTR; pec: LPDIENUMEFFECTSINFILECALLBACK; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function WriteEffectToFile(lpszFileName: LPCSTR; dwEntries: DWORD; rgDiFileEft: LPDIFILEEFFECT; dwFlags: DWORD): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice7A *LPDIRECTINPUTDEVICE7A;
  LPDIRECTINPUTDEVICE7A = IDirectInputDevice7A;

{$IFDEF UNICODE}
const
  IID_IDirectInputDevice7: TIID = '{57D7C6BD-2356-11D3-8E9D-00C04F6844AE}'{IID_IDirectInputDevice7W};
type
  IDirectInputDevice7 = IDirectInputDevice7W;
{$ELSE}
const
  IID_IDirectInputDevice7: TIID = '{57D7C6BC-2356-11D3-8E9D-00C04F6844AE}'{IID_IDirectInputDevice7A};
type
  IDirectInputDevice7 = IDirectInputDevice7A;
{$ENDIF}
//typedef struct IDirectInputDevice7 *LPDIRECTINPUTDEVICE7;
  LPDIRECTINPUTDEVICE7 = IDirectInputDevice7;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInputDevice7_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInputDevice7_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInputDevice7_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInputDevice7_GetCapabilities(p,a) (p)->lpVtbl->GetCapabilities(p,a)
//#define IDirectInputDevice7_EnumObjects(p,a,b,c) (p)->lpVtbl->EnumObjects(p,a,b,c)
//#define IDirectInputDevice7_GetProperty(p,a,b) (p)->lpVtbl->GetProperty(p,a,b)
//#define IDirectInputDevice7_SetProperty(p,a,b) (p)->lpVtbl->SetProperty(p,a,b)
//#define IDirectInputDevice7_Acquire(p) (p)->lpVtbl->Acquire(p)
//#define IDirectInputDevice7_Unacquire(p) (p)->lpVtbl->Unacquire(p)
//#define IDirectInputDevice7_GetDeviceState(p,a,b) (p)->lpVtbl->GetDeviceState(p,a,b)
//#define IDirectInputDevice7_GetDeviceData(p,a,b,c,d) (p)->lpVtbl->GetDeviceData(p,a,b,c,d)
//#define IDirectInputDevice7_SetDataFormat(p,a) (p)->lpVtbl->SetDataFormat(p,a)
//#define IDirectInputDevice7_SetEventNotification(p,a) (p)->lpVtbl->SetEventNotification(p,a)
//#define IDirectInputDevice7_SetCooperativeLevel(p,a,b) (p)->lpVtbl->SetCooperativeLevel(p,a,b)
//#define IDirectInputDevice7_GetObjectInfo(p,a,b,c) (p)->lpVtbl->GetObjectInfo(p,a,b,c)
//#define IDirectInputDevice7_GetDeviceInfo(p,a) (p)->lpVtbl->GetDeviceInfo(p,a)
//#define IDirectInputDevice7_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInputDevice7_Initialize(p,a,b,c) (p)->lpVtbl->Initialize(p,a,b,c)
//#define IDirectInputDevice7_CreateEffect(p,a,b,c,d) (p)->lpVtbl->CreateEffect(p,a,b,c,d)
//#define IDirectInputDevice7_EnumEffects(p,a,b,c) (p)->lpVtbl->EnumEffects(p,a,b,c)
//#define IDirectInputDevice7_GetEffectInfo(p,a,b) (p)->lpVtbl->GetEffectInfo(p,a,b)
//#define IDirectInputDevice7_GetForceFeedbackState(p,a) (p)->lpVtbl->GetForceFeedbackState(p,a)
//#define IDirectInputDevice7_SendForceFeedbackCommand(p,a) (p)->lpVtbl->SendForceFeedbackCommand(p,a)
//#define IDirectInputDevice7_EnumCreatedEffectObjects(p,a,b,c) (p)->lpVtbl->EnumCreatedEffectObjects(p,a,b,c)
//#define IDirectInputDevice7_Escape(p,a) (p)->lpVtbl->Escape(p,a)
//#define IDirectInputDevice7_Poll(p) (p)->lpVtbl->Poll(p)
//#define IDirectInputDevice7_SendDeviceData(p,a,b,c,d) (p)->lpVtbl->SendDeviceData(p,a,b,c,d)
//#define IDirectInputDevice7_EnumEffectsInFile(p,a,b,c,d) (p)->lpVtbl->EnumEffectsInFile(p,a,b,c,d)
//#define IDirectInputDevice7_WriteEffectToFile(p,a,b,c,d) (p)->lpVtbl->WriteEffectToFile(p,a,b,c,d)
//#else
//#define IDirectInputDevice7_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInputDevice7_AddRef(p) (p)->AddRef()
//#define IDirectInputDevice7_Release(p) (p)->Release()
//#define IDirectInputDevice7_GetCapabilities(p,a) (p)->GetCapabilities(a)
//#define IDirectInputDevice7_EnumObjects(p,a,b,c) (p)->EnumObjects(a,b,c)
//#define IDirectInputDevice7_GetProperty(p,a,b) (p)->GetProperty(a,b)
//#define IDirectInputDevice7_SetProperty(p,a,b) (p)->SetProperty(a,b)
//#define IDirectInputDevice7_Acquire(p) (p)->Acquire()
//#define IDirectInputDevice7_Unacquire(p) (p)->Unacquire()
//#define IDirectInputDevice7_GetDeviceState(p,a,b) (p)->GetDeviceState(a,b)
//#define IDirectInputDevice7_GetDeviceData(p,a,b,c,d) (p)->GetDeviceData(a,b,c,d)
//#define IDirectInputDevice7_SetDataFormat(p,a) (p)->SetDataFormat(a)
//#define IDirectInputDevice7_SetEventNotification(p,a) (p)->SetEventNotification(a)
//#define IDirectInputDevice7_SetCooperativeLevel(p,a,b) (p)->SetCooperativeLevel(a,b)
//#define IDirectInputDevice7_GetObjectInfo(p,a,b,c) (p)->GetObjectInfo(a,b,c)
//#define IDirectInputDevice7_GetDeviceInfo(p,a) (p)->GetDeviceInfo(a)
//#define IDirectInputDevice7_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInputDevice7_Initialize(p,a,b,c) (p)->Initialize(a,b,c)
//#define IDirectInputDevice7_CreateEffect(p,a,b,c,d) (p)->CreateEffect(a,b,c,d)
//#define IDirectInputDevice7_EnumEffects(p,a,b,c) (p)->EnumEffects(a,b,c)
//#define IDirectInputDevice7_GetEffectInfo(p,a,b) (p)->GetEffectInfo(a,b)
//#define IDirectInputDevice7_GetForceFeedbackState(p,a) (p)->GetForceFeedbackState(a)
//#define IDirectInputDevice7_SendForceFeedbackCommand(p,a) (p)->SendForceFeedbackCommand(a)
//#define IDirectInputDevice7_EnumCreatedEffectObjects(p,a,b,c) (p)->EnumCreatedEffectObjects(a,b,c)
//#define IDirectInputDevice7_Escape(p,a) (p)->Escape(a)
//#define IDirectInputDevice7_Poll(p) (p)->Poll()
//#define IDirectInputDevice7_SendDeviceData(p,a,b,c,d) (p)->SendDeviceData(a,b,c,d)
//#define IDirectInputDevice7_EnumEffectsInFile(p,a,b,c,d) (p)->EnumEffectsInFile(a,b,c,d)
//#define IDirectInputDevice7_WriteEffectToFile(p,a,b,c,d) (p)->WriteEffectToFile(a,b,c,d)
//#endif

//#endif /* DIJ_RINGZERO */

{$ENDIF} { DIRECTINPUT_VERSION >= $0700 }

{$IF DIRECTINPUT_VERSION >= $0800}

//#ifndef DIJ_RINGZERO

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice8W

type
  IDirectInputDevice8W = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDevice8W methods ***)
    function GetCapabilities(lpDIDevCaps: LPDIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKW; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPDIPROPHEADER): HRESULT; stdcall;
    function SetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPCDIPROPHEADER): HRESULT; stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: LPVOID): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: LPDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(lpdf: LPCDIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: HANDLE): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(pdidoi: LPDIDEVICEOBJECTINSTANCEW; dwObj: DWORD; dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(pdidi: LPDIDEVICEINSTANCEW): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function CreateEffect(const rguid: TGuid {REFGUID}; lpeff: LPCDIEFFECT; out ppdeff: IDirectInputEffect {LPDIRECTINPUTEFFECT *}; punkOuter: {LPUNKNOWN}IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKW; pvRef: LPVOID; dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(pdei: LPDIEFFECTINFOW; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function GetForceFeedbackState(pdwOut: LPDWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback: LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: LPVOID; fl: DWORD): HRESULT; stdcall;
    function Escape(pesc: LPDIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(cbObjectData: DWORD; rgdod: LPCDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; fl: DWORD): HRESULT; stdcall;
    function EnumEffectsInFile(lpszFileName: LPCWSTR; pec: LPDIENUMEFFECTSINFILECALLBACK; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function WriteEffectToFile(lpszFileName: LPCWSTR; dwEntries: DWORD; rgDiFileEft: LPDIFILEEFFECT; dwFlags: DWORD): HRESULT; stdcall;
    function BuildActionMap(lpdiaf: LPDIACTIONFORMATW; lpszUserName: LPCWSTR; dwFlags: DWORD): HRESULT; stdcall;
    function SetActionMap(lpdiActionFormat: LPDIACTIONFORMATW; lptszUserName: LPCWSTR; dwFlags: DWORD): HRESULT; stdcall;
    function GetImageInfo(lpdiDevImageInfoHeader: LPDIDEVICEIMAGEINFOHEADERW): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice8W *LPDIRECTINPUTDEVICE8W;
  LPDIRECTINPUTDEVICE8W = IDirectInputDevice8W;

//#undef INTERFACE
//#define INTERFACE IDirectInputDevice8A

  IDirectInputDevice8A = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputDevice8A methods ***)
    function GetCapabilities(lpDIDevCaps: LPDIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKA; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPDIPROPHEADER): HRESULT; stdcall;
    function SetProperty(const rguidProp: TGuid {REFGUID}; pdiph: LPCDIPROPHEADER): HRESULT; stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: LPVOID): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: LPDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(lpdf: LPCDIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: HANDLE): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(pdidoi: LPDIDEVICEOBJECTINSTANCEA; dwObj: DWORD; dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(pdidi: LPDIDEVICEINSTANCEA): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function CreateEffect(const rguid: TGuid {REFGUID}; lpeff: LPCDIEFFECT;  out ppdeff: IDirectInputEffect {LPDIRECTINPUTEFFECT *}; punkOuter: {LPUNKNOWN}IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKA; pvRef: LPVOID; dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(pdei: LPDIEFFECTINFOA; const rguid: TGuid {REFGUID}): HRESULT; stdcall;
    function GetForceFeedbackState(pdwOut: LPDWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback: LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: LPVOID; fl: DWORD): HRESULT; stdcall;
    function Escape(pesc: LPDIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(cbObjectData: DWORD; rgdod: LPCDIDEVICEOBJECTDATA; pdwInOut: LPDWORD; fl: DWORD): HRESULT; stdcall;
    function EnumEffectsInFile(lpszFileName: LPCSTR; pec: LPDIENUMEFFECTSINFILECALLBACK; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function WriteEffectToFile(lpszFileName: LPCSTR; dwEntries: DWORD; rgDiFileEft: LPDIFILEEFFECT; dwFlags: DWORD): HRESULT; stdcall;
    function BuildActionMap(lpdiaf: LPDIACTIONFORMATA; lpszUserName: LPCSTR; dwFlags: DWORD): HRESULT; stdcall;
    function SetActionMap(lpdiActionFormat: LPDIACTIONFORMATA; lptszUserName: LPCSTR; dwFlags: DWORD): HRESULT; stdcall;
    function GetImageInfo(lpdiDevImageInfoHeader: LPDIDEVICEIMAGEINFOHEADERA): HRESULT; stdcall;
  end;

//typedef struct IDirectInputDevice8A *LPDIRECTINPUTDEVICE8A;
  LPDIRECTINPUTDEVICE8A = IDirectInputDevice8A;

{$IFDEF UNICODE}
const
  IID_IDirectInputDevice8: TIID = '{54D41081-DC15-4833-A41B-748F73A38179}'{IID_IDirectInputDevice8W};
type
  IDirectInputDevice8 = IDirectInputDevice8W;
{$ELSE}
const
  IID_IDirectInputDevice8: TIID = '{54D41080-DC15-4833-A41B-748F73A38179}'{IID_IDirectInputDevice8A};
type
  IDirectInputDevice8 = IDirectInputDevice8A;
{$ENDIF}
//typedef struct IDirectInputDevice8 *LPDIRECTINPUTDEVICE8;
  LPDIRECTINPUTDEVICE8 = IDirectInputDevice8;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInputDevice8_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInputDevice8_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInputDevice8_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInputDevice8_GetCapabilities(p,a) (p)->lpVtbl->GetCapabilities(p,a)
//#define IDirectInputDevice8_EnumObjects(p,a,b,c) (p)->lpVtbl->EnumObjects(p,a,b,c)
//#define IDirectInputDevice8_GetProperty(p,a,b) (p)->lpVtbl->GetProperty(p,a,b)
//#define IDirectInputDevice8_SetProperty(p,a,b) (p)->lpVtbl->SetProperty(p,a,b)
//#define IDirectInputDevice8_Acquire(p) (p)->lpVtbl->Acquire(p)
//#define IDirectInputDevice8_Unacquire(p) (p)->lpVtbl->Unacquire(p)
//#define IDirectInputDevice8_GetDeviceState(p,a,b) (p)->lpVtbl->GetDeviceState(p,a,b)
//#define IDirectInputDevice8_GetDeviceData(p,a,b,c,d) (p)->lpVtbl->GetDeviceData(p,a,b,c,d)
//#define IDirectInputDevice8_SetDataFormat(p,a) (p)->lpVtbl->SetDataFormat(p,a)
//#define IDirectInputDevice8_SetEventNotification(p,a) (p)->lpVtbl->SetEventNotification(p,a)
//#define IDirectInputDevice8_SetCooperativeLevel(p,a,b) (p)->lpVtbl->SetCooperativeLevel(p,a,b)
//#define IDirectInputDevice8_GetObjectInfo(p,a,b,c) (p)->lpVtbl->GetObjectInfo(p,a,b,c)
//#define IDirectInputDevice8_GetDeviceInfo(p,a) (p)->lpVtbl->GetDeviceInfo(p,a)
//#define IDirectInputDevice8_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInputDevice8_Initialize(p,a,b,c) (p)->lpVtbl->Initialize(p,a,b,c)
//#define IDirectInputDevice8_CreateEffect(p,a,b,c,d) (p)->lpVtbl->CreateEffect(p,a,b,c,d)
//#define IDirectInputDevice8_EnumEffects(p,a,b,c) (p)->lpVtbl->EnumEffects(p,a,b,c)
//#define IDirectInputDevice8_GetEffectInfo(p,a,b) (p)->lpVtbl->GetEffectInfo(p,a,b)
//#define IDirectInputDevice8_GetForceFeedbackState(p,a) (p)->lpVtbl->GetForceFeedbackState(p,a)
//#define IDirectInputDevice8_SendForceFeedbackCommand(p,a) (p)->lpVtbl->SendForceFeedbackCommand(p,a)
//#define IDirectInputDevice8_EnumCreatedEffectObjects(p,a,b,c) (p)->lpVtbl->EnumCreatedEffectObjects(p,a,b,c)
//#define IDirectInputDevice8_Escape(p,a) (p)->lpVtbl->Escape(p,a)
//#define IDirectInputDevice8_Poll(p) (p)->lpVtbl->Poll(p)
//#define IDirectInputDevice8_SendDeviceData(p,a,b,c,d) (p)->lpVtbl->SendDeviceData(p,a,b,c,d)
//#define IDirectInputDevice8_EnumEffectsInFile(p,a,b,c,d) (p)->lpVtbl->EnumEffectsInFile(p,a,b,c,d)
//#define IDirectInputDevice8_WriteEffectToFile(p,a,b,c,d) (p)->lpVtbl->WriteEffectToFile(p,a,b,c,d)
//#define IDirectInputDevice8_BuildActionMap(p,a,b,c) (p)->lpVtbl->BuildActionMap(p,a,b,c)
//#define IDirectInputDevice8_SetActionMap(p,a,b,c) (p)->lpVtbl->SetActionMap(p,a,b,c)
//#define IDirectInputDevice8_GetImageInfo(p,a) (p)->lpVtbl->GetImageInfo(p,a)
//#else
//#define IDirectInputDevice8_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInputDevice8_AddRef(p) (p)->AddRef()
//#define IDirectInputDevice8_Release(p) (p)->Release()
//#define IDirectInputDevice8_GetCapabilities(p,a) (p)->GetCapabilities(a)
//#define IDirectInputDevice8_EnumObjects(p,a,b,c) (p)->EnumObjects(a,b,c)
//#define IDirectInputDevice8_GetProperty(p,a,b) (p)->GetProperty(a,b)
//#define IDirectInputDevice8_SetProperty(p,a,b) (p)->SetProperty(a,b)
//#define IDirectInputDevice8_Acquire(p) (p)->Acquire()
//#define IDirectInputDevice8_Unacquire(p) (p)->Unacquire()
//#define IDirectInputDevice8_GetDeviceState(p,a,b) (p)->GetDeviceState(a,b)
//#define IDirectInputDevice8_GetDeviceData(p,a,b,c,d) (p)->GetDeviceData(a,b,c,d)
//#define IDirectInputDevice8_SetDataFormat(p,a) (p)->SetDataFormat(a)
//#define IDirectInputDevice8_SetEventNotification(p,a) (p)->SetEventNotification(a)
//#define IDirectInputDevice8_SetCooperativeLevel(p,a,b) (p)->SetCooperativeLevel(a,b)
//#define IDirectInputDevice8_GetObjectInfo(p,a,b,c) (p)->GetObjectInfo(a,b,c)
//#define IDirectInputDevice8_GetDeviceInfo(p,a) (p)->GetDeviceInfo(a)
//#define IDirectInputDevice8_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInputDevice8_Initialize(p,a,b,c) (p)->Initialize(a,b,c)
//#define IDirectInputDevice8_CreateEffect(p,a,b,c,d) (p)->CreateEffect(a,b,c,d)
//#define IDirectInputDevice8_EnumEffects(p,a,b,c) (p)->EnumEffects(a,b,c)
//#define IDirectInputDevice8_GetEffectInfo(p,a,b) (p)->GetEffectInfo(a,b)
//#define IDirectInputDevice8_GetForceFeedbackState(p,a) (p)->GetForceFeedbackState(a)
//#define IDirectInputDevice8_SendForceFeedbackCommand(p,a) (p)->SendForceFeedbackCommand(a)
//#define IDirectInputDevice8_EnumCreatedEffectObjects(p,a,b,c) (p)->EnumCreatedEffectObjects(a,b,c)
//#define IDirectInputDevice8_Escape(p,a) (p)->Escape(a)
//#define IDirectInputDevice8_Poll(p) (p)->Poll()
//#define IDirectInputDevice8_SendDeviceData(p,a,b,c,d) (p)->SendDeviceData(a,b,c,d)
//#define IDirectInputDevice8_EnumEffectsInFile(p,a,b,c,d) (p)->EnumEffectsInFile(a,b,c,d)
//#define IDirectInputDevice8_WriteEffectToFile(p,a,b,c,d) (p)->WriteEffectToFile(a,b,c,d)
//#define IDirectInputDevice8_BuildActionMap(p,a,b,c) (p)->BuildActionMap(a,b,c)
//#define IDirectInputDevice8_SetActionMap(p,a,b,c) (p)->SetActionMap(a,b,c)
//#define IDirectInputDevice8_GetImageInfo(p,a) (p)->GetImageInfo(a)
//#endif

//#endif /* DIJ_RINGZERO */

{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

(****************************************************************************
 *
 *      Mouse
 *
 ****************************************************************************)

//#ifndef DIJ_RINGZERO

type
  LPDIMOUSESTATE = ^TDIMOUSESTATE;
  PDIMOUSESTATE = ^TDIMOUSESTATE;
  TDIMOUSESTATE = record
    lX: LONG;
    lY: LONG;
    lZ: LONG;
    rgbButtons: array [0..3] of BYTE;
  end;

{$IF DIRECTINPUT_VERSION >= $0700}
  LPDIMOUSESTATE2 = ^TDIMOUSESTATE2;
  PDIMOUSESTATE2 = ^TDIMOUSESTATE2;
  TDIMOUSESTATE2 = record
    lX: LONG;
    lY: LONG;
    lZ: LONG;
    rgbButtons: array [0..7] of BYTE;
  end;
{$ENDIF}


//const
//  DIMOFS_X: LONG       =  Ofs(PDIMOUSESTATE(nil)^.lX); { FIELD_OFFSET(DIMOUSESTATE, lX)}
{  DIMOFS_Y       =  FIELD_OFFSET(DIMOUSESTATE, lY);
  DIMOFS_Z       =  FIELD_OFFSET(DIMOUSESTATE, lZ);
  DIMOFS_BUTTON0 = (FIELD_OFFSET(DIMOUSESTATE, rgbButtons) + 0);
  DIMOFS_BUTTON1 = (FIELD_OFFSET(DIMOUSESTATE, rgbButtons) + 1);
  DIMOFS_BUTTON2 = (FIELD_OFFSET(DIMOUSESTATE, rgbButtons) + 2);
  DIMOFS_BUTTON3 = (FIELD_OFFSET(DIMOUSESTATE, rgbButtons) + 3);}
{$IF DIRECTINPUT_VERSION >= $0700}
{  DIMOFS_BUTTON4 = (FIELD_OFFSET(DIMOUSESTATE2, rgbButtons) + 4);
  DIMOFS_BUTTON5 = (FIELD_OFFSET(DIMOUSESTATE2, rgbButtons) + 5);
  DIMOFS_BUTTON6 = (FIELD_OFFSET(DIMOUSESTATE2, rgbButtons) + 6);
  DIMOFS_BUTTON7 = (FIELD_OFFSET(DIMOUSESTATE2, rgbButtons) + 7);}
{$ENDIF}
//#endif /* DIJ_RINGZERO */

(****************************************************************************
 *
 *      Keyboard
 *
 ****************************************************************************)

//#ifndef DIJ_RINGZERO

(****************************************************************************
 *
 *      DirectInput keyboard scan codes
 *
 ****************************************************************************)
//
//    Copyright (C) Microsoft.  All rights reserved.
//
const
  DIK_ESCAPE          = $01;
  DIK_1               = $02;
  DIK_2               = $03;
  DIK_3               = $04;
  DIK_4               = $05;
  DIK_5               = $06;
  DIK_6               = $07;
  DIK_7               = $08;
  DIK_8               = $09;
  DIK_9               = $0A;
  DIK_0               = $0B;
  DIK_MINUS           = $0C;    { - on main keyboard }
  DIK_EQUALS          = $0D;
  DIK_BACK            = $0E;    { backspace }
  DIK_TAB             = $0F;
  DIK_Q               = $10;
  DIK_W               = $11;
  DIK_E               = $12;
  DIK_R               = $13;
  DIK_T               = $14;
  DIK_Y               = $15;
  DIK_U               = $16;
  DIK_I               = $17;
  DIK_O               = $18;
  DIK_P               = $19;
  DIK_LBRACKET        = $1A;
  DIK_RBRACKET        = $1B;
  DIK_RETURN          = $1C;    { Enter on main keyboard }
  DIK_LCONTROL        = $1D;
  DIK_A               = $1E;
  DIK_S               = $1F;
  DIK_D               = $20;
  DIK_F               = $21;
  DIK_G               = $22;
  DIK_H               = $23;
  DIK_J               = $24;
  DIK_K               = $25;
  DIK_L               = $26;
  DIK_SEMICOLON       = $27;
  DIK_APOSTROPHE      = $28;
  DIK_GRAVE           = $29;    { accent grave }
  DIK_LSHIFT          = $2A;
  DIK_BACKSLASH       = $2B;
  DIK_Z               = $2C;
  DIK_X               = $2D;
  DIK_C               = $2E;
  DIK_V               = $2F;
  DIK_B               = $30;
  DIK_N               = $31;
  DIK_M               = $32;
  DIK_COMMA           = $33;
  DIK_PERIOD          = $34;    { . on main keyboard }
  DIK_SLASH           = $35;    { / on main keyboard }
  DIK_RSHIFT          = $36;
  DIK_MULTIPLY        = $37;    { * on numeric keypad }
  DIK_LMENU           = $38;    { left Alt }
  DIK_SPACE           = $39;
  DIK_CAPITAL         = $3A;
  DIK_F1              = $3B;
  DIK_F2              = $3C;
  DIK_F3              = $3D;
  DIK_F4              = $3E;
  DIK_F5              = $3F;
  DIK_F6              = $40;
  DIK_F7              = $41;
  DIK_F8              = $42;
  DIK_F9              = $43;
  DIK_F10             = $44;
  DIK_NUMLOCK         = $45;
  DIK_SCROLL          = $46;    { Scroll Lock }
  DIK_NUMPAD7         = $47;
  DIK_NUMPAD8         = $48;
  DIK_NUMPAD9         = $49;
  DIK_SUBTRACT        = $4A;    { - on numeric keypad }
  DIK_NUMPAD4         = $4B;
  DIK_NUMPAD5         = $4C;
  DIK_NUMPAD6         = $4D;
  DIK_ADD             = $4E;    { + on numeric keypad }
  DIK_NUMPAD1         = $4F;
  DIK_NUMPAD2         = $50;
  DIK_NUMPAD3         = $51;
  DIK_NUMPAD0         = $52;
  DIK_DECIMAL         = $53;    { . on numeric keypad }
  DIK_OEM_102         = $56;    { <> or \| on RT 102-key keyboard (Non-U.S.) }
  DIK_F11             = $57;
  DIK_F12             = $58;
  DIK_F13             = $64;    {                     (NEC PC98) }
  DIK_F14             = $65;    {                     (NEC PC98) }
  DIK_F15             = $66;    {                     (NEC PC98) }
  DIK_KANA            = $70;    { (Japanese keyboard)            }
  DIK_ABNT_C1         = $73;    { /? on Brazilian keyboard }
  DIK_CONVERT         = $79;    { (Japanese keyboard)            }
  DIK_NOCONVERT       = $7B;    { (Japanese keyboard)            }
  DIK_YEN             = $7D;    { (Japanese keyboard)            }
  DIK_ABNT_C2         = $7E;    { Numpad . on Brazilian keyboard }
  DIK_NUMPADEQUALS    = $8D;    { = on numeric keypad (NEC PC98) }
  DIK_PREVTRACK       = $90;    { Previous Track (DIK_CIRCUMFLEX on Japanese keyboard) }
  DIK_AT              = $91;    {                     (NEC PC98) }
  DIK_COLON           = $92;    {                     (NEC PC98) }
  DIK_UNDERLINE       = $93;    {                     (NEC PC98) }
  DIK_KANJI           = $94;    { (Japanese keyboard)            }
  DIK_STOP            = $95;    {                     (NEC PC98) }
  DIK_AX              = $96;    {                     (Japan AX) }
  DIK_UNLABELED       = $97;    {                        (J3100) }
  DIK_NEXTTRACK       = $99;    { Next Track }
  DIK_NUMPADENTER     = $9C;    { Enter on numeric keypad }
  DIK_RCONTROL        = $9D;
  DIK_MUTE            = $A0;    { Mute }
  DIK_CALCULATOR      = $A1;    { Calculator }
  DIK_PLAYPAUSE       = $A2;    { Play / Pause }
  DIK_MEDIASTOP       = $A4;    { Media Stop }
  DIK_VOLUMEDOWN      = $AE;    { Volume - }
  DIK_VOLUMEUP        = $B0;    { Volume + }
  DIK_WEBHOME         = $B2;    { Web home }
  DIK_NUMPADCOMMA     = $B3;    { , on numeric keypad (NEC PC98) }
  DIK_DIVIDE          = $B5;    { / on numeric keypad }
  DIK_SYSRQ           = $B7;
  DIK_RMENU           = $B8;    { right Alt }
  DIK_PAUSE           = $C5;    { Pause }
  DIK_HOME            = $C7;    { Home on arrow keypad }
  DIK_UP              = $C8;    { UpArrow on arrow keypad }
  DIK_PRIOR           = $C9;    { PgUp on arrow keypad }
  DIK_LEFT            = $CB;    { LeftArrow on arrow keypad }
  DIK_RIGHT           = $CD;    { RightArrow on arrow keypad }
  DIK_END             = $CF;    { End on arrow keypad }
  DIK_DOWN            = $D0;    { DownArrow on arrow keypad }
  DIK_NEXT            = $D1;    { PgDn on arrow keypad }
  DIK_INSERT          = $D2;    { Insert on arrow keypad }
  DIK_DELETE          = $D3;    { Delete on arrow keypad }
  DIK_LWIN            = $DB;    { Left Windows key }
  DIK_RWIN            = $DC;    { Right Windows key }
  DIK_APPS            = $DD;    { AppMenu key }
  DIK_POWER           = $DE;    { System Power }
  DIK_SLEEP           = $DF;    { System Sleep }
  DIK_WAKE            = $E3;    { System Wake }
  DIK_WEBSEARCH       = $E5;    { Web Search }
  DIK_WEBFAVORITES    = $E6;    { Web Favorites }
  DIK_WEBREFRESH      = $E7;    { Web Refresh }
  DIK_WEBSTOP         = $E8;    { Web Stop }
  DIK_WEBFORWARD      = $E9;    { Web Forward }
  DIK_WEBBACK         = $EA;    { Web Back }
  DIK_MYCOMPUTER      = $EB;    { My Computer }
  DIK_MAIL            = $EC;    { Mail }
  DIK_MEDIASELECT     = $ED;    { Media Select }

(*
 *  Alternate names for keys, to facilitate transition from DOS.
 *)
  DIK_BACKSPACE       = DIK_BACK;            { backspace }
  DIK_NUMPADSTAR      = DIK_MULTIPLY;        { * on numeric keypad }
  DIK_LALT            = DIK_LMENU;           { left Alt }
  DIK_CAPSLOCK        = DIK_CAPITAL;         { CapsLock }
  DIK_NUMPADMINUS     = DIK_SUBTRACT;        { - on numeric keypad }
  DIK_NUMPADPLUS      = DIK_ADD;             { + on numeric keypad }
  DIK_NUMPADPERIOD    = DIK_DECIMAL;         { . on numeric keypad }
  DIK_NUMPADSLASH     = DIK_DIVIDE;          { / on numeric keypad }
  DIK_RALT            = DIK_RMENU;           { right Alt }
  DIK_UPARROW         = DIK_UP;              { UpArrow on arrow keypad }
  DIK_PGUP            = DIK_PRIOR;           { PgUp on arrow keypad }
  DIK_LEFTARROW       = DIK_LEFT;            { LeftArrow on arrow keypad }
  DIK_RIGHTARROW      = DIK_RIGHT;           { RightArrow on arrow keypad }
  DIK_DOWNARROW       = DIK_DOWN;            { DownArrow on arrow keypad }
  DIK_PGDN            = DIK_NEXT;            { PgDn on arrow keypad }

(*
 *  Alternate names for keys originally not used on US keyboards.
 *)
  DIK_CIRCUMFLEX      = DIK_PREVTRACK;       { Japanese keyboard }

//#endif /* DIJ_RINGZERO */

(****************************************************************************
 *
 *      Joystick
 *
 ****************************************************************************)

//#ifndef DIJ_RINGZERO

type
  LPDIJOYSTATE = ^TDIJOYSTATE;
  PDIJOYSTATE = ^TDIJOYSTATE;
  TDIJOYSTATE = record
    lX: LONG;                            { x-axis position              }
    lY: LONG;                            { y-axis position              }
    lZ: LONG;                            { z-axis position              }
    lRx: LONG;                           { x-axis rotation              }
    lRy: LONG;                           { y-axis rotation              }
    lRz: LONG;                           { z-axis rotation              }
    rglSlider: array [0..1] of LONG;     { extra axes positions         }
    rgdwPOV: array [0..3] of DWORD;      { POV directions               }
    rgbButtons: array [0..31] of BYTE;   { 32 buttons                   }
  end;

  LPDIJOYSTATE2 = ^TDIJOYSTATE2;
  PDIJOYSTATE2 = ^TDIJOYSTATE2;
  TDIJOYSTATE2 = record
    lX: LONG;                            { x-axis position              }
    lY: LONG;                            { y-axis position              }
    lZ: LONG;                            { z-axis position              }
    lRx: LONG;                           { x-axis rotation              }
    lRy: LONG;                           { y-axis rotation              }
    lRz: LONG;                           { z-axis rotation              }
    rglSlider: array [0..1] of LONG;     { extra axes positions         }
    rgdwPOV: array [0..3] of DWORD;      { POV directions               }
    rgbButtons: array [0..127] of BYTE;  { 128 buttons                  }
    lVX: LONG;                           { x-axis velocity              }
    lVY: LONG;                           { y-axis velocity              }
    lVZ: LONG;                           { z-axis velocity              }
    lVRx: LONG;                          { x-axis angular velocity      }
    lVRy: LONG;                          { y-axis angular velocity      }
    lVRz: LONG;                          { z-axis angular velocity      }
    rglVSlider: array [0..1] of LONG;    { extra axes velocities        }
    lAX: LONG;                           { x-axis acceleration          }
    lAY: LONG;                           { y-axis acceleration          }
    lAZ: LONG;                           { z-axis acceleration          }
    lARx: LONG;                          { x-axis angular acceleration  }
    lARy: LONG;                          { y-axis angular acceleration  }
    lARz: LONG;                          { z-axis angular acceleration  }
    rglASlider: array [0..1] of LONG;    { extra axes accelerations     }
    lFX: LONG;                           { x-axis force                 }
    lFY: LONG;                           { y-axis force                 }
    lFZ: LONG;                           { z-axis force                 }
    lFRx: LONG;                          { x-axis torque                }
    lFRy: LONG;                          { y-axis torque                }
    lFRz: LONG;                          { z-axis torque                }
    rglFSlider: array [0..1] of LONG;    { extra axes forces            }
  end;

(*#define DIJOFS_X            FIELD_OFFSET(DIJOYSTATE, lX)
#define DIJOFS_Y            FIELD_OFFSET(DIJOYSTATE, lY)
#define DIJOFS_Z            FIELD_OFFSET(DIJOYSTATE, lZ)
#define DIJOFS_RX           FIELD_OFFSET(DIJOYSTATE, lRx)
#define DIJOFS_RY           FIELD_OFFSET(DIJOYSTATE, lRy)
#define DIJOFS_RZ           FIELD_OFFSET(DIJOYSTATE, lRz)
#define DIJOFS_SLIDER(n)   (FIELD_OFFSET(DIJOYSTATE, rglSlider) + \
                                                        (n) * sizeof(LONG))
#define DIJOFS_POV(n)      (FIELD_OFFSET(DIJOYSTATE, rgdwPOV) + \
                                                        (n) * sizeof(DWORD))
#define DIJOFS_BUTTON(n)   (FIELD_OFFSET(DIJOYSTATE, rgbButtons) + (n))
#define DIJOFS_BUTTON0      DIJOFS_BUTTON(0)
#define DIJOFS_BUTTON1      DIJOFS_BUTTON(1)
#define DIJOFS_BUTTON2      DIJOFS_BUTTON(2)
#define DIJOFS_BUTTON3      DIJOFS_BUTTON(3)
#define DIJOFS_BUTTON4      DIJOFS_BUTTON(4)
#define DIJOFS_BUTTON5      DIJOFS_BUTTON(5)
#define DIJOFS_BUTTON6      DIJOFS_BUTTON(6)
#define DIJOFS_BUTTON7      DIJOFS_BUTTON(7)
#define DIJOFS_BUTTON8      DIJOFS_BUTTON(8)
#define DIJOFS_BUTTON9      DIJOFS_BUTTON(9)
#define DIJOFS_BUTTON10     DIJOFS_BUTTON(10)
#define DIJOFS_BUTTON11     DIJOFS_BUTTON(11)
#define DIJOFS_BUTTON12     DIJOFS_BUTTON(12)
#define DIJOFS_BUTTON13     DIJOFS_BUTTON(13)
#define DIJOFS_BUTTON14     DIJOFS_BUTTON(14)
#define DIJOFS_BUTTON15     DIJOFS_BUTTON(15)
#define DIJOFS_BUTTON16     DIJOFS_BUTTON(16)
#define DIJOFS_BUTTON17     DIJOFS_BUTTON(17)
#define DIJOFS_BUTTON18     DIJOFS_BUTTON(18)
#define DIJOFS_BUTTON19     DIJOFS_BUTTON(19)
#define DIJOFS_BUTTON20     DIJOFS_BUTTON(20)
#define DIJOFS_BUTTON21     DIJOFS_BUTTON(21)
#define DIJOFS_BUTTON22     DIJOFS_BUTTON(22)
#define DIJOFS_BUTTON23     DIJOFS_BUTTON(23)
#define DIJOFS_BUTTON24     DIJOFS_BUTTON(24)
#define DIJOFS_BUTTON25     DIJOFS_BUTTON(25)
#define DIJOFS_BUTTON26     DIJOFS_BUTTON(26)
#define DIJOFS_BUTTON27     DIJOFS_BUTTON(27)
#define DIJOFS_BUTTON28     DIJOFS_BUTTON(28)
#define DIJOFS_BUTTON29     DIJOFS_BUTTON(29)
#define DIJOFS_BUTTON30     DIJOFS_BUTTON(30)
#define DIJOFS_BUTTON31     DIJOFS_BUTTON(31)*)


//#endif /* DIJ_RINGZERO */

(****************************************************************************
 *
 *  IDirectInput
 *
 ****************************************************************************)

//#ifndef DIJ_RINGZERO

const
  DIENUM_STOP           = 0;
  DIENUM_CONTINUE       = 1;

type
  LPDIENUMDEVICESCALLBACKA = function(lpddi: LPCDIDEVICEINSTANCEA; pvRef: LPVOID): BOOL; stdcall;
  LPDIENUMDEVICESCALLBACKW = function(lpddi: LPCDIDEVICEINSTANCEW; pvRef: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
  LPDIENUMDEVICESCALLBACK = LPDIENUMDEVICESCALLBACKW;
{$ELSE}
  LPDIENUMDEVICESCALLBACK = LPDIENUMDEVICESCALLBACKA;
{$ENDIF} // !UNICODE
  LPDICONFIGUREDEVICESCALLBACK = function(lpDDSTarget: {IUnknown FAR *}IUnknown; pvRef: LPVOID): BOOL; stdcall;

const
  DIEDFL_ALLDEVICES      = $00000000;
  DIEDFL_ATTACHEDONLY    = $00000001;
{$IF DIRECTINPUT_VERSION >= $0500}
  DIEDFL_FORCEFEEDBACK   = $00000100;
{$ENDIF} { DIRECTINPUT_VERSION >= $0500 }
{$IF DIRECTINPUT_VERSION >= $050a}
  DIEDFL_INCLUDEALIASES  = $00010000;
  DIEDFL_INCLUDEPHANTOMS = $00020000;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }
{$IF DIRECTINPUT_VERSION >= $0800}
  DIEDFL_INCLUDEHIDDEN   = $00040000;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }


{$IF DIRECTINPUT_VERSION >= $0800}
type
  LPDIENUMDEVICESBYSEMANTICSCBA = function(lpddi: LPCDIDEVICEINSTANCEA; lpdid: LPDIRECTINPUTDEVICE8A; dwFlags: DWORD; dwRemaining: DWORD; pvRef: LPVOID): BOOL; stdcall;
  LPDIENUMDEVICESBYSEMANTICSCBW = function(lpddi: LPCDIDEVICEINSTANCEW; lpdid: LPDIRECTINPUTDEVICE8W; dwFlags: DWORD; dwRemaining: DWORD; pvRef: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
  LPDIENUMDEVICESBYSEMANTICSCB = LPDIENUMDEVICESBYSEMANTICSCBW;
{$ELSE}
  LPDIENUMDEVICESBYSEMANTICSCB = LPDIENUMDEVICESBYSEMANTICSCBA;
{$ENDIF} // !UNICODE
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

{$IF DIRECTINPUT_VERSION >= $0800}
const
  DIEDBS_MAPPEDPRI1        = $00000001;
  DIEDBS_MAPPEDPRI2        = $00000002;
  DIEDBS_RECENTDEVICE      = $00000010;
  DIEDBS_NEWDEVICE         = $00000020;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

{$IF DIRECTINPUT_VERSION >= $0800}
const
  DIEDBSFL_ATTACHEDONLY       = $00000000;
  DIEDBSFL_THISUSER           = $00000010;
  DIEDBSFL_FORCEFEEDBACK      = DIEDFL_FORCEFEEDBACK;
  DIEDBSFL_AVAILABLEDEVICES   = $00001000;
  DIEDBSFL_MULTIMICEKEYBOARDS = $00002000;
  DIEDBSFL_NONGAMINGDEVICES   = $00004000;
  DIEDBSFL_VALID              = $00007110;
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

//#undef INTERFACE
//#define INTERFACE IDirectInputW

type
  IDirectInputW = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputW methods ***)
    function CreateDevice(const rguid: TGuid; out lplpDirectInputDevice: IDirectInputDeviceW; pUnkOuter: IUnknown): HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKW; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(const rguidInstance: TGuid): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD): HRESULT; stdcall;
  end;

//  typedef struct IDirectInputW *LPDIRECTINPUTW;
  LPDIRECTINPUTW = IDirectInputW;

//#undef INTERFACE
//#define INTERFACE IDirectInputA

  IDirectInputA = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputA methods ***)
    function CreateDevice(const rguid: TGuid; out lplpDirectInputDevice: IDirectInputDeviceA; pUnkOuter: IUnknown): HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKA; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(const rguidInstance: TGuid): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD): HRESULT; stdcall;
  end;

//typedef struct IDirectInputA *LPDIRECTINPUTA;
  LPDIRECTINPUTA = IDirectInputA;

{$IFDEF UNICODE}
const
  IID_IDirectInput: TIID = {IID_IDirectInputW}'{89521361-AA8A-11CF-BFC7-444553540000}';
type
  IDirectInput = IDirectInputW;
{$ELSE}
const
  IID_IDirectInput: TIID = {IID_IDirectInputA}'{89521360-AA8A-11CF-BFC7-444553540000}';
type
  IDirectInput = IDirectInputA;
{$ENDIF}
//typedef struct IDirectInput *LPDIRECTINPUT;
  LPDIRECTINPUT = IDirectInput;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInput_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInput_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInput_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInput_CreateDevice(p,a,b,c) (p)->lpVtbl->CreateDevice(p,a,b,c)
//#define IDirectInput_EnumDevices(p,a,b,c,d) (p)->lpVtbl->EnumDevices(p,a,b,c,d)
//#define IDirectInput_GetDeviceStatus(p,a) (p)->lpVtbl->GetDeviceStatus(p,a)
//#define IDirectInput_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInput_Initialize(p,a,b) (p)->lpVtbl->Initialize(p,a,b)
//#else
//#define IDirectInput_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInput_AddRef(p) (p)->AddRef()
//#define IDirectInput_Release(p) (p)->Release()
//#define IDirectInput_CreateDevice(p,a,b,c) (p)->CreateDevice(a,b,c)
//#define IDirectInput_EnumDevices(p,a,b,c,d) (p)->EnumDevices(a,b,c,d)
//#define IDirectInput_GetDeviceStatus(p,a) (p)->GetDeviceStatus(a)
//#define IDirectInput_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInput_Initialize(p,a,b) (p)->Initialize(a,b)
//#endif

//#undef INTERFACE
//#define INTERFACE IDirectInput2W

type
  IDirectInput2W = interface(IDirectInputW)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputW methods ***)
    {STDMETHOD(CreateDevice)(THIS_ REFGUID,LPDIRECTINPUTDEVICEW *,LPUNKNOWN) PURE;
    STDMETHOD(EnumDevices)(THIS_ DWORD,LPDIENUMDEVICESCALLBACKW,LPVOID,DWORD) PURE;
    STDMETHOD(GetDeviceStatus)(THIS_ REFGUID) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD) PURE;}

    (*** IDirectInput2W methods ***)
    function FindDevice(const rguidClass: TGuid; ptszName: LPCWSTR; pguidInstance: LPGUID): HRESULT; stdcall;
  end;

//typedef struct IDirectInput2W *LPDIRECTINPUT2W;
  LPDIRECTINPUT2W = IDirectInput2W;

//#undef INTERFACE
//#define INTERFACE IDirectInput2A

  IDirectInput2A = interface(IDirectInputA)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInputA methods ***)
    {STDMETHOD(CreateDevice)(THIS_ REFGUID,LPDIRECTINPUTDEVICEA *,LPUNKNOWN) PURE;
    STDMETHOD(EnumDevices)(THIS_ DWORD,LPDIENUMDEVICESCALLBACKA,LPVOID,DWORD) PURE;
    STDMETHOD(GetDeviceStatus)(THIS_ REFGUID) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD) PURE;}

    (*** IDirectInput2A methods ***)
    function FindDevice(const rguidClass: TGuid; ptszName: LPCSTR; pguidInstance: LPGUID): HRESULT; stdcall;
  end;

//typedef struct IDirectInput2A *LPDIRECTINPUT2A;
  LPDIRECTINPUT2A = IDirectInput2A;

{$IFDEF UNICODE}
const
  IID_IDirectInput2: TIID = {IID_IDirectInput2W}'{5944E663-AA8A-11CF-BFC7-444553540000}';
type
  IDirectInput2 = IDirectInput2W;
{$ELSE}
const
  IID_IDirectInput2: TIID = {IID_IDirectInput2A}'{5944E662-AA8A-11CF-BFC7-444553540000}';
type
  IDirectInput2 = IDirectInput2A;
{$ENDIF}
//typedef struct IDirectInput2 *LPDIRECTINPUT2;
  LPDIRECTINPUT2 = IDirectInput2;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInput2_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInput2_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInput2_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInput2_CreateDevice(p,a,b,c) (p)->lpVtbl->CreateDevice(p,a,b,c)
//#define IDirectInput2_EnumDevices(p,a,b,c,d) (p)->lpVtbl->EnumDevices(p,a,b,c,d)
//#define IDirectInput2_GetDeviceStatus(p,a) (p)->lpVtbl->GetDeviceStatus(p,a)
//#define IDirectInput2_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInput2_Initialize(p,a,b) (p)->lpVtbl->Initialize(p,a,b)
//#define IDirectInput2_FindDevice(p,a,b,c) (p)->lpVtbl->FindDevice(p,a,b,c)
//#else
//#define IDirectInput2_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInput2_AddRef(p) (p)->AddRef()
//#define IDirectInput2_Release(p) (p)->Release()
//#define IDirectInput2_CreateDevice(p,a,b,c) (p)->CreateDevice(a,b,c)
//#define IDirectInput2_EnumDevices(p,a,b,c,d) (p)->EnumDevices(a,b,c,d)
//#define IDirectInput2_GetDeviceStatus(p,a) (p)->GetDeviceStatus(a)
//#define IDirectInput2_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInput2_Initialize(p,a,b) (p)->Initialize(a,b)
//#define IDirectInput2_FindDevice(p,a,b,c) (p)->FindDevice(a,b,c)
//#endif


//#undef INTERFACE
//#define INTERFACE IDirectInput7W

type
  IDirectInput7W = interface(IDirectInput2W)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInput2W methods ***)
    {STDMETHOD(CreateDevice)(THIS_ REFGUID,LPDIRECTINPUTDEVICEW *,LPUNKNOWN) PURE;
    STDMETHOD(EnumDevices)(THIS_ DWORD,LPDIENUMDEVICESCALLBACKW,LPVOID,DWORD) PURE;
    STDMETHOD(GetDeviceStatus)(THIS_ REFGUID) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD) PURE;
    STDMETHOD(FindDevice)(THIS_ REFGUID,LPCWSTR,LPGUID) PURE;}

    (*** IDirectInput7W methods ***)
    function CreateDeviceEx(const rguid: TGuid; const riid: TIID; out pvOut: LPVOID; pUnkOuter: IUnknown): HRESULT; stdcall;
  end;

//typedef struct IDirectInput7W *LPDIRECTINPUT7W;
  LPDIRECTINPUT7W = IDirectInput7W;

//#undef INTERFACE
//#define INTERFACE IDirectInput7A

  IDirectInput7A = interface(IDirectInput2A)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInput2A methods ***)
    {STDMETHOD(CreateDevice)(THIS_ REFGUID,LPDIRECTINPUTDEVICEA *,LPUNKNOWN) PURE;
    STDMETHOD(EnumDevices)(THIS_ DWORD,LPDIENUMDEVICESCALLBACKA,LPVOID,DWORD) PURE;
    STDMETHOD(GetDeviceStatus)(THIS_ REFGUID) PURE;
    STDMETHOD(RunControlPanel)(THIS_ HWND,DWORD) PURE;
    STDMETHOD(Initialize)(THIS_ HINSTANCE,DWORD) PURE;
    STDMETHOD(FindDevice)(THIS_ REFGUID,LPCSTR,LPGUID) PURE;}

    (*** IDirectInput7A methods ***)
    function CreateDeviceEx(const rguid: TGuid; const riid: TIID; out pvOut: LPVOID; pUnkOuter: IUnknown): HRESULT; stdcall;
  end;

//typedef struct IDirectInput7A *LPDIRECTINPUT7A;
  LPDIRECTINPUT7A = IDirectInput7A;

{$IFDEF UNICODE}
const
  IID_IDirectInput7: TIID = {IID_IDirectInput7W}'{9A4CB685-236D-11D3-8E9D-00C04F6844AE}';
type
  IDirectInput7 = IDirectInput7W;
{$ELSE}
const
  IID_IDirectInput7: TIID = {IID_IDirectInput7A}'{9A4CB684-236D-11D3-8E9D-00C04F6844AE}';
type
  IDirectInput7 = IDirectInput7A;
{$ENDIF}
//typedef struct IDirectInput7 *LPDIRECTINPUT7;
  LPDIRECTINPUT7 = IDirectInput7;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInput7_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInput7_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInput7_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInput7_CreateDevice(p,a,b,c) (p)->lpVtbl->CreateDevice(p,a,b,c)
//#define IDirectInput7_EnumDevices(p,a,b,c,d) (p)->lpVtbl->EnumDevices(p,a,b,c,d)
//#define IDirectInput7_GetDeviceStatus(p,a) (p)->lpVtbl->GetDeviceStatus(p,a)
//#define IDirectInput7_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInput7_Initialize(p,a,b) (p)->lpVtbl->Initialize(p,a,b)
//#define IDirectInput7_FindDevice(p,a,b,c) (p)->lpVtbl->FindDevice(p,a,b,c)
//#define IDirectInput7_CreateDeviceEx(p,a,b,c,d) (p)->lpVtbl->CreateDeviceEx(p,a,b,c,d)
//#else
//#define IDirectInput7_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInput7_AddRef(p) (p)->AddRef()
//#define IDirectInput7_Release(p) (p)->Release()
//#define IDirectInput7_CreateDevice(p,a,b,c) (p)->CreateDevice(a,b,c)
//#define IDirectInput7_EnumDevices(p,a,b,c,d) (p)->EnumDevices(a,b,c,d)
//#define IDirectInput7_GetDeviceStatus(p,a) (p)->GetDeviceStatus(a)
//#define IDirectInput7_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInput7_Initialize(p,a,b) (p)->Initialize(a,b)
//#define IDirectInput7_FindDevice(p,a,b,c) (p)->FindDevice(a,b,c)
//#define IDirectInput7_CreateDeviceEx(p,a,b,c,d) (p)->CreateDeviceEx(a,b,c,d)
//#endif

{$IF DIRECTINPUT_VERSION >= $0800}
//#undef INTERFACE
//#define INTERFACE IDirectInput8W

type
  IDirectInput8W = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInput8W methods ***)
    function CreateDevice(const rguid: TGuid; out lplpDirectInputDevice: IDirectInputDevice8W; pUnkOuter: IUnknown): HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKW; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(const rguidInstance: TGuid): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD): HRESULT; stdcall;
    function FindDevice(const rguidClass: TGuid; ptszName: LPCWSTR; pguidInstance: LPGUID): HRESULT; stdcall;
    function EnumDevicesBySemantics(ptszUserName: LPCWSTR; lpdiActionFormat: LPDIACTIONFORMATW; lpCallback: LPDIENUMDEVICESBYSEMANTICSCBW; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function ConfigureDevices(lpdiCallback: LPDICONFIGUREDEVICESCALLBACK; lpdiCDParams: LPDICONFIGUREDEVICESPARAMSW; dwFlags: DWORD; pvRefData: LPVOID): HRESULT; stdcall;
  end;

//typedef struct IDirectInput8W *LPDIRECTINPUT8W;
  LPDIRECTINPUT8W = IDirectInput8W;

//#undef INTERFACE
//#define INTERFACE IDirectInput8A

  IDirectInput8A = interface(IUnknown)
    (*** IUnknown methods ***)
    {STDMETHOD(QueryInterface)(THIS_ REFIID riid, LPVOID * ppvObj) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;}

    (*** IDirectInput8A methods ***)
    function CreateDevice(const rguid: TGuid; out lplpDirectInputDevice: IDirectInputDevice8A; pUnkOuter: IUnknown): HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKA; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(const rguidInstance: TGuid): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: HINST; dwVersion: DWORD): HRESULT; stdcall;
    function FindDevice(const rguidClass: TGuid; ptszName: LPCSTR; pguidInstance: LPGUID): HRESULT; stdcall;
    function EnumDevicesBySemantics(ptszUserName: LPCSTR; lpdiActionFormat: LPDIACTIONFORMATA; lpCallback: LPDIENUMDEVICESBYSEMANTICSCBA; pvRef: LPVOID; dwFlags: DWORD): HRESULT; stdcall;
    function ConfigureDevices(lpdiCallback: LPDICONFIGUREDEVICESCALLBACK; lpdiCDParams: LPDICONFIGUREDEVICESPARAMSA; dwFlags: DWORD; pvRefData: LPVOID): HRESULT; stdcall;
  end;

//typedef struct IDirectInput8A *LPDIRECTINPUT8A;
  LPDIRECTINPUT8A = IDirectInput8A;

{$IFDEF UNICODE}
const
  IID_IDirectInput8: TIID = {IID_IDirectInput8W}'{BF798031-483A-4DA2-AA99-5D64ED369700}';
type
  IDirectInput8 = IDirectInput8W;
{$ELSE}
const
  IID_IDirectInput8: TIID = {IID_IDirectInput8A}'{BF798030-483A-4DA2-AA99-5D64ED369700}';
type
  IDirectInput8 = IDirectInput8A;
{$ENDIF}
//typedef struct IDirectInput8 *LPDIRECTINPUT8;
  LPDIRECTINPUT8 = IDirectInput8;

//#if !defined(__cplusplus) || defined(CINTERFACE)
//#define IDirectInput8_QueryInterface(p,a,b) (p)->lpVtbl->QueryInterface(p,a,b)
//#define IDirectInput8_AddRef(p) (p)->lpVtbl->AddRef(p)
//#define IDirectInput8_Release(p) (p)->lpVtbl->Release(p)
//#define IDirectInput8_CreateDevice(p,a,b,c) (p)->lpVtbl->CreateDevice(p,a,b,c)
//#define IDirectInput8_EnumDevices(p,a,b,c,d) (p)->lpVtbl->EnumDevices(p,a,b,c,d)
//#define IDirectInput8_GetDeviceStatus(p,a) (p)->lpVtbl->GetDeviceStatus(p,a)
//#define IDirectInput8_RunControlPanel(p,a,b) (p)->lpVtbl->RunControlPanel(p,a,b)
//#define IDirectInput8_Initialize(p,a,b) (p)->lpVtbl->Initialize(p,a,b)
//#define IDirectInput8_FindDevice(p,a,b,c) (p)->lpVtbl->FindDevice(p,a,b,c)
//#define IDirectInput8_EnumDevicesBySemantics(p,a,b,c,d,e) (p)->lpVtbl->EnumDevicesBySemantics(p,a,b,c,d,e)
//#define IDirectInput8_ConfigureDevices(p,a,b,c,d) (p)->lpVtbl->ConfigureDevices(p,a,b,c,d)
//#else
//#define IDirectInput8_QueryInterface(p,a,b) (p)->QueryInterface(a,b)
//#define IDirectInput8_AddRef(p) (p)->AddRef()
//#define IDirectInput8_Release(p) (p)->Release()
//#define IDirectInput8_CreateDevice(p,a,b,c) (p)->CreateDevice(a,b,c)
//#define IDirectInput8_EnumDevices(p,a,b,c,d) (p)->EnumDevices(a,b,c,d)
//#define IDirectInput8_GetDeviceStatus(p,a) (p)->GetDeviceStatus(a)
//#define IDirectInput8_RunControlPanel(p,a,b) (p)->RunControlPanel(a,b)
//#define IDirectInput8_Initialize(p,a,b) (p)->Initialize(a,b)
//#define IDirectInput8_FindDevice(p,a,b,c) (p)->FindDevice(a,b,c)
//#define IDirectInput8_EnumDevicesBySemantics(p,a,b,c,d,e) (p)->EnumDevicesBySemantics(a,b,c,d,e)
//#define IDirectInput8_ConfigureDevices(p,a,b,c,d) (p)->ConfigureDevices(a,b,c,d)
//#endif
{$ENDIF} { DIRECTINPUT_VERSION >= $0800 }

{$IF DIRECTINPUT_VERSION > $0700}

//extern HRESULT WINAPI DirectInput8Create(HINSTANCE hinst, DWORD dwVersion, REFIID riidltf, LPVOID *ppvOut, LPUNKNOWN punkOuter);
function DirectInput8Create(hinst: HINST; dwVersion: DWORD; const riidltf: TIID; out ppvOut: LPVOID; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput8';

{$ELSE}
//extern HRESULT WINAPI DirectInputCreateA(HINSTANCE hinst, DWORD dwVersion, LPDIRECTINPUTA *ppDI, LPUNKNOWN punkOuter);
function DirectInputCreateA(hinst: HINST; dwVersion: DWORD; out ppDI: IDirectInputA; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput';
//extern HRESULT WINAPI DirectInputCreateW(HINSTANCE hinst, DWORD dwVersion, LPDIRECTINPUTW *ppDI, LPUNKNOWN punkOuter);
function DirectInputCreateW(hinst: HINST; dwVersion: DWORD; out ppDI: IDirectInputW; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput';
{$IFDEF UNICODE}
function DirectInputCreate(hinst: HINST; dwVersion: DWORD; out ppDI: IDirectInputW; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput' name 'DirectInputCreateW';
{$ELSE}
function DirectInputCreate(hinst: HINST; dwVersion: DWORD; out ppDI: IDirectInputA; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput' name 'DirectInputCreateA';
{$ENDIF} // !UNICODE

//extern HRESULT WINAPI DirectInputCreateEx(HINSTANCE hinst, DWORD dwVersion, REFIID riidltf, LPVOID *ppvOut, LPUNKNOWN punkOuter);
function DirectInputCreateEx(hinst: HINST; dwVersion: DWORD; const riidltf: TIID; out ppvOut: LPVOID; punkOuter: IUnknown): HRESULT; stdcall; external 'dinput';

{$ENDIF} { DIRECTINPUT_VERSION > $700 }

//#endif /* DIJ_RINGZERO */


(****************************************************************************
 *
 *  Return Codes
 *
 ****************************************************************************)

(*
 *  The operation completed successfully.
 *)
const
  DI_OK                           = S_OK;

(*
 *  The device exists but is not currently attached.
 *)
  DI_NOTATTACHED                  = S_FALSE;

(*
 *  The device buffer overflowed.  Some input was lost.
 *)
  DI_BUFFEROVERFLOW               = S_FALSE;

(*
 *  The change in device properties had no effect.
 *)
  DI_PROPNOEFFECT                 = S_FALSE;

(*
 *  The operation had no effect.
 *)
  DI_NOEFFECT                     = S_FALSE;

(*
 *  The device is a polled device.  As a result, device buffering
 *  will not collect any data and event notifications will not be
 *  signalled until GetDeviceState is called.
 *)
  DI_POLLEDDEVICE                 = HRESULT($00000002);

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but the effect was not
 *  downloaded because the device is not exclusively acquired
 *  or because the DIEP_NODOWNLOAD flag was passed.
 *)
  DI_DOWNLOADSKIPPED              = HRESULT($00000003);

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but in order to change
 *  the parameters, the effect needed to be restarted.
 *)
  DI_EFFECTRESTARTED              = HRESULT($00000004);

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but some of them were
 *  beyond the capabilities of the device and were truncated.
 *)
  DI_TRUNCATED                    = HRESULT($00000008);

(*
 *  The settings have been successfully applied but could not be
 *  persisted.
 *)
  DI_SETTINGSNOTSAVED             = HRESULT($0000000B);

(*
 *  Equal to DI_EFFECTRESTARTED | DI_TRUNCATED.
 *)
  DI_TRUNCATEDANDRESTARTED        = HRESULT($0000000C);

(*
 *  A SUCCESS code indicating that settings cannot be modified.
 *)
  DI_WRITEPROTECT                 = HRESULT($00000013);

(*
 *  The application requires a newer version of DirectInput.
 *)
//  DIERR_OLDDIRECTINPUTVERSION     \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_OLD_WIN_VERSION)

(*
 *  The application was written for an unsupported prerelease version
 *  of DirectInput.
 *)
//  DIERR_BETADIRECTINPUTVERSION    \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_RMODE_APP)

(*
 *  The object could not be created due to an incompatible driver version
 *  or mismatched or incomplete driver components.
 *)
//  DIERR_BADDRIVERVER              \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_BAD_DRIVER_LEVEL)

(*
 * The device or device instance or effect is not registered with DirectInput.
 *)
  DIERR_DEVICENOTREG              = REGDB_E_CLASSNOTREG;

(*
 * The requested object does not exist.
 *)
//  DIERR_NOTFOUND                  \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_FILE_NOT_FOUND)

(*
 * The requested object does not exist.
 *)
//  DIERR_OBJECTNOTFOUND            \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_FILE_NOT_FOUND)

(*
 * An invalid parameter was passed to the returning function,
 * or the object was not in a state that admitted the function
 * to be called.
 *)
  DIERR_INVALIDPARAM              = E_INVALIDARG;

(*
 * The specified interface is not supported by the object
 *)
  DIERR_NOINTERFACE               = E_NOINTERFACE;

(*
 * An undetermined error occured inside the DInput subsystem
 *)
  DIERR_GENERIC                   = E_FAIL;

(*
 * The DInput subsystem couldn't allocate sufficient memory to complete the
 * caller's request.
 *)
  DIERR_OUTOFMEMORY               = E_OUTOFMEMORY;

(*
 * The function called is not supported at this time
 *)
  DIERR_UNSUPPORTED               = E_NOTIMPL;

(*
 * This object has not been initialized
 *)
//  DIERR_NOTINITIALIZED            \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_NOT_READY)

(*
 * This object is already initialized
 *)
//  DIERR_ALREADYINITIALIZED        \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_ALREADY_INITIALIZED)

(*
 * This object does not support aggregation
 *)
  DIERR_NOAGGREGATION             = CLASS_E_NOAGGREGATION;

(*
 * Another app has a higher priority level, preventing this call from
 * succeeding.
 *)
  DIERR_OTHERAPPHASPRIO           = E_ACCESSDENIED;

(*
 * Access to the device has been lost.  It must be re-acquired.
 *)
//  DIERR_INPUTLOST                 \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_READ_FAULT)

(*
 * The operation cannot be performed while the device is acquired.
 *)
//  DIERR_ACQUIRED                  \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_BUSY)

(*
 * The operation cannot be performed unless the device is acquired.
 *)
//  DIERR_NOTACQUIRED               \
//    MAKE_HRESULT(SEVERITY_ERROR, FACILITY_WIN32, ERROR_INVALID_ACCESS)

(*
 * The specified property cannot be changed.
 *)
  DIERR_READONLY                  = E_ACCESSDENIED;

(*
 * The device already has an event notification associated with it.
 *)
  DIERR_HANDLEEXISTS              = E_ACCESSDENIED;

(*
 * Data is not yet available.
 *)
//#ifndef E_PENDING
  E_PENDING                       = $8000000A;
//#endif

(*
 * Unable to IDirectInputJoyConfig_Acquire because the user
 * does not have sufficient privileges to change the joystick
 * configuration.
 *)
  DIERR_INSUFFICIENTPRIVS         = $80040200;

(*
 * The device is full.
 *)
  DIERR_DEVICEFULL                = $80040201;

(*
 * Not all the requested information fit into the buffer.
 *)
  DIERR_MOREDATA                  = $80040202;

(*
 * The effect is not downloaded.
 *)
  DIERR_NOTDOWNLOADED             = $80040203;

(*
 *  The device cannot be reinitialized because there are still effects
 *  attached to it.
 *)
  DIERR_HASEFFECTS                = $80040204;

(*
 *  The operation cannot be performed unless the device is acquired
 *  in DISCL_EXCLUSIVE mode.
 *)
  DIERR_NOTEXCLUSIVEACQUIRED      = $80040205;

(*
 *  The effect could not be downloaded because essential information
 *  is missing.  For example, no axes have been associated with the
 *  effect, or no type-specific information has been created.
 *)
  DIERR_INCOMPLETEEFFECT          = $80040206;

(*
 *  Attempted to read buffered device data from a device that is
 *  not buffered.
 *)
  DIERR_NOTBUFFERED               = $80040207;

(*
 *  An attempt was made to modify parameters of an effect while it is
 *  playing.  Not all hardware devices support altering the parameters
 *  of an effect while it is playing.
 *)
  DIERR_EFFECTPLAYING             = $80040208;

(*
 *  The operation could not be completed because the device is not
 *  plugged in.
 *)
  DIERR_UNPLUGGED                 = $80040209;

(*
 *  SendDeviceData failed because more information was requested
 *  to be sent than can be sent to the device.  Some devices have
 *  restrictions on how much data can be sent to them.  (For example,
 *  there might be a limit on the number of buttons that can be
 *  pressed at once.)
 *)
  DIERR_REPORTFULL                = $8004020A;


(*
 *  A mapper file function failed because reading or writing the user or IHV
 *  settings file failed.
 *)
  DIERR_MAPFILEFAIL               = $8004020B;


//
//    Copyright (C) Microsoft.  All rights reserved.
//


//
//    Copyright (C) Microsoft.  All rights reserved.
//
(*--- DINPUT Mapper Definitions: New for Dx8         ---*)


(*--- Keyboard
      Physical Keyboard Device       ---*)

//
//    Copyright (C) Microsoft.  All rights reserved.
//
  DIKEYBOARD_ESCAPE                       = $81000401;
  DIKEYBOARD_1                            = $81000402;
  DIKEYBOARD_2                            = $81000403;
  DIKEYBOARD_3                            = $81000404;
  DIKEYBOARD_4                            = $81000405;
  DIKEYBOARD_5                            = $81000406;
  DIKEYBOARD_6                            = $81000407;
  DIKEYBOARD_7                            = $81000408;
  DIKEYBOARD_8                            = $81000409;
  DIKEYBOARD_9                            = $8100040A;
  DIKEYBOARD_0                            = $8100040B;
  DIKEYBOARD_MINUS                        = $8100040C;    { - on main keyboard }
  DIKEYBOARD_EQUALS                       = $8100040D;
  DIKEYBOARD_BACK                         = $8100040E;    { backspace }
  DIKEYBOARD_TAB                          = $8100040F;
  DIKEYBOARD_Q                            = $81000410;
  DIKEYBOARD_W                            = $81000411;
  DIKEYBOARD_E                            = $81000412;
  DIKEYBOARD_R                            = $81000413;
  DIKEYBOARD_T                            = $81000414;
  DIKEYBOARD_Y                            = $81000415;
  DIKEYBOARD_U                            = $81000416;
  DIKEYBOARD_I                            = $81000417;
  DIKEYBOARD_O                            = $81000418;
  DIKEYBOARD_P                            = $81000419;
  DIKEYBOARD_LBRACKET                     = $8100041A;
  DIKEYBOARD_RBRACKET                     = $8100041B;
  DIKEYBOARD_RETURN                       = $8100041C;    { Enter on main keyboard }
  DIKEYBOARD_LCONTROL                     = $8100041D;
  DIKEYBOARD_A                            = $8100041E;
  DIKEYBOARD_S                            = $8100041F;
  DIKEYBOARD_D                            = $81000420;
  DIKEYBOARD_F                            = $81000421;
  DIKEYBOARD_G                            = $81000422;
  DIKEYBOARD_H                            = $81000423;
  DIKEYBOARD_J                            = $81000424;
  DIKEYBOARD_K                            = $81000425;
  DIKEYBOARD_L                            = $81000426;
  DIKEYBOARD_SEMICOLON                    = $81000427;
  DIKEYBOARD_APOSTROPHE                   = $81000428;
  DIKEYBOARD_GRAVE                        = $81000429;    { accent grave }
  DIKEYBOARD_LSHIFT                       = $8100042A;
  DIKEYBOARD_BACKSLASH                    = $8100042B;
  DIKEYBOARD_Z                            = $8100042C;
  DIKEYBOARD_X                            = $8100042D;
  DIKEYBOARD_C                            = $8100042E;
  DIKEYBOARD_V                            = $8100042F;
  DIKEYBOARD_B                            = $81000430;
  DIKEYBOARD_N                            = $81000431;
  DIKEYBOARD_M                            = $81000432;
  DIKEYBOARD_COMMA                        = $81000433;
  DIKEYBOARD_PERIOD                       = $81000434;    { . on main keyboard }
  DIKEYBOARD_SLASH                        = $81000435;    { / on main keyboard }
  DIKEYBOARD_RSHIFT                       = $81000436;
  DIKEYBOARD_MULTIPLY                     = $81000437;    { * on numeric keypad }
  DIKEYBOARD_LMENU                        = $81000438;    { left Alt }
  DIKEYBOARD_SPACE                        = $81000439;
  DIKEYBOARD_CAPITAL                      = $8100043A;
  DIKEYBOARD_F1                           = $8100043B;
  DIKEYBOARD_F2                           = $8100043C;
  DIKEYBOARD_F3                           = $8100043D;
  DIKEYBOARD_F4                           = $8100043E;
  DIKEYBOARD_F5                           = $8100043F;
  DIKEYBOARD_F6                           = $81000440;
  DIKEYBOARD_F7                           = $81000441;
  DIKEYBOARD_F8                           = $81000442;
  DIKEYBOARD_F9                           = $81000443;
  DIKEYBOARD_F10                          = $81000444;
  DIKEYBOARD_NUMLOCK                      = $81000445;
  DIKEYBOARD_SCROLL                       = $81000446;    { Scroll Lock }
  DIKEYBOARD_NUMPAD7                      = $81000447;
  DIKEYBOARD_NUMPAD8                      = $81000448;
  DIKEYBOARD_NUMPAD9                      = $81000449;
  DIKEYBOARD_SUBTRACT                     = $8100044A;    { - on numeric keypad }
  DIKEYBOARD_NUMPAD4                      = $8100044B;
  DIKEYBOARD_NUMPAD5                      = $8100044C;
  DIKEYBOARD_NUMPAD6                      = $8100044D;
  DIKEYBOARD_ADD                          = $8100044E;    { + on numeric keypad }
  DIKEYBOARD_NUMPAD1                      = $8100044F;
  DIKEYBOARD_NUMPAD2                      = $81000450;
  DIKEYBOARD_NUMPAD3                      = $81000451;
  DIKEYBOARD_NUMPAD0                      = $81000452;
  DIKEYBOARD_DECIMAL                      = $81000453;    { . on numeric keypad }
  DIKEYBOARD_OEM_102                      = $81000456;    { <> or \| on RT 102-key keyboard (Non-U.S.) }
  DIKEYBOARD_F11                          = $81000457;
  DIKEYBOARD_F12                          = $81000458;
  DIKEYBOARD_F13                          = $81000464;    {                     (NEC PC98) }
  DIKEYBOARD_F14                          = $81000465;    {                     (NEC PC98) }
  DIKEYBOARD_F15                          = $81000466;    {                     (NEC PC98) }
  DIKEYBOARD_KANA                         = $81000470;    { (Japanese keyboard)            }
  DIKEYBOARD_ABNT_C1                      = $81000473;    { /? on Brazilian keyboard }
  DIKEYBOARD_CONVERT                      = $81000479;    { (Japanese keyboard)            }
  DIKEYBOARD_NOCONVERT                    = $8100047B;    { (Japanese keyboard)            }
  DIKEYBOARD_YEN                          = $8100047D;    { (Japanese keyboard)            }
  DIKEYBOARD_ABNT_C2                      = $8100047E;    { Numpad . on Brazilian keyboard }
  DIKEYBOARD_NUMPADEQUALS                 = $8100048D;    { = on numeric keypad (NEC PC98) }
  DIKEYBOARD_PREVTRACK                    = $81000490;    { Previous Track (DIK_CIRCUMFLEX on Japanese keyboard) }
  DIKEYBOARD_AT                           = $81000491;    {                     (NEC PC98) }
  DIKEYBOARD_COLON                        = $81000492;    {                     (NEC PC98) }
  DIKEYBOARD_UNDERLINE                    = $81000493;    {                     (NEC PC98) }
  DIKEYBOARD_KANJI                        = $81000494;    { (Japanese keyboard)            }
  DIKEYBOARD_STOP                         = $81000495;    {                     (NEC PC98) }
  DIKEYBOARD_AX                           = $81000496;    {                     (Japan AX) }
  DIKEYBOARD_UNLABELED                    = $81000497;    {                        (J3100) }
  DIKEYBOARD_NEXTTRACK                    = $81000499;    { Next Track }
  DIKEYBOARD_NUMPADENTER                  = $8100049C;    { Enter on numeric keypad }
  DIKEYBOARD_RCONTROL                     = $8100049D;
  DIKEYBOARD_MUTE                         = $810004A0;    { Mute }
  DIKEYBOARD_CALCULATOR                   = $810004A1;    { Calculator }
  DIKEYBOARD_PLAYPAUSE                    = $810004A2;    { Play / Pause }
  DIKEYBOARD_MEDIASTOP                    = $810004A4;    { Media Stop }
  DIKEYBOARD_VOLUMEDOWN                   = $810004AE;    { Volume - }
  DIKEYBOARD_VOLUMEUP                     = $810004B0;    { Volume + }
  DIKEYBOARD_WEBHOME                      = $810004B2;    { Web home }
  DIKEYBOARD_NUMPADCOMMA                  = $810004B3;    { , on numeric keypad (NEC PC98) }
  DIKEYBOARD_DIVIDE                       = $810004B5;    { / on numeric keypad }
  DIKEYBOARD_SYSRQ                        = $810004B7;
  DIKEYBOARD_RMENU                        = $810004B8;    { right Alt }
  DIKEYBOARD_PAUSE                        = $810004C5;    { Pause }
  DIKEYBOARD_HOME                         = $810004C7;    { Home on arrow keypad }
  DIKEYBOARD_UP                           = $810004C8;    { UpArrow on arrow keypad }
  DIKEYBOARD_PRIOR                        = $810004C9;    { PgUp on arrow keypad }
  DIKEYBOARD_LEFT                         = $810004CB;    { LeftArrow on arrow keypad }
  DIKEYBOARD_RIGHT                        = $810004CD;    { RightArrow on arrow keypad }
  DIKEYBOARD_END                          = $810004CF;    { End on arrow keypad }
  DIKEYBOARD_DOWN                         = $810004D0;    { DownArrow on arrow keypad }
  DIKEYBOARD_NEXT                         = $810004D1;    { PgDn on arrow keypad }
  DIKEYBOARD_INSERT                       = $810004D2;    { Insert on arrow keypad }
  DIKEYBOARD_DELETE                       = $810004D3;    { Delete on arrow keypad }
  DIKEYBOARD_LWIN                         = $810004DB;    { Left Windows key }
  DIKEYBOARD_RWIN                         = $810004DC;    { Right Windows key }
  DIKEYBOARD_APPS                         = $810004DD;    { AppMenu key }
  DIKEYBOARD_POWER                        = $810004DE;    { System Power }
  DIKEYBOARD_SLEEP                        = $810004DF;    { System Sleep }
  DIKEYBOARD_WAKE                         = $810004E3;    { System Wake }
  DIKEYBOARD_WEBSEARCH                    = $810004E5;    { Web Search }
  DIKEYBOARD_WEBFAVORITES                 = $810004E6;    { Web Favorites }
  DIKEYBOARD_WEBREFRESH                   = $810004E7;    { Web Refresh }
  DIKEYBOARD_WEBSTOP                      = $810004E8;    { Web Stop }
  DIKEYBOARD_WEBFORWARD                   = $810004E9;    { Web Forward }
  DIKEYBOARD_WEBBACK                      = $810004EA;    { Web Back }
  DIKEYBOARD_MYCOMPUTER                   = $810004EB;    { My Computer }
  DIKEYBOARD_MAIL                         = $810004EC;    { Mail }
  DIKEYBOARD_MEDIASELECT                  = $810004ED;    { Media Select }


(*--- MOUSE
      Physical Mouse Device             ---*)

{#define DIMOUSE_XAXISAB                         (0x82000200 |DIMOFS_X ) /* X Axis-absolute: Some mice natively report absolute coordinates  */
#define DIMOUSE_YAXISAB                         (0x82000200 |DIMOFS_Y ) /* Y Axis-absolute: Some mice natively report absolute coordinates */
#define DIMOUSE_XAXIS                           (0x82000300 |DIMOFS_X ) /* X Axis */
#define DIMOUSE_YAXIS                           (0x82000300 |DIMOFS_Y ) /* Y Axis */
#define DIMOUSE_WHEEL                           (0x82000300 |DIMOFS_Z ) /* Z Axis */
#define DIMOUSE_BUTTON0                         (0x82000400 |DIMOFS_BUTTON0) /* Button 0 */
#define DIMOUSE_BUTTON1                         (0x82000400 |DIMOFS_BUTTON1) /* Button 1 */
#define DIMOUSE_BUTTON2                         (0x82000400 |DIMOFS_BUTTON2) /* Button 2 */
#define DIMOUSE_BUTTON3                         (0x82000400 |DIMOFS_BUTTON3) /* Button 3 */
#define DIMOUSE_BUTTON4                         (0x82000400 |DIMOFS_BUTTON4) /* Button 4 */
#define DIMOUSE_BUTTON5                         (0x82000400 |DIMOFS_BUTTON5) /* Button 5 */
#define DIMOUSE_BUTTON6                         (0x82000400 |DIMOFS_BUTTON6) /* Button 6 */
#define DIMOUSE_BUTTON7                         (0x82000400 |DIMOFS_BUTTON7) /* Button 7 */}


(*--- VOICE
      Physical Dplay Voice Device       ---*)

  DIVOICE_CHANNEL1                        = $83000401;
  DIVOICE_CHANNEL2                        = $83000402;
  DIVOICE_CHANNEL3                        = $83000403;
  DIVOICE_CHANNEL4                        = $83000404;
  DIVOICE_CHANNEL5                        = $83000405;
  DIVOICE_CHANNEL6                        = $83000406;
  DIVOICE_CHANNEL7                        = $83000407;
  DIVOICE_CHANNEL8                        = $83000408;
  DIVOICE_TEAM                            = $83000409;
  DIVOICE_ALL                             = $8300040A;
  DIVOICE_RECORDMUTE                      = $8300040B;
  DIVOICE_PLAYBACKMUTE                    = $8300040C;
  DIVOICE_TRANSMIT                        = $8300040D;

  DIVOICE_VOICECOMMAND                    = $83000410;


(*--- Driving Simulator - Racing
      Vehicle control is primary objective  ---*)
  DIVIRTUAL_DRIVING_RACE                  = $01000000;
  DIAXIS_DRIVINGR_STEER                   = $01008A01; { Steering }
  DIAXIS_DRIVINGR_ACCELERATE              = $01039202; { Accelerate }
  DIAXIS_DRIVINGR_BRAKE                   = $01041203; { Brake-Axis }
  DIBUTTON_DRIVINGR_SHIFTUP               = $01000C01; { Shift to next higher gear }
  DIBUTTON_DRIVINGR_SHIFTDOWN             = $01000C02; { Shift to next lower gear }
  DIBUTTON_DRIVINGR_VIEW                  = $01001C03; { Cycle through view options }
  DIBUTTON_DRIVINGR_MENU                  = $010004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIAXIS_DRIVINGR_ACCEL_AND_BRAKE         = $01014A04; { Some devices combine accelerate and brake in a single axis }
  DIHATSWITCH_DRIVINGR_GLANCE             = $01004601; { Look around }
  DIBUTTON_DRIVINGR_BRAKE                 = $01004C04; { Brake-button }
  DIBUTTON_DRIVINGR_DASHBOARD             = $01004405; { Select next dashboard option }
  DIBUTTON_DRIVINGR_AIDS                  = $01004406; { Driver correction aids }
  DIBUTTON_DRIVINGR_MAP                   = $01004407; { Display Driving Map }
  DIBUTTON_DRIVINGR_BOOST                 = $01004408; { Turbo Boost }
  DIBUTTON_DRIVINGR_PIT                   = $01004409; { Pit stop notification }
  DIBUTTON_DRIVINGR_ACCELERATE_LINK       = $0103D4E0; { Fallback Accelerate button }
  DIBUTTON_DRIVINGR_STEER_LEFT_LINK       = $0100CCE4; { Fallback Steer Left button }
  DIBUTTON_DRIVINGR_STEER_RIGHT_LINK      = $0100CCEC; { Fallback Steer Right button }
  DIBUTTON_DRIVINGR_GLANCE_LEFT_LINK      = $0107C4E4; { Fallback Glance Left button }
  DIBUTTON_DRIVINGR_GLANCE_RIGHT_LINK     = $0107C4EC; { Fallback Glance Right button }
  DIBUTTON_DRIVINGR_DEVICE                = $010044FE; { Show input device and controls }
  DIBUTTON_DRIVINGR_PAUSE                 = $010044FC; { Start / Pause / Restart game }

(*--- Driving Simulator - Combat
      Combat from within a vehicle is primary objective  ---*)
  DIVIRTUAL_DRIVING_COMBAT                = $02000000;
  DIAXIS_DRIVINGC_STEER                   = $02008A01; { Steering  }
  DIAXIS_DRIVINGC_ACCELERATE              = $02039202; { Accelerate }
  DIAXIS_DRIVINGC_BRAKE                   = $02041203; { Brake-axis }
  DIBUTTON_DRIVINGC_FIRE                  = $02000C01; { Fire }
  DIBUTTON_DRIVINGC_WEAPONS               = $02000C02; { Select next weapon }
  DIBUTTON_DRIVINGC_TARGET                = $02000C03; { Select next available target }
  DIBUTTON_DRIVINGC_MENU                  = $020004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIAXIS_DRIVINGC_ACCEL_AND_BRAKE         = $02014A04; { Some devices combine accelerate and brake in a single axis }
  DIHATSWITCH_DRIVINGC_GLANCE             = $02004601; { Look around }
  DIBUTTON_DRIVINGC_SHIFTUP               = $02004C04; { Shift to next higher gear }
  DIBUTTON_DRIVINGC_SHIFTDOWN             = $02004C05; { Shift to next lower gear }
  DIBUTTON_DRIVINGC_DASHBOARD             = $02004406; { Select next dashboard option }
  DIBUTTON_DRIVINGC_AIDS                  = $02004407; { Driver correction aids }
  DIBUTTON_DRIVINGC_BRAKE                 = $02004C08; { Brake-button }
  DIBUTTON_DRIVINGC_FIRESECONDARY         = $02004C09; { Alternative fire button }
  DIBUTTON_DRIVINGC_ACCELERATE_LINK       = $0203D4E0; { Fallback Accelerate button }
  DIBUTTON_DRIVINGC_STEER_LEFT_LINK       = $0200CCE4; { Fallback Steer Left button }
  DIBUTTON_DRIVINGC_STEER_RIGHT_LINK      = $0200CCEC; { Fallback Steer Right button }
  DIBUTTON_DRIVINGC_GLANCE_LEFT_LINK      = $0207C4E4; { Fallback Glance Left button }
  DIBUTTON_DRIVINGC_GLANCE_RIGHT_LINK     = $0207C4EC; { Fallback Glance Right button }
  DIBUTTON_DRIVINGC_DEVICE                = $020044FE; { Show input device and controls }
  DIBUTTON_DRIVINGC_PAUSE                 = $020044FC; { Start / Pause / Restart game }

(*--- Driving Simulator - Tank
      Combat from withing a tank is primary objective  ---*)
  DIVIRTUAL_DRIVING_TANK                  = $03000000;
  DIAXIS_DRIVINGT_STEER                   = $03008A01; { Turn tank left / right }
  DIAXIS_DRIVINGT_BARREL                  = $03010202; { Raise / lower barrel }
  DIAXIS_DRIVINGT_ACCELERATE              = $03039203; { Accelerate }
  DIAXIS_DRIVINGT_ROTATE                  = $03020204; { Turn barrel left / right }
  DIBUTTON_DRIVINGT_FIRE                  = $03000C01; { Fire }
  DIBUTTON_DRIVINGT_WEAPONS               = $03000C02; { Select next weapon }
  DIBUTTON_DRIVINGT_TARGET                = $03000C03; { Selects next available target }
  DIBUTTON_DRIVINGT_MENU                  = $030004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_DRIVINGT_GLANCE             = $03004601; { Look around }
  DIAXIS_DRIVINGT_BRAKE                   = $03045205; { Brake-axis }
  DIAXIS_DRIVINGT_ACCEL_AND_BRAKE         = $03014A06; { Some devices combine accelerate and brake in a single axis }
  DIBUTTON_DRIVINGT_VIEW                  = $03005C04; { Cycle through view options }
  DIBUTTON_DRIVINGT_DASHBOARD             = $03005C05; { Select next dashboard option }
  DIBUTTON_DRIVINGT_BRAKE                 = $03004C06; { Brake-button }
  DIBUTTON_DRIVINGT_FIRESECONDARY         = $03004C07; { Alternative fire button }
  DIBUTTON_DRIVINGT_ACCELERATE_LINK       = $0303D4E0; { Fallback Accelerate button }
  DIBUTTON_DRIVINGT_STEER_LEFT_LINK       = $0300CCE4; { Fallback Steer Left button }
  DIBUTTON_DRIVINGT_STEER_RIGHT_LINK      = $0300CCEC; { Fallback Steer Right button }
  DIBUTTON_DRIVINGT_BARREL_UP_LINK        = $030144E0; { Fallback Barrel up button }
  DIBUTTON_DRIVINGT_BARREL_DOWN_LINK      = $030144E8; { Fallback Barrel down button }
  DIBUTTON_DRIVINGT_ROTATE_LEFT_LINK      = $030244E4; { Fallback Rotate left button }
  DIBUTTON_DRIVINGT_ROTATE_RIGHT_LINK     = $030244EC; { Fallback Rotate right button }
  DIBUTTON_DRIVINGT_GLANCE_LEFT_LINK      = $0307C4E4; { Fallback Glance Left button }
  DIBUTTON_DRIVINGT_GLANCE_RIGHT_LINK     = $0307C4EC; { Fallback Glance Right button }
  DIBUTTON_DRIVINGT_DEVICE                = $030044FE; { Show input device and controls }
  DIBUTTON_DRIVINGT_PAUSE                 = $030044FC; { Start / Pause / Restart game }

(*--- Flight Simulator - Civilian
      Plane control is the primary objective  ---*)
  DIVIRTUAL_FLYING_CIVILIAN               = $04000000;
  DIAXIS_FLYINGC_BANK                     = $04008A01; { Roll ship left / right }
  DIAXIS_FLYINGC_PITCH                    = $04010A02; { Nose up / down }
  DIAXIS_FLYINGC_THROTTLE                 = $04039203; { Throttle }
  DIBUTTON_FLYINGC_VIEW                   = $04002401; { Cycle through view options }
  DIBUTTON_FLYINGC_DISPLAY                = $04002402; { Select next dashboard / heads up display option }
  DIBUTTON_FLYINGC_GEAR                   = $04002C03; { Gear up / down }
  DIBUTTON_FLYINGC_MENU                   = $040004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_FLYINGC_GLANCE              = $04004601; { Look around }
  DIAXIS_FLYINGC_BRAKE                    = $04046A04; { Apply Brake }
  DIAXIS_FLYINGC_RUDDER                   = $04025205; { Yaw ship left/right }
  DIAXIS_FLYINGC_FLAPS                    = $04055A06; { Flaps }
  DIBUTTON_FLYINGC_FLAPSUP                = $04006404; { Increment stepping up until fully retracted }
  DIBUTTON_FLYINGC_FLAPSDOWN              = $04006405; { Decrement stepping down until fully extended }
  DIBUTTON_FLYINGC_BRAKE_LINK             = $04046CE0; { Fallback brake button }
  DIBUTTON_FLYINGC_FASTER_LINK            = $0403D4E0; { Fallback throttle up button }
  DIBUTTON_FLYINGC_SLOWER_LINK            = $0403D4E8; { Fallback throttle down button }
  DIBUTTON_FLYINGC_GLANCE_LEFT_LINK       = $0407C4E4; { Fallback Glance Left button }
  DIBUTTON_FLYINGC_GLANCE_RIGHT_LINK      = $0407C4EC; { Fallback Glance Right button }
  DIBUTTON_FLYINGC_GLANCE_UP_LINK         = $0407C4E0; { Fallback Glance Up button }
  DIBUTTON_FLYINGC_GLANCE_DOWN_LINK       = $0407C4E8; { Fallback Glance Down button }
  DIBUTTON_FLYINGC_DEVICE                 = $040044FE; { Show input device and controls }
  DIBUTTON_FLYINGC_PAUSE                  = $040044FC; { Start / Pause / Restart game }

(*--- Flight Simulator - Military
      Aerial combat is the primary objective  ---*)
  DIVIRTUAL_FLYING_MILITARY               = $05000000;
  DIAXIS_FLYINGM_BANK                     = $05008A01; { Bank - Roll ship left / right }
  DIAXIS_FLYINGM_PITCH                    = $05010A02; { Pitch - Nose up / down }
  DIAXIS_FLYINGM_THROTTLE                 = $05039203; { Throttle - faster / slower }
  DIBUTTON_FLYINGM_FIRE                   = $05000C01; { Fire }
  DIBUTTON_FLYINGM_WEAPONS                = $05000C02; { Select next weapon }
  DIBUTTON_FLYINGM_TARGET                 = $05000C03; { Selects next available target }
  DIBUTTON_FLYINGM_MENU                   = $050004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_FLYINGM_GLANCE              = $05004601; { Look around }
  DIBUTTON_FLYINGM_COUNTER                = $05005C04; { Activate counter measures }
  DIAXIS_FLYINGM_RUDDER                   = $05024A04; { Rudder - Yaw ship left/right }
  DIAXIS_FLYINGM_BRAKE                    = $05046205; { Brake-axis }
  DIBUTTON_FLYINGM_VIEW                   = $05006405; { Cycle through view options }
  DIBUTTON_FLYINGM_DISPLAY                = $05006406; { Select next dashboard option }
  DIAXIS_FLYINGM_FLAPS                    = $05055206; { Flaps }
  DIBUTTON_FLYINGM_FLAPSUP                = $05005407; { Increment stepping up until fully retracted }
  DIBUTTON_FLYINGM_FLAPSDOWN              = $05005408; { Decrement stepping down until fully extended }
  DIBUTTON_FLYINGM_FIRESECONDARY          = $05004C09; { Alternative fire button }
  DIBUTTON_FLYINGM_GEAR                   = $0500640A; { Gear up / down }
  DIBUTTON_FLYINGM_BRAKE_LINK             = $050464E0; { Fallback brake button }
  DIBUTTON_FLYINGM_FASTER_LINK            = $0503D4E0; { Fallback throttle up button }
  DIBUTTON_FLYINGM_SLOWER_LINK            = $0503D4E8; { Fallback throttle down button }
  DIBUTTON_FLYINGM_GLANCE_LEFT_LINK       = $0507C4E4; { Fallback Glance Left button }
  DIBUTTON_FLYINGM_GLANCE_RIGHT_LINK      = $0507C4EC; { Fallback Glance Right button }
  DIBUTTON_FLYINGM_GLANCE_UP_LINK         = $0507C4E0; { Fallback Glance Up button }
  DIBUTTON_FLYINGM_GLANCE_DOWN_LINK       = $0507C4E8; { Fallback Glance Down button }
  DIBUTTON_FLYINGM_DEVICE                 = $050044FE; { Show input device and controls }
  DIBUTTON_FLYINGM_PAUSE                  = $050044FC; { Start / Pause / Restart game }

(*--- Flight Simulator - Combat Helicopter
      Combat from helicopter is primary objective  ---*)
  DIVIRTUAL_FLYING_HELICOPTER             = $06000000;
  DIAXIS_FLYINGH_BANK                     = $06008A01; { Bank - Roll ship left / right }
  DIAXIS_FLYINGH_PITCH                    = $06010A02; { Pitch - Nose up / down }
  DIAXIS_FLYINGH_COLLECTIVE               = $06018A03; { Collective - Blade pitch/power }
  DIBUTTON_FLYINGH_FIRE                   = $06001401; { Fire }
  DIBUTTON_FLYINGH_WEAPONS                = $06001402; { Select next weapon }
  DIBUTTON_FLYINGH_TARGET                 = $06001403; { Selects next available target }
  DIBUTTON_FLYINGH_MENU                   = $060004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_FLYINGH_GLANCE              = $06004601; { Look around }
  DIAXIS_FLYINGH_TORQUE                   = $06025A04; { Torque - Rotate ship around left / right axis }
  DIAXIS_FLYINGH_THROTTLE                 = $0603DA05; { Throttle }
  DIBUTTON_FLYINGH_COUNTER                = $06005404; { Activate counter measures }
  DIBUTTON_FLYINGH_VIEW                   = $06006405; { Cycle through view options }
  DIBUTTON_FLYINGH_GEAR                   = $06006406; { Gear up / down }
  DIBUTTON_FLYINGH_FIRESECONDARY          = $06004C07; { Alternative fire button }
  DIBUTTON_FLYINGH_FASTER_LINK            = $0603DCE0; { Fallback throttle up button }
  DIBUTTON_FLYINGH_SLOWER_LINK            = $0603DCE8; { Fallback throttle down button }
  DIBUTTON_FLYINGH_GLANCE_LEFT_LINK       = $0607C4E4; { Fallback Glance Left button }
  DIBUTTON_FLYINGH_GLANCE_RIGHT_LINK      = $0607C4EC; { Fallback Glance Right button }
  DIBUTTON_FLYINGH_GLANCE_UP_LINK         = $0607C4E0; { Fallback Glance Up button }
  DIBUTTON_FLYINGH_GLANCE_DOWN_LINK       = $0607C4E8; { Fallback Glance Down button }
  DIBUTTON_FLYINGH_DEVICE                 = $060044FE; { Show input device and controls }
  DIBUTTON_FLYINGH_PAUSE                  = $060044FC; { Start / Pause / Restart game }

(*--- Space Simulator - Combat
      Space Simulator with weapons  ---*)
  DIVIRTUAL_SPACESIM                      = $07000000;
  DIAXIS_SPACESIM_LATERAL                 = $07008201; { Move ship left / right }
  DIAXIS_SPACESIM_MOVE                    = $07010202; { Move ship forward/backward }
  DIAXIS_SPACESIM_THROTTLE                = $07038203; { Throttle - Engine speed }
  DIBUTTON_SPACESIM_FIRE                  = $07000401; { Fire }
  DIBUTTON_SPACESIM_WEAPONS               = $07000402; { Select next weapon }
  DIBUTTON_SPACESIM_TARGET                = $07000403; { Selects next available target }
  DIBUTTON_SPACESIM_MENU                  = $070004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_SPACESIM_GLANCE             = $07004601; { Look around }
  DIAXIS_SPACESIM_CLIMB                   = $0701C204; { Climb - Pitch ship up/down }
  DIAXIS_SPACESIM_ROTATE                  = $07024205; { Rotate - Turn ship left/right }
  DIBUTTON_SPACESIM_VIEW                  = $07004404; { Cycle through view options }
  DIBUTTON_SPACESIM_DISPLAY               = $07004405; { Select next dashboard / heads up display option }
  DIBUTTON_SPACESIM_RAISE                 = $07004406; { Raise ship while maintaining current pitch }
  DIBUTTON_SPACESIM_LOWER                 = $07004407; { Lower ship while maintaining current pitch }
  DIBUTTON_SPACESIM_GEAR                  = $07004408; { Gear up / down }
  DIBUTTON_SPACESIM_FIRESECONDARY         = $07004409; { Alternative fire button }
  DIBUTTON_SPACESIM_LEFT_LINK             = $0700C4E4; { Fallback move left button }
  DIBUTTON_SPACESIM_RIGHT_LINK            = $0700C4EC; { Fallback move right button }
  DIBUTTON_SPACESIM_FORWARD_LINK          = $070144E0; { Fallback move forward button }
  DIBUTTON_SPACESIM_BACKWARD_LINK         = $070144E8; { Fallback move backwards button }
  DIBUTTON_SPACESIM_FASTER_LINK           = $0703C4E0; { Fallback throttle up button }
  DIBUTTON_SPACESIM_SLOWER_LINK           = $0703C4E8; { Fallback throttle down button }
  DIBUTTON_SPACESIM_TURN_LEFT_LINK        = $070244E4; { Fallback turn left button }
  DIBUTTON_SPACESIM_TURN_RIGHT_LINK       = $070244EC; { Fallback turn right button }
  DIBUTTON_SPACESIM_GLANCE_LEFT_LINK      = $0707C4E4; { Fallback Glance Left button }
  DIBUTTON_SPACESIM_GLANCE_RIGHT_LINK     = $0707C4EC; { Fallback Glance Right button }
  DIBUTTON_SPACESIM_GLANCE_UP_LINK        = $0707C4E0; { Fallback Glance Up button }
  DIBUTTON_SPACESIM_GLANCE_DOWN_LINK      = $0707C4E8; { Fallback Glance Down button }
  DIBUTTON_SPACESIM_DEVICE                = $070044FE; { Show input device and controls }
  DIBUTTON_SPACESIM_PAUSE                 = $070044FC; { Start / Pause / Restart game }

(*--- Fighting - First Person
      Hand to Hand combat is primary objective  ---*)
  DIVIRTUAL_FIGHTING_HAND2HAND            = $08000000;
  DIAXIS_FIGHTINGH_LATERAL                = $08008201; { Sidestep left/right }
  DIAXIS_FIGHTINGH_MOVE                   = $08010202; { Move forward/backward }
  DIBUTTON_FIGHTINGH_PUNCH                = $08000401; { Punch }
  DIBUTTON_FIGHTINGH_KICK                 = $08000402; { Kick }
  DIBUTTON_FIGHTINGH_BLOCK                = $08000403; { Block }
  DIBUTTON_FIGHTINGH_CROUCH               = $08000404; { Crouch }
  DIBUTTON_FIGHTINGH_JUMP                 = $08000405; { Jump }
  DIBUTTON_FIGHTINGH_SPECIAL1             = $08000406; { Apply first special move }
  DIBUTTON_FIGHTINGH_SPECIAL2             = $08000407; { Apply second special move }
  DIBUTTON_FIGHTINGH_MENU                 = $080004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_FIGHTINGH_SELECT               = $08004408; { Select special move }
  DIHATSWITCH_FIGHTINGH_SLIDE             = $08004601; { Look around }
  DIBUTTON_FIGHTINGH_DISPLAY              = $08004409; { Shows next on-screen display option }
  DIAXIS_FIGHTINGH_ROTATE                 = $08024203; { Rotate - Turn body left/right }
  DIBUTTON_FIGHTINGH_DODGE                = $0800440A; { Dodge }
  DIBUTTON_FIGHTINGH_LEFT_LINK            = $0800C4E4; { Fallback left sidestep button }
  DIBUTTON_FIGHTINGH_RIGHT_LINK           = $0800C4EC; { Fallback right sidestep button }
  DIBUTTON_FIGHTINGH_FORWARD_LINK         = $080144E0; { Fallback forward button }
  DIBUTTON_FIGHTINGH_BACKWARD_LINK        = $080144E8; { Fallback backward button }
  DIBUTTON_FIGHTINGH_DEVICE               = $080044FE; { Show input device and controls }
  DIBUTTON_FIGHTINGH_PAUSE                = $080044FC; { Start / Pause / Restart game }

(*--- Fighting - First Person Shooting
      Navigation and combat are primary objectives  ---*)
  DIVIRTUAL_FIGHTING_FPS                  = $09000000;
  DIAXIS_FPS_ROTATE                       = $09008201; { Rotate character left/right }
  DIAXIS_FPS_MOVE                         = $09010202; { Move forward/backward }
  DIBUTTON_FPS_FIRE                       = $09000401; { Fire }
  DIBUTTON_FPS_WEAPONS                    = $09000402; { Select next weapon }
  DIBUTTON_FPS_APPLY                      = $09000403; { Use item }
  DIBUTTON_FPS_SELECT                     = $09000404; { Select next inventory item }
  DIBUTTON_FPS_CROUCH                     = $09000405; { Crouch/ climb down/ swim down }
  DIBUTTON_FPS_JUMP                       = $09000406; { Jump/ climb up/ swim up }
  DIAXIS_FPS_LOOKUPDOWN                   = $09018203; { Look up / down  }
  DIBUTTON_FPS_STRAFE                     = $09000407; { Enable strafing while active }
  DIBUTTON_FPS_MENU                       = $090004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_FPS_GLANCE                  = $09004601; { Look around }
  DIBUTTON_FPS_DISPLAY                    = $09004408; { Shows next on-screen display option/ map }
  DIAXIS_FPS_SIDESTEP                     = $09024204; { Sidestep }
  DIBUTTON_FPS_DODGE                      = $09004409; { Dodge }
  DIBUTTON_FPS_GLANCEL                    = $0900440A; { Glance Left }
  DIBUTTON_FPS_GLANCER                    = $0900440B; { Glance Right }
  DIBUTTON_FPS_FIRESECONDARY              = $0900440C; { Alternative fire button }
  DIBUTTON_FPS_ROTATE_LEFT_LINK           = $0900C4E4; { Fallback rotate left button }
  DIBUTTON_FPS_ROTATE_RIGHT_LINK          = $0900C4EC; { Fallback rotate right button }
  DIBUTTON_FPS_FORWARD_LINK               = $090144E0; { Fallback forward button }
  DIBUTTON_FPS_BACKWARD_LINK              = $090144E8; { Fallback backward button }
  DIBUTTON_FPS_GLANCE_UP_LINK             = $0901C4E0; { Fallback look up button }
  DIBUTTON_FPS_GLANCE_DOWN_LINK           = $0901C4E8; { Fallback look down button }
  DIBUTTON_FPS_STEP_LEFT_LINK             = $090244E4; { Fallback step left button }
  DIBUTTON_FPS_STEP_RIGHT_LINK            = $090244EC; { Fallback step right button }
  DIBUTTON_FPS_DEVICE                     = $090044FE; { Show input device and controls }
  DIBUTTON_FPS_PAUSE                      = $090044FC; { Start / Pause / Restart game }

(*--- Fighting - Third Person action
      Perspective of camera is behind the main character  ---*)
  DIVIRTUAL_FIGHTING_THIRDPERSON          = $0A000000;
  DIAXIS_TPS_TURN                         = $0A020201; { Turn left/right }
  DIAXIS_TPS_MOVE                         = $0A010202; { Move forward/backward }
  DIBUTTON_TPS_RUN                        = $0A000401; { Run or walk toggle switch }
  DIBUTTON_TPS_ACTION                     = $0A000402; { Action Button }
  DIBUTTON_TPS_SELECT                     = $0A000403; { Select next weapon }
  DIBUTTON_TPS_USE                        = $0A000404; { Use inventory item currently selected }
  DIBUTTON_TPS_JUMP                       = $0A000405; { Character Jumps }
  DIBUTTON_TPS_MENU                       = $0A0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_TPS_GLANCE                  = $0A004601; { Look around }
  DIBUTTON_TPS_VIEW                       = $0A004406; { Select camera view }
  DIBUTTON_TPS_STEPLEFT                   = $0A004407; { Character takes a left step }
  DIBUTTON_TPS_STEPRIGHT                  = $0A004408; { Character takes a right step }
  DIAXIS_TPS_STEP                         = $0A00C203; { Character steps left/right }
  DIBUTTON_TPS_DODGE                      = $0A004409; { Character dodges or ducks }
  DIBUTTON_TPS_INVENTORY                  = $0A00440A; { Cycle through inventory }
  DIBUTTON_TPS_TURN_LEFT_LINK             = $0A0244E4; { Fallback turn left button }
  DIBUTTON_TPS_TURN_RIGHT_LINK            = $0A0244EC; { Fallback turn right button }
  DIBUTTON_TPS_FORWARD_LINK               = $0A0144E0; { Fallback forward button }
  DIBUTTON_TPS_BACKWARD_LINK              = $0A0144E8; { Fallback backward button }
  DIBUTTON_TPS_GLANCE_UP_LINK             = $0A07C4E0; { Fallback look up button }
  DIBUTTON_TPS_GLANCE_DOWN_LINK           = $0A07C4E8; { Fallback look down button }
  DIBUTTON_TPS_GLANCE_LEFT_LINK           = $0A07C4E4; { Fallback glance up button }
  DIBUTTON_TPS_GLANCE_RIGHT_LINK          = $0A07C4EC; { Fallback glance right button }
  DIBUTTON_TPS_DEVICE                     = $0A0044FE; { Show input device and controls }
  DIBUTTON_TPS_PAUSE                      = $0A0044FC; { Start / Pause / Restart game }

(*--- Strategy - Role Playing
      Navigation and problem solving are primary actions  ---*)
  DIVIRTUAL_STRATEGY_ROLEPLAYING          = $0B000000;
  DIAXIS_STRATEGYR_LATERAL                = $0B008201; { sidestep - left/right }
  DIAXIS_STRATEGYR_MOVE                   = $0B010202; { move forward/backward }
  DIBUTTON_STRATEGYR_GET                  = $0B000401; { Acquire item }
  DIBUTTON_STRATEGYR_APPLY                = $0B000402; { Use selected item }
  DIBUTTON_STRATEGYR_SELECT               = $0B000403; { Select nextitem }
  DIBUTTON_STRATEGYR_ATTACK               = $0B000404; { Attack }
  DIBUTTON_STRATEGYR_CAST                 = $0B000405; { Cast Spell }
  DIBUTTON_STRATEGYR_CROUCH               = $0B000406; { Crouch }
  DIBUTTON_STRATEGYR_JUMP                 = $0B000407; { Jump }
  DIBUTTON_STRATEGYR_MENU                 = $0B0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_STRATEGYR_GLANCE            = $0B004601; { Look around }
  DIBUTTON_STRATEGYR_MAP                  = $0B004408; { Cycle through map options }
  DIBUTTON_STRATEGYR_DISPLAY              = $0B004409; { Shows next on-screen display option }
  DIAXIS_STRATEGYR_ROTATE                 = $0B024203; { Turn body left/right }
  DIBUTTON_STRATEGYR_LEFT_LINK            = $0B00C4E4; { Fallback sidestep left button }
  DIBUTTON_STRATEGYR_RIGHT_LINK           = $0B00C4EC; { Fallback sidestep right button }
  DIBUTTON_STRATEGYR_FORWARD_LINK         = $0B0144E0; { Fallback move forward button }
  DIBUTTON_STRATEGYR_BACK_LINK            = $0B0144E8; { Fallback move backward button }
  DIBUTTON_STRATEGYR_ROTATE_LEFT_LINK     = $0B0244E4; { Fallback turn body left button }
  DIBUTTON_STRATEGYR_ROTATE_RIGHT_LINK    = $0B0244EC; { Fallback turn body right button }
  DIBUTTON_STRATEGYR_DEVICE               = $0B0044FE; { Show input device and controls }
  DIBUTTON_STRATEGYR_PAUSE                = $0B0044FC; { Start / Pause / Restart game }

(*--- Strategy - Turn based
      Navigation and problem solving are primary actions  ---*)
  DIVIRTUAL_STRATEGY_TURN                 = $0C000000;
  DIAXIS_STRATEGYT_LATERAL                = $0C008201; { Sidestep left/right }
  DIAXIS_STRATEGYT_MOVE                   = $0C010202; { Move forward/backwards }
  DIBUTTON_STRATEGYT_SELECT               = $0C000401; { Select unit or object }
  DIBUTTON_STRATEGYT_INSTRUCT             = $0C000402; { Cycle through instructions }
  DIBUTTON_STRATEGYT_APPLY                = $0C000403; { Apply selected instruction }
  DIBUTTON_STRATEGYT_TEAM                 = $0C000404; { Select next team / cycle through all }
  DIBUTTON_STRATEGYT_TURN                 = $0C000405; { Indicate turn over }
  DIBUTTON_STRATEGYT_MENU                 = $0C0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_STRATEGYT_ZOOM                 = $0C004406; { Zoom - in / out }
  DIBUTTON_STRATEGYT_MAP                  = $0C004407; { cycle through map options }
  DIBUTTON_STRATEGYT_DISPLAY              = $0C004408; { shows next on-screen display options }
  DIBUTTON_STRATEGYT_LEFT_LINK            = $0C00C4E4; { Fallback sidestep left button }
  DIBUTTON_STRATEGYT_RIGHT_LINK           = $0C00C4EC; { Fallback sidestep right button }
  DIBUTTON_STRATEGYT_FORWARD_LINK         = $0C0144E0; { Fallback move forward button }
  DIBUTTON_STRATEGYT_BACK_LINK            = $0C0144E8; { Fallback move back button }
  DIBUTTON_STRATEGYT_DEVICE               = $0C0044FE; { Show input device and controls }
  DIBUTTON_STRATEGYT_PAUSE                = $0C0044FC; { Start / Pause / Restart game }

(*--- Sports - Hunting
      Hunting                ---*)
  DIVIRTUAL_SPORTS_HUNTING                = $0D000000;
  DIAXIS_HUNTING_LATERAL                  = $0D008201; { sidestep left/right }
  DIAXIS_HUNTING_MOVE                     = $0D010202; { move forward/backwards }
  DIBUTTON_HUNTING_FIRE                   = $0D000401; { Fire selected weapon }
  DIBUTTON_HUNTING_AIM                    = $0D000402; { Select aim/move }
  DIBUTTON_HUNTING_WEAPON                 = $0D000403; { Select next weapon }
  DIBUTTON_HUNTING_BINOCULAR              = $0D000404; { Look through Binoculars }
  DIBUTTON_HUNTING_CALL                   = $0D000405; { Make animal call }
  DIBUTTON_HUNTING_MAP                    = $0D000406; { View Map }
  DIBUTTON_HUNTING_SPECIAL                = $0D000407; { Special game operation }
  DIBUTTON_HUNTING_MENU                   = $0D0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_HUNTING_GLANCE              = $0D004601; { Look around }
  DIBUTTON_HUNTING_DISPLAY                = $0D004408; { show next on-screen display option }
  DIAXIS_HUNTING_ROTATE                   = $0D024203; { Turn body left/right }
  DIBUTTON_HUNTING_CROUCH                 = $0D004409; { Crouch/ Climb / Swim down }
  DIBUTTON_HUNTING_JUMP                   = $0D00440A; { Jump/ Climb up / Swim up }
  DIBUTTON_HUNTING_FIRESECONDARY          = $0D00440B; { Alternative fire button }
  DIBUTTON_HUNTING_LEFT_LINK              = $0D00C4E4; { Fallback sidestep left button }
  DIBUTTON_HUNTING_RIGHT_LINK             = $0D00C4EC; { Fallback sidestep right button }
  DIBUTTON_HUNTING_FORWARD_LINK           = $0D0144E0; { Fallback move forward button }
  DIBUTTON_HUNTING_BACK_LINK              = $0D0144E8; { Fallback move back button }
  DIBUTTON_HUNTING_ROTATE_LEFT_LINK       = $0D0244E4; { Fallback turn body left button }
  DIBUTTON_HUNTING_ROTATE_RIGHT_LINK      = $0D0244EC; { Fallback turn body right button }
  DIBUTTON_HUNTING_DEVICE                 = $0D0044FE; { Show input device and controls }
  DIBUTTON_HUNTING_PAUSE                  = $0D0044FC; { Start / Pause / Restart game }

(*--- Sports - Fishing
      Catching Fish is primary objective   ---*)
  DIVIRTUAL_SPORTS_FISHING                = $0E000000;
  DIAXIS_FISHING_LATERAL                  = $0E008201; { sidestep left/right }
  DIAXIS_FISHING_MOVE                     = $0E010202; { move forward/backwards }
  DIBUTTON_FISHING_CAST                   = $0E000401; { Cast line }
  DIBUTTON_FISHING_TYPE                   = $0E000402; { Select cast type }
  DIBUTTON_FISHING_BINOCULAR              = $0E000403; { Look through Binocular }
  DIBUTTON_FISHING_BAIT                   = $0E000404; { Select type of Bait }
  DIBUTTON_FISHING_MAP                    = $0E000405; { View Map }
  DIBUTTON_FISHING_MENU                   = $0E0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_FISHING_GLANCE              = $0E004601; { Look around }
  DIBUTTON_FISHING_DISPLAY                = $0E004406; { Show next on-screen display option }
  DIAXIS_FISHING_ROTATE                   = $0E024203; { Turn character left / right }
  DIBUTTON_FISHING_CROUCH                 = $0E004407; { Crouch/ Climb / Swim down }
  DIBUTTON_FISHING_JUMP                   = $0E004408; { Jump/ Climb up / Swim up }
  DIBUTTON_FISHING_LEFT_LINK              = $0E00C4E4; { Fallback sidestep left button }
  DIBUTTON_FISHING_RIGHT_LINK             = $0E00C4EC; { Fallback sidestep right button }
  DIBUTTON_FISHING_FORWARD_LINK           = $0E0144E0; { Fallback move forward button }
  DIBUTTON_FISHING_BACK_LINK              = $0E0144E8; { Fallback move back button }
  DIBUTTON_FISHING_ROTATE_LEFT_LINK       = $0E0244E4; { Fallback turn body left button }
  DIBUTTON_FISHING_ROTATE_RIGHT_LINK      = $0E0244EC; { Fallback turn body right button }
  DIBUTTON_FISHING_DEVICE                 = $0E0044FE; { Show input device and controls }
  DIBUTTON_FISHING_PAUSE                  = $0E0044FC; { Start / Pause / Restart game }

(*--- Sports - Baseball - Batting
      Batter control is primary objective  ---*)
  DIVIRTUAL_SPORTS_BASEBALL_BAT           = $0F000000;
  DIAXIS_BASEBALLB_LATERAL                = $0F008201; { Aim left / right }
  DIAXIS_BASEBALLB_MOVE                   = $0F010202; { Aim up / down }
  DIBUTTON_BASEBALLB_SELECT               = $0F000401; { cycle through swing options }
  DIBUTTON_BASEBALLB_NORMAL               = $0F000402; { normal swing }
  DIBUTTON_BASEBALLB_POWER                = $0F000403; { swing for the fence }
  DIBUTTON_BASEBALLB_BUNT                 = $0F000404; { bunt }
  DIBUTTON_BASEBALLB_STEAL                = $0F000405; { Base runner attempts to steal a base }
  DIBUTTON_BASEBALLB_BURST                = $0F000406; { Base runner invokes burst of speed }
  DIBUTTON_BASEBALLB_SLIDE                = $0F000407; { Base runner slides into base }
  DIBUTTON_BASEBALLB_CONTACT              = $0F000408; { Contact swing }
  DIBUTTON_BASEBALLB_MENU                 = $0F0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_BASEBALLB_NOSTEAL              = $0F004409; { Base runner goes back to a base }
  DIBUTTON_BASEBALLB_BOX                  = $0F00440A; { Enter or exit batting box }
  DIBUTTON_BASEBALLB_LEFT_LINK            = $0F00C4E4; { Fallback sidestep left button }
  DIBUTTON_BASEBALLB_RIGHT_LINK           = $0F00C4EC; { Fallback sidestep right button }
  DIBUTTON_BASEBALLB_FORWARD_LINK         = $0F0144E0; { Fallback move forward button }
  DIBUTTON_BASEBALLB_BACK_LINK            = $0F0144E8; { Fallback move back button }
  DIBUTTON_BASEBALLB_DEVICE               = $0F0044FE; { Show input device and controls }
  DIBUTTON_BASEBALLB_PAUSE                = $0F0044FC; { Start / Pause / Restart game }

(*--- Sports - Baseball - Pitching
      Pitcher control is primary objective   ---*)
  DIVIRTUAL_SPORTS_BASEBALL_PITCH         = $10000000;
  DIAXIS_BASEBALLP_LATERAL                = $10008201; { Aim left / right }
  DIAXIS_BASEBALLP_MOVE                   = $10010202; { Aim up / down }
  DIBUTTON_BASEBALLP_SELECT               = $10000401; { cycle through pitch selections }
  DIBUTTON_BASEBALLP_PITCH                = $10000402; { throw pitch }
  DIBUTTON_BASEBALLP_BASE                 = $10000403; { select base to throw to }
  DIBUTTON_BASEBALLP_THROW                = $10000404; { throw to base }
  DIBUTTON_BASEBALLP_FAKE                 = $10000405; { Fake a throw to a base }
  DIBUTTON_BASEBALLP_MENU                 = $100004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_BASEBALLP_WALK                 = $10004406; { Throw intentional walk / pitch out }
  DIBUTTON_BASEBALLP_LOOK                 = $10004407; { Look at runners on bases }
  DIBUTTON_BASEBALLP_LEFT_LINK            = $1000C4E4; { Fallback sidestep left button }
  DIBUTTON_BASEBALLP_RIGHT_LINK           = $1000C4EC; { Fallback sidestep right button }
  DIBUTTON_BASEBALLP_FORWARD_LINK         = $100144E0; { Fallback move forward button }
  DIBUTTON_BASEBALLP_BACK_LINK            = $100144E8; { Fallback move back button }
  DIBUTTON_BASEBALLP_DEVICE               = $100044FE; { Show input device and controls }
  DIBUTTON_BASEBALLP_PAUSE                = $100044FC; { Start / Pause / Restart game }

(*--- Sports - Baseball - Fielding
      Fielder control is primary objective  ---*)
  DIVIRTUAL_SPORTS_BASEBALL_FIELD         = $11000000;
  DIAXIS_BASEBALLF_LATERAL                = $11008201; { Aim left / right }
  DIAXIS_BASEBALLF_MOVE                   = $11010202; { Aim up / down }
  DIBUTTON_BASEBALLF_NEAREST              = $11000401; { Switch to fielder nearest to the ball }
  DIBUTTON_BASEBALLF_THROW1               = $11000402; { Make conservative throw }
  DIBUTTON_BASEBALLF_THROW2               = $11000403; { Make aggressive throw }
  DIBUTTON_BASEBALLF_BURST                = $11000404; { Invoke burst of speed }
  DIBUTTON_BASEBALLF_JUMP                 = $11000405; { Jump to catch ball }
  DIBUTTON_BASEBALLF_DIVE                 = $11000406; { Dive to catch ball }
  DIBUTTON_BASEBALLF_MENU                 = $110004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_BASEBALLF_SHIFTIN              = $11004407; { Shift the infield positioning }
  DIBUTTON_BASEBALLF_SHIFTOUT             = $11004408; { Shift the outfield positioning }
  DIBUTTON_BASEBALLF_AIM_LEFT_LINK        = $1100C4E4; { Fallback aim left button }
  DIBUTTON_BASEBALLF_AIM_RIGHT_LINK       = $1100C4EC; { Fallback aim right button }
  DIBUTTON_BASEBALLF_FORWARD_LINK         = $110144E0; { Fallback move forward button }
  DIBUTTON_BASEBALLF_BACK_LINK            = $110144E8; { Fallback move back button }
  DIBUTTON_BASEBALLF_DEVICE               = $110044FE; { Show input device and controls }
  DIBUTTON_BASEBALLF_PAUSE                = $110044FC; { Start / Pause / Restart game }

(*--- Sports - Basketball - Offense
      Offense  ---*)
  DIVIRTUAL_SPORTS_BASKETBALL_OFFENSE     = $12000000;
  DIAXIS_BBALLO_LATERAL                   = $12008201; { left / right }
  DIAXIS_BBALLO_MOVE                      = $12010202; { up / down }
  DIBUTTON_BBALLO_SHOOT                   = $12000401; { shoot basket }
  DIBUTTON_BBALLO_DUNK                    = $12000402; { dunk basket }
  DIBUTTON_BBALLO_PASS                    = $12000403; { throw pass }
  DIBUTTON_BBALLO_FAKE                    = $12000404; { fake shot or pass }
  DIBUTTON_BBALLO_SPECIAL                 = $12000405; { apply special move }
  DIBUTTON_BBALLO_PLAYER                  = $12000406; { select next player }
  DIBUTTON_BBALLO_BURST                   = $12000407; { invoke burst }
  DIBUTTON_BBALLO_CALL                    = $12000408; { call for ball / pass to me }
  DIBUTTON_BBALLO_MENU                    = $120004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_BBALLO_GLANCE               = $12004601; { scroll view }
  DIBUTTON_BBALLO_SCREEN                  = $12004409; { Call for screen }
  DIBUTTON_BBALLO_PLAY                    = $1200440A; { Call for specific offensive play }
  DIBUTTON_BBALLO_JAB                     = $1200440B; { Initiate fake drive to basket }
  DIBUTTON_BBALLO_POST                    = $1200440C; { Perform post move }
  DIBUTTON_BBALLO_TIMEOUT                 = $1200440D; { Time Out }
  DIBUTTON_BBALLO_SUBSTITUTE              = $1200440E; { substitute one player for another }
  DIBUTTON_BBALLO_LEFT_LINK               = $1200C4E4; { Fallback sidestep left button }
  DIBUTTON_BBALLO_RIGHT_LINK              = $1200C4EC; { Fallback sidestep right button }
  DIBUTTON_BBALLO_FORWARD_LINK            = $120144E0; { Fallback move forward button }
  DIBUTTON_BBALLO_BACK_LINK               = $120144E8; { Fallback move back button }
  DIBUTTON_BBALLO_DEVICE                  = $120044FE; { Show input device and controls }
  DIBUTTON_BBALLO_PAUSE                   = $120044FC; { Start / Pause / Restart game }

(*--- Sports - Basketball - Defense
      Defense  ---*)
  DIVIRTUAL_SPORTS_BASKETBALL_DEFENSE     = $13000000;
  DIAXIS_BBALLD_LATERAL                   = $13008201; { left / right }
  DIAXIS_BBALLD_MOVE                      = $13010202; { up / down }
  DIBUTTON_BBALLD_JUMP                    = $13000401; { jump to block shot }
  DIBUTTON_BBALLD_STEAL                   = $13000402; { attempt to steal ball }
  DIBUTTON_BBALLD_FAKE                    = $13000403; { fake block or steal }
  DIBUTTON_BBALLD_SPECIAL                 = $13000404; { apply special move }
  DIBUTTON_BBALLD_PLAYER                  = $13000405; { select next player }
  DIBUTTON_BBALLD_BURST                   = $13000406; { invoke burst }
  DIBUTTON_BBALLD_PLAY                    = $13000407; { call for specific defensive play }
  DIBUTTON_BBALLD_MENU                    = $130004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_BBALLD_GLANCE               = $13004601; { scroll view }
  DIBUTTON_BBALLD_TIMEOUT                 = $13004408; { Time Out }
  DIBUTTON_BBALLD_SUBSTITUTE              = $13004409; { substitute one player for another }
  DIBUTTON_BBALLD_LEFT_LINK               = $1300C4E4; { Fallback sidestep left button }
  DIBUTTON_BBALLD_RIGHT_LINK              = $1300C4EC; { Fallback sidestep right button }
  DIBUTTON_BBALLD_FORWARD_LINK            = $130144E0; { Fallback move forward button }
  DIBUTTON_BBALLD_BACK_LINK               = $130144E8; { Fallback move back button }
  DIBUTTON_BBALLD_DEVICE                  = $130044FE; { Show input device and controls }
  DIBUTTON_BBALLD_PAUSE                   = $130044FC; { Start / Pause / Restart game }

(*--- Sports - Football - Play
      Play selection  ---*)
  DIVIRTUAL_SPORTS_FOOTBALL_FIELD         = $14000000;
  DIBUTTON_FOOTBALLP_PLAY                 = $14000401; { cycle through available plays }
  DIBUTTON_FOOTBALLP_SELECT               = $14000402; { select play }
  DIBUTTON_FOOTBALLP_HELP                 = $14000403; { Bring up pop-up help }
  DIBUTTON_FOOTBALLP_MENU                 = $140004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_FOOTBALLP_DEVICE               = $140044FE; { Show input device and controls }
  DIBUTTON_FOOTBALLP_PAUSE                = $140044FC; { Start / Pause / Restart game }

(*--- Sports - Football - QB
      Offense: Quarterback / Kicker  ---*)
  DIVIRTUAL_SPORTS_FOOTBALL_QBCK          = $15000000;
  DIAXIS_FOOTBALLQ_LATERAL                = $15008201; { Move / Aim: left / right }
  DIAXIS_FOOTBALLQ_MOVE                   = $15010202; { Move / Aim: up / down }
  DIBUTTON_FOOTBALLQ_SELECT               = $15000401; { Select }
  DIBUTTON_FOOTBALLQ_SNAP                 = $15000402; { snap ball - start play }
  DIBUTTON_FOOTBALLQ_JUMP                 = $15000403; { jump over defender }
  DIBUTTON_FOOTBALLQ_SLIDE                = $15000404; { Dive/Slide }
  DIBUTTON_FOOTBALLQ_PASS                 = $15000405; { throws pass to receiver }
  DIBUTTON_FOOTBALLQ_FAKE                 = $15000406; { pump fake pass or fake kick }
  DIBUTTON_FOOTBALLQ_MENU                 = $150004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_FOOTBALLQ_FAKESNAP             = $15004407; { Fake snap  }
  DIBUTTON_FOOTBALLQ_MOTION               = $15004408; { Send receivers in motion }
  DIBUTTON_FOOTBALLQ_AUDIBLE              = $15004409; { Change offensive play at line of scrimmage }
  DIBUTTON_FOOTBALLQ_LEFT_LINK            = $1500C4E4; { Fallback sidestep left button }
  DIBUTTON_FOOTBALLQ_RIGHT_LINK           = $1500C4EC; { Fallback sidestep right button }
  DIBUTTON_FOOTBALLQ_FORWARD_LINK         = $150144E0; { Fallback move forward button }
  DIBUTTON_FOOTBALLQ_BACK_LINK            = $150144E8; { Fallback move back button }
  DIBUTTON_FOOTBALLQ_DEVICE               = $150044FE; { Show input device and controls }
  DIBUTTON_FOOTBALLQ_PAUSE                = $150044FC; { Start / Pause / Restart game }

(*--- Sports - Football - Offense
      Offense - Runner  ---*)
  DIVIRTUAL_SPORTS_FOOTBALL_OFFENSE       = $16000000;
  DIAXIS_FOOTBALLO_LATERAL                = $16008201; { Move / Aim: left / right }
  DIAXIS_FOOTBALLO_MOVE                   = $16010202; { Move / Aim: up / down }
  DIBUTTON_FOOTBALLO_JUMP                 = $16000401; { jump or hurdle over defender }
  DIBUTTON_FOOTBALLO_LEFTARM              = $16000402; { holds out left arm }
  DIBUTTON_FOOTBALLO_RIGHTARM             = $16000403; { holds out right arm }
  DIBUTTON_FOOTBALLO_THROW                = $16000404; { throw pass or lateral ball to another runner }
  DIBUTTON_FOOTBALLO_SPIN                 = $16000405; { Spin to avoid defenders }
  DIBUTTON_FOOTBALLO_MENU                 = $160004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_FOOTBALLO_JUKE                 = $16004406; { Use special move to avoid defenders }
  DIBUTTON_FOOTBALLO_SHOULDER             = $16004407; { Lower shoulder to run over defenders }
  DIBUTTON_FOOTBALLO_TURBO                = $16004408; { Speed burst past defenders }
  DIBUTTON_FOOTBALLO_DIVE                 = $16004409; { Dive over defenders }
  DIBUTTON_FOOTBALLO_ZOOM                 = $1600440A; { Zoom view in / out }
  DIBUTTON_FOOTBALLO_SUBSTITUTE           = $1600440B; { substitute one player for another }
  DIBUTTON_FOOTBALLO_LEFT_LINK            = $1600C4E4; { Fallback sidestep left button }
  DIBUTTON_FOOTBALLO_RIGHT_LINK           = $1600C4EC; { Fallback sidestep right button }
  DIBUTTON_FOOTBALLO_FORWARD_LINK         = $160144E0; { Fallback move forward button }
  DIBUTTON_FOOTBALLO_BACK_LINK            = $160144E8; { Fallback move back button }
  DIBUTTON_FOOTBALLO_DEVICE               = $160044FE; { Show input device and controls }
  DIBUTTON_FOOTBALLO_PAUSE                = $160044FC; { Start / Pause / Restart game }

(*--- Sports - Football - Defense
      Defense     ---*)
  DIVIRTUAL_SPORTS_FOOTBALL_DEFENSE       = $17000000;
  DIAXIS_FOOTBALLD_LATERAL                = $17008201; { Move / Aim: left / right }
  DIAXIS_FOOTBALLD_MOVE                   = $17010202; { Move / Aim: up / down }
  DIBUTTON_FOOTBALLD_PLAY                 = $17000401; { cycle through available plays }
  DIBUTTON_FOOTBALLD_SELECT               = $17000402; { select player closest to the ball }
  DIBUTTON_FOOTBALLD_JUMP                 = $17000403; { jump to intercept or block }
  DIBUTTON_FOOTBALLD_TACKLE               = $17000404; { tackler runner }
  DIBUTTON_FOOTBALLD_FAKE                 = $17000405; { hold down to fake tackle or intercept }
  DIBUTTON_FOOTBALLD_SUPERTACKLE          = $17000406; { Initiate special tackle }
  DIBUTTON_FOOTBALLD_MENU                 = $170004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_FOOTBALLD_SPIN                 = $17004407; { Spin to beat offensive line }
  DIBUTTON_FOOTBALLD_SWIM                 = $17004408; { Swim to beat the offensive line }
  DIBUTTON_FOOTBALLD_BULLRUSH             = $17004409; { Bull rush the offensive line }
  DIBUTTON_FOOTBALLD_RIP                  = $1700440A; { Rip the offensive line }
  DIBUTTON_FOOTBALLD_AUDIBLE              = $1700440B; { Change defensive play at the line of scrimmage }
  DIBUTTON_FOOTBALLD_ZOOM                 = $1700440C; { Zoom view in / out }
  DIBUTTON_FOOTBALLD_SUBSTITUTE           = $1700440D; { substitute one player for another }
  DIBUTTON_FOOTBALLD_LEFT_LINK            = $1700C4E4; { Fallback sidestep left button }
  DIBUTTON_FOOTBALLD_RIGHT_LINK           = $1700C4EC; { Fallback sidestep right button }
  DIBUTTON_FOOTBALLD_FORWARD_LINK         = $170144E0; { Fallback move forward button }
  DIBUTTON_FOOTBALLD_BACK_LINK            = $170144E8; { Fallback move back button }
  DIBUTTON_FOOTBALLD_DEVICE               = $170044FE; { Show input device and controls }
  DIBUTTON_FOOTBALLD_PAUSE                = $170044FC; { Start / Pause / Restart game }

(*--- Sports - Golf
                                ---*)
  DIVIRTUAL_SPORTS_GOLF                   = $18000000;
  DIAXIS_GOLF_LATERAL                     = $18008201; { Move / Aim: left / right }
  DIAXIS_GOLF_MOVE                        = $18010202; { Move / Aim: up / down }
  DIBUTTON_GOLF_SWING                     = $18000401; { swing club }
  DIBUTTON_GOLF_SELECT                    = $18000402; { cycle between: club / swing strength / ball arc / ball spin }
  DIBUTTON_GOLF_UP                        = $18000403; { increase selection }
  DIBUTTON_GOLF_DOWN                      = $18000404; { decrease selection }
  DIBUTTON_GOLF_TERRAIN                   = $18000405; { shows terrain detail }
  DIBUTTON_GOLF_FLYBY                     = $18000406; { view the hole via a flyby }
  DIBUTTON_GOLF_MENU                      = $180004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_GOLF_SCROLL                 = $18004601; { scroll view }
  DIBUTTON_GOLF_ZOOM                      = $18004407; { Zoom view in / out }
  DIBUTTON_GOLF_TIMEOUT                   = $18004408; { Call for time out }
  DIBUTTON_GOLF_SUBSTITUTE                = $18004409; { substitute one player for another }
  DIBUTTON_GOLF_LEFT_LINK                 = $1800C4E4; { Fallback sidestep left button }
  DIBUTTON_GOLF_RIGHT_LINK                = $1800C4EC; { Fallback sidestep right button }
  DIBUTTON_GOLF_FORWARD_LINK              = $180144E0; { Fallback move forward button }
  DIBUTTON_GOLF_BACK_LINK                 = $180144E8; { Fallback move back button }
  DIBUTTON_GOLF_DEVICE                    = $180044FE; { Show input device and controls }
  DIBUTTON_GOLF_PAUSE                     = $180044FC; { Start / Pause / Restart game }

(*--- Sports - Hockey - Offense
      Offense       ---*)
  DIVIRTUAL_SPORTS_HOCKEY_OFFENSE         = $19000000;
  DIAXIS_HOCKEYO_LATERAL                  = $19008201; { Move / Aim: left / right }
  DIAXIS_HOCKEYO_MOVE                     = $19010202; { Move / Aim: up / down }
  DIBUTTON_HOCKEYO_SHOOT                  = $19000401; { Shoot }
  DIBUTTON_HOCKEYO_PASS                   = $19000402; { pass the puck }
  DIBUTTON_HOCKEYO_BURST                  = $19000403; { invoke speed burst }
  DIBUTTON_HOCKEYO_SPECIAL                = $19000404; { invoke special move }
  DIBUTTON_HOCKEYO_FAKE                   = $19000405; { hold down to fake pass or kick }
  DIBUTTON_HOCKEYO_MENU                   = $190004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_HOCKEYO_SCROLL              = $19004601; { scroll view }
  DIBUTTON_HOCKEYO_ZOOM                   = $19004406; { Zoom view in / out }
  DIBUTTON_HOCKEYO_STRATEGY               = $19004407; { Invoke coaching menu for strategy help }
  DIBUTTON_HOCKEYO_TIMEOUT                = $19004408; { Call for time out }
  DIBUTTON_HOCKEYO_SUBSTITUTE             = $19004409; { substitute one player for another }
  DIBUTTON_HOCKEYO_LEFT_LINK              = $1900C4E4; { Fallback sidestep left button }
  DIBUTTON_HOCKEYO_RIGHT_LINK             = $1900C4EC; { Fallback sidestep right button }
  DIBUTTON_HOCKEYO_FORWARD_LINK           = $190144E0; { Fallback move forward button }
  DIBUTTON_HOCKEYO_BACK_LINK              = $190144E8; { Fallback move back button }
  DIBUTTON_HOCKEYO_DEVICE                 = $190044FE; { Show input device and controls }
  DIBUTTON_HOCKEYO_PAUSE                  = $190044FC; { Start / Pause / Restart game }

(*--- Sports - Hockey - Defense
      Defense       ---*)
  DIVIRTUAL_SPORTS_HOCKEY_DEFENSE         = $1A000000;
  DIAXIS_HOCKEYD_LATERAL                  = $1A008201; { Move / Aim: left / right }
  DIAXIS_HOCKEYD_MOVE                     = $1A010202; { Move / Aim: up / down }
  DIBUTTON_HOCKEYD_PLAYER                 = $1A000401; { control player closest to the puck }
  DIBUTTON_HOCKEYD_STEAL                  = $1A000402; { attempt steal }
  DIBUTTON_HOCKEYD_BURST                  = $1A000403; { speed burst or body check }
  DIBUTTON_HOCKEYD_BLOCK                  = $1A000404; { block puck }
  DIBUTTON_HOCKEYD_FAKE                   = $1A000405; { hold down to fake tackle or intercept }
  DIBUTTON_HOCKEYD_MENU                   = $1A0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_HOCKEYD_SCROLL              = $1A004601; { scroll view }
  DIBUTTON_HOCKEYD_ZOOM                   = $1A004406; { Zoom view in / out }
  DIBUTTON_HOCKEYD_STRATEGY               = $1A004407; { Invoke coaching menu for strategy help }
  DIBUTTON_HOCKEYD_TIMEOUT                = $1A004408; { Call for time out }
  DIBUTTON_HOCKEYD_SUBSTITUTE             = $1A004409; { substitute one player for another }
  DIBUTTON_HOCKEYD_LEFT_LINK              = $1A00C4E4; { Fallback sidestep left button }
  DIBUTTON_HOCKEYD_RIGHT_LINK             = $1A00C4EC; { Fallback sidestep right button }
  DIBUTTON_HOCKEYD_FORWARD_LINK           = $1A0144E0; { Fallback move forward button }
  DIBUTTON_HOCKEYD_BACK_LINK              = $1A0144E8; { Fallback move back button }
  DIBUTTON_HOCKEYD_DEVICE                 = $1A0044FE; { Show input device and controls }
  DIBUTTON_HOCKEYD_PAUSE                  = $1A0044FC; { Start / Pause / Restart game }

(*--- Sports - Hockey - Goalie
      Goal tending  ---*)
  DIVIRTUAL_SPORTS_HOCKEY_GOALIE          = $1B000000;
  DIAXIS_HOCKEYG_LATERAL                  = $1B008201; { Move / Aim: left / right }
  DIAXIS_HOCKEYG_MOVE                     = $1B010202; { Move / Aim: up / down }
  DIBUTTON_HOCKEYG_PASS                   = $1B000401; { pass puck }
  DIBUTTON_HOCKEYG_POKE                   = $1B000402; { poke / check / hack }
  DIBUTTON_HOCKEYG_STEAL                  = $1B000403; { attempt steal }
  DIBUTTON_HOCKEYG_BLOCK                  = $1B000404; { block puck }
  DIBUTTON_HOCKEYG_MENU                   = $1B0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_HOCKEYG_SCROLL              = $1B004601; { scroll view }
  DIBUTTON_HOCKEYG_ZOOM                   = $1B004405; { Zoom view in / out }
  DIBUTTON_HOCKEYG_STRATEGY               = $1B004406; { Invoke coaching menu for strategy help }
  DIBUTTON_HOCKEYG_TIMEOUT                = $1B004407; { Call for time out }
  DIBUTTON_HOCKEYG_SUBSTITUTE             = $1B004408; { substitute one player for another }
  DIBUTTON_HOCKEYG_LEFT_LINK              = $1B00C4E4; { Fallback sidestep left button }
  DIBUTTON_HOCKEYG_RIGHT_LINK             = $1B00C4EC; { Fallback sidestep right button }
  DIBUTTON_HOCKEYG_FORWARD_LINK           = $1B0144E0; { Fallback move forward button }
  DIBUTTON_HOCKEYG_BACK_LINK              = $1B0144E8; { Fallback move back button }
  DIBUTTON_HOCKEYG_DEVICE                 = $1B0044FE; { Show input device and controls }
  DIBUTTON_HOCKEYG_PAUSE                  = $1B0044FC; { Start / Pause / Restart game }

(*--- Sports - Mountain Biking
                     ---*)
  DIVIRTUAL_SPORTS_BIKING_MOUNTAIN        = $1C000000;
  DIAXIS_BIKINGM_TURN                     = $1C008201; { left / right }
  DIAXIS_BIKINGM_PEDAL                    = $1C010202; { Pedal faster / slower / brake }
  DIBUTTON_BIKINGM_JUMP                   = $1C000401; { jump over obstacle }
  DIBUTTON_BIKINGM_CAMERA                 = $1C000402; { switch camera view }
  DIBUTTON_BIKINGM_SPECIAL1               = $1C000403; { perform first special move }
  DIBUTTON_BIKINGM_SELECT                 = $1C000404; { Select }
  DIBUTTON_BIKINGM_SPECIAL2               = $1C000405; { perform second special move }
  DIBUTTON_BIKINGM_MENU                   = $1C0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_BIKINGM_SCROLL              = $1C004601; { scroll view }
  DIBUTTON_BIKINGM_ZOOM                   = $1C004406; { Zoom view in / out }
  DIAXIS_BIKINGM_BRAKE                    = $1C044203; { Brake axis  }
  DIBUTTON_BIKINGM_LEFT_LINK              = $1C00C4E4; { Fallback turn left button }
  DIBUTTON_BIKINGM_RIGHT_LINK             = $1C00C4EC; { Fallback turn right button }
  DIBUTTON_BIKINGM_FASTER_LINK            = $1C0144E0; { Fallback pedal faster button }
  DIBUTTON_BIKINGM_SLOWER_LINK            = $1C0144E8; { Fallback pedal slower button }
  DIBUTTON_BIKINGM_BRAKE_BUTTON_LINK      = $1C0444E8; { Fallback brake button }
  DIBUTTON_BIKINGM_DEVICE                 = $1C0044FE; { Show input device and controls }
  DIBUTTON_BIKINGM_PAUSE                  = $1C0044FC; { Start / Pause / Restart game }

(*--- Sports: Skiing / Snowboarding / Skateboarding
        ---*)
  DIVIRTUAL_SPORTS_SKIING                 = $1D000000;
  DIAXIS_SKIING_TURN                      = $1D008201; { left / right }
  DIAXIS_SKIING_SPEED                     = $1D010202; { faster / slower }
  DIBUTTON_SKIING_JUMP                    = $1D000401; { Jump }
  DIBUTTON_SKIING_CROUCH                  = $1D000402; { crouch down }
  DIBUTTON_SKIING_CAMERA                  = $1D000403; { switch camera view }
  DIBUTTON_SKIING_SPECIAL1                = $1D000404; { perform first special move }
  DIBUTTON_SKIING_SELECT                  = $1D000405; { Select }
  DIBUTTON_SKIING_SPECIAL2                = $1D000406; { perform second special move }
  DIBUTTON_SKIING_MENU                    = $1D0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_SKIING_GLANCE               = $1D004601; { scroll view }
  DIBUTTON_SKIING_ZOOM                    = $1D004407; { Zoom view in / out }
  DIBUTTON_SKIING_LEFT_LINK               = $1D00C4E4; { Fallback turn left button }
  DIBUTTON_SKIING_RIGHT_LINK              = $1D00C4EC; { Fallback turn right button }
  DIBUTTON_SKIING_FASTER_LINK             = $1D0144E0; { Fallback increase speed button }
  DIBUTTON_SKIING_SLOWER_LINK             = $1D0144E8; { Fallback decrease speed button }
  DIBUTTON_SKIING_DEVICE                  = $1D0044FE; { Show input device and controls }
  DIBUTTON_SKIING_PAUSE                   = $1D0044FC; { Start / Pause / Restart game }

(*--- Sports - Soccer - Offense
      Offense       ---*)
  DIVIRTUAL_SPORTS_SOCCER_OFFENSE         = $1E000000;
  DIAXIS_SOCCERO_LATERAL                  = $1E008201; { Move / Aim: left / right }
  DIAXIS_SOCCERO_MOVE                     = $1E010202; { Move / Aim: up / down }
  DIAXIS_SOCCERO_BEND                     = $1E018203; { Bend to soccer shot/pass }
  DIBUTTON_SOCCERO_SHOOT                  = $1E000401; { Shoot the ball }
  DIBUTTON_SOCCERO_PASS                   = $1E000402; { Pass  }
  DIBUTTON_SOCCERO_FAKE                   = $1E000403; { Fake }
  DIBUTTON_SOCCERO_PLAYER                 = $1E000404; { Select next player }
  DIBUTTON_SOCCERO_SPECIAL1               = $1E000405; { Apply special move }
  DIBUTTON_SOCCERO_SELECT                 = $1E000406; { Select special move }
  DIBUTTON_SOCCERO_MENU                   = $1E0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_SOCCERO_GLANCE              = $1E004601; { scroll view }
  DIBUTTON_SOCCERO_SUBSTITUTE             = $1E004407; { Substitute one player for another }
  DIBUTTON_SOCCERO_SHOOTLOW               = $1E004408; { Shoot the ball low }
  DIBUTTON_SOCCERO_SHOOTHIGH              = $1E004409; { Shoot the ball high }
  DIBUTTON_SOCCERO_PASSTHRU               = $1E00440A; { Make a thru pass }
  DIBUTTON_SOCCERO_SPRINT                 = $1E00440B; { Sprint / turbo boost }
  DIBUTTON_SOCCERO_CONTROL                = $1E00440C; { Obtain control of the ball }
  DIBUTTON_SOCCERO_HEAD                   = $1E00440D; { Attempt to head the ball }
  DIBUTTON_SOCCERO_LEFT_LINK              = $1E00C4E4; { Fallback sidestep left button }
  DIBUTTON_SOCCERO_RIGHT_LINK             = $1E00C4EC; { Fallback sidestep right button }
  DIBUTTON_SOCCERO_FORWARD_LINK           = $1E0144E0; { Fallback move forward button }
  DIBUTTON_SOCCERO_BACK_LINK              = $1E0144E8; { Fallback move back button }
  DIBUTTON_SOCCERO_DEVICE                 = $1E0044FE; { Show input device and controls }
  DIBUTTON_SOCCERO_PAUSE                  = $1E0044FC; { Start / Pause / Restart game }

(*--- Sports - Soccer - Defense
      Defense       ---*)
  DIVIRTUAL_SPORTS_SOCCER_DEFENSE         = $1F000000;
  DIAXIS_SOCCERD_LATERAL                  = $1F008201; { Move / Aim: left / right }
  DIAXIS_SOCCERD_MOVE                     = $1F010202; { Move / Aim: up / down }
  DIBUTTON_SOCCERD_BLOCK                  = $1F000401; { Attempt to block shot }
  DIBUTTON_SOCCERD_STEAL                  = $1F000402; { Attempt to steal ball }
  DIBUTTON_SOCCERD_FAKE                   = $1F000403; { Fake a block or a steal }
  DIBUTTON_SOCCERD_PLAYER                 = $1F000404; { Select next player }
  DIBUTTON_SOCCERD_SPECIAL                = $1F000405; { Apply special move }
  DIBUTTON_SOCCERD_SELECT                 = $1F000406; { Select special move }
  DIBUTTON_SOCCERD_SLIDE                  = $1F000407; { Attempt a slide tackle }
  DIBUTTON_SOCCERD_MENU                   = $1F0004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_SOCCERD_GLANCE              = $1F004601; { scroll view }
  DIBUTTON_SOCCERD_FOUL                   = $1F004408; { Initiate a foul / hard-foul }
  DIBUTTON_SOCCERD_HEAD                   = $1F004409; { Attempt a Header }
  DIBUTTON_SOCCERD_CLEAR                  = $1F00440A; { Attempt to clear the ball down the field }
  DIBUTTON_SOCCERD_GOALIECHARGE           = $1F00440B; { Make the goalie charge out of the box }
  DIBUTTON_SOCCERD_SUBSTITUTE             = $1F00440C; { Substitute one player for another }
  DIBUTTON_SOCCERD_LEFT_LINK              = $1F00C4E4; { Fallback sidestep left button }
  DIBUTTON_SOCCERD_RIGHT_LINK             = $1F00C4EC; { Fallback sidestep right button }
  DIBUTTON_SOCCERD_FORWARD_LINK           = $1F0144E0; { Fallback move forward button }
  DIBUTTON_SOCCERD_BACK_LINK              = $1F0144E8; { Fallback move back button }
  DIBUTTON_SOCCERD_DEVICE                 = $1F0044FE; { Show input device and controls }
  DIBUTTON_SOCCERD_PAUSE                  = $1F0044FC; { Start / Pause / Restart game }

(*--- Sports - Racquet
      Tennis - Table-Tennis - Squash   ---*)
  DIVIRTUAL_SPORTS_RACQUET                = $20000000;
  DIAXIS_RACQUET_LATERAL                  = $20008201; { Move / Aim: left / right }
  DIAXIS_RACQUET_MOVE                     = $20010202; { Move / Aim: up / down }
  DIBUTTON_RACQUET_SWING                  = $20000401; { Swing racquet }
  DIBUTTON_RACQUET_BACKSWING              = $20000402; { Swing backhand }
  DIBUTTON_RACQUET_SMASH                  = $20000403; { Smash shot }
  DIBUTTON_RACQUET_SPECIAL                = $20000404; { Special shot }
  DIBUTTON_RACQUET_SELECT                 = $20000405; { Select special shot }
  DIBUTTON_RACQUET_MENU                   = $200004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_RACQUET_GLANCE              = $20004601; { scroll view }
  DIBUTTON_RACQUET_TIMEOUT                = $20004406; { Call for time out }
  DIBUTTON_RACQUET_SUBSTITUTE             = $20004407; { Substitute one player for another }
  DIBUTTON_RACQUET_LEFT_LINK              = $2000C4E4; { Fallback sidestep left button }
  DIBUTTON_RACQUET_RIGHT_LINK             = $2000C4EC; { Fallback sidestep right button }
  DIBUTTON_RACQUET_FORWARD_LINK           = $200144E0; { Fallback move forward button }
  DIBUTTON_RACQUET_BACK_LINK              = $200144E8; { Fallback move back button }
  DIBUTTON_RACQUET_DEVICE                 = $200044FE; { Show input device and controls }
  DIBUTTON_RACQUET_PAUSE                  = $200044FC; { Start / Pause / Restart game }

(*--- Arcade- 2D
      Side to Side movement        ---*)
  DIVIRTUAL_ARCADE_SIDE2SIDE              = $21000000;
  DIAXIS_ARCADES_LATERAL                  = $21008201; { left / right }
  DIAXIS_ARCADES_MOVE                     = $21010202; { up / down }
  DIBUTTON_ARCADES_THROW                  = $21000401; { throw object }
  DIBUTTON_ARCADES_CARRY                  = $21000402; { carry object }
  DIBUTTON_ARCADES_ATTACK                 = $21000403; { attack }
  DIBUTTON_ARCADES_SPECIAL                = $21000404; { apply special move }
  DIBUTTON_ARCADES_SELECT                 = $21000405; { select special move }
  DIBUTTON_ARCADES_MENU                   = $210004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_ARCADES_VIEW                = $21004601; { scroll view left / right / up / down }
  DIBUTTON_ARCADES_LEFT_LINK              = $2100C4E4; { Fallback sidestep left button }
  DIBUTTON_ARCADES_RIGHT_LINK             = $2100C4EC; { Fallback sidestep right button }
  DIBUTTON_ARCADES_FORWARD_LINK           = $210144E0; { Fallback move forward button }
  DIBUTTON_ARCADES_BACK_LINK              = $210144E8; { Fallback move back button }
  DIBUTTON_ARCADES_VIEW_UP_LINK           = $2107C4E0; { Fallback scroll view up button }
  DIBUTTON_ARCADES_VIEW_DOWN_LINK         = $2107C4E8; { Fallback scroll view down button }
  DIBUTTON_ARCADES_VIEW_LEFT_LINK         = $2107C4E4; { Fallback scroll view left button }
  DIBUTTON_ARCADES_VIEW_RIGHT_LINK        = $2107C4EC; { Fallback scroll view right button }
  DIBUTTON_ARCADES_DEVICE                 = $210044FE; { Show input device and controls }
  DIBUTTON_ARCADES_PAUSE                  = $210044FC; { Start / Pause / Restart game }

(*--- Arcade - Platform Game
      Character moves around on screen  ---*)
  DIVIRTUAL_ARCADE_PLATFORM               = $22000000;
  DIAXIS_ARCADEP_LATERAL                  = $22008201; { Left / right }
  DIAXIS_ARCADEP_MOVE                     = $22010202; { Up / down }
  DIBUTTON_ARCADEP_JUMP                   = $22000401; { Jump }
  DIBUTTON_ARCADEP_FIRE                   = $22000402; { Fire }
  DIBUTTON_ARCADEP_CROUCH                 = $22000403; { Crouch }
  DIBUTTON_ARCADEP_SPECIAL                = $22000404; { Apply special move }
  DIBUTTON_ARCADEP_SELECT                 = $22000405; { Select special move }
  DIBUTTON_ARCADEP_MENU                   = $220004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_ARCADEP_VIEW                = $22004601; { Scroll view }
  DIBUTTON_ARCADEP_FIRESECONDARY          = $22004406; { Alternative fire button }
  DIBUTTON_ARCADEP_LEFT_LINK              = $2200C4E4; { Fallback sidestep left button }
  DIBUTTON_ARCADEP_RIGHT_LINK             = $2200C4EC; { Fallback sidestep right button }
  DIBUTTON_ARCADEP_FORWARD_LINK           = $220144E0; { Fallback move forward button }
  DIBUTTON_ARCADEP_BACK_LINK              = $220144E8; { Fallback move back button }
  DIBUTTON_ARCADEP_VIEW_UP_LINK           = $2207C4E0; { Fallback scroll view up button }
  DIBUTTON_ARCADEP_VIEW_DOWN_LINK         = $2207C4E8; { Fallback scroll view down button }
  DIBUTTON_ARCADEP_VIEW_LEFT_LINK         = $2207C4E4; { Fallback scroll view left button }
  DIBUTTON_ARCADEP_VIEW_RIGHT_LINK        = $2207C4EC; { Fallback scroll view right button }
  DIBUTTON_ARCADEP_DEVICE                 = $220044FE; { Show input device and controls }
  DIBUTTON_ARCADEP_PAUSE                  = $220044FC; { Start / Pause / Restart game }

(*--- CAD - 2D Object Control
      Controls to select and move objects in 2D  ---*)
  DIVIRTUAL_CAD_2DCONTROL                 = $23000000;
  DIAXIS_2DCONTROL_LATERAL                = $23008201; { Move view left / right }
  DIAXIS_2DCONTROL_MOVE                   = $23010202; { Move view up / down }
  DIAXIS_2DCONTROL_INOUT                  = $23018203; { Zoom - in / out }
  DIBUTTON_2DCONTROL_SELECT               = $23000401; { Select Object }
  DIBUTTON_2DCONTROL_SPECIAL1             = $23000402; { Do first special operation }
  DIBUTTON_2DCONTROL_SPECIAL              = $23000403; { Select special operation }
  DIBUTTON_2DCONTROL_SPECIAL2             = $23000404; { Do second special operation }
  DIBUTTON_2DCONTROL_MENU                 = $230004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_2DCONTROL_HATSWITCH         = $23004601; { Hat switch }
  DIAXIS_2DCONTROL_ROTATEZ                = $23024204; { Rotate view clockwise / counterclockwise }
  DIBUTTON_2DCONTROL_DISPLAY              = $23004405; { Shows next on-screen display options }
  DIBUTTON_2DCONTROL_DEVICE               = $230044FE; { Show input device and controls }
  DIBUTTON_2DCONTROL_PAUSE                = $230044FC; { Start / Pause / Restart game }

(*--- CAD - 3D object control
      Controls to select and move objects within a 3D environment  ---*)
  DIVIRTUAL_CAD_3DCONTROL                 = $24000000;
  DIAXIS_3DCONTROL_LATERAL                = $24008201; { Move view left / right }
  DIAXIS_3DCONTROL_MOVE                   = $24010202; { Move view up / down }
  DIAXIS_3DCONTROL_INOUT                  = $24018203; { Zoom - in / out }
  DIBUTTON_3DCONTROL_SELECT               = $24000401; { Select Object }
  DIBUTTON_3DCONTROL_SPECIAL1             = $24000402; { Do first special operation }
  DIBUTTON_3DCONTROL_SPECIAL              = $24000403; { Select special operation }
  DIBUTTON_3DCONTROL_SPECIAL2             = $24000404; { Do second special operation }
  DIBUTTON_3DCONTROL_MENU                 = $240004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_3DCONTROL_HATSWITCH         = $24004601; { Hat switch }
  DIAXIS_3DCONTROL_ROTATEX                = $24034204; { Rotate view forward or up / backward or down }
  DIAXIS_3DCONTROL_ROTATEY                = $2402C205; { Rotate view clockwise / counterclockwise }
  DIAXIS_3DCONTROL_ROTATEZ                = $24024206; { Rotate view left / right }
  DIBUTTON_3DCONTROL_DISPLAY              = $24004405; { Show next on-screen display options }
  DIBUTTON_3DCONTROL_DEVICE               = $240044FE; { Show input device and controls }
  DIBUTTON_3DCONTROL_PAUSE                = $240044FC; { Start / Pause / Restart game }

(*--- CAD - 3D Navigation - Fly through
      Controls for 3D modeling  ---*)
  DIVIRTUAL_CAD_FLYBY                     = $25000000;
  DIAXIS_CADF_LATERAL                     = $25008201; { move view left / right }
  DIAXIS_CADF_MOVE                        = $25010202; { move view up / down }
  DIAXIS_CADF_INOUT                       = $25018203; { in / out }
  DIBUTTON_CADF_SELECT                    = $25000401; { Select Object }
  DIBUTTON_CADF_SPECIAL1                  = $25000402; { do first special operation }
  DIBUTTON_CADF_SPECIAL                   = $25000403; { Select special operation }
  DIBUTTON_CADF_SPECIAL2                  = $25000404; { do second special operation }
  DIBUTTON_CADF_MENU                      = $250004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_CADF_HATSWITCH              = $25004601; { Hat switch }
  DIAXIS_CADF_ROTATEX                     = $25034204; { Rotate view forward or up / backward or down }
  DIAXIS_CADF_ROTATEY                     = $2502C205; { Rotate view clockwise / counterclockwise }
  DIAXIS_CADF_ROTATEZ                     = $25024206; { Rotate view left / right }
  DIBUTTON_CADF_DISPLAY                   = $25004405; { shows next on-screen display options }
  DIBUTTON_CADF_DEVICE                    = $250044FE; { Show input device and controls }
  DIBUTTON_CADF_PAUSE                     = $250044FC; { Start / Pause / Restart game }

(*--- CAD - 3D Model Control
      Controls for 3D modeling  ---*)
  DIVIRTUAL_CAD_MODEL                     = $26000000;
  DIAXIS_CADM_LATERAL                     = $26008201; { move view left / right }
  DIAXIS_CADM_MOVE                        = $26010202; { move view up / down }
  DIAXIS_CADM_INOUT                       = $26018203; { in / out }
  DIBUTTON_CADM_SELECT                    = $26000401; { Select Object }
  DIBUTTON_CADM_SPECIAL1                  = $26000402; { do first special operation }
  DIBUTTON_CADM_SPECIAL                   = $26000403; { Select special operation }
  DIBUTTON_CADM_SPECIAL2                  = $26000404; { do second special operation }
  DIBUTTON_CADM_MENU                      = $260004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIHATSWITCH_CADM_HATSWITCH              = $26004601; { Hat switch }
  DIAXIS_CADM_ROTATEX                     = $26034204; { Rotate view forward or up / backward or down }
  DIAXIS_CADM_ROTATEY                     = $2602C205; { Rotate view clockwise / counterclockwise }
  DIAXIS_CADM_ROTATEZ                     = $26024206; { Rotate view left / right }
  DIBUTTON_CADM_DISPLAY                   = $26004405; { shows next on-screen display options }
  DIBUTTON_CADM_DEVICE                    = $260044FE; { Show input device and controls }
  DIBUTTON_CADM_PAUSE                     = $260044FC; { Start / Pause / Restart game }

(*--- Control - Media Equipment
      Remote        ---*)
  DIVIRTUAL_REMOTE_CONTROL                = $27000000;
  DIAXIS_REMOTE_SLIDER                    = $27050201; { Slider for adjustment: volume / color / bass / etc }
  DIBUTTON_REMOTE_MUTE                    = $27000401; { Set volume on current device to zero }
  DIBUTTON_REMOTE_SELECT                  = $27000402; { Next/previous: channel/ track / chapter / picture / station }
  DIBUTTON_REMOTE_PLAY                    = $27002403; { Start or pause entertainment on current device }
  DIBUTTON_REMOTE_CUE                     = $27002404; { Move through current media }
  DIBUTTON_REMOTE_REVIEW                  = $27002405; { Move through current media }
  DIBUTTON_REMOTE_CHANGE                  = $27002406; { Select next device }
  DIBUTTON_REMOTE_RECORD                  = $27002407; { Start recording the current media }
  DIBUTTON_REMOTE_MENU                    = $270004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIAXIS_REMOTE_SLIDER2                   = $27054202; { Slider for adjustment: volume }
  DIBUTTON_REMOTE_TV                      = $27005C08; { Select TV }
  DIBUTTON_REMOTE_CABLE                   = $27005C09; { Select cable box }
  DIBUTTON_REMOTE_CD                      = $27005C0A; { Select CD player }
  DIBUTTON_REMOTE_VCR                     = $27005C0B; { Select VCR }
  DIBUTTON_REMOTE_TUNER                   = $27005C0C; { Select tuner }
  DIBUTTON_REMOTE_DVD                     = $27005C0D; { Select DVD player }
  DIBUTTON_REMOTE_ADJUST                  = $27005C0E; { Enter device adjustment menu }
  DIBUTTON_REMOTE_DIGIT0                  = $2700540F; { Digit 0 }
  DIBUTTON_REMOTE_DIGIT1                  = $27005410; { Digit 1 }
  DIBUTTON_REMOTE_DIGIT2                  = $27005411; { Digit 2 }
  DIBUTTON_REMOTE_DIGIT3                  = $27005412; { Digit 3 }
  DIBUTTON_REMOTE_DIGIT4                  = $27005413; { Digit 4 }
  DIBUTTON_REMOTE_DIGIT5                  = $27005414; { Digit 5 }
  DIBUTTON_REMOTE_DIGIT6                  = $27005415; { Digit 6 }
  DIBUTTON_REMOTE_DIGIT7                  = $27005416; { Digit 7 }
  DIBUTTON_REMOTE_DIGIT8                  = $27005417; { Digit 8 }
  DIBUTTON_REMOTE_DIGIT9                  = $27005418; { Digit 9 }
  DIBUTTON_REMOTE_DEVICE                  = $270044FE; { Show input device and controls }
  DIBUTTON_REMOTE_PAUSE                   = $270044FC; { Start / Pause / Restart game }

(*--- Control- Web
      Help or Browser            ---*)
  DIVIRTUAL_BROWSER_CONTROL               = $28000000;
  DIAXIS_BROWSER_LATERAL                  = $28008201; { Move on screen pointer }
  DIAXIS_BROWSER_MOVE                     = $28010202; { Move on screen pointer }
  DIBUTTON_BROWSER_SELECT                 = $28000401; { Select current item }
  DIAXIS_BROWSER_VIEW                     = $28018203; { Move view up/down }
  DIBUTTON_BROWSER_REFRESH                = $28000402; { Refresh }
  DIBUTTON_BROWSER_MENU                   = $280004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_BROWSER_SEARCH                 = $28004403; { Use search tool }
  DIBUTTON_BROWSER_STOP                   = $28004404; { Cease current update }
  DIBUTTON_BROWSER_HOME                   = $28004405; { Go directly to "home" location }
  DIBUTTON_BROWSER_FAVORITES              = $28004406; { Mark current site as favorite }
  DIBUTTON_BROWSER_NEXT                   = $28004407; { Select Next page }
  DIBUTTON_BROWSER_PREVIOUS               = $28004408; { Select Previous page }
  DIBUTTON_BROWSER_HISTORY                = $28004409; { Show/Hide History }
  DIBUTTON_BROWSER_PRINT                  = $2800440A; { Print current page }
  DIBUTTON_BROWSER_DEVICE                 = $280044FE; { Show input device and controls }
  DIBUTTON_BROWSER_PAUSE                  = $280044FC; { Start / Pause / Restart game }

(*--- Driving Simulator - Giant Walking Robot
      Walking tank with weapons  ---*)
  DIVIRTUAL_DRIVING_MECHA                 = $29000000;
  DIAXIS_MECHA_STEER                      = $29008201; { Turns mecha left/right }
  DIAXIS_MECHA_TORSO                      = $29010202; { Tilts torso forward/backward }
  DIAXIS_MECHA_ROTATE                     = $29020203; { Turns torso left/right }
  DIAXIS_MECHA_THROTTLE                   = $29038204; { Engine Speed }
  DIBUTTON_MECHA_FIRE                     = $29000401; { Fire }
  DIBUTTON_MECHA_WEAPONS                  = $29000402; { Select next weapon group }
  DIBUTTON_MECHA_TARGET                   = $29000403; { Select closest enemy available target }
  DIBUTTON_MECHA_REVERSE                  = $29000404; { Toggles throttle in/out of reverse }
  DIBUTTON_MECHA_ZOOM                     = $29000405; { Zoom in/out targeting reticule }
  DIBUTTON_MECHA_JUMP                     = $29000406; { Fires jump jets }
  DIBUTTON_MECHA_MENU                     = $290004FD; { Show menu options }
(*--- Priority 2 controls                            ---*)

  DIBUTTON_MECHA_CENTER                   = $29004407; { Center torso to legs }
  DIHATSWITCH_MECHA_GLANCE                = $29004601; { Look around }
  DIBUTTON_MECHA_VIEW                     = $29004408; { Cycle through view options }
  DIBUTTON_MECHA_FIRESECONDARY            = $29004409; { Alternative fire button }
  DIBUTTON_MECHA_LEFT_LINK                = $2900C4E4; { Fallback steer left button }
  DIBUTTON_MECHA_RIGHT_LINK               = $2900C4EC; { Fallback steer right button }
  DIBUTTON_MECHA_FORWARD_LINK             = $290144E0; { Fallback tilt torso forward button }
  DIBUTTON_MECHA_BACK_LINK                = $290144E8; { Fallback tilt toroso backward button }
  DIBUTTON_MECHA_ROTATE_LEFT_LINK         = $290244E4; { Fallback rotate toroso right button }
  DIBUTTON_MECHA_ROTATE_RIGHT_LINK        = $290244EC; { Fallback rotate torso left button }
  DIBUTTON_MECHA_FASTER_LINK              = $2903C4E0; { Fallback increase engine speed }
  DIBUTTON_MECHA_SLOWER_LINK              = $2903C4E8; { Fallback decrease engine speed }
  DIBUTTON_MECHA_DEVICE                   = $290044FE; { Show input device and controls }
  DIBUTTON_MECHA_PAUSE                    = $290044FC; { Start / Pause / Restart game }

(*
 *  "ANY" semantics can be used as a last resort to get mappings for actions
 *  that match nothing in the chosen virtual genre.  These semantics will be
 *  mapped at a lower priority that virtual genre semantics.  Also, hardware
 *  vendors will not be able to provide sensible mappings for these unless
 *  they provide application specific mappings.
 *)
  DIAXIS_ANY_X_1                          = $FF00C201;
  DIAXIS_ANY_X_2                          = $FF00C202;
  DIAXIS_ANY_Y_1                          = $FF014201;
  DIAXIS_ANY_Y_2                          = $FF014202;
  DIAXIS_ANY_Z_1                          = $FF01C201;
  DIAXIS_ANY_Z_2                          = $FF01C202;
  DIAXIS_ANY_R_1                          = $FF024201;
  DIAXIS_ANY_R_2                          = $FF024202;
  DIAXIS_ANY_U_1                          = $FF02C201;
  DIAXIS_ANY_U_2                          = $FF02C202;
  DIAXIS_ANY_V_1                          = $FF034201;
  DIAXIS_ANY_V_2                          = $FF034202;
  DIAXIS_ANY_A_1                          = $FF03C201;
  DIAXIS_ANY_A_2                          = $FF03C202;
  DIAXIS_ANY_B_1                          = $FF044201;
  DIAXIS_ANY_B_2                          = $FF044202;
  DIAXIS_ANY_C_1                          = $FF04C201;
  DIAXIS_ANY_C_2                          = $FF04C202;
  DIAXIS_ANY_S_1                          = $FF054201;
  DIAXIS_ANY_S_2                          = $FF054202;

  DIAXIS_ANY_1                            = $FF004201;
  DIAXIS_ANY_2                            = $FF004202;
  DIAXIS_ANY_3                            = $FF004203;
  DIAXIS_ANY_4                            = $FF004204;

  DIPOV_ANY_1                             = $FF004601;
  DIPOV_ANY_2                             = $FF004602;
  DIPOV_ANY_3                             = $FF004603;
  DIPOV_ANY_4                             = $FF004604;

//#define DIBUTTON_ANY(instance)                  ( 0xFF004400 | instance )


//#endif /* WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP) */
//#pragma endregion

//#ifdef __cplusplus
//};
//#endif

//#endif  /* __DINPUT_INCLUDED__ */

(****************************************************************************
 *
 *  Definitions for non-IDirectInput (VJoyD) features defined more recently
 *  than the current sdk files
 *
 ****************************************************************************)

{$IFDEF _INC_MMSYSTEM}
{$IFNDEF MMNOJOY}

{$IFNDEF __VJOYDX_INCLUDED__}
{$DEFINE __VJOYDX_INCLUDED__}

//#ifdef __cplusplus
//extern "C" {
//#endif

//#pragma region Desktop Family
//#if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)

(*
 * Flag to indicate that the dwReserved2 field of the JOYINFOEX structure
 * contains mini-driver specific data to be passed by VJoyD to the mini-
 * driver instead of doing a poll.
 *)
const
  JOY_PASSDRIVERDATA          = $10000000;

(*
 * Informs the joystick driver that the configuration has been changed
 * and should be reloaded from the registery.
 * dwFlags is reserved and should be set to zero
 *)
//WINMMAPI MMRESULT WINAPI joyConfigChanged( DWORD dwFlags );

//#ifndef DIJ_RINGZERO
(*
 * Invoke the joystick control panel directly, using the passed window handle
 * as the parent of the dialog.  This API is only supported for compatibility
 * purposes; new applications should use the RunControlPanel method of a
 * device interface for a game controller.
 * The API is called by using the function pointer returned by
 * GetProcAddress( hCPL, TEXT("ShowJoyCPL") ) where hCPL is a HMODULE returned
 * by LoadLibrary( TEXT("joy.cpl") ).  The typedef is provided to allow
 * declaration and casting of an appropriately typed variable.
 *)
//void WINAPI ShowJoyCPL( HWND hWnd );
//typedef void (WINAPI* LPFNSHOWJOYCPL)( HWND hWnd );
type
  LPFNSHOWJOYCPL = procedure(hWnd: HWND); stdcall;
//#endif

(*
 * Hardware Setting indicating that the device is a headtracker
 *)
const
  JOY_HWS_ISHEADTRACKER       = $02000000;

(*
 * Hardware Setting indicating that the VxD is used to replace
 * the standard analog polling
 *)
  JOY_HWS_ISGAMEPORTDRIVER    = $04000000;

(*
 * Hardware Setting indicating that the driver needs a standard
 * gameport in order to communicate with the device.
 *)
  JOY_HWS_ISANALOGPORTDRIVER  = $08000000;

(*
 * Hardware Setting indicating that VJoyD should not load this
 * driver, it will be loaded externally and will register with
 * VJoyD of it's own accord.
 *)
  JOY_HWS_AUTOLOAD            = $10000000;

(*
 * Hardware Setting indicating that the driver acquires any
 * resources needed without needing a devnode through VJoyD.
 *)
  JOY_HWS_NODEVNODE           = $20000000;


(*
 * Hardware Setting indicating that the device is a gameport bus
 *)
  JOY_HWS_ISGAMEPORTBUS       = $80000000;
  JOY_HWS_GAMEPORTBUSBUSY     = $00000001;

(*
 * Usage Setting indicating that the settings are volatile and
 * should be removed if still present on a reboot.
 *)
  JOY_US_VOLATILE             = $00000008;

//#endif /* WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP) */
//#pragma endregion

//#ifdef __cplusplus
//};
//#endif

{$ENDIF}  { __VJOYDX_INCLUDED__ }

{$ENDIF}  { not MMNOJOY }
{$ENDIF}  { _INC_MMSYSTEM }

(****************************************************************************
 *
 *  Definitions for non-IDirectInput (VJoyD) features defined more recently
 *  than the current ddk files
 *
 ****************************************************************************)

//#ifndef DIJ_RINGZERO

{$IFDEF _INC_MMDDK}
{$IFNDEF MMNOJOYDEV}

{$IFNDEF __VJOYDXD_INCLUDED__}
{$DEFINE __VJOYDXD_INCLUDED__}
(*
 * Poll type in which the do_other field of the JOYOEMPOLLDATA
 * structure contains mini-driver specific data passed from an app.
 *)
const
  JOY_OEMPOLL_PASSDRIVERDATA = 7;

{$ENDIF}  { __VJOYDXD_INCLUDED__ }

{$ENDIF}  { not MMNOJOYDEV }
{$ENDIF}  { _INC_MMDDK }

//#endif /* DIJ_RINGZERO */

function GET_DIDEVICE_SUBTYPE(dwDevType: DWORD): BYTE; inline;
function DIDFT_MAKEINSTANCE(n: WORD): DWORD; inline;
function DIDFT_GETINSTANCE(n: DWORD): WORD; inline;
function DIDFT_ENUMCOLLECTION(n: WORD): DWORD; inline;
{$IF DIRECTINPUT_VERSION >= $050a}
function DIMAKEUSAGEDWORD(UsagePage, Usage: Word): DWORD; inline;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }
function DISEQUENCE_COMPARE(dwSequence1: DWORD; const cmp: string; dwSequence2: DWORD): Boolean; inline;
function DIBUTTON_ANY(instance: Byte): DWORD; inline;

implementation

//#define GET_DIDEVICE_SUBTYPE(dwDevType) HIBYTE(dwDevType)
function GET_DIDEVICE_SUBTYPE(dwDevType: DWORD): BYTE; inline;
begin
  Result := HIBYTE(dwDevType);
end;

//#define DIDFT_MAKEINSTANCE(n) ((WORD)(n) << 8)
function DIDFT_MAKEINSTANCE(n: WORD): DWORD; inline;
begin
  Result := n shl 8;
end;

//#define DIDFT_GETINSTANCE(n) LOWORD((n) >> 8)
function DIDFT_GETINSTANCE(n: DWORD): WORD; inline;
begin
  Result := LOWORD(n shr 8);
end;

//#define DIDFT_ENUMCOLLECTION(n) ((WORD)(n) << 8)
function DIDFT_ENUMCOLLECTION(n: WORD): DWORD; inline;
begin
  Result := n shl 8;
end;

{$IF DIRECTINPUT_VERSION >= $050a}
//#define DIMAKEUSAGEDWORD(UsagePage, Usage) \
//                                (DWORD)MAKELONG(Usage, UsagePage)
function DIMAKEUSAGEDWORD(UsagePage, Usage: Word): DWORD; inline;
begin
  Result := MAKELONG(Usage, UsagePage);
end;
{$ENDIF} { DIRECTINPUT_VERSION >= $050a }

//#define DISEQUENCE_COMPARE(dwSequence1, cmp, dwSequence2) \
//                        ((int)((dwSequence1) - (dwSequence2)) cmp 0)
function DISEQUENCE_COMPARE(dwSequence1: DWORD; const cmp: string; dwSequence2: DWORD): Boolean; inline;
begin
{$PUSH}{$Q-,R-}
  if (cmp = '==') or (cmp = '=') then
    Result := LongInt(dwSequence1 - dwSequence2) = 0
  else if (cmp = '!=') or (cmp = '<>') then
    Result := LongInt(dwSequence1 - dwSequence2) <> 0
  else if cmp = '<' then
    Result := LongInt(dwSequence1 - dwSequence2) < 0
  else if cmp = '>' then
    Result := LongInt(dwSequence1 - dwSequence2) > 0
  else if cmp = '<=' then
    Result := LongInt(dwSequence1 - dwSequence2) <= 0
  else if cmp = '>=' then
    Result := LongInt(dwSequence1 - dwSequence2) >= 0;
{$POP}
end;

//#define DIBUTTON_ANY(instance)                  ( 0xFF004400 | instance )
function DIBUTTON_ANY(instance: Byte): DWORD; inline;
begin
  Result := $FF004400 or instance;
end;

end.
