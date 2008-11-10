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
// Module Name:
//
//     devload.h
//
// Abstract:
//
//     Device loader structures and defines
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit devload;

{$CALLING cdecl}

interface

uses Windows;

// @doc DRIVERS

//
// These keys are under HKEY_LOCAL_MACHINE
//
const
      DEVLOAD_DRIVERS_KEY  = 'Drivers';
      DEVLOAD_BUILT_IN_KEY = 'Drivers\BuiltIn';
      DEVLOAD_PCMCIA_KEY   = 'Drivers\PCMCIA';
      DEVLOAD_ACTIVE_KEY   = 'Drivers\Active';
      DEVLOAD_DETECT_KEY   = DEVLOAD_PCMCIA_KEY + '\Detect';
      
//
// These are the required and optional values under a device key.
//
const
      DEVLOAD_DLLNAME_VALNAME     = 'Dll';     // DLL name (required)
      DEVLOAD_DLLNAME_VALTYPE     = REG_SZ;
      DEVLOAD_ICLASS_VALNAME      = 'IClass';  // Class(es) of device interface (semi-required)
      DEVLOAD_ICLASS_VALTYPE      = REG_MULTI_SZ;
      DEVLOAD_LOADORDER_VALNAME   = 'Order';   // LoadOrder (optional)
      DEVLOAD_LOADORDER_VALTYPE   = REG_DWORD;
      DEVLOAD_ENTRYPOINT_VALNAME  = 'Entry';   // Entrypoint name (deprecated)
      DEVLOAD_ENTRYPOINT_VALTYPE  = REG_SZ;
      DEVLOAD_PREFIX_VALNAME      = 'Prefix';  // Device prefix (optional)
      DEVLOAD_PREFIX_VALTYPE      = REG_SZ;
      DEVLOAD_INDEX_VALNAME       = 'Index';   // Device index (optional)
      DEVLOAD_INDEX_VALTYPE       = REG_DWORD;
      DEVLOAD_CONTEXT_VALNAME     = 'Context'; // Device context (optional)
      DEVLOAD_CONTEXT_VALTYPE     = REG_DWORD;
      DEVLOAD_INITCODE_VALNAME    = 'Ioctl';   // Device IO control code to indicate context (deprecated)
      DEVLOAD_INITCODE_VALTYPE    = REG_DWORD;
      DEVLOAD_BUSINITCODE_VALNAME = 'BusIoctl';   // Bus IO control code to indicate context
      DEVLOAD_BUSINITCODE_VALTYPE = REG_DWORD;
      DEVLOAD_FLAGS_VALNAME       = 'Flags';   // Flag to control loading/unloading (optional)
      DEVLOAD_FLAGS_VALTYPE       = REG_DWORD;
      DEVLOAD_INTRPEND_VALNAME    = 'NoIntrPend'; // Is the interrupt pending flag invalid (optional)
      DEVLOAD_INTRPEND_VALTYPE    = REG_DWORD;
      DEVLOAD_REPARMS_VALNAME     = 'RegenumParms'; // Regenum parms passed into Active\xx
      DEVLOAD_REPARMS_VALTYPE     = REG_MULTI_SZ;

      DEVLOAD_MEMBASE_VALNAME     = 'MemBase'; // Memory base (optional)
      DEVLOAD_MEMLEN_VALNAME      = 'MemLen';  // Memory length (optional)
      DEVLOAD_IOBASE_VALNAME      = 'IoBase';  // IO base (optional)
      DEVLOAD_IOLEN_VALNAME       = 'IoLen';   // IO length (optional)
      DEVLOAD_SYSINTR_VALNAME     = 'SysIntr'; // System interrupt number (optional)
      DEVLOAD_IRQ_VALNAME         = 'Irq';     // Irq number (optional)
      DEVLOAD_IFCTYPE_VALNAME     = 'InterfaceType';   // Bus interface type (i.e. PCIbus, ISA, etc) (optional)
      DEVLOAD_BUSNUMBER_VALNAME   = 'BusNumber';       // Bus number (relevant for PCIbus) (optional)
      DEVLOAD_ISRDLL_VALNAME      = 'IsrDll';  // DLL name of ISR (optional)
      DEVLOAD_ISRHANDLER_VALNAME  = 'IsrHandler';      // ISR handler function name (optional)

//
// The presence of the value "Keep" will cause device.exe to skip the call to
// FreeLibrary after calling the specified entrypoint.  This only affects
// builtin drivers that specify an entrypoint. (This usage is deprecated -
// make appropriate use of the UNLOAD flag instead).
//
const
      DEVLOAD_KEEPLIB_VALNAME     = 'Keep';   // (deprecated - see above)
      DEVLOAD_KEEPLIB_VALTYPE     = REG_DWORD;

//
// Flag values.
//
const
      DEVFLAGS_NONE              = $00000000;  // No flags defined
      DEVFLAGS_UNLOAD            = $00000001;  // Unload driver after call to entry point returns
      DEVFLAGS_LOADLIBRARY       = $00000002;  // Use LoadLibrary instead of LoadDriver
      DEVFLAGS_NOLOAD            = $00000004;  // Don't load Dll
      DEVFLAGS_NAKEDENTRIES      = $00000008;  // Entry points don't have Prefix prepended
      DEVFLAGS_BOOTPHASE_1       = $00001000;  // This driver only load at system phase 1
      DEVFLAGS_IRQ_EXCLUSIVE     = $00000100;  // This driver only can be load when it has exclusive access for IRQ.
      DEVFLAGS_TRUSTEDCALLERONLY = $00010000;  // This driver only can be opened by trusted application.

//
// Structure passed in the input buffer of DeviceIoControl() for the 
// post initialization ioctl
//
type
     _POST_INIT_BUF = record
       p_hDevice:HANDLE;        // device handle from RegisterDevice
       p_hDeviceKey:HKEY;     // open registry handle to the driver's device key
     end;
     POST_INIT_BUF = _POST_INIT_BUF;
     PPOST_INIT_BUF = ^POST_INIT_BUF;


//
// For passing additional registry settings to ActivateDeviceEx()
//
type
     _REGINI = record
       lpszVal:LPCWSTR;
       pData:LPBYTE;
       dwLen:DWORD;
       dwType:DWORD;
     end;
     REGINI = _REGINI;
     LPCREGINI = ^_REGINI;

//
// These values reside under a device's active key
//
const
      DEVLOAD_CLIENTINFO_VALNAME    = 'ClientInfo'; // ClientInfo DWORD from ActivateDriver
      DEVLOAD_CLIENTINFO_VALTYPE    = REG_DWORD;
      DEVLOAD_HANDLE_VALNAME        = 'Hnd';     // Device handle (from RegisterDevice)
      DEVLOAD_HANDLE_VALTYPE        = REG_DWORD;
      DEVLOAD_DEVNAME_VALNAME       = 'Name';    // Device name (i.e "COM1:")
      DEVLOAD_DEVNAME_VALTYPE       = REG_SZ;
      DEVLOAD_DEVKEY_VALNAME        = 'Key';     // Device key in \Drivers\(Built-In or PCMCIA)
      DEVLOAD_DEVKEY_VALTYPE        = REG_SZ;
      DEVLOAD_PNPID_VALNAME         = 'PnpId';   // Plug and Play Id (PCMCIA, optional)
      DEVLOAD_PNPID_VALTYPE         = REG_SZ;
      DEVLOAD_SOCKET_VALNAME        = 'Sckt';    // PCMCIA socket (optional)
      DEVLOAD_SOCKET_VALTYPE        = REG_DWORD;       // Actually a CARD_SOCKET_HANDLE
      DEVLOAD_INTERFACETYPE_VALNAME = 'InterfaceType';  // Bus Type
      DEVLOAD_INTERFACETYPE_VALTYPE = REG_DWORD;


//
// These values reside under a device's active key.  They are provided by the device's bus
// driver.  All values are optional.  The BusParent tells the Device Manager which device
// driver loaded the new driver.  The BusPrefix is used to specify DLL entry point decoration
// for devices that do not expose a stream interface to applications (via the legacy or $device
// namespaces).  If a Prefix value is specified in the device's device key and a BusPrefix value
// is specified, they must match.  The BusName provides the Device Manager with the name that the
// device driver will expose in the $bus namespace.  This name must be unique and is completely
// specified by the bus driver; it is not decorated with an instance index; if a duplicate name 
// is found, the driver will not load.
//
const
      DEVLOAD_BUSPARENT_VALNAME   = 'BusParent';   // parent bus driver handle
      DEVLOAD_BUSPARENT_VALTYPE   = REG_DWORD;
      DEVLOAD_BUSPREFIX_VALNAME   = 'BusPrefix';   // DLL entry point decoration -- must match "Prefix" if present in device key
      DEVLOAD_BUSPREFIX_VALTYPE   = REG_SZ;
      DEVLOAD_BUSNAME_VALNAME     = 'BusName';     // device's name on the parent bus
      DEVLOAD_BUSNAME_VALTYPE     = REG_SZ;

//
// TAPI Pnp support
//
const
      DEVLOAD_TSPDLL_VALNAME      = 'Tsp';     // TAPI Service Provider DLL
      DEVLOAD_TSPDLL_VALTYPE      = REG_SZ;
      DEVLOAD_TSPDEV_VALNAME      = 'THnd';    // TAPI device index
      DEVLOAD_TSPDEV_VALTYPE      = REG_DWORD;

//
// Prototype for the optional device driver entrypoint
//
type
     PFN_DEV_ENTRY = function(param1:LPTSTR):DWORD; cdecl; // Parameter is registry path of device's key

const
      DEVKEY_LEN      = 256;  // Max length of registry key path name
      DEVNAME_LEN     =  16;  // Max length of device name
      DEVDLL_LEN      =  64;  // Max length of device driver DLL name
      DEVENTRY_LEN    =  64;  // Max length of device driver entrypoint name
      DEVPREFIX_LEN   =   8;  // Max length of device prefix


//
// Device APIs: EnumPnpIds, EnumDevices, GetDeviceKeys and OpenDeviceKey
// (in coredll.dll)
//
function EnumPnpIds(PnpList:LPTSTR; lpBuflen:LPDWORD):DWORD; external KernelDLL name 'EnumPnpIds'; // index BE
function EnumDevices(DevList:LPTSTR; lpBuflen:LPDWORD):DWORD; external KernelDLL name 'EnumDevices'; // index BF
function GetDeviceKeys(DevName:LPCTSTR; ActiveKey:LPTSTR; lpActiveLen:LPDWORD;
                       DriverKey:LPTSTR; lpDriverLen:LPDWORD):DWORD; external KernelDLL name 'GetDeviceKeys'; // index C0
function OpenDeviceKey(ActiveKey:LPCTSTR):HKEY; external KernelDLL name 'OpenDeviceKey'; // index C1
function GetDeviceHandleFromContext(pContext:LPCTSTR):HANDLE; external KernelDLL name 'GetDeviceHandleFromContext';


const
      DevMgrDLL = 'devmgr.dll';
//
// Device Manager APIs located in devmgr.dll.  These APIs are only available to drivers
// loaded in the Device Manager's process context.
//
function DmAdvertiseInterface(hDevice:HANDLE; const devclass:LPGUID; name:LPCWSTR; fAdd:BOOL):DWORD; external DevMgrDLL name 'DmAdvertiseInterface';

//
// This interface GUID indicates that the device supports differentiating the $bus and $device
// (and legacy) namespaces.  Devices must advertise this GUID via IClass or calls to
// DmAdvertiseInterface() before CreateFile() requests on their $bus names will succeed.  The GUID
// must be associated with their bus name, not their $device or legacy name.
//
const
      DMCLASS_PROTECTEDBUSNAMESPACE   = '{6F40791D-300E-44E4-BC38-E0E63CA8375C}';

//
// Drivers that advertise DMCLASS_PROTECTEDBUSNAMESPACE will have this bit set in the dwAccess
// parameter to their XXX_Open() entry point when a handle is opened using the $bus namespace.
// This allows them to know which handles are allowed to carry out privileged operations.  If they
// advertise DMCLASS_PROTECTEDBUSNAMESPACE, they are required to block privileged operations on
// handles opened in the $device or legacy namespaces.
//
const
      DEVACCESS_BUSNAMESPACE          = FILE_WRITE_ATTRIBUTES;

implementation

end.
