{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
{
service.h

Abstract:
    Defines programming model for Windows CE Services

Notes:
}

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit service;

{$CALLING cdecl}

interface

uses Windows, WinIOCtl;

//
//  Return codes
//
const
      SERVICE_SUCCESS               = 0;

//
//  Service states
//
const
      SERVICE_STATE_OFF           = 0;
      SERVICE_STATE_ON            = 1;
      SERVICE_STATE_STARTING_UP   = 2;
      SERVICE_STATE_SHUTTING_DOWN = 3;
      SERVICE_STATE_UNLOADING     = 4;
      SERVICE_STATE_UNINITIALIZED = 5;
      SERVICE_STATE_UNKNOWN       = $FFFFFFFF;

const
//
// Service startup state (value passed on xxx_Init())
//
      SERVICE_INIT_STARTED  = $00000000;
// Service is a super-service, should not spin its own accept() threads.
      SERVICE_INIT_STOPPED  = $00000001;
// Service is being run in an isolated services.exe.  Interprocess communication
// via IOCTLs or streaming interface is not supported in this configuration.
      SERVICE_INIT_STANDALONE = $00000002;

// 
// Service may need to know whether it was called from device.exe or services.exe or elsewhere.
//
const
      SERVICE_CALLER_PROCESS_SERVICES_EXE      = 1;
      SERVICE_CALLER_PROCESS_DEVICE_EXE        = 2;
                        SERVICE_CALLER_PROCESS_OTHER_EXE         = 100;

      SERVICE_SERVICES_EXE_PROCNAME            = 'services.exe';
      SERVICE_DEVICE_EXE_PROCNAME              = 'device.exe';


type
     _ServiceEnumInfo = record
        szPrefix:array[0..5] of WideChar;
          szDllName:PWideChar;
          hServiceHandle:HANDLE;
          dwServiceState:DWORD;   // one of SERVICE_STATE_XXX values above.
     end;
     ServiceEnumInfo = _ServiceEnumInfo;


// Called from service on initialization to determine where it's running from
procedure SERVICE_FIND_CALLER(var callerProc:DWORD); 

//
//  Service is interfaced via series of IOCTL calls that define service life cycle.
//  Actual implementation is service-specific.
//

//
//  Start the service that has been in inactive state. Return code: SERVICE_SUCCESS or error code.
//
const
      IOCTL_SERVICE_START     = (FILE_DEVICE_SERVICE shl 16) or (1 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Stop service, but do not unload service's DLL
//
      IOCTL_SERVICE_STOP       = (FILE_DEVICE_SERVICE shl 16) or (2 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Refresh service's state from registry or other configuration storage
//
      IOCTL_SERVICE_REFRESH   = (FILE_DEVICE_SERVICE shl 16) or (3 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Have service configure its registry for auto-load
//
      IOCTL_SERVICE_INSTALL   = (FILE_DEVICE_SERVICE shl 16) or (4 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Remove registry configuration
//
      IOCTL_SERVICE_UNINSTALL = (FILE_DEVICE_SERVICE shl 16) or (5 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Unload the service which should be stopped.
//
      IOCTL_SERVICE_UNLOAD    = (FILE_DEVICE_SERVICE shl 16) or (6 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Supply a configuration or command string and code to the service.
//
      IOCTL_SERVICE_CONTROL   = (FILE_DEVICE_SERVICE shl 16) or (7 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Return service status.
//
      IOCTL_SERVICE_STATUS    = (FILE_DEVICE_SERVICE shl 16) or (8 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Set service's debug zone mask (parameter is sizeof(DWORD) and contains the mask)
//
      IOCTL_SERVICE_DEBUG     = (FILE_DEVICE_SERVICE shl 16) or (9 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
//  Toggle service's console on/off (string "on" on no parameters is ON, "off" means off)
//
      IOCTL_SERVICE_CONSOLE   = (FILE_DEVICE_SERVICE shl 16) or (10 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Notify service a service request has arrived.  Input contains connected socket.
//
      IOCTL_SERVICE_REGISTER_SOCKADDR   = (FILE_DEVICE_SERVICE shl 16) or (11 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Notify service a service request has arrived.  For now all sockets associated with service will be closed at once.
//
      IOCTL_SERVICE_DEREGISTER_SOCKADDR = (FILE_DEVICE_SERVICE shl 16) or (12 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Notify service a another socket has bound to it.  Input contains an accepted socket.
//
      IOCTL_SERVICE_CONNECTION = (FILE_DEVICE_SERVICE shl 16) or (13 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Service has finished initial control setup.  Ready to start.
//
      IOCTL_SERVICE_STARTED    = (FILE_DEVICE_SERVICE shl 16) or (14 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Service is called with IOCTL_SERVICE_CAN_DEINIT immediatly before xxx_Deinit is called during DeregisterService.
// If xxx_IOControl returns TRUE and sets buffer in pBufOut to zero, service instance will remain loaded and
// xxx_Deinit will not be called.
//
      IOCTL_SERVICE_QUERY_CAN_DEINIT   = (FILE_DEVICE_SERVICE shl 16) or (15 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// If iphlpapi is present, services receive this notification when table that
// maps IP addresses changes.
// Input contains PIP_ADAPTER_ADDRESSES retrieved from a call to GetAdaptersAddresses().
// As soon as the service returns from this call, this buffer is not valid.  The buffer
// must not be pointed to and it MUST NOT BE COPIED (because internally it has pointers
// to other structures inside it).  If this data is required after the service returns,
// the service must make its own call to GetAdaptersAddresses().
      IOCTL_SERVICE_NOTIFY_ADDR_CHANGE  = (FILE_DEVICE_SERVICE shl 16) or (16 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// Services.exe supports a set of callbacks that can only be called from a running service (not API).
// pBufIn data structure contains a pointer to ServiceCallbackFunctions data structure
// IOCTL_SERVICE_CALLBACKS will sent to service during its initial load, and only if there
// are supported callbacks for service's mode of operation.
//
      IOCTL_SERVICE_CALLBACK_FUNCTIONS  = (FILE_DEVICE_SERVICE shl 16) or (17 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);


//
// Service returns a DWORD containing a set of flags that specify what options it supports.
// The meaning of the flags is service specific.
//
      IOCTL_SERVICE_SUPPORTED_OPTIONS   = (FILE_DEVICE_SERVICE shl 16) or (18 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);


//
// Service is called from services.exe cmd line with arguments passed into it.
// (i.e. "services command HTP0: Arg1 Arg2 Arg3...")
// Args will be passed to service as ServicesExeCommandLineParams struct as the pBufIn paramater.
// In pBufOut paramater services may put a WCHAR* string, which will be displayed to command line user.
//
      IOCTL_SERVICE_COMMAND_LINE_PARAMS = (FILE_DEVICE_SERVICE shl 16) or (19 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);

//
// If a cmd line user of services.exe queries it for help on a particular service
// (i.e. "services help HTP0:") then this IOCTL will be passed to the service.  If
// the service supports command line help, it should return a WCHAR* to be displayed
// to the user in pBufOut.

      IOCTL_SERVICE_COMMAND_LINE_HELP = (FILE_DEVICE_SERVICE shl 16) or (20 shl 2) or METHOD_BUFFERED or (FILE_ANY_ACCESS shl 14);



// A service will be called with IOCTL_SERVICE_CALLBACKS and a pointer to this struct
// on startup potentially.  Note that only functions that are supported for a service's
// mode of operation will be set.  It is possible that function pointers will be NULL.
type
      PFN_SERVICE_SHUTDOWN = procedure; cdecl;

type
     _ServicesExeCallbackFunctions = record
        // Only set when service is running in context SERVICE_INIT_STANDALONE, i.e.
         // in its own copy of services.exe.  Service calls this function to request itself
      // to be unloaded and the services.exe process to exit.  For typical scenario
      // (no SERVICE_INIT_STANDALONE), use DeregisterService() API to cause service to be unloaded.
        pfnServiceShutdown:PFN_SERVICE_SHUTDOWN;
     end;
     ServicesExeCallbackFunctions = _ServicesExeCallbackFunctions;

const
      SERVICES_EXE_MAX_CMD_LINE_OPTIONS        = 16;

type
     _ServicesExeCommandLineParams = record
       dwArgs:DWORD;
          ppwszArgs:array[0..SERVICES_EXE_MAX_CMD_LINE_OPTIONS-1] of WideChar;
     end;
     ServicesExeCommandLineParams = _ServicesExeCommandLineParams;


function ActivateService(lpszDevKey:LPCWSTR; dwClientInfo:DWORD):HANDLE; external KernelDLL name 'ActivateService'; // index 16A
function RegisterService(lpszType:LPCWSTR; dwIndex:DWORD; lpszLib:LPCWSTR; dwInfo:DWORD):HANDLE; external KernelDLL name 'RegisterService'; // index 16B
function DeregisterService(hService:HANDLE):BOOL; external KernelDLL name 'DeregisterService'; // index 16C
// void CloseAllServiceHandles(HANDLE proc);  // not supported
function CreateServiceHandle(lpNew:LPCWSTR; dwAccess:DWORD; dwShareMode:DWORD; hProc:HANDLE):HANDLE; external KernelDLL name 'CreateServiceHandle'; // index 16E
function GetServiceByIndex(dwIndex:DWORD; lpFindFileData:LPWIN32_FIND_DATA):BOOL; external KernelDLL name 'GetServiceByIndex'; // index 16F
function ServiceIoControl(hService:HANDLE;
                          dwIoControlCode:DWORD;
                          lpInBuf:LPVOID;
                          nInBufSize:DWORD;
                          lpOutBuf:LPVOID;
                          nOutBufSize:DWORD;
                          lpBytesReturned:LPDWORD;
                          lpOverlapped:LPOVERLAPPED):BOOL; external KernelDLL name 'ServiceIoControl'; // index 170

{$IF DEFINED(_WINSOCKAPI_) OR DEFINED(_WINSOCK2API_)}
function ServiceAddPort(hService:HANDLE; pSockAddr:LPSOCKADDR; cbSockAddr:integer; iProtocol:integer; szRegWritePath:PWideChar):BOOL; external KernelDLL name 'ServiceAddPort'; // index 171
function ServiceClosePort(hService:HANDLE; pSockAddr:LPSOCKADDR; cbSockAddr:integer; iProtocol:integer; fRemoveFromRegistry:BOOL):BOOL; external KernelDLL name 'ServiceClosePort'; // index 175
{$ELSE}
function ServiceAddPort(hService:HANDLE; pSockAddr:LPVOID{ref. to LPSOCKADDR}; cbSockAddr:integer; iProtocol:integer; szRegWritePath:PWideChar):BOOL; external KernelDLL name 'ServiceAddPort'; // index 171
function ServiceClosePort(hService:HANDLE; pSockAddr:LPVOID{ref. to LPSOCKADDR}; cbSockAddr:integer; iProtocol:integer; fRemoveFromRegistry:BOOL):BOOL; external KernelDLL name 'ServiceClosePort'; // index 175
{$ENDIF}

function ServiceUnbindPorts(hService:HANDLE):BOOL; external KernelDLL name 'ServiceUnbindPorts'; // index 172
function EnumServices(pBuffer:PBYTE; pdwServiceEntries:LPDWORD; pdwBufferLen:LPDWORD):BOOL; external KernelDLL name 'EnumServices'; // index 173
function GetServiceHandle(szPrefix:LPWSTR; szDllName:LPWSTR; pdwDllBuf:LPDWORD):HANDLE; external KernelDLL name 'GetServiceHandle'; // index 174

{
#if defined(WINCEOEM) && defined(UNDER_CE)
#ifdef WINCEMACRO
#include <mservice.h>
#endif
#endif
}
implementation

// Called from service on initialization to determine where it's running from
procedure SERVICE_FIND_CALLER(var callerProc:DWORD); inline;
var szCallerModName:array[0..MAX_PATH-1] of WideChar;
begin
  if GetModuleFileName(HINST(GetCurrentProcess),szCallerModName,SizeOf(szCallerModName))<>0
  then begin
    if wcsstr(szCallerModName,SERVICE_DEVICE_EXE_PROCNAME)<>nil then begin
       callerProc:=SERVICE_CALLER_PROCESS_DEVICE_EXE;
    end else begin
      if wcsstr(szCallerModName,SERVICE_SERVICES_EXE_PROCNAME)<>nil then begin
         callerProc:=SERVICE_CALLER_PROCESS_SERVICES_EXE;
      end else begin
        callerProc:=SERVICE_CALLER_PROCESS_OTHER_EXE;
      end;
    end;
  end else begin
    callerProc:=SERVICE_CALLER_PROCESS_OTHER_EXE;
  end;
end;

end.
