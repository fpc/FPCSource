{$mode delphi}
unit ub1873;

interface

uses Windows;

const
  advapi32 = 'advapi32.dll';


function GetServiceDisplayNameA(hSCManager: SC_HANDLE; lpServiceName: LPCSTR;
  lpDisplayName: LPSTR; var lpcchBuffer: DWORD): BOOL; stdcall;

implementation

function GetServiceDisplayNameA; external advapi32 name 'GetServiceDisplayNameA';

end.
