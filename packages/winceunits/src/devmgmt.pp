//
// Module Name:
//
//     devmgmt.h
//
// DESCRIPTION:
// Device Management APIs
//

//
// Included Module Name:
//
//     cfgmgrapi.h
//
// DESCRIPTION:
// Configmanager API to process XML configuration files
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit devmgmt;

{$CALLING cdecl}

interface

uses Windows, aygshell;

// - cfgmgrapi.h

// Possible error codes returned by Configmanager
const
      CONFIG_E_OBJECTBUSY                 = HRESULT($80042001);
      CONFIG_E_CANCELTIMEOUT              = HRESULT($80042002);
      CONFIG_E_ENTRYNOTFOUND              = HRESULT($80042004);
      CONFIG_S_PROCESSINGCANCELED         = HRESULT($00042005);
      CONFIG_E_CSPEXCEPTION               = HRESULT($80042007);
      CONFIG_E_TRANSACTIONINGFAILURE      = HRESULT($80042008);
      CONFIG_E_BAD_XML                    = HRESULT($80042009);

// Configmanager flags
const
      CFGFLAG_PROCESS                     = $0001;
      CFGFLAG_METADATA                    = $0002;

function DMProcessConfigXML(pszWXMLin:LPCWSTR; dwFlags:DWORD; ppszwXMLout:PLPWSTR):HRESULT; external UserDLLAyg name 'DMProcessConfigXML'; // index 7E5

// - end of cfgmgrapi.h

// - devmgmt.h

function QueryPolicy(dwPolicyId:DWORD; pdwPolicyValue:PDWORD):HRESULT; external UserDLLAyg name 'QueryPolicy';  

// - end of devmgmt.h

implementation

end.