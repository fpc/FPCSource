{ This file is part of the Free Pascal run time library.
  Translation of mapi.h: Simple MAPI, loaded on first use from mapi32.dll or a mail client DLL.
  MAPISendMailW needs Windows 8 or later.
  See the file COPYING.FPC, included in this distribution, for details about the copyright. }
{$IFNDEF FPC_DOTTEDUNITS}
unit Mapi;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
  Windows;
{$ENDIF FPC_DOTTEDUNITS}

type
  LHANDLE = ULONG_PTR;
  LPLHANDLE = ^LHANDLE;
  FLAGS = ULONG;
  LPULONG = ^ULONG;

  { MapiFileDesc }
  MapiFileDesc = record
    ulReserved: ULONG;
    flFlags: ULONG;
    nPosition: ULONG;
    lpszPathName: LPSTR;
    lpszFileName: LPSTR;
    lpFileType: LPVOID;
  end;
  lpMapiFileDesc = ^MapiFileDesc;
  TMapiFileDesc = MapiFileDesc;
  PMapiFileDesc = lpMapiFileDesc;

  { MapiFileDescW }
  MapiFileDescW = record
    ulReserved: ULONG;
    flFlags: ULONG;
    nPosition: ULONG;
    lpszPathName: LPWSTR;
    lpszFileName: LPWSTR;
    lpFileType: LPVOID;
  end;
  lpMapiFileDescW = ^MapiFileDescW;
  TMapiFileDescW = MapiFileDescW;
  PMapiFileDescW = lpMapiFileDescW;

  { MapiFileTagExt }
  MapiFileTagExt = record
    ulReserved: ULONG;
    cbTag: ULONG;
    lpTag: LPBYTE;
    cbEncoding: ULONG;
    lpEncoding: LPBYTE;
  end;
  lpMapiFileTagExt = ^MapiFileTagExt;
  TMapiFileTagExt = MapiFileTagExt;
  PMapiFileTagExt = lpMapiFileTagExt;

  { MapiRecipDesc }
  MapiRecipDesc = record
    ulReserved: ULONG;
    ulRecipClass: ULONG;
    lpszName: LPSTR;
    lpszAddress: LPSTR;
    ulEIDSize: ULONG;
    lpEntryID: LPVOID;
  end;
  lpMapiRecipDesc = ^MapiRecipDesc;
  PlpMapiRecipDesc = ^lpMapiRecipDesc;
  TMapiRecipDesc = MapiRecipDesc;
  PMapiRecipDesc = lpMapiRecipDesc;

  { MapiRecipDescW }
  MapiRecipDescW = record
    ulReserved: ULONG;
    ulRecipClass: ULONG;
    lpszName: LPWSTR;
    lpszAddress: LPWSTR;
    ulEIDSize: ULONG;
    lpEntryID: LPVOID;
  end;
  lpMapiRecipDescW = ^MapiRecipDescW;
  TMapiRecipDescW = MapiRecipDescW;
  PMapiRecipDescW = lpMapiRecipDescW;

  { MapiMessage }
  MapiMessage = record
    ulReserved: ULONG;
    lpszSubject: LPSTR;
    lpszNoteText: LPSTR;
    lpszMessageType: LPSTR;
    lpszDateReceived: LPSTR;
    lpszConversationID: LPSTR;
    flFlags: FLAGS;
    lpOriginator: lpMapiRecipDesc;
    nRecipCount: ULONG;
    lpRecips: lpMapiRecipDesc;
    nFileCount: ULONG;
    lpFiles: lpMapiFileDesc;
  end;
  lpMapiMessage = ^MapiMessage;
  PlpMapiMessage = ^lpMapiMessage;
  TMapiMessage = MapiMessage;
  PMapiMessage = lpMapiMessage;

  { MapiMessageW }
  MapiMessageW = record
    ulReserved: ULONG;
    lpszSubject: LPWSTR;
    lpszNoteText: LPWSTR;
    lpszMessageType: LPWSTR;
    lpszDateReceived: LPWSTR;
    lpszConversationID: LPWSTR;
    flFlags: FLAGS;
    lpOriginator: lpMapiRecipDescW;
    nRecipCount: ULONG;
    lpRecips: lpMapiRecipDescW;
    nFileCount: ULONG;
    lpFiles: lpMapiFileDescW;
  end;
  lpMapiMessageW = ^MapiMessageW;
  TMapiMessageW = MapiMessageW;
  PMapiMessageW = lpMapiMessageW;

const
  { MapiFileDesc.flFlags }
  MAPI_OLE = $00000001;
  MAPI_OLE_STATIC = $00000002;

  { MapiRecipDesc.ulRecipClass }
  MAPI_ORIG = 0;
  MAPI_TO = 1;
  MAPI_CC = 2;
  MAPI_BCC = 3;

  { MapiMessage.flFlags }
  MAPI_UNREAD = $00000001;
  MAPI_RECEIPT_REQUESTED = $00000002;
  MAPI_SENT = $00000004;

  { MAPILogon flags }
  MAPI_LOGON_UI = $00000001;
  MAPI_PASSWORD_UI = $00020000;
  MAPI_NEW_SESSION = $00000002;
  MAPI_FORCE_DOWNLOAD = $00001000;
  MAPI_EXTENDED = $00000020;

  { MAPISendMail flags }
  MAPI_DIALOG = $00000008;
  MAPI_DIALOG_MODELESS = $00000004 or MAPI_DIALOG;
  MAPI_FORCE_UNICODE = $00040000;

  { MAPIFindNext flags }
  MAPI_UNREAD_ONLY = $00000020;
  MAPI_GUARANTEE_FIFO = $00000100;
  MAPI_LONG_MSGID = $00004000;

  { MAPIReadMail flags }
  MAPI_PEEK = $00000080;
  MAPI_SUPPRESS_ATTACH = $00000800;
  MAPI_ENVELOPE_ONLY = $00000040;
  MAPI_BODY_AS_FILE = $00000200;

  { MAPIDetails flags }
  MAPI_AB_NOMODIFY = $00000400;

  { Results }
  SUCCESS_SUCCESS = 0;
  MAPI_USER_ABORT = 1;
  MAPI_E_USER_ABORT = MAPI_USER_ABORT;
  MAPI_E_FAILURE = 2;
  MAPI_E_LOGON_FAILURE = 3;
  MAPI_E_LOGIN_FAILURE = MAPI_E_LOGON_FAILURE;
  MAPI_E_DISK_FULL = 4;
  MAPI_E_INSUFFICIENT_MEMORY = 5;
  MAPI_E_ACCESS_DENIED = 6;
  MAPI_E_TOO_MANY_SESSIONS = 8;
  MAPI_E_TOO_MANY_FILES = 9;
  MAPI_E_TOO_MANY_RECIPIENTS = 10;
  MAPI_E_ATTACHMENT_NOT_FOUND = 11;
  MAPI_E_ATTACHMENT_OPEN_FAILURE = 12;
  MAPI_E_ATTACHMENT_WRITE_FAILURE = 13;
  MAPI_E_UNKNOWN_RECIPIENT = 14;
  MAPI_E_BAD_RECIPTYPE = 15;
  MAPI_E_NO_MESSAGES = 16;
  MAPI_E_INVALID_MESSAGE = 17;
  MAPI_E_TEXT_TOO_LARGE = 18;
  MAPI_E_INVALID_SESSION = 19;
  MAPI_E_TYPE_NOT_SUPPORTED = 20;
  MAPI_E_AMBIGUOUS_RECIPIENT = 21;
  MAPI_E_AMBIG_RECIP = MAPI_E_AMBIGUOUS_RECIPIENT;
  MAPI_E_MESSAGE_IN_USE = 22;
  MAPI_E_NETWORK_FAILURE = 23;
  MAPI_E_INVALID_EDITFIELDS = 24;
  MAPI_E_INVALID_RECIPS = 25;
  MAPI_E_NOT_SUPPORTED = 26;
  MAPI_E_UNICODE_NOT_SUPPORTED = 27;
  MAPI_E_ATTACHMENT_TOO_LARGE = 28;

  { The Windows Simple MAPI stub; it forwards to the default mail client. }
  MAPIDefaultLibrary = 'mapi32.dll';

// Log on to the messaging system and return the session in lplhSession^.
function MAPILogon(ulUIParam: ULONG_PTR; lpszProfileName, lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lplhSession: LPLHANDLE): ULONG;
// End the session lhSession.
function MAPILogoff(lhSession: LHANDLE; ulUIParam: ULONG_PTR; flFlags: FLAGS; ulReserved: ULONG): ULONG;
// Send lpMessage (ANSI strings).
function MAPISendMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG): ULONG;
// Send lpMessage (UTF-16 strings); MAPI_E_NOT_SUPPORTED when the library has no MAPISendMailW.
function MAPISendMailW(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessageW; flFlags: FLAGS; ulReserved: ULONG): ULONG;
// Open a send dialog with the files lpszFilePaths, separated by lpszDelimChar^, attached.
function MAPISendDocuments(ulUIParam: ULONG_PTR; lpszDelimChar, lpszFilePaths, lpszFileNames: LPSTR; ulReserved: ULONG): ULONG;
// Return in lpszMessageID the identifier of the message after lpszSeedMessageID.
function MAPIFindNext(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageType, lpszSeedMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG;
// Read message lpszMessageID into lppMessage^; free it with MAPIFreeBuffer.
function MAPIReadMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppMessage: PlpMapiMessage): ULONG;
// Save lpMessage and return its identifier in lpszMessageID.
function MAPISaveMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG;
// Delete message lpszMessageID.
function MAPIDeleteMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG): ULONG;
// Free memory that a MAPI call allocated.
function MAPIFreeBuffer(pv: LPVOID): ULONG;
// Show the address book and return the chosen recipients in lppNewRecips^.
function MAPIAddress(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszCaption: LPSTR; nEditFields: ULONG; lpszLabels: LPSTR; nRecips: ULONG;
  lpRecips: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG; lpnNewRecips: LPULONG; lppNewRecips: PlpMapiRecipDesc): ULONG;
// Show the details of recipient lpRecip.
function MAPIDetails(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpRecip: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG): ULONG;
// Resolve the name lpszName to a recipient in lppRecip^.
function MAPIResolveName(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszName: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppRecip: PlpMapiRecipDesc): ULONG;

// Load the Simple MAPI library aLibrary, or MAPIDefaultLibrary if aLibrary is empty; True on success.
function InitMAPI(const aLibrary: UnicodeString = ''): Boolean;
// Unload the library loaded by InitMAPI; the MAPI functions load MAPIDefaultLibrary again when called.
procedure FreeMAPI;
// True if a Simple MAPI library is loaded.
function MAPILoaded: Boolean;
// Name of the default mail client in the registry, '' if none is set.
function DefaultMailClient: UnicodeString;
// Path of the Simple MAPI DLL of the default mail client, '' if the registry names none.
function DefaultMailClientLibrary: UnicodeString;

implementation

type
  TMAPILogon = function(ulUIParam: ULONG_PTR; lpszProfileName, lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lplhSession: LPLHANDLE): ULONG; stdcall;
  TMAPILogoff = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; flFlags: FLAGS; ulReserved: ULONG): ULONG; stdcall;
  TMAPISendMail = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG): ULONG; stdcall;
  TMAPISendMailW = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessageW; flFlags: FLAGS; ulReserved: ULONG): ULONG; stdcall;
  TMAPISendDocuments = function(ulUIParam: ULONG_PTR; lpszDelimChar, lpszFilePaths, lpszFileNames: LPSTR; ulReserved: ULONG): ULONG; stdcall;
  TMAPIFindNext = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageType, lpszSeedMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG; stdcall;
  TMAPIReadMail = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppMessage: PlpMapiMessage): ULONG; stdcall;
  TMAPISaveMail = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG; stdcall;
  TMAPIDeleteMail = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG): ULONG; stdcall;
  TMAPIFreeBuffer = function(pv: LPVOID): ULONG; stdcall;
  TMAPIAddress = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszCaption: LPSTR; nEditFields: ULONG; lpszLabels: LPSTR; nRecips: ULONG;
    lpRecips: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG; lpnNewRecips: LPULONG; lppNewRecips: PlpMapiRecipDesc): ULONG; stdcall;
  TMAPIDetails = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpRecip: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG): ULONG; stdcall;
  TMAPIResolveName = function(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszName: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppRecip: PlpMapiRecipDesc): ULONG; stdcall;

var
  MapiLib: HMODULE = 0;
  MapiLibTried: Boolean = False;


// The entry aName of the MAPI library, loading MAPIDefaultLibrary when none is loaded; nil if absent
function MapiProc(aName: PAnsiChar): Pointer;

begin
  if (MapiLib=0) and not MapiLibTried then
    InitMAPI;
  if MapiLib=0 then
    Result:=nil
  else
    Result:=GetProcAddress(MapiLib, aName);
end;


function InitMAPI(const aLibrary: UnicodeString): Boolean;

var
  lName: UnicodeString;

begin
  FreeMAPI;
  MapiLibTried:=True;
  lName:=aLibrary;
  if lName='' then
    lName:=MAPIDefaultLibrary;
  MapiLib:=LoadLibraryW(PWideChar(lName));
  Result:=MapiLib<>0;
end;


procedure FreeMAPI;

begin
  if MapiLib<>0 then
    FreeLibrary(MapiLib);
  MapiLib:=0;
  MapiLibTried:=False;
end;


function MAPILoaded: Boolean;

begin
  Result:=MapiLib<>0;
end;


// The string value aValue of registry key aRoot\aKey, '' if it does not exist
function RegString(aRoot: HKEY; const aKey, aValue: UnicodeString): UnicodeString;

var
  lKey: HKEY;
  lType, lSize: DWORD;
  lValue: PWideChar;

begin
  Result:='';
  if RegOpenKeyExW(aRoot, PWideChar(aKey), 0, KEY_READ, lKey)<>ERROR_SUCCESS then
    Exit;
  try
    if aValue='' then
      lValue:=nil
    else
      lValue:=PWideChar(aValue);
    lSize:=0;
    if (RegQueryValueExW(lKey, lValue, nil, @lType, nil, @lSize)<>ERROR_SUCCESS)
      or ((lType<>REG_SZ) and (lType<>REG_EXPAND_SZ)) or (lSize<SizeOf(WideChar)) then
      Exit;
    SetLength(Result, lSize div SizeOf(WideChar));
    if RegQueryValueExW(lKey, lValue, nil, @lType, PByte(PWideChar(Result)), @lSize)<>ERROR_SUCCESS then
      Exit('');
    SetLength(Result, Length(PWideChar(Result)));
  finally
    RegCloseKey(lKey);
  end;
end;


// aPath with environment variables expanded
function ExpandEnv(const aPath: UnicodeString): UnicodeString;

var
  lLen: DWORD;

begin
  Result:=aPath;
  lLen:=ExpandEnvironmentStringsW(PWideChar(aPath), nil, 0);
  if lLen=0 then
    Exit;
  SetLength(Result, lLen);
  lLen:=ExpandEnvironmentStringsW(PWideChar(aPath), PWideChar(Result), lLen);
  SetLength(Result, Length(PWideChar(Result)));
end;


const
  cMailClients = 'Software\Clients\Mail';


function DefaultMailClient: UnicodeString;

begin
  Result:=RegString(HKEY_CURRENT_USER, cMailClients, '');
  if Result='' then
    Result:=RegString(HKEY_LOCAL_MACHINE, cMailClients, '');
end;


function DefaultMailClientLibrary: UnicodeString;

var
  lKey: UnicodeString;

begin
  Result:='';
  lKey:=DefaultMailClient;
  if lKey='' then
    Exit;
  lKey:=cMailClients+'\'+lKey;
  Result:=RegString(HKEY_LOCAL_MACHINE, lKey, 'DLLPathEx');
  if Result='' then
    Result:=RegString(HKEY_LOCAL_MACHINE, lKey, 'DLLPath');
  if Result<>'' then
    Result:=ExpandEnv(Result);
end;


function MAPILogon(ulUIParam: ULONG_PTR; lpszProfileName, lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lplhSession: LPLHANDLE): ULONG;

var
  lProc: TMAPILogon;

begin
  lProc:=TMAPILogon(MapiProc('MAPILogon'));
  if Assigned(lProc) then
    Result:=lProc(ulUIParam, lpszProfileName, lpszPassword, flFlags, ulReserved, lplhSession)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPILogoff(lhSession: LHANDLE; ulUIParam: ULONG_PTR; flFlags: FLAGS; ulReserved: ULONG): ULONG;

var
  lProc: TMAPILogoff;

begin
  lProc:=TMAPILogoff(MapiProc('MAPILogoff'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, flFlags, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPISendMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG): ULONG;

var
  lProc: TMAPISendMail;

begin
  lProc:=TMAPISendMail(MapiProc('MAPISendMail'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpMessage, flFlags, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPISendMailW(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessageW; flFlags: FLAGS; ulReserved: ULONG): ULONG;

var
  lProc: TMAPISendMailW;

begin
  lProc:=TMAPISendMailW(MapiProc('MAPISendMailW'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpMessage, flFlags, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPISendDocuments(ulUIParam: ULONG_PTR; lpszDelimChar, lpszFilePaths, lpszFileNames: LPSTR; ulReserved: ULONG): ULONG;

var
  lProc: TMAPISendDocuments;

begin
  lProc:=TMAPISendDocuments(MapiProc('MAPISendDocuments'));
  if Assigned(lProc) then
    Result:=lProc(ulUIParam, lpszDelimChar, lpszFilePaths, lpszFileNames, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIFindNext(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageType, lpszSeedMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG;

var
  lProc: TMAPIFindNext;

begin
  lProc:=TMAPIFindNext(MapiProc('MAPIFindNext'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpszMessageType, lpszSeedMessageID, flFlags, ulReserved, lpszMessageID)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIReadMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppMessage: PlpMapiMessage): ULONG;

var
  lProc: TMAPIReadMail;

begin
  lProc:=TMAPIReadMail(MapiProc('MAPIReadMail'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpszMessageID, flFlags, ulReserved, lppMessage)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPISaveMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpMessage: lpMapiMessage; flFlags: FLAGS; ulReserved: ULONG; lpszMessageID: LPSTR): ULONG;

var
  lProc: TMAPISaveMail;

begin
  lProc:=TMAPISaveMail(MapiProc('MAPISaveMail'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpMessage, flFlags, ulReserved, lpszMessageID)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIDeleteMail(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: ULONG): ULONG;

var
  lProc: TMAPIDeleteMail;

begin
  lProc:=TMAPIDeleteMail(MapiProc('MAPIDeleteMail'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpszMessageID, flFlags, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIFreeBuffer(pv: LPVOID): ULONG;

var
  lProc: TMAPIFreeBuffer;

begin
  lProc:=TMAPIFreeBuffer(MapiProc('MAPIFreeBuffer'));
  if Assigned(lProc) then
    Result:=lProc(pv)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIAddress(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszCaption: LPSTR; nEditFields: ULONG; lpszLabels: LPSTR; nRecips: ULONG;
  lpRecips: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG; lpnNewRecips: LPULONG; lppNewRecips: PlpMapiRecipDesc): ULONG;

var
  lProc: TMAPIAddress;

begin
  lProc:=TMAPIAddress(MapiProc('MAPIAddress'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpszCaption, nEditFields, lpszLabels, nRecips, lpRecips, flFlags, ulReserved, lpnNewRecips, lppNewRecips)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIDetails(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpRecip: lpMapiRecipDesc; flFlags: FLAGS; ulReserved: ULONG): ULONG;

var
  lProc: TMAPIDetails;

begin
  lProc:=TMAPIDetails(MapiProc('MAPIDetails'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpRecip, flFlags, ulReserved)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


function MAPIResolveName(lhSession: LHANDLE; ulUIParam: ULONG_PTR; lpszName: LPSTR; flFlags: FLAGS; ulReserved: ULONG; lppRecip: PlpMapiRecipDesc): ULONG;

var
  lProc: TMAPIResolveName;

begin
  lProc:=TMAPIResolveName(MapiProc('MAPIResolveName'));
  if Assigned(lProc) then
    Result:=lProc(lhSession, ulUIParam, lpszName, flFlags, ulReserved, lppRecip)
  else
    Result:=MAPI_E_NOT_SUPPORTED;
end;


finalization
  FreeMAPI;
end.
