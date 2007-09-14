{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Declarations for tapi WinCE API, note than on WinCE simmgr functions originally
  based in coredll.dll

  SIM Manager is an API set that allows access to information stored on the SIM Card

 This port based on Windows Mobile 5 Smartphone Edition SDK
  contact S0vNark0m for more information snk_post@mail.ru (en, ru)
}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************

const
  SIM_ERRORCLASS_SIM      = $02; // Problem with the SIM
  SIM_ERRORCLASS_PASSWORD = $01; // Unspecified phone failure
  SIM_ERRORCLASS_STORAGE  = $03; // Error relating to storage
  SIM_ERRORCLASS_NONE     = $00; // Misc error

  FACILITY_SIM = $800;

  ERROR_BASE = Longword(SEVERITY_ERROR) shl 31 or Longword(FACILITY_SIM) shl 16; // C Macros conversion, internal def

  // Internal def = errclass shl 8
  MSE_ERRORCLASS_SIM = ERROR_BASE or Word(SIM_ERRORCLASS_SIM) shl 8;
  MSE_ERRORCLASS_PASSWORD = ERROR_BASE or Word(SIM_ERRORCLASS_PASSWORD) shl 8;
  MSE_ERRORCLASS_STORAGE = ERROR_BASE or Word(SIM_ERRORCLASS_STORAGE) shl 8;
  MSE_ERRERRORCLASS_NONE = ERROR_BASE or Word(SIM_ERRORCLASS_NONE) shl 8;

  // Error | Various errors
  SIM_E_SIMFAILURE       = MSE_ERRORCLASS_SIM or $01; // SIM failure was detected
  SIM_E_SIMBUSY          = MSE_ERRORCLASS_SIM or $02; // SIM is busy
  SIM_E_SIMWRONG         = MSE_ERRORCLASS_SIM or $03; // Inorrect SIM was inserted
  SIM_E_NOSIMMSGSTORAGE  = MSE_ERRORCLASS_SIM or $04; // SIM isn't capable of storing messages
  SIM_E_SIMTOOLKITBUSY   = MSE_ERRORCLASS_SIM or $05; // SIM Application Toolkit is busy
  SIM_E_SIMDOWNLOADERROR = MSE_ERRORCLASS_SIM or $06; // SIM data download error
  SIM_E_SIMNOTINSERTED   = MSE_ERRORCLASS_SIM or $07; // SIM isn't inserted into the phone

  SIM_E_PHSIMPINREQUIRED  = MSE_ERRORCLASS_PASSWORD or $20; // PH-SIM PIN is required to perform this operation
  SIM_E_PHFSIMPINREQUIRED = MSE_ERRORCLASS_PASSWORD or $21; // PH-FSIM PIN is required to perform this operation
  SIM_E_PHFSIMPUKREQUIRED = MSE_ERRORCLASS_PASSWORD or $22; // PH-FSIM PUK is required to perform this operation
  SIM_E_SIMPINREQUIRED    = MSE_ERRORCLASS_PASSWORD or $23; // SIM PIN is required to perform this operation
  SIM_E_SIMPUKREQUIRED    = MSE_ERRORCLASS_PASSWORD or $24; // SIM PUK is required to perform this operation
  SIM_E_INCORRECTPASSWORD = MSE_ERRORCLASS_PASSWORD or $25; // Incorrect password was supplied
  SIM_E_SIMPIN2REQUIRED   = MSE_ERRORCLASS_PASSWORD or $26; // SIM PIN2 is required to perform this operation
  SIM_E_SIMPUK2REQUIRED   = MSE_ERRORCLASS_PASSWORD or $27; // SIM PUK2 is required to perform this operation
  SIM_E_NETWKPINREQUIRED  = MSE_ERRORCLASS_PASSWORD or $28; // Network Personalization PIN is required to perform this operation
  SIM_E_NETWKPUKREQUIRED  = MSE_ERRORCLASS_PASSWORD or $29; // Network Personalization PUK is required to perform this operation
  SIM_E_SUBSETPINREQUIRED = MSE_ERRORCLASS_PASSWORD or $2A; // Network Subset Personalization PIN is required to perform this operation
  SIM_E_SUBSETPUKREQUIRED = MSE_ERRORCLASS_PASSWORD or $2B; // Network Subset Personalization PUK is required to perform this operation
  SIM_E_SVCPINREQUIRED    = MSE_ERRORCLASS_PASSWORD or $2C; // Service Provider Personalization PIN is required to perform this operation
  SIM_E_SVCPUKREQUIRED    = MSE_ERRORCLASS_PASSWORD or $2D; // Service Provider Personalization PUK is required to perform this operation
  SIM_E_CORPPINREQUIRED   = MSE_ERRORCLASS_PASSWORD or $2E; // Corporate Personalization PIN is required to perform this operation
  SIM_E_CORPPUKREQUIRED   = MSE_ERRORCLASS_PASSWORD or $2F; // Corporate Personalization PUK is required to perform this operation

  SIM_E_MEMORYFULL        = MSE_ERRORCLASS_STORAGE or $40; // Storage memory is full
  SIM_E_INVALIDINDEX      = MSE_ERRORCLASS_STORAGE or $41; // Invalid storage index was supplied
  SIM_E_NOTFOUND          = MSE_ERRORCLASS_STORAGE or $42; // A requested storage entry was not found
  SIM_E_MEMORYFAILURE     = MSE_ERRORCLASS_STORAGE or $43; // Storage memory failure
  SIM_E_SIMMSGSTORAGEFULL = MSE_ERRORCLASS_STORAGE or $44; // Message storage on the SIM is full
  SIM_E_EMPTYINDEX        = MSE_ERRORCLASS_STORAGE or $45; // Storage location is empty

  SIM_E_NOTREADY          = MSE_ERRERRORCLASS_NONE or $60; // SIM isn't yet ready to perform the requested operation
  SIM_E_SECURITYFAILURE   = MSE_ERRERRORCLASS_NONE or $61; // SIM isn't yet ready to perform the requested operation
  SIM_E_BUFFERTOOSMALL    = MSE_ERRERRORCLASS_NONE or $62; // Buffer too small
  SIM_E_NOTTEXTMESSAGE    = MSE_ERRERRORCLASS_NONE or $63; // Requested SMS message is not a text message
  SIM_E_NOSIM             = MSE_ERRERRORCLASS_NONE or $64; // Device doesn't have a SIM
  SIM_E_NETWORKERROR      = MSE_ERRERRORCLASS_NONE or $65; // There was a network error
  SIM_E_MOBILEERROR       = MSE_ERRERRORCLASS_NONE or $66; // Mobile error
  SIM_E_UNSUPPORTED       = MSE_ERRERRORCLASS_NONE or $67; // The command is unsupported
  SIM_E_BADPARAM          = MSE_ERRERRORCLASS_NONE or $68; // Bad parameter
  SIM_E_UNDETERMINED      = MSE_ERRERRORCLASS_NONE or $69; // Undetermined error
  SIM_E_RADIONOTPRESENT   = MSE_ERRERRORCLASS_NONE or $6A; // The Radio is not present
  SIM_E_RADIOOFF          = MSE_ERRERRORCLASS_NONE or $6B; // The Radio is off

  // Callback | Various notifications that are passed to the callback
  
  SIM_NOTIFY_CARD_REMOVED = $100; // SIM card was removed; lpData is NULL
  SIM_NOTIFY_FILE_REFRESH = $101; // Files on the SIM were refreshed; lpData points to a SIMFILEREFRESH structure
  SIM_NOTIFY_MSG_STORED   = $102; // A message was stored to the SIM; lpData points to a SIMMESSAGECHANGE structure
  SIM_NOTIFY_MSG_DELETED  = $103; // A message was removed from the SIM; lpData points to a SIMMESSAGECHANGE structure
  SIM_NOTIFY_PBE_STORED   = $104; // A phone book entry was stored to the SIM; lpData points to a SIMPBECHANGE structure
  SIM_NOTIFY_PBE_DELETED  = $105; // A phone book entry was removed from the SIM; lpData points to a SIMPBECHANGE structure
  SIM_NOTIFY_MSG_RECEIVED = $106; // Class 2 SMS was sent directly to the SIM; lpData points to a SIMMESSAGECHANGE structure
  SIM_NOTIFY_RADIOOFF     = $107; // The Radio has been turned off but AT interpreter is still on; lpData is NULL
  SIM_NOTIFY_RADIOON      = $108; // The Radio is present and is now on; lpData is NULL
  SIM_NOTIFY_RADIOPRESENT = $109; // A Radio Module/Driver has been installed; lpData is points to a DWORD which is 0 if the radio is OFF and 1 if the radio is ON
  SIM_NOTIFY_RADIOREMOVED = $10A; // A Radio Module/Driver has been removed; lpData is NULL }

  // Parameter flags for SimInitialize
  SIM_INIT_NONE = $00000000;                  // Do not send any notifications
  SIM_INIT_SIMCARD_NOTIFICATIONS = $00000001; // Send SIM card related notifications

  // SIMPHONEBOOKENTRY

  SIM_PARAM_PBE_ADDRESS      = $00000001; // lpszAddress field is valid
  SIM_PARAM_PBE_ADDRESS_TYPE = $00000002; // dwAddressType field is valid
  SIM_PARAM_PBE_NUMPLAN      = $00000004; // dwNumPlan field is valid
  SIM_PARAM_PBE_TEXT         = $00000008; // lpszText field is valid
  SIM_PARAM_PBE_ALL          = $0000000f; // All fields are valid

  // SIMMESSAGE

  SIM_PARAM_MSG_ADDRESS       = $00000001; // lpszAddress field is valid
  SIM_PARAM_MSG_ADDRESS_TYPE  = $00000002; // dwAddressType field is valid
  SIM_PARAM_MSG_NUMPLAN       = $00000004; // dwNumPlan field is valid
  SIM_PARAM_MSG_RECEIVE_TIME  = $00000008; // stReceiveTime field is valid
  SIM_PARAM_MSG_HEADER        = $00000010; // rgbHeader field is valid
  SIM_PARAM_MSG_HEADER_LENGTH = $00000020; // cbHdrLength field is valid
  SIM_PARAM_MSG_MESSAGE       = $00000040; // lpszMessage field is valid
  SIM_PARAM_MSG_ALL           = $0000007f; // All fields are valid

  // SIMCAPS }

  SIM_PARAM_CAPS_PBSTORAGES          = $00000001; // dwPBStorages field is valid
  SIM_PARAM_CAPS_PBEMAXADDRESSLENGTH = $00000002; // dwPBEMaxAddressLength field is valid
  SIM_PARAM_CAPS_PBEMAXTEXTLENGTH    = $00000004; // dwPBEMaxTextLength field is valid
  SIM_PARAM_CAPS_PBEMININDEX         = $00000008; // dwMinPBIndex field is valid
  SIM_PARAM_CAPS_PBEMAXINDEX         = $00000010; // dwMaxPBIndex field is valid
  SIM_PARAM_CAPS_LOCKFACILITIES      = $00000020; // dwLockFacilities field is valid
  SIM_PARAM_CAPS_LOCKINGPWDLENGTH    = $00000040; // dwNumLockingPwdLengths and rgLockingPwdLengths fields are valid
  SIM_PARAM_CAPS_READMSGSTORAGES     = $00000080; // dwReadMsgStorages field is valid
  SIM_PARAM_CAPS_WRITEMSGSTORAGES    = $00000100; // dwWriteMsgStorages field is valid
  SIM_PARAM_CAPS_ALL                 = $000001ff; // All fields are valid

  // SIMRECORDINFO

  SIM_PARAM_RECORDINFO_RECORDTYPE = $00000001; // dwRecordType field is valid
  SIM_PARAM_RECORDINFO_ITEMCOUNT  = $00000002; // dwItemCount field is valid
  SIM_PARAM_RECORDINFO_SIZE       = $00000004; // dwSize field is valid
  SIM_PARAM_RECORDINFO_ALL        = $00000007; // All fields are valid

  // SIMFILEREFRESH }

  SIM_PARAM_FILEREFRESH_FLAGS     = $00000001; // dwFlags field is valid
  SIM_PARAM_FILEREFRESH_FILECOUNT = $00000002; // dwFileCount field is valid
  SIM_PARAM_FILEREFRESH_FILEARRAY = $00000004; // rgdwAddress field is valid
  SIM_PARAM_FILEREFRESH_ALL       = $00000007; // All fields are valid

  // Phonebook Storage | Phone book storage locations }

  SIM_PBSTORAGE_EMERGENCY    = $00000001; // Emergency dial list
  SIM_PBSTORAGE_FIXEDDIALING = $00000002; // SIM fixed dialing list
  SIM_PBSTORAGE_LASTDIALING  = $00000004; // SIM last dialing list
  SIM_PBSTORAGE_OWNNUMBERS   = $00000008; // SIM ownnumbers lists
  SIM_PBSTORAGE_SIM          = $00000010; // General SIM Storage
  SIM_NUMPBSTORAGES          = 5;         // Number of phonebook storages

  // SIM DevCaps | Device Capabilities }

  SIM_CAPSTYPE_PBENTRYLENGTH      = $00000001; // Phonebook entry lengths
  SIM_CAPSTYPE_PBSTORELOCATIONS   = $00000002; // Phonebook storage locations
  SIM_CAPSTYPE_LOCKFACILITIES     = $00000004; // Lock facilities
  SIM_CAPSTYPE_PBINDEXRANGE       = $00000008; // Valid phonebook entry indexes
  SIM_CAPSTYPE_LOCKINGPWDLENGTHS  = $00000010; // Locking password lengths
  SIM_CAPSTYPE_MSGMEMORYLOCATIONS = $00000020; // Message memory locations
  SIM_CAPSTYPE_ALL                = $0000003F; // All of the above

  // SMS Storage | SMS storage locations }

  SIM_SMSSTORAGE_BROADCAST = $00000001; // Broadcast message storage location
  SIM_SMSSTORAGE_SIM       = $00000002; // SIM storage location
  SIM_NUMSMSSTORAGES       = 2;         // Number of message storage locations

  // Address Type | Defines different address types }

  SIM_ADDRTYPE_UNKNOWN       = $00000000; // Unknown
  SIM_ADDRTYPE_INTERNATIONAL = $00000001; // International number
  SIM_ADDRTYPE_NATIONAL      = $00000002; // National number
  SIM_ADDRTYPE_NETWKSPECIFIC = $00000003; // Network specific number
  SIM_ADDRTYPE_SUBSCRIBER    = $00000004; // Subscriber number (protocol-specific)
  SIM_ADDRTYPE_ALPHANUM      = $00000005; // Alphanumeric address
  SIM_ADDRTYPE_ABBREV        = $00000006; // Abbreviated number

  // Numbering Plan | Defines different numbering plans for SIM_ADDRTYPE_UNKNOWN,
  // SIM_ADDRTYPE_INTERNATIONAL, and SIM_ADDRTYPE_NATIONAL

  SIM_NUMPLAN_UNKNOWN   = $00000000; // Unknown
  SIM_NUMPLAN_TELEPHONE = $00000001; // ISDN/telephone numbering plan (E.164/E.163)
  SIM_NUMPLAN_DATA      = $00000002; // Data numbering plan (X.121)
  SIM_NUMPLAN_TELEX     = $00000003; // Telex numbering plan
  SIM_NUMPLAN_NATIONAL  = $00000004; // National numbering plan
  SIM_NUMPLAN_PRIVATE   = $00000005; // Private numbering plan
  SIM_NUMPLAN_ERMES     = $00000006; // ERMES numbering plan (ETSI DE/PS 3 01-3)

  // Phone Locked | Indicates if the phone is currently locked (i.e.
  // awaiting password) and what password to enter

  SIM_LOCKEDSTATE_UNKNOWN       = $00000000; // Locking state is unknown
  SIM_LOCKEDSTATE_READY         = $00000001; // Not awaiting a password (unlocked)
  SIM_LOCKEDSTATE_SIM_PIN       = $00000002; // Awaiting the SIM PIN
  SIM_LOCKEDSTATE_SIM_PUK       = $00000003; // Awaiting the SIM PUK
  SIM_LOCKEDSTATE_PH_SIM_PIN    = $00000004; // Awaiting the Phone to SIM Personalization PIN
  SIM_LOCKEDSTATE_PH_FSIM_PIN   = $00000005; // Awaiting the Phone to first SIM Personalization PIN
  SIM_LOCKEDSTATE_PH_FSIM_PUK   = $00000006; // Awaiting the Phone to first SIM Personalization PUK
  SIM_LOCKEDSTATE_SIM_PIN2      = $00000007; // Awaiting the SIM PIN2
  SIM_LOCKEDSTATE_SIM_PUK2      = $00000008; // Awaiting the SIM PUK2
  SIM_LOCKEDSTATE_PH_NET_PIN    = $00000009; // Awaiting the Network Personalization PIN
  SIM_LOCKEDSTATE_PH_NET_PUK    = $0000000a; // Awaiting the Network Personalization PUK
  SIM_LOCKEDSTATE_PH_NETSUB_PIN = $0000000b; // Awaiting the Network Subset Personalization PIN
  SIM_LOCKEDSTATE_PH_NETSUB_PUK = $0000000c; // Awaiting the Network Subset Personalization PUK
  SIM_LOCKEDSTATE_PH_SP_PIN     = $0000000d; // Awaiting the Service Provider Personalization PIN
  SIM_LOCKEDSTATE_PH_SP_PUK     = $0000000e; // Awaiting the Service Provider Personalization PUK
  SIM_LOCKEDSTATE_PH_CORP_PIN   = $0000000f; // Awaiting the Corporate Personalization PIN
  SIM_LOCKEDSTATE_PH_CORP_PUK   = $00000010; // Awaiting the Corporate Personalization PUK

  // Phonebook Misc | Special phonebook constants }
  
  SIM_PBINDEX_FIRSTAVAILABLE = $ffffffff; //  Use first phonebook storage entry available

  // Phone Locking | Indicates the phone's locking behavior }

  SIM_LOCKFACILITY_CNTRL           = $00000001; // Lock control curface
  SIM_LOCKFACILITY_PH_SIM          = $00000002; // Lock phone to SIM card
  SIM_LOCKFACILITY_PH_FSIM         = $00000004; // Lock phone to first SIM card
  SIM_LOCKFACILITY_SIM             = $00000008; // Lock SIM card
  SIM_LOCKFACILITY_SIM_PIN2        = $00000010; // Lock SIM card
  SIM_LOCKFACILITY_SIM_FIXEDIALING = $00000020; // SIM fixed dialing memory
  SIM_LOCKFACILITY_NETWORKPERS     = $00000040; // Network personalization
  SIM_LOCKFACILITY_NETWORKSUBPERS  = $00000080; // Network subset personalization
  SIM_LOCKFACILITY_SERVICEPROVPERS = $00000100; // Service provider personalization
  SIM_LOCKFACILITY_CORPPERS        = $00000200; // Corporate personalization
  SIM_NUMLOCKFACILITIES            = 10;        // Number of locking facilities

  // SIM Record | Different SIM file types
  SIM_RECORDTYPE_UNKNOWN     = $00000000; // An unknown file type
  SIM_RECORDTYPE_TRANSPARENT = $00000001; // A single veriable lengthed record
  SIM_RECORDTYPE_CYCLIC      = $00000002; // A cyclic set of records, each of the same length
  SIM_RECORDTYPE_LINEAR      = $00000003; // A linear set of records, each of the same length
  SIM_RECORDTYPE_MASTER      = $00000004; // Every SIM has a single master record, effectively the head node
  SIM_RECORDTYPE_DEDICATED   = $00000005; // Effectively a "directory" file which is a parent of other records

  // SIM Record Refresh | Different ways of being notified that SIM
  // have been updated
  SIMFILE_FULLFILECHANGE = $00000001; //  All files have been changed
  SIMFILE_FILECHANGE     = $00000002; // Only a few files have been changed
  SIMFILE_SIMINIT        = $00000004; // SIM Initiailization
  SIMFILE_SIMRESET       = $00000008; // Reset the SIM

  // Max_Length | Maximum length constants }
  MAX_LENGTH_ADDRESS            = 256; // Maximum length of an address
  MAX_LENGTH_PHONEBOOKENTRYTEXT = 256; // Maximum length of text in a phonebook entry
  MAX_LENGTH_HEADER             = 256; // Maximum length of a SMS header
  MAX_LENGTH_MESSAGE            = 256; // Maximum length of a SMS message
  MAX_FILES                     = 32;  // Maximum number of files in a file change list

//*****************************************************************************
// types
//*****************************************************************************

type

  // HSIM | Handle to a SIM }

  HSIM = HANDLE;
  LPHSIM = ^HSIM;

  // SIMPHONEBOOKENTRY | A SIM phonebook entry

  simphonebookentry_tag = record
       cbSize : DWORD;   // Size of the structure in bytes
       dwParams : DWORD; // Indicates valid parameter values
       lpszAddress : array[0..(MAX_LENGTH_ADDRESS)-1] of TCHAR; // The actual phone number
       dwAddressType : DWORD; // A SIM_ADDRTYPE_* constant
       dwNumPlan : DWORD;     // A SIM_NUMPLAN_* constant
       lpszText : array[0..(MAX_LENGTH_PHONEBOOKENTRYTEXT)-1] of TCHAR; // Text assocaited with the entry
    end;
  TSIMPHONEBOOKENTRY = simphonebookentry_tag;
  LPSIMPHONEBOOKENTRY = ^simphonebookentry_tag;
  
  // SIMMESSAGE | A SIM message entry

  simmessage_tag = record
       cbSize : DWORD;   // Size of the structure in bytes
       dwParams : DWORD; // Indicates valid parameter values
       lpszAddress : array[0..(MAX_LENGTH_ADDRESS)-1] of TCHAR; // The actual phone number
       dwAddressType : DWORD; // A SIM_ADDRTYPE_* constant
       dwNumPlan : DWORD;     // A SIM_NUMPLAN_* constant
       stReceiveTime : SYSTEMTIME; // Timestamp for the incoming message
       cbHdrLength : DWORD;        // Header length in bytes
       rgbHeader : array[0..(MAX_LENGTH_HEADER)-1] of BYTE;     // The actual header data
       lpszMessage : array[0..(MAX_LENGTH_MESSAGE)-1] of TCHAR; // The actual message data
    end;
  TSIMMESSAGE = simmessage_tag;
  LPSIMMESSAGE = ^simmessage_tag;

  // SIMLOCKINGPWDLENGTH | Minimum password length }

  simlockingpwdlength = record
       dwFacility : DWORD;       // The locking facility
       dwPasswordLength : DWORD; // The minimum password length
    end;
  LPSIMLOCKINGPWDLENGTH = ^simlockingpwdlength;

  // SIMCAPS | Capabilities of the SIM
  simcaps_tag = record
       cbSize : DWORD;                 // Size of the structure in bytes
       dwParams : DWORD;               // Indicates valid parameter values
       dwPBStorages : DWORD;           // Supported phonebook storages
       dwMinPBIndex : DWORD;           // Minimum phonebook storages
       dwMaxPBIndex : DWORD;           // Maximum phonebook storages
       dwMaxPBEAddressLength : DWORD;  // Maximum address length of phonebook entries
       dwMaxPBETextLength : DWORD;     // Maximum text length of phonebook entries
       dwLockFacilities : DWORD;       // Supported locking facilities
       dwReadMsgStorages : DWORD;      // Supported read message stores
       dwWriteMsgStorages : DWORD;     // Supported write message stores
       dwNumLockingPwdLengths : DWORD; // Number of entries in rgLockingPwdLengths
       rgLockingPwdLengths : array[0..(SIM_NUMLOCKFACILITIES)-1] of SIMLOCKINGPWDLENGTH; // Password lengths for each facility
    end;
  TSIMCAPS = simcaps_tag;
  LPSIMCAPS = ^simcaps_tag;

 // SIMRECORDINFO | Information about a particular SIM file

  simrecordinfo_tag = record
       cbSize : DWORD;       // Size of the structure in bytes
       dwParams : DWORD;     // Indicates valid parameter values
       dwRecordType : DWORD; // SIM_RECORDTYPE_* Constant
       dwItemCount : DWORD;  // Number of items in the record
       dwSize : DWORD;       // Size in bytes of each item
    end;
  TSIMRECORDINFO = simrecordinfo_tag;
  LPSIMRECORDINFO = ^simrecordinfo_tag;

 // SIMFILEREFRESH | Information about which file(s) have been updated

  simfilerefresh_tag = record
       cbSize : DWORD;       // Size of the structure in bytes
       dwParams : DWORD;     // Indicates valid parameter values
       dwFlags : DWORD;      // Combination of SIMFILE_* constants
       dwFileCount : DWORD;  // Number of files in the update list
       rgdwAddress : array[0..(MAX_FILES)-1] of DWORD; // Array of files
    end;
  TSIMFILEREFRESH = simfilerefresh_tag;
  LPSIMFILEREFRESH = ^simfilerefresh_tag;

 // SIMPBECHANGE | Information about which SIM Phonebook entries have changed

  simpbechange_tag = record
       dwEntry : DWORD;   // The index of the entry that has changed
       dwStorage : DWORD; // SIM_PBSTORAGE_* constant detailing which phonebook this entry is in
    end;
  TSIMPBECHANGE = simpbechange_tag;
  LPSIMPBECHANGE = ^simpbechange_tag;

 // SIMMESSAGECHANGE | Information about which SMS messages on the SIM have changed }

  simmessagechange_tag = record
       dwEntry : DWORD;   // The index of the entry that has changed
       dwStorage : DWORD; // SIM_SMSSTORAGE_* constant details which storage location this entry is in
    end;
  TSIMMESSAGECHANGE = simmessagechange_tag;
  LPSIMMESSAGECHANGE = ^simmessagechange_tag;

  // This is the callback function prototype used by SIM Manager when
  // sending notifications.
  // -
  // Indicates type of notification received
  // Points to data structure specific to the notification
  // Size of data structure in bytes
  // Parameter passed to simInititialize
  TSIMCALLBACK = procedure (dwNotifyCode:DWORD; const pData:pointer; dwDataSize:DWORD; dwParam:DWORD);
  
//*****************************************************************************
// functions
//*****************************************************************************

// This function must be called by applications in order to use any of the
// functions in this API.
// Passing in a function callback is required only for applications that
// wish to obtain notifications.
// -
// Indicates which notifications to receive
// Function callback for notifications, may be NULL if notifications are not desired
// Parameter to pass on each notification function call, may be NULL
// Points to a HSIM handle to use on subsequent function calls
function SimInitialize(dwFlags:DWORD; lpfnCallBack:TSIMCALLBACK; dwParam:DWORD; lphSim:LPHSIM):HRESULT;external KernelDLL name 'SimInitialize';

// This function deinitializes an HSIM handle.
// -
// A valid HSIM handle to deinitialize
function SimDeinitialize(hSim:HSIM):HRESULT;external KernelDLL name 'SimDeinitialize';

// Gets the device capabilities of the SIM.
// -
// Points to a valid HSIM handle
// Which device capabilities are we interested in?
// Capabilities structure
function SimGetDevCaps(hSim:HSIM; dwCapsType:DWORD; lpSimCaps:LPSIMCAPS):HRESULT;external KernelDLL name 'SimGetDevCaps';

// Reads a phonebook entry off the SIM card.
// -
// Points to a valid HSIM handle
// A SIMPBSTORAGE_* Constant
// Index of the entry to retrieve
// Points to a phonebook entry structure
function SimReadPhonebookEntry(hSim:HSIM; dwLocation:DWORD; dwIndex:DWORD; lpPhonebookEntry:LPSIMPHONEBOOKENTRY):HRESULT;external KernelDLL name 'SimReadPhonebookEntry';

// Gets the status of a phonebook location.
// -
// Points to a valid HSIM handle
// A SIMPBSTORAGE_* Constant
// Nubmer of used locations
// Total number of locations
function SimGetPhonebookStatus(hSim:HSIM; dwLocation:DWORD; lpdwUsed:LPDWORD; lpdwTotal:LPDWORD):HRESULT;external KernelDLL name 'SimGetPhonebookStatus';

// Writes a phonebook entry to the SIM card.
// -
// Points to a valid HSIM handle
// A SIMPBSTORAGE_* Constant
// Index of the entry to retrieve (may be SIM_PBINDEX_FIRSTAVAILABLE)
// Points to a phonebook entry structure
function SimWritePhonebookEntry(hSim:HSIM; dwLocation:DWORD; dwIndex:DWORD; lpPhonebookEntry:LPSIMPHONEBOOKENTRY):HRESULT;external KernelDLL name 'SimWritePhonebookEntry';

// Deletes a phonebook entry from the SIM card.
// -
// Points to a valid HSIM handle
// A SIMPBSTORAGE_* Constant
// Index of the entry to retrieve
function SimDeletePhonebookEntry(hSim:HSIM; dwLocation:DWORD; dwIndex:DWORD):HRESULT;external KernelDLL name 'SimDeletePhonebookEntry';

// Checks if the SIM is currently awaiting a password.
// This is called when powering on the phone.
// -
// Points to a valid HSIM handle
// Points to a SIM_LOCKEDSTATE_* constant
function SimGetPhoneLockedState(hSim:HSIM; lpdwLockedState:LPDWORD):HRESULT;external KernelDLL name 'SimGetPhoneLockedState';

// Sends a password to unlock the phone.
// -
// Points to a valid HSIM handle
// Points to password string
// Some locked states require a second password (e.g. PUK requires a new PIN to replace the old, presumably forgotten PIN)
function SimUnlockPhone(hSim:HSIM; lpszPassword:LPTSTR; lpszNewPin:LPTSTR):HRESULT;external KernelDLL name 'SimUnlockPhone';

// Gets the locking status of the phone.
// -
// Points to a valid HSIM handle
// A SIMLOCKFACILITY_* constant
// Some facilities require a password
// Enabled or diabled
function SimGetLockingStatus(hSim:HSIM; dwLockingFacility:DWORD; lpszPassword:LPTSTR; var pfEnabled:BOOL):HRESULT;external KernelDLL name 'SimGetLockingStatus';

// Sets the locking status of the phone.
// -
// Points to a valid HSIM handle
// A SIMLOCKFACILITY_* constant
// Some facilities require a password
// Enable or diable
function SimSetLockingStatus(hSim:HSIM; dwLockingFacility:DWORD; lpszPassword:LPTSTR; fEnabled:BOOL):HRESULT;external KernelDLL name 'SimSetLockingStatus';

// Changes a locking password.
// -
// Points to a valid HSIM handle
// A SIMLOCKFACILITY_* constant
// The old password
// The new password
function SimChangeLockingPassword(hSim:HSIM; dwLockingFacility:DWORD; lpszOldPassword:LPTSTR; lpszNewPassword:LPTSTR):HRESULT;external KernelDLL name 'SimChangeLockingPassword';

// Gets the status of a SMS storage location.
// -
// Points to a valid HSIM handle
// A SIM_SMSSTORAGE_* constant
// Nubmer of used locations
// Total number of locations
function SimGetSmsStorageStatus(hSim:HSIM; dwStorage:DWORD; lpdwUsed:LPDWORD; lpdwTotal:LPDWORD):HRESULT;external KernelDLL name 'SimGetSmsStorageStatus';

// Reads an SMS from a particular storage location.
// -
// Points to a valid HSIM handle
// A SIM_SMSSTORAGE_* constant
// Index of the entry to retrieve
// Points to an SMS message structure
function SimReadMessage(hSim:HSIM; dwStorage:DWORD; dwIndex:DWORD; lpSimMessage:LPSIMMESSAGE):HRESULT;external KernelDLL name 'SimReadMessage';

// Writes an SMS to a particular storage location.
// Note that SMS messages are always written to the first available storage location
// -
// lpdwIndex will be set to the index to which the message was written
// Points to a valid HSIM handle
// A SIM_SMSSTORAGE_* constant
// Set to the index where the message was written
// Points to an SMS message structure
function SimWriteMessage(hSim:HSIM; dwStorage:DWORD; lpdwIndex:LPDWORD; lpSimMessage:LPSIMMESSAGE):HRESULT;external KernelDLL name 'SimWriteMessage';

// Deletes an SMS from a particular storage location.
// -
// Points to a valid HSIM handle
// A SIM_SMSSTORAGE_* constant
// Index of the entry to retrieve
function SimDeleteMessage(hSim:HSIM; dwStorage:DWORD; dwIndex:DWORD):HRESULT;external KernelDLL name 'SimDeleteMessage';

// Reads a file form the SIM.
// -
// Points to a valid HSIM handle
// SIM address
// A SIM_RECORDTYPE_* constant
// Applies only to SIM_RECORDTYPE_CYCLIC and SIM_RECORDTYPE_LINEAR, otherwise ignored
// Data buffer
// Size of data buffer
// Number of bytes read
function SimReadRecord(hSim:HSIM; dwAddress:DWORD; dwRecordType:DWORD; dwIndex:DWORD;
 lpData: LPBYTE; dwBufferSize: DWORD; lpdwBytesRead: LPDWORD):HRESULT;external KernelDLL name 'SimReadRecord';

// Writes a file to the SIM.
// -
// Points to a valid HSIM handle
// SIM address
// A SIM_RECORDTYPE_* constant
// Applies only to SIM_RECORDTYPE_CYCLIC and SIM_RECORDTYPE_LINEAR, otherwise ignored
function SimWriteRecord(hSim:HSIM; dwAddress:DWORD; dwRecordType:DWORD; dwIndex:DWORD;
 lpData: LPBYTE; dwByteCount: DWORD):HRESULT;external KernelDLL name 'SimWriteRecord';

// Gets information about a particular record.
// -
// Points to a valid HSIM handle
// SIM address
// Points to a SIM record information structure
function SimGetRecordInfo(hSim:HSIM; dwAddress:DWORD; lpSimRecordInfo:LPSIMRECORDINFO):HRESULT;external KernelDLL name 'SimGetRecordInfo';

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
