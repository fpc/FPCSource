{$INLINE ON}
{$MACRO ON}

{$define Rsc := }
(***********************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ConnectionMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Connection Manager Interface.  The Connection Manager allows
 * other applications to access, add, and delete connection profiles
 * contained in the Connection Panel.
 *
 * History:
 *    8/19/98     ADH      Initial Implementation
 *    03/01/2000  PPL      Rewrite API for New Connection Manager
 *    03/30/2000  PPL      Constant change from cncXXXX to kCncXXXX
 *                         and # defining old names (cncXXXX) for compatibility.
 *    10/19/00    PPL      Update the header with GuideLines
 *    10/20/00    PPL      Remove CncProfileBroacast - The notification it sent
 *                         is always in usage.
 *    10/23/00    PPL      Update Connection Manager API
 *    11/06/2000  PPL      Use the CncProfileId abstract type for profileIDs
 *
 ***********************************************************************)

unit connectionmgr;

interface

uses  palmos, coretraps, errorbase, datamgr, modemmgr;

(***********************************************************************
 * Definition
 ***********************************************************************)

type
  CncProfileID = UInt32;

(***********************************************************************
 * Connection Profile Broadcasting
 ***********************************************************************)

const
  kCncProfileNotifyCurrentVersion = 1;

type
  CncProfileNotifyDetailsTag = record
   // In:   version - This definition is version 1 (kCncProfileNotifyCurrentVersion)
   //       later versions should include all the fields of version 1 plus
   //       any additional fields of CncProfileNotifyDetailsType
   version: UInt16;

   // In: Broacasted Profile ID
   profileID: CncProfileID;

   // In:  Device Kind of the profile
   deviceKind: UInt16;

   // In: Resquested Action
   request: UInt16;
  end;
  CncProfileNotifyDetailsType = CncProfileNotifyDetailsTag;

(***********************************************************************
 * Constants
 ***********************************************************************)
const
  kCncProfileInvalidId = CncProfileID(0);

// Request's modifiers flags
const
  kCncNotifyBecomeCurrentModifier = $8000; // Change the Client current settings
  kCncNotifyAlertUserModifier     = $4000; // ask for Client UI
  kNotifyRequestMofifiersMask     = kCncNotifyBecomeCurrentModifier or kCncNotifyAlertUserModifier;

// Requests
  kCncNotifyCreateRequest     = 1; // the profile has been created
  kCncNotifyModifyRequest     = 2; // the profile has been modified
  kCncNotifyDeleteRequest     = 3; // the profile has been deleted
  kCncNotifyUpdateListRequest = 4; // the profile has been deleted

(***********************************************************************
 * Connection Profile ParamID Definition Macros and Constants
 ***********************************************************************)

const
  kCncParamOSRange           = $0000; // bit #15 set at 0
  kCncParamThirdPartiesRange = $8000; // bit #15 set at 1

  kCncParamFixedLength       = $0000; // bit #14 set to 0
  kCncParamVariableLength    = $4000; // bit #14 set to 1

  kCncParamIDMask            = $07FF; // bit #0 to #10 set to 1 (11 bits)
  kCncParamTypeMask          = $7800; // bit #11 to #14 set to 1 (4 bits)

// parameter type definition macros
  kCncParamFixedLen          = $00; // higth bit of 4 set to 0
  kCncParamVariableLen       = $08; // higth bit of 4 set to 1

//#define CncDefineParameterType( variableBit , typeOrder) ( ( (variableBit) | (typeOrder) ) << 11)

// bit number is comprised between 0 and 31

// #define CncDefineSystemFlagMask(bitnum) (1 << (bitnum))

const
  kCncParamSystemFlag = (kCncParamFixedLen or 0) shl 11; // 0x0000
  kCncParamUInt8      = (kCncParamFixedLen or 1) shl 11; // 0x0800
  kCncParamUInt16     = (kCncParamFixedLen or 2) shl 11; // 0x1000
  kCncParamUInt32     = (kCncParamFixedLen or 3) shl 11; // 0x1800
// [free slot from 4 to 7]

  kCncParamUInt8Size  = SizeOf(UInt8);
  kCncParamUInt16Size = SizeOf(UInt16);
  kCncParamUInt32Size = SizeOf(UInt32);

  kCncParamSystemFlagSize = kCncParamUInt8Size;

  kCncParamString = (kCncParamVariableLen or 1) shl 11;       // 0x4800
  kCncParamBuffer = (kCncParamVariableLen or 2) shl 11;       // 0x5000
// [free slot from 3 to 7]

// full Parameter ID  definition macro
(*
#define CncDefineParamID(parameterRange, parameterType, parameterID)    ( (parameterRange) | (parameterType)  | (parameterID) )

#define CncIsSystemRange(parameterID)                                   ( ( (parameterID) & kCncParamThirdPartiesRange)  != kCncParamThirdPartiesRange)
#define CncIsThirdPartiesRange(parameterID)                             ( ( (parameterID) & kCncParamThirdPartiesRange ) == kCncParamThirdPartiesRange)

#define CncIsFixedLengthParamType(parameterID)                          ( ( (parameterID) & kCncParamVariableLength)  != kCncParamVariableLength )
#define CncIsVariableLengthParamType(parameterID)                       ( ( (parameterID) & kCncParamVariableLength ) == kCncParamVariableLength)

#define CncGetTrueParamID(parameterID)                                  ( (parameterID) & kCncParamIDMask)
#define CncGetParamType(parameterID)                                    ( (parameterID) & kCncParamTypeMask)

#define CncIsSystemFlags(parameterID)                                   (  ! (CncGetParamType( (parameterID) ) ) )
#define CncGetSystemFlagBitnum(parameterID)                             CncGetTrueParamID(parameterID)
*)

// Some tests

(***********************************************************************
 * Cnc Manager Feature
 ***********************************************************************)

const
  kCncFtrCncMgrCreator = Rsc('cmgr');

  kCncFtrCncMgrVersion = 0;
  kCncMgrVersion       = $00040001;  // 4.0 =  4->high 0->low
// feature index 1 and 2 are reserved

(***********************************************************************
 * Parameter size values
 ***********************************************************************)

const
// 22 for compatibility
  kCncProfileNameSize   = 22;

// 81 defined in ModemMgr.h
  kCncProfileUsualInitStringSize = mdmCmdBufSize;

//    81  defined in ModemMgr.h
  kCncProfileClassicResetStringSize = mdmCmdSize; // Old size was 8
  kCncProfileUsualResetStringSize   = mdmCmdBufSize;

(***********************************************************************
 * Parameters values
 ***********************************************************************)

// device kinds
  kCncDeviceKindSerial       = 0;
  kCncDeviceKindModem        = 1;
  kCncDeviceKindPhone        = 2;
  kCncDeviceKindLocalNetwork = 3;

// Old flow controls
  kCncFlowControlAuto        = 0;
  kCncFlowControlOFF         = 1;
  kCncFlowControlON          = 2;

  kCncProfileVersion         = 4;

(***********************************************************************
 * Error Codes
 ***********************************************************************)

  kCncErrAddProfileFailed           = cncErrorClass or $01; // Add profile attempt failed
  kCncErrProfileListFull            = cncErrorClass or $02; // Add attempt failed because the
                                                            // profile list is full.
  kCncErrGetProfileFailed           = cncErrorClass or $03; // Get profile attempt failed
  kCncErrDBAccessFailed             = cncErrorClass or $04; // Connection database not found or access failed
  kCncErrGetProfileListFailed       = cncErrorClass or $05; // Could not get profile list
  kCncErrProfileReadOnly            = cncErrorClass or $06; // The profile can not be altered
  kCncErrProfileNotFound            = cncErrorClass or $07; // The profile could not be found

// New API error code
  kCncErrProfileParamNotFound       = cncErrorClass or $08; // The profile parameter could not be found
  kCncErrProfileParamReadOnly       = cncErrorClass or $09; // The profile parameter can only be read
  kCncErrProfileParamNameHasChange  = cncErrorClass or $0a; // The profile parameter Name has been modified to be unique
  kCncErrProfileGetParamFailed      = cncErrorClass or $0b; // failed to get a parameter in a profile
  kCncErrProfileSetParamFailed      = cncErrorClass or $0c; // failed to Set a parameter in a profile
  kCncErrProfileBadParamSize        = cncErrorClass or $0d; // failed to Set a parameter in a profile
  kCncErrProfileBadSystemFlagBitnum = cncErrorClass or $0e; // the bit num of a system flag is not comprise between 0 and 31

(***********************************************************************
 * Parameters ID  and Sizes
 ***********************************************************************)

const
// void param has a size of zero bytes
  kCncNoParam                         = 0;
  kCncNoParamSize                     = 0;

// 22 bytes limited  - for compatibility
  kCncParamName                       = kCncParamOSRange or kCncParamString or 1;
  kCncParamNameMaxSize                = kCncProfileNameSize;

  kCncParamPort                       = kCncParamOSRange or kCncParamUInt32 or 2;
  kCncParamPortSize                   = kCncParamUInt32Size;

  kCncParamBaud                       = kCncParamOSRange or kCncParamUInt32 or 3;
  kCncParamBaudSize                   = kCncParamUInt32Size;

  kCncParamVolume                     = kCncParamOSRange or kCncParamUInt16 or 4;
  kCncParamVolumeSize                 = kCncParamUInt16Size;

  kCncParamFlowControl                = kCncParamOSRange or kCncParamUInt16 or 5;
  kCncParamFlowControlSize            = kCncParamUInt16Size;

// New piece of info - communication time Out  (CTS)
  kCncParamTimeOut                    = kCncParamOSRange or kCncParamUInt32 or 6;
  kCncParamTimeOutSize                = kCncParamUInt32Size;

  kCncParamInitString                 = kCncParamOSRange or kCncParamString or 7;
  kCncParamInitStringMaxSize          = mdmCmdBufSize;

  kCncParamResetString                = kCncParamOSRange or kCncParamString or 8;
  kCncParamResetStringMaxSize         = mdmCmdBufSize;

// New piece of info -  extented device kind cf kCncDeviveXXX  after
  kCncParamDeviceKind                 = kCncParamOSRange or kCncParamUInt16 or 9;
  kCncParamDeviceKindSize             = kCncParamUInt16Size;

// country index for the profile
  kCncParamCountryIndex               = kCncParamOSRange or kCncParamUInt16 or 11;
  kCncParamCountryIndexSize           = kCncParamUInt16Size;

// dialing mode, old pulse param
  kCncParamDialingMode                = kCncParamOSRange or kCncParamUInt8 or 12;
  kCncParamDialingModeSize            = kCncParamUInt8Size;

  kCncParamVersion                    = kCncParamOSRange or kCncParamUInt8 or 13;
  kCncParamVersionSize                = kCncParamUInt8Size;

  kCncParamReceiveTimeOut             = kCncParamOSRange  or kCncParamUInt32 or 14;
  kCncParamReceiveTimeOutSize         = kCncParamUInt32Size;

// International Reset string (count [strings])
  kCncParamIntlModemResetStringList   = kCncParamOSRange or kCncParamBuffer or 15;


// International country string (count [strings])
  kCncParamIntlModemCountryStringList = kCncParamOSRange or kCncParamBuffer or 16;

// special parameters : system flags
// the meaning of these parameters is for the connection panel
// up to 32 flags system flag will be possible

// bit numbering
  kCncParamReadOnlyBit    = 0;
  kCncParamInvisibleBit   = 1;
  kCncParamNonEditableBit = 2;
  kCncParamNoDetailsBit   = 3;
  kCncParamLockedBit      = 4;
  kCncParamReservedBit5   = 5;
  kCncParamReservedBit6   = 6;
  kCncParamReservedBit7   = 7;
  kCncParamReservedBit8   = 8;
  kCncParamReservedBit9   = 9;
  kCncParamReservedBit10  = 10;
  kCncParamReservedBit11  = 11;
  kCncParamReservedBit12  = 12;
  kCncParamReservedBit13  = 13;
  kCncParamReservedBit14  = 14;
  kCncParamReservedBit15  = 15;
  kCncParamSystemBit16    = 16;
  kCncParamSystemBit17    = 17;
  kCncParamReservedBit18  = 18;
  kCncParamReservedBit19  = 19;
  kCncParamReservedBit20  = 20;
  kCncParamReservedBit21  = 21;
  kCncParamReservedBit22  = 22;
  kCncParamReservedBit23  = 23;
  kCncParamReservedBit24  = 24;
  kCncParamReservedBit25  = 25;
  kCncParamReservedBit26  = 26;
  kCncParamReservedBit27  = 27;
  kCncParamReservedBit28  = 28;
  kCncParamReservedBit29  = 29;
  kCncParamReservedBit30  = 30;
  kCncParamReservedBit31  = 31;

  kCncParamSystemFlagsNum = $07FF;

// the following parameter handles  the system flags as an UInt32 integer (all the flags, at once)
  kCncParamSystemFlags                = kCncParamOSRange or kCncParamSystemFlag or kCncParamSystemFlagsNum;
  kCncParamSystemFlagsSize            = kCncParamUInt32Size;

// bit parameters definition : to handle flags bit per bit
  kCncParamReadOnly                   = kCncParamOSRange or kCncParamSystemFlag or 0;

  kCncParamReadOnlySize               = kCncParamSystemFlagSize;

  kCncParamInvisible                  = kCncParamOSRange or kCncParamSystemFlag or 1;
  kCncParamInvisibleSize              = kCncParamSystemFlagSize;

  kCncParamNonEditable                = kCncParamOSRange or kCncParamSystemFlag or 2;
  kCncParamNonEditableSize            = kCncParamSystemFlagSize;

  kCncParamNoDetails                  = kCncParamOSRange or kCncParamSystemFlag or 3;
  kCncParamNoDetailsSize              = kCncParamSystemFlagSize;

  kCncParamLocked                     = kCncParamOSRange or kCncParamSystemFlag or 4;
  kCncParamLockedSize                 = kCncParamSystemFlagSize;

(* Bluetooth parameter IDs - New pieces of info *)

// 48 bit blue Tooth address (BD_ADDR) - This address is derived from the IEEE802 standard

  kCncParamBluetoothDeviceAddr        = kCncParamOSRange or kCncParamBuffer or 50;
  kCncParamBluetoothDeviceAddrSize    = 8;

// Bluetooth device name - 248 bytes coded according to the UTF-8 standard at max + NULL terninaison
  kCncParamBluetoothDeviceName        = kCncParamOSRange or kCncParamString or 51;
  kCncParamBluetoothDeviceNameMaxSize = 249;

// Caution :  system parameter range ID from 80 to 200 are reserved for telephony services
// and should never be reused by any other component

(***********************************************************************
 * Telephony Manager parameter
 ***********************************************************************)

(* TT-AT specific parameters *)

// New piece gathering several parts (uses the serial manager flags cf SerialMgr.h )
  kCncParamSerialPortFlags     = kCncParamOSRange or kCncParamUInt32 or 84;
  kCncParamSerialPortFlagsSize = kCncParamUInt32Size;

// Telephony Task type  - mobile telephony
  kCncParamTTType              = kCncParamOSRange or kCncParamUInt32 or 90;
  kCncParamTTTypeSize          = kCncParamUInt32Size;

// Telephony Task Creator  - mobile telephony
  kCncParamTTCreator           = kCncParamOSRange or kCncParamUInt32 or 91;
  kCncParamTTCreatorSize       = kCncParamUInt32Size;

// Phone Driver Name - mobile telephony
  kCncParam_PSDName            = kCncParamOSRange or kCncParamString or 92;
  kCncParam_PSDNameSize        = dmDBNameLength;

// Phone Driver creator - mobile telephony
  kCncParam_PSDCreator         = kCncParamOSRange or kCncParamUInt32 or 93;
  kCncParam_PSDCreatorSize     = kCncParamUInt32Size;

// Phone Driver type - mobile telephony
  kCncParam_PSDType            = kCncParamOSRange or kCncParamUInt32 or 94;
  kCncParam_PSDTypeSize        = kCncParamUInt32Size;

// Phone Driver Param Buffer - mobile telephony
  kCncParam_PSDParameterBuffer = kCncParamOSRange or kCncParamBuffer or 100;

(***********************************************************************
 * New Connection Manager trap selectors
 ***********************************************************************)

  sysTrapCncMgrProfileSettingGet     = 1;
  sysTrapCncMgrProfileSettingSet     = 2;
  sysTrapCncMgrProfileGetCurrent     = 3;
  sysTrapCncMgrProfileSetCurrent     = 4;
  sysTrapCncMgrProfileGetIDFromName  = 5;
  sysTrapCncMgrProfileCreate         = 6;
  sysTrapCncMgrProfileDelete         = 7;
  sysTrapCncMgrProfileGetIDFromIndex = 8;
  sysTrapCncMgrProfileGetIndex       = 9;
  sysTrapCncMgrProfileCount          = 10;
  sysTrapCncMgrProfileOpenDB         = 11;
  sysTrapCncMgrProfileCloseDB        = 12;

(***********************************************************************
 * Connection Manager  Library Macros
 ***********************************************************************)

(***********************************************************************
 * New  Connection Mgr API
 ***********************************************************************)

function CncProfileSettingGet(profileId: CncProfileID; paramId: UInt16; paramBufferP: Pointer; var ioParamSizeP: UInt16): Err;

function CncProfileSettingSet(iProfileId: CncProfileID; paramId: UInt16; const paramBufferP: Pointer; paramSize: UInt16): Err;

function CncProfileSetCurrent(profileId: CncProfileID): Err;

function CncProfileGetCurrent(var profileIdP: CncProfileID): Err;

function CncProfileGetIDFromName(const profileNameP: PChar; var profileIdP: CncProfileID): Err;

function CncProfileCreate(var profileIdP: CncProfileID): Err;

function CncProfileDelete(profileId: CncProfileID): Err;

function CncProfileGetIDFromIndex(index: UInt16; var profileIdP: CncProfileID): Err;

function CncProfileGetIndex(profileId: CncProfileID; var indexP: UInt16): Err;

function CncProfileCount(var profilesCountP: UInt16): Err;

function CncProfileOpenDB: Err;

function CncProfileCloseDB: Err;

(***********************************************************************
 * Old  Connection Mgr API, For compatibility only
 ***********************************************************************)

const
// Maximum size for a Connection Profile Name
  cncProfileNameSize = 22;

// Error Codes
  cncErrAddProfileFailed      = cncErrorClass or 1; // Add profile attempt failed
  cncErrProfileListFull       = cncErrorClass or 2; // Add attempt failed because the
                                                    // profile list is full.
  cncErrGetProfileFailed      = cncErrorClass or 3; // Get profile attempt failed
  cncErrConDBNotFound         = cncErrorClass or 4; // Connection database not found
  cncErrGetProfileListFailed  = cncErrorClass or 5; // Could not get profile list
  cncErrProfileReadOnly       = cncErrorClass or 6; // The profile can not be altered
  cncErrProfileNotFound       = cncErrorClass or 7; // The profile could not be found

// Functions

function CncGetProfileList(var nameListPPP: PCharPtr; var countP: UInt16): Err; syscall sysTrapCncGetProfileList;

function CncGetProfileInfo(name: PChar; var port, baud: UInt32; var volume, handShake: UInt16;
                           initString: PChar; var resetString: Char; var isModem, isPulse: Boolean): Err; syscall sysTrapCncGetProfileInfo;

function CncAddProfile(name: PChar; port, baud: UInt32; volum, handShake: UInt16;
                       const initString, resetString: PChar; isMode, isPulse: Boolean): Err; syscall sysTrapCncAddProfile;

function CncDeleteProfile(const name: PChar): Err; syscall sysTrapCncDeleteProfile;

implementation

function __CncProfileSettingGet(profileId: CncProfileID; paramId: UInt16; paramBufferP: Pointer; var ioParamSizeP: UInt16): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileSettingSet(iProfileId: CncProfileID; paramId: UInt16; const paramBufferP: Pointer; paramSize: UInt16): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileSetCurrent(profileId: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileGetCurrent(var profileIdP: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileGetIDFromName(const profileNameP: PChar; var profileIdP: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileCreate(var profileIdP: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileDelete(profileId: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileGetIDFromIndex(index: UInt16; var profileIdP: CncProfileID): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileGetIndex(profileId: CncProfileID; var indexP: UInt16): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileCount(var profilesCountP: UInt16): Err; syscall sysTrapCncMgrDispatch;
function __CncProfileOpenDB: Err; syscall sysTrapCncMgrDispatch;
function __CncProfileCloseDB: Err; syscall sysTrapCncMgrDispatch;

function CncProfileSettingGet(profileId: CncProfileID; paramId: UInt16; paramBufferP: Pointer; var ioParamSizeP: UInt16): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileSettingGet, D2
 end;
 CncProfileSettingGet := __CncProfileSettingGet(profileId, paramId, paramBufferP, ioParamSizeP);
end;

function CncProfileSettingSet(iProfileId: CncProfileID; paramId: UInt16; const paramBufferP: Pointer; paramSize: UInt16): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileSettingSet, D2
 end;
 CncProfileSettingSet := __CncProfileSettingSet(iProfileId, paramId, paramBufferP, paramSize);
end;

function CncProfileSetCurrent(profileId: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileSetCurrent, D2
 end;
 CncProfileSetCurrent := __CncProfileSetCurrent(profileId);
end;

function CncProfileGetCurrent(var profileIdP: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileGetCurrent, D2;
 end;
 CncProfileGetCurrent := __CncProfileGetCurrent(profileIdP);
end;

function CncProfileGetIDFromName(const profileNameP: PChar; var profileIdP: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileGetIDFromName, D2;
 end;
 CncProfileGetIDFromName := __CncProfileGetIDFromName(profileNameP, profileIdP);
end;

function CncProfileCreate(var profileIdP: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileCreate, D2;
 end;
 CncProfileCreate := __CncProfileCreate(profileIdP);
end;

function CncProfileDelete(profileId: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileDelete, D2;
 end;
 CncProfileDelete := __CncProfileDelete(profileId);
end;

function CncProfileGetIDFromIndex(index: UInt16; var profileIdP: CncProfileID): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileGetIDFromIndex, D2;
 end;
 CncProfileGetIDFromIndex := __CncProfileGetIDFromIndex(index, profileIdP);
end;

function CncProfileGetIndex(profileId: CncProfileID; var indexP: UInt16): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileGetIndex, D2;
 end;
 CncProfileGetIndex := __CncProfileGetIndex(profileID, indexP);
end;

function CncProfileCount(var profilesCountP: UInt16): Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileCount, D2
 end;
 CncProfileCount := __CncProfileCount(profilesCountP);
end;

function CncProfileOpenDB: Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileOpenDB, D2;
 end;
 CncProfileOpenDB := __CncProfileOpenDB;
end;

function CncProfileCloseDB: Err;
begin
 asm
  move.l #$sysTrapCncMgrProfileCloseDB, D2;
 end;
 CnCProfileCloseDB := __CncProfileCloseDB;
end;

end.
