{******************************************************************************}
{                                                                              }
{ Windows API interface Unit for Object Pascal                                 }
{ Master file for Windows Vista applications                                   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaVista.pas, released Octobre 2007.            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 2007                  }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{ This unit contains declarations that are new in Windows Vista.               }
{ This unit can be used in programs to be run under older versions of windows. }
{ However you should not use these types in a semantic way.                    }
{ This unit is not part of JwaWindows.pas and MUST be included by uses clause. }
{ To use this unit you must compile JwaWindows.pas with include mode.}

unit JwaVista;

interface

uses JwaWindows;


type
  WELL_KNOWN_SID_TYPE = (
  WinNullSid {= 0},
  WinWorldSid {= 1},
  WinLocalSid {= 2},
  WinCreatorOwnerSid {= 3},
  WinCreatorGroupSid {= 4},
  WinCreatorOwnerServerSid {= 5},
  WinCreatorGroupServerSid {= 6},
  WinNtAuthoritySid {= 7},
  WinDialupSid {= 8},
  WinNetworkSid {= 9},
  WinBatchSid {= 10},
  WinInteractiveSid {= 11},
  WinServiceSid {= 12},
  WinAnonymousSid {= 13},
  WinProxySid {= 14},
  WinEnterpriseControllersSid {= 15},
  WinSelfSid {= 16},
  WinAuthenticatedUserSid {= 17},
  WinRestrictedCodeSid {= 18},
  WinTerminalServerSid {= 19},
  WinRemoteLogonIdSid {= 20},
  WinLogonIdsSid {= 21},
  WinLocalSystemSid {= 22},
  WinLocalServiceSid {= 23},
  WinNetworkServiceSid {= 24},
  WinBuiltinDomainSid {= 25},
  WinBuiltinAdministratorsSid {= 26},
  WinBuiltinUsersSid {= 27},
  WinBuiltinGuestsSid {= 28},
  WinBuiltinPowerUsersSid {= 29},
  WinBuiltinAccountOperatorsSid {= 30},
  WinBuiltinSystemOperatorsSid {= 31},
  WinBuiltinPrintOperatorsSid {= 32},
  WinBuiltinBackupOperatorsSid {= 33},
  WinBuiltinReplicatorSid {= 34},
  WinBuiltinPreWindows2000CompatibleAccessSid {= 35},
  WinBuiltinRemoteDesktopUsersSid {= 36},
  WinBuiltinNetworkConfigurationOperatorsSid {= 37},
  WinAccountAdministratorSid {= 38},
  WinAccountGuestSid {= 39},
  WinAccountKrbtgtSid {= 40},
  WinAccountDomainAdminsSid {= 41},
  WinAccountDomainUsersSid {= 42},
  WinAccountDomainGuestsSid {= 43},
  WinAccountComputersSid {= 44},
  WinAccountControllersSid {= 45},
  WinAccountCertAdminsSid {= 46},
  WinAccountSchemaAdminsSid {= 47},
  WinAccountEnterpriseAdminsSid {= 48},
  WinAccountPolicyAdminsSid {= 49},
  WinAccountRasAndIasServersSid {= 50},
  WinNTLMAuthenticationSid {= 51},
  WinDigestAuthenticationSid {= 52},
  WinSChannelAuthenticationSid {= 53},
  WinThisOrganizationSid {= 54},
  WinOtherOrganizationSid {= 55},
  WinBuiltinIncomingForestTrustBuildersSid {= 56},
  WinBuiltinPerfMonitoringUsersSid {= 57},
  WinBuiltinPerfLoggingUsersSid {= 58},
  WinBuiltinAuthorizationAccessSid {= 59},
  WinBuiltinTerminalServerLicenseServersSid {= 60},
  WinBuiltinDCOMUsersSid {= 61},
  WinBuiltinIUsersSid {= 62},
  WinIUserSid {= 63},
  WinBuiltinCryptoOperatorsSid {= 64},
  WinUntrustedLabelSid {= 65},
  WinLowLabelSid {= 66},
  WinMediumLabelSid {= 67},
  WinHighLabelSid {= 68},
  WinSystemLabelSid {= 69},
  WinWriteRestrictedCodeSid {= 70},
  WinCreatorOwnerRightsSid {= 71},
  WinCacheablePrincipalsGroupSid {= 72},
  WinNonCacheablePrincipalsGroupSid {= 73},
  WinEnterpriseReadonlyControllersSid {= 74},
  WinAccountReadonlyControllersSid {= 75},
  WinBuiltinEventLogReadersGroup {= 76},
  WinNewEnterpriseReadonlyControllersSid {= 77},
  WinBuiltinCertSvcDComAccessGroup {= 78}
  );

  TWellKnownSidType = WELL_KNOWN_SID_TYPE;


    {Token elevation type.
     TokenElevationTypePad0 does not belong to this type. It is only a pad.

     Vista only
    }
  _TOKEN_ELEVATION_TYPE = (TokenElevationTypePad0,
    TokenElevationTypeDefault, TokenElevationTypeFull,
    TokenElevationTypeLimited);

  {see @link(_TOKEN_ELEVATION_TYPE)}
  TTokenElevationType = _TOKEN_ELEVATION_TYPE;
  {see @Link(_TOKEN_ELEVATION_TYPE)}
  PTokenElevationType = ^TTokenElevationType;

  {@Name contains the elevation status of a token on a vista system.}
  _TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;

  {see @Link(_TOKEN_ELEVATION)}
  TTokenElevation = _TOKEN_ELEVATION;
  {see @Link(_TOKEN_ELEVATION)}
  PTokenElevation = ^TTokenElevation;

    {@Name is the new token information class of a vista system.
     The enum constant TokenPadding0 is only for padding and is not used.
     }


  _TOKEN_INFORMATION_CLASS = (TokenPadding0,
    TokenUser,
    TokenGroups,
    TokenPrivileges,
    TokenOwner,
    TokenPrimaryGroup,
    TokenDefaultDacl,
    TokenSource,
    TokenType,
    TokenImpersonationLevel,
    TokenStatistics,
    TokenRestrictedSids,
    TokenSessionId,
    TokenGroupsAndPrivileges,
    TokenSessionReference,
    TokenSandBoxInert,
    TokenAuditPolicy,
    TokenOrigin,
    TokenElevationType,
    TokenLinkedToken,
    TokenElevation,
    TokenHasRestrictions,
    TokenAccessInformation,
    TokenVirtualizationAllowed,
    TokenVirtualizationEnabled,
    TokenIntegrityLevel,
    TokenUIAccess,
    TokenMandatoryPolicy,
    TokenLogonSid,
    // MaxTokenInfoClass should always be the last enum
    MaxTokenInfoClass
    );


  {see @Link(_TOKEN_INFORMATION_CLASS)}
  TTokenInformationClass   = _TOKEN_INFORMATION_CLASS;
  {see @Link(_TOKEN_INFORMATION_CLASS)}
  PTokenInformationClass   = ^TTokenInformationClass;

type
  MANDATORY_LEVEL = (
    MandatoryLevelUntrusted, {= 0}
    MandatoryLevelLow,
    MandatoryLevelMedium,
    MandatoryLevelHigh,
    MandatoryLevelSystem,
    MandatoryLevelSecureProcess,
    MandatoryLevelCount);

  PMANDATORY_LEVEL = ^MANDATORY_LEVEL;

  TMandatoryLevel = MANDATORY_LEVEL;
  PMandatoryLevel = ^TMandatoryLevel;

const
  SYSTEM_MANDATORY_LABEL_NO_WRITE_UP = $1; //A principal with a lower mandatory level than the object cannot write to the object.
  SYSTEM_MANDATORY_LABEL_NO_READ_UP = $2; //A principal with a lower mandatory level than the object cannot read the object.
  SYSTEM_MANDATORY_LABEL_NO_EXECUTE_UP = $4; //A principal with a lower mandatory level than the object cannot execute the object.

type
  //http://msdn2.microsoft.com/en-us/library/aa965848.aspx
  _SYSTEM_MANDATORY_LABEL_ACE = record
    Header : ACE_HEADER;
    Mask : ACCESS_MASK;
    SidStart : DWORD;
  end;
  SYSTEM_MANDATORY_LABEL_ACE = _SYSTEM_MANDATORY_LABEL_ACE;
  PSYSTEM_MANDATORY_LABEL_ACE = ^_SYSTEM_MANDATORY_LABEL_ACE;

  TSystemMandatoryLabelAce = _SYSTEM_MANDATORY_LABEL_ACE;
  PSystemMandatoryLabelAce = ^TSystemMandatoryLabelAce;

const
  TOKEN_MANDATORY_POLICY_OFF = $0; //No mandatory integrity policy is enforced for the token.
  TOKEN_MANDATORY_POLICY_NO_WRITE_UP = $1; //A process associated with the token cannot write to objects that have a greater mandatory integrity level.
  TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN = $2; //A process created with the token has an integrity level that is the lesser of the parent-process integrity level and the executable-file integrity level.
  TOKEN_MANDATORY_POLICY_VALID_MASK = $3; //A combination of TOKEN_MANDATORY_POLICY_NO_WRITE_UP and TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN

type
  //http://msdn2.microsoft.com/en-us/library/bb394728.aspx
  _TOKEN_MANDATORY_POLICY = record
    Policy : DWORD;
  end;

  TOKEN_MANDATORY_POLICY = _TOKEN_MANDATORY_POLICY;
  PTOKEN_MANDATORY_POLICY = ^TOKEN_MANDATORY_POLICY;

  TTokenMandatoryPolicy = _TOKEN_MANDATORY_POLICY;
  PTokenMandatoryPolicy = ^TTokenMandatoryPolicy;

const
  {
  The SID_HASH_SIZE array dimension is defined in WinNT.h as 32.
  http://msdn2.microsoft.com/en-us/library/bb394725.aspx
  }
  SID_HASH_SIZE = 32;

type
  SID_HASH_ENTRY = ULONG_PTR;
  TSidHashEntry = SID_HASH_ENTRY;

  //http://msdn2.microsoft.com/en-us/library/bb394725.aspx
  _SID_AND_ATTRIBUTES_HASH = record
    SidCount : DWORD;
    SidAttr : PSID_AND_ATTRIBUTES;
    Hash : array[0..SID_HASH_SIZE-1] of SID_HASH_ENTRY;
  end;

  SID_AND_ATTRIBUTES_HASH = _SID_AND_ATTRIBUTES_HASH;
  PSID_AND_ATTRIBUTES_HASH = ^SID_AND_ATTRIBUTES_HASH;

  TSidAndAttributesHash = _SID_AND_ATTRIBUTES_HASH;
  PSidAndAttributesHash = ^TSidAndAttributesHash;


  //http://msdn2.microsoft.com/en-us/library/bb394726.aspx
  _TOKEN_ACCESS_INFORMATION = record
    SidHash: PSID_AND_ATTRIBUTES_HASH;
    RestrictedSidHash : PSID_AND_ATTRIBUTES_HASH;
    Privileges : PTOKEN_PRIVILEGES;
    AuthenticationId : LUID;
    TokenType : TOKEN_TYPE;
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
    MandatoryPolicy : TOKEN_MANDATORY_POLICY;
    Flags : DWORD;
  end;

  TOKEN_ACCESS_INFORMATION = _TOKEN_ACCESS_INFORMATION;
  PTOKEN_ACCESS_INFORMATION = ^TOKEN_ACCESS_INFORMATION;

  TTokenAccessInformation = _TOKEN_ACCESS_INFORMATION;
  PTokenAccessInformation = ^TTokenAccessInformation;

  //http://msdn2.microsoft.com/en-us/library/bb530719.aspx
  _TOKEN_LINKED_TOKEN = record
    LinkedToken : HANDLE;
  end;
  TOKEN_LINKED_TOKEN = _TOKEN_LINKED_TOKEN;
  PTOKEN_LINKED_TOKEN = ^TOKEN_LINKED_TOKEN;

  TTokenLinkedToken = _TOKEN_LINKED_TOKEN;
  PTokenLinkedToken = ^TTokenLinkedToken;


  _TOKEN_MANDATORY_LABEL = record
    Label_ : SID_AND_ATTRIBUTES;
  end;

  TOKEN_MANDATORY_LABEL = _TOKEN_MANDATORY_LABEL;
  PTOKEN_MANDATORY_LABEL = ^TOKEN_MANDATORY_LABEL;

  TTokenMandatoryLabel = _TOKEN_MANDATORY_LABEL;
  PTokenMandatoryLabel = ^TTokenMandatoryLabel;

const
  SYSTEM_MANDATORY_LABEL_ACE_TYPE = $11;

  SE_RELABEL_NAME = 'SeRelabelPrivilege'; //Required to modify the mandatory integrity level of an object.


  SE_GROUP_INTEGRITY                 = $00000020;
  SE_GROUP_INTEGRITY_ENABLED         = $00000040;

  {@Name includes the integrity ACE into the security descriptor string.
   See http://msdn2.microsoft.com/en-us/library/aa376397.aspx
   }
  LABEL_SECURITY_INFORMATION = $0000010;


  SECURITY_MANDATORY_UNTRUSTED_RID = $0;
  SECURITY_MANDATORY_LOW_RID = $1000;
  SECURITY_MANDATORY_MEDIUM_RID = $2000;
  SECURITY_MANDATORY_HIGH_RID = $3000;          
  SECURITY_MANDATORY_SYSTEM_RID = $4000;
  SECURITY_MANDATORY_PROTECTED_PROCESS_RID = $5000;

  LowIL = 'S-1-16-4096';
  MediumIL = 'S-1-16-8192';
  HighIL = 'S-1-16-12288';
  SystemIL = 'S-1-16-16384';
  ProtectedProcessIL = 'S-1-16-20480';


  function AddMandatoryAce(pAcl: PACL; dwAceRevision, AceFlags, MandatoryPolicy: DWORD;
    pLabelSid: PSID): BOOL; stdcall;
  {$EXTERNALSYM AddMandatoryAce}



implementation
uses JwaWinDLLNames;

{$IFNDEF DYNAMIC_LINK}
  function AddMandatoryAce(pAcl: PACL; dwAceRevision, AceFlags, MandatoryPolicy: DWORD;
    pLabelSid: PSID): BOOL; stdcall; external advapi32 name 'AddMandatoryAce';
{$ELSE}

var
  _AddMandatoryAce: Pointer;

function AddMandatoryAce;
begin
  GetProcedureAddress(_AddMandatoryAce, advapi32, 'AddMandatoryAce');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddMandatoryAce]
  end;
end;

{$ENDIF DYNAMIC_LINK}

end.
