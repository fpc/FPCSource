unit googleandroidenterprise;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 9-5-15 13:22:48
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAppRestrictionsSchema = class;
  TAppRestrictionsSchemaRestriction = class;
  TAppRestrictionsSchemaRestrictionRestrictionValue = class;
  TCollection = class;
  TCollectionViewersListResponse = class;
  TCollectionsListResponse = class;
  TDevice = class;
  TDeviceState = class;
  TDevicesListResponse = class;
  TEnterprise = class;
  TEnterpriseAccount = class;
  TEnterprisesListResponse = class;
  TEntitlement = class;
  TEntitlementsListResponse = class;
  TGroupLicense = class;
  TGroupLicenseUsersListResponse = class;
  TGroupLicensesListResponse = class;
  TInstall = class;
  TInstallsListResponse = class;
  TPermission = class;
  TProduct = class;
  TProductPermission = class;
  TProductPermissions = class;
  TUser = class;
  TUserToken = class;
  TUsersListResponse = class;
  TAppRestrictionsSchemaArray = Array of TAppRestrictionsSchema;
  TAppRestrictionsSchemaRestrictionArray = Array of TAppRestrictionsSchemaRestriction;
  TAppRestrictionsSchemaRestrictionRestrictionValueArray = Array of TAppRestrictionsSchemaRestrictionRestrictionValue;
  TCollectionArray = Array of TCollection;
  TCollectionViewersListResponseArray = Array of TCollectionViewersListResponse;
  TCollectionsListResponseArray = Array of TCollectionsListResponse;
  TDeviceArray = Array of TDevice;
  TDeviceStateArray = Array of TDeviceState;
  TDevicesListResponseArray = Array of TDevicesListResponse;
  TEnterpriseArray = Array of TEnterprise;
  TEnterpriseAccountArray = Array of TEnterpriseAccount;
  TEnterprisesListResponseArray = Array of TEnterprisesListResponse;
  TEntitlementArray = Array of TEntitlement;
  TEntitlementsListResponseArray = Array of TEntitlementsListResponse;
  TGroupLicenseArray = Array of TGroupLicense;
  TGroupLicenseUsersListResponseArray = Array of TGroupLicenseUsersListResponse;
  TGroupLicensesListResponseArray = Array of TGroupLicensesListResponse;
  TInstallArray = Array of TInstall;
  TInstallsListResponseArray = Array of TInstallsListResponse;
  TPermissionArray = Array of TPermission;
  TProductArray = Array of TProduct;
  TProductPermissionArray = Array of TProductPermission;
  TProductPermissionsArray = Array of TProductPermissions;
  TUserArray = Array of TUser;
  TUserTokenArray = Array of TUserToken;
  TUsersListResponseArray = Array of TUsersListResponse;
  //Anonymous types, using auto-generated names
  TAppRestrictionsSchemaTyperestrictionsArray = Array of TAppRestrictionsSchemaRestriction;
  TCollectionViewersListResponseTypeuserArray = Array of TUser;
  TCollectionsListResponseTypecollectionArray = Array of TCollection;
  TDevicesListResponseTypedeviceArray = Array of TDevice;
  TEnterprisesListResponseTypeenterpriseArray = Array of TEnterprise;
  TEntitlementsListResponseTypeentitlementArray = Array of TEntitlement;
  TGroupLicenseUsersListResponseTypeuserArray = Array of TUser;
  TGroupLicensesListResponseTypegroupLicenseArray = Array of TGroupLicense;
  TInstallsListResponseTypeinstallArray = Array of TInstall;
  TProductPermissionsTypepermissionArray = Array of TProductPermission;
  TUsersListResponseTypeuserArray = Array of TUser;
  
  { --------------------------------------------------------------------
    TAppRestrictionsSchema
    --------------------------------------------------------------------}
  
  TAppRestrictionsSchema = Class(TGoogleBaseObject)
  Private
    Frestrictions : TAppRestrictionsSchemaTyperestrictionsArray;
  Protected
    //Property setters
    Procedure Setrestrictions(AIndex : Integer; AValue : TAppRestrictionsSchemaTyperestrictionsArray); virtual;
  Public
  Published
    Property restrictions : TAppRestrictionsSchemaTyperestrictionsArray Index 0 Read Frestrictions Write Setrestrictions;
  end;
  TAppRestrictionsSchemaClass = Class of TAppRestrictionsSchema;
  
  { --------------------------------------------------------------------
    TAppRestrictionsSchemaRestriction
    --------------------------------------------------------------------}
  
  TAppRestrictionsSchemaRestriction = Class(TGoogleBaseObject)
  Private
    FdefaultValue : TAppRestrictionsSchemaRestrictionRestrictionValue;
    Fdescription : String;
    Fentry : TStringArray;
    FentryValue : TStringArray;
    Fkey : String;
    FrestrictionType : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetdefaultValue(AIndex : Integer; AValue : TAppRestrictionsSchemaRestrictionRestrictionValue); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setentry(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetentryValue(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure SetrestrictionType(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property defaultValue : TAppRestrictionsSchemaRestrictionRestrictionValue Index 0 Read FdefaultValue Write SetdefaultValue;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property entry : TStringArray Index 16 Read Fentry Write Setentry;
    Property entryValue : TStringArray Index 24 Read FentryValue Write SetentryValue;
    Property key : String Index 32 Read Fkey Write Setkey;
    Property restrictionType : String Index 40 Read FrestrictionType Write SetrestrictionType;
    Property title : String Index 48 Read Ftitle Write Settitle;
  end;
  TAppRestrictionsSchemaRestrictionClass = Class of TAppRestrictionsSchemaRestriction;
  
  { --------------------------------------------------------------------
    TAppRestrictionsSchemaRestrictionRestrictionValue
    --------------------------------------------------------------------}
  
  TAppRestrictionsSchemaRestrictionRestrictionValue = Class(TGoogleBaseObject)
  Private
    F_type : String;
    FvalueBool : boolean;
    FvalueInteger : integer;
    FvalueMultiselect : TStringArray;
    FvalueString : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetvalueBool(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetvalueInteger(AIndex : Integer; AValue : integer); virtual;
    Procedure SetvalueMultiselect(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetvalueString(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property valueBool : boolean Index 8 Read FvalueBool Write SetvalueBool;
    Property valueInteger : integer Index 16 Read FvalueInteger Write SetvalueInteger;
    Property valueMultiselect : TStringArray Index 24 Read FvalueMultiselect Write SetvalueMultiselect;
    Property valueString : String Index 32 Read FvalueString Write SetvalueString;
  end;
  TAppRestrictionsSchemaRestrictionRestrictionValueClass = Class of TAppRestrictionsSchemaRestrictionRestrictionValue;
  
  { --------------------------------------------------------------------
    TCollection
    --------------------------------------------------------------------}
  
  TCollection = Class(TGoogleBaseObject)
  Private
    FcollectionId : String;
    Fkind : String;
    Fname : String;
    FproductId : TStringArray;
    Fvisibility : String;
  Protected
    //Property setters
    Procedure SetcollectionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property collectionId : String Index 0 Read FcollectionId Write SetcollectionId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property productId : TStringArray Index 24 Read FproductId Write SetproductId;
    Property visibility : String Index 32 Read Fvisibility Write Setvisibility;
  end;
  TCollectionClass = Class of TCollection;
  
  { --------------------------------------------------------------------
    TCollectionViewersListResponse
    --------------------------------------------------------------------}
  
  TCollectionViewersListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fuser : TCollectionViewersListResponseTypeuserArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TCollectionViewersListResponseTypeuserArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property user : TCollectionViewersListResponseTypeuserArray Index 8 Read Fuser Write Setuser;
  end;
  TCollectionViewersListResponseClass = Class of TCollectionViewersListResponse;
  
  { --------------------------------------------------------------------
    TCollectionsListResponse
    --------------------------------------------------------------------}
  
  TCollectionsListResponse = Class(TGoogleBaseObject)
  Private
    Fcollection : TCollectionsListResponseTypecollectionArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcollection(AIndex : Integer; AValue : TCollectionsListResponseTypecollectionArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property collection : TCollectionsListResponseTypecollectionArray Index 0 Read Fcollection Write Setcollection;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TCollectionsListResponseClass = Class of TCollectionsListResponse;
  
  { --------------------------------------------------------------------
    TDevice
    --------------------------------------------------------------------}
  
  TDevice = Class(TGoogleBaseObject)
  Private
    FandroidId : String;
    Fkind : String;
    FmanagementType : String;
  Protected
    //Property setters
    Procedure SetandroidId(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmanagementType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property androidId : String Index 0 Read FandroidId Write SetandroidId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property managementType : String Index 16 Read FmanagementType Write SetmanagementType;
  end;
  TDeviceClass = Class of TDevice;
  
  { --------------------------------------------------------------------
    TDeviceState
    --------------------------------------------------------------------}
  
  TDeviceState = Class(TGoogleBaseObject)
  Private
    FaccountState : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountState(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountState : String Index 0 Read FaccountState Write SetaccountState;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDeviceStateClass = Class of TDeviceState;
  
  { --------------------------------------------------------------------
    TDevicesListResponse
    --------------------------------------------------------------------}
  
  TDevicesListResponse = Class(TGoogleBaseObject)
  Private
    Fdevice : TDevicesListResponseTypedeviceArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setdevice(AIndex : Integer; AValue : TDevicesListResponseTypedeviceArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property device : TDevicesListResponseTypedeviceArray Index 0 Read Fdevice Write Setdevice;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TDevicesListResponseClass = Class of TDevicesListResponse;
  
  { --------------------------------------------------------------------
    TEnterprise
    --------------------------------------------------------------------}
  
  TEnterprise = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
    FprimaryDomain : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprimaryDomain(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property primaryDomain : String Index 24 Read FprimaryDomain Write SetprimaryDomain;
  end;
  TEnterpriseClass = Class of TEnterprise;
  
  { --------------------------------------------------------------------
    TEnterpriseAccount
    --------------------------------------------------------------------}
  
  TEnterpriseAccount = Class(TGoogleBaseObject)
  Private
    FaccountEmail : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetaccountEmail(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accountEmail : String Index 0 Read FaccountEmail Write SetaccountEmail;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEnterpriseAccountClass = Class of TEnterpriseAccount;
  
  { --------------------------------------------------------------------
    TEnterprisesListResponse
    --------------------------------------------------------------------}
  
  TEnterprisesListResponse = Class(TGoogleBaseObject)
  Private
    Fenterprise : TEnterprisesListResponseTypeenterpriseArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setenterprise(AIndex : Integer; AValue : TEnterprisesListResponseTypeenterpriseArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property enterprise : TEnterprisesListResponseTypeenterpriseArray Index 0 Read Fenterprise Write Setenterprise;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEnterprisesListResponseClass = Class of TEnterprisesListResponse;
  
  { --------------------------------------------------------------------
    TEntitlement
    --------------------------------------------------------------------}
  
  TEntitlement = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FproductId : String;
    Freason : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property productId : String Index 8 Read FproductId Write SetproductId;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TEntitlementClass = Class of TEntitlement;
  
  { --------------------------------------------------------------------
    TEntitlementsListResponse
    --------------------------------------------------------------------}
  
  TEntitlementsListResponse = Class(TGoogleBaseObject)
  Private
    Fentitlement : TEntitlementsListResponseTypeentitlementArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setentitlement(AIndex : Integer; AValue : TEntitlementsListResponseTypeentitlementArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property entitlement : TEntitlementsListResponseTypeentitlementArray Index 0 Read Fentitlement Write Setentitlement;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TEntitlementsListResponseClass = Class of TEntitlementsListResponse;
  
  { --------------------------------------------------------------------
    TGroupLicense
    --------------------------------------------------------------------}
  
  TGroupLicense = Class(TGoogleBaseObject)
  Private
    FacquisitionKind : String;
    Fapproval : String;
    Fkind : String;
    FnumProvisioned : integer;
    FnumPurchased : integer;
    FproductId : String;
  Protected
    //Property setters
    Procedure SetacquisitionKind(AIndex : Integer; AValue : String); virtual;
    Procedure Setapproval(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumProvisioned(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumPurchased(AIndex : Integer; AValue : integer); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property acquisitionKind : String Index 0 Read FacquisitionKind Write SetacquisitionKind;
    Property approval : String Index 8 Read Fapproval Write Setapproval;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property numProvisioned : integer Index 24 Read FnumProvisioned Write SetnumProvisioned;
    Property numPurchased : integer Index 32 Read FnumPurchased Write SetnumPurchased;
    Property productId : String Index 40 Read FproductId Write SetproductId;
  end;
  TGroupLicenseClass = Class of TGroupLicense;
  
  { --------------------------------------------------------------------
    TGroupLicenseUsersListResponse
    --------------------------------------------------------------------}
  
  TGroupLicenseUsersListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fuser : TGroupLicenseUsersListResponseTypeuserArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TGroupLicenseUsersListResponseTypeuserArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property user : TGroupLicenseUsersListResponseTypeuserArray Index 8 Read Fuser Write Setuser;
  end;
  TGroupLicenseUsersListResponseClass = Class of TGroupLicenseUsersListResponse;
  
  { --------------------------------------------------------------------
    TGroupLicensesListResponse
    --------------------------------------------------------------------}
  
  TGroupLicensesListResponse = Class(TGoogleBaseObject)
  Private
    FgroupLicense : TGroupLicensesListResponseTypegroupLicenseArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetgroupLicense(AIndex : Integer; AValue : TGroupLicensesListResponseTypegroupLicenseArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property groupLicense : TGroupLicensesListResponseTypegroupLicenseArray Index 0 Read FgroupLicense Write SetgroupLicense;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TGroupLicensesListResponseClass = Class of TGroupLicensesListResponse;
  
  { --------------------------------------------------------------------
    TInstall
    --------------------------------------------------------------------}
  
  TInstall = Class(TGoogleBaseObject)
  Private
    FinstallState : String;
    Fkind : String;
    FproductId : String;
    FversionCode : integer;
  Protected
    //Property setters
    Procedure SetinstallState(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure SetversionCode(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property installState : String Index 0 Read FinstallState Write SetinstallState;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property productId : String Index 16 Read FproductId Write SetproductId;
    Property versionCode : integer Index 24 Read FversionCode Write SetversionCode;
  end;
  TInstallClass = Class of TInstall;
  
  { --------------------------------------------------------------------
    TInstallsListResponse
    --------------------------------------------------------------------}
  
  TInstallsListResponse = Class(TGoogleBaseObject)
  Private
    Finstall : TInstallsListResponseTypeinstallArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setinstall(AIndex : Integer; AValue : TInstallsListResponseTypeinstallArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property install : TInstallsListResponseTypeinstallArray Index 0 Read Finstall Write Setinstall;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TInstallsListResponseClass = Class of TInstallsListResponse;
  
  { --------------------------------------------------------------------
    TPermission
    --------------------------------------------------------------------}
  
  TPermission = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fkind : String;
    Fname : String;
    FpermissionId : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property permissionId : String Index 24 Read FpermissionId Write SetpermissionId;
  end;
  TPermissionClass = Class of TPermission;
  
  { --------------------------------------------------------------------
    TProduct
    --------------------------------------------------------------------}
  
  TProduct = Class(TGoogleBaseObject)
  Private
    FauthorName : String;
    FdetailsUrl : String;
    FiconUrl : String;
    Fkind : String;
    FproductId : String;
    FrequiresContainerApp : boolean;
    Ftitle : String;
    FworkDetailsUrl : String;
  Protected
    //Property setters
    Procedure SetauthorName(AIndex : Integer; AValue : String); virtual;
    Procedure SetdetailsUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequiresContainerApp(AIndex : Integer; AValue : boolean); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SetworkDetailsUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property authorName : String Index 0 Read FauthorName Write SetauthorName;
    Property detailsUrl : String Index 8 Read FdetailsUrl Write SetdetailsUrl;
    Property iconUrl : String Index 16 Read FiconUrl Write SeticonUrl;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property productId : String Index 32 Read FproductId Write SetproductId;
    Property requiresContainerApp : boolean Index 40 Read FrequiresContainerApp Write SetrequiresContainerApp;
    Property title : String Index 48 Read Ftitle Write Settitle;
    Property workDetailsUrl : String Index 56 Read FworkDetailsUrl Write SetworkDetailsUrl;
  end;
  TProductClass = Class of TProduct;
  
  { --------------------------------------------------------------------
    TProductPermission
    --------------------------------------------------------------------}
  
  TProductPermission = Class(TGoogleBaseObject)
  Private
    FpermissionId : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure SetpermissionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property permissionId : String Index 0 Read FpermissionId Write SetpermissionId;
    Property state : String Index 8 Read Fstate Write Setstate;
  end;
  TProductPermissionClass = Class of TProductPermission;
  
  { --------------------------------------------------------------------
    TProductPermissions
    --------------------------------------------------------------------}
  
  TProductPermissions = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fpermission : TProductPermissionsTypepermissionArray;
    FproductId : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpermission(AIndex : Integer; AValue : TProductPermissionsTypepermissionArray); virtual;
    Procedure SetproductId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property permission : TProductPermissionsTypepermissionArray Index 8 Read Fpermission Write Setpermission;
    Property productId : String Index 16 Read FproductId Write SetproductId;
  end;
  TProductPermissionsClass = Class of TProductPermissions;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    FprimaryEmail : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetprimaryEmail(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property primaryEmail : String Index 16 Read FprimaryEmail Write SetprimaryEmail;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUserToken
    --------------------------------------------------------------------}
  
  TUserToken = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Ftoken : String;
    FuserId : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property token : String Index 8 Read Ftoken Write Settoken;
    Property userId : String Index 16 Read FuserId Write SetuserId;
  end;
  TUserTokenClass = Class of TUserToken;
  
  { --------------------------------------------------------------------
    TUsersListResponse
    --------------------------------------------------------------------}
  
  TUsersListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fuser : TUsersListResponseTypeuserArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : TUsersListResponseTypeuserArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property user : TUsersListResponseTypeuserArray Index 8 Read Fuser Write Setuser;
  end;
  TUsersListResponseClass = Class of TUsersListResponse;
  
  { --------------------------------------------------------------------
    TCollectionsResource
    --------------------------------------------------------------------}
  
  TCollectionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(collectionId: string; enterpriseId: string);
    Function Get(collectionId: string; enterpriseId: string) : TCollection;
    Function Insert(enterpriseId: string; aCollection : TCollection) : TCollection;
    Function List(enterpriseId: string) : TCollectionsListResponse;
    Function Patch(collectionId: string; enterpriseId: string; aCollection : TCollection) : TCollection;
    Function Update(collectionId: string; enterpriseId: string; aCollection : TCollection) : TCollection;
  end;
  
  
  { --------------------------------------------------------------------
    TCollectionviewersResource
    --------------------------------------------------------------------}
  
  TCollectionviewersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(collectionId: string; enterpriseId: string; userId: string);
    Function Get(collectionId: string; enterpriseId: string; userId: string) : TUser;
    Function List(collectionId: string; enterpriseId: string) : TCollectionViewersListResponse;
    Function Patch(collectionId: string; enterpriseId: string; userId: string; aUser : TUser) : TUser;
    Function Update(collectionId: string; enterpriseId: string; userId: string; aUser : TUser) : TUser;
  end;
  
  
  { --------------------------------------------------------------------
    TDevicesResource
    --------------------------------------------------------------------}
  
  TDevicesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(deviceId: string; enterpriseId: string; userId: string) : TDevice;
    Function GetState(deviceId: string; enterpriseId: string; userId: string) : TDeviceState;
    Function List(enterpriseId: string; userId: string) : TDevicesListResponse;
    Function SetState(deviceId: string; enterpriseId: string; userId: string; aDeviceState : TDeviceState) : TDeviceState;
  end;
  
  
  { --------------------------------------------------------------------
    TEnterprisesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEnterprisesResource, method Enroll
  
  TEnterprisesEnrollOptions = Record
    token : String;
  end;
  
  
  //Optional query Options for TEnterprisesResource, method Insert
  
  TEnterprisesInsertOptions = Record
    token : String;
  end;
  
  
  //Optional query Options for TEnterprisesResource, method List
  
  TEnterprisesListOptions = Record
    domain : String;
  end;
  
  TEnterprisesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(enterpriseId: string);
    Function Enroll(aEnterprise : TEnterprise; AQuery : string  = '') : TEnterprise;
    Function Enroll(aEnterprise : TEnterprise; AQuery : TEnterprisesenrollOptions) : TEnterprise;
    Function Get(enterpriseId: string) : TEnterprise;
    Function Insert(aEnterprise : TEnterprise; AQuery : string  = '') : TEnterprise;
    Function Insert(aEnterprise : TEnterprise; AQuery : TEnterprisesinsertOptions) : TEnterprise;
    Function List(AQuery : string  = '') : TEnterprisesListResponse;
    Function List(AQuery : TEnterpriseslistOptions) : TEnterprisesListResponse;
    Function SetAccount(enterpriseId: string; aEnterpriseAccount : TEnterpriseAccount) : TEnterpriseAccount;
    Procedure Unenroll(enterpriseId: string);
  end;
  
  
  { --------------------------------------------------------------------
    TEntitlementsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEntitlementsResource, method Patch
  
  TEntitlementsPatchOptions = Record
    install : boolean;
  end;
  
  
  //Optional query Options for TEntitlementsResource, method Update
  
  TEntitlementsUpdateOptions = Record
    install : boolean;
  end;
  
  TEntitlementsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(enterpriseId: string; entitlementId: string; userId: string);
    Function Get(enterpriseId: string; entitlementId: string; userId: string) : TEntitlement;
    Function List(enterpriseId: string; userId: string) : TEntitlementsListResponse;
    Function Patch(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : string  = '') : TEntitlement;
    Function Patch(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : TEntitlementspatchOptions) : TEntitlement;
    Function Update(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : string  = '') : TEntitlement;
    Function Update(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : TEntitlementsupdateOptions) : TEntitlement;
  end;
  
  
  { --------------------------------------------------------------------
    TGrouplicensesResource
    --------------------------------------------------------------------}
  
  TGrouplicensesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(enterpriseId: string; groupLicenseId: string) : TGroupLicense;
    Function List(enterpriseId: string) : TGroupLicensesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TGrouplicenseusersResource
    --------------------------------------------------------------------}
  
  TGrouplicenseusersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(enterpriseId: string; groupLicenseId: string) : TGroupLicenseUsersListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TInstallsResource
    --------------------------------------------------------------------}
  
  TInstallsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(deviceId: string; enterpriseId: string; installId: string; userId: string);
    Function Get(deviceId: string; enterpriseId: string; installId: string; userId: string) : TInstall;
    Function List(deviceId: string; enterpriseId: string; userId: string) : TInstallsListResponse;
    Function Patch(deviceId: string; enterpriseId: string; installId: string; userId: string; aInstall : TInstall) : TInstall;
    Function Update(deviceId: string; enterpriseId: string; installId: string; userId: string; aInstall : TInstall) : TInstall;
  end;
  
  
  { --------------------------------------------------------------------
    TPermissionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPermissionsResource, method Get
  
  TPermissionsGetOptions = Record
    language : String;
  end;
  
  TPermissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(permissionId: string; AQuery : string  = '') : TPermission;
    Function Get(permissionId: string; AQuery : TPermissionsgetOptions) : TPermission;
  end;
  
  
  { --------------------------------------------------------------------
    TProductsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProductsResource, method Get
  
  TProductsGetOptions = Record
    language : String;
  end;
  
  
  //Optional query Options for TProductsResource, method GetAppRestrictionsSchema
  
  TProductsGetAppRestrictionsSchemaOptions = Record
    language : String;
  end;
  
  TProductsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(enterpriseId: string; productId: string; AQuery : string  = '') : TProduct;
    Function Get(enterpriseId: string; productId: string; AQuery : TProductsgetOptions) : TProduct;
    Function GetAppRestrictionsSchema(enterpriseId: string; productId: string; AQuery : string  = '') : TAppRestrictionsSchema;
    Function GetAppRestrictionsSchema(enterpriseId: string; productId: string; AQuery : TProductsgetAppRestrictionsSchemaOptions) : TAppRestrictionsSchema;
    Function GetPermissions(enterpriseId: string; productId: string) : TProductPermissions;
    Function UpdatePermissions(enterpriseId: string; productId: string; aProductPermissions : TProductPermissions) : TProductPermissions;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersResource, method List
  
  TUsersListOptions = Record
    email : String;
  end;
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GenerateToken(enterpriseId: string; userId: string) : TUserToken;
    Function Get(enterpriseId: string; userId: string) : TUser;
    Function List(enterpriseId: string; AQuery : string  = '') : TUsersListResponse;
    Function List(enterpriseId: string; AQuery : TUserslistOptions) : TUsersListResponse;
    Procedure RevokeToken(enterpriseId: string; userId: string);
  end;
  
  
  { --------------------------------------------------------------------
    TAndroidenterpriseAPI
    --------------------------------------------------------------------}
  
  TAndroidenterpriseAPI = Class(TGoogleAPI)
  Private
    FCollectionsInstance : TCollectionsResource;
    FCollectionviewersInstance : TCollectionviewersResource;
    FDevicesInstance : TDevicesResource;
    FEnterprisesInstance : TEnterprisesResource;
    FEntitlementsInstance : TEntitlementsResource;
    FGrouplicensesInstance : TGrouplicensesResource;
    FGrouplicenseusersInstance : TGrouplicenseusersResource;
    FInstallsInstance : TInstallsResource;
    FPermissionsInstance : TPermissionsResource;
    FProductsInstance : TProductsResource;
    FUsersInstance : TUsersResource;
    Function GetCollectionsInstance : TCollectionsResource;virtual;
    Function GetCollectionviewersInstance : TCollectionviewersResource;virtual;
    Function GetDevicesInstance : TDevicesResource;virtual;
    Function GetEnterprisesInstance : TEnterprisesResource;virtual;
    Function GetEntitlementsInstance : TEntitlementsResource;virtual;
    Function GetGrouplicensesInstance : TGrouplicensesResource;virtual;
    Function GetGrouplicenseusersInstance : TGrouplicenseusersResource;virtual;
    Function GetInstallsInstance : TInstallsResource;virtual;
    Function GetPermissionsInstance : TPermissionsResource;virtual;
    Function GetProductsInstance : TProductsResource;virtual;
    Function GetUsersInstance : TUsersResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateCollectionsResource(AOwner : TComponent) : TCollectionsResource;virtual;overload;
    Function CreateCollectionsResource : TCollectionsResource;virtual;overload;
    Function CreateCollectionviewersResource(AOwner : TComponent) : TCollectionviewersResource;virtual;overload;
    Function CreateCollectionviewersResource : TCollectionviewersResource;virtual;overload;
    Function CreateDevicesResource(AOwner : TComponent) : TDevicesResource;virtual;overload;
    Function CreateDevicesResource : TDevicesResource;virtual;overload;
    Function CreateEnterprisesResource(AOwner : TComponent) : TEnterprisesResource;virtual;overload;
    Function CreateEnterprisesResource : TEnterprisesResource;virtual;overload;
    Function CreateEntitlementsResource(AOwner : TComponent) : TEntitlementsResource;virtual;overload;
    Function CreateEntitlementsResource : TEntitlementsResource;virtual;overload;
    Function CreateGrouplicensesResource(AOwner : TComponent) : TGrouplicensesResource;virtual;overload;
    Function CreateGrouplicensesResource : TGrouplicensesResource;virtual;overload;
    Function CreateGrouplicenseusersResource(AOwner : TComponent) : TGrouplicenseusersResource;virtual;overload;
    Function CreateGrouplicenseusersResource : TGrouplicenseusersResource;virtual;overload;
    Function CreateInstallsResource(AOwner : TComponent) : TInstallsResource;virtual;overload;
    Function CreateInstallsResource : TInstallsResource;virtual;overload;
    Function CreatePermissionsResource(AOwner : TComponent) : TPermissionsResource;virtual;overload;
    Function CreatePermissionsResource : TPermissionsResource;virtual;overload;
    Function CreateProductsResource(AOwner : TComponent) : TProductsResource;virtual;overload;
    Function CreateProductsResource : TProductsResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property CollectionsResource : TCollectionsResource Read GetCollectionsInstance;
    Property CollectionviewersResource : TCollectionviewersResource Read GetCollectionviewersInstance;
    Property DevicesResource : TDevicesResource Read GetDevicesInstance;
    Property EnterprisesResource : TEnterprisesResource Read GetEnterprisesInstance;
    Property EntitlementsResource : TEntitlementsResource Read GetEntitlementsInstance;
    Property GrouplicensesResource : TGrouplicensesResource Read GetGrouplicensesInstance;
    Property GrouplicenseusersResource : TGrouplicenseusersResource Read GetGrouplicenseusersInstance;
    Property InstallsResource : TInstallsResource Read GetInstallsInstance;
    Property PermissionsResource : TPermissionsResource Read GetPermissionsInstance;
    Property ProductsResource : TProductsResource Read GetProductsInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAppRestrictionsSchema
  --------------------------------------------------------------------}


Procedure TAppRestrictionsSchema.Setrestrictions(AIndex : Integer; AValue : TAppRestrictionsSchemaTyperestrictionsArray); 

begin
  If (Frestrictions=AValue) then exit;
  Frestrictions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppRestrictionsSchemaRestriction
  --------------------------------------------------------------------}


Procedure TAppRestrictionsSchemaRestriction.SetdefaultValue(AIndex : Integer; AValue : TAppRestrictionsSchemaRestrictionRestrictionValue); 

begin
  If (FdefaultValue=AValue) then exit;
  FdefaultValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.Setentry(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fentry=AValue) then exit;
  Fentry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.SetentryValue(AIndex : Integer; AValue : TStringArray); 

begin
  If (FentryValue=AValue) then exit;
  FentryValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.SetrestrictionType(AIndex : Integer; AValue : String); 

begin
  If (FrestrictionType=AValue) then exit;
  FrestrictionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestriction.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppRestrictionsSchemaRestrictionRestrictionValue
  --------------------------------------------------------------------}


Procedure TAppRestrictionsSchemaRestrictionRestrictionValue.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestrictionRestrictionValue.SetvalueBool(AIndex : Integer; AValue : boolean); 

begin
  If (FvalueBool=AValue) then exit;
  FvalueBool:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestrictionRestrictionValue.SetvalueInteger(AIndex : Integer; AValue : integer); 

begin
  If (FvalueInteger=AValue) then exit;
  FvalueInteger:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestrictionRestrictionValue.SetvalueMultiselect(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvalueMultiselect=AValue) then exit;
  FvalueMultiselect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAppRestrictionsSchemaRestrictionRestrictionValue.SetvalueString(AIndex : Integer; AValue : String); 

begin
  If (FvalueString=AValue) then exit;
  FvalueString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAppRestrictionsSchemaRestrictionRestrictionValue.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCollection
  --------------------------------------------------------------------}


Procedure TCollection.SetcollectionId(AIndex : Integer; AValue : String); 

begin
  If (FcollectionId=AValue) then exit;
  FcollectionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollection.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollection.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollection.SetproductId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollection.Setvisibility(AIndex : Integer; AValue : String); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCollectionViewersListResponse
  --------------------------------------------------------------------}


Procedure TCollectionViewersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectionViewersListResponse.Setuser(AIndex : Integer; AValue : TCollectionViewersListResponseTypeuserArray); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCollectionsListResponse
  --------------------------------------------------------------------}


Procedure TCollectionsListResponse.Setcollection(AIndex : Integer; AValue : TCollectionsListResponseTypecollectionArray); 

begin
  If (Fcollection=AValue) then exit;
  Fcollection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCollectionsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDevice
  --------------------------------------------------------------------}


Procedure TDevice.SetandroidId(AIndex : Integer; AValue : String); 

begin
  If (FandroidId=AValue) then exit;
  FandroidId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevice.SetmanagementType(AIndex : Integer; AValue : String); 

begin
  If (FmanagementType=AValue) then exit;
  FmanagementType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeviceState
  --------------------------------------------------------------------}


Procedure TDeviceState.SetaccountState(AIndex : Integer; AValue : String); 

begin
  If (FaccountState=AValue) then exit;
  FaccountState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceState.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDevicesListResponse
  --------------------------------------------------------------------}


Procedure TDevicesListResponse.Setdevice(AIndex : Integer; AValue : TDevicesListResponseTypedeviceArray); 

begin
  If (Fdevice=AValue) then exit;
  Fdevice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDevicesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnterprise
  --------------------------------------------------------------------}


Procedure TEnterprise.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnterprise.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnterprise.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnterprise.SetprimaryDomain(AIndex : Integer; AValue : String); 

begin
  If (FprimaryDomain=AValue) then exit;
  FprimaryDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnterpriseAccount
  --------------------------------------------------------------------}


Procedure TEnterpriseAccount.SetaccountEmail(AIndex : Integer; AValue : String); 

begin
  If (FaccountEmail=AValue) then exit;
  FaccountEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnterpriseAccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnterprisesListResponse
  --------------------------------------------------------------------}


Procedure TEnterprisesListResponse.Setenterprise(AIndex : Integer; AValue : TEnterprisesListResponseTypeenterpriseArray); 

begin
  If (Fenterprise=AValue) then exit;
  Fenterprise:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnterprisesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlement
  --------------------------------------------------------------------}


Procedure TEntitlement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlement.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntitlementsListResponse
  --------------------------------------------------------------------}


Procedure TEntitlementsListResponse.Setentitlement(AIndex : Integer; AValue : TEntitlementsListResponseTypeentitlementArray); 

begin
  If (Fentitlement=AValue) then exit;
  Fentitlement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntitlementsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupLicense
  --------------------------------------------------------------------}


Procedure TGroupLicense.SetacquisitionKind(AIndex : Integer; AValue : String); 

begin
  If (FacquisitionKind=AValue) then exit;
  FacquisitionKind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicense.Setapproval(AIndex : Integer; AValue : String); 

begin
  If (Fapproval=AValue) then exit;
  Fapproval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicense.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicense.SetnumProvisioned(AIndex : Integer; AValue : integer); 

begin
  If (FnumProvisioned=AValue) then exit;
  FnumProvisioned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicense.SetnumPurchased(AIndex : Integer; AValue : integer); 

begin
  If (FnumPurchased=AValue) then exit;
  FnumPurchased:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicense.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupLicenseUsersListResponse
  --------------------------------------------------------------------}


Procedure TGroupLicenseUsersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicenseUsersListResponse.Setuser(AIndex : Integer; AValue : TGroupLicenseUsersListResponseTypeuserArray); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGroupLicensesListResponse
  --------------------------------------------------------------------}


Procedure TGroupLicensesListResponse.SetgroupLicense(AIndex : Integer; AValue : TGroupLicensesListResponseTypegroupLicenseArray); 

begin
  If (FgroupLicense=AValue) then exit;
  FgroupLicense:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGroupLicensesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstall
  --------------------------------------------------------------------}


Procedure TInstall.SetinstallState(AIndex : Integer; AValue : String); 

begin
  If (FinstallState=AValue) then exit;
  FinstallState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstall.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstall.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstall.SetversionCode(AIndex : Integer; AValue : integer); 

begin
  If (FversionCode=AValue) then exit;
  FversionCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstallsListResponse
  --------------------------------------------------------------------}


Procedure TInstallsListResponse.Setinstall(AIndex : Integer; AValue : TInstallsListResponseTypeinstallArray); 

begin
  If (Finstall=AValue) then exit;
  Finstall:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstallsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPermission
  --------------------------------------------------------------------}


Procedure TPermission.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPermission.SetpermissionId(AIndex : Integer; AValue : String); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProduct
  --------------------------------------------------------------------}


Procedure TProduct.SetauthorName(AIndex : Integer; AValue : String); 

begin
  If (FauthorName=AValue) then exit;
  FauthorName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetdetailsUrl(AIndex : Integer; AValue : String); 

begin
  If (FdetailsUrl=AValue) then exit;
  FdetailsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SeticonUrl(AIndex : Integer; AValue : String); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetrequiresContainerApp(AIndex : Integer; AValue : boolean); 

begin
  If (FrequiresContainerApp=AValue) then exit;
  FrequiresContainerApp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProduct.SetworkDetailsUrl(AIndex : Integer; AValue : String); 

begin
  If (FworkDetailsUrl=AValue) then exit;
  FworkDetailsUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductPermission
  --------------------------------------------------------------------}


Procedure TProductPermission.SetpermissionId(AIndex : Integer; AValue : String); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPermission.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProductPermissions
  --------------------------------------------------------------------}


Procedure TProductPermissions.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPermissions.Setpermission(AIndex : Integer; AValue : TProductPermissionsTypepermissionArray); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProductPermissions.SetproductId(AIndex : Integer; AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.SetprimaryEmail(AIndex : Integer; AValue : String); 

begin
  If (FprimaryEmail=AValue) then exit;
  FprimaryEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserToken
  --------------------------------------------------------------------}


Procedure TUserToken.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserToken.Settoken(AIndex : Integer; AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserToken.SetuserId(AIndex : Integer; AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersListResponse
  --------------------------------------------------------------------}


Procedure TUsersListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersListResponse.Setuser(AIndex : Integer; AValue : TUsersListResponseTypeuserArray); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCollectionsResource
  --------------------------------------------------------------------}


Class Function TCollectionsResource.ResourceName : String;

begin
  Result:='collections';
end;

Class Function TCollectionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Procedure TCollectionsResource.Delete(collectionId: string; enterpriseId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}';
  _Methodid   = 'androidenterprise.collections.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCollectionsResource.Get(collectionId: string; enterpriseId: string) : TCollection;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}';
  _Methodid   = 'androidenterprise.collections.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCollection) as TCollection;
end;

Function TCollectionsResource.Insert(enterpriseId: string; aCollection : TCollection) : TCollection;

Const
  _HTTPMethod = 'POST';
  _Path       = 'enterprises/{enterpriseId}/collections';
  _Methodid   = 'androidenterprise.collections.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCollection,TCollection) as TCollection;
end;

Function TCollectionsResource.List(enterpriseId: string) : TCollectionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/collections';
  _Methodid   = 'androidenterprise.collections.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCollectionsListResponse) as TCollectionsListResponse;
end;

Function TCollectionsResource.Patch(collectionId: string; enterpriseId: string; aCollection : TCollection) : TCollection;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}';
  _Methodid   = 'androidenterprise.collections.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCollection,TCollection) as TCollection;
end;

Function TCollectionsResource.Update(collectionId: string; enterpriseId: string; aCollection : TCollection) : TCollection;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}';
  _Methodid   = 'androidenterprise.collections.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCollection,TCollection) as TCollection;
end;



{ --------------------------------------------------------------------
  TCollectionviewersResource
  --------------------------------------------------------------------}


Class Function TCollectionviewersResource.ResourceName : String;

begin
  Result:='collectionviewers';
end;

Class Function TCollectionviewersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Procedure TCollectionviewersResource.Delete(collectionId: string; enterpriseId: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}/users/{userId}';
  _Methodid   = 'androidenterprise.collectionviewers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCollectionviewersResource.Get(collectionId: string; enterpriseId: string; userId: string) : TUser;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}/users/{userId}';
  _Methodid   = 'androidenterprise.collectionviewers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUser) as TUser;
end;

Function TCollectionviewersResource.List(collectionId: string; enterpriseId: string) : TCollectionViewersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}/users';
  _Methodid   = 'androidenterprise.collectionviewers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCollectionViewersListResponse) as TCollectionViewersListResponse;
end;

Function TCollectionviewersResource.Patch(collectionId: string; enterpriseId: string; userId: string; aUser : TUser) : TUser;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}/users/{userId}';
  _Methodid   = 'androidenterprise.collectionviewers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUser,TUser) as TUser;
end;

Function TCollectionviewersResource.Update(collectionId: string; enterpriseId: string; userId: string; aUser : TUser) : TUser;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/collections/{collectionId}/users/{userId}';
  _Methodid   = 'androidenterprise.collectionviewers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['collectionId',collectionId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUser,TUser) as TUser;
end;



{ --------------------------------------------------------------------
  TDevicesResource
  --------------------------------------------------------------------}


Class Function TDevicesResource.ResourceName : String;

begin
  Result:='devices';
end;

Class Function TDevicesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TDevicesResource.Get(deviceId: string; enterpriseId: string; userId: string) : TDevice;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}';
  _Methodid   = 'androidenterprise.devices.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDevice) as TDevice;
end;

Function TDevicesResource.GetState(deviceId: string; enterpriseId: string; userId: string) : TDeviceState;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/state';
  _Methodid   = 'androidenterprise.devices.getState';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDeviceState) as TDeviceState;
end;

Function TDevicesResource.List(enterpriseId: string; userId: string) : TDevicesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices';
  _Methodid   = 'androidenterprise.devices.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDevicesListResponse) as TDevicesListResponse;
end;

Function TDevicesResource.SetState(deviceId: string; enterpriseId: string; userId: string; aDeviceState : TDeviceState) : TDeviceState;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/state';
  _Methodid   = 'androidenterprise.devices.setState';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDeviceState,TDeviceState) as TDeviceState;
end;



{ --------------------------------------------------------------------
  TEnterprisesResource
  --------------------------------------------------------------------}


Class Function TEnterprisesResource.ResourceName : String;

begin
  Result:='enterprises';
end;

Class Function TEnterprisesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Procedure TEnterprisesResource.Delete(enterpriseId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}';
  _Methodid   = 'androidenterprise.enterprises.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEnterprisesResource.Enroll(aEnterprise : TEnterprise; AQuery : string = '') : TEnterprise;

Const
  _HTTPMethod = 'POST';
  _Path       = 'enterprises/enroll';
  _Methodid   = 'androidenterprise.enterprises.enroll';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aEnterprise,TEnterprise) as TEnterprise;
end;


Function TEnterprisesResource.Enroll(aEnterprise : TEnterprise; AQuery : TEnterprisesenrollOptions) : TEnterprise;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'token',AQuery.token);
  Result:=Enroll(aEnterprise,_Q);
end;

Function TEnterprisesResource.Get(enterpriseId: string) : TEnterprise;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}';
  _Methodid   = 'androidenterprise.enterprises.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEnterprise) as TEnterprise;
end;

Function TEnterprisesResource.Insert(aEnterprise : TEnterprise; AQuery : string = '') : TEnterprise;

Const
  _HTTPMethod = 'POST';
  _Path       = 'enterprises';
  _Methodid   = 'androidenterprise.enterprises.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aEnterprise,TEnterprise) as TEnterprise;
end;


Function TEnterprisesResource.Insert(aEnterprise : TEnterprise; AQuery : TEnterprisesinsertOptions) : TEnterprise;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'token',AQuery.token);
  Result:=Insert(aEnterprise,_Q);
end;

Function TEnterprisesResource.List(AQuery : string = '') : TEnterprisesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises';
  _Methodid   = 'androidenterprise.enterprises.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TEnterprisesListResponse) as TEnterprisesListResponse;
end;


Function TEnterprisesResource.List(AQuery : TEnterpriseslistOptions) : TEnterprisesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'domain',AQuery.domain);
  Result:=List(_Q);
end;

Function TEnterprisesResource.SetAccount(enterpriseId: string; aEnterpriseAccount : TEnterpriseAccount) : TEnterpriseAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/account';
  _Methodid   = 'androidenterprise.enterprises.setAccount';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEnterpriseAccount,TEnterpriseAccount) as TEnterpriseAccount;
end;

Procedure TEnterprisesResource.Unenroll(enterpriseId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'enterprises/{enterpriseId}/unenroll';
  _Methodid   = 'androidenterprise.enterprises.unenroll';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TEntitlementsResource
  --------------------------------------------------------------------}


Class Function TEntitlementsResource.ResourceName : String;

begin
  Result:='entitlements';
end;

Class Function TEntitlementsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Procedure TEntitlementsResource.Delete(enterpriseId: string; entitlementId: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/entitlements/{entitlementId}';
  _Methodid   = 'androidenterprise.entitlements.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'entitlementId',entitlementId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TEntitlementsResource.Get(enterpriseId: string; entitlementId: string; userId: string) : TEntitlement;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/entitlements/{entitlementId}';
  _Methodid   = 'androidenterprise.entitlements.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'entitlementId',entitlementId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEntitlement) as TEntitlement;
end;

Function TEntitlementsResource.List(enterpriseId: string; userId: string) : TEntitlementsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/entitlements';
  _Methodid   = 'androidenterprise.entitlements.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEntitlementsListResponse) as TEntitlementsListResponse;
end;

Function TEntitlementsResource.Patch(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : string = '') : TEntitlement;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/entitlements/{entitlementId}';
  _Methodid   = 'androidenterprise.entitlements.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'entitlementId',entitlementId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEntitlement,TEntitlement) as TEntitlement;
end;


Function TEntitlementsResource.Patch(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : TEntitlementspatchOptions) : TEntitlement;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'install',AQuery.install);
  Result:=Patch(enterpriseId,entitlementId,userId,aEntitlement,_Q);
end;

Function TEntitlementsResource.Update(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : string = '') : TEntitlement;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/entitlements/{entitlementId}';
  _Methodid   = 'androidenterprise.entitlements.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'entitlementId',entitlementId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEntitlement,TEntitlement) as TEntitlement;
end;


Function TEntitlementsResource.Update(enterpriseId: string; entitlementId: string; userId: string; aEntitlement : TEntitlement; AQuery : TEntitlementsupdateOptions) : TEntitlement;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'install',AQuery.install);
  Result:=Update(enterpriseId,entitlementId,userId,aEntitlement,_Q);
end;



{ --------------------------------------------------------------------
  TGrouplicensesResource
  --------------------------------------------------------------------}


Class Function TGrouplicensesResource.ResourceName : String;

begin
  Result:='grouplicenses';
end;

Class Function TGrouplicensesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TGrouplicensesResource.Get(enterpriseId: string; groupLicenseId: string) : TGroupLicense;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/groupLicenses/{groupLicenseId}';
  _Methodid   = 'androidenterprise.grouplicenses.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'groupLicenseId',groupLicenseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroupLicense) as TGroupLicense;
end;

Function TGrouplicensesResource.List(enterpriseId: string) : TGroupLicensesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/groupLicenses';
  _Methodid   = 'androidenterprise.grouplicenses.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroupLicensesListResponse) as TGroupLicensesListResponse;
end;



{ --------------------------------------------------------------------
  TGrouplicenseusersResource
  --------------------------------------------------------------------}


Class Function TGrouplicenseusersResource.ResourceName : String;

begin
  Result:='grouplicenseusers';
end;

Class Function TGrouplicenseusersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TGrouplicenseusersResource.List(enterpriseId: string; groupLicenseId: string) : TGroupLicenseUsersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/groupLicenses/{groupLicenseId}/users';
  _Methodid   = 'androidenterprise.grouplicenseusers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'groupLicenseId',groupLicenseId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TGroupLicenseUsersListResponse) as TGroupLicenseUsersListResponse;
end;



{ --------------------------------------------------------------------
  TInstallsResource
  --------------------------------------------------------------------}


Class Function TInstallsResource.ResourceName : String;

begin
  Result:='installs';
end;

Class Function TInstallsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Procedure TInstallsResource.Delete(deviceId: string; enterpriseId: string; installId: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/installs/{installId}';
  _Methodid   = 'androidenterprise.installs.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'installId',installId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TInstallsResource.Get(deviceId: string; enterpriseId: string; installId: string; userId: string) : TInstall;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/installs/{installId}';
  _Methodid   = 'androidenterprise.installs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'installId',installId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstall) as TInstall;
end;

Function TInstallsResource.List(deviceId: string; enterpriseId: string; userId: string) : TInstallsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/installs';
  _Methodid   = 'androidenterprise.installs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstallsListResponse) as TInstallsListResponse;
end;

Function TInstallsResource.Patch(deviceId: string; enterpriseId: string; installId: string; userId: string; aInstall : TInstall) : TInstall;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/installs/{installId}';
  _Methodid   = 'androidenterprise.installs.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'installId',installId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstall,TInstall) as TInstall;
end;

Function TInstallsResource.Update(deviceId: string; enterpriseId: string; installId: string; userId: string; aInstall : TInstall) : TInstall;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/devices/{deviceId}/installs/{installId}';
  _Methodid   = 'androidenterprise.installs.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deviceId',deviceId,'enterpriseId',enterpriseId,'installId',installId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstall,TInstall) as TInstall;
end;



{ --------------------------------------------------------------------
  TPermissionsResource
  --------------------------------------------------------------------}


Class Function TPermissionsResource.ResourceName : String;

begin
  Result:='permissions';
end;

Class Function TPermissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TPermissionsResource.Get(permissionId: string; AQuery : string = '') : TPermission;

Const
  _HTTPMethod = 'GET';
  _Path       = 'permissions/{permissionId}';
  _Methodid   = 'androidenterprise.permissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPermission) as TPermission;
end;


Function TPermissionsResource.Get(permissionId: string; AQuery : TPermissionsgetOptions) : TPermission;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(permissionId,_Q);
end;



{ --------------------------------------------------------------------
  TProductsResource
  --------------------------------------------------------------------}


Class Function TProductsResource.ResourceName : String;

begin
  Result:='products';
end;

Class Function TProductsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TProductsResource.Get(enterpriseId: string; productId: string; AQuery : string = '') : TProduct;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/products/{productId}';
  _Methodid   = 'androidenterprise.products.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TProduct) as TProduct;
end;


Function TProductsResource.Get(enterpriseId: string; productId: string; AQuery : TProductsgetOptions) : TProduct;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=Get(enterpriseId,productId,_Q);
end;

Function TProductsResource.GetAppRestrictionsSchema(enterpriseId: string; productId: string; AQuery : string = '') : TAppRestrictionsSchema;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/products/{productId}/appRestrictionsSchema';
  _Methodid   = 'androidenterprise.products.getAppRestrictionsSchema';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAppRestrictionsSchema) as TAppRestrictionsSchema;
end;


Function TProductsResource.GetAppRestrictionsSchema(enterpriseId: string; productId: string; AQuery : TProductsgetAppRestrictionsSchemaOptions) : TAppRestrictionsSchema;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'language',AQuery.language);
  Result:=GetAppRestrictionsSchema(enterpriseId,productId,_Q);
end;

Function TProductsResource.GetPermissions(enterpriseId: string; productId: string) : TProductPermissions;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/products/{productId}/permissions';
  _Methodid   = 'androidenterprise.products.getPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProductPermissions) as TProductPermissions;
end;

Function TProductsResource.UpdatePermissions(enterpriseId: string; productId: string; aProductPermissions : TProductPermissions) : TProductPermissions;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'enterprises/{enterpriseId}/products/{productId}/permissions';
  _Methodid   = 'androidenterprise.products.updatePermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'productId',productId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProductPermissions,TProductPermissions) as TProductPermissions;
end;



{ --------------------------------------------------------------------
  TUsersResource
  --------------------------------------------------------------------}


Class Function TUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TandroidenterpriseAPI;
end;

Function TUsersResource.GenerateToken(enterpriseId: string; userId: string) : TUserToken;

Const
  _HTTPMethod = 'POST';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/token';
  _Methodid   = 'androidenterprise.users.generateToken';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserToken) as TUserToken;
end;

Function TUsersResource.Get(enterpriseId: string; userId: string) : TUser;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}';
  _Methodid   = 'androidenterprise.users.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'userId',userId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUser) as TUser;
end;

Function TUsersResource.List(enterpriseId: string; AQuery : string = '') : TUsersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'enterprises/{enterpriseId}/users';
  _Methodid   = 'androidenterprise.users.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUsersListResponse) as TUsersListResponse;
end;


Function TUsersResource.List(enterpriseId: string; AQuery : TUserslistOptions) : TUsersListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'email',AQuery.email);
  Result:=List(enterpriseId,_Q);
end;

Procedure TUsersResource.RevokeToken(enterpriseId: string; userId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'enterprises/{enterpriseId}/users/{userId}/token';
  _Methodid   = 'androidenterprise.users.revokeToken';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['enterpriseId',enterpriseId,'userId',userId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TAndroidenterpriseAPI
  --------------------------------------------------------------------}

Class Function TAndroidenterpriseAPI.APIName : String;

begin
  Result:='androidenterprise';
end;

Class Function TAndroidenterpriseAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TAndroidenterpriseAPI.APIRevision : String;

begin
  Result:='20141112';
end;

Class Function TAndroidenterpriseAPI.APIID : String;

begin
  Result:='androidenterprise:v1';
end;

Class Function TAndroidenterpriseAPI.APITitle : String;

begin
  Result:='Google Play EMM API';
end;

Class Function TAndroidenterpriseAPI.APIDescription : String;

begin
  Result:='Allows MDMs/EMMs and enterprises to manage the deployment of apps to Android for Work users.';
end;

Class Function TAndroidenterpriseAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAndroidenterpriseAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAndroidenterpriseAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/android-16.png';
end;

Class Function TAndroidenterpriseAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/android-32.png';
end;

Class Function TAndroidenterpriseAPI.APIdocumentationLink : String;

begin
  Result:='';
end;

Class Function TAndroidenterpriseAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAndroidenterpriseAPI.APIbasePath : string;

begin
  Result:='/androidenterprise/v1/';
end;

Class Function TAndroidenterpriseAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/androidenterprise/v1/';
end;

Class Function TAndroidenterpriseAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAndroidenterpriseAPI.APIservicePath : string;

begin
  Result:='androidenterprise/v1/';
end;

Class Function TAndroidenterpriseAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAndroidenterpriseAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/androidenterprise';
  Result[0].Description:='Manage corporate Android devices';
  
end;

Class Function TAndroidenterpriseAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAndroidenterpriseAPI.RegisterAPIResources;

begin
  TAppRestrictionsSchema.RegisterObject;
  TAppRestrictionsSchemaRestriction.RegisterObject;
  TAppRestrictionsSchemaRestrictionRestrictionValue.RegisterObject;
  TCollection.RegisterObject;
  TCollectionViewersListResponse.RegisterObject;
  TCollectionsListResponse.RegisterObject;
  TDevice.RegisterObject;
  TDeviceState.RegisterObject;
  TDevicesListResponse.RegisterObject;
  TEnterprise.RegisterObject;
  TEnterpriseAccount.RegisterObject;
  TEnterprisesListResponse.RegisterObject;
  TEntitlement.RegisterObject;
  TEntitlementsListResponse.RegisterObject;
  TGroupLicense.RegisterObject;
  TGroupLicenseUsersListResponse.RegisterObject;
  TGroupLicensesListResponse.RegisterObject;
  TInstall.RegisterObject;
  TInstallsListResponse.RegisterObject;
  TPermission.RegisterObject;
  TProduct.RegisterObject;
  TProductPermission.RegisterObject;
  TProductPermissions.RegisterObject;
  TUser.RegisterObject;
  TUserToken.RegisterObject;
  TUsersListResponse.RegisterObject;
end;


Function TAndroidenterpriseAPI.GetCollectionsInstance : TCollectionsResource;

begin
  if (FCollectionsInstance=Nil) then
    FCollectionsInstance:=CreateCollectionsResource;
  Result:=FCollectionsInstance;
end;

Function TAndroidenterpriseAPI.CreateCollectionsResource : TCollectionsResource;

begin
  Result:=CreateCollectionsResource(Self);
end;


Function TAndroidenterpriseAPI.CreateCollectionsResource(AOwner : TComponent) : TCollectionsResource;

begin
  Result:=TCollectionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetCollectionviewersInstance : TCollectionviewersResource;

begin
  if (FCollectionviewersInstance=Nil) then
    FCollectionviewersInstance:=CreateCollectionviewersResource;
  Result:=FCollectionviewersInstance;
end;

Function TAndroidenterpriseAPI.CreateCollectionviewersResource : TCollectionviewersResource;

begin
  Result:=CreateCollectionviewersResource(Self);
end;


Function TAndroidenterpriseAPI.CreateCollectionviewersResource(AOwner : TComponent) : TCollectionviewersResource;

begin
  Result:=TCollectionviewersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetDevicesInstance : TDevicesResource;

begin
  if (FDevicesInstance=Nil) then
    FDevicesInstance:=CreateDevicesResource;
  Result:=FDevicesInstance;
end;

Function TAndroidenterpriseAPI.CreateDevicesResource : TDevicesResource;

begin
  Result:=CreateDevicesResource(Self);
end;


Function TAndroidenterpriseAPI.CreateDevicesResource(AOwner : TComponent) : TDevicesResource;

begin
  Result:=TDevicesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetEnterprisesInstance : TEnterprisesResource;

begin
  if (FEnterprisesInstance=Nil) then
    FEnterprisesInstance:=CreateEnterprisesResource;
  Result:=FEnterprisesInstance;
end;

Function TAndroidenterpriseAPI.CreateEnterprisesResource : TEnterprisesResource;

begin
  Result:=CreateEnterprisesResource(Self);
end;


Function TAndroidenterpriseAPI.CreateEnterprisesResource(AOwner : TComponent) : TEnterprisesResource;

begin
  Result:=TEnterprisesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetEntitlementsInstance : TEntitlementsResource;

begin
  if (FEntitlementsInstance=Nil) then
    FEntitlementsInstance:=CreateEntitlementsResource;
  Result:=FEntitlementsInstance;
end;

Function TAndroidenterpriseAPI.CreateEntitlementsResource : TEntitlementsResource;

begin
  Result:=CreateEntitlementsResource(Self);
end;


Function TAndroidenterpriseAPI.CreateEntitlementsResource(AOwner : TComponent) : TEntitlementsResource;

begin
  Result:=TEntitlementsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetGrouplicensesInstance : TGrouplicensesResource;

begin
  if (FGrouplicensesInstance=Nil) then
    FGrouplicensesInstance:=CreateGrouplicensesResource;
  Result:=FGrouplicensesInstance;
end;

Function TAndroidenterpriseAPI.CreateGrouplicensesResource : TGrouplicensesResource;

begin
  Result:=CreateGrouplicensesResource(Self);
end;


Function TAndroidenterpriseAPI.CreateGrouplicensesResource(AOwner : TComponent) : TGrouplicensesResource;

begin
  Result:=TGrouplicensesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetGrouplicenseusersInstance : TGrouplicenseusersResource;

begin
  if (FGrouplicenseusersInstance=Nil) then
    FGrouplicenseusersInstance:=CreateGrouplicenseusersResource;
  Result:=FGrouplicenseusersInstance;
end;

Function TAndroidenterpriseAPI.CreateGrouplicenseusersResource : TGrouplicenseusersResource;

begin
  Result:=CreateGrouplicenseusersResource(Self);
end;


Function TAndroidenterpriseAPI.CreateGrouplicenseusersResource(AOwner : TComponent) : TGrouplicenseusersResource;

begin
  Result:=TGrouplicenseusersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetInstallsInstance : TInstallsResource;

begin
  if (FInstallsInstance=Nil) then
    FInstallsInstance:=CreateInstallsResource;
  Result:=FInstallsInstance;
end;

Function TAndroidenterpriseAPI.CreateInstallsResource : TInstallsResource;

begin
  Result:=CreateInstallsResource(Self);
end;


Function TAndroidenterpriseAPI.CreateInstallsResource(AOwner : TComponent) : TInstallsResource;

begin
  Result:=TInstallsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetPermissionsInstance : TPermissionsResource;

begin
  if (FPermissionsInstance=Nil) then
    FPermissionsInstance:=CreatePermissionsResource;
  Result:=FPermissionsInstance;
end;

Function TAndroidenterpriseAPI.CreatePermissionsResource : TPermissionsResource;

begin
  Result:=CreatePermissionsResource(Self);
end;


Function TAndroidenterpriseAPI.CreatePermissionsResource(AOwner : TComponent) : TPermissionsResource;

begin
  Result:=TPermissionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetProductsInstance : TProductsResource;

begin
  if (FProductsInstance=Nil) then
    FProductsInstance:=CreateProductsResource;
  Result:=FProductsInstance;
end;

Function TAndroidenterpriseAPI.CreateProductsResource : TProductsResource;

begin
  Result:=CreateProductsResource(Self);
end;


Function TAndroidenterpriseAPI.CreateProductsResource(AOwner : TComponent) : TProductsResource;

begin
  Result:=TProductsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAndroidenterpriseAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TAndroidenterpriseAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TAndroidenterpriseAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAndroidenterpriseAPI.RegisterAPI;
end.
