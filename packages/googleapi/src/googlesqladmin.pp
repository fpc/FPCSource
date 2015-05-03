unit googlesqladmin;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TAclEntry = class;
  TAclEntryArray = Array of TAclEntry;
  TBackupConfiguration = class;
  TBackupConfigurationArray = Array of TBackupConfiguration;
  TBackupRun = class;
  TBackupRunArray = Array of TBackupRun;
  TBackupRunsListResponse = class;
  TBackupRunsListResponseArray = Array of TBackupRunsListResponse;
  TBackupRunsListResponseitems = class;
  TBackupRunsListResponseitemsArray = Array of TBackupRunsListResponseitems;
  TBinLogCoordinates = class;
  TBinLogCoordinatesArray = Array of TBinLogCoordinates;
  TCloneContext = class;
  TCloneContextArray = Array of TCloneContext;
  TDatabase = class;
  TDatabaseArray = Array of TDatabase;
  TDatabaseFlags = class;
  TDatabaseFlagsArray = Array of TDatabaseFlags;
  TDatabaseInstance = class;
  TDatabaseInstanceArray = Array of TDatabaseInstance;
  TDatabaseInstanceipAddresses = class;
  TDatabaseInstanceipAddressesArray = Array of TDatabaseInstanceipAddresses;
  TDatabaseInstancereplicaNames = class;
  TDatabaseInstancereplicaNamesArray = Array of TDatabaseInstancereplicaNames;
  TDatabasesListResponse = class;
  TDatabasesListResponseArray = Array of TDatabasesListResponse;
  TDatabasesListResponseitems = class;
  TDatabasesListResponseitemsArray = Array of TDatabasesListResponseitems;
  TExportContext = class;
  TExportContextArray = Array of TExportContext;
  TExportContextcsvExportOptions = class;
  TExportContextcsvExportOptionsArray = Array of TExportContextcsvExportOptions;
  TExportContextdatabases = class;
  TExportContextdatabasesArray = Array of TExportContextdatabases;
  TExportContextsqlExportOptions = class;
  TExportContextsqlExportOptionsArray = Array of TExportContextsqlExportOptions;
  TExportContextsqlExportOptionstables = class;
  TExportContextsqlExportOptionstablesArray = Array of TExportContextsqlExportOptionstables;
  TFlag = class;
  TFlagArray = Array of TFlag;
  TFlagallowedStringValues = class;
  TFlagallowedStringValuesArray = Array of TFlagallowedStringValues;
  TFlagappliesTo = class;
  TFlagappliesToArray = Array of TFlagappliesTo;
  TFlagsListResponse = class;
  TFlagsListResponseArray = Array of TFlagsListResponse;
  TFlagsListResponseitems = class;
  TFlagsListResponseitemsArray = Array of TFlagsListResponseitems;
  TImportContext = class;
  TImportContextArray = Array of TImportContext;
  TImportContextcsvImportOptions = class;
  TImportContextcsvImportOptionsArray = Array of TImportContextcsvImportOptions;
  TImportContextcsvImportOptionscolumns = class;
  TImportContextcsvImportOptionscolumnsArray = Array of TImportContextcsvImportOptionscolumns;
  TInstancesCloneRequest = class;
  TInstancesCloneRequestArray = Array of TInstancesCloneRequest;
  TInstancesExportRequest = class;
  TInstancesExportRequestArray = Array of TInstancesExportRequest;
  TInstancesImportRequest = class;
  TInstancesImportRequestArray = Array of TInstancesImportRequest;
  TInstancesListResponse = class;
  TInstancesListResponseArray = Array of TInstancesListResponse;
  TInstancesListResponseitems = class;
  TInstancesListResponseitemsArray = Array of TInstancesListResponseitems;
  TInstancesRestoreBackupRequest = class;
  TInstancesRestoreBackupRequestArray = Array of TInstancesRestoreBackupRequest;
  TIpConfiguration = class;
  TIpConfigurationArray = Array of TIpConfiguration;
  TIpConfigurationauthorizedNetworks = class;
  TIpConfigurationauthorizedNetworksArray = Array of TIpConfigurationauthorizedNetworks;
  TIpMapping = class;
  TIpMappingArray = Array of TIpMapping;
  TLocationPreference = class;
  TLocationPreferenceArray = Array of TLocationPreference;
  TMySqlReplicaConfiguration = class;
  TMySqlReplicaConfigurationArray = Array of TMySqlReplicaConfiguration;
  TOnPremisesConfiguration = class;
  TOnPremisesConfigurationArray = Array of TOnPremisesConfiguration;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TOperationError = class;
  TOperationErrorArray = Array of TOperationError;
  TOperationErrors = class;
  TOperationErrorsArray = Array of TOperationErrors;
  TOperationErrorserrors = class;
  TOperationErrorserrorsArray = Array of TOperationErrorserrors;
  TOperationsListResponse = class;
  TOperationsListResponseArray = Array of TOperationsListResponse;
  TOperationsListResponseitems = class;
  TOperationsListResponseitemsArray = Array of TOperationsListResponseitems;
  TReplicaConfiguration = class;
  TReplicaConfigurationArray = Array of TReplicaConfiguration;
  TRestoreBackupContext = class;
  TRestoreBackupContextArray = Array of TRestoreBackupContext;
  TSettings = class;
  TSettingsArray = Array of TSettings;
  TSettingsauthorizedGaeApplications = class;
  TSettingsauthorizedGaeApplicationsArray = Array of TSettingsauthorizedGaeApplications;
  TSettingsdatabaseFlags = class;
  TSettingsdatabaseFlagsArray = Array of TSettingsdatabaseFlags;
  TSslCert = class;
  TSslCertArray = Array of TSslCert;
  TSslCertDetail = class;
  TSslCertDetailArray = Array of TSslCertDetail;
  TSslCertsInsertRequest = class;
  TSslCertsInsertRequestArray = Array of TSslCertsInsertRequest;
  TSslCertsInsertResponse = class;
  TSslCertsInsertResponseArray = Array of TSslCertsInsertResponse;
  TSslCertsListResponse = class;
  TSslCertsListResponseArray = Array of TSslCertsListResponse;
  TSslCertsListResponseitems = class;
  TSslCertsListResponseitemsArray = Array of TSslCertsListResponseitems;
  TTier = class;
  TTierArray = Array of TTier;
  TTierregion = class;
  TTierregionArray = Array of TTierregion;
  TTiersListResponse = class;
  TTiersListResponseArray = Array of TTiersListResponse;
  TTiersListResponseitems = class;
  TTiersListResponseitemsArray = Array of TTiersListResponseitems;
  TUser = class;
  TUserArray = Array of TUser;
  TUsersListResponse = class;
  TUsersListResponseArray = Array of TUsersListResponse;
  TUsersListResponseitems = class;
  TUsersListResponseitemsArray = Array of TUsersListResponseitems;
  
  { --------------------------------------------------------------------
    TAclEntry
    --------------------------------------------------------------------}
  
  TAclEntry = Class(TGoogleBaseObject)
  Private
    FexpirationTime : TDatetime;
    Fkind : string;
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure SetexpirationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property expirationTime : TDatetime Index 0 Read FexpirationTime Write SetexpirationTime;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property value : string Index 24 Read Fvalue Write Setvalue;
  end;
  TAclEntryClass = Class of TAclEntry;
  
  { --------------------------------------------------------------------
    TBackupConfiguration
    --------------------------------------------------------------------}
  
  TBackupConfiguration = Class(TGoogleBaseObject)
  Private
    FbinaryLogEnabled : boolean;
    Fenabled : boolean;
    Fkind : string;
    FstartTime : string;
  Protected
    //Property setters
    Procedure SetbinaryLogEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setenabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property binaryLogEnabled : boolean Index 0 Read FbinaryLogEnabled Write SetbinaryLogEnabled;
    Property enabled : boolean Index 8 Read Fenabled Write Setenabled;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property startTime : string Index 24 Read FstartTime Write SetstartTime;
  end;
  TBackupConfigurationClass = Class of TBackupConfiguration;
  
  { --------------------------------------------------------------------
    TBackupRun
    --------------------------------------------------------------------}
  
  TBackupRun = Class(TGoogleBaseObject)
  Private
    FendTime : TDatetime;
    FenqueuedTime : TDatetime;
    Ferror : TOperationError;
    Fid : string;
    Finstance : string;
    Fkind : string;
    FselfLink : string;
    FstartTime : TDatetime;
    Fstatus : string;
    FwindowStartTime : TDatetime;
  Protected
    //Property setters
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetenqueuedTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationError); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetwindowStartTime(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property endTime : TDatetime Index 0 Read FendTime Write SetendTime;
    Property enqueuedTime : TDatetime Index 8 Read FenqueuedTime Write SetenqueuedTime;
    Property error : TOperationError Index 16 Read Ferror Write Seterror;
    Property id : string Index 24 Read Fid Write Setid;
    Property instance : string Index 32 Read Finstance Write Setinstance;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property startTime : TDatetime Index 56 Read FstartTime Write SetstartTime;
    Property status : string Index 64 Read Fstatus Write Setstatus;
    Property windowStartTime : TDatetime Index 72 Read FwindowStartTime Write SetwindowStartTime;
  end;
  TBackupRunClass = Class of TBackupRun;
  
  { --------------------------------------------------------------------
    TBackupRunsListResponse
    --------------------------------------------------------------------}
  
  TBackupRunsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TBackupRunsListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TBackupRunsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TBackupRunsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TBackupRunsListResponseClass = Class of TBackupRunsListResponse;
  
  { --------------------------------------------------------------------
    TBackupRunsListResponseitems
    --------------------------------------------------------------------}
  
  TBackupRunsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBackupRunsListResponseitemsClass = Class of TBackupRunsListResponseitems;
  
  { --------------------------------------------------------------------
    TBinLogCoordinates
    --------------------------------------------------------------------}
  
  TBinLogCoordinates = Class(TGoogleBaseObject)
  Private
    FbinLogFileName : string;
    FbinLogPosition : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetbinLogFileName(AIndex : Integer; AValue : string); virtual;
    Procedure SetbinLogPosition(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property binLogFileName : string Index 0 Read FbinLogFileName Write SetbinLogFileName;
    Property binLogPosition : string Index 8 Read FbinLogPosition Write SetbinLogPosition;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TBinLogCoordinatesClass = Class of TBinLogCoordinates;
  
  { --------------------------------------------------------------------
    TCloneContext
    --------------------------------------------------------------------}
  
  TCloneContext = Class(TGoogleBaseObject)
  Private
    FbinLogCoordinates : TBinLogCoordinates;
    FdestinationInstanceName : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetbinLogCoordinates(AIndex : Integer; AValue : TBinLogCoordinates); virtual;
    Procedure SetdestinationInstanceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property binLogCoordinates : TBinLogCoordinates Index 0 Read FbinLogCoordinates Write SetbinLogCoordinates;
    Property destinationInstanceName : string Index 8 Read FdestinationInstanceName Write SetdestinationInstanceName;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TCloneContextClass = Class of TCloneContext;
  
  { --------------------------------------------------------------------
    TDatabase
    --------------------------------------------------------------------}
  
  TDatabase = Class(TGoogleBaseObject)
  Private
    Fcharset : string;
    Fcollation : string;
    Fetag : string;
    Finstance : string;
    Fkind : string;
    Fname : string;
    Fproject : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setcharset(AIndex : Integer; AValue : string); virtual;
    Procedure Setcollation(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setproject(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property charset : string Index 0 Read Fcharset Write Setcharset;
    Property collation : string Index 8 Read Fcollation Write Setcollation;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property instance : string Index 24 Read Finstance Write Setinstance;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property project : string Index 48 Read Fproject Write Setproject;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
  end;
  TDatabaseClass = Class of TDatabase;
  
  { --------------------------------------------------------------------
    TDatabaseFlags
    --------------------------------------------------------------------}
  
  TDatabaseFlags = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TDatabaseFlagsClass = Class of TDatabaseFlags;
  
  { --------------------------------------------------------------------
    TDatabaseInstance
    --------------------------------------------------------------------}
  
  TDatabaseInstance = Class(TGoogleBaseObject)
  Private
    FcurrentDiskSize : string;
    FdatabaseVersion : string;
    Fetag : string;
    FinstanceType : string;
    FipAddresses : TDatabaseInstanceipAddresses;
    Fipv6Address : string;
    Fkind : string;
    FmasterInstanceName : string;
    FmaxDiskSize : string;
    Fname : string;
    FonPremisesConfiguration : TOnPremisesConfiguration;
    Fproject : string;
    Fregion : string;
    FreplicaConfiguration : TReplicaConfiguration;
    FreplicaNames : TDatabaseInstancereplicaNames;
    FselfLink : string;
    FserverCaCert : TSslCert;
    FserviceAccountEmailAddress : string;
    Fsettings : TSettings;
    Fstate : string;
  Protected
    //Property setters
    Procedure SetcurrentDiskSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetdatabaseVersion(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceType(AIndex : Integer; AValue : string); virtual;
    Procedure SetipAddresses(AIndex : Integer; AValue : TDatabaseInstanceipAddresses); virtual;
    Procedure Setipv6Address(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmasterInstanceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxDiskSize(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetonPremisesConfiguration(AIndex : Integer; AValue : TOnPremisesConfiguration); virtual;
    Procedure Setproject(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetreplicaConfiguration(AIndex : Integer; AValue : TReplicaConfiguration); virtual;
    Procedure SetreplicaNames(AIndex : Integer; AValue : TDatabaseInstancereplicaNames); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetserverCaCert(AIndex : Integer; AValue : TSslCert); virtual;
    Procedure SetserviceAccountEmailAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setsettings(AIndex : Integer; AValue : TSettings); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currentDiskSize : string Index 0 Read FcurrentDiskSize Write SetcurrentDiskSize;
    Property databaseVersion : string Index 8 Read FdatabaseVersion Write SetdatabaseVersion;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property instanceType : string Index 24 Read FinstanceType Write SetinstanceType;
    Property ipAddresses : TDatabaseInstanceipAddresses Index 32 Read FipAddresses Write SetipAddresses;
    Property ipv6Address : string Index 40 Read Fipv6Address Write Setipv6Address;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property masterInstanceName : string Index 56 Read FmasterInstanceName Write SetmasterInstanceName;
    Property maxDiskSize : string Index 64 Read FmaxDiskSize Write SetmaxDiskSize;
    Property name : string Index 72 Read Fname Write Setname;
    Property onPremisesConfiguration : TOnPremisesConfiguration Index 80 Read FonPremisesConfiguration Write SetonPremisesConfiguration;
    Property project : string Index 88 Read Fproject Write Setproject;
    Property region : string Index 96 Read Fregion Write Setregion;
    Property replicaConfiguration : TReplicaConfiguration Index 104 Read FreplicaConfiguration Write SetreplicaConfiguration;
    Property replicaNames : TDatabaseInstancereplicaNames Index 112 Read FreplicaNames Write SetreplicaNames;
    Property selfLink : string Index 120 Read FselfLink Write SetselfLink;
    Property serverCaCert : TSslCert Index 128 Read FserverCaCert Write SetserverCaCert;
    Property serviceAccountEmailAddress : string Index 136 Read FserviceAccountEmailAddress Write SetserviceAccountEmailAddress;
    Property settings : TSettings Index 144 Read Fsettings Write Setsettings;
    Property state : string Index 152 Read Fstate Write Setstate;
  end;
  TDatabaseInstanceClass = Class of TDatabaseInstance;
  
  { --------------------------------------------------------------------
    TDatabaseInstanceipAddresses
    --------------------------------------------------------------------}
  
  TDatabaseInstanceipAddresses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatabaseInstanceipAddressesClass = Class of TDatabaseInstanceipAddresses;
  
  { --------------------------------------------------------------------
    TDatabaseInstancereplicaNames
    --------------------------------------------------------------------}
  
  TDatabaseInstancereplicaNames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatabaseInstancereplicaNamesClass = Class of TDatabaseInstancereplicaNames;
  
  { --------------------------------------------------------------------
    TDatabasesListResponse
    --------------------------------------------------------------------}
  
  TDatabasesListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TDatabasesListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TDatabasesListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TDatabasesListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TDatabasesListResponseClass = Class of TDatabasesListResponse;
  
  { --------------------------------------------------------------------
    TDatabasesListResponseitems
    --------------------------------------------------------------------}
  
  TDatabasesListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatabasesListResponseitemsClass = Class of TDatabasesListResponseitems;
  
  { --------------------------------------------------------------------
    TExportContext
    --------------------------------------------------------------------}
  
  TExportContext = Class(TGoogleBaseObject)
  Private
    FcsvExportOptions : TExportContextcsvExportOptions;
    Fdatabases : TExportContextdatabases;
    FfileType : string;
    Fkind : string;
    FsqlExportOptions : TExportContextsqlExportOptions;
    Furi : string;
  Protected
    //Property setters
    Procedure SetcsvExportOptions(AIndex : Integer; AValue : TExportContextcsvExportOptions); virtual;
    Procedure Setdatabases(AIndex : Integer; AValue : TExportContextdatabases); virtual;
    Procedure SetfileType(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetsqlExportOptions(AIndex : Integer; AValue : TExportContextsqlExportOptions); virtual;
    Procedure Seturi(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property csvExportOptions : TExportContextcsvExportOptions Index 0 Read FcsvExportOptions Write SetcsvExportOptions;
    Property databases : TExportContextdatabases Index 8 Read Fdatabases Write Setdatabases;
    Property fileType : string Index 16 Read FfileType Write SetfileType;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property sqlExportOptions : TExportContextsqlExportOptions Index 32 Read FsqlExportOptions Write SetsqlExportOptions;
    Property uri : string Index 40 Read Furi Write Seturi;
  end;
  TExportContextClass = Class of TExportContext;
  
  { --------------------------------------------------------------------
    TExportContextcsvExportOptions
    --------------------------------------------------------------------}
  
  TExportContextcsvExportOptions = Class(TGoogleBaseObject)
  Private
    FselectQuery : string;
  Protected
    //Property setters
    Procedure SetselectQuery(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property selectQuery : string Index 0 Read FselectQuery Write SetselectQuery;
  end;
  TExportContextcsvExportOptionsClass = Class of TExportContextcsvExportOptions;
  
  { --------------------------------------------------------------------
    TExportContextdatabases
    --------------------------------------------------------------------}
  
  TExportContextdatabases = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExportContextdatabasesClass = Class of TExportContextdatabases;
  
  { --------------------------------------------------------------------
    TExportContextsqlExportOptions
    --------------------------------------------------------------------}
  
  TExportContextsqlExportOptions = Class(TGoogleBaseObject)
  Private
    Ftables : TExportContextsqlExportOptionstables;
  Protected
    //Property setters
    Procedure Settables(AIndex : Integer; AValue : TExportContextsqlExportOptionstables); virtual;
  Public
  Published
    Property tables : TExportContextsqlExportOptionstables Index 0 Read Ftables Write Settables;
  end;
  TExportContextsqlExportOptionsClass = Class of TExportContextsqlExportOptions;
  
  { --------------------------------------------------------------------
    TExportContextsqlExportOptionstables
    --------------------------------------------------------------------}
  
  TExportContextsqlExportOptionstables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExportContextsqlExportOptionstablesClass = Class of TExportContextsqlExportOptionstables;
  
  { --------------------------------------------------------------------
    TFlag
    --------------------------------------------------------------------}
  
  TFlag = Class(TGoogleBaseObject)
  Private
    FallowedStringValues : TFlagallowedStringValues;
    FappliesTo : TFlagappliesTo;
    Fkind : string;
    FmaxValue : string;
    FminValue : string;
    Fname : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetallowedStringValues(AIndex : Integer; AValue : TFlagallowedStringValues); virtual;
    Procedure SetappliesTo(AIndex : Integer; AValue : TFlagappliesTo); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetminValue(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowedStringValues : TFlagallowedStringValues Index 0 Read FallowedStringValues Write SetallowedStringValues;
    Property appliesTo : TFlagappliesTo Index 8 Read FappliesTo Write SetappliesTo;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property maxValue : string Index 24 Read FmaxValue Write SetmaxValue;
    Property minValue : string Index 32 Read FminValue Write SetminValue;
    Property name : string Index 40 Read Fname Write Setname;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TFlagClass = Class of TFlag;
  
  { --------------------------------------------------------------------
    TFlagallowedStringValues
    --------------------------------------------------------------------}
  
  TFlagallowedStringValues = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFlagallowedStringValuesClass = Class of TFlagallowedStringValues;
  
  { --------------------------------------------------------------------
    TFlagappliesTo
    --------------------------------------------------------------------}
  
  TFlagappliesTo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFlagappliesToClass = Class of TFlagappliesTo;
  
  { --------------------------------------------------------------------
    TFlagsListResponse
    --------------------------------------------------------------------}
  
  TFlagsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TFlagsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TFlagsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TFlagsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TFlagsListResponseClass = Class of TFlagsListResponse;
  
  { --------------------------------------------------------------------
    TFlagsListResponseitems
    --------------------------------------------------------------------}
  
  TFlagsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFlagsListResponseitemsClass = Class of TFlagsListResponseitems;
  
  { --------------------------------------------------------------------
    TImportContext
    --------------------------------------------------------------------}
  
  TImportContext = Class(TGoogleBaseObject)
  Private
    FcsvImportOptions : TImportContextcsvImportOptions;
    Fdatabase : string;
    FfileType : string;
    Fkind : string;
    Furi : string;
  Protected
    //Property setters
    Procedure SetcsvImportOptions(AIndex : Integer; AValue : TImportContextcsvImportOptions); virtual;
    Procedure Setdatabase(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileType(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Seturi(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property csvImportOptions : TImportContextcsvImportOptions Index 0 Read FcsvImportOptions Write SetcsvImportOptions;
    Property database : string Index 8 Read Fdatabase Write Setdatabase;
    Property fileType : string Index 16 Read FfileType Write SetfileType;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property uri : string Index 32 Read Furi Write Seturi;
  end;
  TImportContextClass = Class of TImportContext;
  
  { --------------------------------------------------------------------
    TImportContextcsvImportOptions
    --------------------------------------------------------------------}
  
  TImportContextcsvImportOptions = Class(TGoogleBaseObject)
  Private
    Fcolumns : TImportContextcsvImportOptionscolumns;
    Ftable : string;
  Protected
    //Property setters
    Procedure Setcolumns(AIndex : Integer; AValue : TImportContextcsvImportOptionscolumns); virtual;
    Procedure Settable(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property columns : TImportContextcsvImportOptionscolumns Index 0 Read Fcolumns Write Setcolumns;
    Property table : string Index 8 Read Ftable Write Settable;
  end;
  TImportContextcsvImportOptionsClass = Class of TImportContextcsvImportOptions;
  
  { --------------------------------------------------------------------
    TImportContextcsvImportOptionscolumns
    --------------------------------------------------------------------}
  
  TImportContextcsvImportOptionscolumns = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImportContextcsvImportOptionscolumnsClass = Class of TImportContextcsvImportOptionscolumns;
  
  { --------------------------------------------------------------------
    TInstancesCloneRequest
    --------------------------------------------------------------------}
  
  TInstancesCloneRequest = Class(TGoogleBaseObject)
  Private
    FcloneContext : TCloneContext;
  Protected
    //Property setters
    Procedure SetcloneContext(AIndex : Integer; AValue : TCloneContext); virtual;
  Public
  Published
    Property cloneContext : TCloneContext Index 0 Read FcloneContext Write SetcloneContext;
  end;
  TInstancesCloneRequestClass = Class of TInstancesCloneRequest;
  
  { --------------------------------------------------------------------
    TInstancesExportRequest
    --------------------------------------------------------------------}
  
  TInstancesExportRequest = Class(TGoogleBaseObject)
  Private
    FexportContext : TExportContext;
  Protected
    //Property setters
    Procedure SetexportContext(AIndex : Integer; AValue : TExportContext); virtual;
  Public
  Published
    Property exportContext : TExportContext Index 0 Read FexportContext Write SetexportContext;
  end;
  TInstancesExportRequestClass = Class of TInstancesExportRequest;
  
  { --------------------------------------------------------------------
    TInstancesImportRequest
    --------------------------------------------------------------------}
  
  TInstancesImportRequest = Class(TGoogleBaseObject)
  Private
    FimportContext : TImportContext;
  Protected
    //Property setters
    Procedure SetimportContext(AIndex : Integer; AValue : TImportContext); virtual;
  Public
  Published
    Property importContext : TImportContext Index 0 Read FimportContext Write SetimportContext;
  end;
  TInstancesImportRequestClass = Class of TInstancesImportRequest;
  
  { --------------------------------------------------------------------
    TInstancesListResponse
    --------------------------------------------------------------------}
  
  TInstancesListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TInstancesListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TInstancesListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TInstancesListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TInstancesListResponseClass = Class of TInstancesListResponse;
  
  { --------------------------------------------------------------------
    TInstancesListResponseitems
    --------------------------------------------------------------------}
  
  TInstancesListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInstancesListResponseitemsClass = Class of TInstancesListResponseitems;
  
  { --------------------------------------------------------------------
    TInstancesRestoreBackupRequest
    --------------------------------------------------------------------}
  
  TInstancesRestoreBackupRequest = Class(TGoogleBaseObject)
  Private
    FrestoreBackupContext : TRestoreBackupContext;
  Protected
    //Property setters
    Procedure SetrestoreBackupContext(AIndex : Integer; AValue : TRestoreBackupContext); virtual;
  Public
  Published
    Property restoreBackupContext : TRestoreBackupContext Index 0 Read FrestoreBackupContext Write SetrestoreBackupContext;
  end;
  TInstancesRestoreBackupRequestClass = Class of TInstancesRestoreBackupRequest;
  
  { --------------------------------------------------------------------
    TIpConfiguration
    --------------------------------------------------------------------}
  
  TIpConfiguration = Class(TGoogleBaseObject)
  Private
    FauthorizedNetworks : TIpConfigurationauthorizedNetworks;
    Fipv4Enabled : boolean;
    FrequireSsl : boolean;
  Protected
    //Property setters
    Procedure SetauthorizedNetworks(AIndex : Integer; AValue : TIpConfigurationauthorizedNetworks); virtual;
    Procedure Setipv4Enabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrequireSsl(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property authorizedNetworks : TIpConfigurationauthorizedNetworks Index 0 Read FauthorizedNetworks Write SetauthorizedNetworks;
    Property ipv4Enabled : boolean Index 8 Read Fipv4Enabled Write Setipv4Enabled;
    Property requireSsl : boolean Index 16 Read FrequireSsl Write SetrequireSsl;
  end;
  TIpConfigurationClass = Class of TIpConfiguration;
  
  { --------------------------------------------------------------------
    TIpConfigurationauthorizedNetworks
    --------------------------------------------------------------------}
  
  TIpConfigurationauthorizedNetworks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TIpConfigurationauthorizedNetworksClass = Class of TIpConfigurationauthorizedNetworks;
  
  { --------------------------------------------------------------------
    TIpMapping
    --------------------------------------------------------------------}
  
  TIpMapping = Class(TGoogleBaseObject)
  Private
    FipAddress : string;
    FtimeToRetire : TDatetime;
  Protected
    //Property setters
    Procedure SetipAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeToRetire(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property ipAddress : string Index 0 Read FipAddress Write SetipAddress;
    Property timeToRetire : TDatetime Index 8 Read FtimeToRetire Write SettimeToRetire;
  end;
  TIpMappingClass = Class of TIpMapping;
  
  { --------------------------------------------------------------------
    TLocationPreference
    --------------------------------------------------------------------}
  
  TLocationPreference = Class(TGoogleBaseObject)
  Private
    FfollowGaeApplication : string;
    Fkind : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetfollowGaeApplication(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property followGaeApplication : string Index 0 Read FfollowGaeApplication Write SetfollowGaeApplication;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property zone : string Index 16 Read Fzone Write Setzone;
  end;
  TLocationPreferenceClass = Class of TLocationPreference;
  
  { --------------------------------------------------------------------
    TMySqlReplicaConfiguration
    --------------------------------------------------------------------}
  
  TMySqlReplicaConfiguration = Class(TGoogleBaseObject)
  Private
    FcaCertificate : string;
    FclientCertificate : string;
    FclientKey : string;
    FconnectRetryInterval : integer;
    FdumpFilePath : string;
    Fkind : string;
    FmasterHeartbeatPeriod : string;
    Fpassword : string;
    FsslCipher : string;
    Fusername : string;
    FverifyServerCertificate : boolean;
  Protected
    //Property setters
    Procedure SetcaCertificate(AIndex : Integer; AValue : string); virtual;
    Procedure SetclientCertificate(AIndex : Integer; AValue : string); virtual;
    Procedure SetclientKey(AIndex : Integer; AValue : string); virtual;
    Procedure SetconnectRetryInterval(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdumpFilePath(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmasterHeartbeatPeriod(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure SetsslCipher(AIndex : Integer; AValue : string); virtual;
    Procedure Setusername(AIndex : Integer; AValue : string); virtual;
    Procedure SetverifyServerCertificate(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property caCertificate : string Index 0 Read FcaCertificate Write SetcaCertificate;
    Property clientCertificate : string Index 8 Read FclientCertificate Write SetclientCertificate;
    Property clientKey : string Index 16 Read FclientKey Write SetclientKey;
    Property connectRetryInterval : integer Index 24 Read FconnectRetryInterval Write SetconnectRetryInterval;
    Property dumpFilePath : string Index 32 Read FdumpFilePath Write SetdumpFilePath;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property masterHeartbeatPeriod : string Index 48 Read FmasterHeartbeatPeriod Write SetmasterHeartbeatPeriod;
    Property password : string Index 56 Read Fpassword Write Setpassword;
    Property sslCipher : string Index 64 Read FsslCipher Write SetsslCipher;
    Property username : string Index 72 Read Fusername Write Setusername;
    Property verifyServerCertificate : boolean Index 80 Read FverifyServerCertificate Write SetverifyServerCertificate;
  end;
  TMySqlReplicaConfigurationClass = Class of TMySqlReplicaConfiguration;
  
  { --------------------------------------------------------------------
    TOnPremisesConfiguration
    --------------------------------------------------------------------}
  
  TOnPremisesConfiguration = Class(TGoogleBaseObject)
  Private
    FhostPort : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SethostPort(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property hostPort : string Index 0 Read FhostPort Write SethostPort;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TOnPremisesConfigurationClass = Class of TOnPremisesConfiguration;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FendTime : TDatetime;
    Ferror : TOperationErrors;
    FexportContext : TExportContext;
    FimportContext : TImportContext;
    FinsertTime : TDatetime;
    Fkind : string;
    Fname : string;
    FoperationType : string;
    FselfLink : string;
    FstartTime : TDatetime;
    Fstatus : string;
    FtargetId : string;
    FtargetLink : string;
    FtargetProject : string;
    Fuser : string;
  Protected
    //Property setters
    Procedure SetendTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationErrors); virtual;
    Procedure SetexportContext(AIndex : Integer; AValue : TExportContext); virtual;
    Procedure SetimportContext(AIndex : Integer; AValue : TImportContext); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetProject(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endTime : TDatetime Index 0 Read FendTime Write SetendTime;
    Property error : TOperationErrors Index 8 Read Ferror Write Seterror;
    Property exportContext : TExportContext Index 16 Read FexportContext Write SetexportContext;
    Property importContext : TImportContext Index 24 Read FimportContext Write SetimportContext;
    Property insertTime : TDatetime Index 32 Read FinsertTime Write SetinsertTime;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property name : string Index 48 Read Fname Write Setname;
    Property operationType : string Index 56 Read FoperationType Write SetoperationType;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property startTime : TDatetime Index 72 Read FstartTime Write SetstartTime;
    Property status : string Index 80 Read Fstatus Write Setstatus;
    Property targetId : string Index 88 Read FtargetId Write SettargetId;
    Property targetLink : string Index 96 Read FtargetLink Write SettargetLink;
    Property targetProject : string Index 104 Read FtargetProject Write SettargetProject;
    Property user : string Index 112 Read Fuser Write Setuser;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationError
    --------------------------------------------------------------------}
  
  TOperationError = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fkind : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationErrorClass = Class of TOperationError;
  
  { --------------------------------------------------------------------
    TOperationErrors
    --------------------------------------------------------------------}
  
  TOperationErrors = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationErrorserrors;
    Fkind : string;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationErrorserrors); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property errors : TOperationErrorserrors Index 0 Read Ferrors Write Seterrors;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TOperationErrorsClass = Class of TOperationErrors;
  
  { --------------------------------------------------------------------
    TOperationErrorserrors
    --------------------------------------------------------------------}
  
  TOperationErrorserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationErrorserrorsClass = Class of TOperationErrorserrors;
  
  { --------------------------------------------------------------------
    TOperationsListResponse
    --------------------------------------------------------------------}
  
  TOperationsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TOperationsListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TOperationsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TOperationsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TOperationsListResponseClass = Class of TOperationsListResponse;
  
  { --------------------------------------------------------------------
    TOperationsListResponseitems
    --------------------------------------------------------------------}
  
  TOperationsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationsListResponseitemsClass = Class of TOperationsListResponseitems;
  
  { --------------------------------------------------------------------
    TReplicaConfiguration
    --------------------------------------------------------------------}
  
  TReplicaConfiguration = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    FmysqlReplicaConfiguration : TMySqlReplicaConfiguration;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmysqlReplicaConfiguration(AIndex : Integer; AValue : TMySqlReplicaConfiguration); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property mysqlReplicaConfiguration : TMySqlReplicaConfiguration Index 8 Read FmysqlReplicaConfiguration Write SetmysqlReplicaConfiguration;
  end;
  TReplicaConfigurationClass = Class of TReplicaConfiguration;
  
  { --------------------------------------------------------------------
    TRestoreBackupContext
    --------------------------------------------------------------------}
  
  TRestoreBackupContext = Class(TGoogleBaseObject)
  Private
    FbackupRunId : string;
    FinstanceId : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetbackupRunId(AIndex : Integer; AValue : string); virtual;
    Procedure SetinstanceId(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property backupRunId : string Index 0 Read FbackupRunId Write SetbackupRunId;
    Property instanceId : string Index 8 Read FinstanceId Write SetinstanceId;
    Property kind : string Index 16 Read Fkind Write Setkind;
  end;
  TRestoreBackupContextClass = Class of TRestoreBackupContext;
  
  { --------------------------------------------------------------------
    TSettings
    --------------------------------------------------------------------}
  
  TSettings = Class(TGoogleBaseObject)
  Private
    FactivationPolicy : string;
    FauthorizedGaeApplications : TSettingsauthorizedGaeApplications;
    FbackupConfiguration : TBackupConfiguration;
    FcrashSafeReplicationEnabled : boolean;
    FdatabaseFlags : TSettingsdatabaseFlags;
    FdatabaseReplicationEnabled : boolean;
    FipConfiguration : TIpConfiguration;
    Fkind : string;
    FlocationPreference : TLocationPreference;
    FpricingPlan : string;
    FreplicationType : string;
    FsettingsVersion : string;
    Ftier : string;
  Protected
    //Property setters
    Procedure SetactivationPolicy(AIndex : Integer; AValue : string); virtual;
    Procedure SetauthorizedGaeApplications(AIndex : Integer; AValue : TSettingsauthorizedGaeApplications); virtual;
    Procedure SetbackupConfiguration(AIndex : Integer; AValue : TBackupConfiguration); virtual;
    Procedure SetcrashSafeReplicationEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdatabaseFlags(AIndex : Integer; AValue : TSettingsdatabaseFlags); virtual;
    Procedure SetdatabaseReplicationEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetipConfiguration(AIndex : Integer; AValue : TIpConfiguration); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlocationPreference(AIndex : Integer; AValue : TLocationPreference); virtual;
    Procedure SetpricingPlan(AIndex : Integer; AValue : string); virtual;
    Procedure SetreplicationType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsettingsVersion(AIndex : Integer; AValue : string); virtual;
    Procedure Settier(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property activationPolicy : string Index 0 Read FactivationPolicy Write SetactivationPolicy;
    Property authorizedGaeApplications : TSettingsauthorizedGaeApplications Index 8 Read FauthorizedGaeApplications Write SetauthorizedGaeApplications;
    Property backupConfiguration : TBackupConfiguration Index 16 Read FbackupConfiguration Write SetbackupConfiguration;
    Property crashSafeReplicationEnabled : boolean Index 24 Read FcrashSafeReplicationEnabled Write SetcrashSafeReplicationEnabled;
    Property databaseFlags : TSettingsdatabaseFlags Index 32 Read FdatabaseFlags Write SetdatabaseFlags;
    Property databaseReplicationEnabled : boolean Index 40 Read FdatabaseReplicationEnabled Write SetdatabaseReplicationEnabled;
    Property ipConfiguration : TIpConfiguration Index 48 Read FipConfiguration Write SetipConfiguration;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property locationPreference : TLocationPreference Index 64 Read FlocationPreference Write SetlocationPreference;
    Property pricingPlan : string Index 72 Read FpricingPlan Write SetpricingPlan;
    Property replicationType : string Index 80 Read FreplicationType Write SetreplicationType;
    Property settingsVersion : string Index 88 Read FsettingsVersion Write SetsettingsVersion;
    Property tier : string Index 96 Read Ftier Write Settier;
  end;
  TSettingsClass = Class of TSettings;
  
  { --------------------------------------------------------------------
    TSettingsauthorizedGaeApplications
    --------------------------------------------------------------------}
  
  TSettingsauthorizedGaeApplications = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSettingsauthorizedGaeApplicationsClass = Class of TSettingsauthorizedGaeApplications;
  
  { --------------------------------------------------------------------
    TSettingsdatabaseFlags
    --------------------------------------------------------------------}
  
  TSettingsdatabaseFlags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSettingsdatabaseFlagsClass = Class of TSettingsdatabaseFlags;
  
  { --------------------------------------------------------------------
    TSslCert
    --------------------------------------------------------------------}
  
  TSslCert = Class(TGoogleBaseObject)
  Private
    Fcert : string;
    FcertSerialNumber : string;
    FcommonName : string;
    FcreateTime : TDatetime;
    FexpirationTime : TDatetime;
    Finstance : string;
    Fkind : string;
    FselfLink : string;
    Fsha1Fingerprint : string;
  Protected
    //Property setters
    Procedure Setcert(AIndex : Integer; AValue : string); virtual;
    Procedure SetcertSerialNumber(AIndex : Integer; AValue : string); virtual;
    Procedure SetcommonName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreateTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetexpirationTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setsha1Fingerprint(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property cert : string Index 0 Read Fcert Write Setcert;
    Property certSerialNumber : string Index 8 Read FcertSerialNumber Write SetcertSerialNumber;
    Property commonName : string Index 16 Read FcommonName Write SetcommonName;
    Property createTime : TDatetime Index 24 Read FcreateTime Write SetcreateTime;
    Property expirationTime : TDatetime Index 32 Read FexpirationTime Write SetexpirationTime;
    Property instance : string Index 40 Read Finstance Write Setinstance;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property selfLink : string Index 56 Read FselfLink Write SetselfLink;
    Property sha1Fingerprint : string Index 64 Read Fsha1Fingerprint Write Setsha1Fingerprint;
  end;
  TSslCertClass = Class of TSslCert;
  
  { --------------------------------------------------------------------
    TSslCertDetail
    --------------------------------------------------------------------}
  
  TSslCertDetail = Class(TGoogleBaseObject)
  Private
    FcertInfo : TSslCert;
    FcertPrivateKey : string;
  Protected
    //Property setters
    Procedure SetcertInfo(AIndex : Integer; AValue : TSslCert); virtual;
    Procedure SetcertPrivateKey(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property certInfo : TSslCert Index 0 Read FcertInfo Write SetcertInfo;
    Property certPrivateKey : string Index 8 Read FcertPrivateKey Write SetcertPrivateKey;
  end;
  TSslCertDetailClass = Class of TSslCertDetail;
  
  { --------------------------------------------------------------------
    TSslCertsInsertRequest
    --------------------------------------------------------------------}
  
  TSslCertsInsertRequest = Class(TGoogleBaseObject)
  Private
    FcommonName : string;
  Protected
    //Property setters
    Procedure SetcommonName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property commonName : string Index 0 Read FcommonName Write SetcommonName;
  end;
  TSslCertsInsertRequestClass = Class of TSslCertsInsertRequest;
  
  { --------------------------------------------------------------------
    TSslCertsInsertResponse
    --------------------------------------------------------------------}
  
  TSslCertsInsertResponse = Class(TGoogleBaseObject)
  Private
    FclientCert : TSslCertDetail;
    Fkind : string;
    FserverCaCert : TSslCert;
  Protected
    //Property setters
    Procedure SetclientCert(AIndex : Integer; AValue : TSslCertDetail); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetserverCaCert(AIndex : Integer; AValue : TSslCert); virtual;
  Public
  Published
    Property clientCert : TSslCertDetail Index 0 Read FclientCert Write SetclientCert;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property serverCaCert : TSslCert Index 16 Read FserverCaCert Write SetserverCaCert;
  end;
  TSslCertsInsertResponseClass = Class of TSslCertsInsertResponse;
  
  { --------------------------------------------------------------------
    TSslCertsListResponse
    --------------------------------------------------------------------}
  
  TSslCertsListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSslCertsListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSslCertsListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TSslCertsListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TSslCertsListResponseClass = Class of TSslCertsListResponse;
  
  { --------------------------------------------------------------------
    TSslCertsListResponseitems
    --------------------------------------------------------------------}
  
  TSslCertsListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSslCertsListResponseitemsClass = Class of TSslCertsListResponseitems;
  
  { --------------------------------------------------------------------
    TTier
    --------------------------------------------------------------------}
  
  TTier = Class(TGoogleBaseObject)
  Private
    FDiskQuota : string;
    FRAM : string;
    Fkind : string;
    Fregion : TTierregion;
    Ftier : string;
  Protected
    //Property setters
    Procedure SetDiskQuota(AIndex : Integer; AValue : string); virtual;
    Procedure SetRAM(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : TTierregion); virtual;
    Procedure Settier(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property DiskQuota : string Index 0 Read FDiskQuota Write SetDiskQuota;
    Property RAM : string Index 8 Read FRAM Write SetRAM;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property region : TTierregion Index 24 Read Fregion Write Setregion;
    Property tier : string Index 32 Read Ftier Write Settier;
  end;
  TTierClass = Class of TTier;
  
  { --------------------------------------------------------------------
    TTierregion
    --------------------------------------------------------------------}
  
  TTierregion = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTierregionClass = Class of TTierregion;
  
  { --------------------------------------------------------------------
    TTiersListResponse
    --------------------------------------------------------------------}
  
  TTiersListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TTiersListResponseitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TTiersListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TTiersListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TTiersListResponseClass = Class of TTiersListResponse;
  
  { --------------------------------------------------------------------
    TTiersListResponseitems
    --------------------------------------------------------------------}
  
  TTiersListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTiersListResponseitemsClass = Class of TTiersListResponseitems;
  
  { --------------------------------------------------------------------
    TUser
    --------------------------------------------------------------------}
  
  TUser = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fhost : string;
    Finstance : string;
    Fkind : string;
    Fname : string;
    Fpassword : string;
    Fproject : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Sethost(AIndex : Integer; AValue : string); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure Setproject(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property host : string Index 8 Read Fhost Write Sethost;
    Property instance : string Index 16 Read Finstance Write Setinstance;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property name : string Index 32 Read Fname Write Setname;
    Property password : string Index 40 Read Fpassword Write Setpassword;
    Property project : string Index 48 Read Fproject Write Setproject;
  end;
  TUserClass = Class of TUser;
  
  { --------------------------------------------------------------------
    TUsersListResponse
    --------------------------------------------------------------------}
  
  TUsersListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TUsersListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUsersListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TUsersListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TUsersListResponseClass = Class of TUsersListResponse;
  
  { --------------------------------------------------------------------
    TUsersListResponseitems
    --------------------------------------------------------------------}
  
  TUsersListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUsersListResponseitemsClass = Class of TUsersListResponseitems;
  
  { --------------------------------------------------------------------
    TBackupRunsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBackupRunsResource, method List
  
  TBackupRunsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TBackupRunsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(id: string; instance: string; project: string) : TBackupRun;
    Function List(instance: string; project: string; AQuery : string  = '') : TBackupRunsListResponse;
    Function List(instance: string; project: string; AQuery : TBackupRunslistOptions) : TBackupRunsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDatabasesResource
    --------------------------------------------------------------------}
  
  TDatabasesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(database: string; instance: string; project: string) : TOperation;
    Function Get(database: string; instance: string; project: string) : TDatabase;
    Function Insert(instance: string; project: string; aDatabase : TDatabase) : TOperation;
    Function List(instance: string; project: string) : TDatabasesListResponse;
    Function Patch(database: string; instance: string; project: string; aDatabase : TDatabase) : TOperation;
    Function Update(database: string; instance: string; project: string; aDatabase : TDatabase) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TFlagsResource
    --------------------------------------------------------------------}
  
  TFlagsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TFlagsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TInstancesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInstancesResource, method List
  
  TInstancesListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TInstancesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Clone(instance: string; project: string; aInstancesCloneRequest : TInstancesCloneRequest) : TOperation;
    Function Delete(instance: string; project: string) : TOperation;
    Function Export(instance: string; project: string; aInstancesExportRequest : TInstancesExportRequest) : TOperation;
    Function Get(instance: string; project: string) : TDatabaseInstance;
    Function Import(instance: string; project: string; aInstancesImportRequest : TInstancesImportRequest) : TOperation;
    Function Insert(project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;
    Function List(project: string; AQuery : string  = '') : TInstancesListResponse;
    Function List(project: string; AQuery : TInstanceslistOptions) : TInstancesListResponse;
    Function Patch(instance: string; project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;
    Function PromoteReplica(instance: string; project: string) : TOperation;
    Function ResetSslConfig(instance: string; project: string) : TOperation;
    Function Restart(instance: string; project: string) : TOperation;
    Function RestoreBackup(instance: string; project: string; aInstancesRestoreBackupRequest : TInstancesRestoreBackupRequest) : TOperation;
    Function StartReplica(instance: string; project: string) : TOperation;
    Function StopReplica(instance: string; project: string) : TOperation;
    Function Update(instance: string; project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOperationsResource, method List
  
  TOperationsListOptions = Record
    instance : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string) : TOperation;
    Function List(project: string; AQuery : string  = '') : TOperationsListResponse;
    Function List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSslCertsResource
    --------------------------------------------------------------------}
  
  TSslCertsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(instance: string; project: string; sha1Fingerprint: string) : TOperation;
    Function Get(instance: string; project: string; sha1Fingerprint: string) : TSslCert;
    Function Insert(instance: string; project: string; aSslCertsInsertRequest : TSslCertsInsertRequest) : TSslCertsInsertResponse;
    Function List(instance: string; project: string) : TSslCertsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTiersResource
    --------------------------------------------------------------------}
  
  TTiersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(project: string) : TTiersListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUsersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUsersResource, method Delete
  
  TUsersDeleteOptions = Record
    host : string;
    _name : string;
  end;
  
  
  //Optional query Options for TUsersResource, method Update
  
  TUsersUpdateOptions = Record
    host : string;
    _name : string;
  end;
  
  TUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(instance: string; project: string; AQuery : string  = '') : TOperation;
    Function Delete(instance: string; project: string; AQuery : TUsersdeleteOptions) : TOperation;
    Function Insert(instance: string; project: string; aUser : TUser) : TOperation;
    Function List(instance: string; project: string) : TUsersListResponse;
    Function Update(instance: string; project: string; aUser : TUser; AQuery : string  = '') : TOperation;
    Function Update(instance: string; project: string; aUser : TUser; AQuery : TUsersupdateOptions) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TSqladminAPI
    --------------------------------------------------------------------}
  
  TSqladminAPI = Class(TGoogleAPI)
  Private
    FBackupRunsInstance : TBackupRunsResource;
    FDatabasesInstance : TDatabasesResource;
    FFlagsInstance : TFlagsResource;
    FInstancesInstance : TInstancesResource;
    FOperationsInstance : TOperationsResource;
    FSslCertsInstance : TSslCertsResource;
    FTiersInstance : TTiersResource;
    FUsersInstance : TUsersResource;
    Function GetBackupRunsInstance : TBackupRunsResource;virtual;
    Function GetDatabasesInstance : TDatabasesResource;virtual;
    Function GetFlagsInstance : TFlagsResource;virtual;
    Function GetInstancesInstance : TInstancesResource;virtual;
    Function GetOperationsInstance : TOperationsResource;virtual;
    Function GetSslCertsInstance : TSslCertsResource;virtual;
    Function GetTiersInstance : TTiersResource;virtual;
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
    Function CreateBackupRunsResource(AOwner : TComponent) : TBackupRunsResource;virtual;overload;
    Function CreateBackupRunsResource : TBackupRunsResource;virtual;overload;
    Function CreateDatabasesResource(AOwner : TComponent) : TDatabasesResource;virtual;overload;
    Function CreateDatabasesResource : TDatabasesResource;virtual;overload;
    Function CreateFlagsResource(AOwner : TComponent) : TFlagsResource;virtual;overload;
    Function CreateFlagsResource : TFlagsResource;virtual;overload;
    Function CreateInstancesResource(AOwner : TComponent) : TInstancesResource;virtual;overload;
    Function CreateInstancesResource : TInstancesResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TOperationsResource;virtual;overload;
    Function CreateOperationsResource : TOperationsResource;virtual;overload;
    Function CreateSslCertsResource(AOwner : TComponent) : TSslCertsResource;virtual;overload;
    Function CreateSslCertsResource : TSslCertsResource;virtual;overload;
    Function CreateTiersResource(AOwner : TComponent) : TTiersResource;virtual;overload;
    Function CreateTiersResource : TTiersResource;virtual;overload;
    Function CreateUsersResource(AOwner : TComponent) : TUsersResource;virtual;overload;
    Function CreateUsersResource : TUsersResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BackupRunsResource : TBackupRunsResource Read GetBackupRunsInstance;
    Property DatabasesResource : TDatabasesResource Read GetDatabasesInstance;
    Property FlagsResource : TFlagsResource Read GetFlagsInstance;
    Property InstancesResource : TInstancesResource Read GetInstancesInstance;
    Property OperationsResource : TOperationsResource Read GetOperationsInstance;
    Property SslCertsResource : TSslCertsResource Read GetSslCertsInstance;
    Property TiersResource : TTiersResource Read GetTiersInstance;
    Property UsersResource : TUsersResource Read GetUsersInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAclEntry
  --------------------------------------------------------------------}


Procedure TAclEntry.SetexpirationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FexpirationTime=AValue) then exit;
  FexpirationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclEntry.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclEntry.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackupConfiguration
  --------------------------------------------------------------------}


Procedure TBackupConfiguration.SetbinaryLogEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FbinaryLogEnabled=AValue) then exit;
  FbinaryLogEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupConfiguration.Setenabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupConfiguration.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackupRun
  --------------------------------------------------------------------}


Procedure TBackupRun.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.SetenqueuedTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FenqueuedTime=AValue) then exit;
  FenqueuedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.Seterror(AIndex : Integer; AValue : TOperationError); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRun.SetwindowStartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FwindowStartTime=AValue) then exit;
  FwindowStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackupRunsListResponse
  --------------------------------------------------------------------}


Procedure TBackupRunsListResponse.Setitems(AIndex : Integer; AValue : TBackupRunsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRunsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBackupRunsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBackupRunsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBinLogCoordinates
  --------------------------------------------------------------------}


Procedure TBinLogCoordinates.SetbinLogFileName(AIndex : Integer; AValue : string); 

begin
  If (FbinLogFileName=AValue) then exit;
  FbinLogFileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinLogCoordinates.SetbinLogPosition(AIndex : Integer; AValue : string); 

begin
  If (FbinLogPosition=AValue) then exit;
  FbinLogPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinLogCoordinates.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCloneContext
  --------------------------------------------------------------------}


Procedure TCloneContext.SetbinLogCoordinates(AIndex : Integer; AValue : TBinLogCoordinates); 

begin
  If (FbinLogCoordinates=AValue) then exit;
  FbinLogCoordinates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloneContext.SetdestinationInstanceName(AIndex : Integer; AValue : string); 

begin
  If (FdestinationInstanceName=AValue) then exit;
  FdestinationInstanceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCloneContext.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabase
  --------------------------------------------------------------------}


Procedure TDatabase.Setcharset(AIndex : Integer; AValue : string); 

begin
  If (Fcharset=AValue) then exit;
  Fcharset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setcollation(AIndex : Integer; AValue : string); 

begin
  If (Fcollation=AValue) then exit;
  Fcollation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.Setproject(AIndex : Integer; AValue : string); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabase.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabaseFlags
  --------------------------------------------------------------------}


Procedure TDatabaseFlags.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseFlags.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabaseInstance
  --------------------------------------------------------------------}


Procedure TDatabaseInstance.SetcurrentDiskSize(AIndex : Integer; AValue : string); 

begin
  If (FcurrentDiskSize=AValue) then exit;
  FcurrentDiskSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetdatabaseVersion(AIndex : Integer; AValue : string); 

begin
  If (FdatabaseVersion=AValue) then exit;
  FdatabaseVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetinstanceType(AIndex : Integer; AValue : string); 

begin
  If (FinstanceType=AValue) then exit;
  FinstanceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetipAddresses(AIndex : Integer; AValue : TDatabaseInstanceipAddresses); 

begin
  If (FipAddresses=AValue) then exit;
  FipAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setipv6Address(AIndex : Integer; AValue : string); 

begin
  If (Fipv6Address=AValue) then exit;
  Fipv6Address:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetmasterInstanceName(AIndex : Integer; AValue : string); 

begin
  If (FmasterInstanceName=AValue) then exit;
  FmasterInstanceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetmaxDiskSize(AIndex : Integer; AValue : string); 

begin
  If (FmaxDiskSize=AValue) then exit;
  FmaxDiskSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetonPremisesConfiguration(AIndex : Integer; AValue : TOnPremisesConfiguration); 

begin
  If (FonPremisesConfiguration=AValue) then exit;
  FonPremisesConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setproject(AIndex : Integer; AValue : string); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetreplicaConfiguration(AIndex : Integer; AValue : TReplicaConfiguration); 

begin
  If (FreplicaConfiguration=AValue) then exit;
  FreplicaConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetreplicaNames(AIndex : Integer; AValue : TDatabaseInstancereplicaNames); 

begin
  If (FreplicaNames=AValue) then exit;
  FreplicaNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetserverCaCert(AIndex : Integer; AValue : TSslCert); 

begin
  If (FserverCaCert=AValue) then exit;
  FserverCaCert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.SetserviceAccountEmailAddress(AIndex : Integer; AValue : string); 

begin
  If (FserviceAccountEmailAddress=AValue) then exit;
  FserviceAccountEmailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setsettings(AIndex : Integer; AValue : TSettings); 

begin
  If (Fsettings=AValue) then exit;
  Fsettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseInstance.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabaseInstanceipAddresses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatabaseInstancereplicaNames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatabasesListResponse
  --------------------------------------------------------------------}


Procedure TDatabasesListResponse.Setitems(AIndex : Integer; AValue : TDatabasesListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabasesListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabasesListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExportContext
  --------------------------------------------------------------------}


Procedure TExportContext.SetcsvExportOptions(AIndex : Integer; AValue : TExportContextcsvExportOptions); 

begin
  If (FcsvExportOptions=AValue) then exit;
  FcsvExportOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportContext.Setdatabases(AIndex : Integer; AValue : TExportContextdatabases); 

begin
  If (Fdatabases=AValue) then exit;
  Fdatabases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportContext.SetfileType(AIndex : Integer; AValue : string); 

begin
  If (FfileType=AValue) then exit;
  FfileType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportContext.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportContext.SetsqlExportOptions(AIndex : Integer; AValue : TExportContextsqlExportOptions); 

begin
  If (FsqlExportOptions=AValue) then exit;
  FsqlExportOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportContext.Seturi(AIndex : Integer; AValue : string); 

begin
  If (Furi=AValue) then exit;
  Furi:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportContextcsvExportOptions
  --------------------------------------------------------------------}


Procedure TExportContextcsvExportOptions.SetselectQuery(AIndex : Integer; AValue : string); 

begin
  If (FselectQuery=AValue) then exit;
  FselectQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportContextdatabases
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExportContextsqlExportOptions
  --------------------------------------------------------------------}


Procedure TExportContextsqlExportOptions.Settables(AIndex : Integer; AValue : TExportContextsqlExportOptionstables); 

begin
  If (Ftables=AValue) then exit;
  Ftables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportContextsqlExportOptionstables
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFlag
  --------------------------------------------------------------------}


Procedure TFlag.SetallowedStringValues(AIndex : Integer; AValue : TFlagallowedStringValues); 

begin
  If (FallowedStringValues=AValue) then exit;
  FallowedStringValues:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.SetappliesTo(AIndex : Integer; AValue : TFlagappliesTo); 

begin
  If (FappliesTo=AValue) then exit;
  FappliesTo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.SetmaxValue(AIndex : Integer; AValue : string); 

begin
  If (FmaxValue=AValue) then exit;
  FmaxValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.SetminValue(AIndex : Integer; AValue : string); 

begin
  If (FminValue=AValue) then exit;
  FminValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlag.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFlag.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFlagallowedStringValues
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFlagappliesTo
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFlagsListResponse
  --------------------------------------------------------------------}


Procedure TFlagsListResponse.Setitems(AIndex : Integer; AValue : TFlagsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlagsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFlagsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImportContext
  --------------------------------------------------------------------}


Procedure TImportContext.SetcsvImportOptions(AIndex : Integer; AValue : TImportContextcsvImportOptions); 

begin
  If (FcsvImportOptions=AValue) then exit;
  FcsvImportOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportContext.Setdatabase(AIndex : Integer; AValue : string); 

begin
  If (Fdatabase=AValue) then exit;
  Fdatabase:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportContext.SetfileType(AIndex : Integer; AValue : string); 

begin
  If (FfileType=AValue) then exit;
  FfileType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportContext.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportContext.Seturi(AIndex : Integer; AValue : string); 

begin
  If (Furi=AValue) then exit;
  Furi:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportContextcsvImportOptions
  --------------------------------------------------------------------}


Procedure TImportContextcsvImportOptions.Setcolumns(AIndex : Integer; AValue : TImportContextcsvImportOptionscolumns); 

begin
  If (Fcolumns=AValue) then exit;
  Fcolumns:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportContextcsvImportOptions.Settable(AIndex : Integer; AValue : string); 

begin
  If (Ftable=AValue) then exit;
  Ftable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportContextcsvImportOptionscolumns
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancesCloneRequest
  --------------------------------------------------------------------}


Procedure TInstancesCloneRequest.SetcloneContext(AIndex : Integer; AValue : TCloneContext); 

begin
  If (FcloneContext=AValue) then exit;
  FcloneContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesExportRequest
  --------------------------------------------------------------------}


Procedure TInstancesExportRequest.SetexportContext(AIndex : Integer; AValue : TExportContext); 

begin
  If (FexportContext=AValue) then exit;
  FexportContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesImportRequest
  --------------------------------------------------------------------}


Procedure TInstancesImportRequest.SetimportContext(AIndex : Integer; AValue : TImportContext); 

begin
  If (FimportContext=AValue) then exit;
  FimportContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesListResponse
  --------------------------------------------------------------------}


Procedure TInstancesListResponse.Setitems(AIndex : Integer; AValue : TInstancesListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstancesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstancesListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstancesRestoreBackupRequest
  --------------------------------------------------------------------}


Procedure TInstancesRestoreBackupRequest.SetrestoreBackupContext(AIndex : Integer; AValue : TRestoreBackupContext); 

begin
  If (FrestoreBackupContext=AValue) then exit;
  FrestoreBackupContext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIpConfiguration
  --------------------------------------------------------------------}


Procedure TIpConfiguration.SetauthorizedNetworks(AIndex : Integer; AValue : TIpConfigurationauthorizedNetworks); 

begin
  If (FauthorizedNetworks=AValue) then exit;
  FauthorizedNetworks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIpConfiguration.Setipv4Enabled(AIndex : Integer; AValue : boolean); 

begin
  If (Fipv4Enabled=AValue) then exit;
  Fipv4Enabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIpConfiguration.SetrequireSsl(AIndex : Integer; AValue : boolean); 

begin
  If (FrequireSsl=AValue) then exit;
  FrequireSsl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIpConfigurationauthorizedNetworks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TIpMapping
  --------------------------------------------------------------------}


Procedure TIpMapping.SetipAddress(AIndex : Integer; AValue : string); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIpMapping.SettimeToRetire(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeToRetire=AValue) then exit;
  FtimeToRetire:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocationPreference
  --------------------------------------------------------------------}


Procedure TLocationPreference.SetfollowGaeApplication(AIndex : Integer; AValue : string); 

begin
  If (FfollowGaeApplication=AValue) then exit;
  FfollowGaeApplication:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationPreference.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocationPreference.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMySqlReplicaConfiguration
  --------------------------------------------------------------------}


Procedure TMySqlReplicaConfiguration.SetcaCertificate(AIndex : Integer; AValue : string); 

begin
  If (FcaCertificate=AValue) then exit;
  FcaCertificate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetclientCertificate(AIndex : Integer; AValue : string); 

begin
  If (FclientCertificate=AValue) then exit;
  FclientCertificate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetclientKey(AIndex : Integer; AValue : string); 

begin
  If (FclientKey=AValue) then exit;
  FclientKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetconnectRetryInterval(AIndex : Integer; AValue : integer); 

begin
  If (FconnectRetryInterval=AValue) then exit;
  FconnectRetryInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetdumpFilePath(AIndex : Integer; AValue : string); 

begin
  If (FdumpFilePath=AValue) then exit;
  FdumpFilePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetmasterHeartbeatPeriod(AIndex : Integer; AValue : string); 

begin
  If (FmasterHeartbeatPeriod=AValue) then exit;
  FmasterHeartbeatPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetsslCipher(AIndex : Integer; AValue : string); 

begin
  If (FsslCipher=AValue) then exit;
  FsslCipher:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.Setusername(AIndex : Integer; AValue : string); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMySqlReplicaConfiguration.SetverifyServerCertificate(AIndex : Integer; AValue : boolean); 

begin
  If (FverifyServerCertificate=AValue) then exit;
  FverifyServerCertificate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOnPremisesConfiguration
  --------------------------------------------------------------------}


Procedure TOnPremisesConfiguration.SethostPort(AIndex : Integer; AValue : string); 

begin
  If (FhostPort=AValue) then exit;
  FhostPort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOnPremisesConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetendTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationErrors); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetexportContext(AIndex : Integer; AValue : TExportContext); 

begin
  If (FexportContext=AValue) then exit;
  FexportContext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetimportContext(AIndex : Integer; AValue : TImportContext); 

begin
  If (FimportContext=AValue) then exit;
  FimportContext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : string); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : string); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetProject(AIndex : Integer; AValue : string); 

begin
  If (FtargetProject=AValue) then exit;
  FtargetProject:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationError
  --------------------------------------------------------------------}


Procedure TOperationError.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationError.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationError.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationErrors
  --------------------------------------------------------------------}


Procedure TOperationErrors.Seterrors(AIndex : Integer; AValue : TOperationErrorserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationErrors.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationErrorserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperationsListResponse
  --------------------------------------------------------------------}


Procedure TOperationsListResponse.Setitems(AIndex : Integer; AValue : TOperationsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReplicaConfiguration
  --------------------------------------------------------------------}


Procedure TReplicaConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaConfiguration.SetmysqlReplicaConfiguration(AIndex : Integer; AValue : TMySqlReplicaConfiguration); 

begin
  If (FmysqlReplicaConfiguration=AValue) then exit;
  FmysqlReplicaConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRestoreBackupContext
  --------------------------------------------------------------------}


Procedure TRestoreBackupContext.SetbackupRunId(AIndex : Integer; AValue : string); 

begin
  If (FbackupRunId=AValue) then exit;
  FbackupRunId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestoreBackupContext.SetinstanceId(AIndex : Integer; AValue : string); 

begin
  If (FinstanceId=AValue) then exit;
  FinstanceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRestoreBackupContext.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSettings
  --------------------------------------------------------------------}


Procedure TSettings.SetactivationPolicy(AIndex : Integer; AValue : string); 

begin
  If (FactivationPolicy=AValue) then exit;
  FactivationPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetauthorizedGaeApplications(AIndex : Integer; AValue : TSettingsauthorizedGaeApplications); 

begin
  If (FauthorizedGaeApplications=AValue) then exit;
  FauthorizedGaeApplications:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetbackupConfiguration(AIndex : Integer; AValue : TBackupConfiguration); 

begin
  If (FbackupConfiguration=AValue) then exit;
  FbackupConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetcrashSafeReplicationEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FcrashSafeReplicationEnabled=AValue) then exit;
  FcrashSafeReplicationEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetdatabaseFlags(AIndex : Integer; AValue : TSettingsdatabaseFlags); 

begin
  If (FdatabaseFlags=AValue) then exit;
  FdatabaseFlags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetdatabaseReplicationEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FdatabaseReplicationEnabled=AValue) then exit;
  FdatabaseReplicationEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetipConfiguration(AIndex : Integer; AValue : TIpConfiguration); 

begin
  If (FipConfiguration=AValue) then exit;
  FipConfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetlocationPreference(AIndex : Integer; AValue : TLocationPreference); 

begin
  If (FlocationPreference=AValue) then exit;
  FlocationPreference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetpricingPlan(AIndex : Integer; AValue : string); 

begin
  If (FpricingPlan=AValue) then exit;
  FpricingPlan:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetreplicationType(AIndex : Integer; AValue : string); 

begin
  If (FreplicationType=AValue) then exit;
  FreplicationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetsettingsVersion(AIndex : Integer; AValue : string); 

begin
  If (FsettingsVersion=AValue) then exit;
  FsettingsVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.Settier(AIndex : Integer; AValue : string); 

begin
  If (Ftier=AValue) then exit;
  Ftier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSettingsauthorizedGaeApplications
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSettingsdatabaseFlags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSslCert
  --------------------------------------------------------------------}


Procedure TSslCert.Setcert(AIndex : Integer; AValue : string); 

begin
  If (Fcert=AValue) then exit;
  Fcert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.SetcertSerialNumber(AIndex : Integer; AValue : string); 

begin
  If (FcertSerialNumber=AValue) then exit;
  FcertSerialNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.SetcommonName(AIndex : Integer; AValue : string); 

begin
  If (FcommonName=AValue) then exit;
  FcommonName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.SetcreateTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.SetexpirationTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FexpirationTime=AValue) then exit;
  FexpirationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCert.Setsha1Fingerprint(AIndex : Integer; AValue : string); 

begin
  If (Fsha1Fingerprint=AValue) then exit;
  Fsha1Fingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSslCertDetail
  --------------------------------------------------------------------}


Procedure TSslCertDetail.SetcertInfo(AIndex : Integer; AValue : TSslCert); 

begin
  If (FcertInfo=AValue) then exit;
  FcertInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCertDetail.SetcertPrivateKey(AIndex : Integer; AValue : string); 

begin
  If (FcertPrivateKey=AValue) then exit;
  FcertPrivateKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSslCertsInsertRequest
  --------------------------------------------------------------------}


Procedure TSslCertsInsertRequest.SetcommonName(AIndex : Integer; AValue : string); 

begin
  If (FcommonName=AValue) then exit;
  FcommonName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSslCertsInsertResponse
  --------------------------------------------------------------------}


Procedure TSslCertsInsertResponse.SetclientCert(AIndex : Integer; AValue : TSslCertDetail); 

begin
  If (FclientCert=AValue) then exit;
  FclientCert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCertsInsertResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCertsInsertResponse.SetserverCaCert(AIndex : Integer; AValue : TSslCert); 

begin
  If (FserverCaCert=AValue) then exit;
  FserverCaCert:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSslCertsListResponse
  --------------------------------------------------------------------}


Procedure TSslCertsListResponse.Setitems(AIndex : Integer; AValue : TSslCertsListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSslCertsListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSslCertsListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTier
  --------------------------------------------------------------------}


Procedure TTier.SetDiskQuota(AIndex : Integer; AValue : string); 

begin
  If (FDiskQuota=AValue) then exit;
  FDiskQuota:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTier.SetRAM(AIndex : Integer; AValue : string); 

begin
  If (FRAM=AValue) then exit;
  FRAM:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTier.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTier.Setregion(AIndex : Integer; AValue : TTierregion); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTier.Settier(AIndex : Integer; AValue : string); 

begin
  If (Ftier=AValue) then exit;
  Ftier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTierregion
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTiersListResponse
  --------------------------------------------------------------------}


Procedure TTiersListResponse.Setitems(AIndex : Integer; AValue : TTiersListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTiersListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTiersListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUser
  --------------------------------------------------------------------}


Procedure TUser.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Sethost(AIndex : Integer; AValue : string); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setinstance(AIndex : Integer; AValue : string); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUser.Setproject(AIndex : Integer; AValue : string); 

begin
  If (Fproject=AValue) then exit;
  Fproject:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersListResponse
  --------------------------------------------------------------------}


Procedure TUsersListResponse.Setitems(AIndex : Integer; AValue : TUsersListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsersListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsersListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBackupRunsResource
  --------------------------------------------------------------------}


Class Function TBackupRunsResource.ResourceName : String;

begin
  Result:='backupRuns';
end;

Class Function TBackupRunsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TBackupRunsResource.Get(id: string; instance: string; project: string) : TBackupRun;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/backupRuns/{id}';
  _Methodid   = 'sql.backupRuns.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id,'instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBackupRun) as TBackupRun;
end;

Function TBackupRunsResource.List(instance: string; project: string; AQuery : string = '') : TBackupRunsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/backupRuns';
  _Methodid   = 'sql.backupRuns.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBackupRunsListResponse) as TBackupRunsListResponse;
end;


Function TBackupRunsResource.List(instance: string; project: string; AQuery : TBackupRunslistOptions) : TBackupRunsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(instance,project,_Q);
end;



{ --------------------------------------------------------------------
  TDatabasesResource
  --------------------------------------------------------------------}


Class Function TDatabasesResource.ResourceName : String;

begin
  Result:='databases';
end;

Class Function TDatabasesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TDatabasesResource.Delete(database: string; instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{project}/instances/{instance}/databases/{database}';
  _Methodid   = 'sql.databases.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['database',database,'instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TDatabasesResource.Get(database: string; instance: string; project: string) : TDatabase;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/databases/{database}';
  _Methodid   = 'sql.databases.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['database',database,'instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDatabase) as TDatabase;
end;

Function TDatabasesResource.Insert(instance: string; project: string; aDatabase : TDatabase) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/databases';
  _Methodid   = 'sql.databases.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabase,TOperation) as TOperation;
end;

Function TDatabasesResource.List(instance: string; project: string) : TDatabasesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/databases';
  _Methodid   = 'sql.databases.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDatabasesListResponse) as TDatabasesListResponse;
end;

Function TDatabasesResource.Patch(database: string; instance: string; project: string; aDatabase : TDatabase) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'projects/{project}/instances/{instance}/databases/{database}';
  _Methodid   = 'sql.databases.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['database',database,'instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabase,TOperation) as TOperation;
end;

Function TDatabasesResource.Update(database: string; instance: string; project: string; aDatabase : TDatabase) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{project}/instances/{instance}/databases/{database}';
  _Methodid   = 'sql.databases.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['database',database,'instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabase,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TFlagsResource
  --------------------------------------------------------------------}


Class Function TFlagsResource.ResourceName : String;

begin
  Result:='flags';
end;

Class Function TFlagsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TFlagsResource.List : TFlagsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'flags';
  _Methodid   = 'sql.flags.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TFlagsListResponse) as TFlagsListResponse;
end;



{ --------------------------------------------------------------------
  TInstancesResource
  --------------------------------------------------------------------}


Class Function TInstancesResource.ResourceName : String;

begin
  Result:='instances';
end;

Class Function TInstancesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TInstancesResource.Clone(instance: string; project: string; aInstancesCloneRequest : TInstancesCloneRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/clone';
  _Methodid   = 'sql.instances.clone';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstancesCloneRequest,TOperation) as TOperation;
end;

Function TInstancesResource.Delete(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{project}/instances/{instance}';
  _Methodid   = 'sql.instances.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.Export(instance: string; project: string; aInstancesExportRequest : TInstancesExportRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/export';
  _Methodid   = 'sql.instances.export';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstancesExportRequest,TOperation) as TOperation;
end;

Function TInstancesResource.Get(instance: string; project: string) : TDatabaseInstance;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}';
  _Methodid   = 'sql.instances.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDatabaseInstance) as TDatabaseInstance;
end;

Function TInstancesResource.Import(instance: string; project: string; aInstancesImportRequest : TInstancesImportRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/import';
  _Methodid   = 'sql.instances.import';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstancesImportRequest,TOperation) as TOperation;
end;

Function TInstancesResource.Insert(project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances';
  _Methodid   = 'sql.instances.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabaseInstance,TOperation) as TOperation;
end;

Function TInstancesResource.List(project: string; AQuery : string = '') : TInstancesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances';
  _Methodid   = 'sql.instances.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstancesListResponse) as TInstancesListResponse;
end;


Function TInstancesResource.List(project: string; AQuery : TInstanceslistOptions) : TInstancesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TInstancesResource.Patch(instance: string; project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'projects/{project}/instances/{instance}';
  _Methodid   = 'sql.instances.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabaseInstance,TOperation) as TOperation;
end;

Function TInstancesResource.PromoteReplica(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/promoteReplica';
  _Methodid   = 'sql.instances.promoteReplica';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.ResetSslConfig(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/resetSslConfig';
  _Methodid   = 'sql.instances.resetSslConfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.Restart(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/restart';
  _Methodid   = 'sql.instances.restart';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.RestoreBackup(instance: string; project: string; aInstancesRestoreBackupRequest : TInstancesRestoreBackupRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/restoreBackup';
  _Methodid   = 'sql.instances.restoreBackup';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstancesRestoreBackupRequest,TOperation) as TOperation;
end;

Function TInstancesResource.StartReplica(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/startReplica';
  _Methodid   = 'sql.instances.startReplica';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.StopReplica(instance: string; project: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/stopReplica';
  _Methodid   = 'sql.instances.stopReplica';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstancesResource.Update(instance: string; project: string; aDatabaseInstance : TDatabaseInstance) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{project}/instances/{instance}';
  _Methodid   = 'sql.instances.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDatabaseInstance,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TOperationsResource
  --------------------------------------------------------------------}


Class Function TOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TOperationsResource.Get(operation: string; project: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/operations/{operation}';
  _Methodid   = 'sql.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TOperationsResource.List(project: string; AQuery : string = '') : TOperationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/operations';
  _Methodid   = 'sql.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationsListResponse) as TOperationsListResponse;
end;


Function TOperationsResource.List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'instance',AQuery.instance);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TSslCertsResource
  --------------------------------------------------------------------}


Class Function TSslCertsResource.ResourceName : String;

begin
  Result:='sslCerts';
end;

Class Function TSslCertsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TSslCertsResource.Delete(instance: string; project: string; sha1Fingerprint: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{project}/instances/{instance}/sslCerts/{sha1Fingerprint}';
  _Methodid   = 'sql.sslCerts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'sha1Fingerprint',sha1Fingerprint]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TSslCertsResource.Get(instance: string; project: string; sha1Fingerprint: string) : TSslCert;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/sslCerts/{sha1Fingerprint}';
  _Methodid   = 'sql.sslCerts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project,'sha1Fingerprint',sha1Fingerprint]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSslCert) as TSslCert;
end;

Function TSslCertsResource.Insert(instance: string; project: string; aSslCertsInsertRequest : TSslCertsInsertRequest) : TSslCertsInsertResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/sslCerts';
  _Methodid   = 'sql.sslCerts.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSslCertsInsertRequest,TSslCertsInsertResponse) as TSslCertsInsertResponse;
end;

Function TSslCertsResource.List(instance: string; project: string) : TSslCertsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/sslCerts';
  _Methodid   = 'sql.sslCerts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSslCertsListResponse) as TSslCertsListResponse;
end;



{ --------------------------------------------------------------------
  TTiersResource
  --------------------------------------------------------------------}


Class Function TTiersResource.ResourceName : String;

begin
  Result:='tiers';
end;

Class Function TTiersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsqladminAPI;
end;

Function TTiersResource.List(project: string) : TTiersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/tiers';
  _Methodid   = 'sql.tiers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTiersListResponse) as TTiersListResponse;
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
  Result:=TsqladminAPI;
end;

Function TUsersResource.Delete(instance: string; project: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{project}/instances/{instance}/users';
  _Methodid   = 'sql.users.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TUsersResource.Delete(instance: string; project: string; AQuery : TUsersdeleteOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'host',AQuery.host);
  AddToQuery(_Q,'name',AQuery._name);
  Result:=Delete(instance,project,_Q);
end;

Function TUsersResource.Insert(instance: string; project: string; aUser : TUser) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/instances/{instance}/users';
  _Methodid   = 'sql.users.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUser,TOperation) as TOperation;
end;

Function TUsersResource.List(instance: string; project: string) : TUsersListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/instances/{instance}/users';
  _Methodid   = 'sql.users.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUsersListResponse) as TUsersListResponse;
end;

Function TUsersResource.Update(instance: string; project: string; aUser : TUser; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{project}/instances/{instance}/users';
  _Methodid   = 'sql.users.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instance',instance,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aUser,TOperation) as TOperation;
end;


Function TUsersResource.Update(instance: string; project: string; aUser : TUser; AQuery : TUsersupdateOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'host',AQuery.host);
  AddToQuery(_Q,'name',AQuery._name);
  Result:=Update(instance,project,aUser,_Q);
end;



{ --------------------------------------------------------------------
  TSqladminAPI
  --------------------------------------------------------------------}

Class Function TSqladminAPI.APIName : String;

begin
  Result:='sqladmin';
end;

Class Function TSqladminAPI.APIVersion : String;

begin
  Result:='v1beta4';
end;

Class Function TSqladminAPI.APIRevision : String;

begin
  Result:='20150325';
end;

Class Function TSqladminAPI.APIID : String;

begin
  Result:='sqladmin:v1beta4';
end;

Class Function TSqladminAPI.APITitle : String;

begin
  Result:='Cloud SQL Administration API';
end;

Class Function TSqladminAPI.APIDescription : String;

begin
  Result:='API for Cloud SQL database instance management.';
end;

Class Function TSqladminAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TSqladminAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TSqladminAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TSqladminAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TSqladminAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/cloud-sql/docs/admin-api/';
end;

Class Function TSqladminAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TSqladminAPI.APIbasePath : string;

begin
  Result:='/sql/v1beta4/';
end;

Class Function TSqladminAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/sql/v1beta4/';
end;

Class Function TSqladminAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TSqladminAPI.APIservicePath : string;

begin
  Result:='sql/v1beta4/';
end;

Class Function TSqladminAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TSqladminAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/sqlservice.admin';
  Result[1].Description:='Manage your Google SQL Service instances';
  
end;

Class Function TSqladminAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TSqladminAPI.RegisterAPIResources;

begin
  TAclEntry.RegisterObject;
  TBackupConfiguration.RegisterObject;
  TBackupRun.RegisterObject;
  TBackupRunsListResponse.RegisterObject;
  TBackupRunsListResponseitems.RegisterObject;
  TBinLogCoordinates.RegisterObject;
  TCloneContext.RegisterObject;
  TDatabase.RegisterObject;
  TDatabaseFlags.RegisterObject;
  TDatabaseInstance.RegisterObject;
  TDatabaseInstanceipAddresses.RegisterObject;
  TDatabaseInstancereplicaNames.RegisterObject;
  TDatabasesListResponse.RegisterObject;
  TDatabasesListResponseitems.RegisterObject;
  TExportContext.RegisterObject;
  TExportContextcsvExportOptions.RegisterObject;
  TExportContextdatabases.RegisterObject;
  TExportContextsqlExportOptions.RegisterObject;
  TExportContextsqlExportOptionstables.RegisterObject;
  TFlag.RegisterObject;
  TFlagallowedStringValues.RegisterObject;
  TFlagappliesTo.RegisterObject;
  TFlagsListResponse.RegisterObject;
  TFlagsListResponseitems.RegisterObject;
  TImportContext.RegisterObject;
  TImportContextcsvImportOptions.RegisterObject;
  TImportContextcsvImportOptionscolumns.RegisterObject;
  TInstancesCloneRequest.RegisterObject;
  TInstancesExportRequest.RegisterObject;
  TInstancesImportRequest.RegisterObject;
  TInstancesListResponse.RegisterObject;
  TInstancesListResponseitems.RegisterObject;
  TInstancesRestoreBackupRequest.RegisterObject;
  TIpConfiguration.RegisterObject;
  TIpConfigurationauthorizedNetworks.RegisterObject;
  TIpMapping.RegisterObject;
  TLocationPreference.RegisterObject;
  TMySqlReplicaConfiguration.RegisterObject;
  TOnPremisesConfiguration.RegisterObject;
  TOperation.RegisterObject;
  TOperationError.RegisterObject;
  TOperationErrors.RegisterObject;
  TOperationErrorserrors.RegisterObject;
  TOperationsListResponse.RegisterObject;
  TOperationsListResponseitems.RegisterObject;
  TReplicaConfiguration.RegisterObject;
  TRestoreBackupContext.RegisterObject;
  TSettings.RegisterObject;
  TSettingsauthorizedGaeApplications.RegisterObject;
  TSettingsdatabaseFlags.RegisterObject;
  TSslCert.RegisterObject;
  TSslCertDetail.RegisterObject;
  TSslCertsInsertRequest.RegisterObject;
  TSslCertsInsertResponse.RegisterObject;
  TSslCertsListResponse.RegisterObject;
  TSslCertsListResponseitems.RegisterObject;
  TTier.RegisterObject;
  TTierregion.RegisterObject;
  TTiersListResponse.RegisterObject;
  TTiersListResponseitems.RegisterObject;
  TUser.RegisterObject;
  TUsersListResponse.RegisterObject;
  TUsersListResponseitems.RegisterObject;
end;


Function TSqladminAPI.GetBackupRunsInstance : TBackupRunsResource;

begin
  if (FBackupRunsInstance=Nil) then
    FBackupRunsInstance:=CreateBackupRunsResource;
  Result:=FBackupRunsInstance;
end;

Function TSqladminAPI.CreateBackupRunsResource : TBackupRunsResource;

begin
  Result:=CreateBackupRunsResource(Self);
end;


Function TSqladminAPI.CreateBackupRunsResource(AOwner : TComponent) : TBackupRunsResource;

begin
  Result:=TBackupRunsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetDatabasesInstance : TDatabasesResource;

begin
  if (FDatabasesInstance=Nil) then
    FDatabasesInstance:=CreateDatabasesResource;
  Result:=FDatabasesInstance;
end;

Function TSqladminAPI.CreateDatabasesResource : TDatabasesResource;

begin
  Result:=CreateDatabasesResource(Self);
end;


Function TSqladminAPI.CreateDatabasesResource(AOwner : TComponent) : TDatabasesResource;

begin
  Result:=TDatabasesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetFlagsInstance : TFlagsResource;

begin
  if (FFlagsInstance=Nil) then
    FFlagsInstance:=CreateFlagsResource;
  Result:=FFlagsInstance;
end;

Function TSqladminAPI.CreateFlagsResource : TFlagsResource;

begin
  Result:=CreateFlagsResource(Self);
end;


Function TSqladminAPI.CreateFlagsResource(AOwner : TComponent) : TFlagsResource;

begin
  Result:=TFlagsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetInstancesInstance : TInstancesResource;

begin
  if (FInstancesInstance=Nil) then
    FInstancesInstance:=CreateInstancesResource;
  Result:=FInstancesInstance;
end;

Function TSqladminAPI.CreateInstancesResource : TInstancesResource;

begin
  Result:=CreateInstancesResource(Self);
end;


Function TSqladminAPI.CreateInstancesResource(AOwner : TComponent) : TInstancesResource;

begin
  Result:=TInstancesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetOperationsInstance : TOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TSqladminAPI.CreateOperationsResource : TOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TSqladminAPI.CreateOperationsResource(AOwner : TComponent) : TOperationsResource;

begin
  Result:=TOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetSslCertsInstance : TSslCertsResource;

begin
  if (FSslCertsInstance=Nil) then
    FSslCertsInstance:=CreateSslCertsResource;
  Result:=FSslCertsInstance;
end;

Function TSqladminAPI.CreateSslCertsResource : TSslCertsResource;

begin
  Result:=CreateSslCertsResource(Self);
end;


Function TSqladminAPI.CreateSslCertsResource(AOwner : TComponent) : TSslCertsResource;

begin
  Result:=TSslCertsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetTiersInstance : TTiersResource;

begin
  if (FTiersInstance=Nil) then
    FTiersInstance:=CreateTiersResource;
  Result:=FTiersInstance;
end;

Function TSqladminAPI.CreateTiersResource : TTiersResource;

begin
  Result:=CreateTiersResource(Self);
end;


Function TSqladminAPI.CreateTiersResource(AOwner : TComponent) : TTiersResource;

begin
  Result:=TTiersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TSqladminAPI.GetUsersInstance : TUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TSqladminAPI.CreateUsersResource : TUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TSqladminAPI.CreateUsersResource(AOwner : TComponent) : TUsersResource;

begin
  Result:=TUsersResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TSqladminAPI.RegisterAPI;
end.
