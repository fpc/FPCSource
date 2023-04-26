unit FBAdmin;

{ Interbase/Firebird Administration using the service manager

  Copyright (C) 2012 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils,
{$IfDef LinkDynamically}
  ibase60dyn,
{$Else}
  ibase60,
{$EndIf}
  IBConnection;

type
  TIBBackupOption=(IBBkpVerbose,IBBkpIgnoreChecksums,IBBkpIgnoreLimbo,IBBkpMetadataOnly,
     IBBkpNoGarbageCollect,IBBkpOldDescriptions,IBBkpNonTransportable,IBBkpConvert,IBBkpWait);
  TIBBackupOptions= set of TIBBackupOption;
  TIBRestoreOption=(IBResVerbose,IBResDeactivateIdx,IBResNoShadow,IBResNoValidity,
     IBResOneAtaTime,IBResReplace,IBResCreate,IBResUseAllSpace,IBResAMReadOnly,IBResAMReadWrite,
     IBFixFssData, IBFixFssMeta,IBResWait);
  TIBRestoreOptions= set of TIBRestoreOption;
  TServiceProtocol=(IBSPLOCAL,IBSPTCPIP,IBSPNETBEUI,IBSPNAMEDPIPE);
  TIBOnOutput= procedure(Sender: TObject; msg: string; IBAdminAction: string) of object;
  TIBStatOption = (IBDataPages, IBDbLog, IBHeaderPages, IBIndexPages, IBSystemRelations,
    IBRecordVersions, IBStatTables);
  TIBStatOptions = set of TIBStatOption;

  { TFBAdmin }

  TFBAdmin=class(TComponent)
  private
    FErrorCode: longint;
    FErrorMsg: string;
    FFixFssDataCharSet: String;
    FHost: string;
    FOnOutput: TIBOnOutput;
    FOutput: TStringList;
    FPassword: string;
    FPort: word;
    FProtocol: TServiceProtocol;
    FServerImplementation: string;
    FServerLockDir: string;
    FServerMsgDir: string;
    FServerRootDir: string;
    FServerSecDBDir: string;
    FServerVersion: string;
    FStatus: array [0..19] of ISC_STATUS;
    FSvcHandle: isc_svc_handle;
    FUseExceptions: boolean;
    FUser: string;
    FWaitInterval: Integer;
    function CheckConnected(const ProcName: string):boolean;
    procedure CheckError(const ProcName : string; Status : PISC_STATUS);
    function GetDBInfo:boolean;
    function GetIBLongint(const buffer:string; var bufptr:integer):longint;overload;
    function GetIBString(const buffer:string; var bufptr:integer):string;overload;
    function GetOutput(const IBAdminAction:string):boolean;
    function IBParamSerialize(isccode:byte;const value:string):string;
    procedure IBRaiseError(GDSErrorCode:Longint; const msg : string; const args : array of const);
    function IBSPBParamSerialize(isccode:byte;const value:string):string;
    function IBSPBParamSerialize(isccode:byte;value:longint):string;
    function MakeBackupOptions(options:TIBBackupOptions):longint;
    function MakeRestoreOptions(options:TIBRestoreOptions):longint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Connect to service manager. Specify User,Password and, for remote databases,
    //Host and, if not standard, Port
    function Connect:boolean;
    //Disconnect from service manager. Done automatically when destroying component
    function DisConnect:boolean;
    //Backup database to a single file on the server.
    //Include IBBkpVerbose in Options to get progress feedback through the OnOutput Handler
    function Backup(const Database,Filename:string;Options:TIBBackupOptions;const RoleName:string=''):boolean;
    //Backup database to multiple files with length FileSize on the server.
    //Filenames is the list of filenames to use. The last file specified has no size limit.
    //Include IBBkpVerbose in Options to get progress feedback through the OnOutput Handler
    function BackupMultiFile(const Database:string;Filenames:TStrings;FileSize:longint;
      Options:TIBBackupOptions;const RoleName:string=''):boolean;
    //Restore database from a single file on the server.
    //Include IBResReplace to restore in and existing database or IBResCreate
    //to create a a new one.
    //Include IBResVerbose in Options to get progress feedback through the OnOutput Handler
    function Restore(const Database,Filename:string;Options:TIBRestoreOptions;const RoleName:string=''):boolean;
    //Restore database from multiple files on the server.
    //Filenames is the list of files to use.
    //Include IBResReplace to restore in and existing database or IBResCreate
    //to create a a new one.
    //Include IBResVerbose in Options to get progress feedback through the OnOutput Handler
    function RestoreMultiFile(const Database:string;Filenames:TStrings;
      Options:TIBRestoreOptions;const RoleName:string=''):boolean;
    //Add a new user.
    function AddUser(const UserName,Password:string;const RoleName:string='';
      const GroupName:string='';const FirstName:string='';const MiddleName:string='';
      const LastName:string='';UserID: longint = 0; GroupID: longint = 0):boolean;
    //Modify an existing user.
    function ModifyUser(const UserName,Password:string;const RoleName:string='';
      const GroupName:string=''; const FirstName:string=''; const MiddleName:string='';
      const LastName:string='';UserID: longint = 0; GroupID: longint = 0):boolean;
    //Delete an existing user.
    function DeleteUser(const UserName:string;const RoleName:string=''):boolean;
    //Get the details of an existing user.
    function GetUser(const UserName:string;var GroupName,FirstName,MiddleName,
      LastName:string;var UserID, GroupID: longint):boolean;
    //Get the list of all users
    function GetUsers(Users:TStrings):boolean;
    //Get database server log file
    function GetDatabaseLog:boolean;
    // For Backup, Restore this will check if the service call is still running.
    function ServiceRunning: Boolean;
    // Wait till the service stops running, or until aTimeout (in milliseconds) is reached.
    // Return true if the service stopped, false if timeout reached.
    // WaitInterval is the interval (in milliseconds) between ServiceRunning calls.
    function WaitForServiceCompletion(aTimeOut: Integer): Boolean;
    //Get database statistics
    function GetDatabaseStats(const Database:string;Options:TIBStatOptions; const TableNames:String = ''): boolean;
    //Database server version
    property ServerVersion:string read FServerVersion;
    //Implementation string of the database server
    property ServerImplementation:string read FServerImplementation;
    //Setting of $FIREBIRD or $INTERBASE
    property ServerRootDir:string read FServerRootDir;
    //Setting of $FIREBIRD_LCK or $INTERBASE_LCK
    property ServerLockDir:string read FServerLockDir;
    //Setting of $FIREBIRD_MSG or $INTERBASE_MSG
    property ServerMsgDir:string read FServerMsgDir;
    //Path to the security database in use by the server
    property ServerSecDBDir:string read FServerSecDBDir;
    // FixFxxData/FixFxxMetaData code page
    property FixFssDataCharSet: String read FFixFssDataCharSet write FFixFssDataCharSet;
  published
    //User name to connect to service manager
    property User: string read FUser write FUser;
    //User name to connect to service manager
    property Password: string read FPassword write FPassword;
    //Database Host
    property Host: string read FHost write FHost;
    //Database Port, Default:3050
    property Port: word read FPort write FPort default 3050;
    //Protocol used to connect to service manager. One of:
    //IBSPLOCAL: Host and port ignored
    //IBSPTCPIP: Connect to Host:Port
    //IBSPNETBEUI: Connect to \\Host\
    //IBSPNAMEDPIPE: Connect to //Host/
    property Protocol: TServiceProtocol read FProtocol write FProtocol;
    //Errorcode returned in status vector or 0 for TFBAdmin errors
    property ErrorCode:longint read FErrorCode;
    //Errormsg returned in status vector or by TFBAdmin
    property ErrorMsg:string read FErrorMsg;
    //Raise exceptions when error encounterd. Default: false
    property UseExceptions:boolean read FUseExceptions write FUseExceptions;
    //Service output messages
    //Result from Backup and Restore operations and GetLog
    property Output:TStringList read FOutput;
    //Event handler for Service output messages
    //Used in Backup and Restore operations and GetLog
    property OnOutput: TIBOnOutput read FOnOutput write FOnOutput;
    // Interval (in milliseconds) to sleep while waiting for the service operation to end.
    Property WaitInterval : Integer Read FWaitInterval Write FWaitInterval;
  end;


implementation

uses dateutils;

resourcestring
  SErrNotConnected = '%s : %s : Not connected.';
  SErrError = '%s : %s : %s';
  SErrConnected = '%s : Connect : Already connected.';
  SErrRestoreOptionsError = '%s : Restore : Nothing to do. Specify IBResReplace or IBResCreate in Options.';
  SErrRestoreMultiOptionsError = '%s : RestoreMultiFile : Nothing to do. Specify IBResReplace or IBResCreate in Options.';
  SErrUserDoesNotExist = '%s : GetUser : User does not exist.';
  SErrUserInvalidReply = '%s : GetUser : Invalid reply (%d).';
  SErrUsersInvalidReply = '%s : GetUsers : Invalid reply (%d).';

{ TFBAdmin }

function TFBAdmin.IBParamSerialize(isccode: byte; const value: string): string;
begin
  result:=chr(isccode)+chr(Length(value))+value;
end;

procedure TFBAdmin.IBRaiseError(GDSErrorCode: Longint; const msg: string;
  const args: array of const);
begin
  FErrorMsg:=Format(msg,args);
  FErrorCode:=GDSErrorCode;
  if FUseExceptions then
    raise EIBDatabaseError.CreateFmt(msg,args,nil,GDSErrorCode,'');
end;

function TFBAdmin.IBSPBParamSerialize(isccode: byte; const value: string): string;
begin
  result:=chr(isccode)+chr(Length(value) and $ff)+chr((Length(value)shr 8) and $ff)+value;
end;

function TFBAdmin.IBSPBParamSerialize(isccode: byte; value: longint): string;
begin
  result:=chr(isccode)+chr(value and $ff)+chr((value shr 8) and $ff)
     +chr((value shr 16) and $ff)+chr((value shr 24) and $ff);
end;

function TFBAdmin.MakeBackupOptions(options: TIBBackupOptions): longint;
begin
  result:=0;
  if IBBkpConvert in Options then
    result:=result or isc_spb_bkp_convert;
  if IBBkpIgnoreChecksums in Options then
    result:=result or isc_spb_bkp_ignore_checksums;
  if IBBkpIgnoreLimbo in Options then
    result:=result or isc_spb_bkp_ignore_limbo;
  if IBBkpMetadataOnly  in Options then
    result:=result or isc_spb_bkp_metadata_only;
  if IBBkpNoGarbageCollect in Options then
    result:=result or isc_spb_bkp_no_garbage_collect;
  if IBBkpNonTransportable in Options then
    result:=result or isc_spb_bkp_non_transportable;
  if IBBkpOldDescriptions  in Options then
    result:=result or isc_spb_bkp_old_descriptions;
end;

function TFBAdmin.MakeRestoreOptions(options: TIBRestoreOptions): longint;
begin
  result:=0;
  if IBResCreate in Options then
    result:=result or isc_spb_res_create;
  if IBResDeactivateIdx in Options then
    result:=result or isc_spb_res_deactivate_idx;
  if IBResNoShadow in Options then
    result:=result or isc_spb_res_no_shadow;
  if IBResNoValidity in Options then
    result:=result or isc_spb_res_no_validity;
  if IBResOneAtaTime in Options then
    result:=result or isc_spb_res_one_at_a_time;
  if IBResReplace in Options then
    result:=result or isc_spb_res_replace;
  if IBResUseAllSpace in Options then
    result:=result or isc_spb_res_use_all_space;
end;


function TFBAdmin.CheckConnected(const ProcName: string): boolean;
begin
  result:=false;
  if FSvcHandle=FB_API_NULLHANDLE then
    begin
    IBRaiseError(0,SErrNotConnected,[self.Name,ProcName]);
    exit;
    end;
  result:=true;
end;

procedure TFBAdmin.CheckError(const ProcName: string; Status: PISC_STATUS);
var
  buf : array [0..1023] of char;
  Msg : string;
  Err : longint;

begin
  if ((Status[0] = 1) and (Status[1] <> 0)) then
  begin
    Err := Status[1];
    msg := '';
    while isc_interprete(Buf, @Status) > 0 do
      Msg := Msg + LineEnding +' -' + StrPas(Buf);
    IBRaiseError(Err,SErrError,[self.Name,ProcName,Msg]);
  end;
end;

function TFBAdmin.GetDBInfo: boolean;

  function QueryInfo(isc:byte):string;
  var
    spb:string;
    len:integer;
  begin
    result:='';
    spb:=chr(isc);
    setlength(result,255);
    if (isc_service_query(@FStatus[0], @FSvcHandle, nil, 0, nil, length(spb),
      @spb[1],255,@result[1])=0) and (result[1]=chr(isc)) then
      begin
      len:=isc_vax_integer(@result[2],2);
      delete(result,1,3); // remove cmd and len
      setlength(result,len);
      end;
  end;

begin
  FServerImplementation:= QueryInfo(isc_info_svc_implementation);
  FServerLockDir:= QueryInfo(isc_info_svc_get_env_lock);
  FServerMsgDir:= QueryInfo(isc_info_svc_get_env_msg);
  FServerRootDir:= QueryInfo(isc_info_svc_get_env);
  FServerSecDBDir:= QueryInfo(isc_info_svc_user_dbpath);
  FServerVersion:= QueryInfo(isc_info_svc_server_version);
end;

function TFBAdmin.GetIBLongint(const buffer: string; var bufptr: integer): longint;
begin
  bufptr:=bufptr+1;
  result:=isc_vax_integer(@Buffer[bufptr], 4);
  bufptr:=bufptr+4;
end;

function TFBAdmin.GetIBString(const buffer: string; var bufptr: integer): string;
var
  len:integer;
begin
  bufptr:=bufptr+1;
  len:=isc_vax_integer(@buffer[bufptr], 2);
  bufptr:=bufptr+2;
  result:=copy(buffer,bufptr,len);
  bufptr:=bufptr+len;
end;

function TFBAdmin.GetOutput(const IBAdminAction: string): boolean;
var
  len:integer;
  buffer:string;
  spb:string;
const
  BUFFERSIZE=1000;
begin
  len:=0;
  FOutput.Clear;
  spb:=chr(isc_info_svc_line);
  repeat
    setlength(buffer,BUFFERSIZE);
    result:=isc_service_query(@FStatus[0], @FSvcHandle, nil, 0, nil, length(spb),
      @spb[1],BUFFERSIZE,@buffer[1])=0;
    if not result then
      begin
      CheckError('GetOutput',FStatus);
      exit;
      end;
    if buffer[1]=chr(isc_info_svc_line) then
      begin
      len:=isc_vax_integer(@buffer[2],2);
      delete(buffer,1,3); // remove cmd and len
      setlength(buffer,len);
      FOutput.Add(buffer);
      if assigned(FOnOutput) then
        begin
        FOnOutput(Self,buffer,IBAdminAction);
        end;
      end;
  until len=0;
end;

constructor TFBAdmin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:= 3050;
  FOutput:=TStringList.Create;
  FFixFssDataCharSet:= '';
end;

destructor TFBAdmin.Destroy;
begin
  try
    if FSvcHandle<>FB_API_NULLHANDLE then
    begin
      WaitInterval:=100;
      DisConnect; // This can raise an exception
    end;
  Finally
    FOutput.Destroy;
    inherited Destroy;
  end;
end;

function TFBAdmin.Connect: boolean;
var
  Service:string;
  spb:string;
begin
  result:=false;
  {$IfDef LinkDynamically}
  result:=InitialiseIBase60<>0;
  {$EndIf}
  if FSvcHandle<>FB_API_NULLHANDLE then
    raise EIBDatabaseError.CreateFmt(SErrConnected,[Self.Name],nil,0,'');
  Service:='service_mgr';
  case FProtocol of
    IBSPTCPIP:if FPort=3050 then
        service:=FHost+':'+service
      else
        service:=FHost+'/'+IntTostr(FPort)+':'+service;
    IBSPNETBEUI:service:='\\'+FHost+'\'+service;
    IBSPNAMEDPIPE:service:='//'+FHost+'/'+service;
  end;
  spb:=chr(isc_spb_version)+chr(isc_spb_current_version)+
    IBParamSerialize(isc_spb_user_name,FUser)+
    IBParamSerialize(isc_spb_password,FPassword);
  result:=isc_service_attach(@FStatus[0], 0,PChar(Service), @FSvcHandle,
      length(spb), @spb[1]) = 0;
  if not result then
    CheckError('Connect',FStatus)
  else
    GetDBInfo;
end;

function TFBAdmin.DisConnect: boolean;

begin
  result:=CheckConnected('DisConnect');
  result:= isc_service_detach(@FStatus[0], @FSvcHandle) = 0;
  if not result then
    CheckError('DisConnect',FStatus);
  FSvcHandle:=FB_API_NULLHANDLE;
  {$IfDef LinkDynamically}
  ReleaseIBase60;
  {$EndIf}
  result:=true;
end;

function TFBAdmin.Backup(const Database, Filename: string; Options: TIBBackupOptions;
  const RoleName: string): boolean;
var
  spb:string;
begin
  result:=CheckConnected('Backup');
  spb:=chr(isc_action_svc_backup)+IBSPBParamSerialize(isc_spb_dbname,Database)
    +IBSPBParamSerialize(isc_spb_bkp_file,Filename);
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if IBBkpVerbose in Options then
    spb:=spb+chr(isc_spb_verbose);
  spb:=spb+IBSPBParamSerialize(isc_spb_options,MakeBackupOptions(Options));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('Backup',FStatus);
    exit;
    end;
  if IBBkpVerbose in Options then
    result:=GetOutput('Backup')
  else if (IBBkpWait in Options) then
    WaitForServiceCompletion(0);
end;

function TFBAdmin.BackupMultiFile(const Database: string; Filenames: TStrings;
  FileSize: longint; Options: TIBBackupOptions; const RoleName: string): boolean;
var
  spb:string;
  i:integer;
begin
  result:=CheckConnected('BackupMultiFile');
  spb:=chr(isc_action_svc_backup)+IBSPBParamSerialize(isc_spb_dbname,Database);
  for i:=0 to Filenames.Count-1 do
    begin
    spb:=spb+IBSPBParamSerialize(isc_spb_bkp_file,Filenames[i]);
    spb:=spb+IBSPBParamSerialize(isc_spb_bkp_length,FileSize);
    end;
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if IBBkpVerbose in Options then
    spb:=spb+chr(isc_spb_verbose);
  spb:=spb+IBSPBParamSerialize(isc_spb_options,MakeBackupOptions(Options));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('BackupMultiFile',FStatus);
    exit;
    end;
  if IBBkpVerbose in Options then
    result:=GetOutput('BackupMultiFile')
  else if (IBBkpWait in Options) then
    WaitForServiceCompletion(0);
end;

Function TFBAdmin.ServiceRunning : Boolean;

const
  BUFFERSIZE=1000;

var
  res:integer;
  buffer: string;
  spb:string;

begin
  FOutput.Clear;
  spb:=chr(isc_info_svc_running);
  setlength(buffer,BUFFERSIZE);
  result:=isc_service_query(@FStatus[0], @FSvcHandle, nil, 0, nil, length(spb),
          @spb[1],BUFFERSIZE,@buffer[1])=0;
  if Not Result then
    CheckError('ServiceRunning',FSTatus);
  if (Buffer[1]=Char(isc_info_svc_running)) then
    begin
    res:=isc_vax_integer(@Buffer[2],4);
    Result:=res=1;
    end
  else
    IBRaiseError(0,'%s: Service status detection returned wrong result',[self.Name]);
end;

Function TFBAdmin.WaitForServiceCompletion(aTimeOut : Integer) : Boolean;

Var
  N : TDateTime;

begin
  N:=Now;
  Repeat
    Sleep(WaitInterval);
    Result:=not ServiceRunning;
  until Result or ((aTimeOut<>0) and (MilliSecondsBetween(Now,N)>aTimeOut*WaitInterval));
end;


function TFBAdmin.Restore(const Database, Filename: string;
  Options: TIBRestoreOptions; const RoleName: string): boolean;
var
  spb:string;
begin
  result:=CheckConnected('Restore');
  if not ((IBResReplace in Options) or (IBResCreate in Options)) then
    begin
    result:=false;
    IBRaiseError(0,SErrRestoreOptionsError,[self.Name]);
    exit;
    end;
  spb:=chr(isc_action_svc_restore)+IBSPBParamSerialize(isc_spb_dbname,Database)
    +IBSPBParamSerialize(isc_spb_bkp_file,Filename);
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if IBResVerbose in Options then
    spb:=spb+chr(isc_spb_verbose);
  if (IBResAMReadOnly in Options) or (IBResAMReadWrite in Options) then
    begin
    if (IBResAMReadOnly in Options) then   //ReadOnly overrides ReadWrite
      spb:=spb+chr(isc_spb_res_access_mode)+chr(isc_spb_res_am_readonly)
    else
      spb:=spb+chr(isc_spb_res_access_mode)+chr(isc_spb_res_am_readwrite);
    end;
  if (IBFixFssData in Options) and (FixFssDataCharSet > ' ') then
    spb:=spb+IBSPBParamSerialize(isc_spb_res_fix_fss_data, FixFssDataCharSet);
  if (IBFixFssMeta in Options) and (FixFssDataCharSet > ' ') then
    spb:=spb+IBSPBParamSerialize(isc_spb_res_fix_fss_metadata, FixFssDataCharSet);
  spb:=spb+IBSPBParamSerialize(isc_spb_options,MakeRestoreOptions(Options));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('Restore',FStatus);
    exit;
    end;
  if IBResVerbose in Options then
    result:=GetOutput('Restore')
  else if IBResWait in Options then
    WaitForServiceCompletion(0);
end;


function TFBAdmin.RestoreMultiFile(const Database: string; Filenames: TStrings;
  Options: TIBRestoreOptions; const RoleName: string): boolean;
var
  spb:string;
  i:integer;
begin
  result:=CheckConnected('RestoreMultiFile');
  if not ((IBResReplace in Options) or (IBResCreate in Options)) then
    begin
    result:=false;
    IBRaiseError(0,SErrRestoreMultiOptionsError,[self.Name]);
    exit;
    end;
  spb:=chr(isc_action_svc_restore)+IBSPBParamSerialize(isc_spb_dbname,Database);
  for i:=0 to Filenames.Count-1 do
    spb:=spb+IBSPBParamSerialize(isc_spb_bkp_file,Filenames[i]);
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if IBResVerbose in Options then
    spb:=spb+chr(isc_spb_verbose);
  if (IBResAMReadOnly in Options) or (IBResAMReadWrite in Options) then
    begin
    if (IBResAMReadOnly in Options) then   //ReadOnly overrides ReadWrite
      spb:=spb+chr(isc_spb_res_access_mode)+chr(isc_spb_res_am_readonly)
    else
      spb:=spb+chr(isc_spb_res_access_mode)+chr(isc_spb_res_am_readwrite);
    end;
  spb:=spb+IBSPBParamSerialize(isc_spb_options,MakeRestoreOptions(Options));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('RestoreMultiFile',FStatus);
    exit;
    end;
  if IBResVerbose in Options then
    result:=GetOutput('RestoreMultiFile');
end;

function TFBAdmin.AddUser(const UserName, Password: string; const RoleName: string;
  const GroupName: string; const FirstName: string; const MiddleName: string; const LastName: string;
  UserID: longint; GroupID: longint): boolean;
var
  spb:string;
begin
  result:=CheckConnected('AddUser');
  spb:=chr(isc_action_svc_add_user)+IBSPBParamSerialize(isc_spb_sec_username,copy(UserName,1,31))+
    IBSPBParamSerialize(isc_spb_sec_password,copy(Password,1,8));
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if GroupName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_groupname,copy(GroupName,1,31));
  if FirstName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_firstname,copy(FirstName,1,255));
  if MiddleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_middlename,copy(MiddleName,1,255));
  if LastName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_lastname,copy(LastName,1,255));
  if UserID<>0 then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_userid,UserID);
  if GroupID<>0 then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_groupid,GroupID);
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    CheckError('AddUser',FStatus);
end;

function TFBAdmin.ModifyUser(const UserName, Password: string; const RoleName: string;
  const GroupName: string; const FirstName: string; const MiddleName: string; const LastName: string;
  UserID: longint; GroupID: longint): boolean;
var
  spb:string;
begin
  result:=CheckConnected('ModifyUser');
  spb:=chr(isc_action_svc_modify_user)+IBSPBParamSerialize(isc_spb_sec_username,copy(UserName,1,31))+
    IBSPBParamSerialize(isc_spb_sec_password,copy(Password,1,8));
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  if GroupName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_groupname,copy(GroupName,1,31));
  if FirstName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_firstname,copy(FirstName,1,255));
  if MiddleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_middlename,copy(MiddleName,1,255));
  if LastName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_lastname,copy(LastName,1,255));
  if UserID<>0 then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_userid,UserID);
  if GroupID<>0 then
    spb:=spb+IBSPBParamSerialize(isc_spb_sec_groupid,GroupID);
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    CheckError('ModifyUser',FStatus);
end;

function TFBAdmin.DeleteUser(const UserName: string; const RoleName: string): boolean;
var
  spb:string;
begin
  result:=CheckConnected('DeleteUser');
  spb:=chr(isc_action_svc_delete_user)+IBSPBParamSerialize(isc_spb_sec_username,copy(UserName,1,31));
  if RoleName<>'' then
    spb:=spb+IBSPBParamSerialize(isc_spb_sql_role_name,copy(RoleName,1,31));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    CheckError('DeleteUser',FStatus);
end;

function TFBAdmin.GetUser(const UserName: string; var GroupName, FirstName,
  MiddleName, LastName: string; var UserID, GroupID: longint): boolean;
var
  spb:string;
  buffer:string;
  bufptr:integer;
const
  BUFFERSIZE=1000;
begin
  result:=CheckConnected('GetUser');
  spb:=chr(isc_action_svc_display_user)+IBSPBParamSerialize(isc_spb_sec_username,copy(UserName,1,31));
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('GetUser',FStatus);
    exit;
    end;
  //retrieve result
  spb:=chr(isc_info_svc_get_users);
  setlength(buffer,BUFFERSIZE);
  result:=isc_service_query(@FStatus[0], @FSvcHandle, nil, 0, nil, length(spb),
    @spb[1],BUFFERSIZE,@buffer[1])=0;
  if not result then
    begin
    CheckError('GetUser',FStatus);
    exit;
    end;
  bufptr:=4;
  if buffer[1]=chr(isc_info_svc_get_users) then
    begin
    if buffer[bufptr]=chr(isc_info_end) then
      begin
      result:=false;
      IBRaiseError(0,SErrUserDoesNotExist,[self.Name]);
      exit;
      end;
    while buffer[bufptr]<>chr(isc_info_end) do
      begin
      case buffer[bufptr] of
        chr(isc_spb_sec_username):GetIBString(buffer,bufptr);  //trash result
        chr(isc_spb_sec_groupname):GroupName:=GetIBString(buffer,bufptr);
        chr(isc_spb_sec_firstname):FirstName:=GetIBString(buffer,bufptr);
        chr(isc_spb_sec_middlename):MiddleName:=GetIBString(buffer,bufptr);
        chr(isc_spb_sec_lastname):LastName:=GetIBString(buffer,bufptr);
        chr(isc_spb_sec_userid):UserID:=GetIBLongint(buffer,bufptr);
        chr(isc_spb_sec_groupid):GroupID:=GetIBLongint(buffer,bufptr);
        else
          begin
          result:=false;
          IBRaiseError(0,SErrUserInvalidReply,[self.Name,ord(buffer[bufptr])]);
          exit;
          end;
        end;
      end;
    end;
end;

function TFBAdmin.GetUsers(Users: TStrings): boolean;
var
  spb:string;
  buffer:string;
  bufptr:integer;
const
  BUFFERSIZE=1000;
begin
  result:=CheckConnected('GetUsers');
  spb:=chr(isc_action_svc_display_user);
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('GetUsers',FStatus);
    exit;
    end;
  //retrieve result
  spb:=chr(isc_info_svc_get_users);
  setlength(buffer,BUFFERSIZE);
  result:=isc_service_query(@FStatus[0], @FSvcHandle, nil, 0, nil, length(spb),
    @spb[1],BUFFERSIZE,@buffer[1])=0;
  if not result then
    begin
    CheckError('GetUsers',FStatus);
    exit;
    end;
  bufptr:=4;
  Users.Clear;
  if buffer[1]=chr(isc_info_svc_get_users) then
    begin
    while buffer[bufptr]<>chr(isc_info_end) do
      begin
      case buffer[bufptr] of
        chr(isc_spb_sec_username):Users.Add(GetIBString(buffer,bufptr));
        chr(isc_spb_sec_groupname),
        chr(isc_spb_sec_firstname),
        chr(isc_spb_sec_middlename),
        chr(isc_spb_sec_lastname):GetIBString(buffer,bufptr);       //trash result
        chr(isc_spb_sec_userid),
        chr(isc_spb_sec_groupid):GetIBLongint(buffer,bufptr);       //trash result
        else
          begin
          result:=false;
          IBRaiseError(0,SErrUsersInvalidReply,[self.Name,ord(buffer[bufptr])]);
          exit;
          end;
        end;
      end;
    end;
end;


function TFBAdmin.GetDatabaseLog: boolean;
var
  spb:string;
begin
  result:=CheckConnected('GetLogFile');
  spb:=chr(isc_action_svc_get_ib_log);
  result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not result then
    begin
    CheckError('GetLogFile',FStatus);
    exit;
    end;
  result:=GetOutput('GetLogFile');
end;

function TFBAdmin.GetDatabaseStats(const Database:string;Options: TIBStatOptions; const TableNames: String): boolean;
var
  spb:string;
  param: Integer;
begin
  Result:=CheckConnected('GetDatabaseStats');
  param := 0;
  if (IBDataPages in Options) then
    param := param or isc_spb_sts_data_pages;
  if (IBDbLog in Options) then
    param := param or isc_spb_sts_db_log;
  if (IBHeaderPages in Options) then
    param := param or isc_spb_sts_hdr_pages;
  if (IBIndexPages in Options) then
    param := param or isc_spb_sts_idx_pages;
  if (IBSystemRelations in Options) then
    param := param or isc_spb_sts_sys_relations;
  if (IBRecordVersions in Options) then
    param := param or isc_spb_sts_record_versions;
  if (IBStatTables in Options) then
    param := param or isc_spb_sts_table;
  spb  := Char(isc_action_svc_db_stats)+IBSPBParamSerialize(isc_spb_dbname,Database)+
    IBSPBParamSerialize(isc_spb_options, param);
  if (IBStatTables in Options) and (TableNames <> '') then
    spb := spb+IBSPBParamSerialize(isc_spb_command_line, TableNames);
  Result:=isc_service_start(@FStatus[0], @FSvcHandle, nil, length(spb),
    @spb[1])=0;
  if not Result then
    begin
    CheckError('GetDatabaseStats',FStatus);
    exit;
    end;
  Result:=GetOutput('GetDatabaseStats');
end;

end.

