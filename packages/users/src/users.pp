{$IFNDEF FPC_DOTTEDUNITS}
unit users;
{$ENDIF FPC_DOTTEDUNITS}

Interface
{$mode delphi}
{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Types,UnixApi.Base,UnixApi.Pwd,UnixApi.Grp, {$ifdef Linux} UnixApi.Shadow,{$endif}System.SysUtils,System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses UnixType,BaseUnix,pwd,grp, {$ifdef Linux} shadow,{$endif}SysUtils,Classes;
{$ENDIF FPC_DOTTEDUNITS}

Type
  EUserLookupError = Class(Exception);
  EGroupLookupError = Class(Exception);
  EShadowLookupError = Class(Exception);
  TPasswordRecord = Tpasswd;
  PPasswordRecord = ^TPasswordRecord;

{ User functions }

Function  getpwnam(Const UserName: AnsiString) : PPasswordRecord;
Procedure GetUserData(Const UserName : AnsiString; Var Data : TPasswordRecord); overload;
Procedure GetUserData(Uid : TUID; Var Data : TPasswordRecord); overload;
function  GetUserName(UID : TUID) : AnsiString;
function  GetUserId(Const UserName : AnsiString) : TUID;
function  GetUserGid(Const UserName : AnsiString) : TGID;
function  GetUserDir(Const UserName : AnsiString): AnsiString;
function  GetUserDescription(Const UserName : AnsiString): AnsiString;
Procedure GetUserList(List : Tstrings);overload;
Procedure GetUserList(List : TStrings; WithIDs : Boolean);overload;

{ Group functions }

Function  getgrnam(Const GroupName: AnsiString) : PGroup;
Procedure GetGroupData(Const GroupName : AnsiString; Var Data : TGroup); overload;
Procedure GetGroupData(Gid : TGID; Var Data : TGroup); overload;
function  GetGroupName(GID : TGID) : AnsiString;
function  GetGroupId(Const GroupName : AnsiString) : TGID;
Procedure GetGroupList(List : Tstrings);overload;
Procedure GetGroupList(List : TStrings; WithIDs : Boolean);overload;
Procedure GetGroupMembers(GID : TGID;List : TStrings);overload;
Procedure GetGroupMembers(Const GroupName : AnsiString;List : TStrings);overload;

{ Shadow password functions }

{$ifdef Linux}
function getspnam(UserName : AnsiString): PPasswordFileEntry;
function sgetspent(Line : AnsiString): PPasswordFileEntry;

Procedure GetUserShadowData(Const UserName : AnsiString; Var Data : TPasswordFileEntry);overload;
Procedure GetUserShadowData(UID : TUID; Var Data : TPasswordFileEntry);overload;
{$endif}

{ Extra functions }

Function GetUserGroup(Const UserName : AnsiString) : AnsiString;

Implementation

ResourceString

EnoSuchUserName = 'Unknown username: "%s"';
EnoSuchUserID = 'Unknown user ID: %d';
EnoSuchGroupName = 'Unknown groupname: "%s"';
EnoSuchGroupID = 'Unknown group ID: %d';
ENoShadowEntry = 'No shadow file entry for "%s"';
EShadowNotPermitted = 'Not enough permissions to access shadow password file';

Function getpwnam(Const UserName: AnsiString) : PPasswordRecord;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}Pwd.fpgetpwnam(PAnsiChar(UserName));
end;

Procedure GetUserData(Const UserName : AnsiString; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=fpGetpwnam(PAnsiChar(UserName));
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserName,[UserName]);
end;

Procedure GetUserData(Uid : tuid; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=fpGetpwuid(Uid);
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserID,[Uid]);
end;

function GetUserName(UID : TUID) : AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetuserData(UID,UserData);
  Result:=UserData.pw_Name;
end;

function  GetUserId(Const UserName : AnsiString) : TUID;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_uid;
end;

function  GetUserGId(Const UserName : AnsiString) : TGID;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_gid;
end;

function GetUserDir(Const UserName : AnsiString): AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_dir;
end;

function  GetUserDescription(Const UserName : AnsiString): AnsiString;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=strpas(UserData.pw_gecos);
end;

Procedure GetUserList(List : Tstrings);

begin
  GetUserList(List,False);
end;

Procedure GetUserList(List : TStrings; WithIDs : Boolean);

Var
  P : PPasswordRecord;

begin
  List.Clear;
  fpsetpwent;
  try
    Repeat
      P:=fpgetpwent;
      If P<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[P^.pw_uid,strpas(p^.pw_name)]))
        else
          List.Add(strpas(p^.pw_name));
        end;
    until (P=Nil);
  finally
    fpendpwent;
  end;
end;

{ ---------------------------------------------------------------------
    Group Functions
  ---------------------------------------------------------------------}


Function  getgrnam(Const GroupName: AnsiString) : PGroup;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}Grp.fpgetgrnam(PAnsiChar(GroupName));
end;

Procedure GetGroupData(Const GroupName : AnsiString; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=fpGetgrnam(PAnsiChar(GroupName));
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupName,[GroupName]);
end;

Procedure GetGroupData(Gid : TGID; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=fpGetgrgid(gid);
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupID,[Gid]);
end;

function GetGroupName(GID : TGID) : AnsiString;

Var
  G : TGroup;

begin
  GetGroupData(Gid,G);
  Result:=G.gr_name;
end;

function  GetGroupId(Const GroupName : AnsiString) : TGID;

Var
  G : TGroup;

begin
  GetGroupData(GroupName,G);
  Result:=G.gr_gid;
end;

Procedure GetGroupList(List : Tstrings);overload;

begin
  GetGroupList(List,False);
end;

Procedure GetGroupList(List : TStrings; WithIDs : Boolean);overload;

Var
  G : PGroup;

begin
  List.Clear;
  fpsetgrent;
  try
    Repeat
      G:=fpgetgrent;
      If G<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[G^.gr_gid,strpas(G^.gr_name)]))
        else
          List.Add(strpas(G^.gr_name));
        end;
    until (G=Nil);
  finally
    fpendgrent;
  end;
end;

Function PCharListToStrings(P : PPAnsiChar; List : TStrings) : Integer;

begin
  List.Clear;
  While P^<>Nil do
    begin
    List.Add(StrPas(P^));
    P:=PPAnsiChar(PAnsiChar(P)+SizeOf(PAnsiChar));
    end;
  Result:=List.Count;
end;


Procedure GetGroupMembers(GID : TGID;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GID,G);
  PCharListToStrings(G.gr_mem,List);
end;

Procedure GetGroupMembers(Const GroupName : AnsiString;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GroupName,G);
  PCharListToStrings(g.gr_mem,List);
end;

{ Shadow password functions }
{$ifdef linux}
function getspnam(UserName : AnsiString): PPasswordFileEntry;

begin
  result:={$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}Shadow.getspnam(PAnsiChar(UserName));
end;

function sgetspent(Line : AnsiString): PPasswordFileEntry;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}shadow.sgetspent(PAnsiChar(Line));
end;

Procedure GetUserShadowData(Const UserName : AnsiString; Var Data : TPasswordFileEntry);

Var
  P : PPasswordFileEntry;

begin
  P:=getspnam(UserName);
  If P=Nil then
    If (fpGetUID<>0) and (fpGetEUID<>0) then
      Raise EShadowLookupError.Create(EShadowNotPermitted)
    else
      Raise EShadowLookupError.CreateFmt(ENoShadowEntry,[UserName])
  else
    Data:=P^;
end;


Procedure GetUserShadowData(UID : TUID; Var Data : TPasswordFileEntry);

begin
  GetUserShadowData(GetUserName(UID),Data);
end;
{$endif}
{ Extra functions }

Function GetUserGroup(Const UserName : AnsiString) : AnsiString;

begin
  GetGroupName(GetUserGid(UserName));
end;

end.
