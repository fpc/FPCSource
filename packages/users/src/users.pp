unit users;

Interface
{$mode delphi}
uses baseunix,pwd,shadow,grp,SysUtils,Classes;

Type
  EUserLookupError = Class(Exception);
  EGroupLookupError = Class(Exception);
  EShadowLookupError = Class(Exception);

{ User functions }

Function  getpwnam(Const UserName: String) : PPasswordRecord;
Procedure GetUserData(Const UserName : String; Var Data : TPasswordRecord); overload;
Procedure GetUserData(Uid : Integer; Var Data : TPasswordRecord); overload;
function  GetUserName(UID : Integer) : String;
function  GetUserId(Const UserName : String) : Integer;
function  GetUserGid(Const UserName : String) : Integer;
function  GetUserDir(Const UserName : String): String;
function  GetUserDescription(Const UserName : String): String;
Procedure GetUserList(List : Tstrings);overload;
Procedure GetUserList(List : TStrings; WithIDs : Boolean);overload;

{ Group functions }

Function  getgrnam(Const GroupName: String) : PGroup;
Procedure GetGroupData(Const GroupName : String; Var Data : TGroup); overload;
Procedure GetGroupData(Gid : Integer; Var Data : TGroup); overload;
function  GetGroupName(GID : Integer) : String;
function  GetGroupId(Const GroupName : String) : Integer;
Procedure GetGroupList(List : Tstrings);overload;
Procedure GetGroupList(List : TStrings; WithIDs : Boolean);overload;
Procedure GetGroupMembers(GID : Integer;List : TStrings);overload;
Procedure GetGroupMembers(Const GroupName : String;List : TStrings);overload;

{ Shadow password functions }

function getspnam(UserName : String): PPasswordFileEntry;
function sgetspent(Line : String): PPasswordFileEntry;
Procedure GetUserShadowData(Const UserName : String; Var Data : TPasswordFileEntry);overload;
Procedure GetUserShadowData(UID : Integer; Var Data : TPasswordFileEntry);overload;

{ Extra functions }

Function GetUserGroup(Const UserName : String) : String;

Implementation

ResourceString

EnoSuchUserName = 'Unknown username: "%s"';
EnoSuchUserID = 'Unknown user ID: %d';
EnoSuchGroupName = 'Unknown groupname: "%s"';
EnoSuchGroupID = 'Unknown group ID: %d';
ENoShadowEntry = 'No shadow file entry for "%s"';
EShadowNotPermitted = 'Not enough permissions to access shadow password file';

Function getpwnam(Const UserName: String) : PPasswordRecord;

begin
  Result:=pwd.getpwnam(Pchar(UserName));
end;

Procedure GetUserData(Const UserName : String; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=Getpwnam(UserName);
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserName,[UserName]);
end;

Procedure GetUserData(Uid : Integer; Var Data : TPasswordRecord);

Var P : PPasswordRecord;

begin
  P:=Getpwuid(Uid);
  If P<>Nil then
    Data:=P^
  else
    Raise EUserLookupError.CreateFmt(ENoSuchUserID,[Uid]);
end;

function GetUserName(UID : Integer) : String;

Var
  UserData : TPasswordRecord;

begin
  GetuserData(UID,UserData);
  Result:=strpas(UserData.pw_Name);
end;

function  GetUserId(Const UserName : String) : Integer;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_uid;
end;

function  GetUserGId(Const UserName : String) : Integer;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=UserData.pw_gid;
end;

function GetUserDir(Const UserName : String): String;

Var
  UserData : TPasswordRecord;

begin
  GetUserData(UserName,UserData);
  Result:=strpas(UserData.pw_dir);
end;

function  GetUserDescription(Const UserName : String): String;

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
  setpwent;
  try
    Repeat
      P:=getpwent;
      If P<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[P^.pw_uid,strpas(p^.pw_name)]))
        else
          List.Add(strpas(p^.pw_name));
        end;
    until (P=Nil);
  finally
    endpwent;
  end;
end;

{ ---------------------------------------------------------------------
    Group Functions
  ---------------------------------------------------------------------}


Function  getgrnam(Const GroupName: String) : PGroup;

begin
  Result:=grp.getgrnam(Pchar(GroupName));
end;

Procedure GetGroupData(Const GroupName : String; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=Getgrnam(GroupName);
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupName,[GroupName]);
end;

Procedure GetGroupData(Gid : Integer; Var Data : TGroup); overload;

Var P : PGroup;

begin
  P:=Getgrgid(gid);
  If P<>Nil then
    Data:=P^
  else
    Raise EGroupLookupError.CreateFmt(ENoSuchGroupID,[Gid]);
end;

function GetGroupName(GID : Integer) : String;

Var
  G : TGroup;

begin
  GetGroupData(Gid,G);
  Result:=StrPas(G.gr_name);
end;

function  GetGroupId(Const GroupName : String) : Integer;

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
  setgrent;
  try
    Repeat
      G:=getgrent;
      If G<>Nil then
        begin
        If WithIDs then
          List.Add(Format('%d=%s',[G^.gr_gid,strpas(G^.gr_name)]))
        else
          List.Add(strpas(G^.gr_name));
        end;
    until (G=Nil);
  finally
    endgrent;
  end;
end;

Function PCharListToStrings(P : PPChar; List : TStrings) : Integer;

begin
  List.Clear;
  While P^<>Nil do
    begin
    List.Add(StrPas(P^));
    P:=PPChar(PChar(P)+SizeOf(PChar));
    end;
  Result:=List.Count;
end;


Procedure GetGroupMembers(GID : Integer;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GID,G);
  PCharListToStrings(G.gr_mem,List);
end;

Procedure GetGroupMembers(Const GroupName : String;List : TStrings);

Var
  G : TGroup;

begin
  GetGroupData(GroupName,G);
  PCharListToStrings(g.gr_mem,List);
end;

{ Shadow password functions }

function getspnam(UserName : String): PPasswordFileEntry;

begin
  result:=shadow.getspnam(Pchar(UserName));
end;

function sgetspent(Line : String): PPasswordFileEntry;

begin
  Result:=shadow.sgetspent(Pchar(Line));
end;

Procedure GetUserShadowData(Const UserName : String; Var Data : TPasswordFileEntry);

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

Procedure GetUserShadowData(UID : Integer; Var Data : TPasswordFileEntry);

begin
  GetUserShadowData(GetUserName(UID),Data);
end;

{ Extra functions }

Function GetUserGroup(Const UserName : String) : String;

begin
  GetGroupName(GetUserGid(UserName));
end;

end.
