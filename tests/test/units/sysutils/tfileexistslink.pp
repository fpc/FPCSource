{ %target=win32,win64 }
{ FileExists and DirectoryExists on junctions and symbolic links, issue #41150 }
program tfileexistslink;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils;

const
  FSCTL_SET_REPARSE_POINT = $900A4;
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  FILE_FLAG_OPEN_REPARSE_POINT = $200000;
  FILE_READ_EA = $8;
  SYMBOLIC_LINK_FLAG_DIRECTORY = 1;
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2;
  SDDL_REVISION_1 = 1;
  { read attributes, delete, read and write the ACL, synchronize }
  cRestrictedSD: UnicodeString = 'D:P(A;;0x170080;;;WD)';
  cOpenSD: UnicodeString = 'D:P(A;;FA;;;WD)';

function ConvertStringSecurityDescriptorToSecurityDescriptorW(aStringSD: PWideChar; aRevision: DWORD;
  out aSD: PSECURITY_DESCRIPTOR; aSize: PULONG): BOOL; stdcall; external 'advapi32';

function CreateSymbolicLinkW(aLinkName, aTargetName: PWideChar; aFlags: DWORD): BOOLEAN; stdcall; external 'kernel32';

var
  lFailed: Boolean;


// Write aMsg and mark the test as failed when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    lFailed:=True;
    end;
end;


// Open the reparse point aName itself with aAccess
function OpenLink(const aName: UnicodeString; aAccess: DWORD): THandle;

begin
  Result:=CreateFileW(PWideChar(aName),aAccess,FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,nil,
                      OPEN_EXISTING,FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,0);
end;


// True when aName exists as a reparse point
function IsReparsePoint(const aName: UnicodeString): Boolean;

var
  lAttr: DWORD;

begin
  lAttr:=GetFileAttributesW(PWideChar(aName));
  Result:=(lAttr<>INVALID_FILE_ATTRIBUTES) and ((lAttr and FILE_ATTRIBUTE_REPARSE_POINT)<>0);
end;


// Create directory aName as a junction to the absolute directory aTarget
function CreateJunction(const aName, aTarget: UnicodeString): Boolean;

var
  lSubst: UnicodeString;
  lBuf: array of Byte;
  lPath: PWideChar;
  lHandle: THandle;
  lReturned: DWORD;
  lDataLen: Integer;

begin
  Result:=False;
  if not CreateDirectoryW(PWideChar(aName),nil) then
    exit;
  lSubst:='\??\'+aTarget;
  lDataLen:=8+(Length(lSubst)+1+Length(aTarget)+1)*SizeOf(WideChar);
  SetLength(lBuf,8+lDataLen);
  FillChar(lBuf[0],Length(lBuf),0);
  PDWORD(@lBuf[0])^:=IO_REPARSE_TAG_MOUNT_POINT;
  PWord(@lBuf[4])^:=lDataLen;
  PWord(@lBuf[8])^:=0;
  PWord(@lBuf[10])^:=Length(lSubst)*SizeOf(WideChar);
  PWord(@lBuf[12])^:=(Length(lSubst)+1)*SizeOf(WideChar);
  PWord(@lBuf[14])^:=Length(aTarget)*SizeOf(WideChar);
  lPath:=PWideChar(@lBuf[16]);
  Move(PWideChar(lSubst)^,lPath^,Length(lSubst)*SizeOf(WideChar));
  Move(PWideChar(aTarget)^,lPath[Length(lSubst)+1],Length(aTarget)*SizeOf(WideChar));
  lHandle:=OpenLink(aName,GENERIC_WRITE);
  if lHandle=INVALID_HANDLE_VALUE then
    exit;
  Result:=DeviceIoControl(lHandle,FSCTL_SET_REPARSE_POINT,@lBuf[0],Length(lBuf),nil,0,@lReturned,nil);
  CloseHandle(lHandle);
  Result:=Result and IsReparsePoint(aName);
end;


// Create a symbolic link aName to aTarget, false when the account may not create links
function CreateSymLink(const aName, aTarget: UnicodeString; aDir: Boolean): Boolean;

var
  lFlags: DWORD;

begin
  lFlags:=SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
  if aDir then
    lFlags:=lFlags or SYMBOLIC_LINK_FLAG_DIRECTORY;
  Result:=CreateSymbolicLinkW(PWideChar(aName),PWideChar(aTarget),lFlags);
  if not Result then
    Result:=CreateSymbolicLinkW(PWideChar(aName),PWideChar(aTarget),lFlags and SYMBOLIC_LINK_FLAG_DIRECTORY);
  Result:=Result and IsReparsePoint(aName);
end;


// Set the ACL of the reparse point aName itself from the SDDL string aSD
function SetLinkSecurity(const aName, aSD: UnicodeString): Boolean;

var
  lSD: PSECURITY_DESCRIPTOR;
  lHandle: THandle;

begin
  Result:=False;
  if not ConvertStringSecurityDescriptorToSecurityDescriptorW(PWideChar(aSD),SDDL_REVISION_1,lSD,nil) then
    exit;
  lHandle:=OpenLink(aName,WRITE_DAC);
  if lHandle<>INVALID_HANDLE_VALUE then
    begin
    Result:=SetKernelObjectSecurity(lHandle,DACL_SECURITY_INFORMATION,lSD);
    CloseHandle(lHandle);
    end;
  LocalFree(HLOCAL(lSD));
end;


// True when the reparse data of aName cannot be read
function ReparseDataDenied(const aName: UnicodeString): Boolean;

var
  lHandle: THandle;

begin
  lHandle:=OpenLink(aName,FILE_READ_EA);
  Result:=lHandle=INVALID_HANDLE_VALUE;
  if not Result then
    CloseHandle(lHandle);
end;


var
  lDir, lTargetDir, lTargetFile: UnicodeString;
  lHaveSymLinks: Boolean;

begin
  lFailed:=False;
  lDir:=GetTempDir(False)+'tfileexistslink'+IntToStr(GetCurrentProcessId);
  lTargetDir:=lDir+'\targetdir';
  lTargetFile:=lDir+'\target.txt';
  if not ForceDirectories(lTargetDir) then
    begin
    writeln('cannot create ',lTargetDir);
    halt(1);
    end;
  FileClose(FileCreate(lTargetFile));

  if not CreateJunction(lDir+'\junction',lTargetDir) then
    writeln('note: cannot create junctions, junction checks skipped')
  else
    begin
    Check(2,DirectoryExists(lDir+'\junction'),'DirectoryExists on junction');
    Check(3,not FileExists(lDir+'\junction'),'FileExists on junction');
    Check(4,CreateJunction(lDir+'\deadjunction',lDir+'\missing'),'create dangling junction');
    Check(5,not DirectoryExists(lDir+'\deadjunction'),'DirectoryExists on dangling junction');
    Check(6,DirectoryExists(lDir+'\deadjunction',False),'DirectoryExists on dangling junction without following it');
    Check(7,CreateJunction(lDir+'\lockedjunction',lTargetDir),'create restricted junction');
    Check(8,SetLinkSecurity(lDir+'\lockedjunction',cRestrictedSD),'restrict junction');
    if not ReparseDataDenied(lDir+'\lockedjunction') then
      writeln('note: access restriction not enforced, restricted junction check is not conclusive');
    Check(9,DirectoryExists(lDir+'\lockedjunction'),'DirectoryExists on junction with unreadable reparse data');
    SetLinkSecurity(lDir+'\lockedjunction',cOpenSD);
    end;

  lHaveSymLinks:=CreateSymLink(lDir+'\filelink',lTargetFile,False);
  if not lHaveSymLinks then
    writeln('note: cannot create symbolic links, symbolic link checks skipped')
  else
    begin
    Check(21,FileExists(lDir+'\filelink'),'FileExists on file link');
    Check(22,not DirectoryExists(lDir+'\filelink'),'DirectoryExists on file link');
    Check(23,CreateSymLink(lDir+'\deadlink',lDir+'\missing.txt',False),'create dangling file link');
    Check(24,not FileExists(lDir+'\deadlink'),'FileExists on dangling file link');
    Check(25,FileExists(lDir+'\deadlink',False),'FileExists on dangling file link without following it');
    Check(26,CreateSymLink(lDir+'\dirlink',lTargetDir,True),'create directory link');
    Check(27,DirectoryExists(lDir+'\dirlink'),'DirectoryExists on directory link');
    Check(28,CreateSymLink(lDir+'\rellink','target.txt',False),'create relative file link');
    Check(29,FileExists(lDir+'\rellink'),'FileExists on relative file link');
    Check(30,CreateSymLink(lDir+'\lockedlink',lTargetFile,False),'create restricted file link');
    Check(31,SetLinkSecurity(lDir+'\lockedlink',cRestrictedSD),'restrict file link');
    if not ReparseDataDenied(lDir+'\lockedlink') then
      writeln('note: access restriction not enforced, restricted file link check is not conclusive');
    Check(32,FileExists(lDir+'\lockedlink'),'FileExists on file link with unreadable reparse data');
    SetLinkSecurity(lDir+'\lockedlink',cOpenSD);
    end;

  DeleteFileW(PWideChar(lDir+'\filelink'));
  DeleteFileW(PWideChar(lDir+'\deadlink'));
  DeleteFileW(PWideChar(lDir+'\rellink'));
  DeleteFileW(PWideChar(lDir+'\lockedlink'));
  RemoveDirectoryW(PWideChar(lDir+'\dirlink'));
  RemoveDirectoryW(PWideChar(lDir+'\junction'));
  RemoveDirectoryW(PWideChar(lDir+'\deadjunction'));
  RemoveDirectoryW(PWideChar(lDir+'\lockedjunction'));
  DeleteFileW(PWideChar(lTargetFile));
  RemoveDirectoryW(PWideChar(lTargetDir));
  RemoveDirectoryW(PWideChar(lDir));
  if lFailed then
    halt(1);
  writeln('ok');
end.
