{ %target=win32,win64 }
{ FileGetDateTimeInfo following a symbolic link }
program tfiledtlink;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils;

const
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2;

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


// Create a symbolic link aName to the file aTarget, false when no link exists afterwards
function CreateSymLink(const aName, aTarget: UnicodeString): Boolean;

var
  lAttr: DWORD;

begin
  Result:=CreateSymbolicLinkW(PWideChar(aName),PWideChar(aTarget),SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE);
  if not Result then
    Result:=CreateSymbolicLinkW(PWideChar(aName),PWideChar(aTarget),0);
  lAttr:=GetFileAttributesW(PWideChar(aName));
  Result:=Result and (lAttr<>INVALID_FILE_ATTRIBUTES) and ((lAttr and FILE_ATTRIBUTE_REPARSE_POINT)<>0);
end;


var
  lDir, lTarget, lLink: string;
  lDate: TDateTime;
  lInfo: TDateTimeInfoRec;

begin
  lFailed:=False;
  lDir:=GetTempDir(False)+'tfiledtlink'+IntToStr(GetCurrentProcessId);
  lTarget:=lDir+'\target.txt';
  lLink:=lDir+'\link.txt';
  if not ForceDirectories(lDir) then
    begin
    writeln('cannot create ',lDir);
    halt(1);
    end;
  FileClose(FileCreate(lTarget));
  lDate:=EncodeDate(2001,2,3)+EncodeTime(4,5,6,0);
  Check(1,FileSetDate(lTarget,DateTimeToFileDate(lDate))=0,'set date of target');
  if not CreateSymLink(UnicodeString(lLink),UnicodeString(lTarget)) then
    writeln('note: cannot create symbolic links, checks skipped')
  else
    begin
    Check(2,FileGetDateTimeInfo(lLink,lInfo,True),'FileGetDateTimeInfo follows file link');
    Check(3,Abs(lInfo.TimeStamp-lDate)<2/SecsPerDay,'TimeStamp of followed link is the target date');
    Check(4,FileGetDateTimeInfo(lLink,lInfo,False),'FileGetDateTimeInfo on file link itself');
    end;
  DeleteFile(lLink);
  DeleteFile(lTarget);
  RemoveDir(lDir);
  if lFailed then
    halt(1);
  writeln('ok');
end.
