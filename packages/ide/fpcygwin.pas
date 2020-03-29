unit fpcygwin;

interface

function GetCygwinVersionString : string;
function GetCygwinFullName : string;
function GetCygwinVersionHigh : longint;
function GetCygwinVersionLow : longint;
procedure CheckCygwinVersion;

implementation

uses
  strings,
  windows;

var
  ModuleH,DummyH : Handle;
  CygwinFullName : pchar;
  fileinfosize : cardinal;
  size : longint;
  InfoData : pointer;
  FileInfo : VS_FIXEDFILEINFO;
  PFileInfo : ^VS_FIXEDFILEINFO;

function GetCygwinVersionHigh : longint;
begin
    GetCygwinVersionHigh:=FileInfo.dwFileVersionMS;
end;

function GetCygwinVersionLow : longint;
begin
  GetCygwinVersionLow:=FileInfo.dwFileVersionLS;
end;

function GetCygwinVersionString : string;
var
  a,b,c,d : word;
  va,vb,vc,vd : string[6];
begin
  a:= Cardinal(GetCygwinVersionHigh) shr 16;
  b:= Cardinal(GetCygwinVersionHigh) and $ffff;
  c:= Cardinal(GetCygwinVersionLow) shr 16;
  d:= Cardinal(GetCygwinVersionLow) and $ffff;
  system.str(a,va);
  system.str(b,vb);
  system.str(c,vc);
  system.str(d,vd);
  GetCygwinVersionString:=va+'.'+vb+'.'+vc+'.'+vd;
end;

procedure CheckCygwinVersion;
begin
  if GetCygwinVersionHigh < 1005 shl 16 then
    begin
      Writeln('The cygwin1.dll that you have in "',CygwinFullName,'" is too old');
      Writeln('If the IDE does not work correctly, please consider');
      Writeln('putting a newer cygwin1.dll version in your path before that one.');
    end;
end;

function GetCygwinFullName : string;
begin
  if assigned(CygwinFullName) then
    GetCygwinFullName:=strpas(CygwinFullName)
  else
    GetCygwinFullName:='cygwin1.dll';
end;

initialization
  ModuleH:=GetModuleHandle('cygwin1');
  GetMem(CygwinFullName,MAX_PATH+1);
  GetModuleFileName(ModuleH,CygwinFullName,MAX_PATH+1);
  size:=GetFileVersionInfoSize(CygwinFullName,@DummyH);
  GetMem(InfoData,size);
  if GetFileVersionInfo(CygwinFullName,DummyH,size,InfoData) then
    begin
      PFileInfo:=nil;
      VerQueryValue(InfoData,'\',PFileInfo,fileinfosize);
      If Assigned(PFileInfo) then
        FileInfo:=PFileInfo^;
    end;
  FreeMem(InfoData,size);
finalization
  FreeMem(CygwinFullName,MAX_PATH+1);
end.
