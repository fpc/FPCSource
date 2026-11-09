{ %target=win32,win64 }
{ In a Unicode RTL (the unicodertl subtarget) TSearchRec, TSymLinkRec and GetCurrentDir follow string, so a name
  the system code page cannot hold survives FindFirst and GetCurrentDir. An Ansi RTL keeps its narrow forms. }
program tsearchrecrtl;

{$mode delphiunicode}

uses
  SysUtils;

{$ifdef FPC_SUBTARGET_UNICODERTL}
const
  cWide = UnicodeChar($0101);
var
  lDir, lFile, lSaved: UnicodeString;
  lRec: TSearchRec;
  lHandle: THandle;
{$endif}

begin
{$ifdef FPC_SUBTARGET_UNICODERTL}
  if SizeOf(lRec.Name[1]) <> 2 then
    Halt(1);
  lDir := GetTempDir(False) + 'tsearchrecrtl' + IntToStr(GetProcessID) + cWide;
  if not CreateDir(lDir) then
    Halt(2);
  lFile := 'f' + cWide + '.txt';
  lHandle := FileCreate(lDir + '\' + lFile);
  if lHandle = THandle(-1) then
    Halt(3);
  FileClose(lHandle);

  if FindFirst(lDir + '\*.txt', faAnyFile, lRec) <> 0 then
    Halt(4);
  if lRec.Name <> lFile then
    Halt(5);
  FindClose(lRec);

  lSaved := GetCurrentDir;
  if not SetCurrentDir(lDir) then
    Halt(6);
  if Pos(cWide, GetCurrentDir) = 0 then
    Halt(7);
  if not SetCurrentDir(lSaved) then
    Halt(8);

  DeleteFile(lDir + '\' + lFile);
  RemoveDir(lDir);
{$endif}
  WriteLn('ok');
end.
