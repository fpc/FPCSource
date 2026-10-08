{ %target=win32,win64 }
{ GetEnvironmentVariable and GetFileVersion against the Windows API, issue #33764 }
program tgetenvwin;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils;

// Halt with aCode and write aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Return the value of aName read directly with GetEnvironmentVariableW
function ApiValue(const aName: UnicodeString): UnicodeString;

var
  lLen: DWORD;

begin
  lLen:=GetEnvironmentVariableW(PWideChar(aName),nil,0);
  if lLen=0 then
    exit('');
  SetLength(Result,lLen);
  lLen:=GetEnvironmentVariableW(PWideChar(aName),PWideChar(Result),lLen);
  SetLength(Result,lLen);
end;


// Return the Windows system directory
function ApiSystemDir: UnicodeString;

var
  lBuf: array[0..MAX_PATH] of WideChar;

begin
  SetString(Result,PWideChar(@lBuf),GetSystemDirectoryW(@lBuf,Length(lBuf)));
end;


// Return dwFileVersionMS of aFile read with the W version API
function ApiFileVersion(const aFile: UnicodeString): Cardinal;

var
  lSize, lHandle: DWORD;
  lBlock: array of Byte;
  lInfo: Pointer;
  lLen: UINT;

begin
  Result:=0;
  lSize:=GetFileVersionInfoSizeW(PWideChar(aFile),@lHandle);
  if lSize=0 then
    exit;
  SetLength(lBlock,lSize);
  if GetFileVersionInfoW(PWideChar(aFile),0,lSize,@lBlock[0]) and
     VerQueryValueW(@lBlock[0],'\',lInfo,lLen) then
    Result:=PVSFixedFileInfo(lInfo)^.dwFileVersionMS;
end;


const
  cValue: UnicodeString = 'Gr'#$00FC#$00DF'e '#$0416' '#$20AC;
  cLatin: UnicodeString = 'Gr'#$00FC#$00DF'e';
  cUpperName: UnicodeString = #$00C4#$00D6#$00DC'_VAR';
  cLowerName: UnicodeString = #$00E4#$00F6#$00FC'_var';
  cFileName: UnicodeString = 'tgetenvwin_'#$0416#$03A9#$4E2D'.dll';

var
  lU, lLong, lSys, lCopy: UnicodeString;
  lA: AnsiString;
  lVer: Cardinal;

begin
  lU:=GetEnvironmentVariable(UnicodeString('PATH'));
  Check(1,(lU<>'') and (lU=ApiValue('PATH')),'PATH unicode equals API');
  Check(2,GetEnvironmentVariable(AnsiString('PATH'))=AnsiString(ApiValue('PATH')),'PATH ansi equals API');

  SetEnvironmentVariableW('TGETENVWIN_MiXeD','abc');
  Check(3,GetEnvironmentVariable(UnicodeString('tgetenvwin_mixed'))='abc','ascii name case-insensitive unicode');
  Check(4,GetEnvironmentVariable(AnsiString('TGETENVWIN_mixed'))='abc','ascii name case-insensitive ansi');

  SetEnvironmentVariableW(PWideChar(cUpperName),'found');
  if ApiValue(cLowerName)='found' then
    Check(5,GetEnvironmentVariable(cLowerName)='found','non-ascii name case-insensitive unicode');

  SetEnvironmentVariableW('TGETENVWIN_VALUE',PWideChar(cValue));
  Check(6,GetEnvironmentVariable(UnicodeString('TGETENVWIN_VALUE'))=cValue,'non-ascii value unicode');
  SetEnvironmentVariableW('TGETENVWIN_LATIN',PWideChar(cLatin));
  lA:=GetEnvironmentVariable(AnsiString('TGETENVWIN_LATIN'));
  Check(7,lA=AnsiString(cLatin),'non-ascii value ansi equals the value converted to the ansi code page');

  Check(8,GetEnvironmentVariable(UnicodeString('TGETENVWIN_MISSING'))='','missing variable unicode');
  Check(9,GetEnvironmentVariable(AnsiString('TGETENVWIN_MISSING'))='','missing variable ansi');
  Check(10,GetEnvironmentVariable(UnicodeString(''))='','empty name unicode');
  Check(11,GetEnvironmentVariable(AnsiString(''))='','empty name ansi');
  SetEnvironmentVariableW('TGETENVWIN_EMPTY','');
  Check(12,GetEnvironmentVariable(UnicodeString('TGETENVWIN_EMPTY'))='','empty value');

  lLong:=StringOfChar(WideChar('x'),5000)+cValue;
  SetEnvironmentVariableW('TGETENVWIN_LONG',PWideChar(lLong));
  Check(13,GetEnvironmentVariable(UnicodeString('TGETENVWIN_LONG'))=lLong,'long value unicode');
  Check(14,GetEnvironmentVariable(AnsiString('TGETENVWIN_LONG'))=AnsiString(lLong),'long value ansi');

  lSys:=ApiSystemDir;
  lVer:=ApiFileVersion(lSys+'\kernel32.dll');
  if lVer<>0 then
    begin
    Check(21,GetFileVersion(lSys+'\kernel32.dll')=lVer,'GetFileVersion unicode equals API');
    Check(22,GetFileVersion(AnsiString(lSys+'\kernel32.dll'))=lVer,'GetFileVersion ansi equals API');
    lCopy:=GetTempDir(False)+cFileName;
    if CopyFileW(PWideChar(lSys+'\kernel32.dll'),PWideChar(lCopy),False) then
      begin
      lVer:=GetFileVersion(lCopy);
      DeleteFileW(PWideChar(lCopy));
      Check(23,lVer=ApiFileVersion(lSys+'\kernel32.dll'),'GetFileVersion unicode with non-ansi file name');
      end;
    end;

  DefaultSystemCodePage:=CP_UTF8;
  lA:=GetEnvironmentVariable(AnsiString('TGETENVWIN_VALUE'));
  Check(31,UnicodeString(lA)=cValue,'non-ascii value ansi with DefaultSystemCodePage=CP_UTF8');
  Check(32,GetEnvironmentVariable(UTF8Encode(cUpperName))='found','non-ascii name ansi with DefaultSystemCodePage=CP_UTF8');
  writeln('ok');
end.
