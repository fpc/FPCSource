{ Web server settings

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lHTTPSettings;

{$mode objfpc}{$H+}

interface

  function GetMimeFile: string;
  function GetPort: Word;
  function GetHTTPPath: string;
  function GetCGIPath: string;
  function GetCGIRoot: string;
  function GetScriptPathPrefix: string;
  function GetPHPCGIBinary: string;
  function GetPHPCGIPort: Word;
  function GetPHPCGIEnv: string;
  
  procedure InitSettings;

implementation

uses
  Classes, SysUtils, IniFiles, Process;

const
  DEF_PORT = '3880';
  DEF_PHPCGI_PORT = '4665';
  DEF_PHPCGI_ENV = 'PHP_FCGI_CHILDREN=5:PHP_FCGI_MAX_REQUESTS=10000';

var
  SettingsFile: TIniFile;
  HomeDir: string;
  CurDir: string;
  InitedSettings: Boolean = False;
  
function CreateDefaultIni(const aFilePath: string): TIniFile;

  procedure AddPath(const aName, aPath: string; const CD: Boolean = True);
  begin
    Result.WriteString('PATH', aName, aPath);
    if CD and not DirectoryExists(aPath) then
      if not CreateDir(aPath) then
        Writeln('Unable to create directory: ', aPath);
  end;
  
  procedure CopyFile(const FromF, ToF: string);
  const
    MAX_SIZE = 65536;
  var
    f1, f2: TFileStream;
    n: Integer;
    Buf: array[0..MAX_SIZE-1] of Byte;
  begin
    f1:=TFileStream.Create(FromF, fmOpenRead);
    f2:=TFileStream.Create(ToF, fmCreate);
    
    while f1.Position < f1.Size do begin
      n:=f1.Read(Buf, MAX_SIZE);
      f2.Write(Buf, n);
    end;
    
    f1.Free;
    f2.Free;
  end;
  
  procedure AddFile(const aName, aFile: string);
  begin
    Result.WriteString('PATH', aName, aFile);
    if not FileExists(aFile) then begin
      if FileExists(CurDir + 'mime.types') then
        CopyFile(CurDir + 'mime.types', aFile)
      else if FileExists('/etc/mime.types') then
        CopyFile('/etc/mime.types', aFile)
      else
        Writeln('Warning! File does not exist: ', aFile);
    end;
  end;
  
  function GetPHPCGI: string;
  const
    DIRS: array[1..5] of string = ('/usr/bin/', '/usr/local/bin/', '/usr/lib/',
                                   '/usr/lib/bin/', '/opt/bin/');
    FILES: array[1..3] of string = ('php-cgi', 'php5-cgi', 'php4-cgi');
  var
    i, j: Integer;
  begin
    Result:='';
    for i:=1 to High(DIRS) do
      for j:=1 to High(FILES) do
        if  FileExists(DIRS[i] + FILES[j])
        and (FileGetAttr(DIRS[i] + FILES[j]) and faDirectory <> faDirectory) then
          Exit(DIRS[i] + FILES[j]);
  end;

begin
  Writeln('Creating default configuration file in: ', aFilePath);
  if not DirectoryExists(ExtractFilePath(aFilePath)) then
    CreateDir(ExtractFilePath(aFilePath));
  Result:=TIniFile.Create(aFilePath);
  
  AddPath('httpdir', HomeDir + 'http_docs');
  AddPath('cgiroot', HomeDir + 'cgi-bin');
 {$ifndef WINDOWS}
  AddPath('cgipath', '/usr/local/bin:/usr/bin:/bin:' + HomeDir + 'bin', False);
 {$else}
  AddPath('cgipath', HomeDir + 'cgi-bin', False);
 {$endif}
  AddFile('mimetypes', HomeDir + 'mime.types');
  Result.WriteString('PATH', 'cgiprefix', 'cgi-bin' + PathDelim);
 {$ifndef WINDOWS}
  Result.WriteString('PATH', 'phpcgibin', GetPHPCGI);
 {$else}
  Result.WriteString('PATH', 'phpcgibin', 'php-cgi.exe');
 {$endif}
  Result.WriteString('NET', 'port', DEF_PORT);
  Result.WriteString('NET', 'phpcgiport', DEF_PHPCGI_PORT);
  Result.WriteString('ENV', 'phpcgienv', DEF_PHPCGI_ENV);
  Result.UpdateFile;
end;
  
procedure InitSettings;
const
  INI_NAME = 'fphttpd.ini';
var
  SearchPaths: TStringList;
  i: Integer;
begin
  if not InitedSettings then begin
    SearchPaths:=TStringList.Create;
    SearchPaths.Add(HomeDir);
    {$ifndef WINDOWS}
    SearchPaths.Add('/etc/');
    SearchPaths.Add('/usr/local/etc/');
    {$endif}
    SearchPaths.Add(ExtractFilePath(ParamStr(0)) + PathDelim);

    for i:=0 to SearchPaths.Count-1 do
      if FileExists(SearchPaths[i] + INI_NAME) then begin
        Writeln('Loading settings from file: ', SearchPaths[i] + INI_NAME);
        SettingsFile:=TIniFile.Create(SearchPaths[i] + INI_NAME);
        SearchPaths.Free;
        InitedSettings:=True;
        Exit;
      end;
    // no file found, create default one in home
    SettingsFile:=CreateDefaultIni(HomeDir + INI_NAME);
    SearchPaths.Free;
    InitedSettings:=True;
  end;
end;

procedure FreeSettings;
begin
  if InitedSettings then
    SettingsFile.Free;
end;

function GetMimeFile: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'mimetypes', '');
end;

function GetPort: Word;
begin
  Result:=Word(StrToInt(SettingsFile.ReadString('NET', 'port', DEF_PORT)));
end;

function GetHTTPPath: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'httpdir', '') + PathDelim;
end;

function GetCGIPath: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgipath', '') + PathDelim;
end;

function GetCGIRoot: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgiroot', '') + PathDelim;
end;

function GetScriptPathPrefix: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'cgiprefix', '') + PathDelim;
end;

function GetPHPCGIBinary: string;
begin
  Result:=SettingsFile.ReadString('PATH', 'phpcgibin', '');
end;

function GetPHPCGIPort: Word;
begin
  Result:=Word(StrToInt(SettingsFile.ReadString('NET', 'phpcgiport', DEF_PORT)));
end;

function GetPHPCGIEnv: string;
begin
  Result:=SettingsFile.ReadString('ENV', 'phpcgienv', DEF_PHPCGI_ENV);
end;
  
initialization
  CurDir:=ExtractFilePath(ParamStr(0));
  {$ifdef WINDOWS}
  HomeDir:=GetEnvironmentVariable('HOMEDRIVE') +
           GetEnvironmentVariable('HOMEPATH') + PathDelim + 'fphttpd' + PathDelim;
  {$else}
  HomeDir:=GetEnvironmentVariable('HOME') + PathDelim + '.fphttpd' + PathDelim;
  {$endif}

finalization
  FreeSettings;

end.

