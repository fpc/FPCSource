{$mode objfpc}
{$H+}
unit fpdeopts;

Interface

uses SysUtils,IniFiles;

Var
  SkipEmptyNodes   : Boolean;
  ConfirmDelete    : Boolean;
  CreateBackup     : Boolean;
  MaxRecentUsed    : Integer;
  BackupExtension  : String;
  DefaultExtension : String;

Procedure LoadOptions;
Procedure SaveOptions;
Function  GetOptionFileName : String;

Implementation

Const
  DefFilename         = 'fpde.ini';
  SecPrefs            = 'Preferences';
  KeySkipEmptyNodes   = 'SkipEmptyNodes';
  KeyConfirmDelete    = 'ConfirmDelete';
  KeyCreateBackup     = 'CreateBackup';
  KeyBackupExtension  = 'BackupExtension';
  KeyDefaultExtension = 'DefaultExtension';
  KeyMaxRecentUsed    = 'MaxMRUitems';

{$ifndef win32}
Function GetOptionFileName : String;

Const
  fpdedir = '.fpde';

Var
  HomeDir : String;

begin
  HomeDir:=GetEnvironmentVariable('HOME');
  If (HomeDir<>'') then
    begin
    HomeDir:=IncludeTrailingPathDelimiter(HomeDir)+fpdedir;
    If not DirectoryExists(HomeDir) then
      If Not CreateDir(HomeDir) then
        HomeDir:=''
      else
        HomeDir:=HomeDir;
    end;
  Result:=IncludeTrailingPathDelimiter(HomeDir)+DefFileName;
end;

{$else}

Function GetOptionFileName : String;

begin
  Result:=ExtractFilePath(Paramstr(0))+DefFileName;
end;
{$endif}

Procedure LoadOptions;

begin
  With TInifile.Create(GetOptionFileName) do
    Try
      SkipEmptyNodes:=ReadBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      ConfirmDelete:=ReadBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      CreateBackup:=ReadBool(SecPrefs,KeyCreateBackup,CreateBackup);
      BackupExtension:=ReadString(SecPrefs,KeyBackupExtension,BackupExtension);
      DefaultExtension:=ReadString(SecPrefs,KeyDefaultExtension,DefaultExtension);
    finally
      Free;
    end;
end;

Procedure SaveOptions;

begin
  With TInifile.Create(GetOptionFileName) do
    Try
      WriteBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      WriteBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      WriteBool(SecPrefs,KeyCreateBackup,CreateBackup);
      WriteString(SecPrefs,KeyBackupExtension,BackupExtension);
      WriteString(SecPrefs,KeyDefaultExtension,DefaultExtension);
      UpdateFile;
    finally
      Free;
    end;
end;

Initialization
  SkipEmptyNodes   := True;
  ConfirmDelete    := True;
  CreateBackup     := True;
  BackupExtension  := '.~xml';
  DefaultExtension := '.xml';
  MaxRecentUSed    := 10;
end.
