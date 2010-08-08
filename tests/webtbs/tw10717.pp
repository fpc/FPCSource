program filefunctest;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  SysUtils;


type
  tsingletestresults = array[1..5] of string;
  tresults = array[1..5] of tsingletestresults;
const
{$ifdef unix}
  results: tresults =
  (
   (
    'ExtractFilePath on ''./:'' = ''./''',
    'ExtractFileName on ''./:'' = '':''',
    'ExtractFileDrive on ''./:'' = ''''',
    'IncludeTrailingPathDelimiter on ''./:'' = ''./:/''',
    'ExcludeTrailingPathDelimiter on ''./:'' = ''./:'''
   ),

   (
    'ExtractFilePath on ''C:/blah:blah'' = ''C:/''',
    'ExtractFileName on ''C:/blah:blah'' = ''blah:blah''',
    'ExtractFileDrive on ''C:/blah:blah'' = ''''',
    'IncludeTrailingPathDelimiter on ''C:/blah:blah'' = ''C:/blah:blah/''',
    'ExcludeTrailingPathDelimiter on ''C:/blah:blah'' = ''C:/blah:blah'''
   ),

   (
    'ExtractFilePath on ''./\'' = ''./''',
    'ExtractFileName on ''./\'' = ''\''',
    'ExtractFileDrive on ''./\'' = ''''',
    'IncludeTrailingPathDelimiter on ''./\'' = ''./\/''',
    'ExcludeTrailingPathDelimiter on ''./\'' = ''./\'''
   ),

   (
    'ExtractFilePath on ''./c:'' = ''./''',
    'ExtractFileName on ''./c:'' = ''c:''',
    'ExtractFileDrive on ''./c:'' = ''''',
    'IncludeTrailingPathDelimiter on ''./c:'' = ''./c:/''',
    'ExcludeTrailingPathDelimiter on ''./c:'' = ''./c:'''
   ),

   (
    'ExtractFilePath on ''\\server\share\file'' = ''''',
    'ExtractFileName on ''\\server\share\file'' = ''\\server\share\file''',
    'ExtractFileDrive on ''\\server\share\file'' = ''''',
    'IncludeTrailingPathDelimiter on ''\\server\share\file'' = ''\\server\share\file/''',
    'ExcludeTrailingPathDelimiter on ''\\server\share\file'' = ''\\server\share\file'''
   )
  );
{$else}
  results: tresults =
  (
   (
    'ExtractFilePath on ''./:'' = ''./:''',
    'ExtractFileName on ''./:'' = ''''',
    'ExtractFileDrive on ''./:'' = ''''',
    'IncludeTrailingPathDelimiter on ''./:'' = ''./:\''',
    'ExcludeTrailingPathDelimiter on ''./:'' = ''./:'''
   ),

   (
    'ExtractFilePath on ''C:/blah:blah'' = ''C:/blah:''',
    'ExtractFileName on ''C:/blah:blah'' = ''blah''',
    'ExtractFileDrive on ''C:/blah:blah'' = ''C:''',
    'IncludeTrailingPathDelimiter on ''C:/blah:blah'' = ''C:/blah:blah\''',
    'ExcludeTrailingPathDelimiter on ''C:/blah:blah'' = ''C:/blah:blah'''
   ),

   (
    'ExtractFilePath on ''./\'' = ''./\''',
    'ExtractFileName on ''./\'' = ''''',
    'ExtractFileDrive on ''./\'' = ''''',
    'IncludeTrailingPathDelimiter on ''./\'' = ''./\''',
    'ExcludeTrailingPathDelimiter on ''./\'' = ''./'''
   ),

   (
    'ExtractFilePath on ''./c:'' = ''./c:''',
    'ExtractFileName on ''./c:'' = ''''',
    'ExtractFileDrive on ''./c:'' = ''''',
    'IncludeTrailingPathDelimiter on ''./c:'' = ''./c:\''',
    'ExcludeTrailingPathDelimiter on ''./c:'' = ''./c:'''
   ),

   (
    'ExtractFilePath on ''\\server\share\file'' = ''\\server\share\''',
    'ExtractFileName on ''\\server\share\file'' = ''file''',
    'ExtractFileDrive on ''\\server\share\file'' = ''\\server\share''',
    'IncludeTrailingPathDelimiter on ''\\server\share\file'' = ''\\server\share\file\''',
    'ExcludeTrailingPathDelimiter on ''\\server\share\file'' = ''\\server\share\file'''
   )
  );
{$endif}

procedure TestFuncs(const strPath: string; const results: tsingletestresults);
begin
  WriteLn(Format('ExtractFilePath on ''%s'' = ''%s''',
                [strPath, ExtractFilePath(strPath)]));
  WriteLn(Format('ExtractFileName on ''%s'' = ''%s''',
                [strPath, ExtractFileName(strPath)]));
  WriteLn(Format('ExtractFileDrive on ''%s'' = ''%s''',
                [strPath, ExtractFileDrive(strPath)]));
  WriteLn(Format('IncludeTrailingPathDelimiter on ''%s'' = ''%s''',
                [strPath, IncludeTrailingPathDelimiter(strPath)]));
  WriteLn(Format('ExcludeTrailingPathDelimiter on ''%s'' = ''%s''',
                [strPath, ExcludeTrailingPathDelimiter(strPath)]));

  if (Format('ExtractFilePath on ''%s'' = ''%s''',[strPath, ExtractFilePath(strPath)]) <> results[1]) then
    begin
      writeln('ExtractFilePath: ',ExtractFilePath(strPath));
      halt(1);
    end;

  if (Format('ExtractFileName on ''%s'' = ''%s''',[strPath, ExtractFileName(strPath)]) <> results[2]) then
    begin
      writeln('ExtractFileName: ',ExtractFileName(strPath));
      halt(2);
    end;

  if (Format('ExtractFileDrive on ''%s'' = ''%s''',[strPath, ExtractFileDrive(strPath)]) <> results[3]) then
    begin
      writeln('ExtractFileDrive: ',ExtractFileDrive(strPath));
      halt(3);
    end;

  if (Format('IncludeTrailingPathDelimiter on ''%s'' = ''%s''',[strPath, IncludeTrailingPathDelimiter(strPath)]) <> results[4]) then
    begin
      writeln('IncludeTrailingPathDelimiter: ',IncludeTrailingPathDelimiter(strPath));
      halt(4);
    end;

  if (Format('ExcludeTrailingPathDelimiter on ''%s'' = ''%s''',[strPath, ExcludeTrailingPathDelimiter(strPath)]) <> results[5]) then
    begin
      writeln('ExcludeTrailingPathDelimiter: ',ExcludeTrailingPathDelimiter(strPath));
      halt(5);
    end;

  WriteLn;
end;

begin
{$ifdef unix}
  { make settings Kylix-compatible }
  AllowDriveSeparators:=[];
  AllowDirectorySeparators:=['/'];
{$endif}
  TestFuncs('./:',results[1]);
  TestFuncs('C:/blah:blah',results[2]);
  TestFuncs('./\',results[3]);
  TestFuncs('./c:',results[4]);
  TestFuncs('\\server\share\file',results[5]);
  writeln('ok');
end.
