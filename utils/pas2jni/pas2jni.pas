{
    pas2jni - JNI bridge generator for Pascal.

    Copyright (c) 2013 by Yury Sidorov.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}

{$mode objfpc}{$H+}
program pas2jni;

uses SysUtils, Classes, writer, ppuparser;

var
  w: TWriter;

procedure ShowUsage;
begin
  writeln('Usage: ', ChangeFileExt(ExtractFileName(ParamStr(0)), ''), ' [options] <unit> [<unit2> <unit3> ...]');
  writeln;
  writeln('Options:');
  writeln('  -U<path> - Unit search path, semicolon delimited. Wildcards are allowed.');
  writeln('  -L<name> - Set output library name.');
  writeln('  -P<name> - Set Java package name.');
  writeln('  -O<path> - Set output path for Pascal files.');
  writeln('  -J<path> - Set output path for Java files.');
  writeln('  -D<prog> - Set full path to the "ppudump" program.');
  writeln('  -I<list> - Include the list of specified objects in the output. The list is');
  writeln('             semicolon delimited. To read the list from a file use -I@<file>');
  writeln('  -E<list> - Exclude the list of specified objects from the output. The list is');
  writeln('             semicolon delimited. To read the list from a file use -E@<file>');
  writeln('  -?       - Show this help information.');
end;

function GetListParam(const p: string): TStringList;
var
  fs: TFileStream;
  r: string;
begin
  if Copy(p, 1, 1) = '@' then begin
    fs:=TFileStream.Create(Copy(p, 2, MaxInt), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(r, fs.Size);
      if r <> '' then
        fs.ReadBuffer(PChar(r)^, fs.Size);
    finally
      fs.Free;
    end;
  end
  else
    r:=p;
  r:=StringReplace(r, ';', LineEnding, [rfReplaceAll]);
  Result:=TStringList.Create;
  Result.Text:=r;
end;

function ParseCmdLine: boolean;
var
  i: integer;
  s, ss: string;
  sl: TStringList;
begin
  Result:=False;
  if ParamCount = 0 then begin
    ShowUsage;
    ExitCode:=1;
    exit;
  end;
  for i:=1 to Paramcount do begin
    s:=ParamStr(i);
    if Copy(s, 1, 1) = '-' then begin
      Delete(s, 1, 1);
      if s = '' then
        continue;
      case s[1] of
        'U':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            if w.SearchPath <> '' then
              w.SearchPath:=w.SearchPath + ';';
            w.SearchPath:=w.SearchPath + s;
          end;
        'L':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            w.LibName:=s;
          end;
        'P':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            w.JavaPackage:=s;
          end;
        'O':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            w.OutPath:=s;
            if w.JavaOutPath = '' then
              w.JavaOutPath:=s;
          end;
        'J':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            w.JavaOutPath:=s;
          end;
        'D':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            ppudumpprog:=s;
          end;
        'I':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            sl:=GetListParam(s);
            w.IncludeList.AddStrings(sl);
            sl.Free;
          end;
        'E':
          begin
            Delete(s, 1, 1);
            if s = '' then
              continue;
            sl:=GetListParam(s);
            w.ExcludeList.AddStrings(sl);
            sl.Free;
          end;
        '?', 'H':
          begin
            ShowUsage;
            exit;
          end;
        else
          begin
            writeln('Illegal parameter: -', s);
            ExitCode:=1;
            exit;
          end;
      end;
    end
    else begin
      ss:=ExtractFilePath(s);
      if ss <> '' then begin
        if w.SearchPath <> '' then
          w.SearchPath:=w.SearchPath + ';';
        w.SearchPath:=w.SearchPath + ss;
      end;
      w.Units.Add(ExtractFileName(s));
    end;
  end;
  Result:=True;
end;

begin
  try
    w:=TWriter.Create;
    try
      if ParseCmdLine then
        w.ProcessUnits;
    finally
      w.Free;
    end;
  except
    writeln(Exception(ExceptObject).Message);
    ExitCode:=2;
  end;
end.

