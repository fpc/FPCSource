{ Compile and run a pascal program.

  Copyright (C) 2011  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
program instantfpc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, InstantFPTools;

const
  Version = '1.0';

var
  i: Integer;
  p: String;
  Filename: String;
  Src: TStringList;
  CacheDir: String;
  CacheFilename: String;
  OutputFilename: String;
  ExeExt: String;
begin
  Filename:='';
  { For example:
      /usr/bin/instantfpc -MObjFpc -Sh ./envvars.pas param1
  }
  for i:=1 to Paramcount do begin
    p:=ParamStr(i);
    //writeln('Param: ',i,' ',p);
    if p='' then
      continue
    else if p='-v' then begin
      writeln('instantfpc '+Version);
      Halt(1);
    end
    else if p='-h' then begin
      writeln('instantfpc '+Version);
      writeln;
      writeln('instantfpc -h');
      writeln('      This help message.');
      writeln;
      writeln('instantfpc -v');
      writeln('      Print version and exit.');
      writeln;
      writeln('instantfpc [compiler options] <source file> [program parameters]');
      writeln('      Compiles source and runs program.');
      writeln('      Source is compared with the cache. If cache is not valid then');
      writeln('      source is copied to cache with the shebang line commented and');
      writeln('      cached source is compiled.');
      writeln('      If compilation fails the fpc output is written to stdout and');
      writeln('      instantfpc exits with error code 1.');
      writeln('      If compilation was successful the program is executed.');
      writeln('      If the compiler options contains -B the program is always');
      writeln('      compiled.');
      writeln;
      writeln('instantfpc --get-cache');
      writeln('      Prints cache directory to stdout.');
      writeln;
      writeln('instantfpc --compiler=<path to compiler>');
      writeln('      Normally fpc is searched in PATH and used as compiler.');
      writeln;
      writeln('Normal usage is to add as first line ("shebang") "#!/usr/bin/instantfpc"');
      writeln('to a program source file. Then you can execute the source like a script.');
      Halt(0);
    end else if p='--get-cache' then begin
      CacheDir:=GetCacheDir;
      write(CacheDir);
      Halt(0);
    end else if (p[1]<>'-') then begin
      // the first non flag parameter is the file name of the script
      // followed by the parameters for the script
      Filename:=p;
      break;
    end;
  end;
  if Filename='' then begin
    writeln('missing source file');
    Halt(1);
  end;

  CheckSourceName(Filename);

  Src:=TStringList.Create;
  try
    Src.LoadFromFile(Filename);
    CommentShebang(Src);
    CacheDir:=GetCacheDir;

    // check cache
    CacheFilename:=CacheDir+ExtractFileName(Filename);
    ExeExt:='';
    OutputFilename:=CacheDir+ChangeFileExt(ExtractFileName(Filename),ExeExt);
    if not IsCacheValid(Src,CacheFilename,OutputFilename) then begin
      // save source in cache to find out next time if something changed
      Src.SaveToFile(CacheFilename);
      Compile(CacheFilename,OutputFilename);
    end;
    // run
    Run(OutputFilename);
  finally
    // memory is freed by OS, but for debugging puposes you can do it manually
    {$IFDEF IFFreeMem}
    Proc.Free;
    Src.Free;
    {$ENDIF}
  end;
end.

