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
  Version = '1.3';
  // 1.3 compile in a separate directory, so that parallel invocations do not overwrite link.res files


Procedure Usage;

begin
  writeln('instantfpc '+Version);
  writeln;
  writeln('Run pascal source files as scripts.');
  writeln('Normal usage is to add to a program source file a first line');
  writeln('("shebang") "#!/usr/bin/instantfpc".');
  writeln('Then you can execute the source directly in the terminal/console.');
  writeln;
  writeln('instantfpc -h');
  writeln('      Print this help message and exit.');
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
  writeln('      If the environment option INSTANTFPCOPTIONS is set it is');
  writeln('      passed to the compiler as first parameters.');
  writeln;
  writeln('instantfpc --get-cache');
  writeln('      Prints current cache directory and exit.');
  writeln;
  writeln('Options:');
  writeln;
  writeln('  --set-cache=<path to cache>');
  writeln('      Set the cache to be used. Otherwise using environment variable');
  writeln('      INSTANTFPCCACHE.');
  writeln;
  writeln('  --compiler=<path to compiler>');
  writeln('      Normally fpc is searched in PATH and used as compiler.');
  writeln;
  writeln('  --skip-run');
  writeln('      Do not execute the program. Useful to test if script compiles.');
  writeln('      You probably want to combine it with -B.');
  writeln;
  writeln('  -B');
  writeln('      Always recompile.');
  Halt(0);
end;

Procedure DisplayCache;
begin
  write(GetCacheDir);
  Halt(0);
end ;

var
  i,j: Integer;
  p: String;
  Filename: String;
  Src: TStringList;
  CacheDir: String;
  CacheFilename: String;
  OutputFilename: String;
  E : String;
  RunIt: boolean = true;
  
// Return true if filename found.
  
Function InterpretParam(p : String) : boolean;
begin
  Result:=False;
  if (P='') then exit;
  if p='-v' then 
    begin
    writeln('instantfpc '+Version);
    Halt(1);
    end
  else if p='-h' then 
    usage
  else if p='--get-cache' then 
    DisplayCache
  else if copy(p,1,11)='--compiler=' then 
    begin
    delete(P,1,11);
    SetCompiler(p);
    end 
  else if copy(p,1,12)='--set-cache=' then 
    begin
    delete(P,1,12);
    SetCacheDir(p);
    end 
  else if p='--skip-run' then
    begin
    RunIt:=false;
    end
  else if (P<>'') and (p[1]<>'-') then
    begin
    Filename:=p;
    Result:=True;
    end;
end;
  
begin
  Filename:='';
  { For example:
      /usr/bin/instantfpc -MObjFpc -Sh ./envvars.pas param1
  }
  for i:=1 to Paramcount do 
    begin
    p:=ParamStr(i);
    if p='' then
      continue
    else 
      begin
      if (I<>1) then
        begin
        if InterpretParam(p) then
          Break;
        end  
      else
        begin  
        // The linux kernel passes the whole shebang line as 1 argument. 
        // We must parse and split it ourselves.
        Repeat
          J:=Pos(' ',P);
          if (J=0) then
            J:=Length(P)+1;
          if InterpretParam(Copy(P,1,J-1)) then 
            Break;
          Delete(P,1,J);
        Until (P='');
        if (FileName<>'') then 
          Break;
        end;
      end;  
  end;
  if (Filename='') then 
    begin
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
    E:=LowerCase(ExtractFileExt(CacheFileName));
    if (E<>'.pp') and (E<>'.pas') and (E<>'.lpr') then
      CacheFileName:=CacheFileName+'.pas';
    OutputFilename:=CacheDir+ChangeFileExt(ExtractFileName(Filename),'');
    if not IsCacheValid(Src,CacheFilename,OutputFilename) then begin
      // save source in cache to find out next time if something changed
      Src.SaveToFile(CacheFilename);
      Compile(Filename,CacheFilename,OutputFilename);
    end;
    // run
    if RunIt then
      Run(OutputFilename);
  finally
    // memory is freed by OS, but for debugging puposes you can do it manually
    {$IFDEF IFFreeMem}
    Proc.Free;
    Src.Free;
    {$ENDIF}
  end;
end.

