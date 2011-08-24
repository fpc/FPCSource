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


Procedure Usage;

begin
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
  writeln('instantfpc --set-cache=<path to cache>');
  writeln('      Set the cache to be used.');
  writeln;
  writeln('instantfpc --compiler=<path to compiler>');
  writeln('      Normally fpc is searched in PATH and used as compiler.');
  writeln;
  writeln('Normal usage is to add as first line ("shebang") "#!/usr/bin/instantfpc"');
  writeln('to a program source file. Then you can execute the source like a script.');
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
  ExeExt: String;
  E : String;
  
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

