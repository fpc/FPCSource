{
    Copyright (c) 2000-2002 by Pierre Muller

    This program allows to run the Makefiles
    with the compiler running inside valgrind

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

{$mode objfpc}
{ Use ansistrings for long PATH variables }
{$H+}
program fpc_with_valgrind;

{
  This program uses:

   -- 'valgrind.fpc' is an optional file in starting directory
      that can contain optional command line parameters for valgrind.
   -- 'VALGRIND_FPC' is an optional environment variable
      that can contain optional command line parameters for valgrind.

  Note that valgrind also parses:
   -- '~/.valgrindrc' user file.
   -- 'VALGRIND_OPTS' environment variable.
   -- './.valgrindrc' local file.

  Use EXTDEBUG conditional to get debug information.
}

uses
  sysutils,
  dos;

const
{$ifdef Unix}
  ValgrindPasExeName : String = 'valgrindpas';
  ValgrindDefaultExeName = 'valgrind';
  DefaultCompilerName = 'ppcx64';
  PathSep=':';
  DirSep = '/';
{$else}
  ValgrindPasExeName : String = 'valgrindpas.exe';
  ValgrindDefaultExeName = 'valgrind.exe';
  DefaultCompilerName = 'ppcx64.exe';
  PathSep=';';
  DirSep = '\';
{$endif not linux}

  { If you add a valgrind.fpc file in a given directory   }
  { This executable will read it; this allows you to add  }
  { specific command line options to valgrind call. PM    }
  FpcValgrindIniName  : string = 'valgrind.fpc';

{ Dos/Windows Valgrind still need forward slashes }
procedure AdaptToValgrind(var filename : string);
var
  i : longint;
begin
  for i:=1 to length(filename) do
    if filename[i]='\' then
      filename[i]:='/';
end;

var
   valgrind_args,
   env_value,
   all_args : String;
   ValGrindExeName : String;
   CompilerName : String;
   FullCompilerName : String;
{$ifdef linux}
   argv0 : pchar;
{$endif}
   Dir,Name,Ext,Param : ShortString;
   ValgrindExitCode,i : longint;
   line : string;
   f : text;

begin
  all_args:='';
  valgrind_args:='';
  if FileExists('.'+DirSep+FpcValgrindIniName) then
    begin
      Assign(F,'.'+DirSep+FpcValgrindIniName);
      Reset(F);
      while not eof(F) do
        begin
          readln(f,line);
	  valgrind_args:=valgrind_args+' '+line;
	end;
      Close(F);
    end;
  env_value:=GetEnvironmentVariable('VALGRIND_FPC');
  if env_value<>'' then
    valgrind_args:=valgrind_args+' '+env_value;

  fsplit(paramstr(0),Dir,Name,Ext);
{$ifdef linux}
  argv0:=argv[0];
  if (argv0 <> '') then
    fsplit(argv0,Dir,Name,Ext);
{$endif}

  if (length(Name)>3) and (UpCase(Name[1])='V') then
    CompilerName:=Copy(Name,2,255)+Ext
  else
    begin
      if (Name+ext = DefaultCompilerName) then
        begin
          writeln(stderr,'Avoiding infinite recursion with ',Name+Ext,' binary');
          halt(1);
        end;
      CompilerName:=DefaultCompilerName;
    end;

  if FileExists(Dir+CompilerName) then
    FullCompilerName:=Dir+CompilerName
  else
    FullCompilerName:=filesearch(CompilerName,Dir+PathSep+GetEnvironmentVariable('PATH'));

  if FullCompilerName='' then
    begin
      writeln(stderr,'Unable to find ',CompilerName,' binary');
      halt(2);
    end;


  { support for info functions directly : used in makefiles }
  if (paramcount=1) and (pos('-i',Paramstr(1))=1) then
    begin
      Exec(FullCompilerName,Paramstr(1));
      exit;
    end;

  {$ifdef EXTDEBUG}
  writeln(stderr,'Using compiler "',FullCompilerName,'"');
  flush(stderr);
  {$endif}
  { this will not work correctly if there are " or '' inside the command line :( }
  for i:=1 to Paramcount do
    begin
      Param:=Paramstr(i);
      if pos(' ',Param)>0 then
        all_args:=all_args+' "'+Param+'"'
      else
        all_args:=all_args+' '+Param;
    end;

  ValgrindExeName:=filesearch(ValgrindPasExeName,Dir+PathSep+GetEnvironmentVariable('PATH'));
  if ValgrindExeName='' then
    ValgrindExeName:=filesearch(ValgrindDefaultExeName,Dir+PathSep+GetEnvironmentVariable('PATH'));

  if ValgrindExeName='' then
    begin
      writeln('Unable to find ',ValgrindDefaultExeName,' and ',ValgrindPasExeName);
      halt(3);
    end;
  AdaptToValgrind(FullCompilerName);
  {$ifdef EXTDEBUG}
  Writeln(stderr,'Starting ',ValgrindExeName+' '+valgrind_args+' '+FullCompilerName+all_args);
  flush(stderr);
  {$endif}
  ValgrindExitCode:=ExecuteProcess(ValgrindExeName,valgrind_args+' '+FullCompilerName+all_args);
  if (ValgrindExitCode<>0) then
    begin
      Writeln('Error running Valgrind');
      Writeln('ExecuteProcess return value = ',ValgrindExitCode);
      RunError(ValgrindExitCode);
    end;
end.
