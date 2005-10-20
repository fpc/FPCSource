{
    Copyright (c) 2000-2002 by Pierre Muller

    This program allows to run the Makefiles
    with the compiler running inside GDB

    GDB only stops if there is something special

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

program fpc_with_gdb;

{
  This program uses several files :

   -- 'gdb4fpc.ini' contains the standard breakpoints (see below)

   -- 'gdb.fpc' is an optional file that can contain any other
      instruction that GDB should do before starting.
      Note that if gdb.fpc is present, no "run" command is
      inserted if gdb4fpc.ini is found
      but it can be inserted in gdb.fpc itself
}

uses
  dos;

const
{$ifdef Unix}
  GDBExeName = 'gdbpas';
  GDBIniName = '.gdbinit';
  DefaultCompilerName = 'ppc386';
{$else}
  GDBExeName = 'gdbpas.exe';
  GDBIniName = 'gdb.ini';
  DefaultCompilerName = 'ppc386.exe';
{$endif not linux}

  { If you add a gdb.fpc file in a given directory }
  { GDB will read it; this allows you to add       }
  { special tests in specific directories   PM     }
  FpcGDBIniName = 'gdb.fpc';
  GDBIniTempName = 'gdb4fpc.ini';

var
   fpcgdbini : text;
   CompilerName,Dir,Name,Ext : String;
   GDBError,GDBExitCode,i : longint;

begin

  fsplit(paramstr(0),Dir,Name,Ext);
  if (length(Name)>3) and (UpCase(Name[1])='G') then
    CompilerName:=Copy(Name,2,255)+Ext
  else
    CompilerName:=DefaultCompilerName;

  { support for info functions directly : used in makefiles }
  if (paramcount=1) and (pos('-i',Paramstr(1))=1) then
    begin
      Exec(fsearch(CompilerName,GetEnv('PATH')),Paramstr(1));
      exit;
    end;

  if fsearch(GDBIniTempName,'./')<>'' then
    begin
      Assign(fpcgdbini,GDBIniTempName);
      erase(fpcgdbini);
    end;
  Assign(fpcgdbini,GdbIniTempName);
  Rewrite(fpcgdbini);

  Writeln(fpcgdbini,'set language pascal');
  Writeln(fpcgdbini,'b SYSTEM_EXIT');
  Writeln(fpcgdbini,'cond 1 EXITCODE <> 0');
  Writeln(fpcgdbini,'b INTERNALERROR');
  Writeln(fpcgdbini,'b HANDLEERRORADDRFRAME');
  Writeln(fpcgdbini,'set $_exitcode := -1');
  Write(fpcgdbini,'set args');

  { this will not work correctly if there are " or '' inside the command line :( }
  for i:=1 to Paramcount do
    begin
      if pos(' ',Paramstr(i))>0 then
        Write(fpcgdbini,' "'+ParamStr(i)+'"')
      else
        Write(fpcgdbini,' '+ParamStr(i));
    end;
  Writeln(fpcgdbini);
  if fsearch(FpcGDBIniName,'./')<>'' then
    begin
      Writeln(fpcgdbini,'source '+FpcGDBIniName);
    end
  else
    Writeln(fpcgdbini,'run');
  Writeln(fpcgdbini,'if ($_exitcode = -1)');
  Writeln(fpcgdbini,'  echo Program not completed');
  Writeln(fpcgdbini,'else');
  Writeln(fpcgdbini,'  quit');
  Writeln(fpcgdbini,'end');
  Close(fpcgdbini);

  Exec(fsearch(GDBExeName,GetEnv('PATH')),
{$ifdef win32}
    '--nw '+
{$endif win32}
    '--nx --quiet --command='+GDBIniTempName+' '+CompilerName);
  GDBError:=DosError;
  GDBExitCode:=DosExitCode;
  if (GDBError<>0) or (GDBExitCode<>0) then
    begin
      Writeln('Error running GDB');
      if (GDBError<>0) then
        Writeln('DosError = ',GDBError);
      if (GDBExitCode<>0) then
        Writeln('DosExitCode = ',GDBExitCode);
      if GDBExitCode<>0 then
        RunError(GDBExitCode)
      else
        RunError(GDBError);
    end
  else
    Erase(fpcgdbini);
end.
