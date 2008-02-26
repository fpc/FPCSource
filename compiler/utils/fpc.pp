{
    Copyright (c) 2000-2002 by Florian Klaempfl

    This file is the "loader" for the Free Pascal compiler

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
program fpc;

{$mode objfpc}{$H+}

  uses
     Sysutils;

  const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
  {$ifdef AMIGA}
    exeext='';
  {$else}
    {$ifdef MORPHOS}
      exeext='';
    {$else}
      {$ifdef NETWARE}
      exeext='.nlm';
      {$else}
      exeext='.exe';
      {$endif NETWARE}
    {$endif MORPHOS}
  {$endif AMIGA}
{$endif UNIX}


  procedure error(const s : string);
    begin
       writeln('Error: ',s);
       halt(1);
    end;


  function SplitPath(Const HStr:String):String;
    var
      i : longint;
    begin
      i:=Length(Hstr);
      while (i>0) and not(Hstr[i] in ['\','/']) do
       dec(i);
      SplitPath:=Copy(Hstr,1,i);
    end;


  function FileExists ( Const F : String) : Boolean;
    var
      Info : TSearchRec;
    begin
      FileExists:= findfirst(F,fareadonly+faarchive+fahidden,info)=0;
      findclose(Info);
    end;

  var
    extrapath : ansistring;

  function findexe(var ppcbin:string): boolean;
    var
      path : string;
    begin
      { add .exe extension }
      findexe:=false;
      ppcbin:=ppcbin+exeext;

      if (extrapath<>'') and (extrapath[length(extrapath)]<>DirectorySeparator) then
        extrapath:=extrapath+DirectorySeparator;
      { get path of fpc.exe }
      path:=splitpath(paramstr(0));
      if FileExists(extrapath+ppcbin) then
       begin
         ppcbin:=extrapath+ppcbin;
         findexe:=true;
       end
      else if FileExists(path+ppcbin) then
       begin
         ppcbin:=path+ppcbin;
         findexe:=true;
       end
      else
       begin
         path:=FileSearch(ppcbin,getenvironmentvariable('PATH'));
         if path<>'' then
          begin
            ppcbin:=path;
            findexe:=true;
          end
       end;
    end;

  var
     s              : ansistring;
     cpusuffix,
     processorname,
     ppcbin,
     versionStr,
     processorstr   : string;
     ppccommandline : array of ansistring;
     ppccommandlinelen : longint;
     i : longint;
     errorvalue     : Longint;
  begin
     setlength(ppccommandline,paramcount);
     ppccommandlinelen:=0;
     cpusuffix     :='';        // if not empty, signals attempt at cross
                                // compiler.
     extrapath     :='';
{$ifdef i386}
     ppcbin:='ppc386';
     processorname:='i386';
{$endif i386}
{$ifdef m68k}
     ppcbin:='ppc68k';
     processorname:='m68k';
{$endif m68k}
{$ifdef alpha}
     ppcbin:='ppcapx';
     processorname:='alpha';
{$endif alpha}
{$ifdef powerpc}
     ppcbin:='ppcppc';
     processorname:='powerpc';
{$endif powerpc}
{$ifdef powerpc64}
     ppcbin:='ppcppc64';
     processorname:='powerpc64';
{$endif powerpc64}
{$ifdef arm}
     ppcbin:='ppcarm';
     processorname:='arm';
{$endif arm}
{$ifdef sparc}
     ppcbin:='ppcsparc';
     processorname:='sparc';
{$endif sparc}
{$ifdef x86_64}
     ppcbin:='ppcx64';
     processorname:='x86_64';
{$endif x86_64}
{$ifdef ia64}
     ppcbin:='ppcia64';
     processorname:='ia64';
{$endif ia64}
     versionstr:='';                      { Default is just the name }
     for i:=1 to paramcount do
       begin
          s:=paramstr(i);
          if pos('-V',s)=1 then
              versionstr:=copy(s,3,length(s)-2)
          else
            begin
              if pos('-P',s)=1 then
                 begin
                   processorstr:=copy(s,3,length(s)-2);
                  { -PB is a special code that will show the
                    default compiler and exit immediatly. It's
                     main usage is for Makefile }
                   if processorstr='B' then
                     begin
                       { report the full name of the ppcbin }
                       findexe(ppcbin);
                       writeln(ppcbin);
                       halt(0);
                     end
                     { -PP is a special code that will show the
                       processor and exit immediatly. It's
                       main usage is for Makefile }
                     else if processorstr='P' then
                      begin
                        { report the processor }
                        writeln(processorname);
                        halt(0);
                      end
                     else
                       if processorstr <> processorname then
                         begin
                           if processorstr='i386' then
                             cpusuffix:='386'
                           else if processorstr='m68k' then
                             cpusuffix:='68k'
                           else if processorstr='alpha' then
                             cpusuffix:='apx'
                           else if processorstr='powerpc' then
                             cpusuffix:='ppc'
                           else if processorstr='powerpc64' then
                             cpusuffix:='ppc64'
                           else if processorstr='arm' then
                             cpusuffix:='arm'
                           else if processorstr='sparc' then
                             cpusuffix:='sparc'
                           else if processorstr='ia64' then
                             cpusuffix:='ia64'
                           else if processorstr='x86_64' then
                             cpusuffix:='x64'
                           else
                             error('Illegal processor type "'+processorstr+'"');

{$ifndef darwin}
                           ppcbin:='ppcross'+cpusuffix;
{$else not darwin}
                           { the mach-o format supports "fat" binaries whereby }
                           { a single executable contains machine code for     }
                           { several architectures -> it is counter-intuitive  }
                           { and non-standard to use different binary names    }
                           { for cross-compilers vs. native compilers          }
                           ppcbin:='ppc'+cpusuffix;
{$endif not darwin}
                         end;
                 end
              else if pos('-Xp',s)=1 then
                extrapath:=copy(s,4,length(s)-3)
              else
                begin
                  ppccommandline[ppccommandlinelen]:=s;
                  inc(ppccommandlinelen);
                end;
            end;
       end;
     SetLength(ppccommandline,ppccommandlinelen);

     if versionstr<>'' then
       ppcbin:=ppcbin+'-'+versionstr;
     { find the full path to the specified exe }
     if not findexe(ppcbin) then
        begin
          if cpusuffix<>'' Then
            begin
              ppcbin:='ppc'+cpusuffix;
              if versionstr<>'' then
                ppcbin:=ppcbin+'-'+versionstr;
              findexe(ppcbin);
            end;
        end;

     { call ppcXXX }
     try
       errorvalue:=ExecuteProcess(ppcbin,ppccommandline);
     except
       on e : exception do
         error(ppcbin+' can''t be executed, error message: '+e.message);
     end;
     if errorvalue<>0 then
       error(ppcbin+' returned an error exitcode (normal if you did not specify a source file to be compiled)');
     halt(errorvalue);
  end.
