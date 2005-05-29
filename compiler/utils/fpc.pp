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
     Sysutils,dos;

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
      Info : SearchRec;
    begin
      findfirst(F,readonly+archive+hidden,info);
      FileExists:=(doserror=0);
      findclose(Info);
    end;


  procedure findexe(var ppcbin:string);
    var
      path : string;
    begin
      { add .exe extension }
      ppcbin:=ppcbin+exeext;

      { get path of fpc.exe }
      path:=splitpath(paramstr(0));
      if FileExists(path+ppcbin) then
       ppcbin:=path+ppcbin
      else
       begin
         path:=FSearch(ppcbin,getenv('PATH'));
         if path<>'' then
          ppcbin:=path;
       end;
    end;


  var
     s              : ansistring;
     processorname,
     ppcbin,
     versionStr,
     processorstr   : string;
     ppccommandline : ansistring;
     i : longint;
     errorvalue     : Longint;
  begin
     ppccommandline:='';
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
                     else if processorstr='i386' then
                       ppcbin:='ppc386'
                     else if processorstr='m68k' then
                       ppcbin:='ppc68k'
                     else if processorstr='alpha' then
                       ppcbin:='ppcapx'
                     else if processorstr='powerpc' then
                       ppcbin:='ppcppc'
                     else if processorstr='arm' then
                       ppcbin:='ppcarm'
                     else if processorstr='sparc' then
                       ppcbin:='ppcsparc'
                     else if processorstr='ia64' then
                       ppcbin:='ppcia64'
                     else if processorstr='x86_64' then
                       ppcbin:='ppcx64'
                     else error('Illegal processor type "'+processorstr+'"');
                     end
                   else
                    ppccommandline:=ppccommandline+s+' ';
            end;
       end;

     if versionstr<>'' then
       ppcbin:=ppcbin+'-'+versionstr;
     { find the full path to the specified exe }
     findexe(ppcbin);

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
{
  $Log: fpc.pp,v $
  Revision 1.20  2005/05/08 19:56:59  marco
   * typo fixed

  Revision 1.19  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.18  2005/01/14 21:04:44  armin
  * added .nlm extension for netware

}
