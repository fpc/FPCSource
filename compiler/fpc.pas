{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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

  uses
     dos;

  const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
    exeext='.exe';
{$endif UNIX}


  procedure error(const s : string);
    begin
       writeln('Error: ',s);
       halt(1);
    end;


  function SplitPath(Const HStr:ShortString):ShortString;
    var
      i : longint;
    begin
      i:=Length(Hstr);
      while (i>0) and not(Hstr[i] in ['\','/']) do
       dec(i);
      SplitPath:=Copy(Hstr,1,i);
    end;


  function FileExists ( Const F : ShortString) : Boolean;
    var
      Info : SearchRec;
    begin
      findfirst(F,readonly+archive+hidden,info);
      FileExists:=(doserror=0);
      findclose(Info);
    end;

  var
     s,path,
     ppcbin,
     processorstr   : shortstring;
     ppccommandline : ansistring;
     i : longint;

  begin
     ppccommandline:='';
{$ifdef i386}
     ppcbin:='ppc386';
{$endif i386}
{$ifdef m68k}
     ppcbin:='ppc68k';
{$endif m68k}
{$ifdef alpha}
     ppcbin:='ppcapx';
{$endif alpha}
{$ifdef powerpc}
     ppcbin:='ppcppc';
{$endif powerpc}
     for i:=1 to paramcount do
       begin
          s:=paramstr(i);
          if pos('-P',s)=1 then
            begin
               processorstr:=copy(s,3,length(s)-2);
               if processorstr='i386' then
                 ppcbin:='ppc386'
               else if processorstr='m68k' then
                 ppcbin:='ppc68k'
               else if processorstr='alpha' then
                 ppcbin:='ppcapx'
               else if processorstr='powerpc' then
                 ppcbin:='ppcppc'
               else error('Illegal processor type "'+processorstr+'"');
            end
          else
            ppccommandline:=ppccommandline+s+' ';
       end;

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

     { call ppcXXX }
     swapvectors;
     exec(ppcbin,ppccommandline);
     swapvectors;
     if doserror<>0 then
       error(ppcbin+' can''t be executed');
     halt(dosexitcode);
  end.
{
}
