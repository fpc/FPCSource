{
    $Id$
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

  uses
     dos;

  const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
  {$ifdef AMIGA}
    exeext='';
  {$else}
    exeext='.exe';
  {$endif}
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
     s,
     processorname,
     ppcbin,
     versionStr,
     processorstr   : shortstring;
     ppccommandline : ansistring;
     i : longint;
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
     versionstr:='';			  { Default is just the name }  
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
     swapvectors;
     exec(ppcbin,ppccommandline);
     swapvectors;
     if doserror<>0 then
       error(ppcbin+' can''t be executed');
     halt(dosexitcode);
  end.
{
  $Log$
  Revision 1.6  2003-09-30 11:24:59  marco
   * -V support

  Revision 1.5  2003/04/08 16:01:40  peter
    * amiga has also no .exe

  Revision 1.4  2002/05/18 13:34:27  peter
    * readded missing revisions

}
