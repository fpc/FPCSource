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
{$ifdef go32v2}
     dpmiexcp,
{$endif go32v2}
     dos;

  procedure error(const s : string);

    begin
       writeln('Error: ',s);
       halt(1);
    end;

  var
     ppccommandline,processorpostfix,processorstr : string;
     i : longint;

  begin
     ppccommandline:='';
{$ifdef i386}
     processorpostfix:='386';
{$endif i386}
{$ifdef m68k}
     processorpostfix:='386';
{$endif m68k}
{$ifdef alpha}
     processorpostfix:='alpha';
{$endif alpha}
{$ifdef powerpc}
     processorpostfix:='powerpc';
{$endif powerpc}
     for i:=1 to paramcount do
       begin
          if pos('-P',paramstr(i))=1 then
            begin
               processorstr:=copy(paramstr(i),3,length(paramstr(i))-2);
               if processorstr='i386' then
                 processorpostfix:='386'
               else if processorstr='m68k' then
                 processorpostfix:='68k'
               else if processorstr='alpha' then
                 processorpostfix:='alpha'
               else if processorstr='powerpc' then
                 processorpostfix:='ppc'
               else error('Illegal processor type');
            end
          else
            ppccommandline:=ppccommandline+paramstr(i)+' ';
       end;

     { ppcXXX is expected to be in the same directory }
     swapvectors;
     exec('ppc'+processorpostfix,ppccommandline);
     swapvectors;
     if doserror<>0 then
       error('ppc'+processorpostfix+' can''t be executed');
     halt(dosexitcode);
  end.
{
  $Log$
  Revision 1.3  2000-10-31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
