{
    $Id$
    Copyright (c) 1999 by Peter Vreman

    This unit implements some support routines for the Dos targets

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

 ****************************************************************************
}
unit dos_targ;

  interface
  uses
    link;

  type
    plinkergo32v2=^tlinkergo32v2;
    tlinkergo32v2=object(tlinker)
      procedure postprocessexecutable(const n : string);virtual;
    end;


  implementation

    uses
       strings,globtype,globals,cobjects,systems,verbose;


{****************************************************************************
                            Postprocess Executable
****************************************************************************}

    procedure tlinkergo32v2.postprocessexecutable(const n : string);
      begin
      end;

{$ifdef dummy}
      type
        tcoffheader=packed record
          mach   : word;
          nsects : word;
          time   : longint;
          sympos : longint;
          syms   : longint;
          opthdr : word;
          flag   : word;
        end;
        tcoffsechdr=packed record
          name     : array[0..7] of char;
          vsize    : longint;
          rvaofs   : longint;
          datalen  : longint;
          datapos  : longint;
          relocpos : longint;
          lineno1  : longint;
          nrelocs  : word;
          lineno2  : word;
          flags    : longint;
        end;
        psecfill=^tsecfill;
        tsecfill=record
          fillpos,
          fillsize : longint;
          next : psecfill;
        end;

      var
         f : file;
         coffheader : tcoffheader;
         firstsecpos,
         maxfillsize,
         l : longint;
         coffsec : tcoffsechdr;
         secroot,hsecroot : psecfill;
         zerobuf : pointer;
      begin
         { when -s is used quit, because there is no .exe }
         if cs_link_extern in aktglobalswitches then
          exit;
         { open file }
         assign(f,n);
         {$I-}
          reset(f,1);
         if ioresult<>0 then
           Message1(execinfo_f_cant_open_executable,n);
         { read headers }
         seek(f,2048);
         blockread(f,coffheader,sizeof(tcoffheader));
         { read section info }
         maxfillsize:=0;
         firstsecpos:=0;
         secroot:=nil;
         for l:=1to coffheader.nSects do
          begin
            blockread(f,coffsec,sizeof(tcoffsechdr));
            if coffsec.datapos>0 then
             begin
               if secroot=nil then
                firstsecpos:=coffsec.datapos;
               new(hsecroot);
               hsecroot^.fillpos:=coffsec.datapos+coffsec.vsize;
               hsecroot^.fillsize:=coffsec.datalen-coffsec.vsize;
               hsecroot^.next:=secroot;
               secroot:=hsecroot;
               if secroot^.fillsize>maxfillsize then
                maxfillsize:=secroot^.fillsize;
             end;
          end;
         if firstsecpos>0 then
          begin
            l:=firstsecpos-filepos(f);
            if l>maxfillsize then
             maxfillsize:=l;
          end
         else
          l:=0;
         { get zero buffer }
         getmem(zerobuf,maxfillsize);
         fillchar(zerobuf^,maxfillsize,0);
         { zero from sectioninfo until first section }
         blockwrite(f,zerobuf^,l);
         { zero section alignments }
         while assigned(secroot) do
          begin
            seek(f,secroot^.fillpos);
            blockwrite(f,zerobuf^,secroot^.fillsize);
            hsecroot:=secroot;
            secroot:=secroot^.next;
            dispose(hsecroot);
          end;
         freemem(zerobuf,maxfillsize);
         close(f);
         {$I+}
      end;
{$endif}

end.
{
  $Log$
  Revision 1.1  1999-08-11 17:26:32  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

}
