{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Florian Klaempfl
    member of the Free Pascal development team.

    This unit implements several classes for charset conversions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit charset;

  interface

    type
       tunicodechar = word;
       tunicodestring = ^tunicodechar;

       tcsconvert = class
         // !!!!!!1constructor create;
       end;

       tunicodecharmappingflag = (umf_noinfo,umf_leadbyte,umf_undefined,
         umf_unused);

       punicodecharmapping = ^tunicodecharmapping;
       tunicodecharmapping = record
          unicode : tunicodechar;
          flag : tunicodecharmappingflag;
          reserved : byte;
       end;

       punicodemap = ^tunicodemap;
       tunicodemap = record
          cpname : shortstring;
          map : punicodecharmapping;
          lastchar : longint;
          next : punicodemap;
          internalmap : boolean;
       end;

       tcp2unicode = class(tcsconvert)
       end;

    function loadunicodemapping(const cpname,f : string) : punicodemap;
    procedure registermapping(p : punicodemap);

  implementation

    var
       mappings : punicodemap;

    function loadunicodemapping(const cpname,f : string) : punicodemap;

      var
         data : punicodecharmapping;
         datasize : longint;
         t : text;
         s,hs : string;
         scanpos,charpos,unicodevalue : longint;
         code : word;
         flag : tunicodecharmappingflag;
         p : punicodemap;
         lastchar : longint;

      begin
         lastchar:=-1;
         loadunicodemapping:=nil;
         datasize:=256;
         getmem(data,sizeof(tunicodecharmapping)*datasize);
         assign(t,f);
         {$I-}
         reset(t);
         {$I+}
         if ioresult<>0 then
           begin
              freemem(data,sizeof(tunicodecharmapping)*datasize);
              exit;
           end;
         readln(t,s);
         while not(eof(t)) do
           begin
              if (s[1]='0') and (s[2]='x') then
                begin
                   flag:=umf_unused;
                   scanpos:=3;
                   hs:='$';
                   while s[scanpos] in ['0'..'9','A'..'F','a'..'f'] do
                     begin
                        hs:=hs+s[scanpos];
                        inc(scanpos);
                     end;
                   val(hs,charpos,code);
                   if code<>0 then
                     begin
                        freemem(data,sizeof(tunicodecharmapping)*datasize);
                        close(t);
                        exit;
                     end;
                   while not(s[scanpos] in ['0','#']) do
                     inc(scanpos);
                   if s[scanpos]='#' then
                     begin
                        { special char }
                        unicodevalue:=$ffff;
                        hs:=copy(s,scanpos,length(s)-scanpos+1);
                        if hs='#DBCS LEAD BYTE' then
                          flag:=umf_leadbyte;
                     end
                   else
                     begin
                        { C hex prefix }
                        inc(scanpos,2);
                        hs:='$';
                        while s[scanpos] in ['0'..'9','A'..'F','a'..'f'] do
                          begin
                             hs:=hs+s[scanpos];
                             inc(scanpos);
                          end;
                        val(hs,unicodevalue,code);
                        if code<>0 then
                          begin
                             freemem(data,sizeof(tunicodecharmapping)*datasize);
                             close(t);
                             exit;
                          end;
                        if charpos>datasize then
                          begin
                             { allocate 1024 bytes more because         }
                             { if we need more than 256 entries it's    }
                             { probably a mbcs with a lot of            }
                             { entries                                  }
                             datasize:=charpos+1024;
                             reallocmem(data,sizeof(tunicodecharmapping)*datasize);
                          end;
                        flag:=umf_noinfo;
                     end;
                   data[charpos].flag:=flag;
                   data[charpos].unicode:=unicodevalue;
                   if charpos>lastchar then
                     lastchar:=charpos;
                end;
              readln(t,s);
           end;
         close(t);
         new(p);
         p^.lastchar:=lastchar;
         p^.cpname:=cpname;
         p^.internalmap:=false;
         p^.next:=nil;
         p^.map:=data;
         loadunicodemapping:=p;
      end;

    procedure registermapping(p : punicodemap);

      begin
         p^.next:=mappings;
         mappings:=p;
      end;

  var
     hp : punicodemap;

initialization
  mappings:=nil;
finalization
  while assigned(mappings) do
    begin
       hp:=mappings^.next;
       if not(mappings^.internalmap) then
         begin
            freemem(mappings^.map);
            dispose(mappings);
         end;
       mappings:=hp;
    end;
end.
{
  $Log$
  Revision 1.1  2000-08-17 07:29:39  florian
    + initial revision

}