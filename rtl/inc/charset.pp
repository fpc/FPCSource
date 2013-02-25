{
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
          cpname : string[20];
          cp : word;
          map : punicodecharmapping;
          lastchar : longint;
          next : punicodemap;
          internalmap : boolean;
       end;

       tcp2unicode = class(tcsconvert)
       end;

    function loadunicodemapping(const cpname,f : string; cp :word) : punicodemap;
    procedure registermapping(p : punicodemap);
    function getmap(const s : string) : punicodemap; 
    function getmap(cp : word) : punicodemap;   
    function mappingavailable(const s : string) : boolean;
    function mappingavailable(cp :word) : boolean;
    function getunicode(c : char;p : punicodemap) : tunicodechar;
    function getunicode(
      AAnsiStr : pansichar;
      AAnsiLen : SizeInt;
      AMap     : punicodemap; 
      ADest    : tunicodestring
    ) : SizeInt;
    function getascii(c : tunicodechar;p : punicodemap) : string;
    function getascii(c : tunicodechar;p : punicodemap; ABuffer : PAnsiChar; ABufferLen : SizeInt) : SizeInt;

  implementation

    const
      UNKNOW_CHAR_A = ansichar(63);
      UNKNOW_CHAR_W = tunicodechar(63);
    var
       mappings : punicodemap;

    function loadunicodemapping(const cpname,f : string; cp :word) : punicodemap;

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
         while not(eof(t)) do
           begin
              readln(t,s);
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
           end;
         close(t);
         new(p);
         p^.lastchar:=lastchar;
         p^.cpname:=cpname;
         p^.cp:=cp;
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

    function getmap(const s : string) : punicodemap;

      var
         hp : punicodemap;

      const
         mapcache : string = '';
         mapcachep : punicodemap = nil;

      begin
         if (mapcache=s) and assigned(mapcachep) and (mapcachep^.cpname=s) then
           begin
              getmap:=mapcachep;
              exit;
           end;
         hp:=mappings;
         while assigned(hp) do
           begin
              if hp^.cpname=s then
                begin
                   getmap:=hp;
                   mapcache:=s;
                   mapcachep:=hp;
                   exit;
                end;
              hp:=hp^.next;
           end;
         getmap:=nil;
      end;////////

    function getmap(cp : word) : punicodemap;

      var
         hp : punicodemap;

      const
         mapcache : word = 0;
         mapcachep : punicodemap = nil;

      begin
         if (mapcache=cp) and assigned(mapcachep) and (mapcachep^.cp=cp) then
           begin
              getmap:=mapcachep;
              exit;
           end;
         hp:=mappings;
         while assigned(hp) do
           begin
              if hp^.cp=cp then
                begin
                   getmap:=hp;
                   mapcache:=cp;
                   mapcachep:=hp;
                   exit;
                end;
              hp:=hp^.next;
           end;
         getmap:=nil;
      end;

    function mappingavailable(const s : string) : boolean;

      begin
         mappingavailable:=getmap(s)<>nil;
      end;

    function mappingavailable(cp : word) : boolean;

      begin
         mappingavailable:=getmap(cp)<>nil;
      end;

    function getunicode(c : char;p : punicodemap) : tunicodechar;

      begin
         if ord(c)<=p^.lastchar then
           getunicode:=p^.map[ord(c)].unicode
         else
           getunicode:=0;
      end;

    function getunicode(
      AAnsiStr : pansichar;
      AAnsiLen : SizeInt;
      AMap     : punicodemap; 
      ADest    : tunicodestring
    ) : SizeInt;

      var
         i, c, k, destLen : longint;
         ps : pansichar;
         pd : ^tunicodechar;
         
      begin
        if (AAnsiStr=nil) or (AAnsiLen<=0) then
          exit(0);
        ps:=AAnsiStr;   
        if (ADest=nil) then
          begin
            c:=AAnsiLen-1;
            destLen:=0;
            i:=0;
            while (i<=c) do
              begin
                if (ord(ps^)<=AMap^.lastchar) then
                  begin
                    if (AMap^.map[ord(ps^)].flag=umf_leadbyte) and (i<c) then
                      Inc(ps);
                  end;  
                i:=i+1;  
                Inc(ps);
                destLen:=destLen+1;
              end; 
            exit(destLen);  
          end;

        pd:=ADest;
        c:=AAnsiLen-1;
        i:=0;
        while (i<=c) do
          begin
            if (ord(ps^)<=AMap^.lastchar) then
              begin
                if (AMap^.map[ord(ps^)].flag=umf_leadbyte) then
                  begin
                    if (i<c) then 
                      begin
                        k:=(Ord(ps^)*256);
                        Inc(ps);
                        k:=k+Ord(ps^);
                        if (k<=AMap^.lastchar) then
                          pd^:=AMap^.map[k].unicode
                        else
                          pd^:=UNKNOW_CHAR_W;
                      end
                    else
                      pd^:=UNKNOW_CHAR_W;
                  end                       
                else   
                  pd^:=AMap^.map[ord(ps^)].unicode
              end  
            else
              pd^:=UNKNOW_CHAR_W;
            i:=i+1;  
            Inc(ps);
            Inc(pd);
          end; 
        result:=((PtrUInt(pd)-PtrUInt(ADest)) div SizeOf(tunicodechar));
      end;

    function getascii(c : tunicodechar;p : punicodemap) : string;

      var
         i : longint;

      begin
         { at least map to '?' }
         getascii:=#63;
         for i:=0 to p^.lastchar do
           if p^.map[i].unicode=c then
             begin
                if i<256 then
                  getascii:=chr(i)
                else
                  getascii:=chr(i div 256)+chr(i mod 256);
                exit;
             end;
      end;

    function getascii(c : tunicodechar;p : punicodemap; ABuffer : PAnsiChar; ABufferLen : SizeInt) : SizeInt;

      var
         i : longint;

      begin
         if (ABuffer<>nil) and (ABufferLen<=0) then
           begin
              Result:=-1;
              exit;
           end;
         for i:=0 to p^.lastchar do
           if p^.map[i].unicode=c then
             begin
                if (ABuffer=nil) then
                  begin
                     if i<256 then
                       Result:=1
                     else
                       Result:=2;
                     exit;
                  end;
                if i<256 then
                  begin
                    Result:=1;
                    ABuffer^:=chr(i);
                  end
                else
                  begin
                    if (ABufferLen<2) then
                      begin
                        Result:=-1;
                        exit;
                      end;
                    ABuffer^:=chr(i div 256);
                    Inc(ABuffer);
                    ABuffer^:=chr(i mod 256);
                  end;
                exit;
             end;
         { at least map to '?' }
         Result := 1;
         if (ABuffer<>nil) then
           ABuffer^:=#63;
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
