{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor independent part for strings and sysutils units

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    function strcat(dest,source : pchar) : pchar;

      begin
        strcopy(strend(dest),source);
        strcat:=dest;
      end;

    function strlcat(dest,source : pchar;l : SizeInt) : pchar;

      var
         destend : pchar;

      begin
         destend:=strend(dest);
         dec(l,destend-dest);
         if l>0 then
          strlcopy(destend,source,l);
         strlcat:=dest;
      end;

    function strmove(dest,source : pchar;l : SizeInt) : pchar;

      begin
         move(source^,dest^,l);
         strmove:=dest;
      end;


    function strpos(str1,str2 : pchar) : pchar;
      var
         p : pchar;
         lstr2 : SizeInt;
      begin
         strpos:=nil;
         if (str1 = nil) or (str2 = nil) then
           exit;
         p:=strscan(str1,str2^);
         if p=nil then
           exit;
         lstr2:=strlen(str2);
         while p<>nil do
           begin
              if strlcomp(p,str2,lstr2)=0 then
                begin
                   strpos:=p;
                   exit;
                end;
              inc(p);
              p:=strscan(p,str2^);
           end;
      end;

