    function strcat(dest,source : pchar) : pchar;

      begin
         strcat:=strcopy(strend(dest),source);
      end;

    function strlcat(dest,source : pchar;l : longint) : pchar;

      var
         destend : pchar;

      begin
         destend:=strend(dest);
         l:=l-(destend-dest);
         strlcat:=strlcopy(destend,source,l);
      end;

    function strmove(dest,source : pchar;l : longint) : pchar;

      begin
         move(source^,dest^,l);
         strmove:=dest;
      end;


    function strpos(str1,str2 : pchar) : pchar;

      var
         p : pchar;
         lstr2 : longint;

      begin
         strpos:=nil;
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
              inc(longint(p));
              p:=strscan(p,str2^);
           end;
      end;

    function strnew(p : pchar) : pchar;

      var
         len : longint;

      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(strnew,len);
         if strnew<>nil then
           strmove(strnew,p,len);
      end;
