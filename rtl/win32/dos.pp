{
    $Id$
    This unit mimics the DOS unit for Win32

    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{$I os.inc}

unit dos;

  interface

    uses
       strings;

    const
       { bit masks for file attributes }
       readonly = $01;
       hidden = $02;
       sysfile = $04;
       volumeid = $08;
       directory = $10;
       archive = $20;
       anyfile = $3F;
       fmclosed = $D7B0;
       fminput = $D7B1;
       fmoutput = $D7B2;
       fminout = $D7B3;

    type
       { some string types }
       comstr = string;          { command line string     }
       pathstr = string;         { string for a file path  }
       dirstr = string;          { string for a directory  }
       namestr = string;         { string for a file name  }
       extstr = string;          { string for an extension }

       { search record which is used by findfirst and findnext }
       { it is compatible with the DOS version                 }
       { if the fields are access using there names            }
       { the fields have another order
{$PACKRECORDS 1}
       searchrec = record
          time : longint;
          size : longint;
          attr : longint;
          name : string;
       end;

{$PACKRECORDS 2}

       { file record for untyped files comes from filerec.inc}
       {$i filerec.inc}

       { file record for text files  comes from textrec.inc}
       {$i textrec.inc}

{$PACKRECORDS 1}
       { record for date and time }
       datetime = record
          year,month,day,hour,min,sec : word;
       end;

    var
       { error variable }
       doserror : longint;

    procedure getdate(var year,month,day,dayofweek : word);
    procedure gettime(var hour,minute,second,sec100 : word);
    function dosversion : word;
    procedure setdate(year,month,day : word);
    procedure settime(hour,minute,second,sec100 : word);
//    procedure getcbreak(var breakvalue : boolean);
//    procedure setcbreak(breakvalue : boolean);
//    procedure getverify(var verify : boolean);
//    procedure setverify(verify : boolean);
//    function diskfree(drive : byte) : longint;
//    function disksize(drive : byte) : longint;
    procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
    procedure findnext(var f : searchRec);

    { is a dummy in win32 }
    procedure swapvectors;

{   not supported:
    procedure getintvec(intno : byte;var vector : pointer);
    procedure setintvec(intno : byte;vector : pointer);
    procedure keep(exitcode : word);
    procedure msdos(var regs : registers);
    procedure intr(intno : byte;var regs : registers);
}

    procedure getfattr(var f;var attr : word);
    procedure setfattr(var f;attr : word);

    function fsearch(const path : pathstr;dirlist : string) : pathstr;
    procedure getftime(var f;var time : longint);
//    procedure setftime(var f;time : longint);
    procedure packtime (var d: datetime; var time: longint);
    procedure unpacktime (time: longint; var d: datetime);
    function fexpand(const path : pathstr) : pathstr;
    procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;
      var ext : extstr);
//    procedure exec(const path : pathstr;const comline : comstr);
    function dosexitcode : word;
    function envcount : longint;
    function envstr(index : longint) : string;
    function getenv(const envvar : string): string;

  implementation

{$I win32.inc}

    { taken from the DOS version }
    function fsearch(const path : pathstr;dirlist : string) : pathstr;

      var
         newdir : pathstr;
         i,p1 : byte;
         s : searchrec;

      begin
         if (pos('?',path)<>0) or (pos('*',path)<>0) then
           { No wildcards allowed in these things }
           fsearch:=''
         else
           begin
              { allow slash as backslash }
              for i:=1 to length(dirlist) do
                if dirlist[i]='/' then dirlist[i]:='\';

              repeat
                { get first path }
                p1:=pos(';',dirlist);
                if p1>0 then
                  begin
                     newdir:=copy(dirlist,1,p1-1);
                     delete(dirlist,1,p1)
                  end
                else
                  begin
                     newdir:=dirlist;
                     dirlist:=''
                  end;
                if (newdir[length(newdir)]<>'\') and
                   (newdir[length(newdir)]<>':') then
                   newdir:=newdir+'\';
                findfirst(newdir+path,anyfile,s);
                if doserror=0 then
                  begin
                     { this should be newdir:=newdir+path
                     because path can contain a path part !! }
                     {newdir:=newdir+s.name;}
                     newdir:=newdir+path;
                     { this was for LINUX:
                     if pos('.\',newdir)=1 then
                       delete(newdir, 1, 2)
                      DOS strips off an initial .\
                     }
                  end
                else newdir:='';
              until(dirlist='') or (length(newdir)>0);
              fsearch:=newdir;
           end;
      end;

    procedure getftime(var f;var time : longint);

      type
         lr = record
            lo,hi : word;
         end;

      var
         dostime : longint;
         ft,lft : FILETIME;

      begin
         if GetFileTime(filerec(f).handle,nil,nil,@ft) and
           FileTimeToLocalFileTime(ft,lft) and
           FileTimeToDosDateTime(lft,lr(time).hi,lr(time).lo) then
           exit
         else
           time:=0;
      end;

   procedure setftime(var f;time : longint);

      begin
         {!!!!}
      end;

    var
       lastdosexitcode : word;

    procedure exec(const path : pathstr;const comline : comstr);

      procedure do_system(p : pchar);

        begin
           {!!!!!}
        end;

      var
         i : longint;
         execute : string;
         b : array[0..255] of char;

      begin
         doserror:=0;
         execute:=path+' '+comline;
         { allow slash as backslash for the program name only }
         for i:=1 to length(path) do
           if execute[i]='/' then execute[i]:='\';
         move(execute[1],b,length(execute));
         b[length(execute)]:=#0;
         do_system(b);
      end;

    function dosexitcode : word;

      begin
         dosexitcode:=lastdosexitcode;
      end;

    function dosversion : word;

      begin
         dosversion:=lo(GetVersion);
      end;

    procedure getdate(var year,month,day,dayofweek : word);

      var
         t : SYSTEMTIME;

      begin
         GetLocalTime(t);
         year:=t.wYear;
         month:=t.wMonth;
         day:=t.wDay;
         dayofweek:=t.wDayOfWeek;
      end;

    procedure setdate(year,month,day : word);

      var
         t : SYSTEMTIME;

      begin
         { we need the time set privilege   }
         { so this function crash currently }
         {!!!!!}
         GetLocalTime(t);
         t.wYear:=year;
         t.wMonth:=month;
         t.wDay:=day;
         { only a quite good solution, we can loose some ms }
         SetLocalTime(t);
      end;

    procedure gettime(var hour,minute,second,sec100 : word);

      var
         t : SYSTEMTIME;

      begin
         GetLocalTime(t);
         hour:=t.wHour;
         minute:=t.wMinute;
         second:=t.wSecond;
         sec100:=t.wMilliSeconds div 10;
      end;

    procedure settime(hour,minute,second,sec100 : word);

      var
         t : SYSTEMTIME;

      begin
         { we need the time set privilege   }
         { so this function crash currently }
         {!!!!!}

         GetLocalTime(t);
         t.wHour:=hour;
         t.wMinute:=minute;
         t.wSecond:=second;
         t.wMilliSeconds:=sec100*10;
         SetLocalTime(t);
      end;

    procedure getcbreak(var breakvalue : boolean);

      begin
         {!!!!}
      end;

    procedure setcbreak(breakvalue : boolean);

      begin
         {!!!!}
      end;

    procedure getverify(var verify : boolean);

      begin
         {!!!!}
      end;

    procedure setverify(verify : boolean);

      begin
         {!!!!}
      end;

    function diskfree(drive : byte) : longint;

      begin
         {!!!!}
      end;

    function disksize(drive : byte) : longint;

      begin
         {!!!!}
      end;

    procedure searchrec2dossearchrec(var f : searchrec);

      var
         l,i : longint;

      begin
         l:=length(f.name);
         for i:=1 to 12 do
           f.name[i-1]:=f.name[i];
         f.name[l]:=#0;
      end;

    procedure dossearchrec2searchrec(var f : searchrec);

      var
         l,i : longint;

      begin
         l:=12;
         for i:=0 to 12 do
           if f.name[i]=#0 then
             begin
                l:=i;
                break;
             end;
         for i:=11 downto 0 do
           f.name[i+1]:=f.name[i];
         f.name[0]:=chr(l);
      end;

    procedure findfirst(const path : pathstr;attr : word;var f : searchRec);

      procedure _findfirst(path : pchar;attr : word;var f : searchrec);

        var
           i : longint;
        begin
           { allow slash as backslash }
           for i:=0 to strlen(path) do
             if path[i]='/' then path[i]:='\';
           {!!!!!!!}
        end;

      var
         path0 : array[0..80] of char;

      begin
         { no error }
         doserror:=0;
         strpcopy(path0,path);
         _findfirst(path0,attr,f);
         dossearchrec2searchrec(f);
      end;

    procedure findnext(var f : searchRec);

      procedure _findnext(var f : searchrec);

        begin
           {!!!!}
        end;

      begin
         { no error }
         doserror:=0;
         searchrec2dossearchrec(f);
         _findnext(f);
         dossearchrec2searchrec(f);
      end;

    procedure swapvectors;

      begin
         { only a dummy }
      end;

    { the environment is a block of zero terminated strings }
    { terminated by a #0                                    }
    function envcount : longint;

      var
         hp,p : pchar;

      begin
         p:=GetEnvironmentStrings;
         hp:=p;
         envcount:=0;
         while  hp^<>#0 do
           begin
              { next string entry}
              hp:=hp+strlen(hp)+1;
              inc(envcount);
           end;
         FreeEnvironmentStrings(p);
      end;

    function envstr(index : longint) : string;

      var
         hp,p : pchar;
         count,i : longint;


      begin
         { envcount takes some time in win32 }
         count:=envcount;

         { range checking }
         if (index<=0) or (index>count) then
           begin
              envstr:='';
              exit;
           end;
         p:=GetEnvironmentStrings;
         hp:=p;

         { retrive the string with the given index }
         for i:=2 to index do
           hp:=hp+strlen(hp)+1;

         envstr:=strpas(hp);
         FreeEnvironmentStrings(p);
      end;

    function getenv(const envvar : string) : string;

      var
         s : string;
         i : longint;
         hp,p : pchar;

      begin
         getenv:='';
         p:=GetEnvironmentStrings;
         hp:=p;
         while hp^<>#0 do
           begin
              s:=strpas(hp);
              i:=pos('=',s);
              if copy(s,1,i-1)=envvar then
                begin
                   getenv:=copy(s,i+1,length(s)-i);
                   break;
                end;
              { next string entry}
              hp:=hp+strlen(hp)+1;
           end;
         FreeEnvironmentStrings(p);
      end;

    procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;
      var ext : extstr);

      var
         p1 : byte;
         i : longint;
      begin
         { allow slash as backslash }
         for i:=1 to length(path) do
             if path[i]='/' then path[i]:='\';
         { get drive name }
         p1:=pos(':',path);
         if p1>0 then
           begin
              dir:=path[1]+':';
              delete(path,1,p1);
           end
         else
           dir:='';
         { split the path and the name, there are no more path informtions }
         { if path contains no backslashes                                 }
         while true do
           begin
              p1:=pos('\',path);
              if p1=0 then
                break;
              dir:=dir+copy(path,1,p1);
              delete(path,1,p1);
           end;
         { try to find out a extension }
         p1:=pos('.',path);
         if p1>0 then
           begin
              ext:=copy(path,p1,4);
              delete(path,p1,length(path)-p1+1);
           end
         else
           ext:='';
         name:=path;
      end;

    function fexpand(const path : pathstr) : pathstr;

       var
          s,pa : string[79];
          i,j : byte;

       begin
          { There are differences between Free Pascal and Turbo Pascal
            e.g. for the string 'D:\DEMO\..\HELLO' which isn't handled }
          getdir(0,s);
          pa:=upcase(path);
          { allow slash as backslash }
          for i:=1 to length(pa) do
             if pa[i]='/' then pa[i]:='\';
          if (ord(pa[0])>1) and (((pa[1]>='A') and (pa[1]<='Z')) and (pa[2]=':')) then
            begin
               { we must get the right directory }
               getdir(ord(pa[1])-ord('A')+1,s);
               if (ord(pa[0])>2) and (pa[3]<>'\') then
                 if pa[1]=s[1] then
                   pa:=s+'\'+copy (pa,3,length(pa))
                 else
                   pa:=pa[1]+':\'+copy (pa,3,length(pa))
            end
          else
            if pa[1]='\' then 
              pa:=s[1]+':'+pa
            else if s[0]=#3 then
              pa:=s+pa
            else
              pa:=s+'\'+pa;
        {First remove all references to '\.\'}
          while pos ('\.\',pa)<>0 do
           delete (pa,pos('\.\',pa),2);
        {Now remove also all references to '\..\' + of course previous dirs..}
          repeat
            i:=pos('\..\',pa);
            j:=i-1;
            while (j>1) and (pa[j]<>'\') do
             dec (j);
            delete (pa,j,i-j+3);
          until i=0;
        {Remove End . and \}
          if (length(pa)>0) and (pa[length(pa)]='.') then
           dec(byte(pa[0]));
          if (length(pa)>0) and (pa[length(pa)]='\') then
           dec(byte(pa[0]));
          fexpand:=pa;
       end;

     procedure packtime(var d : datetime;var time : longint);

       var
          zs : longint;

       begin
          time:=-1980;
          time:=time+d.year and 127;
          time:=time shl 4;
          time:=time+d.month;
          time:=time shl 5;
          time:=time+d.day;
          time:=time shl 16;
          zs:=d.hour;
          zs:=zs shl 6;
          zs:=zs+d.min;
          zs:=zs shl 5;
          zs:=zs+d.sec div 2;
          time:=time+(zs and $ffff);
       end;

     procedure unpacktime (time: longint;var d : datetime);

       begin
          d.sec:=(time and 31) * 2;
          time:=time shr 5;
          d.min:=time and 63;
          time:=time shr 6;
          d.hour:=time and 31;
          time:=time shr 5;
          d.day:=time and 31;
          time:=time shr 5;
          d.month:=time and 15;
          time:=time shr 4;
          d.year:=time + 1980;
       end;

    procedure getfattr(var f;var attr : word);

      var
         l : longint;

      begin
         l:=GetFileAttributes(filerec(f).name);
         if l=$ffffffff then
           doserror:=getlasterror;
          attr:=l;
      end;

    procedure setfattr(var f;attr : word);

      begin
         doserror:=0;
         if not(SetFileAttributes(filerec(f).name,attr)) then
           doserror:=getlasterror;
      end;

end.

{
  $Log$
  Revision 1.3  1998-04-26 22:37:02  florian
    + getftime, unpacktime, packtime

  Revision 1.2  1998/04/26 21:49:09  florian
    + first compiling and working version

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.2  1998/03/10 13:23:56  florian
    * just a few things adapted to win32

  Revision 1.1  1998/03/09 23:19:12  florian
    + Initial revision, just copied from the DOS version
}
