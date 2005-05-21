{
    Copyright (c) 1998-2002 by Peter Vreman

    Remove all revision logs from source files after X revisions or
    older than date X

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fixlog;

{$mode objfpc}
{$H+}

uses
  sysutils;

const
  bufsize = 32*1024;

var
  maxrevs,myear,mmonth,mday : integer;

procedure Date2Int(const date:string;var year,month,day:integer);
begin
  year:=StrToInt(Copy(date,1,4));
  month:=StrToInt(Copy(date,6,2));
  day:=StrToInt(Copy(date,9,2));
  if (year=0) or (month=0) or (day=0) then
   begin
     writeln('wrong date "',date,'", use yyyy/mm/dd');
     halt(2);
   end;
end;


procedure dofile(const fn:string);
var
  t,f : text;
  s : string;
  skip, truncated : boolean;
  year,month,day,
  found,revs,i : integer;
  fbuf,tbuf : pointer;
begin
  getmem(fbuf,bufsize);
  getmem(tbuf,bufsize);
  write('processing ',fn,': ');
  assign(t,fn);
  assign(f,'fixlog.tmp');
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   begin
     writeln('error!');
     exit;
   end;
  rewrite(f);
  settextbuf(t,tbuf^,bufsize);
  settextbuf(f,fbuf^,bufsize);
  found:=0;
  revs:=0;
  skip:=false;
  truncated:=false;
  while not eof(t) do
   begin
     readln(t,s);
     case found of
      0 :
        begin
          if pos('$Log: ',s)>0 then
           found:=1;
          skip:=false;
          writeln(f,s);
        end;
      1 :
        begin
          i:=pos('Revision',s);
          if i>0 then
           begin
             inc(revs);
             if revs>maxrevs then
              begin
                skip:=true;
                truncated:=true;
                found:=2;
              end
             else
              begin
                inc(i,10);
                while (i<length(s)) and (s[i]<>' ') do
                 inc(i);
                while (i<length(s)) and (s[i]=' ') do
                 inc(i);
                if (i<length(s)) and (s[i] in ['0'..'9']) then
                 begin
                   Date2Int(Copy(s,i,10),year,month,day);
                   if (year<Myear) or
                      ((year=MYear) and (month<Mmonth)) or
                      ((year=MYear) and (month=Mmonth) and (day<Mday)) then
                    begin
                      skip:=true;
                      truncated:=true;
                      found:=2;
//                    write(year,'/',month,'/',day,' date');
                    end;
                 end;
              end;
           end
          else
           if pos('}',s)>0 then
            begin
              skip:=false;
              found:=0;
            end;
          if not skip then
           writeln(f,s);
        end;
      2 :
        begin
          if pos('}',s)>0 then
           begin
             skip:=false;
             found:=0;
           end;
          if not skip then
           writeln(f,s);
        end;
     end;
   end;
  close(t);
  close(f);
  if revs=0 then
   writeln(' no log found')
  else
   if truncated then
     writeln(revs-1,' revisions')
  else
     writeln(revs,' revisions');
  erase(t);
  rename(f,fn);
  freemem(tbuf);
  freemem(fbuf);
end;

var
  dir : tsearchrec;
  i   : integer;
  path : string;
begin
  writeln('fixlog v1.01 (C) 1999-2002 Peter Vreman');
  if paramcount<3 then
   begin
     writeln('usage: fixlog <revisions> <yyyy-mm-dd> <files> [files]');
     halt(1);
   end;
  MaxRevs:=StrToInt(ParamStr(1));
  Date2Int(ParamStr(2),MYear,MMonth,MDay);
  for i:=3 to paramcount do
   begin
     path:=ExtractFilePath(paramstr(i));
     if findfirst(paramstr(i),faAnyFile,dir)=0 then
      repeat
        dofile(path+dir.name);
      until findnext(dir)<>0;
     findclose(dir);
   end;
end.
