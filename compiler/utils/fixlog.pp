{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
  skip : boolean;
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
  while not eof(t) do
   begin
     readln(t,s);
     case found of
      0 :
        begin
  $Log$
  Revision 1.2  2000-07-13 11:32:55  michael
  + removed logs
 
}
