{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    Remove all revision logs from source files after 20 revisions

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
  maxrevs = 20;
  bufsize = 32*1024;

procedure dofile(const fn:string);
var
  t,f : text;
  s : string;
  skip : boolean;
  found,revs : integer;
  fbuf,tbuf : pointer;
begin
  getmem(fbuf,bufsize);
  getmem(tbuf,bufsize);
  assign(t,fn);
  assign(f,'fixlog.tmp');
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   exit;
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
          if pos('$Log: ',s)>0 then
           found:=1;
          skip:=false;
          writeln(f,s);
        end;
      1 :
        begin
          if pos('Revision',s)>0 then
           begin
             inc(revs);
             if revs>maxrevs then
              begin
                skip:=true;
                found:=2;
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
  erase(t);
  rename(f,fn);
  freemem(tbuf);
  freemem(fbuf);
end;

var
  dir : tsearchrec;
  i   : integer;
begin
  for i:=1to paramcount do
   begin
     if findfirst(paramstr(i),faAnyFile,dir)=0 then
      repeat
        dofile(dir.name);
      until findnext(dir)<>0;
     findclose(dir);
   end;
end.
{
  $Log$
  Revision 1.2  2000-01-07 01:15:00  peter
    * updated copyright to 2000

  Revision 1.1  1999/10/06 06:29:03  peter
    * new tool

}
