{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    This unit implements the message object

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
unit Messages;
interface

type
  ppchar=^pchar;

  PMessage=^TMessage;
  TMessage=object
    msgfilename : string;
    msgsize,
    msgs        : longint;
    msgtxt      : pchar;
    msgidx      : ppchar;
    constructor Init(p:pointer;n:longint);
    constructor InitExtern(const fn:string;n:longint);
    destructor Done;
    function Get(nr:longint):string;
    function Get3(nr:longint;const s1,s2,s3:string):string;
    function Get2(nr:longint;const s1,s2:string):string;
    function Get1(nr:longint;const s1:string):string;
  end;

implementation

uses
  strings;

constructor TMessage.Init(p:pointer;n:longint);
var
  hp  : pchar;
  hpl : ppchar;
begin
  hp:=pchar(p);
  msgtxt:=hp;
  msgsize:=0;
  msgs:=n;
  getmem(msgidx,msgs shl 2);
  hpl:=msgidx;
  n:=0;
  while (n<msgs) do
   begin
     hpl^:=hp;
     hpl:=pointer(longint(hpl)+4);
     inc(n);
     hp:=pchar(@hp[strlen(hp)+1]);
   end;
end;


constructor TMessage.InitExtern(const fn:string;n:longint);
var
  f       : file;
  bufread : word;
  i,j     : longint;
  p       : pchar;
  hpl     : ppchar;
begin
  msgs:=0;
  msgsize:=0;
  msgidx:=nil;
{Read the message file}
  msgfilename:=fn;
  assign(f,fn);
  {$I-}
   reset(f,1);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+msgfilename+' not found ***');
     exit;
   end;
  msgsize:=filesize(f);
  getmem(msgtxt,msgsize+1);
  blockread(f,msgtxt^,msgsize,bufread);
  msgtxt[msgsize]:=#10;
  close(f);
  inc(msgsize);
{Parse buffer in msgtxt and create indexs}
  msgs:=n;
  getmem(msgidx,msgs shl 2);
  hpl:=msgidx;
  p:=msgtxt;
  i:=0;
  n:=0;
  while (i<bufread) and (n<msgs) do
   begin
     j:=0;
     while (not (p[j] in [#10,#13])) and (j<255) and (i<bufread) do
      begin
        inc(i);
        inc(j);
      end;
     if not (p[0] in [';','#']) then
      begin
        hpl^:=p;
        hpl:=pointer(longint(hpl)+4);
        inc(n);
        if (p[0]='<') and (p[1]='l') and (p[2]='f') and (p[3]='>') then
         p[0]:=#0
        else
         p[j]:=#0;
      end;
     repeat
       inc(i);
       inc(j);
     until not (p[j] in [#10,#13]);
     p:=pchar(@p[j]);
   end;
end;



destructor TMessage.Done;
begin
  if not (msgidx=nil) then
   freemem(msgidx,msgs shl 2);
  if msgsize>0 then
   freemem(msgtxt,msgsize);
end;


function TMessage.Get(nr:longint):string;
var
  s : string[16];
  hp : pchar;
begin
  if msgidx=nil then
   hp:=nil
  else
   hp:=pchar(pointer(longint(msgidx)+nr shl 2)^);
  if hp=nil then
   begin
     Str(nr,s);
     Get:='msg nr '+s;
   end
  else
   Get:=StrPas(hp);
end;


function TMessage.Get3(nr:longint;const s1,s2,s3:string):string;
var
  i : longint;
  s : string;
begin
  s:=Get(nr);
{ $1 -> s1 }
  repeat
    i:=pos('$1',s);
    if i>0 then
     begin
       Delete(s,i,2);
       Insert(s1,s,i);
     end;
  until i=0;
{ $2 -> s2 }
  repeat
    i:=pos('$2',s);
    if i>0 then
     begin
       Delete(s,i,2);
       Insert(s2,s,i);
     end;
  until i=0;
{ $3 -> s3 }
  repeat
    i:=pos('$3',s);
    if i>0 then
     begin
       Delete(s,i,2);
       Insert(s3,s,i);
     end;
  until i=0;
  Get3:=s;
end;


function TMessage.Get2(nr:longint;const s1,s2:string):string;
begin
  Get2:=Get3(nr,s1,s2,'');
end;


function TMessage.Get1(nr:longint;const s1:string):string;
begin
  Get1:=Get3(nr,s1,'','');
end;


end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:13  root
  Initial revision

  Revision 1.3  1998/03/10 01:17:20  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.2  1998/03/05 02:44:12  peter
    * options cleanup and use of .msg file

  Revision 1.1  1998/03/02 01:55:19  peter
    + Initial implementation

}