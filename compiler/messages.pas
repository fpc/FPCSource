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
    procedure CreateIdx;
    function Get(nr:longint):string;
    function Get3(nr:longint;const s1,s2,s3:string):string;
    function Get2(nr:longint;const s1,s2:string):string;
    function Get1(nr:longint;const s1:string):string;
  end;

implementation

uses
  strings;

constructor TMessage.Init(p:pointer;n:longint);
begin
  msgtxt:=pchar(p);
  msgsize:=0;
  msgs:=n;
  CreateIdx;
end;


constructor TMessage.InitExtern(const fn:string;n:longint);
const
  bufsize=8192;
var
  f       : text;
  line,i  : longint;
  ptxt    : pchar;
  s       : string;
  buf     : pointer;
begin
  getmem(buf,bufsize);
{Read the message file}
  assign(f,fn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+fn+' not found ***');
     exit;
   end;
  settextbuf(f,buf^,bufsize);
{ First parse the file and count bytes needed }
  line:=0;
  msgs:=n;
  msgsize:=0;
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if (s<>'') and not(s[1] in ['#',';','%']) then
      begin
        i:=pos('=',s);
        if i>0 then
         inc(msgsize,length(s)-i+1)
        else
         writeln('error in line: ',line,' skipping');
      end;
   end;
{ now read the buffer in mem }
  getmem(msgtxt,msgsize);
  ptxt:=msgtxt;
  reset(f);
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if (s<>'') and not(s[1] in ['#',';']) then
      begin
        i:=pos('=',s);
        if i>0 then
         begin
           {txt}
           move(s[i+1],ptxt^,length(s)-i);
           inc(ptxt,length(s)-i);
           ptxt^:=#0;
           inc(ptxt);
         end;
      end;
   end;
  close(f);
  freemem(buf,bufsize);
{ now we can create the index }
  CreateIdx;
end;



destructor TMessage.Done;
begin
  if not (msgidx=nil) then
   freemem(msgidx,msgs shl 2);
  if msgsize>0 then
   freemem(msgtxt,msgsize);
end;


procedure TMessage.CreateIdx;
var
  hp  : pchar;
  hpl : ppchar;
  n   : longint;
begin
  getmem(msgidx,msgs shl 2);
  hpl:=msgidx;
  hp:=msgtxt;
  n:=0;
  while (n<msgs) do
   begin
     hpl^:=hp;
     hpl:=pointer(longint(hpl)+4);
     inc(n);
     hp:=pchar(@hp[strlen(hp)+1]);
   end;
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
  Revision 1.3  1998-08-29 13:52:31  peter
    + new messagefile
    * merged optione.msg into errore.msg

  Revision 1.2  1998/08/18 09:05:00  peter
    * fixed range errror

}
