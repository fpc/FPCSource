{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
    msgallocsize,
    msgsize,
    msgcrc,
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
  globals,crc,
  verbose,
{$ifdef DELPHI}
  sysutils;
{$else DELPHI}
  strings;
{$endif DELPHI}

constructor TMessage.Init(p:pointer;n:longint);
begin
  msgtxt:=pchar(p);
  msgallocsize:=0;
  msgsize:=0;
  msgcrc:=MsgCrcValue;
  msgs:=n;
  CreateIdx;
end;


constructor TMessage.InitExtern(const fn:string;n:longint);

{$ifndef FPC}
  procedure readln(var t:text;var s:string);
  var
    c : char;
    i : longint;
  begin
    c:=#0;
    i:=0;
    while (not eof(t)) and (c<>#10) do
     begin
       read(t,c);
       if c<>#10 then
        begin
          inc(i);
          s[i]:=c;
        end;
     end;
    if (i>0) and (s[i]=#13) then
     dec(i);
    s[0]:=chr(i);
  end;
{$endif}

const
  bufsize=8192;
var
  f       : text;
{$ifdef DEBUGCRC}
  f2 : text;
{$endif DEBUGCRC}
  msgsread,
  line,i,crc  : longint;
  ptxt    : pchar;
  s,s1    : string;
  buf     : pointer;
begin
  crc:=longint($ffffffff);
  getmem(buf,bufsize);
{Read the message file}
  assign(f,fn);
{$ifdef DEBUGCRC}
  assign(f2,'crcmsg.tst');
  rewrite(f2);
  Writeln(f2,crc);
{$endif DEBUGCRC}
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+fn+' not found ***');
     fail;
   end;
  settextbuf(f,buf^,bufsize);
{ First parse the file and count bytes needed }
  line:=0;
  msgs:=n;
  msgsize:=0;
  msgsread:=0;
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if (s<>'') and not(s[1] in ['#',';','%']) then
      begin
        i:=pos('=',s);
        if i>0 then
         begin
           inc(msgsize,length(s)-i+1);
           inc(msgsread);
         end
        else
         writeln('error in line: ',line,' skipping');
      end;
   end;
{ check amount of messages }
  if msgsread<>msgs then
   begin
     WriteLn('*** message file '+fn+' is corrupt: read ',msgsread,' of ',msgs,' msgs ***');
     close(f);
     freemem(buf,bufsize);
     fail;
   end;
{ now read the buffer in mem }
  msgallocsize:=msgsize;
  getmem(msgtxt,msgallocsize);
  ptxt:=msgtxt;
  reset(f);
  while not eof(f) do
   begin
     readln(f,s);
     if (s<>'') and not(s[1] in ['#',';','%']) then
      begin
        i:=pos('=',s);
        if i>0 then
         begin
           {txt}
           s1:=Copy(s,i+1,255);
           { support <lf> for empty lines }
           if s1='<lf>' then
            begin
              s1:='';
              { update the msgsize also! }
              dec(msgsize,4);
            end;
           {txt}
           move(s1[1],ptxt^,length(s1));
           inc(ptxt,length(s1));
           ptxt^:=#0;
           inc(ptxt);
           s1:=upper(copy(s,1,i-1));
           crc:=UpdateCRC32(crc,@s1[1],length(s1));
{$ifdef DEBUGCRC}
           Writeln(f2,s1);
           Writeln(f2,crc);
{$endif DEBUGCRC}
         end;
      end;
   end;
  close(f);
{$ifdef DEBUGCRC}
  close(f2);
{$endif DEBUGCRC}
  freemem(buf,bufsize);
{ check amount of messages }
  if (MsgCrcValue<>0) and (crc<>MsgCrcValue) then
   begin
     WriteLn('*** message file '+fn+' is incompatible : wrong CRC value ***');
     fail;
   end;
{ now we can create the index }
  CreateIdx;
end;



destructor TMessage.Done;
begin
  if assigned(msgidx) then
   begin
     freemem(msgidx,msgs shl 2);
     msgidx:=nil;
   end;
  if msgallocsize>0 then
   begin
     freemem(msgtxt,msgallocsize);
     msgtxt:=nil;
     msgallocsize:=0;
   end;
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
  if s1<>'$1' then
   repeat
     i:=pos('$1',s);
     if i>0 then
      begin
        Delete(s,i,2);
        Insert(s1,s,i);
      end;
   until i=0;
{ $2 -> s2 }
  if s1<>'$2' then
   repeat
     i:=pos('$2',s);
     if i>0 then
      begin
        Delete(s,i,2);
        Insert(s2,s,i);
      end;
   until i=0;
{ $3 -> s3 }
  if s1<>'S3' then
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
  Revision 1.13  2000-05-15 14:07:33  pierre
   + calculate CRC value and check if consistant

  Revision 1.12  2000/03/01 22:29:18  peter
    * message files are check for amount of msgs found. If not correct a
      line is written to stdout and switched to internal messages

  Revision 1.11  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.10  2000/01/23 16:32:08  peter
    * fixed wrong freemem size when loading message file

  Revision 1.9  2000/01/07 01:14:27  peter
    * updated copyright to 2000

}