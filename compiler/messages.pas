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

const
  maxmsgidxparts = 20;

type
  ppchar=^pchar;

  TArrayOfPChar = array[0..1000] of pchar;
  PArrayOfPChar = ^TArrayOfPChar;

  PMessage=^TMessage;
  TMessage=object
    msgfilename : string;
    msgallocsize,
    msgsize,
    msgparts,
    msgs        : longint;
    msgtxt      : pchar;
    msgidx      : array[1..maxmsgidxparts] of PArrayOfPChar;
    msgidxmax   : array[1..maxmsgidxparts] of longint;
    constructor Init(n:longint;const idxmax:array of longint);
    destructor  Done;
    function  LoadIntern(p:pointer;n:longint):boolean;
    function  LoadExtern(const fn:string):boolean;
    procedure CreateIdx;
    function  GetPChar(nr:longint):pchar;
    function  Get(nr:longint):string;
    function  Get3(nr:longint;const s1,s2,s3:string):string;
    function  Get2(nr:longint;const s1,s2:string):string;
    function  Get1(nr:longint;const s1:string):string;
  end;

{ this will read a line until #10 or #0 and also increase p }
function GetMsgLine(var p:pchar):string;


implementation

uses
  globals,crc,
{$ifdef DELPHI}
  sysutils;
{$else DELPHI}
  strings;
{$endif DELPHI}

constructor TMessage.Init(n:longint;const idxmax:array of longint);
var
  i : longint;
begin
  msgtxt:=nil;
  msgsize:=0;
  msgparts:=n;
  if n<>high(idxmax)+1 then
   fail;
  for i:=1to n do
   begin
     msgidxmax[i]:=idxmax[i-1];
     getmem(msgidx[i],msgidxmax[i]*4);
     fillchar(msgidx[i]^,msgidxmax[i]*4,0);
   end;
end;


destructor TMessage.Done;
var
  i : longint;
begin
  for i:=1to msgparts do
   freemem(msgidx[i],msgidxmax[i]*4);
  if msgallocsize>0 then
   begin
     freemem(msgtxt,msgsize);
     msgallocsize:=0;
   end;
  msgtxt:=nil;
  msgsize:=0;
  msgparts:=0;
end;


function TMessage.LoadIntern(p:pointer;n:longint):boolean;
begin
  msgtxt:=pchar(p);
  msgsize:=n;
  msgallocsize:=0;
  CreateIdx;
  LoadIntern:=true;
end;


function TMessage.LoadExtern(const fn:string):boolean;

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
  error,multiline : boolean;
  code : word;
  numpart,numidx,
  line,i,j,num : longint;
  ptxt    : pchar;
  number,
  s,s1    : string;
  buf     : pointer;

  procedure err(const msgstr:string);
  begin
    writeln('error in line ',line,': ',msgstr);
    error:=true;
  end;

begin
  LoadExtern:=false;
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
  error:=false;
  line:=0;
  multiline:=false;
  msgsize:=0;
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if multiline then
      begin
        if s=']' then
         multiline:=false
        else
         inc(msgsize,length(s)+1); { +1 for linebreak }
      end
     else
      begin
        if (s<>'') and not(s[1] in ['#',';','%']) then
         begin
           i:=pos('=',s);
           if i>0 then
            begin
              j:=i+1;
              if not(s[j] in ['0'..'9']) then
               err('no number found')
              else
               begin
                 while (s[j] in ['0'..'9']) do
                  inc(j);
               end;
              if j-i-1<>5 then
               err('number length is not 5');
              number:=Copy(s,i+1,j-i-1);
              { update the max index }
              val(number,num,code);
              numpart:=num div 1000;
              numidx:=num mod 1000;
              { check range }
              if numpart > msgparts then
               err('number is to large')
              else
               if numidx >= msgidxmax[numpart] then
                err('index is to large');
              if s[j+1]='[' then
               begin
                 inc(msgsize,j-i);
                 multiline:=true
               end
              else
               inc(msgsize,length(s)-i+1);
            end
           else
            err('no = found');
         end;
      end;
   end;
  if multiline then
   err('still in multiline mode');
  if error then
   begin
     freemem(buf,bufsize);
     close(f);
     exit;
   end;
{ now read the buffer in mem }
  msgallocsize:=msgsize;
  getmem(msgtxt,msgallocsize);
  ptxt:=msgtxt;
  reset(f);
  while not eof(f) do
   begin
     readln(f,s);
     if multiline then
      begin
        if s=']' then
         begin
           multiline:=false;
           { overwrite last eol }
           dec(ptxt);
           ptxt^:=#0;
           inc(ptxt);
         end
        else
         begin
           move(s[1],ptxt^,length(s));
           inc(ptxt,length(s));
           ptxt^:=#10;
           inc(ptxt);
         end;
      end
     else
      begin
        if (s<>'') and not(s[1] in ['#',';','%']) then
         begin
           i:=pos('=',s);
           if i>0 then
            begin
              j:=i+1;
              while (s[j] in ['0'..'9']) do
               inc(j);
              { multiline start then no txt }
              if s[j+1]='[' then
               begin
                 s1:=Copy(s,i+1,j-i);
                 move(s1[1],ptxt^,length(s1));
                 inc(ptxt,length(s1));
                 multiline:=true;
               end
              else
               begin
                 { txt including number }
                 s1:=Copy(s,i+1,255);
                 move(s1[1],ptxt^,length(s1));
                 inc(ptxt,length(s1));
                 ptxt^:=#0;
                 inc(ptxt);
               end;
            end;
         end;
      end;
   end;
  close(f);
  freemem(buf,bufsize);
{ now we can create the index }
  CreateIdx;
  LoadExtern:=true;
end;


procedure TMessage.CreateIdx;
var
  hp1,
  hp,hpend : pchar;
  code : word;
  num  : longint;
  number : string[5];
  i   : longint;
  numpart,numidx : longint;
begin
  { clear }
  for i:=1to msgparts do
   fillchar(msgidx[i]^,msgidxmax[i]*4,0);
  { process msgtxt buffer }
  number:='00000';
  hp:=msgtxt;
  hpend:=@msgtxt[msgsize];
  while (hp<hpend) do
   begin
     hp1:=hp;
     for i:=1to 5 do
      begin
        number[i]:=hp1^;
        inc(hp1);
      end;
     val(number,num,code);
     numpart:=num div 1000;
     numidx:=num mod 1000;
     { skip _ }
     inc(hp1);
     { put the address in the idx, the numbers are already checked }
     msgidx[numpart]^[numidx]:=hp1;
     { next string }
     hp:=pchar(@hp[strlen(hp)+1]);
   end;
end;


function GetMsgLine(var p:pchar):string;
var
  i  : longint;
begin
  i:=0;
  while not(p^ in [#0,#10]) and (i<255) do
   begin
     inc(i);
     GetMsgLine[i]:=p^;
     inc(p);
   end;
  { skip #10 }
  if p^=#10 then
   inc(p);
  { if #0 then set p to nil }
  if p^=#0 then
   p:=nil;
  { return string }
  GetMsgLine[0]:=chr(i);
end;


function TMessage.GetPChar(nr:longint):pchar;
begin
  GetPChar:=msgidx[nr div 1000]^[nr mod 1000];
end;


function TMessage.Get(nr:longint):string;
var
  s : string[16];
  hp : pchar;
begin
  hp:=msgidx[nr div 1000]^[nr mod 1000];
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
  i:=pos('$1',s);
  if i>0 then
   begin
     Delete(s,i,2);
     Insert(s1,s,i);
   end;
{ $2 -> s2 }
  i:=pos('$2',s);
  if i>0 then
   begin
     Delete(s,i,2);
     Insert(s2,s,i);
   end;
{ $3 -> s3 }
  i:=pos('$3',s);
  if i>0 then
   begin
     Delete(s,i,2);
     Insert(s3,s,i);
   end;
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
  Revision 1.16  2000-06-30 20:23:36  peter
    * new message files layout with msg numbers (but still no code to
      show the number on the screen)

  Revision 1.15  2000/06/18 18:14:21  peter
    * only replace the $1,$2,$3 once, so it doesn't loop when the
      value to replace with contains $1,$2 or $3

  Revision 1.14  2000/05/23 20:32:11  peter
    * fixed wrong code not detected due a bug in FPC

  Revision 1.13  2000/05/15 14:07:33  pierre
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