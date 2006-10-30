{
    Copyright (c) 1998-2002 by Peter Vreman

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
unit cmsgs;

{$i fpcdefs.inc}

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
    msgintern   : boolean;
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
    procedure ClearIdx;
    procedure CreateIdx;
    function  GetPChar(nr:longint):pchar;
    function  Get(nr:longint;const args:array of string):string;
  end;

{ this will read a line until #10 or #0 and also increase p }
function GetMsgLine(var p:pchar):string;


implementation

uses
  SysUtils,
  cutils;


function MsgReplace(const s:string;const args:array of string):string;
var
  last,
  i  : longint;
  hs : string;

begin
  if s='' then
    begin
      MsgReplace:='';
      exit;
    end;
  hs:='';
  i:=0;
  last:=0;
  while (i<length(s)-1) do
    begin
      inc(i);
      if (s[i]='$') and (s[i+1] in ['1'..'9']) then
        begin
          hs:=hs+copy(s,last+1,i-last-1)+args[byte(s[i+1])-byte('1')];
          inc(i);
          last:=i;
        end;
    end;
  MsgReplace:=hs+copy(s,last+1,length(s)-last);;
end;



constructor TMessage.Init(n:longint;const idxmax:array of longint);
var
  i : longint;
begin
  msgtxt:=nil;
  msgsize:=0;
  msgparts:=n;
  if n<>high(idxmax)+1 then
   fail;
  for i:=1 to n do
   begin
     msgidxmax[i]:=idxmax[i-1];
     getmem(msgidx[i],msgidxmax[i]*sizeof(pointer));
     fillchar(msgidx[i]^,msgidxmax[i]*sizeof(pointer),0);
   end;
end;


destructor TMessage.Done;
var
  i : longint;
begin
  for i:=1 to msgparts do
   freemem(msgidx[i],msgidxmax[i]*sizeof(pointer));
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
  msgintern:=true;
  ClearIdx;
  CreateIdx;
  LoadIntern:=true;
end;


function TMessage.LoadExtern(const fn:string):boolean;
const
  bufsize=8192;
var
  f       : text;
  error,multiline : boolean;
  line,i,j : longint;
  ptxt    : pchar;
  s,s1    : string;
  buf     : pointer;

  procedure err(const msgstr:string);
  begin
    writeln('*** PPC, file ',fn,', error in line ',line,': ',msgstr);
    error:=true;
  end;

begin
  LoadExtern:=false;
  getmem(buf,bufsize);
  { Read the message file }
  assign(f,fn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** PPC, can not open message file ',fn);
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
{ now we can create the index, clear if the previous load was also
  an external file, because those can't be reused }
  if not msgintern then
   ClearIdx;
  CreateIdx;
{ set that we've loaded an external file }
  msgintern:=false;
  LoadExtern:=true;
end;


procedure TMessage.ClearIdx;
var
  i : longint;
begin
  { clear }
  for i:=1 to msgparts do
   fillchar(msgidx[i]^,msgidxmax[i]*sizeof(pointer),0);
end;


procedure TMessage.CreateIdx;
var
  hp1,
  hp,hpend : pchar;
  code : integer;
  num  : longint;
  number : string[5];
  i   : longint;
  numpart,numidx : longint;
begin
  { process msgtxt buffer }
  number:='00000';
  hp:=msgtxt;
  hpend:=@msgtxt[msgsize];
  while (hp<hpend) do
   begin
     hp1:=hp;
     for i:=1 to 5 do
      begin
        number[i]:=hp1^;
        inc(hp1);
      end;
     val(number,num,code);
     numpart:=num div 1000;
     numidx:=num mod 1000;
     { check range }
     if (numpart <= msgparts) and (numidx < msgidxmax[numpart]) then
      begin
        { skip _ }
        inc(hp1);
        { put the address in the idx, the numbers are already checked }
        msgidx[numpart]^[numidx]:=hp1;
      end;
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


function TMessage.Get(nr:longint;const args:array of string):string;
var
  hp : pchar;
begin
  hp:=msgidx[nr div 1000]^[nr mod 1000];
  if hp=nil then
    Get:='msg nr '+tostr(nr)
  else
    Get:=MsgReplace(strpas(hp),args);
end;

end.
