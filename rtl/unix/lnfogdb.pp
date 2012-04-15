{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by Jonas Maebe

    Stabs Line Info Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}
unit lnfogdb;

interface

{$S-}
{$Q-}

function GetLineInfo(addr:ptruint;var func,source:string;var line:longint) : boolean;

implementation

uses
  ctypes,baseunix,unix;

function GetLineInfo(addr:ptruint;var func,source:string;var line:longint) : boolean;
  var
    mypid: pid_t;
    res,
    err: cint;
    command,
    pidstr: string;
    commfile,
    resfile: text;
  begin
    GetLineInfo:=false;
    {$i-}
    { reset inoutres in case it was set by a previous operation }
    ioresult;
    mypid:=fpgetpid;
    str(mypid,pidstr);
    { create temporary file containig gdb command }
    assign(commfile,'/tmp/fpcbt'+pidstr);
    rewrite(commfile);
    if ioresult<>0 then
      exit;
    str(addr,command);
    writeln(commfile,'attach '+pidstr);
    writeln(commfile,'info line *'+command);
    res:=ioresult;
    close(commfile);
    if (res<>0) or
       (ioresult<>0) then
      begin
        erase(commfile);
        exit;
      end;
    { execute gdb to get the linenr info (set language to English (=C) for
      parsing reasons) }
    res:=fpsystem('LANG=C gdb '+paramstr(0)+' -n -batch -x /tmp/fpcbt'+pidstr+' > /tmp/fpcbt'+pidstr+'.out');
    erase(commfile);
{$ifdef DEBUG_LINEINFO}
    writeln('rescode from executing gdb: ',res);
{$endif}
    if res<>0 then
      exit(false);
    assign(resfile,'/tmp/fpcbt'+pidstr+'.out');
    reset(resfile);
    if ioresult<>0 then
      begin
        erase(resfile);
        exit;
      end;
    { get last line }
    while not eof(resfile) do
      readln(resfile,command);
    res:=ioresult;
    close(resfile);
    { clear inoutres, don't really care about result of close }
    ioresult;
    erase(resfile);
    if (res<>0) or
       (ioresult<>0) then
      exit;
    { format:
        Line 16 of "hello.pp" starts at address 0x100003a4 <PASCALMAIN+24> and ends at 0x100003b0 <PASCALMAIN+36>.
          or
        No line number information available for address 0x3aca
     }
{$ifdef DEBUG_LINEINFO}
     writeln('gdb result: ',command);
{$endif}
     if copy(command,1,5)<>'Line ' then
       exit(false);
     { extract line number }
     delete(command,1,5);
     res:=pos(' ',command);
     if res=0 then
       exit(false);
     val(copy(command,1,res-1),line,err);
     if err<>0 then
       exit;
     { extra file name }
     delete(command,1,res+4);
     res:=pos('"',command);
     if res=0 then
       exit;
     source:=copy(command,1,res-1);
     { if we can't extract the function name: no big deal }
     func:='';
     GetLineInfo:=true;
     res:=pos('<',command);
     if res=0 then
       exit;
     delete(command,1,res);
     res:=pos('>',command);
     if res=0 then
       res:=length(command)
     else
       begin
         err:=pos('+',command);
         if err<res then
           res:=err;
       end;
     func:=copy(command,1,res-1)
  end;

function GdbBackTraceStr(addr:Pointer):shortstring;
var
  func,
  source : string;
  hs     : string[32];
  line   : longint;
  Store  : TBackTraceStrFunc;
  Success : boolean;
begin
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'StabxBackTraceStr called');
{$endif DEBUG_LINEINFO}
  { reset to prevent infinite recursion if problems inside the code PM }
  Success:=false;
  Store:=BackTraceStrFunc;
  BackTraceStrFunc:=@SysBackTraceStr;
  Success:=GetLineInfo(ptruint(addr),func,source,line);
{ create string }
  GdbBackTraceStr:='  $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
  if func<>'' then
    GdbBackTraceStr:=GdbBackTraceStr+'  '+func;
  if source<>'' then
   begin
     if func<>'' then
      GdbBackTraceStr:=GdbBackTraceStr+', ';
     if line<>0 then
      begin
        str(line,hs);
        GdbBackTraceStr:=GdbBackTraceStr+' line '+hs;
      end;
     GdbBackTraceStr:=GdbBackTraceStr+' of '+source;
   end;
  if Success then
    BackTraceStrFunc:=Store;
end;


initialization
  BackTraceStrFunc:=@GdbBackTraceStr;

end.

