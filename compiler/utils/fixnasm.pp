{
    $Id: fixnasm.pp,v 1.6 2005/02/14 17:13:10 peter Exp $
    Copyright (c) 1998-2002 by Peter Vreman

    Convert insns.dat from Nasm to an i386ins.dat for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fixnasm;

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
  spaces='                                                       ';
var
  t,f : text;
  para,bytes,flags,
  opcode,lastop,
  s : string;
  i,j : longint;
begin
  writeln('Fixing insns.dat -> i386ins.dat');
  assign(t,'insns.dat');
  reset(t);
  assign(f,'insns.new');
  rewrite(f);
  lastop:='';
  while not eof(t) do
   begin
     readln(t,s);
     if (s<>'') and (s[1]<>';') then
      begin
        i:=pos(' ',s);
        j:=pos(',',s);
        if (j>0) and (j<i) then
         opcode:=Copy(s,1,j-1)
        else
         opcode:=Copy(s,1,i-1);
        if opcode<>lastop then
         begin
           writeln(f,'');
           writeln(f,'[',Copy(s,1,i-1),']');
           writeln(f,'(Ch_All, Ch_None, Ch_None)');
           lastop:=opcode;
         end;
        while (i<length(s)) and (s[i+1]=' ') do
         inc(i);
        Delete(s,1,i);
        i:=pos(' ',s);
        para:=Copy(s,1,i-1);
        para:=para+Copy(spaces,1,22-length(para));
        while (i<length(s)) and (s[i+1]=' ') do
         inc(i);
        Delete(s,1,i);
        i:=pos(' ',s);
        bytes:=Copy(s,1,i-1);
        bytes:=bytes+Copy(spaces,1,32-length(bytes));
        while (i<length(s)) and (s[i+1]=' ') do
         inc(i);
        Delete(s,1,i);
        i:=pos(' ',s);
        if i=0 then
         i:=255;
        flags:=Copy(s,1,i-1);
        writeln(f,para,bytes,flags);
      end
     else
      writeln(f,s);
   end;
  close(f);
  close(t);
end.
{
  $Log: fixnasm.pp,v $
  Revision 1.6  2005/02/14 17:13:10  peter
    * truncate log

}
