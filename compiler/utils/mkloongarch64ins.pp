{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    Convert loongarchins.dat from Nasm to a .inc file for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

program mkloongarch64ins;

const
  Version = '1.0';

var
  insfile,opfile,nopfile,attfile : text;

type
  topcodes = array[1..64] of string;

procedure bug(errormsg : string);
begin
  writeln(errormsg);
  close(insfile);
  close(opfile);
  close(nopfile);
  close(attfile);
  halt;
end;

procedure copy_present_opcodes(start,len : longint; var ops : topcodes);
var
  i : longint;
begin
  for i:=start to start+len-1 do
    ops[i]:=ops[1+i-start];
end;

procedure set_suffix(start,len : longint; var ops : topcodes; suffix : string);
var
  i : longint;
begin
  for i:=start to start+len-1 do
    ops[i]:=ops[i]+suffix;
end;

function decode_format(format,prefix : string; var ops : topcodes) : longint;
var
  i,j,nr_comma,last_comma,nr_op : longint;
  suffixs : string;
begin
  nr_op:=1;
  ops[1]:=prefix;
  i:=1;
  while (format[i]<>'0') do
    begin
      case format[i] of
        'a': suffixs:='W,D';
        'b': suffixs:='W';
        'c': suffixs:='D';
        'd': suffixs:='W,WU,D';
        'e': suffixs:='W,WU,D,DU';
        'f': suffixs:='W,WU';
        'g': suffixs:='B,H';
        'h': suffixs:='2H,4H,2W,D';
        'i': suffixs:='2W,D';
        'j': suffixs:='4B,8B,W,D';
        'k': suffixs:='B,H,W,D';
        'l': suffixs:='BU,HU,WU';
        'm': suffixs:='S,D';
        'n': suffixs:='CAF,CUN,CEQ,CUEQ,CLT,CULT,CUGT,CLE,CULE,CUGE,CNE,COR,CUNE,SAF,SUN,SEQ,SUEQ,SLT,SGT,SULT,SLE,SGE,SULE,SNE,SOR,SUNE';
        'o': suffixs:='S';
        'p': suffixs:='D';
        'q': suffixs:='L,W';
        'r': suffixs:='GLOBAL,LOCAL,ABS,PCREL,GOT,TLE#LE,TLS#IE,TLS#LD,TLS#GD';
      else
        bug('Error Format');
      end;
      i:=i+1;

      { For each comma, add suffix for present opcodes }
      nr_comma:=1;
      last_comma:=length(suffixs)+1;
      for j:=length(suffixs) downto 0 do
        begin
          if (j=0) then
            set_suffix(1,nr_op,ops,'#'+copy(suffixs,1,last_comma-1));
          if (suffixs[j]<>',') then
            continue;
          copy_present_opcodes(nr_comma*nr_op+1, nr_op, ops);
          set_suffix(nr_comma*nr_op+1,nr_op,ops,'#'+copy(suffixs,j+1,last_comma-j-1));
          last_comma:=j;
          nr_comma:=nr_comma+1;
        end;
      nr_op:=nr_comma*nr_op;
    end;
    result:=nr_op;
end;

procedure writeop(op : string);
var
  i : longint;
  s : string;
begin
  for i:=1 to length(op) do
    if op[i]='#' then
      s[i]:='_'
    else
      s[i]:=op[i];
  s[0]:=op[0];
  write(opfile, 'A_', s);
end;

procedure writeatt(op : string);
var
  i : longint;
  s : string;
begin
  for i:=1 to length(op) do
    if op[i] in ['A'..'Z'] then
      s[i]:=char(byte(op[i])+32)
    else if op[i]='#' then
      s[i]:='.'
    else
      s[i]:=op[i];
  s[0]:=op[0];
  write(attfile, '''', s, '''');
end;

var
  i,j,all_op,nr_op : longint;
  s : string;
  opcode : string;
  opcodes : topcodes;
  is_not_first_op : boolean;
begin
  writeln('FPC Instruction Table Converter Version ',Version);
  assign(insfile,'../loongarch64/loongarchins.dat');
  reset(insfile);
  assign(opfile,'../loongarch64/loongarch64op.inc');
  rewrite(opfile);
  writeln(opfile,'{ don''t edit, this file is generated from loongarchins.dat }');
  writeln(opfile,'(');
  assign(nopfile,'../loongarch64/loongarch64nop.inc');
  rewrite(nopfile);
  writeln(nopfile,'{ don''t edit, this file is generated from loongarchins.dat }');
  assign(attfile,'../loongarch64/loongarch64att.inc');
  rewrite(attfile);
  writeln(attfile,'{ don''t edit, this file is generated from loongarchins.dat }');
  writeln(attfile,'(');

  all_op:=0;
  is_not_first_op:=false;
  while not(eof(insfile)) do
    begin
      readln(insfile,s);
      if (s='') or (s[1]=';') then
        continue;
      if (s[1]<>'[') then
        continue;
      i:=pos(']',s);
      opcode:=copy(s,2,i-2);
      i:=pos('(',s);
      j:=pos(')',s);
      nr_op:=decode_format(copy(s,i+1,j-i-1),opcode,opcodes);
      for i:=1 to nr_op do
        begin
          if is_not_first_op then
            begin
              writeln(opfile,',');
              writeln(attfile,',');
            end;
          writeop(opcodes[i]);
          if opcodes[i]='BXX' then
            writeatt('B')
          else
            writeatt(opcodes[i]);
          is_not_first_op:=true;
        end;
      all_op:=all_op+nr_op;
    end;
  writeln(opfile);
  write(opfile,');');
  writeln(attfile);
  write(attfile,');');
  write(nopfile, all_op, ';');
  close(insfile);
  close(opfile);
  close(nopfile);
  close(attfile);
end.
