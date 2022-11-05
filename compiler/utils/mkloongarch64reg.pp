{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    Convert loongarchreg.dat to a .inc file for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

program mkloongarchreg;

const
  Version = '1.0';
  max_regcount = 512;

var
  nr_regs : word;
  srcfile : text;
  names,abinames,stdnames : array[0..max_regcount] of string;
  regtypes,subtypes,values,stabs,dwarf,std_regname_index,regnumber_index : array[0..max_regcount] of word;
  numbers : array[0..max_regcount] of longint;

procedure bug(errormsg : string);
begin
  writeln(errormsg);
  close(srcfile);
  halt;
end;

function str2word(s : string) : word;
var
  v,errcode : integer;
begin
  val(s,v,errcode);
  if (errcode<>0) or (v>65535) or (v<0) then
    str2word:=0
  else
    str2word:=word(v);
end;

procedure readdatfile;
var
  i,last,idx : longint;
  s,subs : string;
begin
  assign(srcfile, '../loongarch64/loongarchreg.dat');
  reset(srcfile);
  nr_regs:=0;
  while not(eof(srcfile)) do
    begin
      readln(srcfile,s);
      if (s='') or (s[1]=';') then
        continue;
      { <name>,<type>,<subtype>,<value>,<abiname>,<stdname>,<stab idx>,<dwarf idx> }
      last:=length(s)+1;
      idx:=1;
      for i:=length(s) downto 0 do
        begin
          if (i=0) then
            begin
              if (idx<>8) then
                bug('Incomplete tables');
              names[nr_regs]:=copy(s,1,last-1);
              continue;
            end;
          if (s[i]<>',') then
            continue;
          subs:=copy(s,i+1,last-i-1);
          case (idx) of
            1: dwarf[nr_regs]:=str2word(subs);
            2: stabs[nr_regs]:=str2word(subs);
            3: stdnames[nr_regs]:=subs;
            4: abinames[nr_regs]:=subs;
            5: values[nr_regs]:=str2word(subs);
            6: subtypes[nr_regs]:=str2word(subs);
            7: regtypes[nr_regs]:=str2word(subs);
            8: bug('Overflow tables');
          end;
          idx:=idx+1;
          last:=i;
        end;
      nr_regs:=nr_regs+1;
    end;
end;

type
  SWAP_FUNC=procedure (i,j : longint; p : pointer);
  CMPR_FUNC=function (i,j : longint; p :pointer) : boolean;

procedure qsort(l,r : longint; p : pointer; s : SWAP_FUNC; c : CMPR_FUNC);
var
  i,j : longint;
begin
  if l>=r then
    exit;
  i:=l;
  j:=r;
  while i<j do
    begin
      while (i<j) and c(i,j,p) do
        j:=j-1;
      if i<j then
        begin
          s(i,j,p);
          i:=i+1;
        end;
      while (i<j) and c(i,j,p) do
        i:=i+1;
      if i<j then
        begin
          s(i,j,p);
          j:=j-1;
        end;
    end;
  qsort(l,i-1,p,s,c);
  qsort(i+1,r,p,s,c);
end;

procedure swap_rni(i,j : longint; p : pointer);
var
  t : word;
begin
  t:=regnumber_index[i];
  regnumber_index[i]:=regnumber_index[j];
  regnumber_index[j]:=t;
end;

function cmpr_rn(i,j : longint; p :pointer) : boolean;
begin
  cmpr_rn:=numbers[regnumber_index[i]]<numbers[regnumber_index[j]];
end;

procedure swap_sri(i,j : longint; p : pointer);
var
  t : word;
begin
  t:=std_regname_index[i];
  std_regname_index[i]:=std_regname_index[j];
  std_regname_index[j]:=t;
end;

function cmpr_sr(i,j : longint; p :pointer) : boolean;
begin
  cmpr_sr:=stdnames[std_regname_index[i]]<stdnames[std_regname_index[j]];
end;

procedure build_regnum_index;
var
  i :longint;
  s : SWAP_FUNC;
  c : CMPR_FUNC;
begin
  for i:=0 to nr_regs-1 do
    regnumber_index[i]:=i;
  s:=@swap_rni;
  c:=@cmpr_rn;
  qsort(0,nr_regs-1,nil,s,c);
end;

procedure build_std_regname_index;
var
  i : longint;
  s : SWAP_FUNC;
  c : CMPR_FUNC;
begin
  for i:=0 to nr_regs-1 do
    std_regname_index[i]:=i;
  s:=@swap_sri;
  c:=@cmpr_sr;
  qsort(0,nr_regs-1,nil,s,c);
end;

procedure setarrays;
var
  i : longint;
begin
  for i:=0 to nr_regs-1 do
    numbers[i]:=(regtypes[i] shl 24) or (subtypes[i] shl 16) or values[i];
  build_regnum_index;
  build_std_regname_index;
end;

procedure openinc(out f:text;const fn:string);
begin
  writeln('creating ',fn);
  assign(f,fn);
  rewrite(f);
  writeln(f,'{ don''t edit, this file is generated from loongarchreg.dat }');
end;

procedure closeinc(var f:text);
begin
  writeln(f);
  close(f);
end;

procedure write_inc_files;
var
  first : boolean;
  numfile,stdfile,stabfile,dwarffile,rnifile,srifile,supfile,norfile,confile,abinamefile : text;
  i : longint;
begin
  { create inc files }
  openinc(confile,'../loongarch64/rloongarch64con.inc');
  openinc(supfile,'../loongarch64/rloongarch64sup.inc');
  openinc(numfile,'../loongarch64/rloongarch64num.inc');
  openinc(stdfile,'../loongarch64/rloongarch64std.inc');
  openinc(abinamefile,'../loongarch64/rloongarch64abi.inc');
  openinc(stabfile,'../loongarch64/rloongarch64sta.inc');
  openinc(dwarffile,'../loongarch64/rloongarch64dwa.inc');
  openinc(rnifile,'../loongarch64/rloongarch64rni.inc');
  openinc(srifile,'../loongarch64/rloongarch64sri.inc');
  first:=true;
  for i:=0 to nr_regs-1 do
    begin
      if not first then
        begin
          writeln(numfile,',');
          writeln(stdfile,',');
          writeln(abinamefile,',');
          writeln(stabfile,',');
          writeln(dwarffile,',');
          writeln(rnifile,',');
          writeln(srifile,',');
        end
      else
        first:=false;
      writeln(supfile,'RS_',names[i],' = ',values[i],';');
      writeln(confile,'NR_'+names[i],' = ','tregister(',numbers[i],')',';');
      write(numfile,'tregister(',numbers[i],')');
      write(stdfile,'''',stdnames[i],'''');
      write(abinamefile,'''',abinames[i],'''');
      write(stabfile,stabs[i]);
      write(dwarffile,dwarf[i]);
      write(rnifile,regnumber_index[i]);
      write(srifile,std_regname_index[i]);
    end;
  openinc(norfile,'../loongarch64/rloongarch64nor.inc');
  write(norfile,nr_regs);
  closeinc(norfile);
  close(confile);
  close(supfile);
  closeinc(numfile);
  closeinc(stdfile);
  closeinc(abinamefile);
  closeinc(stabfile);
  closeinc(dwarffile);
  closeinc(rnifile);
  closeinc(srifile);
  writeln('Done!');
  writeln(nr_regs,' registers processed');
end;

begin
   writeln('Register Table Converter Version ',Version);
   readdatfile;
   setarrays;
   write_inc_files;
end.

