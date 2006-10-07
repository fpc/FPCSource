{
    Copyright (c) 1998-2002 by Peter Vreman and Florian Klaempfl

    Convert i386reg.dat to several .inc files for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$i+}
program mkx86reg;

const Version = '1.00';
      max_regcount = 128;

var s : string;
    i : longint;
    line : longint;
    regcount:byte;
    regcount_bsstart:byte;
    names,numbers,stdnames,intnames,nasmnames,attnames,stabs,dwarf32,dwarf64,ots,ops:
        array[0..max_regcount-1] of string[63];
    regnumber_index,std_regname_index,int_regname_index,att_regname_index,
    nasm_regname_index:array[0..max_regcount-1] of byte;
    x86_64 : boolean;
    fileprefix : string;

function tostr(l : longint) : string;

begin
  str(l,tostr);
end;

function readstr : string;
  begin
     result:='';
     while (s[i]<>',') and (i<=length(s)) do
       begin
          result:=result+s[i];
          inc(i);
       end;
     readstr:=result;
  end;


procedure readcomma;
  begin
     if s[i]<>',' then
       begin
         writeln('Missing "," at line ',line);
         writeln('Line: "',s,'"');
         halt(1);
       end;
     inc(i);
  end;


procedure skipspace;

  begin
     while (s[i] in [' ',#9]) do
       inc(i);
  end;

procedure openinc(var f:text;const fn:string);
begin
  writeln('creating ',fn);
  assign(f,fn);
  rewrite(f);
  writeln(f,'{ don''t edit, this file is generated from x86reg.dat }');
end;


procedure closeinc(var f:text);
begin
  writeln(f);
  close(f);
end;

procedure build_regnum_index;

var h,i,j,p,t:byte;

begin
  {Build the registernumber2regindex index.
   Step 1: Fill.}
  for i:=0 to regcount-1 do
    regnumber_index[i]:=i;
  {Step 2: Sort. We use a Shell-Metzner sort.}
  p:=regcount_bsstart;
  repeat
    for h:=0 to regcount-p-1 do
      begin
        i:=h;
        repeat
          j:=i+p;
          if numbers[regnumber_index[j]]>=numbers[regnumber_index[i]] then
            break;
          t:=regnumber_index[i];
          regnumber_index[i]:=regnumber_index[j];
          regnumber_index[j]:=t;
          if i<p then
            break;
          dec(i,p);
        until false;
      end;
    p:=p shr 1;
  until p=0;
end;

procedure build_std_regname_index;

var h,i,j,p,t:byte;

begin
  {Build the registernumber2regindex index.
   Step 1: Fill.}
  for i:=0 to regcount-1 do
    std_regname_index[i]:=i;
  {Step 2: Sort. We use a Shell-Metzner sort.}
  p:=regcount_bsstart;
  repeat
    for h:=0 to regcount-p-1 do
      begin
        i:=h;
        repeat
          j:=i+p;
          if stdnames[std_regname_index[j]]>=stdnames[std_regname_index[i]] then
            break;
          t:=std_regname_index[i];
          std_regname_index[i]:=std_regname_index[j];
          std_regname_index[j]:=t;
          if i<p then
            break;
          dec(i,p);
        until false;
      end;
    p:=p shr 1;
  until p=0;
end;

procedure build_int_regname_index;

var h,i,j,p,t:byte;

begin
  {Build the registernumber2regindex index.
   Step 1: Fill.}
  for i:=0 to regcount-1 do
    int_regname_index[i]:=i;
  {Step 2: Sort. We use a Shell-Metzner sort.}
  p:=regcount_bsstart;
  repeat
    for h:=0 to regcount-p-1 do
      begin
        i:=h;
        repeat
          j:=i+p;
          if intnames[int_regname_index[j]]>=intnames[int_regname_index[i]] then
            break;
          t:=int_regname_index[i];
          int_regname_index[i]:=int_regname_index[j];
          int_regname_index[j]:=t;
          if i<p then
            break;
          dec(i,p);
        until false;
      end;
    p:=p shr 1;
  until p=0;
end;

procedure build_att_regname_index;

var h,i,j,p,t:byte;

begin
  {Build the registernumber2regindex index.
   Step 1: Fill.}
  for i:=0 to regcount-1 do
    att_regname_index[i]:=i;
  {Step 2: Sort. We use a Shell-Metzner sort.}
  p:=regcount_bsstart;
  repeat
    for h:=0 to regcount-p-1 do
      begin
        i:=h;
        repeat
          j:=i+p;
          if attnames[att_regname_index[j]]>=attnames[att_regname_index[i]] then
            break;
          t:=att_regname_index[i];
          att_regname_index[i]:=att_regname_index[j];
          att_regname_index[j]:=t;
          if i<p then
            break;
          dec(i,p);
        until false;
      end;
    p:=p shr 1;
  until p=0;
end;

procedure build_nasm_regname_index;

var h,i,j,p,t:byte;

begin
  {Build the registernumber2regindex index.
   Step 1: Fill.}
  for i:=0 to regcount-1 do
    nasm_regname_index[i]:=i;
  {Step 2: Sort. We use a Shell-Metzner sort.}
  p:=regcount_bsstart;
  repeat
    for h:=0 to regcount-p-1 do
      begin
        i:=h;
        repeat
          j:=i+p;
          if nasmnames[nasm_regname_index[j]]>=nasmnames[nasm_regname_index[i]] then
            break;
          t:=nasm_regname_index[i];
          nasm_regname_index[i]:=nasm_regname_index[j];
          nasm_regname_index[j]:=t;
          if i<p then
            break;
          dec(i,p);
        until false;
      end;
    p:=p shr 1;
  until p=0;
end;

procedure read_x86reg_file;

var infile:text;
    cpustr:string;

begin
   { open dat file }
   assign(infile,'x86reg.dat');
   reset(infile);
   while not(eof(infile)) do
     begin
        { handle comment }
        readln(infile,s);
        inc(line);
        while (s[1]=' ') do
         delete(s,1,1);
        if (s='') or (s[1]=';') then
          continue;

        i:=1;
        names[regcount]:=readstr;
        readcomma;
        numbers[regcount]:=readstr;
        readcomma;
        stdnames[regcount]:=readstr;
        readcomma;
        attnames[regcount]:=readstr;
        readcomma;
        intnames[regcount]:=readstr;
        readcomma;
        nasmnames[regcount]:=readstr;
        readcomma;
        stabs[regcount]:=readstr;
        readcomma;
        dwarf32[regcount]:=readstr;
        readcomma;
        dwarf64[regcount]:=readstr;
        readcomma;
        ots[regcount]:=readstr;
        readcomma;
        ops[regcount]:=readstr;
        if s[i]=',' then
          begin
            readcomma;
            cpustr:=readstr;
          end
        else
          cpustr:='';
        if i<length(s) then
          begin
            writeln('Extra chars at end of line, at line ',line);
            writeln('Line: "',s,'"');
            halt(1);
          end;
        if (cpustr<>'64') or x86_64 then
          begin
            inc(regcount);
            if regcount>max_regcount then
              begin
                writeln('Error: Too much registers, please increase maxregcount in source');
                halt(2);
              end;
          end;
     end;
   close(infile);
end;

procedure write_inc_files;

var attfile,intfile,otfile,opfile,
    norfile,nasmfile,stdfile,
    numfile,stabfile,dwrffile,confile,
    rnifile,irifile,srifile,
    arifile,nrifile:text;
    first:boolean;

begin
  { create inc files }
  openinc(confile,fileprefix+'con.inc');
  openinc(numfile,fileprefix+'num.inc');
  openinc(stdfile,fileprefix+'std.inc');
  openinc(attfile,fileprefix+'att.inc');
  openinc(intfile,fileprefix+'int.inc');
  if not(x86_64) then
    begin
      openinc(nasmfile,fileprefix+'nasm.inc');
    end;
  openinc(stabfile,fileprefix+'stab.inc');
  openinc(dwrffile,fileprefix+'dwrf.inc');
  openinc(otfile,fileprefix+'ot.inc');
  openinc(opfile,fileprefix+'op.inc');
  openinc(norfile,fileprefix+'nor.inc');
  openinc(rnifile,fileprefix+'rni.inc');
  openinc(srifile,fileprefix+'sri.inc');
  openinc(arifile,fileprefix+'ari.inc');
  openinc(irifile,fileprefix+'iri.inc');
  if not(x86_64) then
    begin
      openinc(nrifile,fileprefix+'nri.inc');
    end;
  first:=true;
  for i:=0 to regcount-1 do
    begin
      if not first then
        begin
          writeln(numfile,',');
          writeln(stdfile,',');
          writeln(attfile,',');
          writeln(intfile,',');
          if not(x86_64) then
            begin
              writeln(nasmfile,',');
            end;
          writeln(stabfile,',');
          writeln(dwrffile,',');
          writeln(otfile,',');
          writeln(opfile,',');
          writeln(rnifile,',');
          writeln(srifile,',');
          writeln(arifile,',');
          writeln(irifile,',');
          if not(x86_64) then
            begin
              writeln(nrifile,',');
            end;
        end
      else
        first:=false;
      writeln(confile,names[i],' = ','tregister(',numbers[i],')',';');
      write(numfile,'tregister(',numbers[i],')');
      write(stdfile,'''',stdnames[i],'''');
      write(attfile,'''',attnames[i],'''');
      write(intfile,'''',intnames[i],'''');
      if not(x86_64) then
        begin
          write(nasmfile,'''',nasmnames[i],'''');
        end;
      write(stabfile,stabs[i]);
      if x86_64 then
        write(dwrffile,dwarf64[i])
      else
        write(dwrffile,dwarf32[i]);
      write(otfile,ots[i]);
      write(opfile,ops[i]);
      write(rnifile,regnumber_index[i]);
      write(srifile,std_regname_index[i]);
      write(arifile,att_regname_index[i]);
      write(irifile,int_regname_index[i]);
      if not(x86_64) then
        begin
          write(nrifile,nasm_regname_index[i]);
        end;
    end;
  write(norfile,regcount);
  close(confile);
  closeinc(numfile);
  closeinc(attfile);
  closeinc(stdfile);
  closeinc(intfile);
  if not(x86_64) then
    begin
      closeinc(nasmfile);
    end;
  closeinc(stabfile);
  closeinc(dwrffile);
  closeinc(otfile);
  closeinc(opfile);
  closeinc(norfile);
  closeinc(rnifile);
  closeinc(srifile);
  closeinc(arifile);
  closeinc(irifile);
  if not(x86_64) then
    begin
      closeinc(nrifile);
    end;
  writeln('Done!');
  writeln(regcount,' registers procesed');
end;


begin
   writeln('Register Table Converter Version ',Version);
   x86_64:=paramstr(1)='x86_64';
   if x86_64 then
     fileprefix:='r8664'
   else
     fileprefix:='r386';
   line:=0;
   regcount:=0;
   read_x86reg_file;
   regcount_bsstart:=1;
   while 2*regcount_bsstart<regcount do
     regcount_bsstart:=regcount_bsstart*2;
   build_regnum_index;
   build_int_regname_index;
   if not(x86_64) then
     build_nasm_regname_index;
   build_std_regname_index;
   build_att_regname_index;
   write_inc_files;
end.
