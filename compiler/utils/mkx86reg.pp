{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman and Florian Klaempfl

    Convert i386reg.dat to several .inc files for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program mkx86reg;

const Version = '1.00';
      max_regcount = 128;

var s : string;
    i : longint;
    line : longint;
    regcount:byte;
    regcount_bsstart:byte;
    names,numbers,stdnames,intnames,nasmnames,attnames,stabs,ots,ops:
        array[0..max_regcount-1] of string[63];
    regnumber_index,std_regname_index,int_regname_index,att_regname_index,
    nasm_regname_index:array[0..max_regcount-1] of byte;

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

function tostr(l : longint) : string;

begin
  str(l,tostr);
end;

function readstr : string;

  var
     result : string;

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
        if cpustr<>'64' then
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
    numfile,stabfile,confile,
    rnifile,irifile,srifile,
    arifile,nrifile:text;
    first:boolean;

begin
  { create inc files }
  openinc(confile,'r386con.inc');
  openinc(numfile,'r386num.inc');
  openinc(stdfile,'r386std.inc');
  openinc(attfile,'r386att.inc');
  openinc(intfile,'r386int.inc');
  openinc(nasmfile,'r386nasm.inc');
  openinc(stabfile,'r386stab.inc');
  openinc(otfile,'r386ot.inc');
  openinc(opfile,'r386op.inc');
  openinc(norfile,'r386nor.inc');
  openinc(rnifile,'r386rni.inc');
  openinc(irifile,'r386iri.inc');
  openinc(srifile,'r386sri.inc');
  openinc(arifile,'r386ari.inc');
  openinc(nrifile,'r386nri.inc');
  first:=true;
  for i:=0 to regcount-1 do
    begin
      if not first then
        begin
          writeln(numfile,',');
          writeln(stdfile,',');
          writeln(attfile,',');
          writeln(intfile,',');
          writeln(nasmfile,',');
          writeln(stabfile,',');
          writeln(otfile,',');
          writeln(opfile,',');
          writeln(rnifile,',');
          writeln(irifile,',');
          writeln(srifile,',');
          writeln(arifile,',');
          writeln(nrifile,',');
        end
      else
        first:=false;
      writeln(confile,names[i],' = ',numbers[i],';');
      write(numfile,numbers[i]);
      write(stdfile,'''',stdnames[i],'''');
      write(attfile,'''',attnames[i],'''');
      write(intfile,'''',intnames[i],'''');
      write(nasmfile,'''',nasmnames[i],'''');
      write(stabfile,stabs[i]);
      write(otfile,ots[i]);
      write(opfile,ops[i]);
      write(rnifile,regnumber_index[i]);
      write(irifile,int_regname_index[i]);
      write(srifile,std_regname_index[i]);
      write(arifile,att_regname_index[i]);
      write(nrifile,nasm_regname_index[i]);
    end;
  write(norfile,regcount);
  close(confile);
  closeinc(numfile);
  closeinc(attfile);
  closeinc(stdfile);
  closeinc(intfile);
  closeinc(nasmfile);
  closeinc(stabfile);
  closeinc(otfile);
  closeinc(opfile);
  closeinc(norfile);
  closeinc(rnifile);
  closeinc(irifile);
  closeinc(srifile);
  closeinc(arifile);
  closeinc(nrifile);
  writeln('Done!');
  writeln(regcount,' registers procesed');
end;


begin
   writeln('Register Table Converter Version ',Version);
   line:=0;
   regcount:=0;
   read_x86reg_file;
   regcount_bsstart:=1;
   while 2*regcount_bsstart<regcount do
     regcount_bsstart:=regcount_bsstart*2;
   build_regnum_index;
   build_int_regname_index;
   build_std_regname_index;
   build_att_regname_index;
   build_nasm_regname_index;
   write_inc_files;
end.
{
  $Log$
  Revision 1.2  2003-09-03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.1.2.4  2003/08/31 18:46:57  peter
    * removed warning

  Revision 1.1.2.3  2003/08/29 09:41:25  daniel
    * Further mkx86reg development

  Revision 1.1.2.2  2003/08/27 20:30:46  peter
    * updated for opcode

  Revision 1.1.2.1  2003/08/27 19:13:10  peter
    * new tools

}
