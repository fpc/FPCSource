{
    Copyright (c) 1998-2002 by Peter Vreman and Florian Klaempfl

    Convert i386ins.dat from Nasm to a .inc file for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program mkx86ins;

const
  Version = '1.5.1';

var
   s : string;
   i : longint;
   x86_64 : boolean;

    function lower(const s : string) : string;
    {
      return lowercased string of s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['A'..'Z'] then
           lower[i]:=char(byte(s[i])+32)
          else
           lower[i]:=s[i];
         lower[0]:=s[0];
      end;

      function Replace(var s:string;const s1,s2:string):boolean;
      var
        i  : longint;
      begin
        i:=pos(s1,s);
        if i>0 then
         begin
           Delete(s,i,length(s1));
           Insert(s2,s,i);
           Replace:=true;
         end
        else
         Replace:=false;
      end;


function formatop(s:string;allowsizeonly:boolean):string;
   const
     replaces=26;
     replacetab : array[1..replaces,1..2] of string[32]=(
       (':',' or ot_colon'),
       ('reg','regnorm'),
       ('regmem','regmem'),
       ('rm8','regmem or ot_bits8'),
       ('rm16','regmem or ot_bits16'),
       ('rm32','regmem or ot_bits32'),
       ('rm64','regmem or ot_bits64'),
       ('rm80','regmem or ot_bits80'),
       ('mem8','memory or ot_bits8'),
       ('mem16','memory or ot_bits16'),
       ('mem32','memory or ot_bits32'),
       ('mem64','memory or ot_bits64'),
       ('mem80','memory or ot_bits80'),
       ('mem','memory'),
       ('memory_offs','mem_offs'),
       ('imm8','immediate or ot_bits8'),
       ('imm16','immediate or ot_bits16'),
       ('imm32','immediate or ot_bits32'),
       ('imm64','immediate or ot_bits64'),
       ('imm80','immediate or ot_bits80'),
       ('imm','immediate'),
       ('8','bits8'),
       ('16','bits16'),
       ('32','bits32'),
       ('64','bits64'),
       ('80','bits80')
     );
  var
    i : longint;
  begin
    for i:=1to replaces do
      begin
        if s=replacetab[i,1] then
          begin
            s:=replacetab[i,2];
            break;
          end;
      end;
    formatop:=s;
  end;


function readnumber : longint;

  var
     base : longint;
     result : longint;

  begin
     result:=0;
     if s[i]='\' then
       begin
          base:=8;
          inc(i);
          if s[i]='x' then
            begin
               base:=16;
               inc(i);
            end;
       end
     else
       base:=10;
     s[i]:=upcase(s[i]);
     while s[i] in ['0'..'9','A'..'F'] do
       begin
          case s[i] of
             '0'..'9':
               result:=result*base+ord(s[i])-ord('0');

             'A'..'F':
               result:=result*base+ord(s[i])-ord('A')+10;
          end;
          inc(i);
       end;
     readnumber:=result;
  end;

function tostr(l : longint) : string;

  var
     hs : string;

  begin
     str(l,hs);
     tostr:=hs;
  end;

function readstr : string;

  var
     result : string;

  begin
     result:='';
     while (s[i] in ['0'..'9','A'..'Z','a'..'z','_']) and (i<=length(s)) do
       begin
          result:=result+s[i];
          inc(i);
       end;
     readstr:=result;
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
  writeln(f,'{ don''t edit, this file is generated from x86ins.dat }');
  writeln(f,'(');
end;


procedure closeinc(var f:text);
begin
  writeln(f);
  writeln(f,');');
  close(f);
end;


var
   attsuffix,
   hs : string;
   j : longint;
   firstopcode,
   first : boolean;
   maxinfolen,
   code : byte;
   insns : longint;
   attsuffile,propfile,opfile,
   nopfile,attfile,intfile,
   infile,insfile : text;
   { instruction fields }
   skip : boolean;
   last,
   ops    : longint;
   intopcode,
   attopcode,
   opcode,
   codes,
   flags   : string;
   optypes : array[1..3] of string;
begin
   writeln('Nasm Instruction Table Converter Version ',Version);
   x86_64:=paramstr(1)='x86_64';
   insns:=0;
   maxinfolen:=0;
   { open dat file }
   assign(infile,'../x86/x86ins.dat');
   if x86_64 then
     begin
       { create inc files }
       openinc(insfile,'x8664tab.inc');
       openinc(opfile,'x8664op.inc');
       assign(nopfile,'x8664nop.inc');
       openinc(attfile,'x8664att.inc');
       openinc(attsuffile,'x8664ats.inc');
       openinc(intfile,'x8664int.inc');
       openinc(propfile,'x8664pro.inc');
     end
   else
     begin
       { create inc files }
       openinc(insfile,'i386tab.inc');
       openinc(opfile,'i386op.inc');
       assign(nopfile,'i386nop.inc');
       openinc(attfile,'i386att.inc');
       openinc(attsuffile,'i386atts.inc');
       openinc(intfile,'i386int.inc');
       openinc(propfile,'i386prop.inc');
     end;
   rewrite(nopfile);
   writeln(nopfile,'{ don''t edit, this file is generated from x86ins.dat }');
   reset(infile);
   first:=true;
   opcode:='';
   firstopcode:=true;
   while not(eof(infile)) do
     begin
        { handle comment }
        readln(infile,s);
        while (s[1]=' ') do
         delete(s,1,1);
        if (s='') or (s[1]=';') then
          continue;
        if (s[1]='[') then
         begin
           i:=pos(',',s);
           j:=pos(']',s);
           if i=0 then
            begin
              opcode:='A_'+Copy(s,2,j-2);
              intopcode:=Copy(s,2,j-2);
              { Conditional }
              if (intopcode[length(intopcode)]='c') and
                 (intopcode[length(intopcode)-1]='c') then
                dec(byte(intopcode[0]),2);
              attopcode:=intopcode;
              attsuffix:='attsufNONE';
            end
           else
            begin
              opcode:='A_'+Copy(s,2,i-2);
              intopcode:=Copy(s,2,i-2);
              { intel conditional }
              if (intopcode[length(intopcode)]='c') and
                 (intopcode[length(intopcode)-1]='c') then
                dec(byte(intopcode[0]),2);
              attopcode:=Copy(s,i+1,j-i-1);
              { att Suffix }
              case attopcode[length(attopcode)] of
                'X' :
                  begin
                    dec(attopcode[0]);
                    attsuffix:='attsufINT';
                  end;
                'F' :
                  begin
                    dec(attopcode[0]);
                    attsuffix:='attsufFPU';
                  end;
                'R' :
                  begin
                    dec(attopcode[0]);
                    attsuffix:='attsufFPUint';
                  end;
                else
                  attsuffix:='attsufNONE';
              end;
              { att Conditional }
              if (attopcode[length(attopcode)]='C') and
                 (attopcode[length(attopcode)-1]='C') then
                dec(byte(attopcode[0]),2);
            end;
           intopcode:=Lower(intopcode);
           attopcode:=Lower(attopcode);
           if firstopcode then
            firstopcode:=false
           else
            begin
              writeln(opfile,',');
              writeln(attfile,',');
              writeln(attsuffile,',');
              writeln(intfile,',');
              writeln(propfile,',');
            end;
           write(opfile,opcode);
           write(intfile,'''',intopcode,'''');
           write(attfile,'''',attopcode,'''');
           write(attsuffile,attsuffix);
           { read the next line which contains the Change options }
           repeat
             readln(infile,s);
           until eof(infile) or ((s<>'') and (s[1]<>';'));
           write(propfile,'(Ch: ',s,')');
           continue;
         end;
        { we must have an opcode }
        if opcode='' then
         runerror(234);
        { clear }
        ops:=0;
        optypes[1]:='';
        optypes[2]:='';
        optypes[3]:='';
        codes:='';
        flags:='';
        skip:=false;
        { ops and optypes }
        i:=1;
        repeat
          hs:=readstr;
          if (hs='void') or (hs='ignore') then
            break;
          inc(ops);
          optypes[ops]:=optypes[ops]+'ot_'+formatop(hs,false);
          while s[i]='|' do
            begin
               inc(i);
               optypes[ops]:=optypes[ops]+' or ot_'+formatop(readstr,true);
            end;
          if s[i] in [',',':'] then
            inc(i)
          else
            break;
        until false;
        for j:=1 to 3-ops do
          optypes[3-j+1]:='ot_none';
        { codes }
        skipspace;
        j:=0;
        last:=0;
        if s[i] in ['\','0'..'9'] then
          begin
             while not(s[i] in [' ',#9]) do
               begin
                 code:=readnumber;
                 { for some codes we want also to change the optypes, but not
                   if the last byte was a 1 then this byte belongs to a direct
                   copy }
                 if last<>1 then
                  begin
                    case code of
                      12,13,14 :
                        optypes[code-11]:=optypes[code-11]+' or ot_signed';
                    end;
                  end;
                 codes:=codes+'#'+tostr(code);
                 last:=code;
                 inc(j);
               end;
          end
        else
          begin
            readstr;
            codes:='#0';
          end;
        if j>maxinfolen then
         maxinfolen:=j;
        { flags }
        skipspace;
        while not(s[i] in [' ',#9,#13,#10]) and (i<=length(s)) do
          begin
             hs:=readstr;
             if x86_64 then
               begin
                 if (upcase(hs)='NOX86_64') then
                   skip:=true;
               end
             else
               begin
                 if (upcase(hs)='X86_64') then
                   skip:=true;
               end;
             if hs<>'ND' then
              begin
                if flags<>'' then
                 flags:=flags+' or ';
                flags:=flags+'if_'+lower(hs);
              end;
             if (s[i]=',') and (i<=length(s)) then
              inc(i)
             else
              break;
          end;
        { write instruction }
        if not skip then
          begin
            if not(first) then
              writeln(insfile,',')
            else
              first:=false;
            writeln(insfile,'  (');
            writeln(insfile,'    opcode  : ',opcode,';');
            writeln(insfile,'    ops     : ',ops,';');
            writeln(insfile,'    optypes : (',optypes[1],',',optypes[2],',',optypes[3],');');
            writeln(insfile,'    code    : ',codes,';');
            writeln(insfile,'    flags   : ',flags);
            write(insfile,'  )');
            inc(insns);
          end;
     end;
   close(infile);
   closeinc(insfile);
   closeinc(intfile);
   closeinc(attfile);
   closeinc(attsuffile);
   closeinc(opfile);
   writeln(nopfile,insns,';');
   close(nopfile);
   closeinc(propfile);
   writeln(insns,' nodes procesed (maxinfolen=',maxinfolen,')');
end.
