{
    $Id$
    Copyright (c) 1999 by Peter Vreman and Florian Klaempfl

    Convert insns.dat from Nasm to a .inc file for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program nasmconv;

var
   infile,outfile : text;
   s : string;
   i : longint;


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


function formatop(s:string):string;
   const
     replaces=19;
     replacetab : array[1..replaces,1..2] of string[32]=(
       (':',' or ot_colon'),
       ('mem8','mem or ot_bits8'),
       ('mem16','mem or ot_bits16'),
       ('mem32','mem or ot_bits32'),
       ('mem64','mem or ot_bits64'),
       ('mem80','mem or ot_bits80'),
       ('mem','memory'),
       ('memory_offs','mem_offs'),
       ('imm8','imm or ot_bits8'),
       ('imm16','imm or ot_bits16'),
       ('imm32','imm or ot_bits32'),
       ('imm64','imm or ot_bits64'),
       ('imm80','imm or ot_bits80'),
       ('imm','immediate'),
       ('rm8','regmem or ot_bits8'),
       ('rm16','regmem or ot_bits16'),
       ('rm32','regmem or ot_bits32'),
       ('rm64','regmem or ot_bits64'),
       ('rm80','regmem or ot_bits80')
     );
  var
    i : longint;
  begin
    for i:=1to replaces do
     replace(s,replacetab[i,1],replacetab[i,2]);
    formatop:=s;
  end;


procedure maybe_newline;

  begin
     if s[i]=#10 then
       begin
          readln(infile,s);
          i:=1;
       end;
     while s[1]=';' do
       begin
          readln(infile,s);
          i:=1;
       end;
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

var
   hs : string;
   j : longint;
   first : boolean;
   maxinfolen,
   code : byte;
   insns : longint;
   { instruction fields }
   last,
   ops    : longint;
   opcode,
   codes,
   flags   : string;
   optypes : array[1..3] of string;
begin
   writeln('Nasm Instruction Table Converter Version 0.99.11');
   insns:=0;
   assign(infile,'insns.dat');
   reset(infile);
   assign(outfile,'i386tab.inc');
   rewrite(outfile);
   writeln(outfile,'(');
   first:=true;
   while not(eof(infile)) do
     begin
        { handle comment }
        readln(infile,s);
        if s[1]=';' then
          continue;
        { clear }
        opcode:='';
        ops:=0;
        optypes[1]:='';
        optypes[2]:='';
        optypes[3]:='';
        codes:='';
        flags:='';
        { opcode }
        opcode:='A_';
        i:=1;
        while not(s[i] in [' ',#9]) do
          begin
            opcode:=opcode+s[i];
            inc(i);
          end;
        skipspace;
        { ops and optypes }
        repeat
          hs:=readstr;
          if (hs='void') or (hs='ignore') then
            break;
          inc(ops);
          optypes[ops]:=optypes[ops]+'ot_'+formatop(hs);
          if s[i]=':' then
            begin
               inc(i);
               optypes[ops]:=optypes[ops]+' or ot_'+formatop(readstr);
            end;
          while s[i]='|' do
            begin
               inc(i);
               optypes[ops]:=optypes[ops]+' or ot_'+formatop(readstr);
            end;
          if s[i]=',' then
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
          codes:='#0';
        if j>maxinfolen then
         maxinfolen:=j;
        { flags }
        skipspace;
        while not(s[i] in [' ',#9,#13,#10]) and (i<=length(s)) do
          begin
             hs:=readstr;
             if hs='ignore' then
              begin
                flags:='0';
                break;
              end;
             if hs<>'ND' then
              begin
                if flags<>'' then
                 flags:=flags+' or ';
                flags:=flags+'if_'+hs;
              end;
             if (s[i]=',') and (i<=length(s)) then
              inc(i)
             else
              break;
          end;
      { write instruction }
        if not(first) then
          writeln(outfile,',')
        else
          first:=false;
        writeln(outfile,'  (');
        writeln(outfile,'    opcode  : ',opcode,';');
        writeln(outfile,'    ops     : ',ops,';');
        writeln(outfile,'    optypes : (',optypes[1],',',optypes[2],',',optypes[3],');');
        writeln(outfile,'    code    : ',codes,';');
        writeln(outfile,'    flags   : ',flags);
        write(outfile,'  )');
        maybe_newline;
        inc(insns);
     end;
   writeln(outfile);
   writeln(outfile,');');
   close(infile);
   close(outfile);
   writeln(insns,' nodes procesed (maxinfolen=',maxinfolen,')');
end.
{
  $Log$
  Revision 1.2  1999-05-23 18:42:24  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.1  1999/05/12 16:17:10  peter
    * init

  Revision 1.1  1999/05/12 16:08:27  peter
    + moved compiler utils

}
