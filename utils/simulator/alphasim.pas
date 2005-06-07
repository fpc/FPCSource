{
    This file is part of the Free Pascal simulator environment
    Copyright (c) 1999-2000 by Florian Klaempfl

    This file is the main file of the DEC Alpha simulation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$N+}
{ $define DEBUG}
program alphaemu;

  uses
{$ifdef delphi}
     dmisc,
{$else}
     dos,
{$endif}
     simbase,simlib,
{$ifdef FPC}
{$ifdef go32v2}
     dpmiexcp,
{$endif go32v2}
{$endif FPC}
{$ifdef TP}
     mm64
{$else TP}
     {$define fastmem}
     fastmm64
{$endif TP}
     ;

  { elf file types }
  type
     telf64_hdr = packed record
        e_ident : array[0..15] of char;
        e_type : integer;
        e_machine : word;
        version : longint;
        e_entry : qword;
        e_phoff : qword;
        e_shoff : qword;
        e_flags : longint;
        e_ehsize : integer;
        e_phentsize : integer;
        e_phnum : integer;
        e_shentsize : integer;
        e_shnum : integer;
        e_shstrndx : integer;
     end;

     telf64_phdr = packed record
        p_type : longint;
        p_flags : longint;
        { Segment file offset }
        p_offset : qword;
        { Segment virtual address }
        p_vaddr : qword;
        { Segment physical address }
        p_paddr : qword;
        { Segment size in file }
        p_filesz : qword;
        { Segment size in memory }
        p_memsz : qword;
        { Segment alignment, file & memory }
        p_align : qword;
     end;

     telf64_phdr_array = array[0..0] of telf64_phdr;
     pelf64_phdr_array = ^telf64_phdr_array;

  const
{$ifdef fpc}
     { 64kB Stacksize }
     stacksize = 64*1024;
     { stack start at 4 GB }
     stackstart : dword = 1024*1024*1024*4-stacksize;
{$else fpc}
     { 64kB Stacksize }
     stacksize = 64*1024.0;
     { stack start at 4 GB }
     stackstart = 1024.0*1024.0*1024.0*4-stacksize;
{$endif fpc}
  { alpha specific types }
  type
     tintreg = record
        case tindex of
           1 : (all64 : qword);
           2 : (valueq : int64);
           3 : (low32 : dword;high32 : dword);
           4 : (bytes : array[0..7] of byte)
     end;

     tfloatreg = record
        case tindex of
           1 : (valued : double);
           2 : (valueq : qword);
     end;

     tinstruction = dword;

     tintregs = array[0..31] of tintreg;
     tfloatregs = array[0..31] of tfloatreg;

     tstate = object
        r : tintregs;
        f : tfloatregs;
        pc : taddr;
        fpcr : qword;
     end;

  const
     r_v0 = 0;
     r_t0 = 1;

     r_fp = 15;
     r_a0 = 16;
     r_a1 = 17;
     r_a2 = 18;
     r_a3 = 19;
     r_a4 = 20;
     r_a5 = 11;

     r_ra = 26;
     r_at = 28;
     r_gp = 29;
     r_sp = 30;
     r_zero = 31;

     f_zero = 31;

  type
     talphasim = object
        state : tstate;
        memory : tmemorymanager;
        { number of executed instructions }
        instrcount : qword;
        { time when the emulation was started }
        starttime : double;
        { starts the execution at address pc }
        procedure run(pc : taddr);
        { gives a message about an illegal opcode }
        { at the given address                    }
        procedure illegalopcode(addr : taddr);
        { dumps the contens of the register a0 to a[count] }
        procedure dumparguments(count : tindex);
        { dumps the contents of the function result register }
        procedure dumpv0;
        constructor init;
        destructor done;
     end;

  var
     sim : talphasim;

  procedure dump_phdr(const h : telf64_phdr);

    begin
{$ifdef DEBUG}
       writeln('  Type: $',hexstr(h.p_type,8));
       writeln('  Flags: $',hexstr(h.p_flags,8));
       writeln('  Segment file offset: $',qword2str(h.p_offset));
       writeln('  Segment virtual address: $',qword2str(h.p_vaddr));
       writeln('  Segment physical address: $',qword2str(h.p_paddr));
       writeln('  Segment size in file: $',qword2str(h.p_filesz));
       writeln('  Segment size in memory: $',qword2str(h.p_memsz));
       writeln('  Segment alignment, file & memory: $',qword2str(h.p_align));
{$endif DEBUG}
    end;

  procedure _stopsim;{$ifdef TP}far;{$endif TP}

    var
      elapsedtime : double;

    begin
{$ifdef DEBUG}
       elapsedtime:=realtime-sim.starttime;

       write('Executed ',sim.instrcount:0,' instructions in ',
         elapsedtime:0:2,' sec');
       if elapsedtime<>0.0 then
         begin
            writeln(',');
            writeln('equals to ',sim.instrcount/(elapsedtime*1000000.0):0:4,' MIPS');
         end
       else
         writeln;
{$endif DEBUG}
       halt(1);
    end;

  constructor talphasim.init;

    begin
       memory.init;
       { setup dummy registers }
       state.r[31].valueq:=0;
       state.f[31].valued:=0;
       memory.allocate(stackstart,stacksize);
    end;

  procedure talphasim.illegalopcode(addr : taddr);

    var
       instruction : tinstruction;

    begin
       instruction:=memory.readd(addr);
       writeln('Illegal instruction $',hexstr(instruction,8),' at $',qword2str(addr));
       writeln('Opcode is: $',hexstr((instruction and $fc000000) shr 26,2));
       writeln('  Function would be: $',hexstr((instruction and $1fe0) shr 5,3));
       writeln;
       stopsim;
    end;

  procedure talphasim.dumparguments(count : tindex);

    var
       i : tindex;

    begin
       if count>6 then
         begin
            writeln('Illegal number of arguments to print');
            halt(1);
         end;
{$ifdef DEBUG}
       for i:=0 to count-1 do
         writeln('  Register a',i,' = $',qword2str(state.r[r_a0+i].valueq));
{$endif DEBUG}
    end;

  procedure talphasim.dumpv0;

    var
       i : tindex;

    begin
{$ifdef DEBUG}
       writeln('  Register v0 = $',qword2str(state.r[r_v0].valueq));
{$endif DEBUG}
    end;

  procedure talphasim.run(pc : taddr);

    var
       instruction : tinstruction;
       rega,regb,regc : tindex;
       lit : byte;
       va : tintreg;

    function getbranchdisp : int64;

      var
         l : longint;

      begin
         l:=longint(instruction and $1fffff)*4;
         { sign extend }
         if (l and $100000)<>0 then
           l:=l or $fff00000;
         getbranchdisp:=l;
      end;

    procedure instructionignored(const s : string);

      begin
{$ifdef DEBUG}
         writeln('Instruction "',s,'" at $',qword2str(instructionpc),' ignored');
{$endif DEBUG}
      end;

    procedure syscallignored(const s : string);

      begin
{$ifdef DEBUG}
         writeln('SYSCALL "',s,'" at $',qword2str(instructionpc),' ignored');
{$endif DEBUG}
      end;

    procedure syscalldefault(const s : string);

      begin
{$ifdef DEBUG}
         writeln('SYSCALL "',s,'" at $',qword2str(instructionpc),', default value returned');
{$endif DEBUG}
      end;

    var
       i : tindex;
       fs : single;
       ib : byte;
       il : longint;
       fc : comp;
       ic : char;
       valueqa,valueqb : qword;
       oi : oword;
       count : qword;
{$ifdef FASTMEM}
       block : pdword;
       fastpc : longint;
       updatepc : boolean;
{$endif FASTMEM}

    begin
       instrcount:=0;
       state.pc:=pc;
       { setting up the stack pointer }
       state.r[r_sp].valueq:=stackstart+stacksize-24;
       { setting up command line parameters ... }
       state.r[r_a0].valueq:=0;
       state.r[r_a1].valueq:=0;
       { ... and environment }
       state.r[r_a2].valueq:=0;

       starttime:=realtime;
{$ifdef FASTMEM}
       updatepc:=true;
{$endif FASTMEM}
       repeat
         { read the next instruction }
{$ifdef FASTMEM}
         if updatepc then
           begin
              block:=pdword(memory.mem[((tqwordrec(state.pc).high32 and $f) shl 12) or
                ((tqwordrec(state.pc).low32 and $fff) shr 20)]);
              fastpc:=(tqwordrec(state.pc).low32 and $fffff) shr 2;
           end;
         instruction:=block[fastpc];
         inc(fastpc);
         updatepc:=fastpc>1024*256-1;
{$else FASTMEM}
         instruction:=memory.readalignedd(state.pc);
{$endif FASTMEM}
         instructionpc:=state.pc;
         state.pc:=state.pc+4;

         { decode the instruction }
         case (instruction and $fc000000) shr 26 of
            { CALL_PAL }
            $0:
              begin
                 case instruction and $3ffffff of
                    { halt }
                    0:
                       exit;
                    131:
                      begin
                         if state.r[r_v0].high32=0 then
                           case state.r[r_v0].low32 of
                              { Setup }
                              0:
                                begin
                                   syscallignored('setup');
                                   { mimic proper execution }
                                   state.r[r_v0].valueq:=0;
                                end;
                              1:
                                begin
                                   exit;
                                end;
                              4:
                                begin
                                   syscallignored('write');
                                   state.r[r_v0].valueq:=0;
                                   count:=0;
                                   while count<state.r[r_a2].valueq do
                                     begin
                                        byte(ic):=memory.readb(state.r[r_a1].valueq+count);
                                        { all output goes currently to stdout }
                                        if ic=#10 then
                                          writeln(output)
                                        else
                                          write(output,ic);

                                        count:=count+1;
                                        state.r[r_v0].valueq:=state.r[r_v0].valueq+1;
                                     end;
                                end;
                              20:
                                begin
                                   syscalldefault('getpid');
                                   { return a default value }
                                   state.r[r_v0].valueq:=501;
                                end;
                              24:
                                begin
                                   syscalldefault('getuid');
                                   { return a default value }
                                   state.r[r_v0].valueq:=501;
                                end;
                              45:
                                begin
                                   syscallignored('brk');
                                   { mimic proper execution }
                                   state.r[r_v0].valueq:=0;
                                end;
                              { alpha specific }
                              $100:
                                begin
                                   syscallignored('osf_getsysinfo');
                                   { mimic proper execution }
                                   state.r[r_v0].valueq:=0;
                                end;
                              $101:
                                begin
                                   syscallignored('osf_setsysinfo');
                                   { mimic proper execution }
                                   state.r[r_v0].valueq:=0;
                                end;
                              $144:
                                begin
                                   syscallignored('personality');
                                   { mimic proper execution }
                                   state.r[r_v0].valueq:=0;
                                end;
                              else
                                begin
                                   syscallignored('<Unknown>');
                                   dumpv0;
                                   dumparguments(4);
                                end;

                           end
                         else
                           begin
                              syscallignored('<Unknown>');
                              dumpv0;
                              dumparguments(4);
                           end;
                      end;
                    else
                      writeln('PAL code $',hexstr(instruction and $3ffffff,8),' at $',
                        qword2str(instructionpc),' ignored');
                 end;
              end;
            { LDA }
            $8:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 if rega<>r_zero then
                   state.r[rega].valueq:=state.r[regb].valueq+int64(integer(instruction and $ffff));
              end;
            { LDAH }
            $9:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 if rega<>r_zero then
                   state.r[rega].valueq:=state.r[regb].valueq+
                     (int64(integer(instruction and $ffff))*65536);
              end;
            { LDQ_U }
            $B:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 valueqb:=state.r[regb].valueq+
                     (int64(integer(instruction and $ffff)));
                 tqwordrec(valueqb).low32:=tqwordrec(valueqb).low32 and $fffffff8;
                 if rega<>r_zero then
                   state.r[rega].valueq:=memory.readq(valueqb);
              end;
            { STQ_U }
            $f:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 memory.writeq(va.valueq,state.r[rega].valueq);
              end;

            { ************* opcode $10 ************** }
            $10:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 regc:=instruction and $1f;
                 valueqa:=state.r[rega].valueq;
                 if (instruction and $1000)<>0 then
                   valueqb:=(instruction and $1fe000) shr 13
                 else
                   valueqb:=state.r[regb].valueq;
                 case (instruction and $fe0) shr 5 of
                    { ADDL }
                    $0:
                      begin
                         if regc<>r_zero then
                           state.r[regc].low32:=tqwordrec(valueqa).low32+tqwordrec(valueqb).low32;
                      end;
                    { CMPULT }
                    $1D:
                      begin
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=byte(ltu(valueqa,valueqb));
                      end;
                    { ADDQ }
                    $20:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa+valueqb;
                      end;
                    { S4ADDQ }
                    $22:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa*4+valueqb;
                      end;
                    { SUBQ }
                    $29:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa-valueqb;
                      end;
                    { S4SUBQ }
                    $2B:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa*4-valueqb;
                      end;
                    { CMPEQ }
                    $2D:
                      begin
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=byte(valueqa=valueqb);
                      end;
                    { S8ADDQ }
                    $32:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa*8+valueqb;
                      end;
                    { S8SUBQ }
                    $3B:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=valueqa*8-valueqb;
                      end;
                    { CMPULE }
                    $3D:
                      begin
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=byte(leu(valueqa,valueqb));
                      end;
                    { CMPLT }
                    $4D:
                      begin
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=byte(valueqa<valueqb);
                      end;
                    { CMPLE }
                    $6D:
                      begin
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=byte(valueqa<=valueqb);
                      end;
                    else
                      illegalopcode(instructionpc);
                 end;
              end;

            { ************* opcode $11 ************** }
            $11:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 regc:=instruction and $1f;
                 valueqa:=state.r[rega].valueq;
                 if (instruction and $1000)<>0 then
                   valueqb:=(instruction and $1fe000) shr 13
                 else
                   valueqb:=state.r[regb].valueq;
                 case (instruction and $fe0) shr 5 of
                    { AND }
                    $00:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].low32:=tqwordrec(valueqa).low32 and
                                tqwordrec(valueqb).low32;
                              state.r[regc].high32:=tqwordrec(valueqa).high32 and
                                tqwordrec(valueqb).high32;
                           end;
                      end;
                    { BIC }
                    $08:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].low32:=tqwordrec(valueqa).low32 and
                                not(tqwordrec(valueqb).low32);
                              state.r[regc].high32:=tqwordrec(valueqa).high32 and
                                not(tqwordrec(valueqb).high32);
                           end;
                      end;
                    { CMOVLBS }
                    $14:
                      begin
                         if (regc<>r_zero) and ((tqwordrec(valueqa).low32 and 1)<>0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { CMOVLBC }
                    $16:
                      begin
                         if (regc<>r_zero) and ((tqwordrec(valueqa).low32 and 1)=0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { BIS }
                    $20:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].low32:=tqwordrec(valueqa).low32 or
                                tqwordrec(valueqb).low32;
                              state.r[regc].high32:=tqwordrec(valueqa).high32 or
                                tqwordrec(valueqb).high32;
                           end;
                      end;
                    { CMOVEQ }
                    $24:
                      begin
                         if (regc<>r_zero) and (valueqa=0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { CMOVNE }
                    $26:
                      begin
                         if (regc<>r_zero) and (valueqa<>0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { ORNOT }
                    $28:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].low32:=tqwordrec(valueqa).low32 or
                                not(tqwordrec(valueqb).low32);
                              state.r[regc].high32:=tqwordrec(valueqa).high32 or
                                not(tqwordrec(valueqb).high32);
                           end;
                      end;
                    { XOR }
                    $40:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].valueq:=state.r[rega].valueq xor
                                valueqb;
                           end;
                      end;
                    { CMOVLT }
                    $44:
                      begin
                         if (regc<>r_zero) and (valueqa<0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { CMOVGE }
                    $46:
                      begin
                         if (regc<>r_zero) and (valueqa>=0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { EQV }
                    $48:
                      begin
                         if regc<>r_zero then
                           begin
                              state.r[regc].valueq:=valueqa xor
                                not(valueqb);
                           end;
                      end;
                    { CMOVLE }
                    $64:
                      begin
                         if (regc<>r_zero) and (valueqa<=0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    { CMOVGT }
                    $66:
                      begin
                         if (regc<>r_zero) and (valueqa<=0) then
                           state.r[regc].valueq:=valueqb;
                      end;
                    else
                      illegalopcode(instructionpc);
                 end;
              end;

            { ************* opcode $12 ************** }
            $12:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 regc:=instruction and $1f;
                 valueqa:=state.r[rega].valueq;
                 if (instruction and $1000)<>0 then
                   valueqb:=(instruction and $1fe000) shr 13
                 else
                   valueqb:=state.r[regb].valueq;
                 case (instruction and $fe0) shr 5 of
                    { MSKBL }
                    $02:
                      begin
                         { !!!!! no MSB support yet! }
                         il:=1 shl (tqwordrec(valueqb).low32 and $7);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,il and $ff,state.r[regc].valueq);
                      end;
                    { EXTBL }
                    $06:
                      begin
                         { !!!!! no MSB support yet! }
                         shift_right_q(valueqa,(tqwordrec(valueqb).low32 and $7)*8,valueqa);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,$fe,state.r[regc].valueq);
                      end;
                    { INSBL }
                    $0B:
                      begin
                         { !!!!! no MSB support yet! }
                         il:=1 shl (tqwordrec(valueqb).low32 and $7);
                         shift_left_q(valueqa,(tqwordrec(valueqb).low32 and $7)*8,valueqa);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,not(il and $ff),state.r[regc].valueq);
                      end;
                    { MSKWL }
                    $12:
                      begin
                         { !!!!! no MSB support yet! }
                         il:=3 shl (tqwordrec(valueqb).low32 and $7);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,il and $ff,state.r[regc].valueq);
                      end;
                    { EXTWL }
                    $16:
                      begin
                         { !!!!! no MSB support yet! }
                         shift_right_q(valueqa,(tqwordrec(valueqb).low32 and $7)*8,valueqa);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,$fc,state.r[regc].valueq);
                      end;
                    { MSKLL }
                    $22:
                      begin
                         { !!!!! no MSB support yet! }
                         il:=$f shl (tqwordrec(valueqb).low32 and $7);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,il and $ff,state.r[regc].valueq);
                      end;
                    { EXTLL }
                    $26:
                      begin
                         { !!!!! no MSB support yet! }
                         shift_right_q(valueqa,(tqwordrec(valueqb).low32 and $7)*8,valueqa);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,$f0,state.r[regc].valueq);
                      end;
                    { ZAP }
                    $30:
                      begin
                         if regc<>r_zero then
                           byte_zap(valueqa,trunc(valueqb),state.r[regc].valueq);
                      end;
                    { ZAPNOT }
                    $31:
                      begin
                         if regc<>r_zero then
                           byte_zap(valueqa,not(trunc(valueqb)),state.r[regc].valueq);
                      end;
                    { MSKQL }
                    $32:
                      begin
                         { !!!!! no MSB support yet! }
                         il:=$ff shl (tqwordrec(valueqb).low32 and $7);
                         if (regc<>r_zero) then
                           byte_zap(valueqa,il and $ff,state.r[regc].valueq);
                      end;
                    { SRL }
                    $34:
                      begin
                         if regc<>r_zero then
                           state.r[regc].valueq:=state.r[regc].valueq shr (valueqb and $3f);
                      end;
                    { EXTQL }
                    $36:
                      begin
                         { !!!!! no MSB support yet! }
                         shift_right_q(valueqa,(tqwordrec(valueqb).low32 and $7)*8,valueqa);
                         if (regc<>r_zero) then
                           state.r[regc].valueq:=valueqa;
                      end;
                    { SLL }
                    $39:
                      begin
                         if regc<>r_zero then
                           shift_left_q(valueqa,trunc(valueqb) and $3f,state.r[regc].valueq);
                      end
                    else
                      illegalopcode(instructionpc);
                 end;
              end;

            { ************* opcode $13 ************** }
            $13:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 regc:=instruction and $1f;
                 valueqa:=state.r[rega].valueq;
                 if (instruction and $1000)<>0 then
                   valueqb:=(instruction and $1fe000) shr 13
                 else
                   valueqb:=state.r[regb].valueq;
                 case (instruction and $fe0) shr 5 of
                    { UMULH }
                    $30:
                      if regc<>31 then
                        begin
                           mulqword(valueqa,valueqb,oi);
                           state.r[regc].valueq:=towordrec(oi).high64;
                        end;
                    else
                      illegalopcode(instructionpc);
                 end;
              end;

            { ************* opcode $17 ************** }
            $17:
              case (instruction and $ffe0) shr 5 of
                 { MT_FPCR }
                 $24:
                   begin
                      rega:=(instruction and $3e00000) shr 21;
                      state.fpcr:=state.f[rega].valueq;
                   end;
                 { MF_FPCR }
                 $25:
                   begin
                      rega:=(instruction and $3e00000) shr 21;
                      if rega<>f_zero then
                        state.f[rega].valueq:=state.fpcr;
                   end;
                 else
                   illegalopcode(instructionpc);
              end;

            { ************* opcode $18 ************** }
            $18:
              case instruction and $ffff of
                 { EXCB }
                 $400:
                    instructionignored('EXCN');
                 else
                    illegalopcode(instructionpc);
              end;

            { JMP,JSR,RET JSR_COROUTINE }
            $1a:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va:=state.r[regb];
                 va.low32:=va.low32 and $fffffffe;
                 if rega<>31 then
                   state.r[rega].valueq:=state.pc;
                 state.pc:=va.valueq;
{$ifdef FASTMEM}
                 updatepc:=true;
{$endif FASTMEM}
              end;
            { LDS }
            $22:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 if rega<>f_zero then
                   begin
                      { we need to copy the bit pattern! }
                      dword(fs):=memory.readd(va.valueq);
                      state.f[rega].valued:=fs;
                   end;
                 { !!!!!! no translation exceptions! }
              end;
            { LDT }
            $23:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 if rega<>f_zero then
                   state.f[rega].valueq:=memory.readq(va.valueq);
                 { !!!!!! no translation exceptions! }
              end;
{$ifdef dummy}
            { !!!!!!!! STF }
            $24:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 fs:=state.f[rega].valued;
                 memory.writed(va.valueq,longint(fs));
                 { !!!!!! no tranlation exceptions! }
              end;
            { !!!!!!!!!!!! STG }
            $25:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 memory.writeq(va.valueq,state.f[rega].valueq);
                 { !!!!!! no translation exceptions! }
              end;
{$endif dummy}
            { !!!!!!!!!!!!! STS }
            $26:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 fs:=state.f[rega].valued;
                 memory.writed(va.valueq,longint(fs));
                 { !!!!!! no tranlation exceptions! }
              end;
            { STT }
            $27:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 memory.writeq(va.valueq,state.f[rega].valueq);
                 { !!!!!! no translation exceptions! }
              end;
            { LDL }
            $28:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 if rega<>r_zero then
                   state.r[rega].low32:=memory.readalignedd(state.r[regb].valueq+
                     (int64(integer(instruction and $ffff))));
                 { sign extend }
                 if state.r[rega].low32<0 then
                   state.r[rega].high32:=$ffffffff
                 else
                   state.r[rega].high32:=0;
              end;
            { LDQ }
            $29:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 if rega<>r_zero then
                   state.r[rega].valueq:=memory.readalignedq(state.r[regb].valueq+
                     (int64(integer(instruction and $ffff))));
              end;
            { STL }
            $2C:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 memory.writealignedd(va.valueq,state.r[rega].low32);
              end;
            { STQ }
            $2D:
              begin
                 { !!!!! no MSB support yet! }
                 rega:=(instruction and $3e00000) shr 21;
                 regb:=(instruction and $1f0000) shr 16;
                 va.valueq:=state.r[regb].valueq+
                   (int64(integer(instruction and $ffff)));
                 memory.writeq(va.valueq,state.r[rega].valueq);
              end;
            { BR,BSR }
            $30,$34:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 if rega<>31 then
                   state.r[rega].valueq:=state.pc;
                 state.pc:=state.pc+getbranchdisp;
{$ifdef FASTMEM}
                 updatepc:=true;
{$endif FASTMEM}
              end;
            { BLSC }
            $38:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if (state.r[rega].low32 and 1)=0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BEQ }
            $39:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq=0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BLT }
            $3A:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq<0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BLE }
            $3B:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq<=0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BLBS }
            $3C:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if (state.r[rega].low32 and 1)<>0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BNE }
            $3D:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq<>0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BGE }
            $3E:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq>=0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
            { BGT }
            $3F:
              begin
                 rega:=(instruction and $3e00000) shr 21;
                 va.valueq:=state.pc+getbranchdisp;
                 if state.r[rega].valueq>0 then
                   begin
                      state.pc:=va.valueq;
{$ifdef FASTMEM}
                      updatepc:=true;
{$endif FASTMEM}
                   end;
              end;
          else
              illegalopcode(instructionpc);
         end;
         instrcount:=instrcount+1;
       until false;
    end;

  destructor talphasim.done;

    begin
       { deallocate memory }
       { memory.done; }
    end;

  procedure illelfformat;

    begin
       writeln('Illegal format of ELF');
       halt(1);
    end;

  var
     f : file;
     elf64_hdr : telf64_hdr;
     i : tindex;
     j,q : qword;
     b : byte;
     elf64_phdr : pelf64_phdr_array;

  const
     et2str : array[0..6] of string[10] = ('ET_NONE','ET_REL','ET_EXEC',
                                           'ET_DYN','ET_CORE','ET_LOPROC',
                                           'ET_HIPROC');
     em2str : array[0..11] of string[10] = ('EM_NONE','EM_M32','EM_SPARC',
                                            'EM_386','EM_68K','EM_88K',
                                            'EM_486','EM_860','EM_MIPS','',
                                            'EM_MIPS_RS4_BE','EM_SPARC64');

  begin
     if paramcount<>1 then
       begin
          writeln('Usage ALPHAEMU <elf-executable>');
          halt(1);
       end;
{$ifdef DEBUG}
     write('Init... ');
{$endif DEBUG}
     assign(f,paramstr(1));
     {$I-}
     reset(f,1);
     {$I+}
     if ioresult<>0 then
       begin
          writeln;
          writeln('Can''t open input file ',paramstr(1));
          halt(1);
       end;
     blockread(f,elf64_hdr,sizeof(elf64_hdr));
{$ifdef DEBUG}
     writeln('Signature:');
     for i:=0 to 15 do
       write(elf64_hdr.e_ident[i],'(',ord(elf64_hdr.e_ident[i]),') ');
     writeln;
     writeln('ELF type: ',et2str[elf64_hdr.e_type]);
     case elf64_hdr.e_machine of
        0..11:
          writeln('ELF machine: ',em2str[elf64_hdr.e_machine]);
        15:
          writeln('ELF machine: EM_PARISC');
        18:
          writeln('ELF machine: EM_SPARC32PLUS');
        20:
          writeln('ELF machine: EM_PPC');
        $9026:
          writeln('ELF machine: EM_ALPHA');
        else
          illelfformat;
     end;

     writeln('ELF header size: $',hexstr(elf64_hdr.e_ehsize,8));
     writeln('Entry point: $',qword2str(elf64_hdr.e_entry));

     writeln('Program header table file offset: $',qword2str(elf64_hdr.e_phoff));
     writeln('Number of program headers : $',hexstr(elf64_hdr.e_phnum,8));
     writeln('Size of one program header: $',hexstr(elf64_hdr.e_phentsize,8));

     writeln('Section header table file offset: $',qword2str(elf64_hdr.e_shoff));
     { writeln('Section name index: $',hexstr(elf64_hdr.e_shstrndx,8)); }
{$endif}
     if (elf64_hdr.e_ident[0]<>chr(127)) or
       (elf64_hdr.e_ident[1]<>'E') or
       (elf64_hdr.e_ident[2]<>'L') or
       (elf64_hdr.e_ident[3]<>'F') or
       (elf64_hdr.e_type<>2) or
       (elf64_hdr.e_machine<>$9026) then
       illelfformat;

     { load programm headers }
     getmem(elf64_phdr,elf64_hdr.e_phentsize*elf64_hdr.e_phnum);
     seek(f,trunc(elf64_hdr.e_phoff));
     blockread(f,elf64_phdr^,elf64_hdr.e_phentsize*elf64_hdr.e_phnum);
     for i:=0 to elf64_hdr.e_phnum-1 do
       begin
{$ifdef DEBUG}
          writeln('Programm header ',i);
          dump_phdr(elf64_phdr^[i]);
{$endif DEBUG}
       end;
     { ok, now init the emulator }
     sim.init;
     {$ifdef FPC}
     stopsim:=@_stopsim;
     {$else FPC}
     stopsim:=_stopsim;
     {$endif FPC}
{$ifdef DEBUG}
     writeln('OK');
     write('Loading memory... ');
{$endif DEBUG}
     { load memory }
     for i:=0 to elf64_hdr.e_phnum-1 do
       begin
{$ifdef DEBUG}
          write(i+1,' ');
{$endif DEBUG}
          sim.memory.allocate(elf64_phdr^[i].p_vaddr,elf64_phdr^[i].p_memsz);
          seek(f,trunc(elf64_phdr^[i].p_offset));
          j:=0;
          { can we speedup the loading? }
          if (tqwordrec(elf64_phdr^[i].p_filesz).low32 and $7)=0 then
            while j<elf64_phdr^[i].p_filesz do
              begin
                 blockread(f,q,8);
                 sim.memory.writeq(j+elf64_phdr^[i].p_vaddr,q);
                 j:=j+8;
              end
          else
            while j<elf64_phdr^[i].p_filesz do
              begin
                 blockread(f,b,1);
                 sim.memory.writeb(j+elf64_phdr^[i].p_vaddr,b);
                 j:=j+1;
              end;
       end;
     { clean up from the file loading }
     freemem(elf64_phdr,elf64_hdr.e_phentsize*elf64_hdr.e_phnum);
     close(f);
{$ifdef DEBUG}
     writeln('OK');
     writeln('Running program ...');
{$endif DEBUG}
     sim.run(elf64_hdr.e_entry);
{$ifdef DEBUG}
     writeln('Ready');
{$endif DEBUG}
     stopsim;
     sim.done;
  end.
