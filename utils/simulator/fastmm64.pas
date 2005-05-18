{
    This file is part of the Free Pascal simulator environment
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit implemements a memory manager for 64 bit processor
    simulations, it needs a 32 bit compiler to be compiled

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$N+}
unit fastmm64;

  interface

    uses
       simbase;

    type
       taddr = qword;

       tmemorymanager = object
          mem : array[0..65535] of pbyte;
          constructor init;
          { "memory" access routines }
          function readalignedq(addr : taddr) : qword;
          function readq(addr : taddr) : qword;
          function readalignedd(addr : taddr) : dword;
          function readd(addr : taddr) : dword;
          function readb(addr : taddr) : dword;
          procedure writeb(addr : taddr;b : byte);
          procedure writealignedd(addr : taddr;d : dword);
          procedure writed(addr : taddr;d : dword);
          procedure writeq(addr : taddr;q : qword);
          procedure allocate(addr : taddr;size : qword);
       end;

    var
       { address of the currently executed instruction, }
       { necessary for correct output of exception      }
       instructionpc : taddr;

  implementation

    procedure exception(const s : string;addr : taddr);

      begin
         writeln;
         writeln('Exception: ',s,' at $',qword2str(addr));
         runerror(255);
         stopsim;
      end;

    constructor tmemorymanager.init;

      begin
         fillchar(mem,sizeof(mem),0);
      end;

    procedure tmemorymanager.allocate(addr : taddr;size : qword);

      procedure allocateblock(addr : taddr);

        var
           upperbits : longint;

        begin
           if (tqwordrec(addr).high32 and $fffffff0)<>0 then
             begin
                writeln('This memory manager supports only 36 bit');
                writeln('Base address was ',qword2str(addr));
                halt(1);
             end;
           upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
           if not(assigned(mem[upperbits])) then
             begin
                getmem(mem[upperbits],1024*1024);
                fillchar(mem[upperbits]^,1024*1024,0);
             end;
        end;

      var
         asize : qword;

      begin
         while size>0 do
           begin
              if size>1024*1024 then
                asize:=1024*1024;
              allocateblock(addr);
              if asize>size then
                break;
              size:=size-asize;
              addr:=addr+asize;
           end;
      end;

    function tmemorymanager.readq(addr : taddr) : qword;

      var
         h : qword;

      begin
         tqwordrec(h).low32:=readd(addr);
         tqwordrec(h).high32:=readd(addr+4);
         readq:=h;
      end;

    function tmemorymanager.readd(addr : taddr) : dword;

      begin
         readd:=readb(addr)+readb(addr+1) shl 8+readb(addr+2) shl 16+
           readb(addr+3) shl 24;
      end;

    function tmemorymanager.readalignedd(addr : taddr) : dword;

      var
         upperbits : longint;

      begin
         if (tqwordrec(addr).low32 and $3)<>0 then
           exception('Alignment violation (dword) to $'+qword2str(addr),instructionpc);
         upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
         if not(assigned(mem[upperbits])) then
           exception('Access violation to $'+qword2str(addr),instructionpc);
         readalignedd:=pdword(mem[upperbits])[(tqwordrec(addr).low32 and $fffff) shr 2];
      end;

    function tmemorymanager.readalignedq(addr : taddr) : qword;

      var
         upperbits : longint;

      begin
         if (tqwordrec(addr).low32 and $7)<>0 then
           exception('Alignment violation (dword) to $'+qword2str(addr),instructionpc);
         upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
         if not(assigned(mem[upperbits])) then
           exception('Access violation to $'+qword2str(addr),instructionpc);
         readalignedq:=pqword(mem[upperbits])[(tqwordrec(addr).low32 and $fffff) shr 3];
      end;

    function tmemorymanager.readb(addr : taddr) : dword;

      var
         upperbits : longint;

      begin
         upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
         if not(assigned(mem[upperbits])) then
           exception('Access violation to $'+qword2str(addr),instructionpc);
         readb:=mem[upperbits,tqwordrec(addr).low32 and $fffff];
      end;

    procedure tmemorymanager.writeb(addr : taddr;b : byte);

      var
         upperbits : longint;

      begin
         upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
         if not(assigned(mem[upperbits])) then
           exception('Access violation to $'+qword2str(addr),instructionpc);
         mem[upperbits,tqwordrec(addr).low32 and $fffff]:=b;
      end;

    procedure tmemorymanager.writealignedd(addr : taddr;d : dword);

      var
         upperbits : longint;

      begin
         if (tqwordrec(addr).low32 and $3)<>0 then
           exception('Alignment violation (dword) to $'+qword2str(addr),instructionpc);
         upperbits:=((tqwordrec(addr).high32 and $f) shl 12) or ((tqwordrec(addr).low32 and $fff) shr 20);
         if not(assigned(mem[upperbits])) then
           exception('Access violation to $'+qword2str(addr),instructionpc);
         pdword(mem[upperbits])[(tqwordrec(addr).low32 and $fffff) shr 2]:=d;
      end;

    procedure tmemorymanager.writed(addr : taddr;d : dword);

      begin
         writeb(addr,tdword(d)[0]);
         writeb(addr+1,tdword(d)[1]);
         writeb(addr+2,tdword(d)[2]);
         writeb(addr+3,tdword(d)[3]);
      end;

    procedure tmemorymanager.writeq(addr : taddr;q : qword);

      begin
         writed(addr,tqwordrec(q).low32);
         writed(addr+4,tqwordrec(q).high32);
      end;

end.
