{
    This file is part of the Free Pascal simulator environment
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit implemements a memory manager for 64 bit processor
    simulations, it works also with TP

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ a simple 64 bit simulator memory manager, also running with TP }
{$N+}
unit mm64;

  interface

    uses
       simbase;

    const
       memoryblocksize = 32768;

    type
       taddr = qword;
       tmemoryblock = array[0..memoryblocksize-1] of byte;
       pmemoryblock = ^tmemoryblock;

       pmemoryarea = ^tmemoryarea;
       tmemoryarea = record
         addr : qword;
         memory : pmemoryblock;
         size : dword;
         next : pmemoryarea;
       end;

       tmemorymanager = object
          mem : pmemoryarea;
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
         stopsim;
      end;

    constructor tmemorymanager.init;

      begin
         mem:=nil;
      end;

    procedure tmemorymanager.allocate(addr : taddr;size : qword);

      var
         ma : pmemoryarea;
         asize : qword;

      begin
         while size>0 do
           begin
              if size>32768 then
                asize:=32768
              else
                asize:=size;
              size:=size-asize;
              new(ma);
              getmem(ma^.memory,trunc(asize));
              fillchar(ma^.memory^,trunc(asize),0);
              ma^.size:=trunc(asize);
              ma^.addr:=addr;
              addr:=addr+asize;

              ma^.next:=mem;
              mem:=ma;
           end;
      end;

    function tmemorymanager.readq(addr : taddr) : qword;

      var
         h : qword;
         ma : pmemoryarea;
         qw : tqwordrec;

      begin
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                   if addr<ma^.addr+ma^.size-7 then
                     begin
                        move(ma^.memory^[trunc(addr-ma^.addr)],h,8);
                        readq:=h;
                        exit;
                     end
                   else
                     begin
                        qw.low32:=readd(addr);
                        qw.high32:=readd(addr+4);
                        readq:=comp(qw);
                        exit;
                     end;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    function tmemorymanager.readalignedq(addr : taddr) : qword;

      var
         h : qword;
         ma : pmemoryarea;
         qw : tqwordrec;

      begin
         if (tqwordrec(addr).low32 and $7)<>0 then
           exception('Alignment violation (dword) to $'+qword2str(addr),instructionpc);
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                    move(ma^.memory^[trunc(addr-ma^.addr)],h,8);
                    readalignedq:=h;
                    exit;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    function tmemorymanager.readd(addr : taddr) : dword;

      var
         h : dword;
         ma : pmemoryarea;

      begin
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                   if addr<ma^.addr+ma^.size-3 then
                     begin
                        move(ma^.memory^[trunc(addr-ma^.addr)],h,4);
                        readd:=h;
                        exit;
                     end
                   else
                     begin
                        readd:=readb(addr)+readb(addr+1) shl 8+readb(addr+2) shl 16+
                          readb(addr+3) shl 24;
                        exit;
                     end;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    function tmemorymanager.readalignedd(addr : taddr) : dword;

      var
         h : dword;
         ma : pmemoryarea;

      begin
         if (tqwordrec(addr).low32 and $3)<>0 then
           exception('Alignment violation (dword) to $'+qword2str(addr),instructionpc);
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                   move(ma^.memory^[trunc(addr-ma^.addr)],h,4);
                   readalignedd:=h;
                   exit;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    function tmemorymanager.readb(addr : taddr) : dword;

      var
         ma : pmemoryarea;

      begin
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                   readb:=ma^.memory^[trunc(addr-ma^.addr)];
                   exit;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    procedure tmemorymanager.writeb(addr : taddr;b : byte);

      var
         ma : pmemoryarea;

      begin
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                begin
                   ma^.memory^[trunc(addr-ma^.addr)]:=b;
                   exit;
                end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

    procedure tmemorymanager.writed(addr : taddr;d : dword);

      begin
         writeb(addr,tdword(d)[0]);
         writeb(addr+1,tdword(d)[1]);
         writeb(addr+2,tdword(d)[2]);
         writeb(addr+3,tdword(d)[3]);
      end;

    procedure tmemorymanager.writealignedd(addr : taddr;d : dword);

      begin
         writeb(addr,tdword(d)[0]);
         writeb(addr+1,tdword(d)[1]);
         writeb(addr+2,tdword(d)[2]);
         writeb(addr+3,tdword(d)[3]);
      end;

    procedure tmemorymanager.writeq(addr : taddr;q : qword);

      var
         ma : pmemoryarea;

      begin
         ma:=mem;
         while assigned(ma) do
           begin
              if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size-7) then
                begin
                   move(q,ma^.memory^[trunc(addr-ma^.addr)],8);
                   exit;
                end
              else
                { misaligned write! }
                if (addr>=ma^.addr) and (addr<ma^.addr+ma^.size) then
                  begin
                     writeln('Not implemented 1!');
                     halt(1);
                  end;
              ma:=ma^.next;
           end;
         exception('Access violation to $'+qword2str(addr),instructionpc);
      end;

end.
