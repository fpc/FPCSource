{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Pierre Muller

    Register debug routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPRegs;
{$ifdef NODEBUG}
interface
implementation
end.
{$else NODEBUG}
interface
uses
{$ifdef Windows}
  Windows,
{$endif Windows}
  Objects,Dialogs,Drivers,Views,
  FPViews;


  const
    MaxRegs = 128;

  type

{$undef cpu_known}

    TIntRegs = record
{$ifndef test_generic_cpu}
{$ifdef i386}
{$define cpu_known}
       eax,ebx,ecx,edx,eip,esi,edi,esp,ebp : dword;
       cs,ds,es,ss,fs,gs : word;
       eflags : dword;
{$endif i386}
{$ifdef x86_64}
{$define cpu_known}
       rax,rbx,rcx,rdx,rsi,rdi,rbp,rsp,
       r8,r9,r10,r11,r12,r13,r14,r15,
       rip : qword;
       cs,ds,es,ss,fs,gs : word;
       eflags : dword;
{$endif x86_64}
{$ifdef m68k}
{$define cpu_known}
       d0,d1,d2,d3,d4,d5,d6,d7 : dword;
       a0,a1,a2,a3,a4,a5,fp,sp : dword;
       ps,pc : dword;
{$endif m68k}
{$ifdef powerpc}
{$define cpu_known}
       r : array [0..31] of dword;
       pc,ps,cr,lr,ctr,xer : dword;
{$endif powerpc}
{$ifdef sparc}
{$define cpu_known}
       o : array [0..7] of dword;
       i : array [0..7] of dword;
       l : array [0..7] of dword;
       g : array [0..7] of dword;
       y,psr,wim,tbr,pc,npc,fsr,csr : dword;
{$endif sparc}
{$endif not test_generic_cpu}
{$ifndef cpu_known}
       reg : array [0..MaxRegs-1] of string;
{$endif not cpu_known}
    end;

    PRegistersView = ^TRegistersView;
    TRegistersView = object(TView)
      NewReg,OldReg : TIntRegs;
      InDraw : boolean;
      GDBCount : longint;
      first : boolean;
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PRegistersWindow = ^TRegistersWindow;
    TRegistersWindow = Object(TFPDlgWindow)
      RV : PRegistersView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    TFPURegs = record
{$ifndef test_generic_cpu}
{$if defined(i386) or defined(x86_64)}
      st0,st1,st2,st3,st4,st5,st6,st7 :string;
      ftag,fop,fctrl,fstat,fiseg,foseg : word;
      fioff,fooff : cardinal;
{$endif i386 or x86_64}
{$ifdef m68k}
      fp0,fp1,fp2,fp3,fp4,fp5,fp6,fp7 : string;
      fpcontrol,fpstatus,fpiaddr : dword;
{$endif m68k}
{$ifdef powerpc}
       f : array [0..31] of string;
{$endif powerpc}
{$ifdef sparc}
       f : array [0..31] of string;
{$endif sparc}
{$endif not test_generic_cpu}
{$ifndef cpu_known}
       freg : array [0..MaxRegs-1] of string;
{$endif not cpu_known}
    end;

    PFPUView = ^TFPUView;
    TFPUView = object(TView)
      NewReg,OldReg : TFPURegs;
      InDraw : boolean;
      GDBCount : longint;
{$ifndef cpu_known}
      UseInfoFloat : boolean;
{$endif not cpu_known}
      first : boolean;
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PFPUWindow = ^TFPUWindow;
    TFPUWindow = Object(TFPDlgWindow)
      RV : PFPUView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;

    tssereg = record
      case byte of
        1 : (bytearray : array[0..15] of byte);
        2 : (wordarray : array[0..7] of word);
        3 : (dwordarray : array[0..3] of dword);
        4 : (qwordarray : array[0..1] of qword);
        5 : (twordfield : array[0..1] of qword);
        6 : (singlearray : array[0..3] of single);
        7 : (doublearray : array[0..1] of double);
    end;

    tmmxreg = record
      case byte of
        1 : (bytearray : array[0..7] of byte);
        2 : (wordarray : array[0..3] of word);
        3 : (dwordarray : array[0..1] of dword);
        4 : (qwordfield : qword);
        6 : (singlearray : array[0..1] of single);
    end;

    TVectorRegs = record
{$ifndef test_generic_cpu}
{$if defined(i386) or defined(x86_64)}
      xmm : array[0..7] of string;
      mmx : array[0..7] of string;
      mxcsr : string;
{$endif i386 or x86_64}
{$ifdef powerpc}
      m : array[0..31] of string;
{$endif powerpc}
{$endif not test_generic_cpu}
{$ifndef cpu_known}
       vreg : array [0..MaxRegs-1] of string;
{$endif not cpu_known}
    end;


    PVectorView = ^TVectorView;
    TVectorView = object(TView)
      NewReg,OldReg : TVectorRegs;
      InDraw : boolean;
      GDBCount : longint;
{$ifndef cpu_known}
      UseInfoVector : boolean;
{$endif not cpu_known}
      first : boolean;
      constructor Init(var Bounds: TRect);
      procedure   Draw;virtual;
      destructor  Done; virtual;
    end;

    PVectorWindow = ^TVectorWindow;
    TVectorWindow = Object(TFPDlgWindow)
      RV : PVectorView;
      Constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      destructor  Done; virtual;
    end;


  procedure InitRegistersWindow;
  procedure DoneRegistersWindow;
  procedure InitFPUWindow;
  procedure DoneFPUWindow;
  procedure InitVectorWindow;
  procedure DoneVectorWindow;

  procedure RegisterFPRegsViews;


implementation

uses
  Strings,
{$ifndef NODEBUG}
  {$ifdef GDBMI}
    GDBMICon, GDBMIInt,
  {$else GDBMI}
    GDBCon,GDBInt,
  {$endif GDBMI}
{$endif NODEBUG}
  App,Menus,
  WViews,WEditor,
  wutils,
  FPConst,FPVars,
  FPDebug;


Const
  RRegistersWindow: TStreamRec = (
     ObjType: 1711;
     VmtLink: Ofs(TypeOf(TRegistersWindow)^);
     Load:    @TRegistersWindow.Load;
     Store:   @TRegistersWindow.Store
  );

  RRegistersView: TStreamRec = (
     ObjType: 1712;
     VmtLink: Ofs(TypeOf(TRegistersView)^);
     Load:    @TRegistersView.Load;
     Store:   @TRegistersView.Store
  );

  RFPUWindow: TStreamRec = (
     ObjType: 1713;
     VmtLink: Ofs(TypeOf(TFPUWindow)^);
     Load:    @TFPUWindow.Load;
     Store:   @TFPUWindow.Store
  );

  RFPUView: TStreamRec = (
     ObjType: 1714;
     VmtLink: Ofs(TypeOf(TFPUView)^);
     Load:    @TFPUView.Load;
     Store:   @TFPUView.Store
  );

  RVectorView: TStreamRec = (
     ObjType: 1715;
     VmtLink: Ofs(TypeOf(TVectorView)^);
     Load:    @TVectorView.Load;
     Store:   @TVectorView.Store
  );

{$ifdef useresstrings}
resourcestring
{$else}
const
{$endif}
      dialog_registers = 'Register View';
      dialog_fpu = 'FPU View';
      dialog_vector = 'Vector Unit View';

{****************************************************************************
                         TRegistersView
****************************************************************************}

  function GetIntRegs(var rs : TIntRegs) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
       buffer : array[0..255] of char;
       i : byte;

    begin
       GetIntRegs:=false;
{$ifndef NODEBUG}
{$ifdef cpu_known}
{$ifdef i386}
       GetIntRegs :=
         Debugger^.GetIntRegister('eax', rs.eax) and
         Debugger^.GetIntRegister('ebx', rs.ebx) and
         Debugger^.GetIntRegister('ecx', rs.ecx) and
         Debugger^.GetIntRegister('edx', rs.edx) and
         Debugger^.GetIntRegister('esi', rs.esi) and
         Debugger^.GetIntRegister('edi', rs.edi) and
         Debugger^.GetIntRegister('ebp', rs.ebp) and
         Debugger^.GetIntRegister('esp', rs.esp) and
         Debugger^.GetIntRegister('eip', rs.eip) and
       { under Windows flags are on a register named ps !! PM }
         (Debugger^.GetIntRegister('eflags', rs.eflags) or Debugger^.GetIntRegister('ps', rs.eflags)) and
         Debugger^.GetIntRegister('cs', rs.cs) and
         Debugger^.GetIntRegister('ds', rs.ds) and
         Debugger^.GetIntRegister('es', rs.es) and
         Debugger^.GetIntRegister('fs', rs.fs) and
         Debugger^.GetIntRegister('gs', rs.gs) and
         Debugger^.GetIntRegister('ss', rs.ss);
{$endif i386}
{$ifdef x86_64}
       GetIntRegs :=
         Debugger^.GetIntRegister('rax', rs.rax) and
         Debugger^.GetIntRegister('rbx', rs.rbx) and
         Debugger^.GetIntRegister('rcx', rs.rcx) and
         Debugger^.GetIntRegister('rdx', rs.rdx) and
         Debugger^.GetIntRegister('rsi', rs.rsi) and
         Debugger^.GetIntRegister('rdi', rs.rdi) and
         Debugger^.GetIntRegister('rbp', rs.rbp) and
         Debugger^.GetIntRegister('rsp', rs.rsp) and
         Debugger^.GetIntRegister('r8', rs.r8) and
         Debugger^.GetIntRegister('r9', rs.r9) and
         Debugger^.GetIntRegister('r10', rs.r10) and
         Debugger^.GetIntRegister('r11', rs.r11) and
         Debugger^.GetIntRegister('r12', rs.r12) and
         Debugger^.GetIntRegister('r13', rs.r13) and
         Debugger^.GetIntRegister('r14', rs.r14) and
         Debugger^.GetIntRegister('r15', rs.r15) and
         Debugger^.GetIntRegister('rip', rs.rip) and
       { under Windows flags are on a register named ps !! PM }
         (Debugger^.GetIntRegister('eflags', rs.eflags) or Debugger^.GetIntRegister('ps', rs.eflags)) and
         Debugger^.GetIntRegister('cs', rs.cs) and
         Debugger^.GetIntRegister('ds', rs.ds) and
         Debugger^.GetIntRegister('es', rs.es) and
         Debugger^.GetIntRegister('fs', rs.fs) and
         Debugger^.GetIntRegister('gs', rs.gs) and
         Debugger^.GetIntRegister('ss', rs.ss);
{$endif x86_64}
{$ifdef m68k}
       GetIntRegs :=
         Debugger^.GetIntRegister('d0', rs.d0) and
         Debugger^.GetIntRegister('d1', rs.d1) and
         Debugger^.GetIntRegister('d2', rs.d2) and
         Debugger^.GetIntRegister('d3', rs.d3) and
         Debugger^.GetIntRegister('d4', rs.d4) and
         Debugger^.GetIntRegister('d5', rs.d5) and
         Debugger^.GetIntRegister('d6', rs.d6) and
         Debugger^.GetIntRegister('d7', rs.d7) and
         Debugger^.GetIntRegister('a0', rs.a0) and
         Debugger^.GetIntRegister('a1', rs.a1) and
         Debugger^.GetIntRegister('a2', rs.a2) and
         Debugger^.GetIntRegister('a3', rs.a3) and
         Debugger^.GetIntRegister('a4', rs.a4) and
         Debugger^.GetIntRegister('a5', rs.a5) and
         Debugger^.GetIntRegister('fp', rs.fp) and
         Debugger^.GetIntRegister('sp', rs.sp) and
         Debugger^.GetIntRegister('ps', rs.ps) and
         Debugger^.GetIntRegister('pc', rs.pc);
{$endif m68k}
{$ifdef powerpc}
       GetIntRegs := true;
       for i:=0 to 31 do
         GetIntRegs := GetIntRegs and Debugger^.GetIntRegister('r'+inttostr(i), rs.r[i]);
       { other regs
         pc,ps,cr,lr,ctr,xer : dword; }
       GetIntRegs := GetIntRegs and
         Debugger^.GetIntRegister('pc', rs.pc) and
         Debugger^.GetIntRegister('ps', rs.ps) and
         Debugger^.GetIntRegister('lr', rs.lr) and
         Debugger^.GetIntRegister('ctr', rs.ctr) and
         Debugger^.GetIntRegister('xer', rs.xer);
{$endif powerpc}
{$ifdef sparc}
       GetIntRegs := true;
       for i:=0 to 7 do
         GetIntRegs := GetIntRegs and Debugger^.GetIntRegister('o'+inttostr(i), rs.o[i]);
       for i:=0 to 7 do
         if i = 6 then
           GetIntRegs := GetIntRegs and (Debugger^.GetIntRegister('i6', rs.i[6]) or Debugger^.GetIntRegister('fp', rs.i[6]))
         else
           GetIntRegs := GetIntRegs and Debugger^.GetIntRegister('i'+inttostr(i), rs.i[i]);
       for i:=0 to 7 do
         GetIntRegs := GetIntRegs and Debugger^.GetIntRegister('l'+inttostr(i), rs.l[i]);
       for i:=0 to 7 do
         GetIntRegs := GetIntRegs and Debugger^.GetIntRegister('g'+inttostr(i), rs.g[i]);

       GetIntRegs := GetIntRegs and
         Debugger^.GetIntRegister('y', rs.y) and
         Debugger^.GetIntRegister('psr', rs.psr) and
         Debugger^.GetIntRegister('wim', rs.wim) and
         Debugger^.GetIntRegister('tbs', rs.tbr) and
         Debugger^.GetIntRegister('pc', rs.pc) and
         Debugger^.GetIntRegister('npc', rs.npc) and
         Debugger^.GetIntRegister('fsr', rs.fsr) and
         Debugger^.GetIntRegister('csr', rs.csr);
{$endif sparc}
{$else cpu_known}
       Debugger^.Command('info registers');
       if Debugger^.Error then
         exit
       else
         begin
            i:=0;
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      p1:=strscan(p,#10);
                      if assigned(p1) then
                        begin
                          strlcopy(buffer,p,p1-p);
                          rs.reg[i]:=ExtractTabs(strpas(buffer),8);
                          if i<MaxRegs-1 then
                            inc(i);
                        end;
                      p:=strscan(p1,#10);
                      if assigned(p) then
                        begin
                           p1:=strscan(p,' ');
                           inc(p);
                        end
                      else
                        break;
                   end;
                 { free allocated memory }
                 strdispose(po);
              end
            else
              exit;
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
       GetIntRegs:=true;
{$endif cpu_known}
{$endif not NODEBUG}
    end;

  constructor TRegistersView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
       InDraw:=false;
       first:=true;
       FillChar(OldReg,Sizeof(OldReg),#0);
       FillChar(NewReg,Sizeof(NewReg),#0);
       GrowMode:=gfGrowHiX or GfGrowHiY;
       GDBCount:=-1;
    end;

  procedure TRegistersView.Draw;

    var
       rs : tintregs;
       OK : boolean;
       color :byte;
       i : byte;

    procedure SetColor(x,y : longint);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    procedure SetColor(x,y : qword);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    procedure SetStrColor(const x,y : string);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
{$ifdef NODEBUG}
       WriteStr(1,0,'<no values available>',7);
{$else NODEBUG}
       If not assigned(Debugger) then
         begin
            WriteStr(1,0,'<no values available>',7);
            exit;
         end;
       if InDraw then exit;
       InDraw:=true;
       if GDBCount<>Debugger^.RunCount then
         begin
           OldReg:=NewReg;
           OK:=GetIntRegs(rs);
           NewReg:=rs;
           { get inital values }
           if first then
             begin
               OldReg:=NewReg;
               first:=false;
             end;
           GDBCount:=Debugger^.RunCount;
         end
       else
         begin
           rs:=NewReg;
           OK:=true;
         end;
       if  OK then
         begin
{$ifdef cpu_known}
{$ifdef i386}
            SetColor(rs.eax,OldReg.eax);
            WriteStr(1,0,'EAX '+HexStr(longint(rs.eax),8),color);
            SetColor(rs.ebx,OldReg.ebx);
            WriteStr(1,1,'EBX '+HexStr(longint(rs.ebx),8),color);
            SetColor(rs.ecx,OldReg.ecx);
            WriteStr(1,2,'ECX '+HexStr(longint(rs.ecx),8),color);
            SetColor(rs.edx,OldReg.edx);
            WriteStr(1,3,'EDX '+HexStr(longint(rs.edx),8),color);
            SetColor(rs.eip,OldReg.eip);
            WriteStr(1,4,'EIP '+HexStr(longint(rs.eip),8),color);
            SetColor(rs.esi,OldReg.esi);
            WriteStr(1,5,'ESI '+HexStr(longint(rs.esi),8),color);
            SetColor(rs.edi,OldReg.edi);
            WriteStr(1,6,'EDI '+HexStr(longint(rs.edi),8),color);
            SetColor(rs.esp,OldReg.esp);
            WriteStr(1,7,'ESP '+HexStr(longint(rs.esp),8),color);
            SetColor(rs.ebp,OldReg.ebp);
            WriteStr(1,8,'EBP '+HexStr(longint(rs.ebp),8),color);
            SetColor(rs.cs,OldReg.cs);
            WriteStr(14,0,'CS '+HexStr(rs.cs,4),color);
            SetColor(rs.ds,OldReg.ds);
            WriteStr(14,1,'DS '+HexStr(rs.ds,4),color);
            SetColor(rs.es,OldReg.es);
            WriteStr(14,2,'ES '+HexStr(rs.es,4),color);
            SetColor(rs.fs,OldReg.fs);
            WriteStr(14,3,'FS '+HexStr(rs.fs,4),color);
            SetColor(rs.gs,OldReg.gs);
            WriteStr(14,4,'GS '+HexStr(rs.gs,4),color);
            SetColor(rs.ss,OldReg.ss);
            WriteStr(14,5,'SS '+HexStr(rs.ss,4),color);
            SetColor(rs.eflags and $1,OldReg.eflags and $1);
            WriteStr(22,0,'c='+chr(byte((rs.eflags and $1)<>0)+48),color);
            SetColor(rs.eflags and $20,OldReg.eflags and $20);
            WriteStr(22,1,'z='+chr(byte((rs.eflags and $20)<>0)+48),color);
            SetColor(rs.eflags and $80,OldReg.eflags and $80);
            WriteStr(22,2,'s='+chr(byte((rs.eflags and $80)<>0)+48),color);
            SetColor(rs.eflags and $800,OldReg.eflags and $800);
            WriteStr(22,3,'o='+chr(byte((rs.eflags and $800)<>0)+48),color);
            SetColor(rs.eflags and $4,OldReg.eflags and $4);
            WriteStr(22,4,'p='+chr(byte((rs.eflags and $4)<>0)+48),color);
            SetColor(rs.eflags and $200,OldReg.eflags and $200);
            WriteStr(22,5,'i='+chr(byte((rs.eflags and $200)<>0)+48),color);
            SetColor(rs.eflags and $10,OldReg.eflags and $10);
            WriteStr(22,6,'a='+chr(byte((rs.eflags and $10)<>0)+48),color);
            SetColor(rs.eflags and $400,OldReg.eflags and $400);
            WriteStr(22,7,'d='+chr(byte((rs.eflags and $400)<>0)+48),color);
{$endif i386}
{$ifdef x86_64}
            SetColor(rs.rax,OldReg.rax);
            WriteStr(1,0,'RAX '+HexStr(rs.rax,16),color);
            SetColor(rs.rbx,OldReg.rbx);
            WriteStr(1,1,'RBX '+HexStr(rs.rbx,16),color);
            SetColor(rs.rcx,OldReg.rcx);
            WriteStr(1,2,'RCX '+HexStr(rs.rcx,16),color);
            SetColor(rs.rdx,OldReg.rdx);
            WriteStr(1,3,'RDX '+HexStr(rs.rdx,16),color);
            SetColor(rs.rsi,OldReg.rsi);
            WriteStr(1,4,'RSI '+HexStr(rs.rsi,16),color);
            SetColor(rs.rdi,OldReg.rdi);
            WriteStr(1,5,'RDI '+HexStr(rs.rdi,16),color);
            SetColor(rs.rbp,OldReg.rbp);
            WriteStr(1,6,'RBP '+HexStr(rs.rbp,16),color);
            SetColor(rs.rsp,OldReg.rsp);
            WriteStr(1,7,'RSP '+HexStr(rs.rsp,16),color);
            SetColor(rs.r8,OldReg.r8);
            WriteStr(1,8,'R8  '+HexStr(rs.r8,16),color);
            SetColor(rs.r9,OldReg.r9);
            WriteStr(1,9,'R9  '+HexStr(rs.r9,16),color);
            SetColor(rs.r10,OldReg.r10);
            WriteStr(1,10,'R10 '+HexStr(rs.r10,16),color);
            SetColor(rs.r11,OldReg.r11);
            WriteStr(1,11,'R11 '+HexStr(rs.r11,16),color);
            SetColor(rs.r12,OldReg.r12);
            WriteStr(1,12,'R12 '+HexStr(rs.r12,16),color);
            SetColor(rs.r13,OldReg.r13);
            WriteStr(1,13,'R13 '+HexStr(rs.r13,16),color);
            SetColor(rs.r14,OldReg.r14);
            WriteStr(1,14,'R14 '+HexStr(rs.r14,16),color);
            SetColor(rs.r15,OldReg.r15);
            WriteStr(1,15,'R15 '+HexStr(rs.r15,16),color);
            SetColor(rs.rip,OldReg.rip);
            WriteStr(1,16,'RIP '+HexStr(rs.rip,16),color);
            SetColor(rs.cs,OldReg.cs);
            WriteStr(22,11,'CS '+HexStr(rs.cs,4),color);
            SetColor(rs.ds,OldReg.ds);
            WriteStr(22,12,'DS '+HexStr(rs.ds,4),color);
            SetColor(rs.es,OldReg.es);
            WriteStr(22,13,'ES '+HexStr(rs.es,4),color);
            SetColor(rs.fs,OldReg.fs);
            WriteStr(22,14,'FS '+HexStr(rs.fs,4),color);
            SetColor(rs.gs,OldReg.gs);
            WriteStr(22,15,'GS '+HexStr(rs.gs,4),color);
            SetColor(rs.ss,OldReg.ss);
            WriteStr(22,16,'SS '+HexStr(rs.ss,4),color);
            SetColor(rs.eflags and $1,OldReg.eflags and $1);
            WriteStr(24,0,'c='+chr(byte((rs.eflags and $1)<>0)+48),color);
            SetColor(rs.eflags and $20,OldReg.eflags and $20);
            WriteStr(24,1,'z='+chr(byte((rs.eflags and $20)<>0)+48),color);
            SetColor(rs.eflags and $80,OldReg.eflags and $80);
            WriteStr(24,2,'s='+chr(byte((rs.eflags and $80)<>0)+48),color);
            SetColor(rs.eflags and $800,OldReg.eflags and $800);
            WriteStr(24,3,'o='+chr(byte((rs.eflags and $800)<>0)+48),color);
            SetColor(rs.eflags and $4,OldReg.eflags and $4);
            WriteStr(24,4,'p='+chr(byte((rs.eflags and $4)<>0)+48),color);
            SetColor(rs.eflags and $200,OldReg.eflags and $200);
            WriteStr(24,5,'i='+chr(byte((rs.eflags and $200)<>0)+48),color);
            SetColor(rs.eflags and $10,OldReg.eflags and $10);
            WriteStr(24,6,'a='+chr(byte((rs.eflags and $10)<>0)+48),color);
            SetColor(rs.eflags and $400,OldReg.eflags and $400);
            WriteStr(24,7,'d='+chr(byte((rs.eflags and $400)<>0)+48),color);
{$endif x86_64}
{$ifdef m68k}
            SetColor(rs.d0,OldReg.d0);
            WriteStr(1,0,'d0 '+HexStr(longint(rs.d0),8),color);
            SetColor(rs.d1,OldReg.d1);
            WriteStr(1,1,'d1 '+HexStr(longint(rs.d1),8),color);
            SetColor(rs.d2,OldReg.d2);
            WriteStr(1,2,'d2 '+HexStr(longint(rs.d2),8),color);
            SetColor(rs.d3,OldReg.d3);
            WriteStr(1,3,'d3 '+HexStr(longint(rs.d3),8),color);
            SetColor(rs.d4,OldReg.d4);
            WriteStr(1,4,'d4 '+HexStr(longint(rs.d4),8),color);
            SetColor(rs.d5,OldReg.d5);
            WriteStr(1,5,'d5 '+HexStr(longint(rs.d5),8),color);
            SetColor(rs.d6,OldReg.d6);
            WriteStr(1,6,'d6 '+HexStr(longint(rs.d6),8),color);
            SetColor(rs.d7,OldReg.d7);
            WriteStr(1,7,'d7 '+HexStr(longint(rs.d7),8),color);
            SetColor(rs.a0,OldReg.a0);
            WriteStr(14,0,'a0 '+HexStr(longint(rs.a0),8),color);
            SetColor(rs.a1,OldReg.a1);
            WriteStr(14,1,'a1 '+HexStr(longint(rs.a1),8),color);
            SetColor(rs.a2,OldReg.a2);
            WriteStr(14,2,'a2 '+HexStr(longint(rs.a2),8),color);
            SetColor(rs.a3,OldReg.a3);
            WriteStr(14,3,'a3 '+HexStr(longint(rs.a3),8),color);
            SetColor(rs.a4,OldReg.a4);
            WriteStr(14,4,'a4 '+HexStr(longint(rs.a4),8),color);
            SetColor(rs.a5,OldReg.a5);
            WriteStr(14,5,'a5 '+HexStr(longint(rs.a5),8),color);
            SetColor(rs.fp,OldReg.fp);
            WriteStr(14,6,'fp '+HexStr(longint(rs.fp),8),color);
            SetColor(rs.sp,OldReg.sp);
            WriteStr(14,7,'sp '+HexStr(longint(rs.sp),8),color);
            SetColor(rs.pc,OldReg.pc);
            WriteStr(1,8,'pc '+HexStr(longint(rs.pc),8),color);
            SetColor(rs.ps and $1,OldReg.ps and $1);
            WriteStr(22,8,' c'+chr(byte((rs.ps and $1)<>0)+48),color);
            SetColor(rs.ps and $2,OldReg.ps and $2);
            WriteStr(19,8,' v'+chr(byte((rs.ps and $2)<>0)+48),color);
            SetColor(rs.ps and $4,OldReg.ps and $4);
            WriteStr(16,8,' z'+chr(byte((rs.ps and $4)<>0)+48),color);
            SetColor(rs.ps and $8,OldReg.ps and $8);
            WriteStr(14,8, 'x'+chr(byte((rs.ps and $8)<>0)+48),color);
{$endif m68k}
{$ifdef powerpc}
            for i:=0 to 15 do
              begin
                SetColor(rs.r[i],OldReg.r[i]);
                if i<10 then
                  WriteStr(1,i,'r'+IntToStr(i)+'  '+HexStr(longint(rs.r[i]),8),color)
                else
                  WriteStr(1,i,'r'+IntToStr(i)+' '+HexStr(longint(rs.r[i]),8),color);
              end;
            for i:=16 to 31 do
              begin
                SetColor(rs.r[i],OldReg.r[i]);
                WriteStr(15,i-16,'r'+IntToStr(i)+' '+HexStr(longint(rs.r[i]),8),color);
              end;
            { other regs pc,ps,cr,lr,ctr,xer : dword; }
            SetColor(rs.pc,OldReg.pc);
            WriteStr(1,16,'pc  '+HexStr(longint(rs.pc),8),color);
            SetColor(rs.ps,OldReg.ps);
            WriteStr(15,16,'ps  '+HexStr(longint(rs.ps),8),color);
            SetColor(rs.lr,OldReg.lr);
            WriteStr(1,17,'lr  '+HexStr(longint(rs.lr),8),color);
            SetColor(rs.ctr,OldReg.ctr);
            WriteStr(15,17,'ctr '+HexStr(longint(rs.ctr),8),color);
            SetColor(rs.xer,OldReg.xer);
            WriteStr(15,18,'xer '+HexStr(longint(rs.xer),8),color);
{$endif powerpc}
{$ifdef sparc}
            for i:=0 to 7 do
              begin
                SetColor(rs.g[i],OldReg.g[i]);
                WriteStr(1,i,'g'+IntToStr(i)+'  '+HexStr(longint(rs.g[i]),8),color);
                SetColor(rs.l[i],OldReg.l[i]);
                WriteStr(1,i+8,'l'+IntToStr(i)+'  '+HexStr(longint(rs.l[i]),8),color);
              end;
            for i:=0 to 7 do
              begin
                SetColor(rs.i[i],OldReg.i[i]);
                if i=6 then
                  WriteStr(15,i,'fp  '+HexStr(longint(rs.i[i]),8),color)
                else
                  WriteStr(15,i,'i'+IntToStr(i)+'  '+HexStr(longint(rs.i[i]),8),color);
                SetColor(rs.o[i],OldReg.o[i]);
                WriteStr(15,i+8,'o'+IntToStr(i)+'  '+HexStr(longint(rs.o[i]),8),color);
              end;
            SetColor(rs.pc,OldReg.pc);
            WriteStr(1,16,'pc  '+HexStr(longint(rs.pc),8),color);
            SetColor(rs.y,OldReg.y);
            WriteStr(1,17,'y   '+HexStr(longint(rs.y),8),color);
            SetColor(rs.psr,OldReg.psr);
            WriteStr(1,18,'psr '+HexStr(longint(rs.psr),8),color);
            SetColor(rs.csr,OldReg.csr);
            WriteStr(1,19,'csr '+HexStr(longint(rs.csr),8),color);
            SetColor(rs.npc,OldReg.npc);
            WriteStr(15,16,'npc '+HexStr(longint(rs.npc),8),color);
            SetColor(rs.tbr,OldReg.tbr);
            WriteStr(15,17,'tbr '+HexStr(longint(rs.tbr),8),color);
            SetColor(rs.wim,OldReg.wim);
            WriteStr(15,18,'wim '+HexStr(longint(rs.wim),8),color);
            SetColor(rs.fsr,OldReg.fsr);
            WriteStr(15,19,'fsr '+HexStr(longint(rs.fsr),8),color);
{$endif sparc}
{$else cpu_known}
            for i:=0 to MaxRegs-1 do
              begin
                SetStrColor(rs.reg[i],OldReg.reg[i]);
                WriteStr(1,i,rs.reg[i],color);
              end;
{$endif cpu_known}
         end
       else
         WriteStr(0,0,'<debugger error>',7);
       InDraw:=false;
{$endif NODEBUG}
    end;

  destructor TRegistersView.Done;

    begin
       inherited done;
    end;

{****************************************************************************
                         TRegistersWindow
****************************************************************************}

  constructor TRegistersWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
{$ifdef i386}
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+11;
{$endif i386}
{$ifdef x86_64}
       R.A.X:=R.B.X-32;
       R.B.Y:=R.A.Y+19;
{$endif x86_64}
{$ifdef m68k}
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+11;
{$endif m68k}
{$ifdef powerpc}
       R.A.X:=R.B.X-30;
       R.B.Y:=R.A.Y+21;
{$endif powerpc}
{$ifdef sparc}
       R.A.X:=R.B.X-30;
       R.B.Y:=R.A.Y+22;
{$endif sparc}
{$ifndef cpu_known}
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+22;
{$endif cpu_known}
       inherited Init(R,dialog_registers, wnNoNumber);
       Flags:=wfClose or wfMove;
{$ifndef cpu_known}
       Flags:=Flags or wfgrow;
{$endif cpu_known}
       Palette:=wpCyanWindow;
       HelpCtx:=hcRegistersWindow;
       R.Assign(1,1,Size.X-2,Size.Y-1);
       RV:=new(PRegistersView,init(R));
       Insert(RV);
       If assigned(RegistersWindow) then
         dispose(RegistersWindow,done);
       RegistersWindow:=@Self;
       Update;
    end;

  constructor TRegistersWindow.Load(var S: TStream);

    begin
       inherited load(S);
       GetSubViewPtr(S,RV);
       If assigned(RegistersWindow) then
         dispose(RegistersWindow,done);
       RegistersWindow:=@Self;
    end;

  procedure TRegistersWindow.Store(var S: TStream);

    begin
       inherited Store(s);
       PutSubViewPtr(S,RV);
    end;

  procedure TRegistersWindow.Update;

    begin
       ReDraw;
    end;

  destructor TRegistersWindow.Done;

    begin
       RegistersWindow:=nil;
       inherited done;
    end;

{****************************************************************************
                         TFPUView
****************************************************************************}

  function GetFPURegs(var rs : TFPURegs
{$ifndef cpu_known}
             ; UseInfoFloat : boolean
{$endif not cpu_known}
             ) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
    {$ifndef NODEBUG}
       reg,value : string;
       buffer : array[0..255] of char;
       v : string;
       res : cardinal;
       i : longint;
       err : word;
    {$endif}

    begin
       GetFPURegs:=false;
{$ifndef NODEBUG}
{$ifndef cpu_known}
       if UseInfoFloat then
         begin
           Debugger^.Command('info float');
           if Debugger^.Error then
             begin
               UseInfofloat:=false;
               Debugger^.Command('info all');
             end;
         end
       else
{$endif not cpu_known}
         Debugger^.Command('info all');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
{$ifndef cpu_known}
            i:=0;
{$endif not cpu_known}
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
{$ifndef cpu_known}
                      p1:=strscan(p,#10);
                      if assigned(p1) then
                        begin
                          strlcopy(buffer,p,p1-p);
                          rs.freg[i]:=ExtractTabs(strpas(buffer),8);
                          if i<MaxRegs-1 then
                            inc(i);
                        end;
{$else  cpu_known}
                      p:=p1;
                      while p^=' ' do
                        inc(p);
                      if p^='$' then
                        p1:=strscan(p,#9)
                      else
                        p1:=strscan(p,#10);
                      strlcopy(buffer,p,p1-p);
                      v:=strpas(buffer);
                      for i:=1 to length(v) do
                        if v[i]=#9 then
                          v[i]:=' ';
                      val(v,res,err);
{$if defined(i386) or defined(x86_64)}
                      if reg='st0' then
                        rs.st0:=v
                      else if reg='st1' then
                        rs.st1:=v
                      else if reg='st2' then
                        rs.st2:=v
                      else if reg='st3' then
                        rs.st3:=v
                      else if reg='st4' then
                        rs.st4:=v
                      else if reg='st5' then
                        rs.st5:=v
                      else if reg='st6' then
                        rs.st6:=v
                      else if reg='st7' then
                        rs.st7:=v
                      else if reg='ftag' then
                        rs.ftag:=res
                      else if reg='fctrl' then
                        rs.fctrl:=res
                      else if reg='fstat' then
                        rs.fstat:=res
                      else if reg='fiseg' then
                        rs.fiseg:=res
                      else if reg='fioff' then
                        rs.fioff:=res
                      else if reg='foseg' then
                        rs.foseg:=res
                      else if reg='fooff' then
                        rs.fooff:=res
                      else if reg='fop' then
                        rs.fop:=res;
{$endif i386 or x86_64}
{$ifdef m68k}
                      if reg='fp0' then
                        rs.fp0:=v
                      else if reg='fp1' then
                        rs.fp1:=v
                      else if reg='fp2' then
                        rs.fp2:=v
                      else if reg='fp3' then
                        rs.fp3:=v
                      else if reg='fp4' then
                        rs.fp4:=v
                      else if reg='fp5' then
                        rs.fp5:=v
                      else if reg='fp6' then
                        rs.fp6:=v
                      else if reg='fp7' then
                        rs.fp7:=v
                      else if reg='fpcontrol' then
                        rs.fpcontrol:=res
                      else if reg='fpstatus' then
                        rs.fpstatus:=res
                      else if reg='fpiaddr' then
                        rs.fpiaddr:=res;
{$endif m68k}
{$ifdef powerpc}
                      if reg[1]='f' then
                        for i:=0 to 31 do
                          if reg='f'+inttostr(i) then
                            rs.f[i]:=v;
{$endif powerpc}
{$ifdef sparc}
                      if reg[1]='f' then
                        for i:=0 to 31 do
                          if reg='f'+inttostr(i) then
                            rs.f[i]:=v;
{$endif sparc}
{$endif cpu_known}
                      p:=strscan(p1,#10);
                      if assigned(p) then
                        begin
                           p1:=strscan(p,' ');
                           inc(p);
                        end
                      else
                        break;
                   end;
                 { free allocated memory }
                 strdispose(po);
              end
            else
              exit;
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
       GetFPURegs:=true;
{$endif}
    end;

  constructor TFPUView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
       GrowMode:=gfGrowHiX or GfGrowHiY;
       InDraw:=false;
       first:=true;
       FillChar(OldReg,Sizeof(oldreg),#0);
       FillChar(NewReg,Sizeof(newreg),#0);
       GDBCount:=-1;
{$ifndef cpu_known}
       UseInfoFloat:=true;
{$endif not cpu_known}
    end;

  procedure TFPUView.Draw;

    var
       rs : tfpuregs;
       top : byte;
       color :byte;
       ok : boolean;
       i : byte;
    const
      TypeStr : Array[0..3] of string[6] =
      ('Valid ','Zero  ','Spec  ','Empty ');

    procedure SetColor(Const x,y : string);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    procedure SetIColor(Const x,y : cardinal);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
{$ifdef NODEBUG}
       WriteStr(1,0,'<no values available>',7);
{$else NODEBUG}
       If not assigned(Debugger) then
         begin
            WriteStr(1,0,'<no values available>',7);
            exit;
         end;
       if InDraw then
         exit;
       InDraw:=true;
       if GDBCount<>Debugger^.RunCount then
         begin
           OldReg:=NewReg;
           OK:=GetFPURegs(rs
{$ifndef cpu_known}
             ,UseInfoFloat
{$endif not cpu_known}
             );
           NewReg:=rs;
           { get inital values }
           if first then
             begin
               OldReg:=NewReg;
               first:=false;
             end;
           GDBCount:=Debugger^.RunCount;
         end
       else
         begin
           rs:=newreg;
           OK:=true;
         end;
       if OK then
         begin
{$ifdef cpu_known}
{$if defined(i386) or defined(x86_64)}
            top:=(rs.fstat shr 11) and 7;
            SetColor(rs.st0,OldReg.st0);
            WriteStr(1,0,'ST0 '+TypeStr[(rs.ftag shr (2*((0+top) and 7))) and 3]+rs.st0,color);
            SetColor(rs.st1,OldReg.st1);
            WriteStr(1,1,'ST1 '+TypeStr[(rs.ftag shr (2*((1+top) and 7))) and 3]+rs.st1,color);
            SetColor(rs.st2,OldReg.st2);
            WriteStr(1,2,'ST2 '+TypeStr[(rs.ftag shr (2*((2+top) and 7))) and 3]+rs.st2,color);
            SetColor(rs.st3,OldReg.st3);
            WriteStr(1,3,'ST3 '+TypeStr[(rs.ftag shr (2*((3+top) and 7))) and 3]+rs.st3,color);
            SetColor(rs.st4,OldReg.st4);
            WriteStr(1,4,'ST4 '+TypeStr[(rs.ftag shr (2*((4+top) and 7))) and 3]+rs.st4,color);
            SetColor(rs.st5,OldReg.st5);
            WriteStr(1,5,'ST5 '+TypeStr[(rs.ftag shr (2*((5+top) and 7))) and 3]+rs.st5,color);
            SetColor(rs.st6,OldReg.st6);
            WriteStr(1,6,'ST6 '+TypeStr[(rs.ftag shr (2*((6+top) and 7))) and 3]+rs.st6,color);
            SetColor(rs.st7,OldReg.st7);
            WriteStr(1,7,'ST7 '+TypeStr[(rs.ftag shr (2*((7+top) and 7))) and 3]+rs.st7,color);
            SetIColor(rs.ftag,OldReg.ftag);
            WriteStr(1,8,'FTAG   '+hexstr(rs.ftag,4),color);
            SetIColor(rs.fctrl,OldReg.fctrl);
            WriteStr(13,8,'FCTRL  '+hexstr(rs.fctrl,4),color);
            SetIColor(rs.fstat,OldReg.fstat);
            WriteStr(1,9,'FSTAT  '+hexstr(rs.fstat,4),color);
            SetIColor(rs.fop,OldReg.fop);
            WriteStr(13,9,'FOP    '+hexstr(rs.fop,4),color);
            if (rs.fiseg<>OldReg.fiseg) or
               (rs.fioff<>OldReg.fioff) then
              color:=8
            else
              color:=7;
            WriteStr(1,10,'FI    '+hexstr(rs.fiseg,4)+':'+hexstr(rs.fioff,8),color);
            if (rs.foseg<>OldReg.foseg) or
               (rs.fooff<>OldReg.fooff) then
              color:=8
            else
              color:=7;
            WriteStr(1,11,'FO    '+hexstr(rs.foseg,4)+':'+hexstr(rs.fooff,8),color);
{$endif i386 or x86_64}
{$ifdef m68k}
            SetColor(rs.fp0,OldReg.fp0);
            WriteStr(1,0,'fp0 '+rs.fp0,color);
            SetColor(rs.fp1,OldReg.fp1);
            WriteStr(1,1,'fp1 '+rs.fp1,color);
            SetColor(rs.fp2,OldReg.fp2);
            WriteStr(1,2,'fp2 '+rs.fp2,color);
            SetColor(rs.fp3,OldReg.fp3);
            WriteStr(1,3,'fp3 '+rs.fp3,color);
            SetColor(rs.fp4,OldReg.fp4);
            WriteStr(1,4,'fp4 '+rs.fp4,color);
            SetColor(rs.fp5,OldReg.fp5);
            WriteStr(1,5,'fp5 '+rs.fp5,color);
            SetColor(rs.fp6,OldReg.fp6);
            WriteStr(1,6,'fp6 '+rs.fp6,color);
            SetColor(rs.fp7,OldReg.fp7);
            WriteStr(1,7,'fp7 '+rs.fp7,color);
            SetIColor(rs.fpcontrol,OldReg.fpcontrol);
            WriteStr(1,8,'fpcontrol   '+hexstr(rs.fpcontrol,8),color);
            SetIColor(rs.fpstatus,OldReg.fpstatus);
            WriteStr(1,9,'fpstatus    '+hexstr(rs.fpstatus,8),color);
            SetIColor(rs.fpiaddr,OldReg.fpiaddr);
            WriteStr(1,10,'fpiaddr    '+hexstr(rs.fpiaddr,8),color);
{$endif m68k}
{$ifdef powerpc}
            for i:=0 to 31 do
              begin
                SetColor(rs.f[i],OldReg.f[i]);
                if i<10 then
                  WriteStr(1,i,'f'+IntToStr(i)+'  '+rs.f[i],color)
                else
                  WriteStr(1,i,'f'+IntToStr(i)+' '+rs.f[i],color);
              end;
{$endif powerpc}
{$ifdef sparc}
            for i:=0 to 31 do
              begin
                SetColor(rs.f[i],OldReg.f[i]);
                if i<10 then
                  WriteStr(1,i,'f'+IntToStr(i)+'  '+rs.f[i],color)
                else
                  WriteStr(1,i,'f'+IntToStr(i)+' '+rs.f[i],color);
              end;
{$endif sparc}
{$else not cpu_known}
            for i:=0 to MaxRegs-1 do
              begin
                SetColor(rs.freg[i],OldReg.freg[i]);
                WriteStr(1,i,rs.freg[i],color);
              end;
{$endif cpu_known}
         end
       else
         WriteStr(0,0,'<debugger error>',7);
       InDraw:=false;
{$endif NODEBUG}
    end;

  destructor TFPUView.Done;

    begin
       inherited done;
    end;

{****************************************************************************
                         TFPUWindow
****************************************************************************}

  constructor TFPUWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
{$if defined(i386) or defined(x86_64)}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+14;
{$endif i386 or x86_64}
{$ifdef m68k}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+14;
{$endif m68k}
{$ifdef powerpc}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+33;
{$endif powerpc}
{$ifdef sparc}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+33;
{$endif sparc}
{$ifndef cpu_known}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+33;
{$endif cpu_known}
       inherited Init(R,dialog_fpu, wnNoNumber);
       Flags:=wfClose or wfMove or wfgrow;
       Palette:=wpCyanWindow;
       HelpCtx:=hcFPURegisters;
       R.Assign(1,1,Size.X-2,Size.Y-2);
       RV:=new(PFPUView,init(R));
       Insert(RV);
       If assigned(FPUWindow) then
         dispose(FPUWindow,done);
       FPUWindow:=@Self;
       Update;
    end;

  constructor TFPUWindow.Load(var S: TStream);

    begin
       inherited load(S);
       GetSubViewPtr(S,RV);
       If assigned(FPUWindow) then
         dispose(FPUWindow,done);
       FPUWindow:=@Self;
    end;

  procedure TFPUWindow.Store(var S: TStream);

    begin
       inherited Store(s);
       PutSubViewPtr(S,RV);
    end;

  procedure TFPUWindow.Update;

    begin
       ReDraw;
    end;

  destructor TFPUWindow.Done;

    begin
       FPUWindow:=nil;
       inherited done;
    end;


{****************************************************************************
                         TVectorView
****************************************************************************}

  function GetVectorRegs(var rs : TVectorRegs
{$ifndef cpu_known}
             ; UseInfoVector : boolean
{$endif not cpu_known}
             ) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
    {$ifndef NODEBUG}
       reg,value : string;
       buffer : array[0..255] of char;
       v : string;
       res : cardinal;
       i : longint;
       err : word;
    {$endif}

    begin
       GetVectorRegs:=false;
{$ifndef NODEBUG}
{$ifndef cpu_known}
       if UseInfoVector then
         begin
           Debugger^.Command('info vector');
           if Debugger^.Error then
             begin
               UseInfoVector:=false;
               Debugger^.Command('info all');
             end;
         end
       else
{$endif not cpu_known}
         Debugger^.Command('info vector');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
{$ifndef cpu_known}
            i:=0;
{$endif not cpu_known}
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
{$ifndef cpu_known}
                      p1:=strscan(p,#10);
                      if assigned(p1) then
                        begin
                          strlcopy(buffer,p,p1-p);
                          rs.vreg[i]:=ExtractTabs(strpas(buffer),8);
                          if i<MaxRegs-1 then
                            inc(i);
                        end;
{$else  cpu_known}
                      p:=p1;
                      while p^=' ' do
                        inc(p);
                      if p^='$' then
                        p1:=strscan(p,#9)
                      else
                        p1:=strscan(p,#10);
                      strlcopy(buffer,p,p1-p);
                      v:=strpas(buffer);
                      for i:=1 to length(v) do
                        if v[i]=#9 then
                          v[i]:=' ';
                      val(v,res,err);
{$if defined(i386) or defined(x86_64)}
                      if reg[1]='x' then
                        for i:=0 to 7 do
                          begin
                            if reg='xmm'+inttostr(i) then
                              rs.xmm[i]:=v
                          end
                      else if reg='mxcsr' then
                        rs.mxcsr:=v
                      else if reg[1]='m' then
                        for i:=0 to 7 do
                          begin
                            if reg='mm'+inttostr(i) then
                              rs.mmx[i]:=v;
                          end;
{$endif i386 or x86_64}
{$ifdef powerpc}
                      { !!!! fixme }
                      if reg[1]='v' then
                        for i:=0 to 31 do
                          if reg='v'+inttostr(i) then
                            rs.m[i]:=v;
{$endif powerpc}
{$ifdef sparc}
{$endif sparc}
{$endif cpu_known}
                      p:=strscan(p1,#10);
                      if assigned(p) then
                        begin
                           p1:=strscan(p,' ');
                           inc(p);
                        end
                      else
                        break;
                   end;
                 { free allocated memory }
                 strdispose(po);
              end
            else
              exit;
         end;
       { do not open a messagebox for such errors }
       Debugger^.got_error:=false;
       GetVectorRegs:=true;
{$endif}
    end;

  constructor TVectorView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
       GrowMode:=gfGrowHiX or GfGrowHiY;
       InDraw:=false;
       first:=true;
       FillChar(OldReg,Sizeof(oldreg),#0);
       FillChar(NewReg,Sizeof(newreg),#0);
       GDBCount:=-1;
{$ifndef cpu_known}
       UseInfoVector:=true;
{$endif not cpu_known}
    end;

  procedure TVectorView.Draw;

    var
       rs : tVectorregs;
       top : byte;
       color :byte;
       ok : boolean;
       i : byte;
    const
      TypeStr : Array[0..3] of string[6] =
      ('Valid ','Zero  ','Spec  ','Empty ');

    procedure SetColor(Const x,y : string);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    procedure SetIColor(Const x,y : cardinal);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
{$ifdef NODEBUG}
       WriteStr(1,0,'<no values available>',7);
{$else NODEBUG}
       If not assigned(Debugger) then
         begin
            WriteStr(1,0,'<no values available>',7);
            exit;
         end;
       if InDraw then
         exit;
       InDraw:=true;
       if GDBCount<>Debugger^.RunCount then
         begin
           OldReg:=NewReg;
           OK:=GetVectorRegs(rs
{$ifndef cpu_known}
             ,UseInfoVector
{$endif not cpu_known}
             );
           NewReg:=rs;
           { get inital values }
           if first then
             begin
               OldReg:=NewReg;
               first:=false;
             end;
           GDBCount:=Debugger^.RunCount;
         end
       else
         begin
           rs:=newreg;
           OK:=true;
         end;
       if OK then
         begin
{$ifdef cpu_known}
{$if defined(i386) or defined(x86_64)}
            for i:=0 to 7 do
              begin
                SetColor(rs.xmm[i],OldReg.xmm[i]);
                WriteStr(1,i,'xmm'+IntToStr(i)+'  '+rs.xmm[i],color);
              end;

            SetColor(rs.mxcsr,OldReg.mxcsr);
            WriteStr(1,8,'mxcsr'+IntToStr(i)+'  '+rs.mxcsr,color);

            for i:=0 to 7 do
              begin
                SetColor(rs.mmx[i],OldReg.mmx[i]);
                WriteStr(1,i+9,'mmx'+IntToStr(i)+'  '+rs.mmx[i],color);
              end;
{$endif i386 or x86_64}
{$ifdef powerpc}
            for i:=0 to 31 do
              begin
                SetColor(rs.m[i],OldReg.m[i]);
                if i<10 then
                  WriteStr(1,i,'m'+IntToStr(i)+'  '+rs.m[i],color)
                else
                  WriteStr(1,i,'m'+IntToStr(i)+' '+rs.m[i],color);
              end;
{$endif powerpc}
{$ifdef sparc}
            { no mm regs on the sparc }
{$endif sparc}
{$else not cpu_known}
            for i:=0 to MaxRegs-1 do
              begin
                SetColor(rs.vreg[i],OldReg.vreg[i]);
                WriteStr(1,i,rs.vreg[i],color);
              end;
{$endif cpu_known}
         end
       else
         WriteStr(0,0,'<debugger error>',7);
       InDraw:=false;
{$endif NODEBUG}
    end;

  destructor TVectorView.Done;

    begin
       inherited done;
    end;

{****************************************************************************
                         TVectorWindow
****************************************************************************}

  constructor TVectorWindow.Init;

    var
       R : TRect;

    begin
       Desktop^.GetExtent(R);
{$if defined(i386) or defined(x86_64)}
       R.A.X:=R.B.X-60;
       R.B.Y:=R.A.Y+20;
{$endif i386 or x86_64}
{$ifdef m68k}
       R.A.X:=R.B.X-60;
       R.B.Y:=R.A.Y+14;
{$endif m68k}
{$ifdef powerpc}
       R.A.X:=R.B.X-60;
       R.B.Y:=R.A.Y+33;
{$endif powerpc}
{$ifdef sparc}
       R.A.X:=R.B.X-60;
       R.B.Y:=R.A.Y+33;
{$endif sparc}
{$ifndef cpu_known}
       R.A.X:=R.B.X-60;
       R.B.Y:=R.A.Y+33;
{$endif cpu_known}
       inherited Init(R,dialog_Vector, wnNoNumber);
       Flags:=wfClose or wfMove or wfgrow;
       Palette:=wpCyanWindow;
       HelpCtx:=hcVectorRegisters;
       R.Assign(1,1,Size.X-2,Size.Y-2);
       RV:=new(PVectorView,init(R));
       Insert(RV);
       If assigned(VectorWindow) then
         dispose(VectorWindow,done);
       VectorWindow:=@Self;
       Update;
    end;

  constructor TVectorWindow.Load(var S: TStream);

    begin
       inherited load(S);
       GetSubViewPtr(S,RV);
       If assigned(VectorWindow) then
         dispose(VectorWindow,done);
       VectorWindow:=@Self;
    end;

  procedure TVectorWindow.Store(var S: TStream);

    begin
       inherited Store(s);
       PutSubViewPtr(S,RV);
    end;

  procedure TVectorWindow.Update;

    begin
       ReDraw;
    end;

  destructor TVectorWindow.Done;

    begin
       VectorWindow:=nil;
       inherited done;
    end;


procedure InitRegistersWindow;
begin
  if RegistersWindow=nil then
    begin
      new(RegistersWindow,init);
      DeskTop^.Insert(RegistersWindow);
    end;
end;


procedure DoneRegistersWindow;
begin
  if assigned(RegistersWindow) then
    begin
      DeskTop^.Delete(RegistersWindow);
      RegistersWindow:=nil;
    end;
end;


procedure InitFPUWindow;
begin
  if FPUWindow=nil then
    begin
      new(FPUWindow,init);
      DeskTop^.Insert(FPUWindow);
    end;
end;


procedure DoneFPUWindow;
begin
  if assigned(FPUWindow) then
    begin
      DeskTop^.Delete(FPUWindow);
      FPUWindow:=nil;
    end;
end;


procedure InitVectorWindow;
begin
  if VectorWindow=nil then
    begin
      new(VectorWindow,init);
      DeskTop^.Insert(VectorWindow);
    end;
end;


procedure DoneVectorWindow;
begin
  if assigned(VectorWindow) then
    begin
      DeskTop^.Delete(VectorWindow);
      VectorWindow:=nil;
    end;
end;


procedure RegisterFPRegsViews;
begin
  RegisterType(RRegistersWindow);
  RegisterType(RRegistersView);
  RegisterType(RFPUWindow);
  RegisterType(RFPUView);
  RegisterType(RVectorView);
end;

end.
{$endif NODEBUG}
