{
    $Id$
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

interface

uses
{$ifdef win32}
  Windows,
{$endif win32}
  Objects,Dialogs,Drivers,Views,
  FPViews;

  type

{$ifdef TP}
    dword = longint;
{$endif TP}

    TIntRegs = record
{$ifdef I386}
       eax,ebx,ecx,edx,eip,esi,edi,esp,ebp : dword;
       cs,ds,es,ss,fs,gs : word;
       eflags : dword;
{$endif I386}
{$ifdef m68k}
       d0,d1,d2,d3,d4,d5,d6,d7 : dword;
       a0,a1,a2,a3,a4,a5,fp,sp : dword;
       ps,pc : dword;
{$endif m68k}
{$ifdef powerpc}
       r : array [0..31] of dword;
       pc,ps,cr,lr,ctr,xer : dword;
{$endif powerpc}
    end;

    PRegistersView = ^TRegistersView;
    TRegistersView = object(TView)
      NewReg,OldReg : TIntRegs;
      InDraw : boolean;
      GDBCount : longint;
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
{$ifdef I386}
      st0,st1,st2,st3,st4,st5,st6,st7 :string;
      ftag,fop,fctrl,fstat,fiseg,foseg : word;
      fioff,fooff : cardinal;
{$endif I386}
{$ifdef m68k}
      fp0,fp1,fp2,fp3,fp4,fp5,fp6,fp7 : string;
      fpcontrol,fpstatus,fpiaddr : dword;
{$endif m68k}
{$ifdef powerpc}
       f : array [0..31] of string;
{$endif powerpc}
    end;

    PFPUView = ^TFPUView;
    TFPUView = object(TView)
      NewReg,OldReg : TFPURegs;
      InDraw : boolean;
      GDBCount : longint;
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

  procedure InitRegistersWindow;
  procedure DoneRegistersWindow;
  procedure InitFPUWindow;
  procedure DoneFPUWindow;

  procedure RegisterFPRegsViews;

implementation

uses
  Strings,
  GDBCon,GDBInt,
  App,Menus,
  WViews,WEditor,
  FPConst,FPVars,
  FPString,
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


{****************************************************************************
                         TRegistersView
****************************************************************************}

  function GetIntRegs(var rs : TIntRegs) : boolean;

    var
       p,po : pchar;
       p1 : pchar;
       reg,value : string;
       buffer : array[0..255] of char;
       v : dword;
       code : word;
{$ifdef powerpc}
       i : byte;
{$endif powerpc}

    begin
       GetIntRegs:=false;
{$ifndef NODEBUG}
       Debugger^.Command('info registers');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
                      p:=strscan(p,'$');
                      p1:=strscan(p,#9);
                      strlcopy(buffer,p,p1-p);
                      value:=strpas(buffer);
                      val(value,v,code);
{$ifdef i386}
                      if reg='eax' then
                        rs.eax:=v
                      else if reg='ebx' then
                        rs.ebx:=v
                      else if reg='ecx' then
                        rs.ecx:=v
                      else if reg='edx' then
                        rs.edx:=v
                      else if reg='eip' then
                        rs.eip:=v
                      else if reg='esi' then
                        rs.esi:=v
                      else if reg='edi' then
                        rs.edi:=v
                      else if reg='esp' then
                        rs.esp:=v
                      else if reg='ebp' then
                        rs.ebp:=v
                      { under win32 flags are on a register named ps !! PM }
                      else if (reg='eflags') or (reg='ps') then
                        rs.eflags:=v
                      else if reg='cs' then
                        rs.cs:=v
                      else if reg='ds' then
                        rs.ds:=v
                      else if reg='es' then
                        rs.es:=v
                      else if reg='fs' then
                        rs.fs:=v
                      else if reg='gs' then
                        rs.gs:=v
                      else if reg='ss' then
                        rs.ss:=v;
{$endif i386}
{$ifdef m68k}
                      if reg='d0' then
                        rs.d0:=v
                      else if reg='d1' then
                        rs.d1:=v
                      else if reg='d2' then
                        rs.d2:=v
                      else if reg='d3' then
                        rs.d3:=v
                      else if reg='d4' then
                        rs.d4:=v
                      else if reg='d5' then
                        rs.d5:=v
                      else if reg='d6' then
                        rs.d6:=v
                      else if reg='d7' then
                        rs.d7:=v
                      else if reg='a0' then
                        rs.a0:=v
                      else if reg='a1' then
                        rs.a1:=v
                      else if reg='a2' then
                        rs.a2:=v
                      else if reg='a3' then
                        rs.a3:=v
                      else if reg='a4' then
                        rs.a4:=v
                      else if reg='a5' then
                        rs.a5:=v
                      else if reg='fp' then
                        rs.fp:=v
                      else if reg='sp' then
                        rs.sp:=v
                      else if (reg='ps') then
                        rs.ps:=v
                      else if reg='pc' then
                        rs.pc:=v;
{$endif m68k}
{$ifdef powerpc}
                      if (reg[1]='r') then
                        begin
                          for i:=0 to 31 do
                            if reg='r'+inttostr(i) then
                              rs.r[i]:=v;
                        end
                      { other regs
                        pc,ps,cr,lr,ctr,xer : dword; }
                      else if (reg='pc') then
                        rs.pc:=v
                      else if (reg='ps') then
                        rs.ps:=v
                      else if (reg='lr') then
                        rs.lr:=v
                      else if (reg='ctr') then
                        rs.ctr:=v
                      else if (reg='xer') then
                        rs.xer:=v;
{$endif powerpc}
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
{$endif}
    end;

  constructor TRegistersView.Init(var Bounds: TRect);

    begin
       inherited init(Bounds);
       InDraw:=false;
       FillChar(OldReg,Sizeof(OldReg),#0);
       FillChar(NewReg,Sizeof(NewReg),#0);
       GDBCount:=-1;
    end;

  procedure TRegistersView.Draw;

    var
       rs : tintregs;
       OK : boolean;
       color :byte;
{$ifdef powerpc}
       i : byte;
{$endif powerpc}

    procedure SetColor(x,y : longint);
    begin
      if x=y then
        color:=7
      else
        color:=8;
    end;

    begin
       inherited draw;
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
           GDBCount:=Debugger^.RunCount;
         end
       else
         begin
           rs:=NewReg;
           OK:=true;
         end;
       if  OK then
         begin
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
         end
       else
         WriteStr(0,0,'<debugger error>',7);
       InDraw:=false;
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
{$ifdef m68k}
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+11;
{$endif m68k}
{$ifdef powerpc}
       R.A.X:=R.B.X-28;
       R.B.Y:=R.A.Y+22;
{$endif powerpc}
       inherited Init(R,dialog_registers, wnNoNumber);
       Flags:=wfClose or wfMove;
       Palette:=wpCyanWindow;
       HelpCtx:=hcRegistersWindow;
       R.Assign(1,1,Size.X-1,Size.Y-1);
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

  function GetFPURegs(var rs : TFPURegs) : boolean;

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
       Debugger^.Command('info all');
       if Debugger^.Error then
         exit
       else
         begin
            po:=StrNew(Debugger^.GetOutput);
            p:=po;
            if assigned(p) then
              begin
                 fillchar(rs,sizeof(rs),0);
                 p1:=strscan(p,' ');
                 while assigned(p1) do
                   begin
                      strlcopy(buffer,p,p1-p);
                      reg:=strpas(buffer);
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
{$ifdef i386}
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
{$endif i386}
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
       GrowMode:=gfGrowHiY+gfGrowHiX;
       InDraw:=false;
       FillChar(OldReg,Sizeof(oldreg),#0);
       FillChar(NewReg,Sizeof(newreg),#0);
       GDBCount:=-1;
    end;

  procedure TFPUView.Draw;

    var
       rs : tfpuregs;
       top : byte;
       color :byte;
       ok : boolean;
{$ifdef powerpc}
       i : byte;
{$endif powerpc}
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
           OK:=GetFPURegs(rs);
           NewReg:=rs;
           GDBCount:=Debugger^.RunCount;
         end
       else
         begin
           rs:=newreg;
           OK:=true;
         end;
       if OK then
         begin
{$ifdef i386}
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
{$endif i386}
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
         end
       else
         WriteStr(0,0,'<debugger error>',7);
       InDraw:=false;
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
{$ifdef i386}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+14;
{$endif i386}
{$ifdef m68k}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+14;
{$endif m68k}
{$ifdef powerpc}
       R.A.X:=R.B.X-44;
       R.B.Y:=R.A.Y+33;
{$endif powerpc}
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


procedure RegisterFPRegsViews;
begin
  RegisterType(RRegistersWindow);
  RegisterType(RRegistersView);
  RegisterType(RFPUWindow);
  RegisterType(RFPUView);
end;

end.

{
  $Log$
  Revision 1.1  2002-12-12 00:01:59  pierre
    Register window code separated in a new unit

}
