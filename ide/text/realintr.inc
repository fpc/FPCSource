{$IFDEF DPMI}
const
    DPMI_INTR      = $31;

type
    TDPMIRegisters = record     { DPMI call structure }
      EDI     : LongInt;
      ESI     : LongInt;
      EBP     : LongInt;
      Reserved: LongInt;
      EBX     : LongInt;
      EDX     : LongInt;
      ECX     : LongInt;
      EAX     : LongInt;
      Flags   : Word;
      ES      : Word;
      DS      : Word;
      FS      : Word;
      GS      : Word;
      IP      : Word;
      CS      : Word;
      SP      : Word;
      SS      : Word;
    end;

  var
    DPMIRegs: TDPMIRegisters;

  procedure realintr(IntNo: byte; var r: registers);
  var Regs: Registers;
  begin
    FillChar(DPMIRegs, SizeOf(TDPMIRegisters), 0);
    DPMIRegs.EAX := r.ax;
    DPMIRegs.EBX := r.bx;
    DPMIRegs.ECX := r.cx;
    DPMIRegs.EDX := r.dx;
    DPMIRegs.EDI := r.di;
    DPMIRegs.ESI := r.si;
    DPMIRegs.EBP := r.bp;
    DPMIRegs.DS := r.ds;
    DPMIRegs.ES := r.es;
    { --- }
    DPMIRegs.FS := 0;
    DPMIRegs.GS := 0;
    DPMIRegs.SS := 0;
    DPMIRegs.SP := 0;
    { --- }
    DPMIRegs.Flags := r.flags;
    Regs.AX := $0300;
    Regs.BL := IntNo;
    Regs.BH := 0;
    Regs.CX := 0;
    Regs.ES := Seg(DPMIRegs);
    Regs.DI := Ofs(DPMIRegs);
    Intr(DPMI_INTR, Regs);
    r.ax := DPMIRegs.EAX;
    r.bx := DPMIRegs.EBX;
    r.cx := DPMIRegs.ECX;
    r.dx := DPMIRegs.EDX;
    r.di := DPMIRegs.EDI;
    r.si := DPMIRegs.ESI;
    r.bp := DPMIRegs.EBP;
    r.ds := DPMIRegs.DS;
    r.es := DPMIRegs.ES;
    r.Flags := DPMIRegs.Flags;
  end;
{$ENDIF}
