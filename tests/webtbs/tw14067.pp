{ %cpu=x86_64 }
{ %interactive }

{ check the assembler file for superfluous register moves from/to the
  parameter xmm regs }

program disasm;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { you can add units after this };

(*procedure ProcReg(A: Int64; B: Int64; C: Int64; D: Int64; E, F, G, H: Int64); register;
begin

end;

procedure ProcStd(A: Int64; B: Int64; C: Int64; D: Int64; E, F, G, H: Int64); stdcall;
begin

end;*)

procedure ProcFReg(A, B, C, D, E, F, G, H: Double); register;
begin

end;

procedure ProcFStd(A, B, C, D, E, F, G, H: Double); stdcall;
begin

end;

begin
  (*asm
    push %R8
    pop %R8
  end;
  ProcReg($10, $20, $30, $40, $50, $60, $70, $80);
  asm
    push %R8
    pop %R8
  end;
  ProcStd($10, $20, $30, $40, $50, $60, $70, $80);
  asm
    push %R8
    pop %R8
  end;
  ProcFReg(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80);
  asm
    push %R8
    pop %R8
  end;                                                     *)
  ProcFStd(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80);
  (*asm
    push %R8
    pop %R8
  end;*)
end.

