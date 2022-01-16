{ Registers record used by Intr and MsDos. This include file is shared between
  the system unit and the dos unit. }

type
  Registers = packed record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;
