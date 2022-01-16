{ Registers record used by Intr and MsxDos. This include file is shared between
  the system unit and the dos unit. }

type
  Registers = packed record
    case Integer of
      0: (BC, DE, AF, HL, IX, IY: Word);
      1: (C, B, E, D, Flags, A, L, H, IXl, IXh, IYl, IYh: Byte);
  end;

