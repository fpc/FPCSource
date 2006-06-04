{$mode delphi}
const
  RS_CR0 = 1;
  RS_CR1 = 2;
  RS_CR2 = 3;
  RS_CR3 = 4;
  RS_CR4 = 5;
  RS_CR5 = 6;
  RS_CR6 = 7;
  RS_CR7 = 8;
type
  TResFlagsEnum = (F_EQ, F_NE, F_LT, F_LE, F_GT, F_GE, F_SO, F_FX, F_FEX, F_VX,
    F_OX);
  TResFlags = record
    cr: RS_CR0..RS_CR7;
    flag: TResFlagsEnum;
  end;

type
  TAsmCondFlag = (C_None { unconditional jumps },
    { conditions when not using ctr decrement etc }
    C_LT, C_LE, C_EQ, C_GE, C_GT, C_NL, C_NE, C_NG, C_SO, C_NS, C_UN, C_NU,
    { conditions when using ctr decrement etc }
    C_T, C_F, C_DNZ, C_DNZT, C_DNZF, C_DZ, C_DZT, C_DZF);

  TDirHint = (DH_None, DH_Minus, DH_Plus);

const
  { these are in the XER, but when moved to CR_x they correspond with the }
  { bits below                                                            }
  C_OV = C_GT;
  C_CA = C_EQ;
  C_NO = C_NG;
  C_NC = C_NE;


type
  TAsmCond = packed record
    dirhint: tdirhint;
    case simple: boolean of
      false: (BO, BI: byte);
      true: (
        cond: TAsmCondFlag;
        case byte of
          0: ();
          { specifies in which part of the cr the bit has to be }
          { tested for blt,bgt,beq,..,bnu                       }
          1: (cr: RS_CR0..RS_CR7);
          { specifies the bit to test for bt,bf,bdz,..,bdzf }
          2: (crbit: byte)
          );
  end;

procedure error(err : int64);
begin
  writeln('Error: ', err);
  halt(1);
end;


function flags_to_cond(const f: TResFlags): TAsmCond;
const
  flag_2_cond: array[F_EQ..F_SO] of TAsmCondFlag =
  (C_EQ, C_NE, C_LT, C_LE, C_GT, C_GE, C_SO);
begin
  if f.flag > high(flag_2_cond) then
    error(1);
  case f.flag of
    F_EQ, F_NE, F_LT, F_LE, F_GT, F_GE, F_SO, F_FX, F_FEX, F_VX,
    F_OX:
      ;
  else
    error(2);
  end;
  result.simple := true;
  result.cr := f.cr;
  result.cond := flag_2_cond[f.flag];
end;

var flags : TResFlags;
begin
   flags.cr := RS_CR7;
   flags.flag := F_EQ;
   flags_to_cond(flags);  
   writeln('Passed');
end.


