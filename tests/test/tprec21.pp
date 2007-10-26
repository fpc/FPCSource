const
RS_CR = $00;
RS_CR0 = $01;
RS_CR1 = $02;
RS_CR2 = $03;
RS_CR3 = $04;
RS_CR4 = $05;
RS_CR5 = $06;
RS_CR6 = $07;
RS_CR7 = $08;

    type
      TAsmCondFlag = (C_None { unconditional jumps },
        { conditions when not using ctr decrement etc }
        C_LT,C_LE,C_EQ,C_GE,C_GT,C_NL,C_NE,C_NG,C_SO,C_NS,C_UN,C_NU,
        { conditions when using ctr decrement etc }
        C_T,C_F,C_DNZ,C_DNZT,C_DNZF,C_DZ,C_DZT,C_DZF);

      TDirHint = (DH_None,DH_Minus,DH_Plus);

    type
      TAsmCond = bitpacked record
                   dirhint : tdirhint;
                   case simple: boolean of
                     false: (BO, BI: 0..31);
                     true: (
                       cond: TAsmCondFlag;
                       case byte of
                         0: ();
                         { specifies in which part of the cr the bit has to be }
                         { tested for blt,bgt,beq,..,bnu                       }
                         1: (cr: RS_CR0..RS_CR7);
                         { specifies the bit to test for bt,bf,bdz,..,bdzf }
                         2: (crbit: 0..31)
                       );
                 end;

const
   zerocond: tasmcond = (dirhint: DH_Plus; 
                         simple: true;
                         cond:C_NE;
                         cr: RS_CR1);


begin
  with zerocond do
    if (dirhint <> DH_Plus) or
       not simple or
       (cond <> C_NE) or
       (cr <> RS_CR1) then
      halt(1);
end.
