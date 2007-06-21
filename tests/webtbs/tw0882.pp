{$D+,I+,L+,P-,Q+,R+,S+,T+,V+,X+,Y+}
{$M 8192,0,655360}
PROGRAM TEST;
CONST
        maxBlockSize    = 1 SHL 13;
TYPE
        byteBlock               = ARRAY[0..PRED(maxBlockSize)] OF byte;
VAR
        bb0                     : ^byteBlock;
TYPE
        rec     =       RECORD  i1,     len     : word  END;
VAR
        mr              : rec;
        bw              : word;
BEGIN
        NEW(bb0);
        mr.i1:=0; mr.len:=0;
        bb0^[0] := 1;
        bb0^[1] := 2;
       {$T+}
        bw:=word(Addr(bb0^[mr.i1])^);
        if bw <> 1 then
          halt(1);
       {$T-}
        bw:=word(Addr(bb0^[mr.i1])^);
{$ifndef ENDIAN_BIG}
        if bw <> (2 shl 8 + 1) then
{$else ENDIAN_BIG}
        if bw <> (1 shl 8 + 2) then
{$endif ENDIAN_BIG}
          halt(1);
END
.

