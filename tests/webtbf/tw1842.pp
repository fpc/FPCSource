{ %fail }

type
  s8=shortint;
  s16=smallint;
  s32=longint;
  addrtype=pchar;
  InPacket=pchar;

PROCEDURE getlrc1 (     Buffer   : AddrType ;
                       StartPos : s32 ;
                       MaxPos   : s32 ;
                       LastChar : s32 ;
                   VAR LRCPos   : s16 ;
                   VAR LRCVal   : s8);  CDECL; [Public, alias: 'getlrc'];
begin
  LRCPos:=200;
  LRCVal:=100;
end;


PROCEDURE CalcLRC   ( APacket   : InPacket ;
                          PacketLen : s32;
                          StartPos  : s16  ;
                          EndingChar: s16  ;
                      VAR LRCPos    : s16  ;
                      VAR LRCVal    : s8     );


PROCEDURE getlrc (     Buffer   : AddrType ;
                       StartPos : s32 ;
                       MaxPos   : s32 ;
                       LastChar : s32 ;
                   VAR LRCPos   : s16 ;
                   VAR LRCVal   : s8);  CDECL; EXTERNAL;

BEGIN
getlrc (APacket,
        StartPos - 1,
        PacketLen,
        EndingChar,   {--ETX or SO--}
        LRCPos,
        LRCVal);
LRCPos := LRCPos + 1;
END; { CalcLRC }


VAR
  LRCPos    : s16  ;
  LRCVal    : s8;

begin
  calclrc('12345',5,2,ord('5'),LRCPos,LRCVal);
  writeln(LRCPos,' ',LRCVal);
end.
