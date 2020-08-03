unit wasmbincode;

interface

uses
  Classes, SysUtils, lebutils;

const
  VALTYPE_NONE = $40;
  VALTYPE_I32  = $7F;
  VALTYPE_I64  = $7E;
  VALTYPE_F32  = $7D;
  VALTYPE_F64  = $7C;

  INST_TRAP = $00;
  // ..
  INST_IF   = $04;
  INST_ELSE = $05;
  // ...
  INST_END  = $0b;
  // ...
  INST_REINTERPRET_I64 = $BF;

  MIN_INST = INST_TRAP;
  MAX_INST = INST_REINTERPRET_I64;

type
  TInstParamType = (ipNone,
    ipLeb,   // label index or function index
    ip2Leb,  // memory arguments, ask for offset + align
    ipi32,     // signed Leb of maximum 4 bytes
    ipi64,     // signed Leb of maximum 8 bytes
    ipf32,     // float point single
    ipf64,     // float point double
    ipTable,   // a complex structure... used for br_table only
    ipResType  // result type used for blocks, such as If, block or loop
  );
  TInstFlag = record
    valid : Boolean;
    Param : TInstParamType;
  end;

const
  INST_FLAGS : array [MIN_INST..MAX_INST] of TInstFlag = (
    (valid: true;  Param: ipNone)     // 00 trap (unreachable)
   ,(valid: true;  Param: ipNone)     // 01 nop
   ,(valid: true;  Param: ipResType)  // 02
   ,(valid: true;  Param: ipResType)  // 03
   ,(valid: true;  Param: ipResType)  // 04
   ,(valid: true;  Param: ipResType)  // 05
   ,(valid: false; Param: ipNone)     // 06
   ,(valid: false; Param: ipNone)     // 07
   ,(valid: false; Param: ipNone)     // 08
   ,(valid: false; Param: ipNone)     // 09
   ,(valid: false; Param: ipNone)     // 0A
   ,(valid: true;  Param: ipNone)     // 0B  end
   ,(valid: true;  Param: ipLeb)      // 0C  br
   ,(valid: true;  Param: ipLeb)      // 0D  br_if
   ,(valid: true;  Param: ipTable)    // 0E  br_table
   ,(valid: true;  Param: ipNone)     // 0F
   ,(valid: true;  Param: ipLeb)      // 10  call
   ,(valid: true;  Param: ipLeb)      // 11  call_indirect
   ,(valid: false; Param: ipNone)     // 12
   ,(valid: false; Param: ipNone)     // 13
   ,(valid: false; Param: ipNone)     // 14
   ,(valid: false; Param: ipNone)     // 15
   ,(valid: false; Param: ipNone)     // 16
   ,(valid: false; Param: ipNone)     // 17
   ,(valid: false; Param: ipNone)     // 18
   ,(valid: false; Param: ipNone)     // 19
   ,(valid: true;  Param: ipNone)     // 1A
   ,(valid: true;  Param: ipNone)     // 1B
   ,(valid: false; Param: ipNone)     // 1C
   ,(valid: false; Param: ipNone)     // 1D
   ,(valid: false; Param: ipNone)     // 1E
   ,(valid: false; Param: ipNone)     // 1F
   ,(valid: true;  Param: ipLeb)      // 20  local.get
   ,(valid: true;  Param: ipLeb)      // 21  local.set
   ,(valid: true;  Param: ipLeb)      // 22  local.tee
   ,(valid: true;  Param: ipLeb)      // 23  global.get
   ,(valid: true;  Param: ipLeb)      // 24  global.set
   ,(valid: false; Param: ipNone)     // 25
   ,(valid: false; Param: ipNone)     // 26
   ,(valid: false; Param: ipNone)     // 27
   ,(valid: true;  Param: ip2Leb)     // 28 i32.load
   ,(valid: true;  Param: ip2Leb)     // 29
   ,(valid: true;  Param: ip2Leb)     // 2A
   ,(valid: true;  Param: ip2Leb)     // 2B
   ,(valid: true;  Param: ip2Leb)     // 2C
   ,(valid: true;  Param: ip2Leb)     // 2D
   ,(valid: true;  Param: ip2Leb)     // 2E
   ,(valid: true;  Param: ip2Leb)     // 2F
   ,(valid: true;  Param: ip2Leb)     // 30
   ,(valid: true;  Param: ip2Leb)     // 31
   ,(valid: true;  Param: ip2Leb)     // 32
   ,(valid: true;  Param: ip2Leb)     // 33
   ,(valid: true;  Param: ip2Leb)     // 34 i64.load32_s
   ,(valid: true;  Param: ip2Leb)     // 35 i64.load32_u
   ,(valid: true;  Param: ip2Leb)     // 36
   ,(valid: true;  Param: ip2Leb)     // 37
   ,(valid: true;  Param: ip2Leb)     // 38
   ,(valid: true;  Param: ip2Leb)     // 39
   ,(valid: true;  Param: ip2Leb)     // 3A
   ,(valid: true;  Param: ip2Leb)     // 3B
   ,(valid: true;  Param: ip2Leb)     // 3C
   ,(valid: true;  Param: ip2Leb)     // 3D
   ,(valid: true;  Param: ip2Leb)     // 3E i64.store32
   ,(valid: true;  Param: ipNone)     // 3F
   ,(valid: true;  Param: ipNone)     // 40
   ,(valid: true;  Param: ipi32)      // 41
   ,(valid: true;  Param: ipi64)      // 42
   ,(valid: true;  Param: ipf32)      // 43
   ,(valid: true;  Param: ipf64)      // 44
   ,(valid: true;  Param: ipNone)     // 45
   ,(valid: true;  Param: ipNone)     // 46
   ,(valid: true;  Param: ipNone)     // 47
   ,(valid: true;  Param: ipNone)     // 48
   ,(valid: true;  Param: ipNone)     // 49
   ,(valid: true;  Param: ipNone)     // 4A
   ,(valid: true;  Param: ipNone)     // 4B
   ,(valid: true;  Param: ipNone)     // 4C
   ,(valid: true;  Param: ipNone)     // 4D
   ,(valid: true;  Param: ipNone)     // 4E
   ,(valid: true;  Param: ipNone)     // 4F
   ,(valid: true;  Param: ipNone)     // 50
   ,(valid: true;  Param: ipNone)     // 51
   ,(valid: true;  Param: ipNone)     // 52
   ,(valid: true;  Param: ipNone)     // 53
   ,(valid: true;  Param: ipNone)     // 54
   ,(valid: true;  Param: ipNone)     // 55
   ,(valid: true;  Param: ipNone)     // 56
   ,(valid: true;  Param: ipNone)     // 57
   ,(valid: true;  Param: ipNone)     // 58
   ,(valid: true;  Param: ipNone)     // 59
   ,(valid: true;  Param: ipNone)     // 5A
   ,(valid: true;  Param: ipNone)     // 5B
   ,(valid: true;  Param: ipNone)     // 5C
   ,(valid: true;  Param: ipNone)     // 5D
   ,(valid: true;  Param: ipNone)     // 5E
   ,(valid: true;  Param: ipNone)     // 5F
   ,(valid: true;  Param: ipNone)     // 60
   ,(valid: true;  Param: ipNone)     // 61
   ,(valid: true;  Param: ipNone)     // 62
   ,(valid: true;  Param: ipNone)     // 63
   ,(valid: true;  Param: ipNone)     // 64
   ,(valid: true;  Param: ipNone)     // 65
   ,(valid: true;  Param: ipNone)     // 66
   ,(valid: true;  Param: ipNone)     // 67
   ,(valid: true;  Param: ipNone)     // 68
   ,(valid: true;  Param: ipNone)     // 69
   ,(valid: true;  Param: ipNone)     // 6A
   ,(valid: true;  Param: ipNone)     // 6B
   ,(valid: true;  Param: ipNone)     // 6C
   ,(valid: true;  Param: ipNone)     // 6D
   ,(valid: true;  Param: ipNone)     // 6E
   ,(valid: true;  Param: ipNone)     // 6F
   ,(valid: true;  Param: ipNone)     // 70
   ,(valid: true;  Param: ipNone)     // 71
   ,(valid: true;  Param: ipNone)     // 72
   ,(valid: true;  Param: ipNone)     // 73
   ,(valid: true;  Param: ipNone)     // 74
   ,(valid: true;  Param: ipNone)     // 75
   ,(valid: true;  Param: ipNone)     // 76
   ,(valid: true;  Param: ipNone)     // 77
   ,(valid: true;  Param: ipNone)     // 78
   ,(valid: true;  Param: ipNone)     // 79
   ,(valid: true;  Param: ipNone)     // 7A
   ,(valid: true;  Param: ipNone)     // 7B
   ,(valid: true;  Param: ipNone)     // 7C
   ,(valid: true;  Param: ipNone)     // 7D
   ,(valid: true;  Param: ipNone)     // 7E
   ,(valid: true;  Param: ipNone)     // 7F
   ,(valid: true;  Param: ipNone)     // 80
   ,(valid: true;  Param: ipNone)     // 81
   ,(valid: true;  Param: ipNone)     // 82
   ,(valid: true;  Param: ipNone)     // 83
   ,(valid: true;  Param: ipNone)     // 84
   ,(valid: true;  Param: ipNone)     // 85
   ,(valid: true;  Param: ipNone)     // 86
   ,(valid: true;  Param: ipNone)     // 87
   ,(valid: true;  Param: ipNone)     // 88
   ,(valid: true;  Param: ipNone)     // 89
   ,(valid: true;  Param: ipNone)     // 8A
   ,(valid: true;  Param: ipNone)     // 8B
   ,(valid: true;  Param: ipNone)     // 8C
   ,(valid: true;  Param: ipNone)     // 8D
   ,(valid: true;  Param: ipNone)     // 8E
   ,(valid: true;  Param: ipNone)     // 8F
   ,(valid: true;  Param: ipNone)     // 90
   ,(valid: true;  Param: ipNone)     // 91
   ,(valid: true;  Param: ipNone)     // 92
   ,(valid: true;  Param: ipNone)     // 93
   ,(valid: true;  Param: ipNone)     // 94
   ,(valid: true;  Param: ipNone)     // 95
   ,(valid: true;  Param: ipNone)     // 96
   ,(valid: true;  Param: ipNone)     // 97
   ,(valid: true;  Param: ipNone)     // 98
   ,(valid: true;  Param: ipNone)     // 99
   ,(valid: true;  Param: ipNone)     // 9A
   ,(valid: true;  Param: ipNone)     // 9B
   ,(valid: true;  Param: ipNone)     // 9C
   ,(valid: true;  Param: ipNone)     // 9D
   ,(valid: true;  Param: ipNone)     // 9E
   ,(valid: true;  Param: ipNone)     // 9F
   ,(valid: true;  Param: ipNone)     // A0
   ,(valid: true;  Param: ipNone)     // A1
   ,(valid: true;  Param: ipNone)     // A2
   ,(valid: true;  Param: ipNone)     // A3
   ,(valid: true;  Param: ipNone)     // A4
   ,(valid: true;  Param: ipNone)     // A5
   ,(valid: true;  Param: ipNone)     // A6
   ,(valid: true;  Param: ipNone)     // A7
   ,(valid: true;  Param: ipNone)     // A8
   ,(valid: true;  Param: ipNone)     // A9
   ,(valid: true;  Param: ipNone)     // AA
   ,(valid: true;  Param: ipNone)     // AB
   ,(valid: true;  Param: ipNone)     // AC
   ,(valid: true;  Param: ipNone)     // AD
   ,(valid: true;  Param: ipNone)     // AE
   ,(valid: true;  Param: ipNone)     // AF
   ,(valid: true;  Param: ipNone)     // B0
   ,(valid: true;  Param: ipNone)     // B1
   ,(valid: true;  Param: ipNone)     // B2
   ,(valid: true;  Param: ipNone)     // B3
   ,(valid: true;  Param: ipNone)     // B4
   ,(valid: true;  Param: ipNone)     // B5
   ,(valid: true;  Param: ipNone)     // B6
   ,(valid: true;  Param: ipNone)     // B7
   ,(valid: true;  Param: ipNone)     // B8
   ,(valid: true;  Param: ipNone)     // B9
   ,(valid: true;  Param: ipNone)     // BA
   ,(valid: true;  Param: ipNone)     // BB
   ,(valid: true;  Param: ipNone)     // BC
   ,(valid: true;  Param: ipNone)     // BD
   ,(valid: true;  Param: ipNone)     // BE
   ,(valid: true;  Param: ipNone)     // BF
  );

function InstLen(st: TStream; endOfInst: Byte = INST_END): Integer;

implementation

function InstLen(st: TStream; endOfInst: Byte = INST_END): Integer;
var
  cd  : byte;
  ofs : int64;
  b   : byte;
  sz  : int64;
begin
  ofs := st.Position;
  try
    sz:=st.Size;
    while ofs < sz do begin
      cd := st.ReadByte;

      if (cd > MAX_INST) then begin
        Result:=-1; // invalid code
        Exit;
      end;
      if cd = endOfInst then break;

      case INST_FLAGS[cd].Param of
        ipLeb:
          ReadU(st);
        ip2Leb: begin
          ReadU(st);
          ReadU(st);
        end;
        ipTable: begin  // not implemented :(
          Result:=-2;
          Exit;
        end;
        ipi32: ReadS(st, 32);
        ipi64: ReadS(st, 64);
        ipf32: st.Position:=st.Position+4;
        ipf64: st.Position:=st.Position+8;
        ipResType: begin
          // it's a block. must go into recursion
          b := st.ReadByte; // reading type

          if (cd=INST_IF) and (b <> VALTYPE_NONE) then begin
            InstLen(st, INST_ELSE);
            InstLen(st, INST_END);
          end else
            InstLen(st, INST_END)
        end;
      end;
    end;
  finally
    Result := st.Position - ofs;
    st.Position:=ofs;
  end;
end;

end.
