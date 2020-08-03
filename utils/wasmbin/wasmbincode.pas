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

  INST_block       = $02;
  INST_loop        = $03;
  INST_unreachable = $00;
  INST_nop         = $01;
  INST_br          = $0C;
  INST_br_if          = $0D;
  INST_br_table       = $0E;
  INST_return         = $0F;
  INST_call           = $10;
  INST_call_indirect  = $11;

  //INST_if             = $04;
  //INST_else           = $05;
  //INST_end            = $0B;
  INST_drop           = $1A;
  INST_select         = $1B;
  INST_local_get      = $20;
  INST_local_set      = $21;
  INST_local_tee      = $22;
  INST_global_get     = $23;
  INST_global_set     = $24;

  INST_i32_load       = $28;
  INST_i64_load       = $29;
  INST_f32_load       = $2a;
  INST_f64_load       = $2b;
  INST_i32_load8_s    = $2c;
  INST_i32_load8_u    = $2d;
  INST_i32_load16_s   = $2e;
  INST_i32_load16_u   = $2f;
  INST_i64_load8_s    = $30;
  INST_i64_load8_u    = $31;
  INST_i64_load16_s   = $32;
  INST_i64_load16_u   = $33;
  INST_i64_load32_s   = $34;
  INST_i64_load32_u   = $35;
  INST_i32_store      = $36;
  INST_i64_store      = $37;
  INST_f32_store      = $38;
  INST_f64_store      = $39;
  INST_i32_store8     = $3a;
  INST_i32_store16    = $3b;
  INST_i64_store8     = $3c;
  INST_i64_store16    = $3d;
  INST_i64_store32    = $3e;
  INST_memory_size    = $3f;
  INST_memory_grow    = $40;
  INST_i32_const      = $41;
  INST_i64_const      = $42;
  INST_f32_const      = $43;
  INST_f64_const      = $44;

  INST_i32_clz        = $67;
  INST_i32_ctz        = $68;
  INST_i32_popcnt     = $69;
  INST_i32_add        = $6a;
  INST_i32_sub        = $6b;
  INST_i32_mul        = $6c;
  INST_i32_div_s      = $6d;
  INST_i32_div_u      = $6e;
  INST_i32_rem_s      = $6f;
  INST_i32_rem_u      = $70;
  INST_i32_and        = $71;
  INST_i32_or         = $72;
  INST_i32_xor        = $73;
  INST_i32_shl        = $74;
  INST_i32_shr_s      = $75;
  INST_i32_shr_u      = $76;
  INST_i32_rotl       = $77;
  INST_i32_rotr       = $78;
  INST_i64_clz        = $79;
  INST_i64_ctz        = $7a;
  INST_i64_popcnt     = $7b;
  INST_i64_add        = $7c;
  INST_i64_sub        = $7d;
  INST_i64_mul        = $7e;
  INST_i64_div_s      = $7f;
  INST_i64_div_u      = $80;
  INST_i64_rem_s      = $81;
  INST_i64_rem_u      = $82;
  INST_i64_and        = $83;
  INST_i64_or         = $84;
  INST_i64_xor        = $85;
  INST_i64_shl        = $86;
  INST_i64_shr_s      = $87;
  INST_i64_shr_u      = $88;
  INST_i64_rotl       = $89;
  INST_i64_rotr       = $8a;
  INST_f32_abs        = $8b;
  INST_f32_neg        = $8c;
  INST_f32_ceil       = $8d;
  INST_f32_floor      = $8e;
  INST_f32_trunc      = $8f;
  INST_f32_nearest    = $90;
  INST_f32_sqrt       = $91;
  INST_f32_add        = $92;
  INST_f32_sub        = $93;
  INST_f32_mul        = $94;
  INST_f32_div        = $95;
  INST_f32_min        = $96;
  INST_f32_max        = $97;
  INST_f32_copysign   = $98;
  INST_f64_abs        = $99;
  INST_f64_neg        = $99;
  INST_f64_ceil       = $99;
  INST_f64_floor      = $9a;
  INST_f64_trunc      = $9b;
  INST_f64_nearest    = $9c;
  INST_f64_sqrt       = $9f;
  INST_f64_add        = $a0;
  INST_f64_sub        = $a1;
  INST_f64_mul        = $a2;
  INST_f64_div        = $a3;
  INST_f64_min        = $a4;
  INST_f64_max        = $a5;
  INST_f64_copysign   = $a6;

  INST_i32_eqz        = $45;
  INST_i32_eq         = $46;
  INST_i32_ne         = $47;
  INST_i32_lt_s       = $48;
  INST_i32_lt_u       = $49;
  INST_i32_gt_s       = $4a;
  INST_i32_gt_u       = $4b;
  INST_i32_le_s       = $4c;
  INST_i32_le_u       = $4d;
  INST_i32_ge_s       = $4e;
  INST_i32_ge_u       = $4f;
  INST_i64_eqz        = $50;
  INST_i64_eq         = $51;
  INST_i64_ne         = $52;
  INST_i64_lt_s       = $53;
  INST_i64_lt_u       = $54;
  INST_i64_gt_s       = $55;
  INST_i64_gt_u       = $56;
  INST_i64_le_s       = $57;
  INST_i64_le_u       = $58;
  INST_i64_ge_s       = $59;
  INST_i64_ge_u       = $5a;
  INST_f32_eq         = $5b;
  INST_f32_ne         = $5c;
  INST_f32_lt         = $5d;
  INST_f32_gt         = $5e;
  INST_f32_le         = $5f;
  INST_f32_ge         = $60;
  INST_f64_eq         = $61;
  INST_f64_ne         = $62;
  INST_f64_lt         = $63;
  INST_f64_gt         = $64;
  INST_f64_le         = $65;
  INST_f64_ge         = $66;

  INST_i32_wrap_i64        = $a7;
  INST_i32_trunc_f32_s     = $a8;
  INST_i32_trunc_f32_u     = $a9;
  INST_i32_trunc_f64_s     = $aa;
  INST_i32_trunc_f64_u     = $ab;
  INST_i64_extend_i32_s    = $ac;
  INST_i64_extend_i32_u    = $ad;
  INST_i64_trunc_f32_s     = $ae;
  INST_i64_trunc_f32_u     = $af;
  INST_i64_trunc_f64_s     = $b0;
  INST_i64_trunc_f64_u     = $b1;
  INST_f32_convert_i32_s   = $b2;
  INST_f32_convert_i32_u   = $b3;
  INST_f32_convert_i64_s   = $b4;
  INST_f32_convert_i64_u   = $b5;
  INST_f32_demote_f64      = $b6;
  INST_f64_convert_i32_s   = $b7;
  INST_f64_convert_i32_u   = $b8;
  INST_f64_convert_i64_s   = $b9;
  INST_f64_convert_i64_u   = $ba;
  INST_f64_promote_f32     = $bb;
  INST_i32_reinterpret_f32 = $bc;
  INST_i64_reinterpret_f64 = $bd;
  INST_f32_reinterpret_i32 = $be;
  INST_f64_reinterpret_i64 = $bf;
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
