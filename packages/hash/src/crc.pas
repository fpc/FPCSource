unit crc;

{$mode objfpc}

{
  crc32.c -- compute the CRC-32 of a data stream
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt


  crc64.c -- compute the CRC-64 of a data stream
  By David T. Jones (dtj@cs.ucl.ac.uk)  - September 28th 2002

  Pascal tranlastion
  Copyright (C) 2009 by Ivo Steinmann
}

{.$DEFINE DYNAMIC_CRC_TABLE}

interface


(******************************************************************************
 * CRC32
 ******************************************************************************)

function crc32(crc: cardinal; buf: Pbyte; len: cardinal): cardinal;

{  Update a running crc with the bytes buf[0..len-1] and return the updated
   crc. If buf is NULL, this function returns the required initial value
   for the crc. Pre- and post-conditioning (one's complement) is performed
   within this function so it shouldn't be done by the application.
   Usage example:

    var
      crc : cardinal;
    begin
      crc := crc32(0, nil, 0);

      while (read_buffer(buffer, length) <> EOF) do
        crc := crc32(crc, buffer, length);

      if (crc <> original_crc) then error();
    end;

}

function get_crc32_table: Pcardinal;  { can be used by asm versions of crc32() }



(******************************************************************************
 * CRC64
 ******************************************************************************)

function crc64(crc: qword; buf: Pbyte; len: cardinal) : qword;

function get_crc64_table: PQword;  { can be used by asm versions of crc64() }

implementation


(******************************************************************************
 * CRC32
 ******************************************************************************)

const
  CRC32_XINIT = $FFFFFFFF;
  CRC32_XOROT = $FFFFFFFF;


{$IFDEF DYNAMIC_CRC_TABLE}

{local}
const
  crc32_table_empty : boolean = TRUE;
{local}
var
  crc32_table : array[Byte] of Longword;


{
  Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
  x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.

  Polynomials over GF(2) are represented in binary, one bit per coefficient,
  with the lowest powers in the most significant bit.  Then adding polynomials
  is just exclusive-or, and multiplying a polynomial by x is a right shift by
  one.  If we call the above polynomial p, and represent a byte as the
  polynomial q, also with the lowest power in the most significant bit (so the
  byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
  where a mod b means the remainder after dividing a by b.

  This calculation is done using the shift-register method of multiplying and
  taking the remainder.  The register is initialized to zero, and for each
  incoming bit, x^32 is added mod p to the register if the bit is a one (where
  x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
  x (which is shifting right by one and adding x^32 mod p if the bit shifted
  out is a one).  We start with the highest power (least significant bit) of
  q and repeat for all eight bits of q.

  The table is simply the CRC of all possible eight bit values.  This is all
  the information needed to generate CRC's on data a byte at a time for all
  combinations of CRC register values and incoming bytes.
}
{local}
procedure make_crc32_table;
var
 c    : cardinal;
 n,k  : integer;
 poly : cardinal; { polynomial exclusive-or pattern }

const
 { terms of polynomial defining this crc (except x^32): }
 p: array [0..13] of Byte = (0,1,2,4,5,7,8,10,11,12,16,22,23,26);

begin
  { make exclusive-or pattern from polynomial ($EDB88320) }
  poly := longint(0);
  for n := 0 to (sizeof(p) div sizeof(Byte))-1 do
    poly := poly or (longint(1) shl (31 - p[n]));

  for n := 0 to 255 do
  begin
    c := cardinal(n);
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := poly xor (c shr 1)
      else
        c := (c shr 1);
    end;
    crc32_table[n] := c;
  end;
  crc32_table_empty := FALSE;
end;

{$ELSE}

{ ========================================================================
  Table of CRC-32's of all single-byte values (made by make_crc32_table) }

{local}
const
  crc32_table : array[Byte] of cardinal = (
  $00000000, $77073096, $ee0e612c, $990951ba, $076dc419,
  $706af48f, $e963a535, $9e6495a3, $0edb8832, $79dcb8a4,
  $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07,
  $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de,
  $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7, $136c9856,
  $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9,
  $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
  $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
  $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3,
  $45df5c75, $dcd60dcf, $abd13d59, $26d930ac, $51de003a,
  $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599,
  $b8bda50f, $2802b89e, $5f058808, $c60cd9b2, $b10be924,
  $2f6f7c87, $58684c11, $c1611dab, $b6662d3d, $76dc4190,
  $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
  $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e,
  $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
  $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed,
  $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950,
  $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3,
  $fbd44c65, $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
  $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
  $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5,
  $aa0a4c5f, $dd0d7cc9, $5005713c, $270241aa, $be0b1010,
  $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
  $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17,
  $2eb40d81, $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6,
  $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615,
  $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344,
  $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb,
  $196c3671, $6e6b06e7, $fed41b76, $89d32be0, $10da7a5a,
  $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
  $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1,
  $a6bc5767, $3fb506dd, $48b2364b, $d80d2bda, $af0a1b4c,
  $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
  $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
  $cc0c7795, $bb0b4703, $220216b9, $5505262f, $c5ba3bbe,
  $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31,
  $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c,
  $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
  $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b,
  $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
  $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1,
  $18b74777, $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
  $8f659eff, $f862ae69, $616bffd3, $166ccf45, $a00ae278,
  $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7,
  $4969474d, $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66,
  $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
  $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
  $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8,
  $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b,
  $2d02ef8d);

{$ENDIF}

{ =========================================================================
  This function can be used by asm versions of crc32() }

function get_crc32_table : {const} Pcardinal;
begin
{$ifdef DYNAMIC_CRC_TABLE}
  if (crc32_table_empty) then
    make_crc32_table;
{$endif}
  get_crc32_table :=  {const} Pcardinal(@crc32_table);
end;

{ ========================================================================= }

function crc32 (crc : cardinal; buf : Pbyte; len : cardinal): cardinal;
begin
  if buf = nil then
    exit(0{CRC32_XINIT});

{$IFDEF DYNAMIC_CRC_TABLE}
  if crc32_table_empty then
    make_crc32_table;
{$ENDIF}

  while (len >= 8) do
  begin
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    dec(len, 8);
  end;

  if (len <> 0) then
  repeat
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    dec(len);
  until (len = 0);

  result := crc; //xor CRC32_XOROT;
end;



(******************************************************************************
 * CRC64
 ******************************************************************************)

const
  POLY64REV  = QWord($95AC9329AC4BC9B5);
  INITIALCRC = QWord($FFFFFFFFFFFFFFFF);

{$IFDEF DYNAMIC_CRC_TABLE}

{local}
const
  crc64_table_empty : boolean = TRUE;
{local}
var
  crc64_table : array[Byte] of QWord;

{local}
procedure make_crc64_table;
var
  i,j: Integer;
  part: QWord;
begin
  for i := 0 to 255 do
  begin
    part := i;
    for j := 0 to 7 do
    begin
      if part and $1 <> 0 then
        part := (part shr 1) xor POLY64REV
      else
        part := part shr 1;
    end;
    crc64_table[i] := part;
  end;
  crc64_table_empty := FALSE;
end;

{$ELSE}

{ ========================================================================
  Table of CRC-64's of all single-byte values (made by make_crc64_table) }

{local}
const
  crc64_table : array[Byte] of QWord = (
    $0000000000000000,$7AD870C830358979,$F5B0E190606B12F2,$8F689158505E9B8B,$C038E5739841B68F,$BAE095BBA8743FF6,
    $358804E3F82AA47D,$4F50742BC81F2D04,$AB28ECB46814FE75,$D1F09C7C5821770C,$5E980D24087FEC87,$24407DEC384A65FE,
    $6B1009C7F05548FA,$11C8790FC060C183,$9EA0E857903E5A08,$E478989FA00BD371,$7D08FF3B88BE6F81,$07D08FF3B88BE6F8,
    $88B81EABE8D57D73,$F2606E63D8E0F40A,$BD301A4810FFD90E,$C7E86A8020CA5077,$4880FBD87094CBFC,$32588B1040A14285,
    $D620138FE0AA91F4,$ACF86347D09F188D,$2390F21F80C18306,$594882D7B0F40A7F,$1618F6FC78EB277B,$6CC0863448DEAE02,
    $E3A8176C18803589,$997067A428B5BCF0,$FA11FE77117CDF02,$80C98EBF2149567B,$0FA11FE77117CDF0,$75796F2F41224489,
    $3A291B04893D698D,$40F16BCCB908E0F4,$CF99FA94E9567B7F,$B5418A5CD963F206,$513912C379682177,$2BE1620B495DA80E,
    $A489F35319033385,$DE51839B2936BAFC,$9101F7B0E12997F8,$EBD98778D11C1E81,$64B116208142850A,$1E6966E8B1770C73,
    $8719014C99C2B083,$FDC17184A9F739FA,$72A9E0DCF9A9A271,$08719014C99C2B08,$4721E43F0183060C,$3DF994F731B68F75,
    $B29105AF61E814FE,$C849756751DD9D87,$2C31EDF8F1D64EF6,$56E99D30C1E3C78F,$D9810C6891BD5C04,$A3597CA0A188D57D,
    $EC09088B6997F879,$96D1784359A27100,$19B9E91B09FCEA8B,$636199D339C963F2,$DF7ADABD7A6E2D6F,$A5A2AA754A5BA416,
    $2ACA3B2D1A053F9D,$50124BE52A30B6E4,$1F423FCEE22F9BE0,$659A4F06D21A1299,$EAF2DE5E82448912,$902AAE96B271006B,
    $74523609127AD31A,$0E8A46C1224F5A63,$81E2D7997211C1E8,$FB3AA75142244891,$B46AD37A8A3B6595,$CEB2A3B2BA0EECEC,
    $41DA32EAEA507767,$3B024222DA65FE1E,$A2722586F2D042EE,$D8AA554EC2E5CB97,$57C2C41692BB501C,$2D1AB4DEA28ED965,
    $624AC0F56A91F461,$1892B03D5AA47D18,$97FA21650AFAE693,$ED2251AD3ACF6FEA,$095AC9329AC4BC9B,$7382B9FAAAF135E2,
    $FCEA28A2FAAFAE69,$8632586ACA9A2710,$C9622C4102850A14,$B3BA5C8932B0836D,$3CD2CDD162EE18E6,$460ABD1952DB919F,
    $256B24CA6B12F26D,$5FB354025B277B14,$D0DBC55A0B79E09F,$AA03B5923B4C69E6,$E553C1B9F35344E2,$9F8BB171C366CD9B,
    $10E3202993385610,$6A3B50E1A30DDF69,$8E43C87E03060C18,$F49BB8B633338561,$7BF329EE636D1EEA,$012B592653589793,
    $4E7B2D0D9B47BA97,$34A35DC5AB7233EE,$BBCBCC9DFB2CA865,$C113BC55CB19211C,$5863DBF1E3AC9DEC,$22BBAB39D3991495,
    $ADD33A6183C78F1E,$D70B4AA9B3F20667,$985B3E827BED2B63,$E2834E4A4BD8A21A,$6DEBDF121B863991,$1733AFDA2BB3B0E8,
    $F34B37458BB86399,$8993478DBB8DEAE0,$06FBD6D5EBD3716B,$7C23A61DDBE6F812,$3373D23613F9D516,$49ABA2FE23CC5C6F,
    $C6C333A67392C7E4,$BC1B436E43A74E9D,$95AC9329AC4BC9B5,$EF74E3E19C7E40CC,$601C72B9CC20DB47,$1AC40271FC15523E,
    $5594765A340A7F3A,$2F4C0692043FF643,$A02497CA54616DC8,$DAFCE7026454E4B1,$3E847F9DC45F37C0,$445C0F55F46ABEB9,
    $CB349E0DA4342532,$B1ECEEC59401AC4B,$FEBC9AEE5C1E814F,$8464EA266C2B0836,$0B0C7B7E3C7593BD,$71D40BB60C401AC4,
    $E8A46C1224F5A634,$927C1CDA14C02F4D,$1D148D82449EB4C6,$67CCFD4A74AB3DBF,$289C8961BCB410BB,$5244F9A98C8199C2,
    $DD2C68F1DCDF0249,$A7F41839ECEA8B30,$438C80A64CE15841,$3954F06E7CD4D138,$B63C61362C8A4AB3,$CCE411FE1CBFC3CA,
    $83B465D5D4A0EECE,$F96C151DE49567B7,$76048445B4CBFC3C,$0CDCF48D84FE7545,$6FBD6D5EBD3716B7,$15651D968D029FCE,
    $9A0D8CCEDD5C0445,$E0D5FC06ED698D3C,$AF85882D2576A038,$D55DF8E515432941,$5A3569BD451DB2CA,$20ED197575283BB3,
    $C49581EAD523E8C2,$BE4DF122E51661BB,$3125607AB548FA30,$4BFD10B2857D7349,$04AD64994D625E4D,$7E7514517D57D734,
    $F11D85092D094CBF,$8BC5F5C11D3CC5C6,$12B5926535897936,$686DE2AD05BCF04F,$E70573F555E26BC4,$9DDD033D65D7E2BD,
    $D28D7716ADC8CFB9,$A85507DE9DFD46C0,$273D9686CDA3DD4B,$5DE5E64EFD965432,$B99D7ED15D9D8743,$C3450E196DA80E3A,
    $4C2D9F413DF695B1,$36F5EF890DC31CC8,$79A59BA2C5DC31CC,$037DEB6AF5E9B8B5,$8C157A32A5B7233E,$F6CD0AFA9582AA47,
    $4AD64994D625E4DA,$300E395CE6106DA3,$BF66A804B64EF628,$C5BED8CC867B7F51,$8AEEACE74E645255,$F036DC2F7E51DB2C,
    $7F5E4D772E0F40A7,$05863DBF1E3AC9DE,$E1FEA520BE311AAF,$9B26D5E88E0493D6,$144E44B0DE5A085D,$6E963478EE6F8124,
    $21C640532670AC20,$5B1E309B16452559,$D476A1C3461BBED2,$AEAED10B762E37AB,$37DEB6AF5E9B8B5B,$4D06C6676EAE0222,
    $C26E573F3EF099A9,$B8B627F70EC510D0,$F7E653DCC6DA3DD4,$8D3E2314F6EFB4AD,$0256B24CA6B12F26,$788EC2849684A65F,
    $9CF65A1B368F752E,$E62E2AD306BAFC57,$6946BB8B56E467DC,$139ECB4366D1EEA5,$5CCEBF68AECEC3A1,$2616CFA09EFB4AD8,
    $A97E5EF8CEA5D153,$D3A62E30FE90582A,$B0C7B7E3C7593BD8,$CA1FC72BF76CB2A1,$45775673A732292A,$3FAF26BB9707A053,
    $70FF52905F188D57,$0A2722586F2D042E,$854FB3003F739FA5,$FF97C3C80F4616DC,$1BEF5B57AF4DC5AD,$61372B9F9F784CD4,
    $EE5FBAC7CF26D75F,$9487CA0FFF135E26,$DBD7BE24370C7322,$A10FCEEC0739FA5B,$2E675FB4576761D0,$54BF2F7C6752E8A9,
    $CDCF48D84FE75459,$B71738107FD2DD20,$387FA9482F8C46AB,$42A7D9801FB9CFD2,$0DF7ADABD7A6E2D6,$772FDD63E7936BAF,
    $F8474C3BB7CDF024,$829F3CF387F8795D,$66E7A46C27F3AA2C,$1C3FD4A417C62355,$935745FC4798B8DE,$E98F353477AD31A7,
    $A6DF411FBFB21CA3,$DC0731D78F8795DA,$536FA08FDFD90E51,$29B7D047EFEC8728
  );

{$ENDIF}

function get_crc64_table : {const} PQWord;
begin
{$ifdef DYNAMIC_CRC_TABLE}
  if (crc64_table_empty) then
    make_crc64_table;
{$endif}
  get_crc64_table :=  {const} PQWord(@crc64_table);
end;

{void crc64(char *seq, char *res)

    int i, j, low, high;
    unsigned long long crc = INITIALCRC, part;
    static int init = 0;
    static unsigned long long CRCTable[256];


    while (*seq)
	crc = CRCTable[(crc ^ *seq++) & 0xff] ^ (crc >> 8);

    /*
     The output is done in two parts to avoid problems with
     architecture-dependent word order
     */
    low = crc & 0xffffffff;
    high = (crc >> 32) & 0xffffffff;
    sprintf (res, "%08X%08X", high, low);

    return;
}
function crc64(crc: qword; buf: Pbyte; len: cardinal): qword;
begin
  if (buf = nil) then
    exit(INITIALCRC);

{$IFDEF DYNAMIC_CRC_TABLE}
  if crc64_table_empty then
    make_crc64_table;
{$ENDIF}

  while (len >= 8) do
  begin
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    dec(len, 8);
  end;

  if (len <> 0) then
  repeat
    crc := crc64_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    dec(len);
  until (len = 0);

  result := crc;
end;


end.
