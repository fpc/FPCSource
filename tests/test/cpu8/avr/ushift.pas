unit ushift;

interface

procedure testSHL8;
procedure testSHR8;
procedure testSHL16;
procedure testSHR16;
procedure testSHL32;
procedure testSHR32;
procedure testSHL64;
procedure testSHR64;

implementation

var
  b, b1: byte;
  w, w1: word;
  d, d1: dword;
  q, q1: qword;

procedure testSHL8;
const
  data: array of byte = (
    $C3, $86, $0C, $18,
    $30, $60, $C0, $80);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if b1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(b, 2), ' shl ', i, ' = $', HexStr(b1, 2), ', should be $', HexStr(data[i], 2));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  b := data[0];
  writeln('Testing SHL8:');

  b1 := b shl 1; status(1);
  b1 := b shl 2; status(2);
  b1 := b shl 3; status(3);
  b1 := b shl 4; status(4);
  b1 := b shl 5; status(5);
  b1 := b shl 6; status(6);
  b1 := b shl 7; status(7);
end;

procedure testSHR8;
const
  data: array of byte = (
    $C3, $61, $30, $18,
    $0C, $06, $03, $01);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if b1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(b, 2), ' shr ', i, ' = $', HexStr(b1, 2), ', should be $', HexStr(data[i], 2));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  b := data[0];
  writeln('Testing SHR8:');

  b1 := b shr 1; status(1);
  b1 := b shr 2; status(2);
  b1 := b shr 3; status(3);
  b1 := b shr 4; status(4);
  b1 := b shr 5; status(5);
  b1 := b shr 6; status(6);
  b1 := b shr 7; status(7);
end;

procedure testSHL16;
const
  data: array of word = (
    $A54B, $4A96, $952C, $2A58,
    $54B0, $A960, $52C0, $A580,
    $4B00, $9600, $2C00, $5800,
    $B000, $6000, $C000, $8000);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if w1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(w, 4), ' shl ', i, ' = $', HexStr(w1, 4), ', should be $', HexStr(data[i], 4));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  w := data[0];
  writeln('Testing SHL16:');

  w1 := w shl  1; status( 1);
  w1 := w shl  2; status( 2);
  w1 := w shl  3; status( 3);
  w1 := w shl  4; status( 4);
  w1 := w shl  5; status( 5);
  w1 := w shl  6; status( 6);
  w1 := w shl  7; status( 7);
  w1 := w shl  8; status( 8);
  w1 := w shl  9; status( 9);
  w1 := w shl 10; status(10);
  w1 := w shl 11; status(11);
  w1 := w shl 12; status(12);
  w1 := w shl 13; status(13);
  w1 := w shl 14; status(14);
  w1 := w shl 15; status(15);
end;

procedure testSHR16;
const
  data: array of word = (
    $A54B, $52A5, $2952, $14A9,
    $0A54, $052A, $0295, $014A,
    $00A5, $0052, $0029, $0014,
    $000A, $0005, $0002, $0001);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if w1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(w, 4), ' shr ', i, ' = $', HexStr(w1, 4), ', should be $', HexStr(data[i], 4));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  w := data[0];
  writeln('Testing SHR16:');

  w1 := w shr  1; status( 1);
  w1 := w shr  2; status( 2);
  w1 := w shr  3; status( 3);
  w1 := w shr  4; status( 4);
  w1 := w shr  5; status( 5);
  w1 := w shr  6; status( 6);
  w1 := w shr  7; status( 7);
  w1 := w shr  8; status( 8);
  w1 := w shr  9; status( 9);
  w1 := w shr 10; status(10);
  w1 := w shr 11; status(11);
  w1 := w shr 12; status(12);
  w1 := w shr 13; status(13);
  w1 := w shr 14; status(14);
  w1 := w shr 15; status(15);
end;

procedure testSHL32;
const
  data: array of dword = (
    $ABCD4321, $579A8642, $AF350C84, $5E6A1908,
    $BCD43210, $79A86420, $F350C840, $E6A19080,
    $CD432100, $9A864200, $350C8400, $6A190800,
    $D4321000, $A8642000, $50C84000, $A1908000,
    $43210000, $86420000, $0C840000, $19080000,
    $32100000, $64200000, $C8400000, $90800000,
    $21000000, $42000000, $84000000, $08000000,
    $10000000, $20000000, $40000000, $80000000);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if d1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(d, 8), ' shl ', i, ' = $', HexStr(d1, 8), ', should be $', HexStr(data[i], 8));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  d := data[0];
  writeln('Testing SHL32:');

  d1 := d shl  1; status( 1);
  d1 := d shl  2; status( 2);
  d1 := d shl  3; status( 3);
  d1 := d shl  4; status( 4);
  d1 := d shl  5; status( 5);
  d1 := d shl  6; status( 6);
  d1 := d shl  7; status( 7);
  d1 := d shl  8; status( 8);
  d1 := d shl  9; status( 9);
  d1 := d shl 10; status(10);
  d1 := d shl 11; status(11);
  d1 := d shl 12; status(12);
  d1 := d shl 13; status(13);
  d1 := d shl 14; status(14);
  d1 := d shl 15; status(15);
  d1 := d shl 16; status(16);
  d1 := d shl 17; status(17);
  d1 := d shl 18; status(18);
  d1 := d shl 19; status(19);
  d1 := d shl 20; status(20);
  d1 := d shl 21; status(21);
  d1 := d shl 22; status(22);
  d1 := d shl 23; status(23);
  d1 := d shl 24; status(24);
  d1 := d shl 25; status(25);
  d1 := d shl 26; status(26);
  d1 := d shl 27; status(27);
  d1 := d shl 28; status(28);
  d1 := d shl 29; status(29);
  d1 := d shl 30; status(30);
  d1 := d shl 31; status(31);
end;

procedure testSHR32;
const
  data: array of dword = (
    $ABCD1234, $55E6891A, $2AF3448D, $1579A246,
    $0ABCD123, $055E6891, $02AF3448, $01579A24,
    $00ABCD12, $0055E689, $002AF344, $001579A2,
    $000ABCD1, $00055E68, $0002AF34, $0001579A,
    $0000ABCD, $000055E6, $00002AF3, $00001579,
    $00000ABC, $0000055E, $000002AF, $00000157,
    $000000AB, $00000055, $0000002A, $00000015,
    $0000000A, $00000005, $00000002, $00000001);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if d1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(d, 8), ' shr ', i, ' = $', HexStr(d1, 8), ', should be $', HexStr(data[i], 8));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  d := data[0];
  writeln('Testing SHR32:');

  d1 := d shr  1; status( 1);
  d1 := d shr  2; status( 2);
  d1 := d shr  3; status( 3);
  d1 := d shr  4; status( 4);
  d1 := d shr  5; status( 5);
  d1 := d shr  6; status( 6);
  d1 := d shr  7; status( 7);
  d1 := d shr  8; status( 8);
  d1 := d shr  9; status( 9);
  d1 := d shr 10; status(10);
  d1 := d shr 11; status(11);
  d1 := d shr 12; status(12);
  d1 := d shr 13; status(13);
  d1 := d shr 14; status(14);
  d1 := d shr 15; status(15);
  d1 := d shr 16; status(16);
  d1 := d shr 17; status(17);
  d1 := d shr 18; status(18);
  d1 := d shr 19; status(19);
  d1 := d shr 20; status(20);
  d1 := d shr 21; status(21);
  d1 := d shr 22; status(22);
  d1 := d shr 23; status(23);
  d1 := d shr 24; status(24);
  d1 := d shr 25; status(25);
  d1 := d shr 26; status(26);
  d1 := d shr 27; status(27);
  d1 := d shr 28; status(28);
  d1 := d shr 29; status(29);
  d1 := d shr 30; status(30);
  d1 := d shr 31; status(31);
end;

procedure testSHL64;
const
  data: array of qword = (
    $1234567890ABCDEF, $2468ACF121579BDE, $48D159E242AF37BC, $91A2B3C4855E6F78,
    $234567890ABCDEF0, $468ACF121579BDE0, $8D159E242AF37BC0, $1A2B3C4855E6F780,
    $34567890ABCDEF00, $68ACF121579BDE00, $D159E242AF37BC00, $A2B3C4855E6F7800,
    $4567890ABCDEF000, $8ACF121579BDE000, $159E242AF37BC000, $2B3C4855E6F78000,
    $567890ABCDEF0000, $ACF121579BDE0000, $59E242AF37BC0000, $B3C4855E6F780000,
    $67890ABCDEF00000, $CF121579BDE00000, $9E242AF37BC00000, $3C4855E6F7800000,
    $7890ABCDEF000000, $F121579BDE000000, $E242AF37BC000000, $C4855E6F78000000,
    $890ABCDEF0000000, $121579BDE0000000, $242AF37BC0000000, $4855E6F780000000,
    $90ABCDEF00000000, $21579BDE00000000, $42AF37BC00000000, $855E6F7800000000,
    $0ABCDEF000000000, $1579BDE000000000, $2AF37BC000000000, $55E6F78000000000,
    $ABCDEF0000000000, $579BDE0000000000, $AF37BC0000000000, $5E6F780000000000,
    $BCDEF00000000000, $79BDE00000000000, $F37BC00000000000, $E6F7800000000000,
    $CDEF000000000000, $9BDE000000000000, $37BC000000000000, $6F78000000000000,
    $DEF0000000000000, $BDE0000000000000, $7BC0000000000000, $F780000000000000,
    $EF00000000000000, $DE00000000000000, $BC00000000000000, $7800000000000000,
    $F000000000000000, $E000000000000000, $C000000000000000, $8000000000000000);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if q1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(q, 16), ' shl ', i, ' = $', HexStr(q1, 16), ', should be $', HexStr(data[i], 16));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  q := data[0];
  writeln('Testing SHL64:');

  q1 := q shl  1; status( 1);
  q1 := q shl  2; status( 2);
  q1 := q shl  3; status( 3);
  q1 := q shl  4; status( 4);
  q1 := q shl  5; status( 5);
  q1 := q shl  6; status( 6);
  q1 := q shl  7; status( 7);
  q1 := q shl  8; status( 8);
  q1 := q shl  9; status( 9);
  q1 := q shl 10; status(10);
  q1 := q shl 11; status(11);
  q1 := q shl 12; status(12);
  q1 := q shl 13; status(13);
  q1 := q shl 14; status(14);
  q1 := q shl 15; status(15);
  q1 := q shl 16; status(16);
  q1 := q shl 17; status(17);
  q1 := q shl 18; status(18);
  q1 := q shl 19; status(19);
  q1 := q shl 20; status(20);
  q1 := q shl 21; status(21);
  q1 := q shl 22; status(22);
  q1 := q shl 23; status(23);
  q1 := q shl 24; status(24);
  q1 := q shl 25; status(25);
  q1 := q shl 26; status(26);
  q1 := q shl 27; status(27);
  q1 := q shl 28; status(28);
  q1 := q shl 29; status(29);
  q1 := q shl 30; status(30);
  q1 := q shl 31; status(31);
  q1 := q shl 32; status(32);
  q1 := q shl 33; status(33);
  q1 := q shl 34; status(34);
  q1 := q shl 35; status(35);
  q1 := q shl 36; status(36);
  q1 := q shl 37; status(37);
  q1 := q shl 38; status(38);
  q1 := q shl 39; status(39);
  q1 := q shl 40; status(40);
  q1 := q shl 41; status(41);
  q1 := q shl 42; status(42);
  q1 := q shl 43; status(43);
  q1 := q shl 44; status(44);
  q1 := q shl 45; status(45);
  q1 := q shl 46; status(46);
  q1 := q shl 47; status(47);
  q1 := q shl 48; status(48);
  q1 := q shl 49; status(49);
  q1 := q shl 50; status(50);
  q1 := q shl 51; status(51);
  q1 := q shl 52; status(52);
  q1 := q shl 53; status(53);
  q1 := q shl 54; status(54);
  q1 := q shl 55; status(55);
  q1 := q shl 56; status(56);
  q1 := q shl 57; status(57);
  q1 := q shl 58; status(58);
  q1 := q shl 59; status(59);
  q1 := q shl 60; status(60);
  q1 := q shl 61; status(61);
  q1 := q shl 62; status(62);
  q1 := q shl 63; status(63);
end;

procedure testSHR64;
const
  data: array of qword = (
    $1234567890ABCDEF, $091A2B3C4855E6F7, $048D159E242AF37B, $02468ACF121579BD,
    $01234567890ABCDE, $0091A2B3C4855E6F, $0048D159E242AF37, $002468ACF121579B,
    $001234567890ABCD, $00091A2B3C4855E6, $00048D159E242AF3, $0002468ACF121579,
    $0001234567890ABC, $000091A2B3C4855E, $000048D159E242AF, $00002468ACF12157,
    $00001234567890AB, $0000091A2B3C4855, $0000048D159E242A, $000002468ACF1215,
    $000001234567890A, $00000091A2B3C485, $00000048D159E242, $0000002468ACF121,
    $0000001234567890, $000000091A2B3C48, $000000048D159E24, $00000002468ACF12,
    $0000000123456789, $0000000091A2B3C4, $0000000048D159E2, $000000002468ACF1,
    $0000000012345678, $00000000091A2B3C, $00000000048D159E, $0000000002468ACF,
    $0000000001234567, $000000000091A2B3, $000000000048D159, $00000000002468AC,
    $0000000000123456, $0000000000091A2B, $0000000000048D15, $000000000002468A,
    $0000000000012345, $00000000000091A2, $00000000000048D1, $0000000000002468,
    $0000000000001234, $000000000000091A, $000000000000048D, $0000000000000246,
    $0000000000000123, $0000000000000091, $0000000000000048, $0000000000000024,
    $0000000000000012, $0000000000000009, $0000000000000004, $0000000000000002,
    $0000000000000001, $0000000000000000, $0000000000000000, $0000000000000000);

  procedure status(const i: byte);
  begin
    write(i:2, ' - ');
    if q1 <> data[i] then
    begin
      writeln('Fail');
      writeln('$',HexStr(q, 16), ' shr ', i, ' = $', HexStr(q1, 16), ', should be $', HexStr(data[i], 16));
      Halt(i);
    end;
    writeln('Pass');
  end;

begin
  q := data[0];
  writeln('Testing SHR64:');

  q1 := q shr  1; status( 1);
  q1 := q shr  2; status( 2);
  q1 := q shr  3; status( 3);
  q1 := q shr  4; status( 4);
  q1 := q shr  5; status( 5);
  q1 := q shr  6; status( 6);
  q1 := q shr  7; status( 7);
  q1 := q shr  8; status( 8);
  q1 := q shr  9; status( 9);
  q1 := q shr 10; status(10);
  q1 := q shr 11; status(11);
  q1 := q shr 12; status(12);
  q1 := q shr 13; status(13);
  q1 := q shr 14; status(14);
  q1 := q shr 15; status(15);
  q1 := q shr 16; status(16);
  q1 := q shr 17; status(17);
  q1 := q shr 18; status(18);
  q1 := q shr 19; status(19);
  q1 := q shr 20; status(20);
  q1 := q shr 21; status(21);
  q1 := q shr 22; status(22);
  q1 := q shr 23; status(23);
  q1 := q shr 24; status(24);
  q1 := q shr 25; status(25);
  q1 := q shr 26; status(26);
  q1 := q shr 27; status(27);
  q1 := q shr 28; status(28);
  q1 := q shr 29; status(29);
  q1 := q shr 30; status(30);
  q1 := q shr 31; status(31);
  q1 := q shr 32; status(32);
  q1 := q shr 33; status(33);
  q1 := q shr 34; status(34);
  q1 := q shr 35; status(35);
  q1 := q shr 36; status(36);
  q1 := q shr 37; status(37);
  q1 := q shr 38; status(38);
  q1 := q shr 39; status(39);
  q1 := q shr 40; status(40);
  q1 := q shr 41; status(41);
  q1 := q shr 42; status(42);
  q1 := q shr 43; status(43);
  q1 := q shr 44; status(44);
  q1 := q shr 45; status(45);
  q1 := q shr 46; status(46);
  q1 := q shr 47; status(47);
  q1 := q shr 48; status(48);
  q1 := q shr 49; status(49);
  q1 := q shr 50; status(50);
  q1 := q shr 51; status(51);
  q1 := q shr 52; status(52);
  q1 := q shr 53; status(53);
  q1 := q shr 54; status(54);
  q1 := q shr 55; status(55);
  q1 := q shr 56; status(56);
  q1 := q shr 57; status(57);
  q1 := q shr 58; status(58);
  q1 := q shr 59; status(59);
  q1 := q shr 60; status(60);
  q1 := q shr 61; status(61);
  q1 := q shr 62; status(62);
  q1 := q shr 63; status(63);
end;

end.

