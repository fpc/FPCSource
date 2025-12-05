{ %target=linux }
{ %cpu=riscv32,riscv64 }
{$mode objfpc}
uses
  linux,sysutils;

type
  TRiscvExtension = record
    mask: QWord;
    name: string;
  end;

const
  { Define all extensions to check }
  RISCV_EXTENSIONS: array[0..58] of TRiscvExtension = (
    { 0 } (mask: RISCV_HWPROBE_IMA_FD; name: 'F and D'),
    { 1 } (mask: RISCV_HWPROBE_IMA_C; name: 'C'),
    { 2 } (mask: RISCV_HWPROBE_IMA_V; name: 'V'),
    { 3 } (mask: RISCV_HWPROBE_EXT_ZBA; name: 'ZBA'),
    { 4 } (mask: RISCV_HWPROBE_EXT_ZBB; name: 'ZBB'),
    { 5 } (mask: RISCV_HWPROBE_EXT_ZBS; name: 'ZBS'),
    { 6 } (mask: RISCV_HWPROBE_EXT_ZICBOZ; name: 'ZICBOZ'),
    { 7 } (mask: RISCV_HWPROBE_EXT_ZBC; name: 'ZBC'),
    { 8 } (mask: RISCV_HWPROBE_EXT_ZBKB; name: 'ZBKB'),
    { 9 } (mask: RISCV_HWPROBE_EXT_ZBKC; name: 'ZBKC'),
    { 10 } (mask: RISCV_HWPROBE_EXT_ZBKX; name: 'ZBKX'),
    { 11 } (mask: RISCV_HWPROBE_EXT_ZKND; name: 'ZKND'),
    { 12 } (mask: RISCV_HWPROBE_EXT_ZKNE; name: 'ZKNE'),
    { 13 } (mask: RISCV_HWPROBE_EXT_ZKNH; name: 'ZKNH'),
    { 14 } (mask: RISCV_HWPROBE_EXT_ZKSED; name: 'ZKSED'),
    { 15 } (mask: RISCV_HWPROBE_EXT_ZKSH; name: 'ZKSH'),
    { 16 } (mask: RISCV_HWPROBE_EXT_ZKT; name: 'ZKT'),
    { 17 } (mask: RISCV_HWPROBE_EXT_ZVBB; name: 'ZVBB'),
    { 18 } (mask: RISCV_HWPROBE_EXT_ZVBC; name: 'ZVBC'),
    { 19 } (mask: RISCV_HWPROBE_EXT_ZVKB; name: 'ZVKB'),
    { 20 } (mask: RISCV_HWPROBE_EXT_ZVKG; name: 'ZVKG'),
    { 21 } (mask: RISCV_HWPROBE_EXT_ZVKNED; name: 'ZVKNED'),
    { 22 } (mask: RISCV_HWPROBE_EXT_ZVKNHA; name: 'ZVKNHA'),
    { 23 } (mask: RISCV_HWPROBE_EXT_ZVKNHB; name: 'ZVKNHB'),
    { 24 } (mask: RISCV_HWPROBE_EXT_ZVKSED; name: 'ZVKSED'),
    { 25 } (mask: RISCV_HWPROBE_EXT_ZVKSH; name: 'ZVKSH'),
    { 26 } (mask: RISCV_HWPROBE_EXT_ZVKT; name: 'ZVKT'),
    { 27 } (mask: RISCV_HWPROBE_EXT_ZFH; name: 'ZFH'),
    { 28 } (mask: RISCV_HWPROBE_EXT_ZFHMIN; name: 'ZFHMIN'),
    { 29 } (mask: RISCV_HWPROBE_EXT_ZIHINTNTL; name: 'ZIHINTNTL'),
    { 30 } (mask: RISCV_HWPROBE_EXT_ZVFH; name: 'ZVFH'),
    { 31 } (mask: RISCV_HWPROBE_EXT_ZVFHMIN; name: 'ZVFHMIN'),
    { 32 } (mask: RISCV_HWPROBE_EXT_ZFA; name: 'ZFA'),
    { 33 } (mask: RISCV_HWPROBE_EXT_ZTSO; name: 'ZTSO'),
    { 34 } (mask: RISCV_HWPROBE_EXT_ZACAS; name: 'ZACAS'),
    { 35 } (mask: RISCV_HWPROBE_EXT_ZICOND; name: 'ZICOND'),
    { 36 } (mask: RISCV_HWPROBE_EXT_ZIHINTPAUSE; name: 'ZIHINTPAUSE'),
    { 37 } (mask: RISCV_HWPROBE_EXT_ZVE32X; name: 'ZVE32X'),
    { 38 } (mask: RISCV_HWPROBE_EXT_ZVE32F; name: 'ZVE32F'),
    { 39 } (mask: RISCV_HWPROBE_EXT_ZVE64X; name: 'ZVE64X'),
    { 40 } (mask: RISCV_HWPROBE_EXT_ZVE64F; name: 'ZVE64F'),
    { 41 } (mask: RISCV_HWPROBE_EXT_ZVE64D; name: 'ZVE64D'),
    { 42 } (mask: RISCV_HWPROBE_EXT_ZIMOP; name: 'ZIMOP'),
    { 43 } (mask: RISCV_HWPROBE_EXT_ZCA; name: 'ZCA'),
    { 44 } (mask: RISCV_HWPROBE_EXT_ZCB; name: 'ZCB'),
    { 45 } (mask: RISCV_HWPROBE_EXT_ZCD; name: 'ZCD'),
    { 46 } (mask: RISCV_HWPROBE_EXT_ZCF; name: 'ZCF'),
    { 47 } (mask: RISCV_HWPROBE_EXT_ZCMOP; name: 'ZCMOP'),
    { 48 } (mask: RISCV_HWPROBE_EXT_ZAWRS; name: 'ZAWRS'),
    { 49 } (mask: RISCV_HWPROBE_EXT_SUPM; name: 'SUPM'),
    { 50 } (mask: RISCV_HWPROBE_EXT_ZFBFMIN; name: 'ZFBFMIN'),
    { 51 } (mask: RISCV_HWPROBE_EXT_ZIHPM; name: 'ZIHPM'),
    { 52 } (mask: RISCV_HWPROBE_EXT_ZFBMIN; name: 'ZFBMIN'),
    { 53 } (mask: RISCV_HWPROBE_EXT_ZVFBFMIN; name: 'ZVFBFMIN'),
    { 54 } (mask: RISCV_HWPROBE_EXT_ZVFBFWMA; name: 'ZVFBFWMA'),
    { 55 } (mask: RISCV_HWPROBE_EXT_ZICBOM; name: 'ZICBOM'),
    { 56 } (mask: RISCV_HWPROBE_EXT_ZAAMO; name: 'ZAAMO'),
    { 57 } (mask: RISCV_HWPROBE_EXT_ZALRSC; name: 'ZALRSC'),
    { 58 } (mask: RISCV_HWPROBE_EXT_ZABHA; name: 'ZABHA')
  );

procedure CheckExtension(value: QWord; ext: TRiscvExtension);
  begin
    if (value and ext.mask) <> 0 then
      writeln(ext.name, ' extension supported')
    else
      writeln('  ', ext.name, ' extension not supported');
  end;


function GetAllTestedBits: QWord;
  var
    i: Integer;
  begin
    Result := 0;
    for i := Low(RISCV_EXTENSIONS) to High(RISCV_EXTENSIONS) do
      Result := Result or RISCV_EXTENSIONS[i].mask;
  end;


var
  ariscv_hwprobe: triscv_hwprobe;
  i: Integer;
  all_tested_bits: QWord;
  untested_bits: QWord;

begin
  ariscv_hwprobe.key := RISCV_HWPROBE_KEY_IMA_EXT_0;
  riscv_hwprobe(@ariscv_hwprobe, 1, 0, nil, 0);
  writeln('Raw key value returned by RISCV_HWPROBE_KEY_IMA_EXT_0: %',
          Binstr(ariscv_hwprobe.value, 64));

  { Check all extensions }
  for i := Low(RISCV_EXTENSIONS) to High(RISCV_EXTENSIONS) do
    CheckExtension(ariscv_hwprobe.value, RISCV_EXTENSIONS[i]);

  { Verify all set bits are tested }
  all_tested_bits := GetAllTestedBits;
  untested_bits := ariscv_hwprobe.value and (not all_tested_bits);

  if untested_bits <> 0 then
  begin
    writeln;
    writeln('WARNING: The following bits are set but not tested:');
    writeln('  Untested bits: %', Binstr(untested_bits, 64));
  end
  else
  begin
    writeln;
    writeln('All set bits have been tested.');
  end;
end.
