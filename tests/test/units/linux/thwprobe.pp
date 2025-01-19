{ %target=linux }
{ %cpu=riscv32,riscv64 }
uses
  linux,sysutils;

var
  ariscv_hwprobe: triscv_hwprobe;

begin
  ariscv_hwprobe.key:=RISCV_HWPROBE_KEY_IMA_EXT_0;
  riscv_hwprobe(@ariscv_hwprobe,1,0,nil,0);
  writeln('Raw key value returned by RISCV_HWPROBE_KEY_IMA_EXT_0: %',Binstr(ariscv_hwprobe.value,64));

  if (ariscv_hwprobe.value and RISCV_HWPROBE_IMA_FD)<>0 then
    writeln('F and D extensions supported')
  else
    writeln('  F and D extensions not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_IMA_V)<>0 then
    writeln('V extension supported')
  else
    writeln('  V extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBA)<>0 then
    writeln('ZBA extension supported')
  else
    writeln('  ZBA extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBB)<>0 then
    writeln('ZBB extension supported')
  else
    writeln('  ZBB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBS)<>0 then
    writeln('ZBS extension supported')
  else
    writeln('  ZBS extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZICBOZ)<>0 then
    writeln('ZICBOZ extension supported')
  else
    writeln('  ZICBOZ extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBC)<>0 then
    writeln('ZBC extension supported')
  else
    writeln('  ZBC extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBKB)<>0 then
    writeln('ZBKB extension supported')
  else
    writeln('  ZBKB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBKC)<>0 then
    writeln('ZBKC extension supported')
  else
    writeln('  ZBKC extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBKX)<>0 then
    writeln('ZBKX extension supported')
  else
    writeln('  ZBKX extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKND)<>0 then
    writeln('ZKND extension supported')
  else
    writeln('  ZKND extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKNE)<>0 then
    writeln('ZKNE extension supported')
  else
    writeln('  ZKNE extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKNH)<>0 then
    writeln('ZKNH extension supported')
  else
    writeln('  ZKNH extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKSED)<>0 then
    writeln('ZKSED extension supported')
  else
    writeln('  ZKSED extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKSH)<>0 then
    writeln('ZKSH extension supported')
  else
    writeln('  ZKSH extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZKT)<>0 then
    writeln('ZKT extension supported')
  else
    writeln('  ZKT extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVBB)<>0 then
    writeln('ZVBB extension supported')
  else
    writeln('  ZVBB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVBC)<>0 then
    writeln('ZVBC extension supported')
  else
    writeln('  ZVBC extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKB)<>0 then
    writeln('ZVKB extension supported')
  else
    writeln('  ZVKB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKG)<>0 then
    writeln('ZVKG extension supported')
  else
    writeln('  ZVKG extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKNED)<>0 then
    writeln('ZVKNED extension supported')
  else
    writeln('  ZVKNED extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKNHA)<>0 then
    writeln('ZVKNHA extension supported')
  else
    writeln('  ZVKNHA extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKNHB)<>0 then
    writeln('ZVKNHB extension supported')
  else
    writeln('  ZVKNHB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKSED)<>0 then
    writeln('ZVKSED extension supported')
  else
    writeln('  ZVKSED extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKSH)<>0 then
    writeln('ZVKSH extension supported')
  else
    writeln('  ZVKSH extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVKT)<>0 then
    writeln('ZVKT extension supported')
  else
    writeln('  ZVKT extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZFH)<>0 then
    writeln('ZFH extension supported')
  else
    writeln('  ZFH extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZFHMIN)<>0 then
    writeln('ZFHMIN extension supported')
  else
    writeln('  ZFHMIN extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZIHINTNTL)<>0 then
    writeln('ZIHINTNTL extension supported')
  else
    writeln('  ZIHINTNTL extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVFH)<>0 then
    writeln('ZVFH extension supported')
  else
    writeln('  ZVFH extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVFHMIN)<>0 then
    writeln('ZVFHMIN extension supported')
  else
    writeln('  ZVFHMIN extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZFA)<>0 then
    writeln('ZFA extension supported')
  else
    writeln('  ZFA extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZTSO)<>0 then
    writeln('ZTSO extension supported')
  else
    writeln('  ZTSO extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZACAS)<>0 then
    writeln('ZACAS extension supported')
  else
    writeln('  ZACAS extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZICOND)<>0 then
    writeln('ZICOND extension supported')
  else
    writeln('  ZICOND extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZIHINTPAUSE)<>0 then
    writeln('ZIHINTPAUSE extension supported')
  else
    writeln('  ZIHINTPAUSE extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVE32X)<>0 then
    writeln('ZVE32X extension supported')
  else
    writeln('  ZVE32X extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVE32F)<>0 then
    writeln('ZVE32F extension supported')
  else
    writeln('  ZVE32F extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVE64X)<>0 then
    writeln('ZVE64X extension supported')
  else
    writeln('  ZVE64X extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVE64F)<>0 then
    writeln('ZVE64F extension supported')
  else
    writeln('  ZVE64F extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZVE64D)<>0 then
    writeln('ZVE64D extension supported')
  else
    writeln('  ZVE64D extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZIMOP)<>0 then
    writeln('ZIMOP extension supported')
  else
    writeln('  ZIMOP extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZCA)<>0 then
    writeln('ZCA extension supported')
  else
    writeln('  ZCA extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZCB)<>0 then
    writeln('ZCB extension supported')
  else
    writeln('  ZCB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZCD)<>0 then
    writeln('ZCD extension supported')
  else
    writeln('  ZCD extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZCF)<>0 then
    writeln('ZCF extension supported')
  else
    writeln('  ZCF extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZCMOP)<>0 then
    writeln('ZCMOP extension supported')
  else
    writeln('  ZCMOP extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZAWRS)<>0 then
    writeln('ZAWRS extension supported')
  else
    writeln('  ZAWRS extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_SUPM)<>0 then
    writeln('SUPM extension supported')
  else
    writeln('  SUPM extension not supported');
end.
