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
    writeln('F and D extensions not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_IMA_V)<>0 then
    writeln('V extension supported')
  else
    writeln('V extension supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBA)<>0 then
    writeln('ZBA extension supported')
  else
    writeln('ZBA extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBB)<>0 then
    writeln('ZBB extension supported')
  else
    writeln('ZBB extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBS)<>0 then
    writeln('ZBS extension supported')
  else
    writeln('ZBS extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZICBOZ)<>0 then
    writeln('ZICBOZ extension supported')
  else
    writeln('ZICBOZ extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBC)<>0 then
    writeln('ZBC extension supported')
  else
    writeln('ZBC extension not supported');

  if (ariscv_hwprobe.value and RISCV_HWPROBE_EXT_ZBKB)<>0 then
    writeln('ZBKB extension supported')
  else
    writeln('ZBKB extension supported');
end.