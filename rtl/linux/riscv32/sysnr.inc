{$i ../sysnr-gen.inc}
const
  syscall_nr__llseek      = syscall_nr_lseek;
  syscall_nr_ftruncate64  = syscall_nr_ftruncate;
  syscall_nr_fstat64      = syscall_nr_fstat;

  syscall_nr_riscv_hwprobe                      = 258;
  syscall_nr_riscv_flush_icache                 = 259;
