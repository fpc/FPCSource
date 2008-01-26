{
    Copyright (c) 1999 by Pierre Muller

    direct GDB objects linking for debug info

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ gdb-version/gdb directory must be in objects path }
{ use ifdef or ifndef GDB_V416 to change objects needed for
  different gdb versions }

{$L infcmd.o}
{$L symtab.o}
{$L top.o}
{$L target.o}
{$L utils.o}
{$L breakpoi.o}
{$L corefile.o}
{$L symfile.o}
{$L ../libibert/concat.o}
{$L environ.o}
{$L eval.o}
{$L infrun.o}
{$L blockfra.o}
{$L findvar.o}
{$L printcmd.o}
{$L i386-tde.o}
{$L minsyms.o}
{$L stack.o}
{$L gdbtypes.o}
{$L valops.o}
{$L values.o}
{$L valprint.o}
{$L language.o}
{$L source.o}
{$L i387-tde.o}
{$L command.o}
{$L objfiles.o}
{$L demangle.o}
{$L ../libibert/cplus-de.o}
{$L gnu-rege.o}
{$L c-typepr.o}
{$L typeprin.o}
{$L init.o}
{$L parse.o}
{$L serial.o}
{$L ../libibert/getrunti.o}
{$L ../readline/complete.o}
{$L ../readline/readline.o}
{$L ../readline/history.o}
{$L ../gdb/version.o}
{$L inflow.o}
{$L ../readline/tilde.o}
{$L ../readline/bind.o}
{$L mem-brea.o}
{$L ../libibert/strsigna.o}
{$L ../bfd/bfd.o}
{$L ../libibert/vasprint.o}
{$L ch-lang.o}
{$ifdef GDB_V416}
{$L go32targ.o}
{$else}
{$L go32-nat.o}
{$endif}
{$L thread.o}
{$L valarith.o}
{$L expprint.o}
{$L exec.o}
{$L ../bfd/corefile.o}
{$L ../libibert/obstack.o}
{$L ../bfd/section.o}
{$L complain.o}
{$L ../libibert/argv.o}
{$L ../bfd/opncls.o}
{$L ../bfd/format.o}
{$L bcache.o}
{$L symmisc.o}
{$L f-lang.o}
{$L f-valpri.o}
{$L cp-valpr.o}
{$L ../opcodes/dis-buf.o}
{$L ../opcodes/i386-dis.o}
{$L scm-lang.o}
{$L ../libibert/basename.o}
{$L ../libibert/floatfor.o}
{$L copying.o}
{$L ser-go32.o}
{$L remote.o}
{$L dcache.o}
{$L remote-u.o}
{$L buildsym.o}
{$L maint.o}
{$L dbxread.o}
{$L coffread.o}
{$L elfread.o}
{$L mipsread.o}
{$L stabsrea.o}
{$L c-lang.o}
{$L m2-lang.o}
{$L nlmread.o}
{$L mdebugre.o}
{$L os9kread.o}
{$L p-lang.o}
{$L p-valpri.o}
{$L ../readline/display.o}
{$L ../readline/keymaps.o}
{$L ../readline/rltty.o}
{$L ../readline/vi_mode.o}
{$L ../readline/funmap.o}
{$L ../readline/parens.o}
{$L ../bfd/cache.o}
{$L ch-exp.o}
{$L ch-typep.o}
{$L ch-valpr.o}
{$L ../bfd/libbfd.o}
{$L ../bfd/archures.o}
{$L ../bfd/targets.o}
{$L ../bfd/binary.o}
{$L f-exp_ta.o}
{$L f-typepr.o}
{$L c-valpri.o}
{$L scm-exp.o}
{$L c-exp_ta.o}
{$L scm-valp.o}
{$L dwarfrea.o}
{$L m2-exp_t.o}
{$L m2-typep.o}
{$L m2-valpr.o}
{$L p-exp_ta.o}
{$L p-typepr.o}
{$L ../readline/isearch.o}
{$L ../readline/search.o}
{$L ../bfd/cpu-i386.o}
{$L ../bfd/coff-go3.o}
{$L ../bfd/srec.o}
{$L ../bfd/tekhex.o}
{$L ../bfd/ihex.o}
{$L ../bfd/syms.o}
{$L ../bfd/archive.o}
{$L ../bfd/reloc.o}
{$L ../bfd/linker.o}
{$L ../bfd/coffgen.o}
{$L ../bfd/cofflink.o}
{$L ../libibert/hex.o}
{$L ../bfd/hash.o}
{$L ../bfd/stabs.o}
{$ifdef GDB_V418}
{$L ../readline/histexpa.o}
{$L ../readline/histfile.o}
{$L gdbarch.o}
{$L tracepoi.o}
{$L ax-gdb.o}
{$L jv-lang.o}
{$L ../readline/input.o}
{$L ../readline/util.o}
{$L ../readline/terminal.o}
{$L ../readline/undo.o}
{$L ../readline/macro.o}
{$L ../readline/shell.o}
{$L ../readline/nls.o}
{$L ../readline/kill.o}
{$L ../libibert/xstrerro.o}
{$L ../libibert/objalloc.o}
{$L dwarf2re.o}
{$L ../bfd/coff-stg.o}
{$L ../bfd/i386aout.o}
{$L ../libibert/fnmatch.o}
{$L ../readline/histsear.o}
{$L ax-gener.o}
{$L jv-exp_t.o}
{$L jv-typep.o}
{$L jv-valpr.o}
{$L ../bfd/aout32.o}
{$L ../bfd/stab-sym.o}
{$endif GDB_V418}
