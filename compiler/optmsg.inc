const optiontxt : array[0..00019,1..255] of char=(
  ' [options] <inputfile> [options]'#000+
  'Only one source file supported'#000+
  'DEF file can be created only for OS/2'#000+
  'nested response files are not supported'#000+
  'No source file name in command line'#000+
  'Illegal parameter: $1'#000+
  '-? writes help pages'#000+
  'Unable to open file $1'#000+
  'Reading fur','ther options from $1'#000+
  'Target is already set to: $1'#000+
  'Shared libs not supported on DOS platform, reverting to static'#000+
  'too many IF(N)DEFs'#000+
  'too many ENDIFs'#000+
  'open conditional at the end of the file'#000+
  'Debug information generation is not supported by this executable'#000+
  'Tr','y recompiling with -dGDB'#000+
  'Free Pascal Compiler version $VER for $TARGET'#000+
  'Copyright (c) 1993-98 by Florian Klaempfl'#000+
  'Free Pascal Compiler version $VER'#000+
  #000+
  'This program comes under the GNU General Public Licence'#000+
  'For more information read COPYING'#000+
  #000+
  'Report bugs,sugg','estions etc to:'#000+
  '                ba2395@fen.baynet.de'#000+
  '**0*_+ switch option on, - off'#000+
  '**1a_the compiler doesn'#039't delete the generated assembler file'#000+
  '*t1b_use EMS'#000+
  '**1B+_build'#000+
  '**1C_code generation options'#000+
  '**2Ca_not implemented'#000+
  '**2Ce_not implemented'#000+
  '3*2CD_Dynam','ic linking'#000+
  '**2Ch<n>_<n> bytes heap (between 1023 and 67107840)'#000+
  '**2Ci_IO-checking'#000+
  '**2Cn_omit linking stage'#000+
  '**2Co_check overflow of integer operations'#000+
  '**2Cr_range checking'#000+
  '*O2Cs<n>_set stack size to <n>'#000+
  '**2Ct_stack checking'#000+
  '3*2CS_static linking'#000+
  '**1d<x>_defi','nes the symbol <x>'#000+
  '*O1D_controls the generation of DEF file (only OS/2)'#000+
  '*O2Dd<x>_set description to <x>'#000+
  '*O2Do_generate DEF file'#000+
  '*O2Dw_PM application'#000+
  '*L1e<x>_set path to executables'#000+
  '**1E_same as -Cn'#000+
  '**1g_generate debugger information'#000+
  '**1F_set file names an','d paths'#000+
  '**2Fe<x>_redirect error output to <x>'#000+
  '*L2Fg<x>_<x> search path for the GNU C lib'#000+
  '*L2Fr<x>_<x> search path for the error message file'#000+
  '**2Fi<x>_adds <x> to include path'#000+
  '**2Fl<x>_adds <x> to library path'#000+
  '*L2FL<x>_uses <x> as dynamic linker'#000+
  '**2Fu<x>_a','dds <x> to unit path'#000+
  '**1k<x>_Pass <x> to the linker'#000+
  '**1L_set language'#000+
  '**2LD_german'#000+
  '**2LE_english'#000+
  '**2LN_dutch'#000+
  '**1l_write logo'#000+
  '**1i_information'#000+
  '**1n_don'#039't read the default config file'#000+
  '**1o<x>_change the name of the executable produced to <x>'#000+
  '**1pg_generate ','profile code for gprof'#000+
  '*L1P_use pipes instead of creating temporary assembler files'#000+
  '**1S_syntax options'#000+
  '**2S2_switch some Delphi 2 extension on'#000+
  '**2Sc_supports operators like C (*=,+=,/= and -=)'#000+
  '**2Sd_compiler disposes asm lists (uses less memory but slowe','r)'#000+
  '**2Se_compiler stops after the first error'#000+
  '**2Sg_allows LABEL and GOTO'#000+
  '**2Si_support C++ stlyed INLINE'#000+
  '**2Sm_support macros like C (global)'#000+
  '**2So_tries to be TP/BP 7.0 compatible'#000+
  '**2Ss_constructor name must be init (destructor must be done)'#000+
  '**2St_allow','s static keyword in objects'#000+
  '**1s_don'#039't call assembler and linker (only with -a)'#000+
  '**1T<x>_Target operating system'#000+
  '3*2TGO32V1_version 1 of DJ Delorie DOS extender'#000+
  '3*2TGO32V2_version 2 of DJ Delorie DOS extender'#000+
  '3*2TLINUX_Linux'#000+
  '3*2TOS2_OS/2 2.x'#000+
  '3*2TWin32_Wind','ows 32 Bit'#000+
  '6*2TAMIGA_Commodore Amiga'#000+
  '6*2TATARI_Atari ST/STe/TT'#000+
  '6*2TMACOS_Macintosh m68k'#000+
  '6*2TLINUX_Linux-68k'#000+
  '**1u<x>_undefines the symbol <x>'#000+
  '**1U_unit options'#000+
  '**2Uls_make static library from unit'#000+
  '**2Uld_make dynamic library from unit'#000+
  '**2Un_don'#039't check the',' unit name'#000+
  '**2Up<x>_same as -Fu<x>'#000+
  '**2Us_compile a system unit'#000+
  '**1v<x>_Be verbose. <x> is a combination of the following letters :'#000+
  '**2*_e : Show errors (default)       d : Show debug info'#000+
  '**2*_w : Show warnings               u : Show used files'#000+
  '**2*_n : S','how notes                  t : Show tried files'#000+
  '**2*_h : Show hints                  m : Show defined macros'#000+
  '**2*_i : Show general info           p : Show compiled procedures'#000+
  '**2*_l : Show linenumbers            c : Show conditionals'#000+
  '**2*_a : Show everyth','ing             0 : Show nothing (except errors'+
  ')'#000+
  '**1X_executable options'#000+
  '*L2Xc_link with the c library'#000+
  '**2Xs_strip all symbols from executable'#000+
  '**0*_Processor specific options:'#000+
  '3*1A_output format'#000+
  '3*2Ao_coff file using GNU AS'#000+
  '3*2Anasmcoff_coff file using Na','sm'#000+
  '3*2Anasmelf_elf32 (linux) file using Nasm'#000+
  '3*2Anasmobj_obj file using Nasm'#000+
  '3*2Amasm_obj using Masm (Mircosoft)'#000+
  '3*2Atasm_obj using Tasm (Borland)'#000+
  '3*1R_assembler reading style'#000+
  '3*2Ratt_read AT&T style assembler'#000+
  '3*2Rintel_read Intel style assembler'#000+
  '3*2Rdire','ct_copy assembler text directly to assembler file'#000+
  '3*1O_optimizations'#000+
  '3*2Oa_turn on the optimizer'#000+
  '3*2Og_generate smaller code'#000+
  '3*2OG_generate faster code (default)'#000+
  '3*2Ox_optimize maximum (still BUGGY!!!)'#000+
  '3*2Oz_uncertain optimizations (see docs)'#000+
  '3*2O2_set ta','rget processor to Pentium II (tm)'#000+
  '3*2O3_set target processor to i386'#000+
  '3*2O4_set target processor to i486'#000+
  '3*2O5_set target processor to Pentium (tm)'#000+
  '3*2O6_set target processor to PentiumPro (tm)'#000+
  '3*2O7_set target processor to Cyrix 6x86'#000+
  '3*2O8_set target proc','essor to AMD K6'#000+
  '6*1A_output format'#000+
  '6*2Ao_Unix o-file using GNU AS'#000+
  '6*2Agas_GNU Motorola assembler'#000+
  '6*2Amit_MIT Syntax (old GAS)'#000+
  '6*2Amot_Standard Motorola assembler'#000+
  '6*1O_optimizations'#000+
  '6*2Oa_ turn on the optimizer'#000+
  '6*2Og_ generate smaller code'#000+
  '6*2OG_ generate ','faster code (default)'#000+
  '6*2Ox_optimize maximum (still BUGGY!!!)'#000+
  '6*2O2_set target processor to a MC68020+'#000+
  '**1*_'#000+
  '**1?_shows this help'#000+
  '**1h_shows this help without waiting'#000
);
