This directory contains some utilities that are used during the
development of the Free Pascal Compiler.

msg2inc  : Convert a compiler message file (errorX.msg) to .inc files to
           include it as the default language in the compiler. It can
	   also convert the .msg to .tex for inclusion the documentation
	  
nasmconv : Convert a Nasm insns.dat to i386tab.inc so it can be used with
           the compiler

makecfg  : This script will make the samplecfg for linux installations

msgdif   : analyzes the differences between two msg files
           msgdif errore.msg errord.msg
           will print out new error msg, removed old ones
           and will create a new file new.msg that will
           contain the new error messages (supposing TeX comment is after
           the message line itself)
           removed messages are prepended by "%%% "
           (they can be useful in case on error enum renaming !)
