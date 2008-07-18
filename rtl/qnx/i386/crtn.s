//
// Copyright 2001, QNX Software Systems Ltd. All Rights Reserved
//
// QNX has kindly released this source code under the QNX open 
// Community license, expressly to be used with  the 
// Free Pascal runtime library
//

	/* Make a placeholder .note segment */
	.section .note,"a"
#if 0
	.long	4					/* Elf32_Nhdr.n_namesz = sizeof QNX_NOTE_NAME */
	.long	4					/* Elf32_Nhdr.n_descsz = sizeof Elf32_Word */
	.long	3					/* Elf32_Nhdr.n_type = QNT_STACK */
	.byte	'Q', 'N', 'X', 0	/* QNX_NOTE_NAME */
	.long	32768				/* stack size of 32k */
#endif

	.section .init
	ret	$0x0

	.section .fini
	ret	$0x0
