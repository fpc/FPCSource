{****************************************************************************

						   DOSCALLS interface unit
					 FPK-Pascal Runtime Library for OS/2
				   Copyright (c) 1993,94 by Florian Kl„mpfl
					Copyright (c) 1997 by Dani‰l Mantione

	  This file may be reproduced and modified under the same conditions
					  as all other FPK-Pascal source code.

****************************************************************************}

unit doscalls;

{This unit was called bsedos in the original OS/2 runtime library.
 Changed, because it's an interface library to DOSCALLS.DLL.

 The goal was to make this unit a real Pascal unit instead of a C unit in
 Pascal clothes. Not that I want to make every translated statement look
 exactly like other Pascal code, but in this case, it was too crazy.
 Therefore typenames, constants etc. have names like the ones in IBM's
 documentation (which is C oriented), but do not match exactly. In general
 constants do now use the common two letter prefix, followed by the constant
 description. Type names use a capital T in front of their name, handles do
 not have separate types, longints are used, and whenever possible pointers
 were replaced by var parameters. All constructions like 'type LONG=longint'
 have been removed. Furthermore, most of the procedures also accept strings
 instead of just a Pchar, thanks to that cool feature of procedure
 overloading.

 Dani‰l Mantione,
 June 1997}

interface

uses	strings;

{$packrecords 1}

type	Tbytearray=array[0..$fff0] of byte;
		Pbytearray=^Tbytearray;
		Twordarray=array[0..$7ff8] of byte;
		Pwordarray=^Twordarray;

{****************************************************************************

							Thread related routines.

****************************************************************************}

type   Tthreadentry=procedure(param:pointer);


const	dtsuspended			=1;	{Thread is started suspended instead of
								 started at once.}
		dtstack_commited	=2;	{Allocate all stack space at once. The
								 operating system normally allocates more
								 memory to the stack if the stack grows with
								 the given stacksize as limit. This has the
								 restriction that you cannot create a stack
								 frame > 4 kb. at once. If you want to do
								 this, or for other reasons you can allocate
								 the complete stack at once with this flag.}

		dtwait				=0;	{Wait until termination.}
		dtnowait			=1;	{Do not wait. Return with error if not yet
								 terminated.}

{Create a new thread.
 tid 		= Thread ID of new thread is returned here.
 address	= Thread entry point. The new thread starts executing here.
 Aparam		= This one is passed to the thread entry procedure.
 flags		= Flags. Either dtsuspended or dt_stackcommited.
 cbstack	= Size of the stack of the new thread.}
function _DosCreateThread(var tid:longint;address:Tthreadentry;
						  Aparam:pointer;flags:longint;cbstack:longint):word;

{Suspend a running thread.}
function _DosSuspendThread(tid:longint):word;

{Resume a suspended thread.}
function _DosResumeThread(tid:longint):word;

{Terminate a specific thread.}
function _DosKillThread(tid:longint):word;

{Wait until a specific thread has ended.
 tid			= Thread to terminate. Can also be zero. In that case we
				  wait until the next thread terminates. It's thread ID is
				  returned back.
 option			= Flags. Either dtwait or dtnowait.}
function _DosWaitThread(var tid:longint;option:longint):word;

{All other threads in the same process are suspended until a
_DosExitCritSec.}
function _DosEnterCritSec:word;

{Resume the other threads again.}
function _DosExitCritSec:word;

const	dethread=0;			{Terminate thread only.}
		deprocess=1;		{Terminate the whole process.}

{Terminate the thread or the program. Never returns, so it's defined as
 procedure.}
procedure _DosExit(action:longint;result:longint);

type	Pthreadinfoblock=^Tthreadinfoblock;
		Psysthreadib=^Tsysthreadib;
		Pprocessinfoblock=^Tprocessinfoblock;

		Tthreadinfoblock=record
			pexchain,				{Head of exeption handler chain.}
			stack,					{Pointer to the thread's stack.}
			stacklimit:pointer;		{Pointer to the thread's stack-end.}
			tib2:Psysthreadib;		{Pointer to system specific thread info.}
			version,				{Version of this datastructure.}
			ordinal:longint;		{Thread ordinal number.}
		end;

		Tsysthreadib=record
			tid,					{Thread ID.}
			priority,				{Low byte of low word: thread priority.
									 High byte of high word: thread class
										1 = Idle
										2 = Regular
										3 = Time critical
										4 = Server}
			version:longint;		{Version of this datastructure.}
			MCcount,				{Must complete count. ??? Info wanted!}
			MCforceflag:word;		{Must complete force flag. Info wanted!}
		end;

		Tprocessinfoblock=record
			pid,					{Process ID.}
			parentpid,				{Parent's process ID.}
			hmte:longint;			{Module handle of executable program.
									 ??? Info wanted!}
			cmd,					{Command line options.}
			env:Pbytearray;			{Environment strings.}
			flstatus,				{1 means that the process is in exit list
									 processing.}
			ttype:longint;			{Type of process:
										0:	Full screen protected mode.
										1:	DOS emulation.
										2:	Windowable full screen protected
											mode program.
										3:	Presentation manager program.
										4:	Detached mode process.}
		end;

{OS/2 keeps information about the current process and the current thread
 is the datastructures Tprocessinfoblock and Tthreadinfoblock. All data
 can both be read and be changed. Use _DosGetInfoBlocks to get their
 address. The service cannot fail, so it is defined as procedure.}

procedure _DosGetInfoBlocks(var Atib:Pthreadinfoblock;
							var Apib:Pprocessinfoblock);

{Wait a number of microseconds. Cannot fail, so it is defined as procedure.}
procedure _DosSleep(msec:longint);

{Beep the speaker. You do not need to check for an error if you can
 guarantee that the frequency is correct.}
function _DosBeep(freq,ms:longint):word;

{****************************************************************************

						Process handling routines.

****************************************************************************}

function _DosDebug(pdbgbuf:pointer):word;

const	TC_EXIT=0;
		TC_HARDERROR=1;
		TC_TRAP=2;
		TC_KILLPROCESS=3;
		TC_EXCEPTION=4;

		EXLST_ADD=1;
		EXLST_REMOVE=2;
		EXLST_EXIT=3;

type	Texitproc=procedure(reason:longint);

{Add/remove an exitprocedure to the exit list. Also used to terminate an
 exit procedure. An exit procedure will be called on exiting of the program.

	ordercode		= One of the EXLST_XXXX constants.
	proc			= Address of the exit procedure.

An exit procedure is called with one of the TC_XXXX constants. When it is
done it must call _DosExitList with EXLST_EXIT.

Exit procedures are called in random order.}
function _DosExitList(ordercode:longint;proc:Texitproc):word;

const	desync			= 0;	{Wait until program terminates.}
		deasync			= 1;	{Do not wait.}
		deasyncresult	= 2;	{Do not wait. DosWaitChild will follow to
								 check if process has been terminated. If
								 you use this, you must use DosWaitChild,
								 because OS/2 will not free memory that is
								 allocated for the result codes if you don't.}
		detrace			= 3;	{Trace-able. Info Wanted!}
		debackground	= 4;	{Do not run as child. Run in a separate
								 session.}
		desuspended		= 5;	{Child will be loaded, but not executed.}
		deasyncresultdb	= 6;	{?? Info wanted.}

type	resultcodes=record
			terminatereason,		{0 = Normal termionation.
									 1 = Critical error.
									 2 = Trapped. (GPE, etc.)
									 3 = Killed by DosKillProcess.}
			exitcode:longint;		{Exit code of child.}
		end;

{Execute a program.

 objnaam		= If a DLL cannot be found, it's name will be returned here.
 objlen			= Size of your objnaam buffer.
 execflag		= One of the dexxxx constants.
 res			= See resultcodes.
 args			= Arguments. ASCIIZ strings. End of ARGS given by an empty
				  string (#0). First arg must be filename without path and
				  extension. NIL is also allowed.
 env			= Environment. ASCIIZ strings. A variable has the format
				  NAME=CONTENTS. End of ENV given by an empty string (#0).
				  NIL is also allowed.
 filenaam		= Filename with full path and extension. Is not sensitive
				  for the PATH environment variabele.}
function _DosExecPgm(objnaam:Pchar;cbobjnaam,execflag:longint;
					 args,env:Pbytearray;var res:resultcodes;
					 filenaam:Pchar):word;
function _DosExecPgm(var objnaam:string;execflag:longint;
					 args,env:Pbytearray;var res:resultcodes;
					 const filenaam:string):word;

{Wait until a child process terminated. Sometimes called DosCWait.

action				= 0 = Wait until child terminates.
					  1 = Wait until child and all it's childs terminate.
option				= Flags. Either dtwait or dtnowait.
res					= See resultcodes.
termpid				= Process ID that has been terminated. Usefull when
					  terminating a random process.
pid					= Process ID of process to terminate. Use a zero to
					  terminate a random process.}
function _DosWaitChild(action:longint;option:longint;var res:resultcodes;
					   var termpid:longint;pid:longint):word;

const	dpprocess		= 0;
		dpprocesschilds = 1;
		dpthread		= 2;

		dpsameclass		= 0;
		dpidleclass		= 1;
		dpregular		= 2;
		dptimecritical	= 3;

{Set priority of a thread or all threads in another process.

 scope				= 0 = Set for all threads of a process.
					  1 = Set for all threads of a process and it's childs.
					  2 = Set for a thread of the current process.
 class				= 0 = Do not change class.
					  1 = Change to idle time class.
					  2 = Change to regular class.
					  3 = Change to time-critical class.
 delta				= Value to add to priority. Resulting priority must be in
					  the range 0..31.
 portid				= Process ID when scope=0 or 1, thread ID when scope=2.}
function _DosSetPriority(scope,class,delta,portid:longint):word;

{Terminate a process. If the process isn't a child process, it can refuse
 to terminate.

 action				= 0 = Terminate process and all it's childs.
					  1 = Terminate process only.
 pid				= Process ID of process to terminate.}
function _DosKillProcess(action:longint;pid:longint):word;

{****************************************************************************

						File handling related routines.

****************************************************************************}

const	maxpathlength=260;
		maxpathcomponent=256;

type	Tfilelock=record
			offset,range:longint;
		end;
		Pfilelock=^Tfilelock;

{Lock or unlock an area of a file. Other processes may not access that part
 of the file.

 unlock			= Area to unlock. (0,0) = Do not unlock.
 lock			= Area to lock.	  (0,0) = Do not lock.
 timeout		= Number of miliseconds to wait if another process has locked
				  the file.
 flags			= Bitfield:
				  Bit 0:	0 = Other processes are denied access.
							1 = Other processes may still read from the area.
				  Bit 1:	0 = Normal locking mode.
							1 = Atomic mode. Refer IBM's documentation.}
function _DosSetFileLocks(handle:longint;var unlock,lock:Tfilelock;
						  timeout:longint;flags:longint):word;

{Cancel a filelock area.

handle	= File handle.
lock	= Area that is locked now.}
function _DosCancelLockRequest(handle:longint;var lock:Tfilelock):word;

{Data structures for extended attributes. Reading IBM's documentation is
 highly recommended before experimenting with EAs.}

const   fEA_needEA=$80;

		eabinary		= $fffe;
		eaASCII			= $fffd;
		eabitmap		= $fffb;
		eametafile		= $fffa;
		eaicon			= $fff9;
		eaEA			= $ffee;
		eaMVMT			= $ffdf;
		eaMVST			= $ffde;
		eaASN1			= $ffdd;

type	TgEA=record
			naamlen:byte;
			naam:array[0..1-1] of char;
		end;
		PgEA=^TgEA;

		TgEAlist=record
			listlen:longint;
			list:array[0..1-1] of TgEA;
		end;
		PgEAlist=^TgEAlist;

		TfEA=record
			EA,
			naamlen:byte;
			value:word;
		end;
		PfEA=^TfEA;

		TfEAlist=record
			size:longint;
			list:array[0..1-1] of TfEA;
		end;
		PfEAlist=^TfEAlist;

		TEAop=record
			gEAlist:PgEAlist;
			fEAlist:PfEAlist;
			error:longint;
		end;
		PEAop=^TEAop;

		TfEA2=record
			nextentry:longint;
			EA,
			naam:byte;
			value:word;
			szname:array[0..1-1] of char;
		end;
		PfEA2=^TfEA2;

		TfEA2list=record
			listlen:longint;
			list:array[0..1-1] of TfEA2;
		end;
		PfEA2list=^TfEA2list;

		TgEA2=record
			nextentry:longint;
			naamlen:BYTE;
			naam:array[0..1-1] of char;
		end;
		PgEA2=^TgEA2;

		TgEA2list=record
		  listlen:longint;
		  list:array[0..1-1] of TgEA2;
		end;
		PgEA2list=^gEA2list;

		TEAop2=record
			gEA2list:PgEA2list;
			fEA2list:PfEA2list;
			error:longint;
		end;
		PEAop2=^TEAop2;

		TEAsizebuf=record
			maxEAsize:word;
			maxEAlistsize:longint;
		end;
		PEAsizebuf=^EAsizebuf;


{*******************End of extented attribute datastructures.***************}

	const
	   DKP_PROCESSTREE=0;
	   DKP_PROCESS=1;

	const
	   FSAT_CHARDEV=1;
	   FSAT_PSEUDODEV=2;
	   FSAT_LOCALDRV=3;
	   FSAT_REMOTEDRV=4;


{Usefull constanst for action parameter.}
const		doopened		=  1;
			docreated		=  2;
			dooverwritten	=  3;

{Usefull constants for openflags parameter.}
const		dofail			=  0;
			doopen			=  1;
			dooverwrite		=  2;
			docreate		= 10;

{Usefull constants for openmode parameter.}

const		doread		 	=     0;
			dowrite		 	=     1;
			doreadwrite  	=	  2;
			dodenyRW	 	=	 16;
			dodenywrite  	=	 32;
			dodenyread   	=	 48;
			dodenynone	 	=	 64;
			donoinherit  	=	128;
			dosequential 	= 	256;
			dorandom	 	=   512;
			donocache	 	=  4096;
			dofailonerr	 	=  8192;
			dowritethru  	= 16384;
			doDASD		 	= 32768;

{ Open a file.

 filenaam		= Name of file.
 handle			= Receives filehandle.
 action			= Receives result of opening.
					1 = Existing file opened.
					2 = File did not exist. Created.
					3 = File existed. Overwritten.
 initsize		= Initial size of file when creating or overwriting.
				  Ignored when you do not. Must be zero when the file is
				  created or overwritten in read-only mode.
 attrib			= Attributes when creating or overwriting files.
 openflags		= Bitfield describing what to do when file exists or doesn't
				  exist.
 openmode		= Bitfield describing describing how to open a file.
 ea				= Extended attributes to give file when created. Use a NIL
				  pointer if you don't want to give it extended attributes.
				  Use it only when creating or overwriting file. Use NIL
				  when not. Only the FEA list will be used.

The bits in the openflags parameter have the following meanings:

 Bit 0-3:	Action to take when file exists.	0000 = Return with error.
												0001 = Open it.
												0010 = Overwrite it.
 Bit 4-7:	Action to take when file does not   0000 = Return with error.
			exist.								0001 = Create it.

The bits in the filemode parameter have the following meanings:

 Bit 0-2:	Access mode:		000 = Read-only
								001 = Write-only
								010 = Read/Write
 Bit 3:		Reserved.
 Bit 4-6:	Sharing mode.		001 = Deny all
								010 = Deny write
								011 = Deny read
								100 = Deny none
 Bit 7:		Inheritance.		0 = Handle will be inherited by childs.
								1 = Handle will not be inherited.
 Bit 8-11:	Reserved.
 Bit 12:	Cache flag.			0 = Use caching.
								1 = Disable both read and write caching.
 Bit 13:	Error handling.		0 = Use critical error handler.
								1 = Return just an error code.
 Bit 14:	Write cache flag.	0 = Write operations may be cached.
								1 = Write operations must be executed
									before write operation functions return.
 Bit 15:	DASD flag.			0 = Open a file or device.
								1 = Open a drive as file.

When the DASD flag is set, the whole drive is read as a single file. The
file starts with 512 of bootsector, then 512 bytes of the second sector etc.
The filename must consist of the driveletter followed by a semicolon.}
function _DosOpen(filenaam:Pchar;var handle,action:longint;
				  initsize:longint;attrib,openflags,filemode:longint;
				  ea:PEAOP2):word;
{This variant of _DosOpen always creates or overwrites a file.}
function _DosCreate(filenaam:Pchar;var handle:longint;
					attrib,openmode:longint):word;
{This variant of _DosOpen always opens an existing file.}
function _DosOpen(filenaam:Pchar;var handle:longint;
				  attrib,openmode:longint):word;
{There are also string variants.}
function _DosOpen(const filenaam:string;var handle,action:longint;
				  initsize:longint;attrib,openflags,openmode:longint;
				  peaop2:PEAOP2):word;
function _DosCreate(const filenaam:string;var handle:longint;
					attrib,openmode:longint):word;
function _DosOpen(const filenaam:string;var handle:longint;
				  attrib,openmode:longint):word;


{Close a file.
Cannot fail if handle does exist.}
function _DosClose(handle:longint):word;

{Read from a file or other type of handle.

	handle		= File handle.
	buffer		= The read data is stored here.
	count		= Number of bytes to read.
	actcount	= Number of bytes actually read.}
function _DosRead(handle:longint;var buffer;count:longint;
				  var actcount:longint):word;

{Write to a file or other type of handle.

	handle		= File handle.
	buffer		= The data to be written.
	count		= Number of bytes to write.
	actcount	= Number of bytes actually written.}
function _DosWrite(handle:longint;var buffer;count:longint;
				   var actcount:longint):word;

const	dszerobased=0;		{Set filepointer from begin of file.}
		dsrelative=1;		{Set filepointer relative to the current one.}
		dsendbased=2;		{Set filepointer from end of file.}

{Change the filepointer of a file.}
function _DosSetFilePtr(handle:word;pos:longint;method:longint;
						var posactual:longint):word;
{This variant seeks always from begin of file and does not return the
 actual position.}
function _DosSetFilePtr(handle:word;pos:longint):word;
{This variant returns the current filepointer.}
function _DosGetFilePtr(handle:word;posactual:longint);

{Use dosqueryfileinfo or dosquerypathinfo to get the size of a file.}

{Change the size of a file.}
function _DosSetFileSize(handle,size:longint):word;

{Flush update the changes to a file to disk.}
function _DosResetBuffer(handle:longint):word;

{Duplicate or redirect a handle.
To duplicate a handle: Fill handle with source handle and duplicate with -1.
					   Copy of handle will be returned in duplicate.
To redirect a handle:  Fill handle with handle to which the handle to
					   redirect will be redirected. The handle that will be
					   redirected should be placed in duplicate.}
function _DosDupHandle(handle:longint;var duplicate:longint):word;

{Return information about a specific handle. See _dosopen for a
 description of filemode.}
function _DosQueryFHState(handle:longint;var filemode:longint):word;

{Set information about a specific handle. See _dosopen for a description
 of filemode.}
function _DosSetFHState(handle,filemode:longint):word;

{Usefull constants for the handle type.}
const	dhfile		=    0;
		dhdevice	=    1;
		dhpipe		=    2;
		dhnetwork	= 8192;

{Determine if a handle belongs to a file, a device or a pipe.
 handle				= Handle tp query info about.
 handtype			= Bits 0-1:   00 = File
								  01 = Device
								  02 = Pipe
					  Bit 15:	  0 = Local.
								  1 = On network.}
function _DosQueryHType(handle:longint;var handtype:longint;
						var attr:longint):word;

{****************************************************************************

					File management related routines.

****************************************************************************}


{Edit a filename using wildcard.

Example editing CONFIG.SYS with *.BAK becomes CONFIG.BAK.
Usefull when parsing commands like 'copy config.sys *.bak'.
All filename characters are casemapped.'

metalevel		= 0 Use modern semantics
metalevel		= 1 Use OS/2 1.2 semantics
source			= string to edit
edit			= editstring
target			= destination buffer
cbtarget		= size of the destination buffer}
function _DosEditName(metalevel:longint;source,edit:Pchar;
					  target:Pchar;cbtarget:longint):word;
function _DosEditName(metalevel:longint;const source,edit:string;
					  var target:string):word;

{Move or rename a file.
 Oud    - Old name of file.
 Nieuw	- New name of file.}
function _DosMove(oud,nieuw:Pchar):word;
function _DosMove(const oud,nieuw:string):word;


const	dcexisting=1;			{Overwrite existing files.}
		dcappend=2;				{Append to existing file.}
		dcfailas=4;				{?? Info wanted!}

{Copy a file.
 Oud    - Source-file.
 Nieuw	- Destination-file.}
function _DosCopy(oud,nieuw:Pchar;option:longint):word;
function _DosCopy(const oud,nieuw:string;option:longint):word;

{Delete a file from disk.}
function _DosDelete(filenaam:Pchar):word;
function _DosDelete(const filenaam:string):word;

{Destroy a file on disk. _DosForceDelete makes sure that the file cannot
 be unerased anymore.}
function _DosForceDelete(filenaam:Pchar):word;
function _DosForceDelete(const filenaam:string):word;

{Create a new directory.

naam			= Name of directory to create.
ea				= Extented attributes to give the directory. Use NIL if you
				  do not want do give it extented attributes. Only the FEA
				  list is used.}
function _DosCreateDir(naam:Pchar;ea:PEAOP2):word;
function _DosCreateDir(const naam:string;ea:PEAOP2):word;

{Remove a directory.}
function _DosDeleteDir(naam:Pchar):word;
function _DosDeleteDir(const naam:string):word;

{Set the current drive.}
function _DosSetDefaultDisk(disknum:longint):word;

{Get the current drive.}
function _DosQueryCurrentDisk(var disknum:longint;var logical:longint):word;

{Set the current directory.}
function _DosSetCurrentDir(naam:Pchar):word;
function _DosSetCurrentDir(const naam:string):word;

{Get the current directory.}
function _DosQueryCurrentDir(disknum:longint;var buffer;var buflen:longint):word;
function _DosQueryCurrentDir(disknum:longint;var buffer:string):word;

{****************************************************************************

					  File searching related routines.

****************************************************************************}

const	fareadonly		=  1;
		fahidden		=  2;
		fasystem		=  4;
		fareserve		=  8;
		fadirectory		= 16;
		faarchive		= 32;

		ilstandard		=  1;
		ilqueryEAsize	=  2;
		ilqueryEAs		=  3;
		ilqueryfullname	=  5;

{Format of date records:

 Bit 0..4:		Day.
 Bit 5..8:		Month.
 Bit 9..15:		Year minus 1980.

 Format of time records:

 Bit 0..4:		Seconds divided by 2.
 Bit 5..10		Minutes.
 Bit 11..15:	Hours.}

type	Tfilestatus=object
			datecreation,			{Date of file creation.}
			timecreation,			{Time of file creation.}
			datelastaccess,			{Date of last access to file.}
			timelastaccess,			{Time of last access to file.}
			datelastwrite,			{Date of last modification of file.}
			timelastwrite:word;		{Time of last modification of file.}
			filesize,				{Size of file.}
			filealloc:longint;		{Amount of space the file really
									 occupies on disk.}
		end;
		Pfilestatus=^Tfilestatus;

		Tfilestatus1=object(Tfilestatus)
			attrfile:word;			{Attributes of file.}
		end;
		Pfilestatus1=^Tfilestatus1;

		Tfilestatus2=object(Tfilestatus)
			attrfile:word;
			cblist:longint;
		end;
		Pfilestatus2=^Tfilestatus2;

		Tfilestatus3=object(Tfilestatus)
			attrfile:longint;  		{Attributes of file.}
		end;
		Pfilestatus3=^Tfilestatus3;

		Tfilestatus4=object(Tfilestatus)
			attrfile:longint;
			cblist:longint;
		end;
		Pfilestatus4=^Tfilestatus4;

		Tfilefindbuf1=object(Tfilestatus1)
			name:string;				{Also possible to use as ASCIIZ.
										 The byte following the last string
										 character is always zero.}
		end;
		Pfilefindbuf1=^Tfilefindbuf1;


		Tfilefindbuf2=object(Tfilestatus2)
			name:string;				{Also possible to use as ASCIIZ.
										 The byte following the last string
										 character is always zero.}
		end;
		Pfilefindbuf2=^Tfilefindbuf2;

		Tfilefindbuf3=object(Tfilestatus3)
			name:string;				{Also possible to use as ASCIIZ.
										 The byte following the last string
										 character is always zero.}
		end;
		Pfilefindbuf3=^Tfilefindbuf3;

		Tfilefindbuf4=object(Tfilestatus4)
			name:string;				{Also possible to use as ASCIIZ.
										 The byte following the last string
										 character is always zero.}
		end;
		Pfilefindbuf4=^Tfilefindbuf4;

{Find first file matching a filemask. In contradiction to DOS, a search
 handle is returned which should be closed with findclose when done.
 filemask		= Filemask to search.
 handle			= Search handle will be returned here, fill with -1 before
				  call.
 attrib			= Fileattributes to search for.
 Afilestatus	= Return buffer.
 cbfilestatus	= Size of return buffer.
 count			= Fill with maximum number of files to search for, the
				  actual number of matching files found is returned here.
 infolevel		= One of the ilxxxx constants. Consult IBM documentation
				  for exact meaning. For normal use: Use ilstandard and
				  use Pfilefindbuf3 for Afilestatus.}
function _DosFindFirst(filemask:Pchar;var handle:longint;attrib:longint;
					   Afilestatus:Pfilestatus;cbfilestatus:longint;
					   var count:longint;infolevel:longint):word;

{Find next matching file.}
function _DosFindNext(handle:longint;Afilestatus:Pfilefindbuf;
					  cbfilestatus:longint;var count:longint):word;

{Close a search handle. Cannot fail if handle does exist.}
function _DosFindClose(handle:longint):word;

{Get info about a file.

 handle			= Handle of file.
 infolevel		= One of the ilxxxx constants. Consult IBM documentation
				  for exect meaning. For normal use: Use ilstandard and
				  Pfilefindbuf3 for Afilestatus.
 Afilestatus	= An info return buffer.
 cbfilestatus	= Size of info buffer.}
function _DosQueryFileInfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
						   cbfilestatus:longint):word;

{Set info about a file. File must be opened with write permissions. See
 above fo the parameters.}
function _DosSetFileInfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
						 cbfilestatus:longint):word;

{Return info about a file. In contradiction to the above functions, the
 file does not have to be openened.}
function _DosQueryPathInfo(filenaam:Pchar;infolevel:longint;
						   Afilestatus:Pfilestatus;cbfilestatus:longint):word;
function _DosQueryPathInfo(const filenaam:string;infolevel:longint;
						   Afilestatus:Pfilestatus;cbfilestatus:longint):word;

{Set information about a file.}
function _DosSetPathInfo(filenaam:Pchar;infolevel:longint;
						 Afilestatus:Pfilestatus;cbfilestatus,
						 options:longint):word;

{Get info about the names and lengths of the EA's for a file or directory.

 reftype			= 0 = Afile is a pointer to a file-handle.
					  1 = Afile is a pointer to an ASCIIZ string.
 AFile				= Pointer file's name or handle.
 entry				= Number of EA to query inof about. (1 = first EA).
 buf				= Buffer where requested info is returned. For infolevel
					  1, the buffer is a TfEA2 datastructure.
 buflen				= Size of buf in bytes.
 count				= Number of EA's to return info for. Number of EA's that
					  actually fitted in buf is returned here.
 infolevel			= Level of information to return. Only level 1 is
					  currently allowed.}

function _DosEnumAttribute(reftype:longint;Afile:pointer;
						   entry:longint;var buf;bufsize:longint;
						   var count:longint;infolevel:longint):word;
{A variant for strings is of course also included.}
function _DosEnumAttribute(const filenaam:string;
						   entry:longint;var buf;bufsize:longint;
						   var count:longint;infolevel:longint):word;

{Get an environment variabele.
 naam				= Name of environment variabele to get.
 value				= Receives pointer to environment string.}
function _DosScanEnv(naam:Pchar;var value:Pchar):word;
{There is, of course a string variant.}
function _DosScanEnv(const naam:Pchar;var value:string):word;

const	dspathonly		= 0;	{Do not search current dir. (Unless it is
								 in the directory list.)}
		dscurrentdir	= 1;	{Search in the current direcotry and in the
								 directory list.}
		dsenvironment	= 2;	{The dirlist parameter is not a directory
								 list, but an environment variabele
								 containing one.}
		dsignoreneterrs	= 4;	{Ignore network errors when searching.}

{Search for a file in a given number of directories.
 flags			= A combination of the dsxxxx constants.
 dirlist		= Directory list or environmant variabele containing list
				  to search in.
 filenaam		= Filename to search for. May contain wildcards.
 fullname		= Receives filename found, including path.
 fulllen		= Length of your fullname buffer.}
function _DosSearchPath(flag:longint;dirlist,filenaam:Pchar;
						fullname:Pchar;fulllen:longint):word;
function _DosSearchPath(flag:longint;const dirlist,filenaam:string;
						var fullname:string):word;

{****************************************************************************

					   File system related routines.

****************************************************************************}

type	TFSinfo=record
			case word of
				1:
					(file_sys_ID,
					 sectors_per_cluster,
					 total_clusters,
					 free_clusters:longint;
					 bytes_per_sector:word);
				2:          				{For date/time description,
											 see file searching realted
											 routines.}
					(label_date, 			{Date when volumelabel created.}
					 label_time:word;		{Time when volumelabel created.}
					 volumelabel:string);	{Volume label. Can also be used
											 as ASCIIZ, because the byte
											 following the last character of
											 the string is always zero.}
		end;
		PFSinfo=^TFSinfo;

		Tattachdata=record
			case integer of			{Flag in [0,1,2].}
				0,1:  				{Flag = 0.}
					(count:word;
					 data:Tchararray);
				2:					{Flag = 2.}
					(pipehandle:longint;
									{Handle of named pipe opened by spooler.}
					 spoolname:string);
									{Name of spooler object. Can also be used
									 as ASCIIZ, because the bute following
									 the last character is always zero.}
		end;
		Pattachdata=^Tattachdata;

		TFSqbuffer2=record
			_type:word;
			naamlen:word;
			FSDnaamlen:word;
			FSAdatalen:word;
{!!!!!!}	szName:array[0..1-1] of UCHAR;
			szFSDName:array[0..1-1] of UCHAR;
			rgFSAData:array[0..1-1] of UCHAR;
		end;
		PFSqbuffer2=^TFSQbuffer2;

const	fsattach		= 0;	{Attach a drive.}
		fsdetach		= 1;	{Detach a drive.}
		fsspoolattach	= 2;	{Attach a spool device.}
		fsspooldetach	= 3;	{Detach a spool device.}

{IBM DOCS: "DosFSAttach attaches or detaches a drive to or from a remote file
 system driver (FSD), or a pseudocharacter device name to or from a local or
 remote FSD."

 devnaam			= When flag is 0 or 1, the name of a drive or a pseudo-
					  character device. When using a drivename use the drive-
					  letter followed by a colon.
					  When flag is 2 or 3, the name of a spooled device.
 filesystem			= Name of the driver that should be attached or detached
					  to devnaam. Use nil when flag is 2 or 3.
 data				= Should contain a number of ASCIIZ strings that will
					  be passed to the filesystem driver when flag is 0 or 1.
					  Should contain de pipehandle and spoolname when flag is
					  2. Should be nil when flag is 3.
 datalen			= Number of bytes in data parameter.
 flag				= One of the dsxxxx constants. See above}
function _DosFSAttach(devnaam,filesystem:Pchar;var data:Tattachdata;
					  datalen:longint;flag:longint):word;
function _DosFSAttach(const devnaam,filesystem:string;var data:Tattachdata;
					  datalen:longint;flag:longint):word;

{IBMDOCS: "DosQueryFSAttach obtains information about an attached file system
 (local or remote), or about a character device or pseudocharacter device
 attached to the file system."

 devnaam			= Name info drive or pseudo character device to query
					  info about. Ignored for infolevels 2 and 3.
 ordinal			= Index into list of character/pseudo-character
					  devices. Starts at 1. Ignored for infolevel 1.
 infolevel			= 1 = Return information about a drive or device named
						  by devnaam.
					  2 = Return information about a (pseudo) charachter
						  device numbered by ordinal.
					  3 = Return information about a drive numbered by
						  ordinal.
 buffer				= Will be filled with infomation.
 buflen				= Size of your buffer in bytes. Number of bytes filled
					  in your buffer is returned here.}
function _DosQueryFSAttach(devnaam:Pchar;ordinal,infolevel:longint;
						   var buffer:TFSqbuffer2;var buflen:longint):word;
function _DosQueryFSAttach(const devnaam:string;ordinal,infolevel:longint;
						   var buffer:TFSqbuffer2;var buflen:longint):word;

const	FSCTL_HANDLE=1;
		FSCTL_PATHNAME=2;
		FSCTL_FSDNAME=3;
		FSCTL_ERROR_INFO=1;
		FSCTL_MAX_EASIZE=2;

{IBMDOCS: "DosFSCtl provides an extended standard interface between an
 application and a file-system driver (FSD).

 Consult IBM documentation about this function..}
function _DosFSCtl(data:pointer;datalen:longint;var resdatalen:longint;
				   parms:pointer;parmslen:longint;var parmslen:longint;
				   _function:longint;route:Pchar;
				   handle,method:longint):word;
function _DosFSCtl(data:pointer;datalen:longint;var resdatalen:longint;
				   parms:pointer;parmslen:longint;var parmslen:longint;
				   _function:longint;const route:string;
				   handle,method:longint):word;

{Get information about a drive.
Infolevels:
 1				Get total/free space etc.
 2				Get volumelabel.}
function _DosQueryFSInfo(disknum,infolevel:longint;var buffer:TFSinfo;
						 buflen:longint):word;

{Set information about a drive.}
function _DosSetFSInfo(disknum,infolevel:longint;var buffer:TFSinfo;
					   buflen:longint):word;

{Check if verify mode is enabled.}
function _DosQueryVerify(var enabled:longint):word;

{Turn the verify mode on or off.}
function _DosSetVerify(enable:longint):word;

{Change the number of filehandles our program can open. (Default=50).}
function _DosSetMaxFH(count:longint):word;

	function _DosSetRelMaxFH(pcbReqCount:PLONG;pcbCurMaxFH:PULONG):word;

const	dsfull=0;		{IBM DOCS: "Perform full system shutdown and
						 file-system lock."}
		dsquiescient=1;	{IBM DOCS: "Perform buffer and cache flushing to
						 make system quiescent."}

{Prepare the system for shutdown.}
function _DosShutdown(flags:longint):word;

{****************************************************************************

					   Memory allocation related routines.

****************************************************************************}

const	mfpag_read		= $00001;	{Give read access to memory.}
		mfpag_write		= $00002;	{Give write access to memory.}
		mfpag_execute	= $00004;	{Allow code execution in memory.}
		mfpag_guard		= $00008;	{Used for dynamic memory growing. Create
									 uncommitted memory and make the first
									 page guarded. Once it is accessed it
									 will be made committed, and the next
									 uncommitted page will be made guarded.}
		mfpag_commit	= $00010;	{Make the memory committed.}
		mfpag_decommit	= $00020;	{Decommit the page.}
		mfobj_tile		= $00040;	{Also allocate 16-bit segments of 64k
									 which map the memory. (Makes 16<>32 bit
									 pointer conversion possible.}
		mfobj_protected = $00080;
		mfobj_gettable	= $00100;
		mfobj_giveable	= $00200;
		mfpag_default	= $00400;
		mfpag_shared	= $02000;
		mfpag_free		= $04000;
		mfpag_base		= $10000;

		mfsub_init		= $00001;	{Use base, if not set, choose a base
									 address yourself.}
		mfsub_grow		= $00002;	{Grow the specified heap, instead of
									 allocating it. Ignore fmsub_init.}
		mfsub_sparse	= $00004;
		mfsub_serialize	= $00008;

{Get some memory.
 p			= Pointer to memory will be returned here.
 size		= Number of bytes to get. The size is rounded up to a multiple
			  of 4096. This is propably not the case on non-intel 386
			  versions of OS/2.
 flags		= One or more of the mfxxxx constants.}
function _DosAllocMem(var p:pointer;size:longint;flag:longint):word;

{Free a memory block.}
function _DosFreeMem(p:pointer):word;

{Set settings for a block of memory.
 p			= Pointer to the memory. Doesn't need to be the start of the
			  memory block allocated with dosallocmem, but must be a multiple
			  of 4096.
 cb			= Number of bytes to change settings for. Is rounded up to a
			  multile of 4096.
 flags		= New flags for the memory.}
function _DosSetMem(p:pointer;cb,flag:longint):word;

{Give another process access to a shared memory block.

 p			= Pointer to the shared memory object.
 pid		= Process of destination process.
 flag		= Permissions the the destination process gets.}
function _DosGiveSharedMem(p:pointer;pid,flag:longint):word;

{Get access to a shared memory object.

 p			= Pointer to shared memory object.
 flag		= Permissions to ask.}
function _DosGetSharedMem(p:pointer;flag:longint):word;

{Get access to a shared memory object that has a name.

 p			= Pointer to shared memory object.
 naa,		= Name of the memory object. (Starting with '\SHAREMEM\'.
 flag		= Permissions to ask.}
function _DosGetNamedSharedMem(var p:pointer;naam:Pchar;flag:longint):word;
function _DosGetNamedSharedMem(var p:pointer;const naam:string;flag:longint):word;

{Allocate memory so that it can later be shared with another program.
 p			= Reveives pointer to memory.
 naam		= Optional: name to give memory. Must start with '\SHAREMEM\'.
			  Use nil for the Pchar or '' for the string variant for no name.
 cm			= Number of bytes to allocate.}
function _DosAllocSharedMem(var p:pointer;naam:Pchar;cb:longint;flag:longint):word;
function _DosAllocSharedMem(var p:pointer;const naam:string;cb:longint;flag:longint):word;

{Get the size and flags of a block of memory.

 p			= Pointer to the block of memory.
 size		= Receives block size.
 flag		= Receives the flags.}
function _DosQueryMem(p:pointer;var size,flag:longint):word;

{Allocate a block of memory in a heap.
 base		= Pointer to the start of the heap.
 p			= Receives pointer to the memory bock.
 cb			= Number of bytes to allocate.}
function _DosSubAllocMem(base:pointer;var p:pointer;cb:longint):word;

{Free a block of memory in a heap.
 base		= Pointer to the start of the heap.
 p			= Pointer to memory block to free.
 cb			= Number of bytes to free.}
function _DosSubFreeMem(base,p:pointer;cb:longint):word;

{Turn a block of memory into a heap.

base		= Pointer to memory block to turn into a heap.
flag		= One or more of the mfsub_xxxx flags.
cb			= Size of the requested heap.}
function _DosSubSetMem(base:pointer;flag:longint;cb:longint):word;

{Destroy a heap. (Memory remains allocated.

base		= Pointer to the heap to destroy.}
function _DosSubUnsetMem(base:pointer):word;

{****************************************************************************

						Semaphore related routines

****************************************************************************}

const	smshared		= $0001;	{Semafore is shared.}
		smMWwaitany		= $0002;	{Muxwait only: Wait until a semafore
									 is cleared.}
		smMWwaitall		= $0004;	{Muxwait only: Wait until all semafores
									 are cleared.}

type   Psemrecord=^Tsemrecord;
	   Tsemrecord=record
		  semafore,					{Handle of semafore to link.}
		  user:longint;
	   end;

	   Psemarray=^Tsemarray;
	   Tsemarray=array[0..$ffff] of Tsemrecord;

{Create an event semafore.
 naam		= Optional: Name of semafore to create. Must start with '\SEM32\.
			  Use nil for Pchar or '' for string variant for noname. A
			  unnamed semafore is not shared unless the sm_shared flag is
			  set.
 handle		= Receives handle of semafore.
 attr		= One or more of the smxxxx constants.
 state		= Initial state: 0 = Reset (False), 1 = Posted (True).}
function _DosCreateEventSem(naam:Pchar;var handle:longint;
							attr,state:longint):word;
function _DosCreateEventSem(const naam:string;var handle:longint;
							attr,state:longint):word;

{Open a semafore created by another process or thread.

 naam		= Name of semafore.
 handle		= Receives handle of semafore.}
function _DosOpenEventSem(naam:Pchar;var handle:longint):word;
function _DosOpenEventSem(const naam:string;var handle:longint):word;

{Close an event semafore.
 handle		= Handle of semofore to close.}
function _DosCloseEventSem(handle:longint):word;

{Reset an event semafore: set it to false.
 handle		= Handle of semafore.
 postcount	= Number of times _DosPostEventSem has been called since last
			  reset.

 Note:		Returns errorcode 300 if semafore is already reset.}
function _DosResetEventSem(handle:longint;var postcount:longint):word;

{Post an event semafore: set it to true.
 handle		= Handle of semafore.

Note:		Returns errorcode 299 if semafore is already posted.}
function _DosPostEventSem(handle:longint):word;

{Wait until an event semafore is posted (set to true).
 handle		= Handle of semafore.
 timeout	= Return with errorcode if timeout milliseconds have past and the
			  semafore is still reset. To return immideatly use 0,
			  to wait forever use -1.}
function _DosWaitEventSem(handle,timeout:longint):word;

{Check if an event semafore is posted.
 handle		= Handle of semafore.
 posted		= Receives number of times _DosPostEventSem was called since
			  the last reset.}
function _DosQueryEventSem(handle:longint;var posted:longint):word;

{Create a Mutual Exclusion semafore (Mutex).
 naam		= Optional: Name to give semafore. Must start with '\SEM32\'.
			  Use nil for Pchar or '' for string variant to use no name.
			  If a name if used the semafore is shared.
 handle		= Receives handle of semafore.
 attr		= One or more of the smxxxx constants.
 state		= Initial state: (0=Unowned, 1=Owned.)}
function _DosCreateMutexSem(naam:Pchar;var handle:longint;
							attr,state:longint):word;
function _DosCreateMutexSem(const naam:string;var handle:longint;
							attr,state:longint):word;

{Open a shared mutex semafore.
 naam		= Name of semafore to open, always starts with '\SEM32\'.
 handle		= Receives handle to semafore.}
function _DosOpenMutexSem(naam:Pchar;var handle:longint):word;
function _DosOpenMutexSem(const naam:string;var handle:longint):word;

{Close a mutex semafore.
 handle		= Handle of semafore to close.}
function _DosCloseMutexSem(handle:longint):word;

{Request ownership of a mutex semafore. If the semafore is already owned the
 process is halted until the semafore is released.
 handle		= Handle of semafore.
 timeout	= Return with errorcode if the semafore is still owned after
			  timeout milliseconds.}
function _DosRequestMutexSem(handle,timeout:longint):word;

{Release the ownership of a mutex semafore.
 handle		= Handle of semafore to release.}
function _DosReleaseMutexSem(handle:longint):word;

{Query the pid and tib of the owner of a mutex semafore.
 handle		= Handle of semafore.
 pid		= Receives process id of owner.
 tib		= Receives thread if of owner.
 count		= Number of threads (within and outside current process) waiting
			  for ownership of semafore.}
function _DosQueryMutexSem(handle:longint;var pid,tid,count:longint):word;

{Create a Multiple Wait (Muxwait) semafore.
 naam		= Optional: Name to give semafore. Must start with '\SEM32\'.
			  Use nil for Pchar or '' for string variant to use no name.
			  If a name if used the semafore is shared.
 handle		= Receives handle of semafore.
 csemrec	= Number of semafores to link muxwait semafore with.
 semarray	= Array of semafore records to link with muxwait semafore.
 attr		= One or more of the smxxxx constants.}
function _DosCreateMuxWaitSem(naam:Pchar;var handle:longint;csemrec:longint;
							  var semarray:Tsemarray;attr:longint):word;
function _DosCreateMuxWaitSem(const naam:string;var handle:longint;
							  csemrec:longint;var semarray:Tsemarray;
							  attr:longint):word;

{Open a muxwait semafore.
 naam		= Name of semafore to open.
 handle		= Receives handle of semafore.}
function _DosOpenMuxWaitSem(naam:Pchar;var handle:longint):word;
function _DosOpenMuxWaitSem(const naam:string;var handle:longint):word;

{Close a mutex semafore.}
function _DosCloseMuxWaitSem(handle:longint):word;

{Wait for the muxwait semafore to be cleared.
 handle		= Handle of semafore.
 timeout	= Timeout. See above.
 user		= Receives user value of the semafore that caused the muxwait
			  semafore to be cleared.}
function _DosWaitMuxWaitSem(handle,timeout:longint;var user:longint):word;

{Add a semafore to the muxwait semafore.

 handle		= Handle of semafore.
 semrec		= The semafore to add.}
function _DosAddMuxWaitSem(handle:longint;var semrec:Tsemrecord):word;

{Remove a semafore from the muxwait semafore.
 handle		= Handle of muxwait semafore.
 sem		= Handle of semafore to remove.}
function _DosDeleteMuxWaitSem(handle,sem:longint):word;

{Query the semafores from a muxwait semafore.
 handle		= Handle of semafore.
 csemrec	= Input: Size of our array. Output: Number of items in array.
 semrecs	= Array where Tsemrecords are stored.
 attr		= Flags used by creation of semafore.}
function _DosQueryMuxWaitSem(handle:longint;var csemrec:longint;
							 var semrecs:Tsemarray;var attr:longint):word;

{****************************************************************************

						Timing related routines.

****************************************************************************}


type	Tdatetime=record
			hour,
			minute,
			second,
			sec100,
			day,
			month:byte;
			year:word;
			timezone:integer;
			weekday:byte;
		end;

		Pdatetime=^Tdatetime;


{Get the date and time.}
function _DosGetDateTime(var buf:Tdatetime):word;

{Set the date and time.}
function _DosSetDateTime(var buf:Tdatetime):word;

{Start a one shot timer.
 msec		= Number of miliseconds the timer will run.
 hsem		= Handle of event semafore that is posted when time has expired.
 TIMhandle	= Receives timer handle.}
function _DosAsyncTimer(msec:longint;hsem:SEMhandle;
						var TIMhandle:longint):word;

{Start a cyclic timer.
 msec		= Number of miliseconds the timer will run.
 hsem		= Handle of event semafore that is posted when time has expired.
 TIMhandle	= Receives timer handle.}
function _DosStartTimer(msec:longint;hsem:SEMhandle;
						var TIMhandle:longint):word;

{Stop a timer and destroy it's handle.}
function _DosStopTimer(TIMhandle:longint):word;

{****************************************************************************

							 DLL specific routines.

****************************************************************************}

{Load a DLL in memory if it is not yet loaded.
 objnaam		= When the DLL cannot be found, or one of the DLL's it needs
				  cannot be found, the name of the DLL will be put here.
 objlen			= Size of the objnaam result buffer.
 DLLnaam		= Name of DLL to load. Do not give an extension or a path,
				  just the name. OS/2 will automatically search through the
				  LIBPATH for the DLL.
 handle			= Receives DLL handle.}
function _DosLoadModule(objnaam:Pchar;objlen:longint;DLLnaam:Pchar;
						var handle:longint):word;
function _DosLoadModule(var objnaam:string;objlen:lognint;
						const DLLnaam:string;var handle:longint):word;

{Let OS/2 know that we do not need a DLL anymore. If we were the only process
 using the DLL, it is unloaded.}
function _DosFreeModule(handle:longint):word;

{Get the address of a procedure.
 handle			= DLL handle,
 ordinal		= Procedure to get address form. 0=Use it's name.
 procname		= Name of the procedure to query address for. Must be nil
				  for Pchar or '' for string variant if ordinal is nonzero.
 address		= Receives address of procedure.}
function _DosQueryProcAddr(handle:longint;ordinal:longint;procname:Pchar;
						   var address:pointer):word;
function _DosQueryProcAddr(handle:longint;ordinal:longint;
						   const procname:Pchar;var address:pointer):word;

{Get the handle of a loaded DLL or a loaded executable.
 DLLnaam		= Name of DLL.
 handle			= Receives DLL handle if present.}
function _DosQueryModuleHandle(DLLnaam:Pchar;var handle:longint):word;
function _DosQueryModuleHandle(const DLLnaam:string;var handle:longint):word;

{Get the pathname of a loaded DLL or a loaded executable.

 handle			= Handle of DLL.
 naamlen		= Maximum length of char array.
 naam			= Chararray (or string) where name is returned.}
function _DosQueryModuleName(handle:longint;naamlen:longint;naam:Pchar):word;
function _DosQueryModuleName(handle:longint;var naam:string):word;

const	pt16bit=0;
		pt32bit=1;

{Return if a procedure is either 16 or 32 bit.
 handle			= Handle of DLL.
 ordinal		= DLL index number. 0 means use naam.
 naam			= Must be nil for Pchar or '' for string variant if ordinal
				  is zero. Otherwise it contains the procname.
 proctype		= One of the ptxxxx constants.}
function _DosQueryProcType(handle:longint;ordinal:longint;naam:Pchar;
						   var proctype:longint):word;
function _DosQueryProcType(handle:longint;ordinal:longint;naam:string;
						   var proctype:longint):word;

{****************************************************************************

						   Resource related routines.

****************************************************************************}

const   RT_POINTER=1;
		RT_BITMAP=2;
		RT_MENU=3;
		RT_DIALOG=4;
		RT_STRING=5;
		RT_FONTDIR=6;
		RT_FONT=7;
		RT_ACCELTABLE=8;
		RT_RCDATA=9;
		RT_MESSAGE=10;
		RT_DLGINCLUDE=11;
		RT_VKEYTBL=12;
		RT_KEYTBL=13;
		RT_CHARTBL=14;
		RT_DISPLAYINFO=15;
		RT_FKASHORT=16;
		RT_FKALONG=17;
		RT_HELPTABLE=18;
		RT_HELPSUBTABLE=19;
		RT_FDDIR=20;
		RT_FD=21;
		RT_MAX=22;
		RF_ORDINALID=$80000000;

{Get the address of a resource object.
 handle			= Handle of DLL (or executable) to get resource from.
 restype		= One of the RT_xxxx constants.
 resnaam		= Number associated to resource object by resource compiler.}
function _DosGetResource(handle,restype,resnaam:longint;var p:pointer):word;

{Remove a resource object from memory.
 p				= Pointer to resource.}
function _DosFreeResource(p:pointer):word;

{Get the size of a resource object.
 handle			= Handle to DLL (or executable).
 idt			= One of the RT_xxxx constants.
 idn			= Number associated to resource object by resource compiler.
 size			= Receives resource size.}
function _DosQueryResourceSize(handle,idt,idn:longint;var size:longint):word;


{****************************************************************************

				   Country and codepage specific routines.

****************************************************************************}

const	currentcountry:Tcountrycode=(country:0;codepage:0);

type	Tcountrycode=record
			country,			{Country to query info about (0=current).}
			codepage:longint;	{Code page to query info about (0=current).}
		end;

		Ttimefmt=(clock12,clock24);

		Tcountryinfo=record
			country,codepage,				{Country and codepage requested.}
			dateformat:longint;				{1=ddmmyy 2=yymmdd 3=mmddyy}
			currencyunit:array[0..4] of char;
			thousandseparator:char;			{Thousands separator.}
			zero1:byte;						{Always zero.}
			decimalseparator:char;			{Decimals separator,}
			zero2:byte;
			dateseparator:char				{Date separator.}
			zero3:byte;
			timeseparator:char;				{Time separator.}
			zero4:byte;
			currencyformat,					{Bit field:
											 Bit 0:	0=indicator before value
													1=indicator after value
											 Bit 1: 1=insert space after
													  indicator.
											 Bit 2: 1=Ignore bit 0&1, replace
													  decimal separator with
													  indicator.}
			decimalplace:byte;				{Number of decimal places used in
											 currency indication.}
			timeformat:Ttimefmt;			{12/24 hour.}
			reserve1:array[0..1] of word;
			dataseparator:char;				{Data list separator.}
			zero5:byte;
			reserve2:array[0..4] of word;
		end;
		Pcountryinfo=^Tcountryinfo;

		TDBCSrange=record
			start,stop:byte;
		end;

		TDBCSarray=array[0..$ffff] of TDBCSrange;
		PDBCSarray=^TDBCSarray;

{Get country specific information.
	cb 			= Size of our datastructure. (sizeof(Tcountryinfo))
	cbactual	= Size of OS/2's datastructure. cbactual bytes of
				  our Tcountryinfo have been filled.}
function _DosQueryCtryInfo(cb:longint;var country:Tcountrycode;
						   var res:Tcountryinfo;var cbactual:longint):word;

{Get info about a code page with a DBCS character set.}
function _DosQueryDBCSEnv(cb:longint;var country:Pcountrycode;buf:Pchar):word;

{Convert a string to uppercase.
	cb			= Length of string.
	country		= Country and codepage for converting.
	Astring		= String to convert.}
function _DosMapCase(cb:longint;var country:Tcountrycode;
					 Astring:Pchar):word;
function _DosMapCase(cb:longint;var country:Tcountrycode;
					 var Astring:string):word;

{Get a collate table. (A table which says which character if a character is
 smaller than another.
	cb			= Length of the databuffer the program has.
	country		= Country to query table for. (0,0) is default country and
				  codepage.
	buf			= Buffer to return table in. It's filled with the sort
				  weights of the ascii code. For example the 128th byte
				  contains the weight for ascii code 128.
	tablelen	= Length of collating table.}
function _DosQueryCollate(cb:longint;var country:Tcountrycode;
						  buf:Pbytearray;var tablelen:longint):word;

{Get the current codepage. The Pwordarray is filled with the current code
 page followed by alternative codepages.}
function _DosQueryCp(cb:longint;codepages:Pwordarray;var actcb:longint):word;

{Change the codepage, but only for the current process.}
function _DosSetProcessCp(cp:longint):word;

{****************************************************************************

					   Exception handling related functions

****************************************************************************}


{Exception constants.}
const		XCPT_continue_search			= $00000000;
			XCPT_continue_execution			= $ffffffff;
			XCPT_continue_stop				= $00716668;

			XCPT_signal_intr				= $1;
			XCPT_signal_killproc			= $3;
			XCPT_signal_break				= $4;

			XCPT_fatal_exception			= $c0000000
			XCPT_severity_code				= $c0000000
			XCPT_customer_code				= $20000000
			XCPT_facility_code				= $1fff0000
			XCPT_exception_code				= $0000ffff

			XCPT_unkwown_access				= $00000000
			XCPT_read_access				= $00000001
			XCPT_write_access				= $00000002
			XCPT_execute_access				= $00000004
			XCPT_space_access				= $00000008
			XCPT_limit_access				= $00000010
			XCPT_data_unknown				= $ffffffff

			XCPT_guard_page_violation		= $80000001
			XCPT_unable_to_grow_stack		= $80010001
			XCPT_access_violation			= $c0000005
			XCPT_in_page_error				= $c0000006
			XCPT_illegal_instruction		= $c000001c
			XCPT_invalid_lock_sequence		= $c000001d
			XCPT_noncontinuable_exception	= $c0000024
			XCPT_invalid_disposition		= $c0000025
			XCPT_unwind						= $c0000026
			XCPT_bad_stack					= $c0000027
			XCPT_invalid_unwind_target		= $c0000028
			XCPT_array_bounds_exceeded		= $c0000093
			XCPT_float_denormal_operand		= $c0000094
			XCPT_float_divide_by_zero		= $c0000095
			XCPT_float_inexact_result		= $c0000096
			XCPT_float_invalid_operation	= $c0000097
			XCPT_float_overflow				= $c0000098
			XCPT_float_stack_check			= $c0000099
			XCPT_float_underflow			= $c000009a
			XCPT_integer_divide_by_zero		= $c000009b
			XCPT_integer_overflow			= $c000009c
			XCPT_privileged_instruction		= $c000009d
			XCPT_datatype_misalignment		= $c000009e
			XCPT_breakpoint					= $c000009f
			XCPT_single_step				= $c00000a0
			XCPT_process_terminate			= $c0010001
			XCPT_async_process_terminate	= $c0010002
			XCPT_signal						= $c0010003

{!!!!!!!!    function _DosSetExceptionHandler(pERegRec:PEXCEPTIONREGISTRATIONRECORD):word;

	function _DosUnsetExceptionHandler(pERegRec:PEXCEPTIONREGISTRATIONRECORD):word;

	function _DosRaiseException(pexcept:PEXCEPTIONREPORTRECORD):word;

	function _DosSendSignalException(pid:PID;exception:longint):word;

	function _DosUnwindException(phandler:PEXCEPTIONREGISTRATIONRECORD;pTargetIP:pointer;pERepRec:PEXCEPTIONREPORTRECORD):word;
}
{Full screen applications can get ctrl-c and ctrl-brk focus. For all
 processed sharing one screen, only only v=can have ctrl-c focus.
 enable		= 0 = Release focus, 1 = Get focus.
 times		= Number of times focus has been get minus number of times it
			  has been released.}
function _DosSetSignalExceptionFocus(enable:longint;var times:longint):word;

{Tell OS/2 that if an exception occurs, it must queue it up, until a
 _DosExitMustComplete follows. Urgent exceptions still occur. The only
 possible error is that the nesting becomes too high, so error checking
 is only needed in seldom cases.
 nesting	= Number of _DosEnterMustComplete calls minus number of
			  _DosExitMustComplete calls.}
function _DosEnterMustComplete(var nesting:longint):word;

{Tell OS/2 that it can send exceptions again. See above}
function _DosExitMustComplete(var nesting:longint):word;

{Tell we want further signal exceptions.
 signalnum	= Signal nummer to acknowlegde.}
function _DosAcknowledgeSignalException(signalnum:longint):word;


{****************************************************************************

						   Queue related routines.

****************************************************************************}

type	Trequestdata=record
			pid,				{ID iof process that wrote element.}
			data:longint;		{Information from process writing the data.}
		end;
		Prequestdata=^Trequestdata;

{Usefull constants for priority parameters.}
const	quFIFO=0;
		quLIFO=1;
		qupriority=2;
		qunoconvert_address=0;
		quconvert_address=4;

{Close a queue. If the calling process has created the queue, it is
 destroyed.}
function _DosCloseQueue(handle:longint):word;

{Create a queue. The process that creates a queue, owns that queue, and is
 the only one who can read from that queue. Other processes can only write
 to that queue. The queuename must have the format '\QUEUES\name.ext' .

 handle			= Receives queue handle.
 priority		= 0 = Use FIFO system.
				  1 = Use LIFO system.
				  2 = Use priority system.
				  Add 4 to convert addresses of data inserted by 16-bit
				  processes to 32 bit pointers.
 naam			= Name of queue to create.}
function _DosCreateQueue(var handle:longint;priority:longint;
						 naam:Pchar):word;
function _DosCreateQueue(var handle:longint;priority:longint;
						 const naam:string):word;

{Open an existing queue. You cannot read from the queue unless you are the
 process that created it. The name must have the format '\QUEUES\name.ext'}
function _DosOpenQueue(var parent_pid:longint;var handle:longint;
					   naam:Pchar):word;
function _DosOpenQueue(var parent_pid:longint;var handle:longint;
					   const naam:string):word;

{Read a record from a queue, but do not remove it from the queue.
 handle			= Handle of queue to read from.
 reqbuffer		= Receives information about read data.
 datalen		= Receives length of data read.
 dataptr		= Receives the address of the data.
 element		= 0 = Return first element in queue.
				  1 = Return next element in queue. Can be repeated.
					  Current element number is returned here, for use
					  with dosreadqueue.
 wait			= 0 = Wait until there is a queue element available.
				  1 = Return with an error when queue is empty.
 priority		= Receives priority of queue record (1..15).
 Asem			= Use NIL if wait=0, give a handle of a semafore when
				  wait=1. The semafore will be cleared when there is an
				  element inserted it the queue.
				  !! event queue}
function _DosPeekQueue(handle:longint;var reqbuffer:Trequestdata;
					   var datalen:longint;var dataptr:pointer;
					   var element:longint;wait:longint;
					   var priority:byte;Asem:SEMhandle):word;

{Empty a queue. You must be the process the created the queue.}
function _DosPurgeQueue(handle:longint):word;

{Return the number of elements in the queue.}
function _DosQueryQueue(handle:longint;var count:longint):word;

{Read a record from a queue, but do not remove it from the queue.
 handle			= Handle of queue to read from.
 reqbuffer		= Receives information about read data.
 datalen		= Receives length of data read.
 dataptr		= Receives the address of the data.
 element		= 0 = Return first element in queue.
				  Otherwise: Return the element numbered with this.
 wait			= 0 = Wait until there is a queue element available.
				  1 = Return with an error when queue is empty.
 priority		= Receives priority of queue record (1..15).
 Asem			= Use NIL if wait=0, give a handle of a semafore when
				  wait=1. The semafore will be cleared when there is an
				  element inserted it the queue.
				  !! event queue}
function _DosReadQueue(handle:longint;var reqbuffer:Trequestdata;
					   var datalen:longint;var dataptr:pointer;
					   element,wait:longint;var priority:byte;
					   Asem:semhandle):word;

{Write a data record to a queue.
 handle			= Handle of queue to write to.
 request		= Value that will be inserted in the requestdata field when
				  element is read from queue.
 datalen		= Size of data to write.
 databuf		= Data to write.
 priority		= Priority of data in buffer. On relevant when queue is
				  created with priority support.}
function _DosWriteQueue(handle,request,datalen:longint;var databuf;
						priority:longint):word;

	const
	   DSP_IMPLIEDCUR=1;
	   DSP_PATHREF=2;
	   DSP_IGNORENETERR=4;
	   QSV_MAX_PATH_LENGTH=1;
	   Q_MAX_PATH_LENGTH=QSV_MAX_PATH_LENGTH;
	   QSV_MAX_TEXT_SESSIONS=2;
	   QSV_MAX_PM_SESSIONS=3;
	   QSV_MAX_VDM_SESSIONS=4;
	   QSV_BOOT_DRIVE=5;
	   QSV_DYN_PRI_VARIATION=6;
	   QSV_MAX_WAIT=7;
	   QSV_MIN_SLICE=8;
	   QSV_MAX_SLICE=9;
	   QSV_PAGE_SIZE=10;
	   QSV_VERSION_MAJOR=11;
	   QSV_VERSION_MINOR=12;
	   QSV_VERSION_REVISION=13;
	   QSV_MS_COUNT=14;
	   QSV_TIME_LOW=15;
	   QSV_TIME_HIGH=16;
	   QSV_TOTPHYSMEM=17;
	   QSV_TOTRESMEM=18;
	   QSV_TOTAVAILMEM=19;
	   QSV_MAXPRMEM=20;
	   QSV_MAXSHMEM=21;
	   QSV_TIMER_INTERVAL=22;
	   QSV_MAX_COMP_LENGTH=23;
	   QSV_MAX=QSV_MAX_COMP_LENGTH;
	   FERR_DISABLEHARDERR=$00000000;
	   FERR_ENABLEHARDERR=$00000001;
	   FERR_ENABLEEXCEPTION=$00000000;
	   FERR_DISABLEEXCEPTION=$00000002;
	   SIS_MMIOADDR=0;
	   SIS_MEC_TABLE=1;
	   SIS_SYS_LOG=2;

	function _DosError(error:longint):word;

{****************************************************************************

						  Message specific routines.

****************************************************************************}


	function _DosGetMessage(pTable:PPCHAR;cTable:longint;pBuf:PCHAR;cbBuf:longint;msgnumber:longint;pszFile:PSZ;pcbMsg:PULONG):word;

	function _DosErrClass(code:longint;pClass:PULONG;pAction:PULONG;pLocus:PULONG):word;

	function _DosInsertMessage(pTable:PPCHAR;cTable:longint;pszMsg:PSZ;cbMsg:longint;pBuf:PCHAR;cbBuf:longint;pcbMsg:PULONG):word;

	function _DosPutMessage(hfile:HFILE;cbMsg:longint;pBuf:PCHAR):word;

	function _DosQuerySysInfo(iStart:longint;iLast:longint;pBuf:pointer;cbBuf:longint):word;

	function _DosQueryMessageCP(pb:PCHAR;cb:longint;pszFilename:PSZ;cbBuf:PULONG):word;

	function _DosQueryRASInfo(Index:longint;Addr:PPVOID):word;

	const
	   SSF_RELATED_INDEPENDENT=0;
	   SSF_RELATED_CHILD=1;
	   SSF_FGBG_FORE=0;
	   SSF_FGBG_BACK=1;
	   SSF_TRACEOPT_NONE=0;
	   SSF_TRACEOPT_TRACE=1;
	   SSF_TRACEOPT_TRACEALL=2;
	   SSF_INHERTOPT_SHELL=0;
	   SSF_INHERTOPT_PARENT=1;
	   SSF_TYPE_DEFAULT=0;
	   SSF_TYPE_FULLSCREEN=1;
	   SSF_TYPE_WINDOWABLEVIO=2;
	   SSF_TYPE_PM=3;
	   SSF_TYPE_VDM=4;
	   SSF_TYPE_GROUP=5;
	   SSF_TYPE_DLL=6;
	   SSF_TYPE_WINDOWEDVDM=7;
	   SSF_TYPE_PDD=8;
	   SSF_TYPE_VDD=9;
	   SSF_CONTROL_VISIBLE=$0000;
	   SSF_CONTROL_INVISIBLE=$0001;
	   SSF_CONTROL_MAXIMIZE=$0002;
	   SSF_CONTROL_MINIMIZE=$0004;
	   SSF_CONTROL_NOAUTOCLOSE=$0008;
	   SSF_CONTROL_SETPOS=$8000;

	type
	   STATUSDATA=record
		  Length:word;
		  SelectInd:word;
		  BondInd:word;
       end;

	   PSTATUSDATA=^STATUSDATA;

    const
	   SET_SESSION_UNCHANGED=0;
	   SET_SESSION_SELECTABLE=1;
	   SET_SESSION_NON_SELECTABLE=2;
	   SET_SESSION_BOND=1;
	   SET_SESSION_NO_BOND=2;
	   STOP_SESSION_SPECIFIED=0;
	   STOP_SESSION_ALL=1;
{****************************************************************************

						   Session specific routines.

****************************************************************************}

	type
	   STARTDATA=record
		  Length:word;
		  Related:word;
		  FgBg:word;
		  TraceOpt:word;
		  PgmTitle:PSZ;
		  PgmName:PSZ;
		  PgmInputs:PBYTE;
		  TermQ:PBYTE;
		  Environment:PBYTE;
		  InheritOpt:word;
		  SessionType:word;
		  IconFile:PSZ;
		  PgmHandle:longint;
		  PgmControl:word;
		  InitXPos:word;
		  InitYPos:word;
		  InitXSize:word;
		  InitYSize:word;
		  Reserved:word;
		  ObjectBuffer:PSZ;
		  ObjectBuffLen:longint;
	   end;

	   PSTARTDATA=^STARTDATA;

	function _DosStartSession(psd:PSTARTDATA;pidSession:PULONG;ppid:PPID):word;

	function _DosSetSession(idSession:longint;psd:PSTATUSDATA):word;

	function _DosSelectSession(idSession:longint):word;

	function _DosStopSession(scope:longint;idSession:longint):word;

	function _DosQueryAppType(pszName:PSZ;pFlags:PULONG):word;

	const
	   FAPPTYP_NOTSPEC=$0000;
	   FAPPTYP_NOTWINDOWCOMPAT=$0001;
	   FAPPTYP_WINDOWCOMPAT=$0002;
	   FAPPTYP_WINDOWAPI=$0003;
	   FAPPTYP_BOUND=$0008;
	   FAPPTYP_DLL=$0010;
	   FAPPTYP__Dos=$0020;
	   FAPPTYP_PHYSDRV=$0040;
	   FAPPTYP_VIRTDRV=$0080;
	   FAPPTYP_PROTDLL=$0100;
	   FAPPTYP_WINDOWSREAL=$0200;
	   FAPPTYP_WINDOWSPROT=$0400;
	   FAPPTYP_32BIT=$4000;
	   FAPPTYP_EXETYPE=FAPPTYP_WINDOWAPI;
	   FAPPTYP_RESERVED=not (FAPPTYP_WINDOWAPI or FAPPTYP_BOUND or FAPPTYP_DLL or FAPPTYP__Dos or FAPPTYP_PHYSDRV or FAPPTYP_VIRTDRV or FAPPTYP_PROTDLL or FAPPTYP_32BIT);
	   EAT_APPTYP_PMAPI=$00;
	   EAT_APPTYP__Dos=$01;
	   EAT_APPTYP_PMW=$02;
	   EAT_APPTYP_NOPMW=$03;
	   EAT_APPTYP_EXETYPE=$03;
	   EAT_APPTYP_RESERVED=not (EAT_APPTYP_EXETYPE);

	function _DosDevConfig(pdevinfo:pointer;item:longint):word;

	const
	   DEVINFO_PRINTER=0;
	   DEVINFO_RS232=1;
	   DEVINFO_FLOPPY=2;
	   DEVINFO_COPROCESSOR=3;
	   DEVINFO_SUBMODEL=4;
	   DEVINFO_MODEL=5;
	   DEVINFO_ADAPTER=6;

	function _DosDevIOCtl(hDevice:HFILE;category:longint;_function:longint;pParams:pointer;cbParmLenMax:longint;pcbParmLen:PULONG;pData:pointer;cbDataLenMax:longint;pcbDataLen:PULONG):word;

	function _DosPhysicalDisk(_function:longint;pBuf:pointer;cbBuf:longint;pParams:pointer;cbParams:longint):word;

	const
	   INFO_COUNT_PARTITIONABLE_DISKS=1;
	   INFO_GETIOCTLHANDLE=2;
	   INFO_FREEIOCTLHANDLE=3;

	type
	   HPIPE=LHANDLE;

	   PHPIPE=^HPIPE;

	   AVAILDATA=record
		  cbpipe:word;
		  cbmessage:word;
	   end;

	   PAVAILDATA=^AVAILDATA;

	   PIPEINFO=record
		  cbOut:word;
		  cbIn:word;
		  cbMaxInst:BYTE;
		  cbCurInst:BYTE;
		  cbName:BYTE;
		  szName:array[0..1-1] of CHAR;
       end;

	   PPIPEINFO=^PIPEINFO;

	   PIPESEMSTATE=record
		  fStatus:BYTE;
		  fFlag:BYTE;
		  usKey:word;
		  usAvail:word;
       end;

	   PPIPESEMSTATE=^PIPESEMSTATE;

    const
	   NP_INDEFINITE_WAIT=-1;
	   NP_DEFAULT_WAIT=0;
	   NP_STATE_DISCONNECTED=$0001;
	   NP_STATE_LISTENING=$0002;
	   NP_STATE_CONNECTED=$0003;
	   NP_STATE_CLOSING=$0004;
	   NP_ACCESS_INBOUND=$0000;
	   NP_ACCESS_OUTBOUND=$0001;
	   NP_ACCESS_DUPLEX=$0002;
	   NP_INHERIT=$0000;
	   NP_NOINHERIT=$0080;
	   NP_WRITEBEHIND=$0000;
	   NP_NOWRITEBEHIND=$4000;
	   NP_READMODE_BYTE=$0000;
	   NP_READMODE_MESSAGE=$0100;
	   NP_TYPE_BYTE=$0000;
	   NP_TYPE_MESSAGE=$0400;
	   NP_END_CLIENT=$0000;
	   NP_END_SERVER=$4000;
	   NP_WAIT=$0000;
	   NP_NOWAIT=$8000;
	   NP_UNLIMITED_INSTANCES=$00FF;

{****************************************************************************

						Named pipe specific routines.

****************************************************************************}

	function _DosCreatePipe(phfRead:PHFILE;phfWrite:PHFILE;cb:longint):word;

	function _DosCallNPipe(pszName:PSZ;pInbuf:pointer;cbIn:longint;pOutbuf:pointer;cbOut:longint;pcbActual:PULONG;msec:longint):word;

	function _DosConnectNPipe(hpipe:HPIPE):word;

	function _DosDisConnectNPipe(hpipe:HPIPE):word;

	function _DosCreateNPipe(pszName:PSZ;pHpipe:PHPIPE;openmode:longint;pipemode:longint;cbInbuf:longint;cbOutbuf:longint;msec:longint):word;

	function _DosPeekNPipe(hpipe:HPIPE;pBuf:pointer;cbBuf:longint;pcbActual:PULONG;pAvail:PAVAILDATA;pState:PULONG):word;

	function _DosQueryNPHState(hpipe:HPIPE;pState:PULONG):word;

	function _DosQueryNPipeInfo(hpipe:HPIPE;infolevel:longint;pBuf:pointer;cbBuf:longint):word;

	function _DosQueryNPipeSemState(hsem:HSEM;pnpss:PPIPESEMSTATE;cbBuf:longint):word;

	function _DosRawReadNPipe(pszName:PSZ;cb:longint;pLen:PULONG;pBuf:pointer):word;

	function _DosRawWriteNPipe(pszName:PSZ;cb:longint):word;

	function _DosSetNPHState(hpipe:HPIPE;state:longint):word;

	function _DosSetNPipeSem(hpipe:HPIPE;hsem:HSEM;key:longint):word;

	function _DosTransactNPipe(hpipe:HPIPE;pOutbuf:pointer;cbOut:longint;pInbuf:pointer;cbIn:longint;pcbRead:PULONG):word;

	function _DosWaitNPipe(pszName:PSZ;msec:longint):word;

	const
	   NPSS_EOI=0;
	   NPSS_RDATA=1;
	   NPSS_WSPACE=2;
	   NPSS_CLOSE=3;
	   NPSS_WAIT=$01;
	   NP_NBLK=$8000;
	   NP_SERVER=$4000;
	   NP_WMESG=$0400;
	   NP_RMESG=$0100;
	   NP_ICOUNT=$00FF;
	   NP_DISCONNECTED=1;
	   NP_LISTENING=2;
	   NP_CONNECTED=3;
	   NP_CLOSING=4;

	function _DosTmrQueryFreq(pulTmrFreq:PULONG):word;

	function _DosTmrQueryTime(pqwTmrTime:PQWORD):word;

	function _DosRegisterPerfCtrs(pbDataBlk:PBYTE;pbTextBlk:PBYTE;flFlags:longint):word;

	const
	   PROF_ORDINAL=133;
	   PROF_C=0;
	   PROF_USER=1;
	   PROF_USEDD=2;
	   PROF_KERNEL=4;
	   PROF_VERBOSE=8;
	   PROF_ENABLE=16;
	   PROF_ALLOC=0;
	   PROF_CLEAR=1;
	   PROF_ON=2;
	   PROF_OFF=3;
	   PROF_DUMP=4;
	   PROF_FREE=5;
	   PROF_SHIFT=2;
	   PROF_MOD_NAME_SIZE=10;
	   PROF_END_OF_DATA=13;

	type
	   HVDD=LHANDLE;

	   PHVDD=^HVDD;


	function _DosOpenVDD(pszVDD:PSZ;phvdd:PHVDD):word;

	function _DosRequestVDD(hvdd:HVDD;sgid:SGID;cmd:longint;cbInput:longint;pInput:pointer;cbOutput:longint;pOutput:pointer):word;

	function _DosCloseVDD(hvdd:HVDD):word;

	function _DosQuerydosProperty(sgid:SGID;pszName:PSZ;cb:longint;pch:PSZ):word;

	function _DosSetdosProperty(sgid:SGID;pszName:PSZ;cb:longint;pch:PSZ):word;

{***************************************************************************}
{***************************************************************************}

  implementation

	procedure _DosExit(action:longint;result:longint);[C];
	function _DosBeep(freq:longint;dur:longint):word;[C];
	function _DosCreateThread(ptid:PTID;pfn:PFNTHREAD;param:longint;flag:longint;cbStack:longint):word;[C];
	function _DosResumeThread(longint:longint):word;[C];
	function _DosSuspendThread(longint:longint):word;[C];
{!!!!!!!!    function _DosGetInfoBlocks(pptib:PPTIB;pppib:PPPIB):word;[C]; }
	function _DosKillThread(longint:longint):word;[C];
	function _DosWaitChild(action:longint;option:longint;pres:PRESULTCODES;ppid:PPID;pid:PID):word;[C];
	function _DosWaitThread(ptid:PTID;option:longint):word;[C];
	function _DosSleep(msec:longint):word;[C];
	function _DosDebug(pdbgbuf:pointer):word;[C];
{!!!!!!!    function _DosExitList(ordercode:longint;pfn:PFNEXITLIST):word;[C]; }
	function _DosExecPgm(pObjname:PCHAR;cbObjname:LONG;execFlag:longint;pArg:PSZ;pEnv:PSZ;pRes:PRESULTCODES;pName:PSZ):word;[C];
	function _DosSetPriority(scope:longint;Class:longint;delta:LONG;PorTid:longint):word;[C];
	function _DosEnterCritSec:word;[C];
	function _DosExitCritSec:word;[C];
	function _DosKillProcess(action:longint;pid:PID):word;[C];
	function _DosSetFileLocks(handle:HFILE;pflUnlock:PFILELOCK;pflLock:PFILELOCK;timeout:longint;flags:longint):word;[C];
	function _DosCancelLockRequest(handle:HFILE;pflLock:PFILELOCK):word;[C];
	function _DosOpen(pszFileName:PSZ;pHf:PHFILE;pulAction:PULONG;cbFile:longint;ulAttribute:longint;fsOpenFlags:longint;fsOpenMode:longint;peaop2:PEAOP2):word;[C];
	function _DosClose(handle:HFILE):word;[C];
	function _DosRead(handle:HFILE;pBuffer:pointer;cbRead:longint;pcbActual:PULONG):word;[C];
	function _DosWrite(handle:HFILE;pBuffer:pointer;cbWrite:longint;pcbActual:PULONG):word;[C];
	function _DosDelete(pszFile:PSZ):word;[C];
	function _DosForceDelete(pszFile:PSZ):word;[C];
	function _DosDupHandle(handle:HFILE;pHfile:PHFILE):word;[C];
	function _DosQueryFHState(handle:HFILE;pMode:PULONG):word;[C];
	function _DosSetFHState(handle:HFILE;mode:longint):word;[C];
	function _DosQueryHType(handle:HFILE;pType:PULONG;pAttr:PULONG):word;[C];
	function _DosFindFirst(pszFileSpec:PSZ;phdir:PHDIR;flAttribute:longint;pfindbuf:pointer;cbBuf:longint;pcFileNames:PULONG;ulInfoLevel:longint):word;[C];
	function _DosFindNext(hDir:HDIR;pfindbuf:pointer;cbfindbuf:longint;pcFilenames:PULONG):word;[C];
	function _DosFindClose(hDir:HDIR):word;[C];
	function _DosFSAttach(pszDevice:PSZ;pszFileC:PSZ;pData:pointer;cbData:longint;flag:longint):word;[C];
	function _DosQueryFSAttach(pszDeviceName:PSZ;ulOrdinal:longint;ulFSAInfoLevel:longint;pfsqb:PFSQBUFFER2;pcbBuffLength:PULONG):word;[C];
	function _DosFSCtl(pData:pointer;cbData:longint;pcbData:PULONG;pParms:pointer;cbParms:longint;pcbParms:PULONG;_function:longint;pszRoute:PSZ;handle:HFILE;method:longint):word;[C];
	function _DosSetFileSize(handle:HFILE;cbSize:longint):word;[C];
	function _DosResetBuffer(handle:HFILE):word;[C];
	function _DosSetFilePtr(handle:HFILE;ib:LONG;method:longint;ibActual:PULONG):word;[C];
	function _DosMove(pszOld:PSZ;pszNew:PSZ):word;[C];
	function _DosCopy(pszOld:PSZ;pszNew:PSZ;option:longint):word;[C];
	function _DosEditName(metalevel:longint;pszSource:PSZ;pszEdit:PSZ;pszTarget:PBYTE;cbTarget:longint):word;[C];
	function _DosCreateDir(pszDirName:PSZ;peaop2:PEAOP2):word;[C];
	function _DosDeleteDir(pszDir:PSZ):word;[C];
	function _DosSetDefaultDisk(disknum:longint):word;[C];
	function _DosQueryCurrentDisk(pdisknum:PULONG;plogical:PULONG):word;[C];
	function _DosSetCurrentDir(pszDir:PSZ):word;[C];
	function _DosQueryCurrentDir(disknum:longint;pBuf:PBYTE;pcbBuf:PULONG):word;[C];
	function _DosQueryFSInfo(disknum:longint;infolevel:longint;pBuf:pointer;cbBuf:longint):word;[C];
	function _DosSetFSInfo(disknum:longint;infolevel:longint;pBuf:pointer;cbBuf:longint):word;[C];
	function _DosQueryVerify(pBool:PBOOL32):word;[C];
	function _DosSetVerify(Bool:BOOL32):word;[C];
	function _DosSetMaxFH(cFH:longint):word;[C];
	function _DosSetRelMaxFH(pcbReqCount:PLONG;pcbCurMaxFH:PULONG):word;[C];
	function _DosQueryFileInfo(hf:HFILE;ulInfoLevel:longint;pInfo:pointer;cbInfoBuf:longint):word;[C];
	function _DosSetFileInfo(hf:HFILE;ulInfoLevel:longint;pInfoBuf:pointer;cbInfoBuf:longint):word;[C];
	function _DosQueryPathInfo(pszPathName:PSZ;ulInfoLevel:longint;pInfoBuf:pointer;cbInfoBuf:longint):word;[C];
	function _DosSetPathInfo(pszPathName:PSZ;ulInfoLevel:longint;pInfoBuf:pointer;cbInfoBuf:longint;flOptions:longint):word;[C];
	function _DosShutdown(ulReserved:longint):word;[C];
	function _DosEnumAttribute(ulRefType:longint;pvFile:pointer;ulEntry:longint;pvBuf:pointer;cbBuf:longint;pulCount:PULONG;ulInfoLevel:longint):word;[C];
	function _DosAllocMem(ppb:PPVOID;cb:longint;flag:longint):word;[C];
	function _DosFreeMem(pb:pointer):word;[C];
	function _DosSetMem(pb:pointer;cb:longint;flag:longint):word;[C];
	function _DosGiveSharedMem(pb:pointer;pid:PID;flag:longint):word;[C];
	function _DosGetSharedMem(pb:pointer;flag:longint):word;[C];
	function _DosGetNamedSharedMem(ppb:PPVOID;pszName:PSZ;flag:longint):word;[C];
	function _DosAllocSharedMem(ppb:PPVOID;pszName:PSZ;cb:longint;flag:longint):word;[C];
	function _DosQueryMem(pb:pointer;pcb:PULONG;pFlag:PULONG):word;[C];
	function _DosSubAllocMem(pbBase:pointer;ppb:PPVOID;cb:longint):word;[C];
	function _DosSubFreeMem(pbBase:pointer;pb:pointer;cb:longint):word;[C];
	function _DosSubSetMem(pbBase:pointer;flag:longint;cb:longint):word;[C];
	function _DosSubUnsetMem(pbBase:pointer):word;[C];
	function _DosCreateEventSem(pszName:PSZ;phev:PHEV;flAttr:longint;fState:BOOL32):word;[C];
	function _DosOpenEventSem(pszName:PSZ;phev:PHEV):word;[C];
	function _DosCloseEventSem(hev:HEV):word;[C];
	function _DosResetEventSem(hev:HEV;pulPostCt:PULONG):word;[C];
	function _DosPostEventSem(hev:HEV):word;[C];
	function _DosWaitEventSem(hev:HEV;ulTimeout:longint):word;[C];
	function _DosQueryEventSem(hev:HEV;pulPostCt:PULONG):word;[C];
	function _DosCreateMutexSem(pszName:PSZ;phmtx:PHMTX;flAttr:longint;fState:BOOL32):word;[C];
	function _DosOpenMutexSem(pszName:PSZ;phmtx:PHMTX):word;[C];
	function _DosCloseMutexSem(hmtx:HMTX):word;[C];
	function _DosRequestMutexSem(hmtx:HMTX;ulTimeout:longint):word;[C];
	function _DosReleaseMutexSem(hmtx:HMTX):word;[C];
	function _DosQueryMutexSem(hmtx:HMTX;ppid:PPID;ptid:PTID;pulCount:PULONG):word;[C];
	function _DosCreateMuxWaitSem(pszName:PSZ;phmux:PHMUX;cSemRec:longint;pSemRec:PSEMRECORD;flAttr:longint):word;[C];
	function _DosOpenMuxWaitSem(pszName:PSZ;phmux:PHMUX):word;[C];
	function _DosCloseMuxWaitSem(hmux:HMUX):word;[C];
	function _DosWaitMuxWaitSem(hmux:HMUX;ulTimeout:longint;pulUser:PULONG):word;[C];
	function _DosAddMuxWaitSem(hmux:HMUX;pSemRec:PSEMRECORD):word;[C];
	function _DosDeleteMuxWaitSem(hmux:HMUX;hSem:HSEM):word;[C];
	function _DosQueryMuxWaitSem(hmux:HMUX;pcSemRec:PULONG;pSemRec:PSEMRECORD;pflAttr:PULONG):word;[C];
	function _DosGetDateTime(pdt:PDATETIME):word;[C];
	function _DosSetDateTime(pdt:PDATETIME):word;[C];
	function _DosAsyncTimer(msec:longint;hsem:HSEM;phtimer:PHTIMER):word;[C];
	function _DosStartTimer(msec:longint;hsem:HSEM;phtimer:PHTIMER):word;[C];
	function _DosStopTimer(htimer:HTIMER):word;[C];
	function _DosLoadModule(pszName:PSZ;cbName:longint;pszModname:PSZ;phmod:PHMODULE):word;[C];
	function _DosFreeModule(hmod:HMODULE):word;[C];
	function _DosQueryProcAddr(hmod:HMODULE;ordinal:longint;pszName:PSZ;ppfn:PPFN):word;[C];
	function _DosQueryModuleHandle(pszModname:PSZ;phmod:PHMODULE):word;[C];
	function _DosQueryModuleName(hmod:HMODULE;cbName:longint;pch:PCHAR):word;[C];
	function _DosQueryProcType(hmod:HMODULE;ordinal:longint;pszName:PSZ;pulproctype:PULONG):word;[C];
	function _DosGetResource(hmod:HMODULE;idType:longint;idName:longint;ppb:PPVOID):word;[C];
	function _DosFreeResource(pb:pointer):word;[C];
	function _DosQueryResourceSize(hmod:HMODULE;idt:longint;idn:longint;pulsize:PULONG):word;[C];
	function _DosQueryCtryInfo(cb:longint;pcc:PCOUNTRYCODE;pci:PCOUNTRYINFO;pcbActual:PULONG):word;[C];
	function _DosQueryDBCSEnv(cb:longint;pcc:PCOUNTRYCODE;pBuf:PCHAR):word;[C];
	function _DosMapCase(cb:longint;pcc:PCOUNTRYCODE;pch:PCHAR):word;[C];
	function _DosQueryCollate(cb:longint;pcc:PCOUNTRYCODE;pch:PCHAR;pcch:PULONG):word;[C];
	function _DosQueryCp(cb:longint;arCP:PULONG;pcCP:PULONG):word;[C];
	function _DosSetProcessCp(cp:longint):word;[C];
{!!!!!!!!    function _DosSetExceptionHandler(pERegRec:PEXCEPTIONREGISTRATIONRECORD):word;[C];
	function _DosUnsetExceptionHandler(pERegRec:PEXCEPTIONREGISTRATIONRECORD):word;[C];
	function _DosRaiseException(pexcept:PEXCEPTIONREPORTRECORD):word;[C];
	function _DosSendSignalException(pid:PID;exception:longint):word;[C];
	function _DosUnwindException(phandler:PEXCEPTIONREGISTRATIONRECORD;pTargetIP:pointer;pERepRec:PEXCEPTIONREPORTRECORD):word;[C];
}
	function _DosSetSignalExceptionFocus(flag:BOOL32;pulTimes:PULONG):word;[C];
	function _DosEnterMustComplete(pulNesting:PULONG):word;[C];
	function _DosExitMustComplete(pulNesting:PULONG):word;[C];
	function _DosAcknowledgeSignalException(ulSignalNum:longint):word;[C];
	function _DosCreatePipe(phfRead:PHFILE;phfWrite:PHFILE;cb:longint):word;[C];
	function _DosCloseQueue(hq:HQUEUE):word;[C];
	function _DosCreateQueue(phq:PHQUEUE;priority:longint;pszName:PSZ):word;[C];
	function _DosOpenQueue(ppid:PPID;phq:PHQUEUE;pszName:PSZ):word;[C];
	function _DosPeekQueue(hq:HQUEUE;pRequest:PREQUESTDATA;pcbData:PULONG;ppbuf:PPVOID;element:PULONG;nowait:BOOL32;ppriority:PBYTE;hsem:HEV):word;[C];
	function _DosPurgeQueue(hq:HQUEUE):word;[C];
	function _DosQueryQueue(hq:HQUEUE;pcbEntries:PULONG):word;[C];
	function _DosReadQueue(hq:HQUEUE;pRequest:PREQUESTDATA;pcbData:PULONG;ppbuf:PPVOID;element:longint;wait:BOOL32;ppriority:PBYTE;hsem:HEV):word;[C];
	function _DosWriteQueue(hq:HQUEUE;request:longint;cbData:longint;pbData:pointer;priority:longint):word;[C];
	function _DosError(error:longint):word;[C];
	function _DosGetMessage(pTable:PPCHAR;cTable:longint;pBuf:PCHAR;cbBuf:longint;msgnumber:longint;pszFile:PSZ;pcbMsg:PULONG):word;[C];
	function _DosErrClass(code:longint;pClass:PULONG;pAction:PULONG;pLocus:PULONG):word;[C];
	function _DosInsertMessage(pTable:PPCHAR;cTable:longint;pszMsg:PSZ;cbMsg:longint;pBuf:PCHAR;cbBuf:longint;pcbMsg:PULONG):word;[C];
	function _DosPutMessage(hfile:HFILE;cbMsg:longint;pBuf:PCHAR):word;[C];
	function _DosQuerySysInfo(iStart:longint;iLast:longint;pBuf:pointer;cbBuf:longint):word;[C];
	function _DosScanEnv(pszName:PSZ;ppszValue:PPSZ):word;[C];
	function _DosSearchPath(flag:longint;pszPathOrName:PSZ;pszFilename:PSZ;pBuf:PBYTE;cbBuf:longint):word;[C];
	function _DosQueryMessageCP(pb:PCHAR;cb:longint;pszFilename:PSZ;cbBuf:PULONG):word;[C];
	function _DosQueryRASInfo(Index:longint;Addr:PPVOID):word;[C];
	function _DosStartSession(psd:PSTARTDATA;pidSession:PULONG;ppid:PPID):word;[C];
	function _DosSetSession(idSession:longint;psd:PSTATUSDATA):word;[C];
	function _DosSelectSession(idSession:longint):word;[C];
	function _DosStopSession(scope:longint;idSession:longint):word;[C];
	function _DosQueryAppType(pszName:PSZ;pFlags:PULONG):word;[C];
	function _DosDevConfig(pdevinfo:pointer;item:longint):word;[C];
	function _DosDevIOCtl(hDevice:HFILE;category:longint;_function:longint;pParams:pointer;cbParmLenMax:longint;pcbParmLen:PULONG;pData:pointer;cbDataLenMax:longint;pcbDataLen:PULONG):word;[C];
	function _DosPhysicalDisk(_function:longint;pBuf:pointer;cbBuf:longint;pParams:pointer;cbParams:longint):word;[C];
	function _DosCallNPipe(pszName:PSZ;pInbuf:pointer;cbIn:longint;pOutbuf:pointer;cbOut:longint;pcbActual:PULONG;msec:longint):word;[C];
	function _DosConnectNPipe(hpipe:HPIPE):word;[C];
	function _DosDisConnectNPipe(hpipe:HPIPE):word;[C];
	function _DosCreateNPipe(pszName:PSZ;pHpipe:PHPIPE;openmode:longint;pipemode:longint;cbInbuf:longint;cbOutbuf:longint;msec:longint):word;[C];
	function _DosPeekNPipe(hpipe:HPIPE;pBuf:pointer;cbBuf:longint;pcbActual:PULONG;pAvail:PAVAILDATA;pState:PULONG):word;[C];
	function _DosQueryNPHState(hpipe:HPIPE;pState:PULONG):word;[C];
	function _DosQueryNPipeInfo(hpipe:HPIPE;infolevel:longint;pBuf:pointer;cbBuf:longint):word;[C];
	function _DosQueryNPipeSemState(hsem:HSEM;pnpss:PPIPESEMSTATE;cbBuf:longint):word;[C];
	function _DosRawReadNPipe(pszName:PSZ;cb:longint;pLen:PULONG;pBuf:pointer):word;[C];
	function _DosRawWriteNPipe(pszName:PSZ;cb:longint):word;[C];
	function _DosSetNPHState(hpipe:HPIPE;state:longint):word;[C];
	function _DosSetNPipeSem(hpipe:HPIPE;hsem:HSEM;key:longint):word;[C];
	function _DosTransactNPipe(hpipe:HPIPE;pOutbuf:pointer;cbOut:longint;pInbuf:pointer;cbIn:longint;pcbRead:PULONG):word;[C];
	function _DosWaitNPipe(pszName:PSZ;msec:longint):word;[C];
	function _DosTmrQueryFreq(pulTmrFreq:PULONG):word;[C];
	function _DosTmrQueryTime(pqwTmrTime:PQWORD):word;[C];
	function _DosRegisterPerfCtrs(pbDataBlk:PBYTE;pbTextBlk:PBYTE;flFlags:longint):word;[C];
	function _DosOpenVDD(pszVDD:PSZ;phvdd:PHVDD):word;[C];
	function _DosRequestVDD(hvdd:HVDD;sgid:SGID;cmd:longint;cbInput:longint;pInput:pointer;cbOutput:longint;pOutput:pointer):word;[C];
	function _DosCloseVDD(hvdd:HVDD):word;[C];
	function _DosQuerydosProperty(sgid:SGID;pszName:PSZ;cb:longint;pch:PSZ):word;[C];
	function _DosSetdosProperty(sgid:SGID;pszName:PSZ;cb:longint;pch:PSZ):word;[C];

end.
