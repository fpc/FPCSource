{****************************************************************************

					 FPK-Pascal -- OS/2 runtime library

				  Copyright (c) 1993,95 by Florian Kl„mpfl
				   Copyright (c) 1997 by Dani‰l Mantione

 FPK-Pascal is distributed under the GNU Public License v2. So is this unit.
 The GNU Public License requires you to distribute the source code of this
 unit with any product that uses it. We grant you an exception to this, and
 that is, when you compile a program with the FPK Pascal compiler, you do not
 need to ship source code with that program, AS LONG AS YOU ARE USING
 UNMODIFIED CODE! If you modify this code, you MUST change the next line:

 <This an official, unmodified FPK Pascal source code file.>

 Send us your modified files, we can work together if you want!

****************************************************************************}

unit sysos2;

{$I os.inc}

interface

{ die betriebssystemunabhangigen Deklarationen einfuegen: }

{$I	SYSTEMH.INC}
{$I	heaph.inc}

type	Tos=(osDOS,osOS2,osDPMI);

var		os_mode:Tos;
		first_meg:pointer;

type	Psysthreadib=^Tsysthreadib;
		Pthreadinfoblock=^Tthreadinfoblock;
		Pprocessinfoblock=^Tprocessinfoblock;

		Tbytearray=array[0..$ffff] of byte;
		Pbytearray=^Tbytearray;

		Tsysthreadib=record
			tid,
			priority,
			version:longint;
			MCcount,
			MCforceflag:word;
		end;

		Tthreadinfoblock=record
			pexchain,
			stack,
			stacklimit:pointer;
			tib2:Psysthreadib;
			version,
			ordinal:longint;
		end;

		Tprocessinfoblock=record
			pid,
			parentpid,
			hmte:longint;
			cmd,
			env:Pbytearray;
			flstatus,
			ttype:longint;
		end;

procedure _DosGetInfoBlocks(var Atib:Pthreadinfoblock;
							var Apib:Pprocessinfoblock);

implementation

{ die betriebssystemunabhangigen Implementationen einfuegen: }

{$I	SYSTEM.INC}

procedure _DosGetInfoBlocks(var Atib:Pthreadinfoblock;
							var Apib:Pprocessinfoblock);[C];

{****************************************************************************

					Miscelleanious related routines.

****************************************************************************}

procedure halt;

begin
	asm
		movw $0x4c00,%ax
		call ___syscall
	end;
end;

procedure halt(errnum :	byte);

begin
	asm
		movb $0x4c,%ah
		movb errnum,%al
		call ___syscall
	end;
end;

function paramcount	: longint;

begin
	 asm
		movl _argc,%eax
		decl %eax
		leave
		ret
	 end ['EAX'];
end;

function paramstr(l	: longint):string;

	function args	: pointer;

	begin
		asm
			movl _argv,%eax
			leave
			ret
		end ['EAX'];
	end;

var	p:^Pchar;

begin
	 if	(l>=0) and (l<=paramcount) then
		begin
			p:=args;
			paramstr:=strpas(p[l]);
		end
	 else paramstr:='';
end;

procedure randomize;

var	hl:longint;

begin
	asm
		movb $0x2c,%ah
		call ___syscall
		movw %cx,-4(%ebp)
		movw %dx,-2(%ebp)
	end;
	randseed:=hl;
end;

{****************************************************************************

						Text-file I/O related routines.

****************************************************************************}


function open(f:Pchar;flags:longint):longint;

begin
	asm
		movb $0x3d,%ah
		movl 8(%ebp),%edx
		movb 12(%ebp),%al
		call ___syscall
		jnc	LOPEN1
		movw %ax,U_SYSOS2_INOUTRES;
		xorl %eax,%eax
	LOPEN1:
		; Returnwert ist in	EAX
		leave
		ret	$8
	 end;
end;

function create(f :	pchar):longint;

begin
	 asm
		movb $0x3c,%ah
		movl 8(%ebp),%edx
		xor	%ecx,%ecx
		call ___syscall
		jnc	Lcreate1
		movw %ax,U_SYSOS2_INOUTRES;
		xorl %eax,%eax
	 Lcreate1:
		; Returnwert ist in	EAX
		leave
		ret	$8
	 end;
end;

procedure do_close(h:longint);

begin
	 asm
		movb $0x3e,%ah
		mov	h,%ebx
		call ___syscall
	 end;
end;

function dosfilepos(handle:longint) :	longint;

begin
	asm
		movb $0x42,%ah
		movb $0x1,%al
		movl 8(%ebp),%ebx
		xorl %edx,%edx
		call ___syscall
		jnc	LDOSFILEPOS
		movw %ax,U_SYSOS2_INOUTRES;
		xorl %eax,%eax
	LDOSFILEPOS:
		leave
		ret	$4
	 end;
end;

procedure dosseek(handle:longint;pos:longint);

begin
	asm
		movb $0x42,%ah
		xorb %al,%al
		movl 8(%ebp),%ebx
		movl 12(%ebp),%edx
		call ___syscall
		jnc	LDOSSEEK1
		movw %ax,U_SYSOS2_INOUTRES;
	LDOSSEEK1:
	end;
end;

function dosfilesize(handle	: longint):longint;

	function set_at_end(handle:longint)	: longint;

	begin
		asm
			movb $0x42,%ah
			movb $0x2,%al
			;	Vorsicht Stack:	0 %ebp;	4 retaddr;
			;	8 nextstackframe; 12 handle
			movl 12(%ebp),%ebx
			xorl %edx,%edx
			call ___syscall
			jnc Lset_at_end
			movw %ax,U_SYSOS2_INOUTRES;
			xorl %eax,%eax
		Lset_at_end:
			leave
			ret $8
		end;
	end;

var	tempfilesize,aktfilepos:longint;

begin
	aktfilepos:=dosfilepos(handle);
	tempfilesize:=set_at_end(handle);
	dosseek(handle,aktfilepos);
	dosfilesize:=tempfilesize;
end;

procedure fileclosefunc(var	t :	textrec);

begin
	do_close(t.handle);
	t.mode:=fmclosed;
end;

procedure fileopenfunc(var f:textrec);

var	b:array[0..255] of char;
	size:longint;

begin
	move(f.name[1],b,length(f.name));
	b[length(f.name)]:=#0;
	f.inoutfunc:=@fileinoutfunc;
	f.flushfunc:=@fileinoutfunc;
	f.closefunc:=@fileclosefunc;
	case f.mode of
		fminput:
			f.handle:=open(b,0);
		fmoutput:
			f.handle:=create(b);
		fmappend:
			begin
				f.handle:=open(b,1);
				f.mode:=fmoutput;
				size:=dosfilesize(f.handle);
				if size>0 then
					begin
						{Set filepointer to eof character if present,
						 or to end of file if not. Any change to the
						 file causes the eof character to be overwritten,
						 so we get a correct text file.}
						dosseek(f.handle,size-1);
						dosread(f.handle,longint(@b),1);
						if b[0]<>#26 then
							dosseek(f.handle,size);
					end;
			end;
	end;
end;


function eof(var t:text):boolean;[iocheck];

var	zoekpos:byte;

begin
	{ maybe we	need new data }
	if	textrec(t).bufpos+3>=textrec(t).bufend then
		dateifunc(textrec(t).inoutfunc)(textrec(t));
	eof:=dosfilesize(textrec(t).handle)<=dosfilepos(textrec(t).handle);
	if	eof	then
		eof:=textrec(t).bufend<=textrec(t).bufpos;
		if	not	eof	then
			begin
				{If	the	next character is an end-of-file character,
				 or	if we are at eoln and first	character on next line
				 is	eof	then we	should also	return true.}
				zoekpos:=textrec(t).bufpos;
				while textrec(t).buffer[zoekpos] in	[#13,#10] do
					inc(zoekpos);
				if zoekpos>textrec(t).bufpos+2 then
					eof:=false
				else
					eof:=textrec(t).buffer[zoekpos]=#26;
			end;
end;

{****************************************************************************

						File I/O related routines.

****************************************************************************}


procedure doserase(p:Pchar);

begin
	asm
		movl 8(%ebp),%edx
		movb $0x41,%ah
		call ___syscall
		jnc	LERASE1
		movw %ax,U_SYSOS2_INOUTRES;
	LERASE1:
	end;
end;

procedure dosrename(p1,p2:Pchar);

begin
	asm
		movl 8(%ebp),%edx
		movl 12(%ebp),%edi
		movb $0x56,%ah
		call ___syscall
		jnc	LRENAME1
		movw %ax,U_SYSOS2_INOUTRES;
	LRENAME1:
	end;
end;

function dosread(h,addr,len:longint):longint;

begin
	asm
		movl 16(%ebp),%ecx
		movl 12(%ebp),%edx
		movl 8(%ebp),%ebx
		movb $0x3f,%ah
		call ___syscall
		jnc	LDOSREAD1
		movw %ax,U_SYSOS2_INOUTRES;
		xorl %eax,%eax
	LDOSREAD1:
		leave
		ret	$12
	end;
end;

function doswrite(h,addr,len:longint) : longint;

begin
	asm
		movl 16(%ebp),%ecx
		movl 12(%ebp),%edx
		movl 8(%ebp),%ebx
		movb $0x40,%ah
		call ___syscall
		jnc	LDOSWRITE1
		movw %ax,U_SYSOS2_INOUTRES;
	LDOSWRITE1:
       movl %eax,-4(%ebp)
	end;
end;

procedure rewrite(var f:file;l:word);

var	b:array[0..255] of char;

begin
	{According to Turbo Pascal helpfile, a file is automatically closed
	 if it's open.}
	if	filerec(f).mode<>fmclosed then
	close(f);
	filerec(f).mode:=fmoutput;
	move(filerec(f).name[1],b,length(filerec(f).name));
	b[length(filerec(f).name)]:=#0;
	filerec(f).handle:=create(b);
	filerec(f).recsize:=l;
end;

procedure rewrite(var f:file);

begin
	rewrite(f,128);
end;

procedure reset(var	f:file;l:word);

var	b:array[0..255] of char;

begin
	{According to Turbo Pascal helpfile, a file is automatically closed
				  if it's open.}
	if filerec(f).mode<>fmclosed then
	close(f);
	move(filerec(f).name[1],b,length(filerec(f).name));
	b[length(filerec(f).name)]:=#0;
	case filemode of
		0:
			begin
				filerec(f).mode:=fminput;
				filerec(f).handle:=open(b,0);
			end;
		1:
			begin
				filerec(f).mode:=fmoutput;
				filerec(f).handle:=open(b,1);
			end;
		2:
			begin
				filerec(f).mode:=fminout;
				filerec(f).handle:=open(b,2);
			end;
	end;
	filerec(f).recsize:=l;
end;

procedure reset(var	f:file);

begin
	reset(f,128);
end;

procedure blockwrite(var f:file;var buf;count:longint);

var p:pointer;
	size:longint;

begin
	p:=@buf;
	doswrite(filerec(f).handle,longint(p),count*filerec(f).recsize);
end;

procedure blockread(var	f:file;var buf;count:longint;var result:longint);

begin
	result:=dosread(filerec(f).handle,longint(@buf),
	 count*filerec(f).recsize) div filerec(f).recsize;
end;

procedure blockread(var	f:file;var buf;count:longint);

var	result:longint;

begin
	blockread(f,buf,count,result);
end;

procedure truncate (var f : file);[iocheck];

var p : pointer;

begin
   doswrite(filerec(f).handle,longint(p),0);
end;

procedure close(var	f:file);

begin
	if (filerec(f).mode<>fmclosed) then
		begin
			filerec(f).mode:=fmclosed;
			do_close(filerec(f).handle);
		end;
end;

function filepos(var f:file):longint;

var	l:longint;

begin
	filepos:=dosfilepos(filerec(f).handle) div filerec(f).recsize;
end;

function filesize(var f:file)	: longint;

begin
	filesize:=dosfilesize(filerec(f).handle) div filerec(f).recsize;
end;

function eof(var f:file):boolean;[iocheck];

begin
	eof:=filesize(f)<=filepos(f);
end;

procedure seek(var f:file;pos	: longint);

begin
	dosseek(filerec(f).handle,pos*filerec(f).recsize);
end;

{****************************************************************************

						  Directory related routines.

****************************************************************************}

procedure dos_dirs(func:byte;name:Pchar);

begin
	asm
		movl 10(%ebp),%edx
		movb 8(%ebp),%ah
		call ___syscall
		jnc	LDOS_DIRS1
		movw %ax,U_SYSOS2_INOUTRES;
	LDOS_DIRS1:
		leave
		ret	$6
	end;
end;

procedure _dir(func:byte;const s:string);

var	buffer:array[0..255] of char;

begin
	move(s[1],buffer,length(s));
	buffer[length(s)]:=#0;
	dos_dirs(func,buffer);
end;

procedure mkdir(const s:string);

begin
	_dir($39,s);
end;

procedure rmdir(const s:string);

begin
	_dir($3a,s);
end;

procedure chdir(const s:string);

begin
	_dir($3b,s);
end;

{ thanks to	Michael	Van	Canneyt	<michael@tfdec1.fys.kuleuven.ac.be>, }
{ who wrote this code												 }
procedure getdir(drivenr:byte;var dir:string);

var	temp:string;
	sof:pointer;
	i:byte;

begin
	sof:=@dir[4];

	{ dir[1..3] will contain '[drivenr]:\', but is	not	}
	{ supplied by DOS, so we let dos string start at	}
	{ dir[4]											}
	asm
		{ Get dir from drivenr:0=default,	1=A	etc... }
		movb drivenr,%dl

		{ put (previously saved) offset	in si }
		movl sof,%esi

		{ call msdos function 47H :	Get	dir	}
		mov	$0x47,%ah

		{ make the call	}
		call ___syscall

		{ Rem: if call unsuccesfull, carry is set, and AX has }
		{ error	code										  }

	end;
	{ Now Dir should be filled	with directory in ASCIIZ, }
	{ starting	from dir[4]								  }
	dir[0]:=#3;
	dir[2]:=':';
	dir[3]:='\';

	i:=4;
	{ conversation	to Pascal string }
	while (dir[i]<>#0)	do
		begin
			{ convert path name to DOS }
			if dir[i]='/'	then
				dir[i]:='\';
			dir[0]:=chr(i);
			inc(i);
		end;

	{ upcase the string (FPKPascal	function) }
	dir:=upcase(dir);
	if drivenr<>0 then	  {	Drive was supplied.	We know	it }
		dir[1]:=chr(65+drivenr-1)
	else
		begin
			{ We need to get the current drive from DOS function 19H }
			{ because the drive was the default, which can be unknown	}
			asm
				movb $0x19,%ah
				call ___syscall
				addb $65,%al
				movb %al,i
			end;
			dir[1]:=chr(i)
		end;
end;

{****************************************************************************

					Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):longint;

begin
	asm
		movl size,%edx
		movl $0x7f00,%ax
		int  $0x21
		movl %eax,__RESULT
	end;
end;

function getheapstart:pointer;

begin
	asm
		movl __heap_base,%eax
		leave
		ret
	end ['EAX'];
end;

{$i	heap.inc}

{***************************************************************************

				Runtime error checking related routines.

***************************************************************************}

{$S-}
procedure st1(stack_size:longint);[public,alias: 'STACKCHECK'];

begin
	{ called when trying to get local stack }
	{ if the compiler directive $S is set   }
	asm
		movl stack_size,%ebx
		movl %esp,%eax
		subl %ebx,%eax
{$ifdef SYSTEMDEBUG}
		movl U_SYSOS2_LOWESTSTACK,%ebx
		cmpl %eax,%ebx
		jb   _is_not_lowest
		movl %eax,U_SYSOS2_LOWESTSTACK
	_is_not_lowest:
{$endif SYSTEMDEBUG}
		cmpb $2,U_SYSOS2_OS_MODE
		jne _running_in_dos
		movl U_SYSOS2_STACKBOTTOM,%ebx
		jmp _running_in_os2
	_running_in_dos:
		movl __heap_brk,%ebx
	_running_in_os2:
		cmpl %eax,%ebx
		jae  __short_on_stack
		leave
		ret  $4
	__short_on_stack:
	end ['EAX','EBX'];
	{ this needs a local variable }
	{ so the function called itself !! }
	{ Writeln('low in stack ');}
	RunError(202);
end;
{no stack check in system }

{****************************************************************************

						System unit initialization.

****************************************************************************}


var
	pib:Pprocessinfoblock;
	tib:Pthreadinfoblock;

begin
	{Determine the operating system	we are running on.}
	asm
		movw $0x7f0a,%ax
		call ___syscall
		test $512,%bx		   ; Bit 9 is OS/2 flag.
		setnzb U_SYSOS2_OS_MODE
		test $4096,%bx
		jz _noRSX
		movb $2,U_SYSOS2_OS_MODE
	_noRSX:
	end;
	{Now request, if we	are	running	under DOS,
	 read-access to	the	first meg. of memory.}
	if os_mode in [osDOS,osDPMI] then
		asm
			mov	$0x7f13,%ax
			xor	%ebx,%ebx
			mov	$0xfff,%ecx
			xor	%edx,%edx
			call ___syscall
			mov	%eax,U_SYSOS2_FIRST_MEG
		end
	else
		first_meg:=nil;
	{At 0.9.2, case for enumeration does not work.}
	case os_mode of
		osDOS:
			stackbottom:=0;
		osOS2:
			begin
				_DosGetInfoBlocks(tib,pib);
				stackbottom:=longint(tib^.stack);
			end;
		osDPMI:
			stackbottom:=0;		{Not sure how to get it, but seems to be
								 always zero.}
	end;
	exitproc:=nil;

	{Initialize the heap.}
	InitHeap;
	
   { to test stack depth }
   loweststack:=maxlongint;

	{Enable the brk area by initializing it with the initial heap size.}
	asm
		mov $0x7f01,%ax
		movl HEAPSIZE,%edx
		call ___syscall
	end;

	{ Ein- und Ausgabe initialisieren }
	assign(input,'');
	textrec(input).handle:=0;
	textrec(input).mode:=fminput;
	textrec(input).inoutfunc:=@fileinoutfunc;
	textrec(input).flushfunc:=@fileinoutfunc;
	assign(output,'');
	textrec(output).handle:=1;
	textrec(output).mode:=fmoutput;
	textrec(output).inoutfunc:=@fileinoutfunc;
	textrec(output).flushfunc:=@fileinoutfunc;
	textrec(input).mode:=fminput;

	{ kein Ein-	Ausgabefehler }
	inoutres:=0;
end.
