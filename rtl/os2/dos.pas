{****************************************************************************

						  FPKPascal Runtime-Library
						  Copyright (c) 1994,97 by
					Florian Klaempfl and Michael Spiegel
						 OS/2 port by Daniâl Mantione

 ****************************************************************************}

{
  History:
  2.7.1994: Version 0.2
			Datenstrukturen sind deklariert sowie
			50 % der Unterprogramme sind implementiert
  12.8.1994: exec implemented
  14.8.1994: findfirst and findnext implemented
  24.8.1994: Version 0.3
  28.2.1995: Version 0.31
			 some parameter lists with const optimized
   3.7.1996: bug in fsplit removed (dir and ext were not intializised)
   7.7.1996: packtime and unpacktime implemented
  20.9.1996: Version 0.5
			 setftime and getftime implemented
			 some optimizations done (integer -> longint)
			 procedure fsearch from the LINUX version ported
			 msdos call implemented
  26th november 1996:
			 better fexpand
  29th january 1997:
			 bug in getftime and setftime removed
			 setfattr and getfattr added
   2th february 1997: Version 0.9
			 bug of searchrec corrected
   2 june 1997:
			 OS/2 support added.
   12 june 1997:
			 OS/2 port done.
   12 November 1997:
			 Adapted to new DLL stuff.
}

unit dos;

{$I os.inc}

{$I386_DIRECT}

  interface

	uses
	   strings;

	const
	   { bit masks for CPU flags}
	   fcarry = $0001;
	   fparity = $0004;
	   fauxiliary = $0010;
	   fzero = $0040;
       fsign = $0080;
       foverflow  = $0800;

       { Bitmasken fuer Dateiattribute }
       readonly = $01;
       hidden = $02;
       sysfile = $04;
       volumeid = $08;
       directory = $10;
       archive = $20;
       anyfile = $3F;
       fmclosed = $D7B0;
       fminput = $D7B1;
       fmoutput = $D7B2;
       fminout = $D7B3;

    type
	   { some string types }
	   {$IFDEF OS2}
	   comstr=string; 				{Filenames can be long in OS/2.}
	   pathstr=string;
	   {$ELSE}
	   comstr = string[127];        { Kommandozeilenstring }
	   pathstr = string[79];        { String fuer einen Pfadnamen }
	   {$ENDIF}
	   dirstr = string[67];         { String fuer kompletten Pfad }
	   namestr = string[8];         { Dateinamenstring }
	   extstr = string[4];          { String fuer Dateinamensuffix }

	   { search record which is used by findfirst and findnext }
{$PACKRECORDS 1}
	   searchrec = record
		  fill : array[1..21] of byte;
		  attr : byte;
		  time : longint;
		  {$IFNDEF OS2}			{ A DJGPP strange thing.}
		  reserved : word; 		{ requires the DOS extender (DJ GNU-C) }
		  {$ENDIF}
		  size : longint;
		  {$IFNDEF OS2}
		  name : string[15]; 	{ the same size as declared by (DJ GNU C) }
		  {$ELSE}
		  name:string;			{Filenames can be very long in OS/2!}
		  {$ENDIF}
	   end;
{$PACKRECORDS 2}

       { file record for untyped files }
       filerec = record
          handle : word;
          mode : word;
		  recsize : word;
          _private : array[1..26] of byte;
          userdata: array[1..16] of byte;
          name: array[0..79] of char;
	   end;

       { file record for text files }
	   textbuf = array[0..127] of char;

       textrec = record
          handle : word;
          mode : word;
          bufSize : word;
          _private : word;
          bufpos : word;
          bufend : word;
		  bufptr : ^textbuf;
          openfunc : pointer;
		  inoutfunc : pointer;
          flushfunc : pointer;
          closefunc : pointer;
		  userdata : array[1..16] of byte;
		  name : array[0..79] of char;
		  buffer : textbuf;
       end;

       { data structure for the registers needed by msdos and intr }	
       registers = record
		 case i : integer of
			0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
			1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
			2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
	   end;

	   { record for date and time }
	   datetime = record
		  year,month,day,hour,min,sec : word;
	   end;

	{Flags for the exec procedure:

	 Starting the program:
	 efwait:		Wait until program terminates, otherwise the program
					continues execution.
	 efno_wait:		? Function unknown. Not implemented in EMX.
	 efoverlay:		Terminate this program, then execute the requested
					program. WARNING: Exit-procedures are not called!
	 efdebug:		Debug program. Details are unknown.
	 efsession:		Do not execute as child of this program. Use a seperate
					session instead.
	 efdetach:		Detached. Function unknown. Info wanted!
	 efpm:			Run as presentation manager program.

	 Determining the window state of the program:
	 efdefault:		Run the pm program in it's default situation.
	 efminimize:	Run the pm program minimized.
	 efmaximize:	Run the pm program maximized.
	 effullscreen:	Run the non-pm program fullscreen.
	 efwindowed:	Run the non-pm program in a window.

	 Other options are not implemented defined because lack of
	 knowledge abou what they do.}

	type	execrunflags=(efwait,efno_wait,efoverlay,efdebug,efsession,
						  efdetach,efpm);
			execwinflags=(efdefault,efminimize,efmaximize,effullscreen,
						  efwindowed);
			execset=set of execrunflags;

	var
	   { error variable }
	   doserror : integer;

	procedure getdate(var year,month,day,dayofweek : word);
	procedure gettime(var hour,minute,second,sec100 : word);
	function dosversion : word;
	procedure setdate(year,month,day : word);
	procedure settime(hour,minute,second,sec100 : word);
	procedure getcbreak(var breakvalue : boolean);
	procedure setcbreak(breakvalue : boolean);
	procedure getverify(var verify : boolean);
	procedure setverify(verify : boolean);
	function diskfree(drive : byte) : longint;
	function disksize(drive : byte) : longint;
	procedure findfirst(const path : pathstr;attr : word;var f : searchRec);
	procedure findnext(var f : searchRec);

	{ is a dummy }
	procedure swapvectors;

{   not supported:
	procedure getintvec(intno : byte;var vector : pointer);
	procedure setintvec(intno : byte;vector : pointer);
	procedure keep(exitcode : word);
}
	procedure msdos(var regs : registers);
	procedure intr(intno : byte;var regs : registers);

	procedure getfattr(var f;var attr : word);
	procedure setfattr(var f;attr : word);

	function fsearch(const path : pathstr;dirlist : string) : pathstr;
	procedure getftime(var f;var time : longint);
	procedure setftime(var f;time : longint);
	procedure packtime (var d: datetime; var time: longint);
	procedure unpacktime (time: longint; var d: datetime);
	function fexpand(const path : pathstr) : pathstr;
	procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;
	  var ext : extstr);
	procedure exec(const path : pathstr;const comline : comstr);
	{$IFDEF OS2}
	function exec(path:pathstr;runflags:execset;winflags:execwinflags;
				  const comline:comstr):longint;
	{$ENDIF}
	function dosexitcode : word;
    function envcount : longint;
	function envstr(index : longint) : string;
	function getenv(const envvar : string): string;

  implementation

  {$ifdef OS2}

  type	OS2FSAllocate=record
			idfilesystem,
			csectorunit,
			cunit,
			cunitavail:longint;
			cbsector:word;
		end;

  function dosqueryFSinfo(driveno:word;infolevel:word;
					  var info;infolen:word):word;
					  external 'DOSCALLS' index 278;

  {$endif OS2}

	{ this was first written for the LINUX version,    }
	{ by Michael Van Canneyt but it works also         }
	{ for the DOS version (I hope so)                  }
	function fsearch(const path : pathstr;dirlist : string) : pathstr;

	  var
		 newdir : pathstr;
		 p1 : byte;
		 s : searchrec;

	  begin
		 if (pos('?',path)<>0) or (pos('*',path)<>0) then
		   { No wildcards allowed in these things }
		   fsearch:=''
		 else
		   begin
			  repeat
				{ get first path }
				p1:=pos(';',dirlist);
				if p1>0 then
				  begin
					 newdir:=copy(dirlist,1,p1-1);
					 delete(dirlist,1,p1)
				  end
				else
				  begin
					 newdir:=dirlist;
					 dirlist:=''
                  end;
                findfirst(newdir+'\'+path,anyfile,s);
                if doserror=0 then
                  begin
                     newdir:=newdir+'\'+s.name;
                     { this was for LINUX:
                     if pos('.\',newdir)=1 then
                       delete(newdir, 1, 2)
                     { DOS strips off an initial .\ }
					 }
				  end
				else newdir:='';
			  until(dirlist='') or (length(newdir)>0);
			  fsearch:=newdir;
		   end;
	  end;

	procedure getftime(var f;var time : longint);

	  begin
	{$IFNDEF OS2}
		 asm
			{ load handle }
			movl f,%ebx
			movw (%ebx),%bx
			{ get date }
			movw $0x5700,%ax
			int $0x21
			shll $16,%edx
			movw %cx,%dx
			movl time,%ebx
			movl %edx,(%ebx)
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
	{$ELSE}
		 asm
			{ load handle }
			movl f,%ebx
			movw (%ebx),%bx
			{ get date }
			movw $0x5700,%ax
			call ___SYSCALL
			shll $16,%edx
			movw %cx,%dx
			movl time,%ebx
			movl %edx,(%ebx)
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
	{$ENDIF}
	  end;

   procedure setftime(var f;time : longint);

	  begin
	  {$IFNDEF OS2}
		 asm
			{ load handle }
			movl f,%ebx
			movw (%ebx),%bx
			movl time,%ecx
			shldl $16,%ecx,%edx
			{ set date }
			movw $0x5701,%ax
			int $0x21
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
	  {$ELSE}
		 asm
			{ load handle }
			movl f,%ebx
			movw (%ebx),%bx
			movl time,%ecx
			shldl $16,%ecx,%edx
			{ set date }
			movw $0x5701,%ax
			call ___SYSCALL
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
	  {$ENDIF}
	  end;

	procedure msdos(var regs : registers);

	{ Not recommended for EMX. Only works in DOS mode, not in OS/2 mode.}

	  begin
		 intr($21,regs);
	  end;

	procedure intr(intno : byte;var regs : registers);

	{ Not recommended for EMX. Only works in DOS mode, not in OS/2 mode.}

	  begin
		 asm
			.data
	int86:
			.byte        0xcd
	int86_vec:
			.byte        0x03
			jmp        int86_retjmp

			.text
			movl        8(%ebp),%eax
			movb        %al,int86_vec

			movl        10(%ebp),%eax
			{do not use first int}
			addl        $2,%eax

			movl        4(%eax),%ebx
			movl        8(%eax),%ecx
			movl        12(%eax),%edx
			movl        16(%eax),%ebp
			movl        20(%eax),%esi
			movl        24(%eax),%edi
			movl        (%eax),%eax

			jmp        int86
	int86_retjmp:
			pushf
			pushl	%ebp
			pushl       %eax
			movl        %esp,%ebp
			{calc EBP new}
			addl        $12,%ebp
			movl        10(%ebp),%eax
			{do not use first int}
			addl        $2,%eax

			popl        (%eax)
			movl        %ebx,4(%eax)
			movl        %ecx,8(%eax)
			movl        %edx,12(%eax)
			{restore EBP}
			popl	%edx
			movl	%edx,16(%eax)
			movl        %esi,20(%eax)
			movl        %edi,24(%eax)
			{ignore ES and DS}
			popl        %ebx        /* flags */
			movl        %ebx,32(%eax)
			{FS and GS too}
		 end;
	  end;

	var
	   lastdosexitcode : word;

	{$IFNDEF OS2}

	procedure exec(const path : pathstr;const comline : comstr);

	  procedure do_system(p : pchar);

		begin
		   asm
			  movl 12(%ebp),%ebx
			  movw $0xff07,%ax
			  int $0x21
			  movw %ax,_LASTDOSEXITCODE
		   end;
		end;

	  var
		 execute : string;
		 b : array[0..255] of char;

	  begin
		 execute:=path+' '+comline;
		 move(execute[1],b,length(execute));
		 b[length(execute)]:=#0;
		 do_system(b);
	  end;
	{$ELSE}

	procedure exec(const path:pathstr;const comline:comstr);

	{Execute a program.}

	begin
		exec(path,[efwait],efdefault,comline);
	end;

	function exec(path:pathstr;runflags:execset;winflags:execwinflags;
				  const comline:comstr):longint;

	{Execute a program. More suitable for OS/2 than the exec above.}

	{512 bytes should be enough to contain the command-line.}

	type	bytearray=array[0..8191] of byte;
			Pbytearray=^bytearray;

			setarray=array[0..3] of byte;

			execstruc=record
				argofs,envofs,nameofs:pointer;
				argseg,envseg,nameseg:word;
				numarg,sizearg,
				numenv,sizeenv:word;
				mode1,mode2:byte;
			end;

	var	args:Pbytearray;
		env:Pbytearray;
		i,j:word;
		es:execstruc;
		esadr:pointer;

	begin
		getmem(args,512);
		getmem(env,8192);
		i:=1;
		j:=0;
		es.numarg:=0;
		while i<=length(comline) do
			begin
				if comline[i]<>' ' then
					begin
						{Commandline argument found. Copy it.}
						inc(es.numarg);
						args^[j]:=$80;
						inc(j);
						while (i<=length(comline)) and (comline[i]<>' ') do
							begin
								args^[j]:=byte(comline[i]);
								inc(j);
								inc(i);
							end;
						args^[j]:=0;
						inc(j);
					end;
				inc(i);
			end;
		args^[j]:=0;
		inc(j);

		{Commandline ready, now build the environment.

		 Oh boy, I always had the opinion that executing a program under Dos
		 was a hard job!}

		asm
			movl env,%edi		{Setup destination pointer.}
			movl _envc,%ecx		{Load number of arguments in edx.}
			movl _environ,%esi	{Load env. strings.}
			xorl %edx,%edx		{Count environment size.}
		exa1:
			lodsl				{Load a Pchar.}
			xchgl %eax,%ebx
		exa2:
			movb (%ebx),%al		{Load a byte.}
			incl %ebx			{Point to next byte.}
			stosb				{Store it.}
			incl %edx			{Increase counter.}
			cmpb $0,%al			{Ready ?.}
			jne exa2
			loop exa1			{Next argument.}
			stosb				{Store an extra 0 to finish. (AL is now 0).}
			incl %edx
			movl %edx,(24)es	{Store environment size.}
		end;

		{Environtment ready, now set-up exec structure.}
		es.argofs:=args;
		es.envofs:=env;
		asm
			leal path,%esi
			lodsb
			movzbl %al,%eax
			incl %eax
			addl %eax,%esi
			movb $0,(%esi)
		end;
		es.nameofs:=pointer(longint(@path)+1);
		asm
			movw %ss,(12)es		{Compiler doesn't like record elems in asm.}
			movw %ss,(14)es
			movw %ss,(16)es
		end;
		es.sizearg:=j;
		es.numenv:=0;
		{Typecasting of sets in FPK is a bit hard.}
		es.mode1:=setarray(runflags)[0];
		es.mode2:=byte(winflags);

		{Now exec the program.}
		esadr:=@es;
		asm
			movl esadr,%edx
			mov $0x7f06,%ax
			call ___SYSCALL
			jnc exprg1
			movl %eax,U_DOS_DOSERROR
			xorl %eax,%eax
			decl %eax
		exprg1:
			movl %eax,__RESULT
		end;

		freemem(args,512);
		freemem(env,8192);
		{Phew! That's it. This was the most sophisticated procedure to call
		 a system function I ever wrote!}
	end;

	{$ENDIF}

	function dosexitcode : word;

	  begin
		 dosexitcode:=lastdosexitcode;
	  end;

	function dosversion : word;

	  begin
		{$IFNDEF OS2}
		 asm
			movb $0x30,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			leave
			ret
		 end;
		{$ELSE}
		 {Returns DOS version in DOS and OS/2 version in OS/2}
		 asm
			movb $0x30,%ah
			call ___SYSCALL
			leave
			ret
		 end;
		{$ENDIF}
	  end;

	procedure getdate(var year,month,day,dayofweek : word);

	  begin
		{$IFNDEF OS2}
		 asm
			movb $0x2a,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			xorb %ah,%ah
			movl 20(%ebp),%edi
			stosw
			movl 16(%ebp),%edi
			movb %dl,%al
			stosw
			movl 12(%ebp),%edi
			movb %dh,%al
			stosw
			movl 8(%ebp),%edi
			movw %cx,%ax
			stosw
		 end;
		{$ELSE}
		 asm
			movb $0x2a,%ah
			call ___SYSCALL
			xorb %ah,%ah
			movl 20(%ebp),%edi
			stosw
			movl 16(%ebp),%edi
			movb %dl,%al
			stosw
			movl 12(%ebp),%edi
			movb %dh,%al
			stosw
			movl 8(%ebp),%edi
			xchgw %ecx,%eax
			stosw
		 end;
		{$ENDIF}
	  end;

	procedure setdate(year,month,day : word);

	  begin
		{$IFNDEF OS2}
		 asm
			movw 8(%ebp),%cx
			movb 10(%ebp),%dh
			movb 12(%ebp),%dl
			movb $0x2b,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
		{$ELSE}
		{DOS only! You cannot change the system date in OS/2!}
		 asm
			movw 8(%ebp),%cx
			movb 10(%ebp),%dh
			movb 12(%ebp),%dl
			movb $0x2b,%ah
			call ___SYSCALL
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
		{$ENDIF}
	  end;

	procedure gettime(var hour,minute,second,sec100 : word);

	  begin
		{$IFNDEF OS2}
		 asm
			movb $0x2c,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			xorb %ah,%ah
			movl 20(%ebp),%edi
			movb %dl,%al
			stosw
			movl 16(%ebp),%edi
			movb %dh,%al
			stosw
			movl 12(%ebp),%edi
			movb %cl,%al
			stosw
			movl 8(%ebp),%edi
			movb %ch,%al
			stosw
		 end;
		{$ELSE}
		 asm
			movb $0x2c,%ah
			call ___SYSCALL
			xorb %ah,%ah
			movl 20(%ebp),%edi
			movb %dl,%al
			stosw
			movl 16(%ebp),%edi
			movb %dh,%al
			stosw
			movl 12(%ebp),%edi
			movb %cl,%al
			stosw
			movl 8(%ebp),%edi
			movb %ch,%al
			stosw
		 end;
		{$ENDIF}
	  end;

	procedure settime(hour,minute,second,sec100 : word);

	  begin
		{$IFNDEF OS2}
		 asm
			movb 8(%ebp),%ch
			movb 10(%ebp),%cl
			movb 12(%ebp),%dh
			movb 14(%ebp),%dl
			movb $0x2d,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
		{$ELSE}
		 asm
			movb 8(%ebp),%ch
			movb 10(%ebp),%cl
			movb 12(%ebp),%dh
			movb 14(%ebp),%dl
			movb $0x2d,%ah
			call ___SYSCALL
			xorb %ah,%ah
			movw %ax,U_DOS_DOSERROR
		 end;
		{$ENDIF}
	  end;

	procedure getcbreak(var breakvalue : boolean);

	  begin
		{$IFNDEF OS2}
		 asm
			movw $0x3300,%ax
			pushl %ebp
			int $0x21
			popl %ebp
			movl 8(%ebp),%eax
			movb %dl,(%eax)
		 end;
		{$ELSE}
		 {! Do not use in OS/2. Also not recommended in DOS. Use
			signal handling instead.}
		 asm
			movw $0x3300,%ax
			call ___SYSCALL
			movl 8(%ebp),%eax
			movb %dl,(%eax)
		 end;
		{$ENDIF}
	  end;

	procedure setcbreak(breakvalue : boolean);

	  begin
		{$IFNDEF OS2}
		 asm
			movb 8(%ebp),%dl
			movl $0x3301,%ax
			pushl %ebp
			int $0x21
			popl %ebp
		 end;
		{$ELSE}
		 {! Do not use in OS/2. Also not recommended in DOS. Use
			signal handling instead.}
		 asm
			movb 8(%ebp),%dl
			movl $0x3301,%ax
			call ___SYSCALL
		 end;
		{$ENDIF}
	  end;

	procedure getverify(var verify : boolean);

	  begin
		{$IFNDEF OS2}
		 asm
			movb $0x54,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			movl 8(%ebp),%edi
			stosb
		 end;
		{$ELSE}
		 {! Do not use in OS/2.}
		 asm
			movb $0x54,%ah
			call ___SYSCALL
			movl 8(%ebp),%edi
			stosb
		 end;
		{$ENDIF}
	  end;

	procedure setverify(verify : boolean);

	  begin
		{$IFNDEF OS2}
		 asm
			movb 8(%ebp),%al
			movl $0x2e,%ah
			pushl %ebp
			int $0x21
			popl %ebp
		 end;
		{$ELSE}
		 {! Do not use in OS/2.}
		 asm
			movb 8(%ebp),%al
			movl $0x2e,%ah
			call ___SYSCALL
		 end;
		{$ENDIF}
	  end;

	function diskfree(drive : byte) : longint;

	var	fi:OS2FSallocate;

	begin
		{$IFNDEF OS2}
		asm
			movb 8(%ebp),%dl
			movb $0x36,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			cmpw $-1,%ax
			je LDISKFREE1
			mulw %cx
			mulw %bx
			shll $16,%edx
			movw %ax,%dx
			movl %edx,%eax
			leave
			ret
		 LDISKFREE1:
			cwde
			leave
			ret
		end;
		{$ELSE}
		if os_mode=osDOS then
			{Function 36 is not supported in OS/2.}
			asm
				movb 8(%ebp),%dl
				movb $0x36,%ah
				call ___SYSCALL
				cmpw $-1,%ax
				je LDISKFREE1
				mulw %cx
				mulw %bx
				shll $16,%edx
				movw %ax,%dx
				xchgl %edx,%eax
				leave
				ret
			 LDISKFREE1:
				cwde
				leave
				ret
			end
		 else
			{In OS/2, we use the filesystem information.}
			begin
				doserror:=dosqueryFSinfo(drive,1,FI,sizeof(FI));
				if doserror=0 then
					diskfree:=FI.cunitavail*FI.csectorunit*FI.cbsector
				else
					diskfree:=-1;
			end;
		{$ENDIF}
	end;

	function disksize(drive : byte) : longint;

	begin
		{$IFNDEF OS/2}
		asm
			movb 8(%ebp),%dl
			movb $0x36,%ah
			pushl %ebp
			int $0x21
			popl %ebp
			movw %dx,%bx
			cmpw $-1,%ax
			je LDISKSIZE1
			mulw %cx
			mulw %bx
			shll $16,%edx
			movw %ax,%dx
			movl %edx,%eax
			leave
			ret
		 LDISKSIZE1:
			movl $-1,%eax
			leave
			ret
		end;
		{$ELSE}
		if os_mode=osDOS then
			{Function 36 is not supported in OS/2.}
			asm
				movb 8(%ebp),%dl
				movb $0x36,%ah
				call ___SYSCALL
				movw %dx,%bx
				cmpw $-1,%ax
				je LDISKSIZE1
				mulw %cx
				mulw %bx
				shll $16,%edx
				movw %ax,%dx
				xchgl %edx,%eax
				leave
				ret
			 LDISKSIZE1:
				cwde
				leave
				ret
			 end;
		 else
			{In OS/2, we use the filesystem information.}
			begin
				doserror:=dosQFSinfo(drive,1,FI,sizeof(FI));
				if doserror=0 then
					diskfree:=FI.cunit*FI.csectorunit*FI.cbsector
				else
					diskfree:=-1;
			end;
		{$ENDIF}
	  end;

	procedure searchrec2dossearchrec(var f : searchrec);

	  var
		 l,i : longint;

	  {$IFDEF OS2}
	  const	namesize=255;
	  {$ELSE}
	  const	namesize=12;
	  {$ENDIF}

	  begin
		 l:=length(f.name);
		 for i:=1 to namesize do
		   f.name[i-1]:=f.name[i];
		 f.name[l]:=#0;
	  end;

	procedure dossearchrec2searchrec(var f : searchrec);

	  var
		 l,i : longint;

	  {$IFDEF OS2}
	  const	namesize=255;
	  {$ELSE}
	  const namesize=12;
	  {$ENDIF}

	  begin
		 for i:=0 to namesize do
		   if f.name[i]=#0 then
			 begin
				l:=i;
				break;
			 end;
		 for i:=namesize-1 downto 0 do
		   f.name[i+1]:=f.name[i];
		 f.name[0]:=chr(l);
	  end;

	procedure findfirst(const path : pathstr;attr : word;var f : searchRec);

	  procedure _findfirst(path : pchar;attr : word;var f : searchrec);

		begin
		   {$IFNDEF OS2}
		   asm
			  movl 18(%ebp),%edx
			  movb $0x1a,%ah
			  int $0x21
			  movl 12(%esp),%edx
			  movzwl 16(%esp),%ecx
			  movb $0x4e,%ah
			  int $0x21
			  jnc LFF
			  movw %ax,U_DOS_DOSERROR
		   LFF:
		   end;
		   {$ELSE}
		   asm
			  movl 12(%esp),%edx
			  movw 16(%esp),%cx
			  {No need to set DTA in EMX. Just give a pointer in ESI.}
			  movl 18(%ebp),%esi
			  movb $0x4e,%ah
			  call ___SYSCALL
			  jnc LFF
			  movw %ax,U_DOS_DOSERROR
		   LFF:
		   end;
		   {$ENDIF}
		end;

	  var
		 path0 : array[0..80] of char;

	  begin
		 { no error }
		 doserror:=0;
		 strpcopy(path0,path);
		 _findfirst(path0,attr,f);
		 dossearchrec2searchrec(f);
	  end;

	procedure findnext(var f : searchRec);

	  procedure _findnext(var f : searchrec);

		begin
		   {$IFNDEF OS2}
		   asm
			  movl 12(%ebp),%edx
			  movb $0x1a,%ah
			  int $0x21
			  movb $0x4f,%ah
			  int $0x21
			  jnc LFN
			  movw %ax,U_DOS_DOSERROR
		   LFN:
		   end;
		   {$ELSE}
		   asm
			  movl 12(%ebp),%esi
			  movb $0x4f,%ah
			  call ___SYSCALL
			  jnc LFN
			  movw %ax,U_DOS_DOSERROR
		   LFN:
		   end;
		   {$ENDIF}
		end;

	  begin
		 { no error }
		 doserror:=0;
		 searchrec2dossearchrec(f);
		 _findnext(f);
		 dossearchrec2searchrec(f);
	  end;

	procedure swapvectors;

	  begin
		 { tut nichts, DOS-Extender Åbernimmt das Nîtige }
		 { normalerweise selber                          }
		 { nur aus KompatibilitÑtsgrÅnden implementiert  }
	  end;

	type
	   ppchar = ^pchar;

	function envs : ppchar;

	  begin
		 asm
			movl _environ,%eax
			leave
			ret
		 end ['EAX'];
	  end;

	function envcount : longint;

	  var
		 hp : ppchar;

	  begin
		{$IFNDEF OS2}
		 hp:=envs;
		 envcount:=0;
		 while assigned(hp^) do
		   begin
			  { not the best solution, but quite understandable }
			  inc(envcount);
			  hp:=hp+4;
		   end;
		{$ELSE}
			asm
				movl _envc,%eax
				leave
				ret
			end ['EAX'];
		{$ENDIF}
	  end;

	function envstr(index : longint) : string;

	  var
		 hp : ppchar;

	  begin
		 if (index<=0) or (index>envcount) then
		   begin
              envstr:='';
              exit;
           end;
		 hp:=envs+4*(index-1);
         envstr:=strpas(hp^);
	  end;

	function getenv(const envvar : string) : string;

      var
         hs,_envvar : string;
         eqpos,i : longint;

      begin
         _envvar:=upcase(envvar);
         getenv:='';
		 for i:=1 to envcount do
		   begin
              hs:=envstr(i);
              eqpos:=pos('=',hs);
			  if copy(hs,1,eqpos-1)=_envvar then
                begin
				   getenv:=copy(hs,eqpos+1,length(hs)-eqpos);
                   exit;
                end;
           end;
      end;

	procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;
      var ext : extstr);

      var
         p1 : byte;

      begin
         { try to find out a extension }
         p1:=pos('.',path);
         if p1>0 then
		   begin
              ext:=copy(path,p1,4);
              delete(path,p1,length(path)-p1+1);
           end
		 else
           ext:='';
		 { get drive name }
         p1:=pos(':',path);
         if p1>0 then
           begin
			  dir:=path[1]+':';
              delete(path,1,p1);
           end
         else
           dir:='';
         { split the path and the name, there are no more path informtions }
         { if path contains no backslashes                                 }
         while true do
           begin
              p1:=pos('\',path);
              if p1=0 then
                break;
              dir:=dir+copy(path,1,p1);
			  delete(path,1,p1);
		   end;
         name:=path;
      end;

    function fexpand(const path : pathstr) : pathstr;

      function get_current_drive : byte;
      
        var
           r : registers;
           
		begin
           r.ah:=$19;
           msdos(r);
           get_current_drive:=r.al;
        end;           

	   var
		  {$IFDEF DOS}
		  s,pa : string[79];
		  {$ELSE}
		  s,pa:string;
		  {$ENDIF}

	   begin
		  { There are differences between FPKPascal and Turbo Pascal
			e.g. for the string 'D:\DEMO\..\HELLO' which isn't handled }
		  getdir(0,s);
		  pa:=upcase(path);
		  if (byte(pa[0])>1) and ((pa[1] in ['A'..'Z']) and (pa[2]=':')) then
			begin
			   if (byte(pa[0])>2) and (pa[3]<>'\') then
				 if pa[1]=s[1] then
				   pa:=s+'\'+copy (pa,3,length(pa))
				 else
				   pa:=pa[1]+':\'+copy (pa,3,length(pa))
			end
		  else
			if pa[1]='\' then
			  pa:=s[1]+':'+pa
			else if s[0]=#3 then
			  pa:=s+pa
			else
			  pa:=s+'\'+pa;
		  fexpand:=pa;
	   end;

     procedure packtime(var d : datetime;var time : longint);

	   var
          zs : longint;

	   begin
          time:=-1980;
          time:=time+d.year and 127;
          time:=time shl 4;
          time:=time+d.month;
          time:=time shl 5;
          time:=time+d.day;
          time:=time shl 16;
		  zs:=d.hour;
          zs:=zs shl 6;
          zs:=zs+d.min;
          zs:=zs shl 5;
		  zs:=zs+d.sec div 2;
		  time:=time+(zs and $ffff);
	   end;

     procedure unpacktime (time: longint; var d: datetime);

	   begin
          d.sec:=(time and 31) * 2;
          time:=time shr 5;
          d.min:=time and 63;
          time:=time shr 6;
          d.hour:=time and 31;
          time:=time shr 5;
          d.day:=time and 31;
          time:=time shr 5;
          d.month:=time and 15;
          time:=time shr 4;
          d.year:=time + 1980;
       end;

    procedure getfattr(var f;var attr : word);

      var
         { to avoid problems }
		 n : array[0..255] of char;
		 {$IFNDEF OS2}
		 r : registers;
		 {$ENDIF}

	  begin
		 strpcopy(n,filerec(f).name);
		{$IFNDEF OS2}
		 r.ax:=$4300;
		 r.edx:=longint(@n);
		 msdos(r);
		 attr:=r.cx;
		{$ELSE}
		 {Alas, msdos(r) doesn't work when we are running in OS/2.}
		 asm
			movw $0x4300,%ax
			leal n,%edx
			call ___SYSCALL
			movl attr,%ebx
			movw %cx,(%ebx)
		 end;
		{$ENDIF}
	  end;

	procedure setfattr(var f;attr : word);

	  var
		 { to avoid problems }
		 n : array[0..255] of char;
		 {$IFNDEF OS2}
		 r : registers;
		 {$ENDIF}

	  begin
		 strpcopy(n,filerec(f).name);
		{$IFNDEF OS2}
		 r.ax:=$4301;
		 r.edx:=longint(@n);
		 r.cx:=attr;
		 msdos(r);
		{$ELSE}
		 {Alas, msdos(r) doesn't work when we are running in OS/2.}
		 asm
			movw $0x4301,%ax
			leal n,%edx
			movw attr,%cx
			call ___SYSCALL
		 end;
		{$ENDIF}
	  end;
end.
