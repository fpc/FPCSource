{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ to be able to cross compile from v1 to v2 }

{$I os.inc}

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
  30th may 1997:
             bug in fsplit fixed (thanks to Pierre Muller):
               If you have a relative path as argument
               fsplit gives a wrong result because it
               first tries to find the extension by searching the first
               occurence of '.'.

               The file extension should be tested last !!
  15th june 1997:
             versions for go32v1 and go32v2 merged
  september 1997:
             removed some bugs for go32v2
             - searchrec structure is different (direct dos call !!)
  27th november 1997:
             bug in findfirst fixed esp was instead of ebp used
}

{$ifndef GO32V2}
{$ifdef DOS}
{$define GO32V1}
{$endif DOS}
{$endif not GO32V2}

unit dos;

  interface

    uses
       strings
{$ifdef GO32V2}
       ,go32
{$endif GO32V2}
       ;

    const
       { bit masks for CPU flags}
       fcarry = $0001;
       fparity = $0004;
       fauxiliary = $0010;
       fzero = $0040;
       fsign = $0080;
       foverflow  = $0800;

       { bit masks for file attributes }
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
       comstr = string[127];        { command line string }
       pathstr = string[79];        { string for a file path }
       dirstr = string[67];         { string for a directory }
       namestr = string[8];         { string for a file name }
       extstr = string[4];          { string for an extension }

       { search record which is used by findfirst and findnext }
{$ifndef GO32V2}
{$PACKRECORDS 1}
       searchrec = record
          fill : array[1..21] of byte;
          attr : byte;
          time : longint;
          reserved : word; { requires the DOS extender (DJ GNU-C) }
          size : longint;
          name : string[15]; { the same size as declared by (DJ GNU C) }
       end;
{$else GO32V2}
{$PACKRECORDS 1}
       searchrec = record
          fill : array[1..21] of byte;
          attr : byte;
          time : longint;
          { reserved : word; not in DJGPP V2 }
          size : longint;
          name : string[12]; { the same size as declared by (DJ GNU C) }
       end;
{$endif GO32V2}
{$PACKRECORDS 2}

       { file record for untyped files comes from filerec.inc}
       {$i filerec.inc}

       { file record for text files  comes from textrec.inc}
       {$i textrec.inc}

{$ifdef GO32V1}
       { data structure for the registers needed by msdos and intr }
       { Go32 V2 follows trealregs of go32 }

       registers = record
         case i : integer of
            0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
            1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
            2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
       end;
{$endif GO32V1}

{$ifdef GO32V2}
       { data structure for the registers needed by msdos and intr }
       { Go32 V2 follows trealregs of go32 }

       registers = go32.registers;

{$endif GO32V2}

{$PACKRECORDS 1}
       { record for date and time }
       datetime = record
          year,month,day,hour,min,sec : word;
       end;

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

    { is a dummy for go32v1 not for go32v2 }
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
    function dosexitcode : word;
    function envcount : longint;
    function envstr(index : longint) : string;
    function getenv(const envvar : string): string;

  implementation

    var
       dosregs : registers;

    { this was first written for the LINUX version,    }
    { by Michael Van Canneyt but it works also         }
    { for the DOS version (I hope so)                  }
    function fsearch(const path : pathstr;dirlist : string) : pathstr;

      var
         newdir : pathstr;
         i,p1 : byte;
         s : searchrec;

      begin
         if (pos('?',path)<>0) or (pos('*',path)<>0) then
           { No wildcards allowed in these things }
           fsearch:=''
         else
           begin
              { allow slash as backslash }
              for i:=1 to length(dirlist) do
                if dirlist[i]='/' then dirlist[i]:='\';

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
                if (newdir[length(newdir)]<>'\') and
                   (newdir[length(newdir)]<>':') then
                   newdir:=newdir+'\';
                findfirst(newdir+path,anyfile,s);
                if doserror=0 then
                  begin
                     { this should be newdir:=newdir+path
                     because path can contain a path part !! }
                     {newdir:=newdir+s.name;}
                     newdir:=newdir+path;
                     { this was for LINUX:
                     if pos('.\',newdir)=1 then
                       delete(newdir, 1, 2)
                      DOS strips off an initial .\
                     }
                  end
                else newdir:='';
              until(dirlist='') or (length(newdir)>0);
              fsearch:=newdir;
           end;
      end;

    procedure getftime(var f;var time : longint);

      begin
         dosregs.bx:=textrec(f).handle;
         dosregs.ax:=$5700;
         msdos(dosregs);
         time:=(dosregs.dx shl 16)+dosregs.cx;
         doserror:=dosregs.al;
      end;

   procedure setftime(var f;time : longint);

      begin
         dosregs.bx:=textrec(f).handle;
         dosregs.ecx:=time;
         dosregs.ax:=$5701;
         msdos(dosregs);
         doserror:=dosregs.al;
      end;

    procedure msdos(var regs : registers);

      begin
         intr($21,regs);
      end;
{$ifdef GO32V2}
    procedure intr(intno : byte;var regs : registers);

      begin
         realintr(intno,regs);
      end;
{$else GO32V2}
    procedure intr(intno : byte;var regs : registers);

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
            // do not use first int
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
            // calc EBP new
            addl        $12,%ebp
            movl        10(%ebp),%eax
            // do not use first int
            addl        $2,%eax

            popl        (%eax)
            movl        %ebx,4(%eax)
            movl        %ecx,8(%eax)
            movl        %edx,12(%eax)
            // restore EBP
            popl	%edx
            movl	%edx,16(%eax)
            movl        %esi,20(%eax)
            movl        %edi,24(%eax)
            // ignore ES and DS
            popl        %ebx        /* flags */
            movl        %ebx,32(%eax)
            // FS and GS too
         end;
      end;
{$endif GO32V2}
    var
       lastdosexitcode : word;
{$ifdef GO32V2}

    { this code is just the most basic part of dosexec.c from
    the djgpp code }

    procedure exec(const path : pathstr;const comline : comstr);

      procedure do_system(p,c : string);

      {
        Table 0931
        Format of EXEC parameter block for AL=00h,01h,04h:
        Offset	Size	Description
         00h	WORD	segment of environment to copy for child process (copy caller's
		          environment if 0000h)
         this does not seem to work (PM)
         02h	DWORD	pointer to command tail to be copied into child's PSP
         06h	DWORD	pointer to first FCB to be copied into child's PSP
         0Ah	DWORD	pointer to second FCB to be copied into child's PSP
         0Eh	DWORD	(AL=01h) will hold subprogram's initial SS:SP on return
         12h	DWORD	(AL=01h) will hold entry point (CS:IP) on return
        INT 21 4B--

        Copied from Ralf Brown's Interrupt List
      }

      type
         realptr = record
	    ofs,seg : word;
  	 end;

         texecblock = record
	    envseg : word;
	    comtail : realptr;
	    firstFCB : realptr;
	    secondFCB : realptr;
	    iniStack : realptr;
	    iniCSIP : realptr;
 	 end;

      var current_dos_buffer_pos : longint;
      function paste_to_dos(src : string) : boolean;
        var c : array[0..255] of char;
        begin
           paste_to_dos:=false;
           if current_dos_buffer_pos+length(src)+1>transfer_buffer+tb_size then
             begin
              doserror:=200;{ what value should we use here ? }
              exit;
             end;
           move(src[1],c[0],length(src));
           c[length(src)]:=#0;
           seg_move(get_ds,longint(@c),dosmemselector,current_dos_buffer_pos,length(src)+1);
           current_dos_buffer_pos:=current_dos_buffer_pos+length(src)+1;
           paste_to_dos:=true;
        end;
      var
         i,la_env,la_p,la_c,la_e,fcb1_la,fcb2_la : longint;
         arg_ofs : longint;
	      execblock : texecblock;

      begin
         la_env:=transfer_buffer;
         while (la_env mod 16)<>0 do inc(la_env);
         current_dos_buffer_pos:=la_env;
         for i:=1 to envcount do
           begin
              paste_to_dos(envstr(i));
           end;
         paste_to_dos(''); { adds a double zero at the end }
         { allow slash as backslash }
         for i:=1 to length(p) do
           if p[i]='/' then p[i]:='\';
         la_p:=current_dos_buffer_pos;
         paste_to_dos(p);
         la_c:=current_dos_buffer_pos;
         paste_to_dos(c);
	      la_e:=current_dos_buffer_pos;
         fcb1_la:=la_e;
         la_e:=la_e+16;
         fcb2_la:=la_e;
         la_e:=la_e+16;
         { allocate FCB see dosexec code }
         dosregs.ax:=$2901;
         arg_ofs:=1;
         while (c[arg_ofs]=' ') or (c[arg_ofs]=#9) do inc(arg_ofs);
         dosregs.ds:=(la_c+arg_ofs) div 16;
         dosregs.si:=(la_c+arg_ofs) mod 16;
         dosregs.es:=fcb1_la div 16;
         dosregs.di:=fcb1_la mod 16;
         msdos(dosregs);
         repeat
            inc(arg_ofs);
         until (c[arg_ofs]=' ') or
               (c[arg_ofs]=#9) or
               (c[arg_ofs]=#13);
         if c[arg_ofs]<>#13 then
           begin
              inc(arg_ofs);
              while (c[arg_ofs]=' ') or (c[arg_ofs]=#9) do inc(arg_ofs);
           end;
         { allocate second FCB see dosexec code }
         dosregs.ax:=$2901;
         dosregs.ds:=(la_c+arg_ofs) div 16;
         dosregs.si:=(la_c+arg_ofs) mod 16;
         dosregs.es:=fcb2_la div 16;
         dosregs.di:=fcb2_la mod 16;
         msdos(dosregs);
     	   with execblock do
      	  begin
      	     envseg:=la_env div 16;
      	     comtail.seg:=la_c div 16;
      	     comtail.ofs:=la_c mod 16;
      	     firstFCB.seg:=fcb1_la div 16;
      	     firstFCB.ofs:=fcb1_la mod 16;
      	     secondFCB.seg:=fcb2_la div 16;
      	     secondFCB.ofs:=fcb2_la mod 16;
      	  end;
       	seg_move(get_ds,longint(@execblock),dosmemselector,la_e,sizeof(texecblock));
         dosregs.edx:=la_p mod 16;
         dosregs.ds:=la_p div 16;
         dosregs.ebx:=la_e mod 16;
         dosregs.es:=la_e div 16;
         dosregs.ax:=$4b00;
         msdos(dosregs);
         if (dosregs.flags and 1) <> 0 then
           begin
              doserror:=dosregs.ax;
   	        lastdosexitcode:=0;
              exit;
           end
         else
           begin
              dosregs.ax:=$4d00;
              msdos(dosregs);
              lastdosexitcode:=dosregs.al;
           end;
        end;

      { var
         p,c : array[0..255] of char; }
        var  c : string;
      begin
         doserror:=0;
         { move(path[1],p,length(path));
         p[length(path)]:=#0; }
         move(comline[0],c[1],length(comline)+1);
         c[length(comline)+2]:=#13;
         c[0]:=char(length(comline)+2);
         do_system(path,c);
      end;

{$else GO32V2}

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
         i : longint;
         execute : string;
         b : array[0..255] of char;

      begin
         doserror:=0;
         execute:=path+' '+comline;
         { allow slash as backslash for the program name only }
         for i:=1 to length(path) do
           if execute[i]='/' then execute[i]:='\';
         move(execute[1],b,length(execute));
         b[length(execute)]:=#0;
         do_system(b);
      end;

{$endif GO32V2}

    function dosexitcode : word;

      begin
         dosexitcode:=lastdosexitcode;
      end;

    function dosversion : word;

      begin
         dosregs.ax:=$3000;
         msdos(dosregs);
         dosversion:=dosregs.ax;
      end;

    procedure getdate(var year,month,day,dayofweek : word);

      begin
         dosregs.ax:=$2a00;
         msdos(dosregs);
         dayofweek:=dosregs.al;
         year:=dosregs.cx;
         month:=dosregs.dh;
         day:=dosregs.dl;
      end;

    procedure setdate(year,month,day : word);

      begin
         dosregs.cx:=year;
         dosregs.dx:=month*$100+day;
         dosregs.ah:=$2b;
         msdos(dosregs);
         doserror:=dosregs.al;
      end;

    procedure gettime(var hour,minute,second,sec100 : word);

      begin
         dosregs.ah:=$2c;
         msdos(dosregs);
         hour:=dosregs.ch;
         minute:=dosregs.cl;
         second:=dosregs.dh;
         sec100:=dosregs.dl;
      end;

    procedure settime(hour,minute,second,sec100 : word);

      begin
         dosregs.cx:=hour*$100+minute;
         dosregs.dx:=second*$100+sec100;
         dosregs.ah:=$2d;
         msdos(dosregs);
         doserror:=dosregs.al;
      end;

    procedure getcbreak(var breakvalue : boolean);

      begin
         dosregs.ax:=$3300;
         msdos(dosregs);
         breakvalue:=dosregs.dl<>0;
      end;

    procedure setcbreak(breakvalue : boolean);

      begin
         dosregs.ax:=$3301;
         dosregs.dl:=ord(breakvalue);
         msdos(dosregs);
      end;

    procedure getverify(var verify : boolean);

      begin
         dosregs.ah:=$54;
         msdos(dosregs);
         verify:=dosregs.al<>0;
      end;

    procedure setverify(verify : boolean);

      begin
         dosregs.ah:=$2e;
         dosregs.al:=ord(verify);
         msdos(dosregs);
      end;

    function diskfree(drive : byte) : longint;

      begin
         dosregs.dl:=drive;
         dosregs.ah:=$36;
         msdos(dosregs);
         if dosregs.ax<>$FFFF then
           begin
              diskfree:=dosregs.ax;
              diskfree:=diskfree*dosregs.bx;
              diskfree:=diskfree*dosregs.cx;
           end
         else
           diskfree:=-1;
      end;

    function disksize(drive : byte) : longint;

      begin
         dosregs.dl:=drive;
         dosregs.ah:=$36;
         msdos(dosregs);
         if dosregs.ax<>$FFFF then
           begin
              disksize:=dosregs.ax;
              disksize:=disksize*dosregs.cx;
              disksize:=disksize*dosregs.dx;
           end
         else
           disksize:=-1;
      end;

    procedure searchrec2dossearchrec(var f : searchrec);

      var
         l,i : longint;

      begin
         l:=length(f.name);
         for i:=1 to 12 do
           f.name[i-1]:=f.name[i];
         f.name[l]:=#0;
      end;

    procedure dossearchrec2searchrec(var f : searchrec);

      var
         l,i : longint;

      begin
         l:=12;
         for i:=0 to 12 do
           if f.name[i]=#0 then
             begin
                l:=i;
                break;
             end;
         for i:=11 downto 0 do
           f.name[i+1]:=f.name[i];
         f.name[0]:=chr(l);
      end;

    procedure findfirst(const path : pathstr;attr : word;var f : searchRec);

{$ifdef GO32V2}

      procedure _findfirst(path : pchar;attr : word;var f : searchrec);

        var
           i : longint;
        begin
           { allow slash as backslash }
           for i:=0 to strlen(path) do
             if path[i]='/' then path[i]:='\';
           copytodos(f,sizeof(searchrec));
           dosregs.edx:=transfer_buffer mod 16;
           dosregs.ds:=transfer_buffer div 16;
           dosregs.ah:=$1a;
           msdos(dosregs);
           dosregs.ecx:=attr;
           dosregs.edx:=(transfer_buffer mod 16) + Sizeof(searchrec)+1;
           dosmemput(transfer_buffer div 16,
             (transfer_buffer mod 16) +Sizeof(searchrec)+1,path^,strlen(path)+1);
           dosregs.ds:=transfer_buffer div 16;
	   dosregs.ah:=$4e;
           msdos(dosregs);
           copyfromdos(f,sizeof(searchrec));
           if dosregs.flags and carryflag<>0 then
             doserror:=dosregs.ax;
        end;

{$else GO32V2}

      procedure _findfirst(path : pchar;attr : word;var f : searchrec);

        var
           i : longint;
        begin
           { allow slash as backslash }
           for i:=0 to strlen(path) do
             if path[i]='/' then path[i]:='\';
           asm
              movl 18(%ebp),%edx
              movb $0x1a,%ah
              int $0x21
              movl 12(%ebp),%edx
              movzwl 16(%ebp),%ecx
              movb $0x4e,%ah
              int $0x21
              jnc .LFF
              movw %ax,U_DOS_DOSERROR
           .LFF:
           end;
        end;

{$endif GO32V2}

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

{$ifdef GO32V2}

      procedure _findnext(var f : searchrec);

        begin
           copytodos(f,sizeof(searchrec));
           dosregs.edx:=transfer_buffer mod 16;
           dosregs.ds:=transfer_buffer div 16;
	        dosregs.ah:=$1a;
           msdos(dosregs);
	        dosregs.ah:=$4f;
           msdos(dosregs);
           copyfromdos(f,sizeof(searchrec));
           if dosregs.flags and carryflag <> 0 then
             doserror:=dosregs.ax;
        end;

{$else GO32V2}

      procedure _findnext(var f : searchrec);

        begin
           asm
              movl 12(%ebp),%edx
              movb $0x1a,%ah
              int $0x21
              movb $0x4f,%ah
              int $0x21
              jnc .LFN
              movw %ax,U_DOS_DOSERROR
           .LFN:
           end;
        end;

{$endif GO32V2}

      begin
         { no error }
         doserror:=0;
         searchrec2dossearchrec(f);
         _findnext(f);
         dossearchrec2searchrec(f);
      end;

    procedure swapvectors;

{$ifdef go32v2}
{ uses four global symbols from v2prt0.as
  to be able to know the current exception state
  without using dpmiexcp unit }
      begin
         asm
            movl _exception_exit,%eax
            orl  %eax,%eax
            je   .Lno_excep
            movl _v2prt0_exceptions_on,%eax
            orl  %eax,%eax
            je   .Lexceptions_off
            movl _swap_out,%eax
            call *%eax
            jmp  .Lno_excep
         .Lexceptions_off:
            movl _swap_in,%eax
            call *%eax
         .Lno_excep:
         end;
      end;
{$else not go32v2}
      begin
         { only a dummy }
      end;
{$endif go32v2}

    type
       ppchar = ^pchar;

{$ifdef GO32V1}

    function envs : ppchar;

      begin
         asm
            movl _environ,%eax
            leave
            ret
         end ['EAX'];
      end;

{$endif}

    function envcount : longint;

      var
         hp : ppchar;

      begin
{$ifdef GO32V2}
         hp:=environ;
{$else GO32V2}
         hp:=envs;
{$endif}
         envcount:=0;
         while assigned(hp^) do
           begin
              { not the best solution, but quite understandable }
              inc(envcount);
              hp:=hp+4;
           end;
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
{$ifdef GO32V2}
         hp:=environ+4*(index-1);
{$else GO32V2}
         hp:=envs+4*(index-1);
{$endif GO32V2}
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
         i : longint;
      begin
         { allow slash as backslash }
         for i:=1 to length(path) do
             if path[i]='/' then path[i]:='\';
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
         { try to find out a extension }
         p1:=pos('.',path);
         if p1>0 then
           begin
              ext:=copy(path,p1,4);
              delete(path,p1,length(path)-p1+1);
           end
         else
           ext:='';
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
          s,pa : string[79];
          i,j : byte;

       begin
          { There are differences between FPKPascal and Turbo Pascal
            e.g. for the string 'D:\DEMO\..\HELLO' which isn't handled }
          getdir(0,s);
          pa:=upcase(path);
          { allow slash as backslash }
          for i:=1 to length(pa) do
             if pa[i]='/' then pa[i]:='\';
          if (ord(pa[0])>1) and (((pa[1]>='A') and (pa[1]<='Z')) and (pa[2]=':')) then
            begin
               { we must get the right directory }
               getdir(ord(pa[1])-ord('A')+1,s);
               if (ord(pa[0])>2) and (pa[3]<>'\') then
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
        {First remove all references to '\.\'}
          while pos ('\.\',pa)<>0 do
           delete (pa,pos('\.\',pa),2);
        {Now remove also all references to '\..\' + of course previous dirs..}
          repeat
            i:=pos('\..\',pa);
            if i<>0 then j:=i-1;
            while (j>1) and (pa[j]<>'\') do
             dec (j);
            delete (pa,j,i-j+3);
          until i=0;
        {Remove End . and \}
          if (length(pa)>0) and (pa[length(pa)]='.') then
           dec(byte(pa[0]));
          if (length(pa)>0) and (pa[length(pa)]='\') then
           dec(byte(pa[0]));
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

{$ifdef GO32V2}

    procedure getfattr(var f;var attr : word);

      var
         r : registers;

      begin
         copytodos(filerec(f).name,strlen(filerec(f).name)+1);
         r.ax:=$4300;
         r.edx:=transfer_buffer mod 16;
         r.ds:=transfer_buffer div 16;
         msdos(r);
         if (r.flags and carryflag) <> 0 then
           doserror:=r.ax;
         attr:=r.cx;
      end;

    procedure setfattr(var f;attr : word);

      var
         r : registers;

      begin
         copytodos(filerec(f).name,strlen(filerec(f).name)+1);
         r.ax:=$4301;
         r.edx:=transfer_buffer mod 16;
         r.ds:=transfer_buffer div 16;
         r.cx:=attr;
         msdos(r);
         if (r.flags and carryflag) <> 0 then
           doserror:=r.ax;
      end;

{$else GO32V2}

    procedure getfattr(var f;var attr : word);

      var
         { to avoid problems }
         n : array[0..255] of char;
         r : registers;

      begin
         strpcopy(n,filerec(f).name);
         r.ax:=$4300;
         r.edx:=longint(@n);
         msdos(r);
         attr:=r.cx;
      end;

    procedure setfattr(var f;attr : word);

      var
         { to avoid problems }
         n : array[0..255] of char;
         r : registers;

      begin
         strpcopy(n,filerec(f).name);
         r.ax:=$4301;
         r.edx:=longint(@n);
         r.cx:=attr;
         msdos(r);
      end;

{$endif GO32V2}

end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:41  root
  Initial revision

  Revision 1.10  1998/03/12 04:02:32  carl
    * bugfix of Range Check error in FExpand

  Revision 1.9  1998/02/05 12:08:48  pierre
    * added packrecords to about dword alignment
      for structures used in dos calls

  Revision 1.8  1998/02/03 15:52:41  pierre
    * swapvectors really disable exception handling
      and interrupt redirection with go32v2
    * in dos.pp bug if arg path from fsearch had a directory part fixed

  Revision 1.7  1998/01/26 11:56:22  michael
  + Added log at the end


  
  Working file: rtl/dos/dos.pp
  description:
  ----------------------------
  revision 1.6
  date: 1998/01/16 00:04:58;  author: michael;  state: Exp;  lines: +17 -18
  Added some fixes of Peter Vreman
  ----------------------------
  revision 1.5
  date: 1997/12/22 10:22:05;  author: pierre;  state: Exp;  lines: +2 -2
    * bug in disksize corrected (thanks to Papai Andras)
  ----------------------------
  revision 1.4
  date: 1997/12/12 13:17:15;  author: florian;  state: Exp;  lines: +3 -2
  dos.doserror wasn't set to zero in dos.exec (go32v2)
  ----------------------------
  revision 1.3
  date: 1997/12/01 12:15:45;  author: michael;  state: Exp;  lines: +12 -5
  + added copyright reference in header.
  ----------------------------
  revision 1.2
  date: 1997/11/27 22:49:03;  author: florian;  state: Exp;  lines: +6 -5
  - CPU.PP added
  - some bugs in DOS fixed (espsecially for go32v1)
  - the win32 system unit is now compilable
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
