Unit system;

interface

// Was needed too bootstrap with our old 2.1 fpc for BeOS
// to define real
{ $define VER2_0}

{$define FPC_IS_SYSTEM}

{$I sysunixh.inc}

  
type
  THeapPointer = ^pointer;
var
  heapstartpointer : THeapPointer;
  heapstart : pointer;//external;//external name 'HEAP';
  myheapsize : longint; //external;//external name 'HEAPSIZE';
  myheaprealsize : longint;
  heap_handle : longint;
implementation

procedure debugger(s : PChar); cdecl; external 'root' name 'debugger';
//begin
//end;

{ OS independant parts}

{$I system.inc}

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
procedure prthaltproc;external name '_haltproc';

procedure system_exit;
begin
  asm
    jmp prthaltproc
  end;
End;


{ OS dependant parts  }

{*****************************************************************************
                              Heap Management
*****************************************************************************}

(*var myheapstart:pointer;
    myheapsize:longint;
    myheaprealsize:longint;
    heap_handle:longint;
    zero:longint;


{ first address of heap }
function getheapstart:pointer;
begin
   getheapstart:=myheapstart;
end;

{ current length of heap }
function getheapsize:longint;
begin
   getheapsize:=myheapsize;
end;
*)


(*function getheapstart:pointer;
assembler;
asm
        leal    HEAP,%eax
end ['EAX'];


function getheapsize:longint;
assembler;
asm
        movl    intern_HEAPSIZE,%eax
end ['EAX'];*)

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
(*function Sbrk(size : longint):pointer;
var newsize,newrealsize:longint;
  s : string;
begin
  WriteLn('SBRK');
  Str(size, s);
  WriteLn('size : ' + s);
  if (myheapsize+size)<=myheaprealsize then 
  begin
    Sbrk:=pointer(heapstart+myheapsize);
    myheapsize:=myheapsize+size;
    exit;
  end;
  newsize:=myheapsize+size;
  newrealsize:=(newsize and $FFFFF000)+$1000;
  case resize_area(heap_handle,newrealsize) of
    B_OK : 
      begin
        WriteLn('B_OK');
        Sbrk:=pointer(heapstart+myheapsize);
        myheapsize:=newsize;
        myheaprealsize:=newrealsize;
        exit;
      end;
    B_BAD_VALUE : WriteLn('B_BAD_VALUE');
    B_NO_MEMORY : WriteLn('B_NO_MEMORY');
    B_ERROR : WriteLn('B_ERROR');
    else
      begin
        Sbrk:=pointer(heapstart+myheapsize);
        myheapsize:=newsize;
        myheaprealsize:=newrealsize;
        exit;
      end;
  end;

//  Sbrk:=nil;
end;*)

function sys_resize_area (handle:cardinal; size:longint):longint; cdecl; external name 'sys_resize_area';

//function sbrk2 (size : longint):pointer; cdecl; external name 'sbrk';

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
//function Sbrk(size : longint):pointer;
//var newsize,newrealsize:longint;
//  s : string;
//begin
//  sbrk := sbrk2(size);
(*  sbrk := nil;
  WriteLn('sbrk');
  Str(size, s);
  WriteLn('size : ' + s);
  if (myheapsize+size)<=myheaprealsize then 
  begin
    Sbrk:=heapstart+myheapsize;
    myheapsize:=myheapsize+size;
    exit;
  end;
  newsize:=myheapsize+size;
  newrealsize:=(newsize and $FFFFF000)+$1000;
  if sys_resize_area(heap_handle,newrealsize+$1000)=0 then 
  begin
    WriteLn('sys_resize_area OK');
    Str(longint(newrealsize), s);
    WriteLn('newrealsize : $' + Hexstr(longint(newrealsize), 8));
    Str(longint(heapstartpointer), s);
    WriteLn('heapstart : $' + Hexstr(longint(heapstart), 8));
    Str(myheapsize, s);
    WriteLn('myheapsize : ' + s);
    Str(myheapsize, s);
    WriteLn('Total : ' + s);
    WriteLn('Before fillchar');
    WriteLn('sbrk : $' + Hexstr(longint(heapstart+myheapsize), 8));        
    Sbrk:=heapstart+myheapsize;
    FillChar(sbrk^, size, #0);    
    WriteLn('EndFillChar');
    WriteLn('sbrk : $' + Hexstr(longint(sbrk), 8));
//    ReadLn(s);
    myheapsize:=newsize;
    Str({longint(heapstartpointer) +} myheapsize, s);
    WriteLn('Total : ' + s);    
    myheaprealsize:=newrealsize;
    exit;
  end
  else
  begin
    debugger('Bad resize_area');
    WriteLn('Bad resize_area');
  end;
  Sbrk:=nil;
*)
//end;

{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}


(*
{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  if handle<=2 then exit;
  InOutRes := fpclose(handle);
end;


procedure do_erase(p : pchar);
begin
  if fpunlink(p)<>0 then InOutRes:=1
  else InOutRes:=0;
end;

procedure do_rename(p1,p2 : pchar);
begin
  InOutRes := fprename(p1, p2);
end;

function do_write(h : longint; addr : pointer; len : longint) : longint;
begin
  do_write := fpwrite (h,addr,len);
  if (do_write<0) then begin
    InOutRes:=do_write;
    do_write:=0;
  end else InOutRes:=0;
end;

function do_read(h:longint; addr : Pointer; len : longint) : longint;
begin
  do_read := fpread (h,addr,len);
  if (do_read<0) then begin
    InOutRes:=do_read;
    do_read:=0;
  end else InOutRes:=0;
end;

function do_filepos(handle : longint) : longint;
begin
  do_filepos := fplseek(handle,0,1); {1=SEEK_CUR}
  if (do_filepos<0) then begin
    InOutRes:=do_filepos;
    do_filepos:=0;
  end else InOutRes:=0;
end;

procedure do_seek(handle,pos : longint);
begin
  InOutRes := fplseek(handle,pos,0);
  if InOutRes>0 then InOutRes:=0;
end;

function do_seekend(handle:longint):longint;
begin
  do_seekend := fplseek (handle,0,2); {2=SEEK_END}
  if do_seekend<0 then begin
    InOutRes:=do_seekend;
    do_seekend:=0;
  end else InOutRes:=0;
end;

function do_filesize(handle : longint) : longint;
var cur:longint;
begin
  cur := fplseek (handle,0,1); {1=SEEK_CUR}
  if cur<0 then begin
    InOutRes:=cur;
    do_filesize:=0;
    exit;
  end;
  do_filesize := fplseek (handle,0,2); {2=SEEK_END}
  if do_filesize<0 then begin
    InOutRes:=do_filesize;
    do_filesize:=0;
    exit;
  end;
  cur := fplseek (handle,cur,0); {0=SEEK_POS}
  if cur<0 then begin
    InOutRes:=cur;
    do_filesize:=0;
    exit;
  end;
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  InOutRes:=1;
end;

(*procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var m:longint;
    mode,h:longint;
    s : string;
begin
//  WriteLn('do_open; -> ');
//  WriteLn('p : ' + p);
//  Str(flags, s);
//  WriteLn('flags : ' + s);
{  printf ('OPEN %d ',longint(f));
  printf (' %s',longint(p));
  printf (' %x',flags);}

  m:=0;
  case (flags and 3) of
        0: 
          begin 
//            WriteLn('0');
            m:=m or O_RDONLY; 
            mode:=fminput;  
          end;
        1: 
          begin 
//            WriteLn('1');          
            m:=m or O_WRONLY or O_CREAT; 
            mode:=fmoutput;
          end;
        2: 
          begin 
//            WriteLn('2');          
            m:=m or O_RDWR or O_CREAT; 
            mode:=fminout; 
          end;
        3: 
          begin 
//            WriteLn('3');          
            m:=m or O_APPEND; 
            mode:=fmAppend; 
          end;
  end;

//  if (flags and $100)<>0 then m:=m or O_APPEND;
{  if (flags and $200)<>0 then 
  begin
    WriteLn('Création');
    m := m or O_CREAT;
  end;
}


//  if (flags and $1000)<>0 then m:=m {or O_TRUNC} or O_CREAT;
//  m := m or O_CREAT;

//  if (flags and $10000)<>0 then m:=m or O_TEXT else m:=m or O_BINARY;

  h := fpopen(p, m, mode);
  Str(mode, s);
//  WriteLn('mode : ' + s);

//  Str(m, s);
//  WriteLn('m : ' + s);

//  Str(h, s);
//  WriteLn('h : ' + s);
  if h<0 then InOutRes:=h
  else InOutRes:=0;

  if InOutRes=0 then begin
    FileRec(f).handle:=h;
    FileRec(f).mode:=mode;
  end;
end;*)

const
     { Default creation mode for directories and files }

     { read/write permission for everyone }
     MODE_OPEN = S_IWUSR OR S_IRUSR OR
                 S_IWGRP OR S_IRGRP OR
                 S_IWOTH OR S_IROTH;
     { read/write search permission for everyone }
     MODE_MKDIR = MODE_OPEN OR
                 S_IXUSR OR S_IXGRP OR S_IXOTH;
Procedure Do_Open(var f;p:pchar;flags:longint);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  oflags : cint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=O_RDONLY;
         FileRec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=O_WRONLY;
         FileRec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=O_RDWR;
         FileRec(f).mode:=fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
  if (FileRec(f).Handle<0) and
     (getErrNo=ESysEROFS) and
     ((OFlags and O_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(O_RDWR);
     FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
   end;
  If Filerec(f).Handle<0 Then
   InOutRes := 2
  else
   InOutRes:=0;
end;
*)
{function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=false;
  InOutRes:=0;
end;
}

{ $I text.inc}

{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}


{ $i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{ $i typefile.inc}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

Function ParamCount: Longint;
var
  s : string;
Begin
  ParamCount := 0;
  Paramcount:=argc - 1;
End;

 { variable where full path and filename and executable is stored }
 { is setup by the startup of the system unit.                    }
var
 execpathstr : shortstring;

{$ifdef FPC_USE_LIBC}

// private; use the macros, below
function _get_image_info(image : image_id; var info : image_info; size : size_t)
         : status_t; cdecl; external 'root' name '_get_image_info';

function _get_next_image_info(team : team_id; var cookie : Longint; var info : image_info; size : size_t)
         : status_t; cdecl; external 'root' name '_get_next_image_info';

function get_image_info(image : image_id; var info : image_info) : status_t;
begin
  Result := _get_image_info(image, info, SizeOf(info));
end;

function get_next_image_info(team : team_id; var cookie : Longint; var info : image_info) : status_t;
begin
  Result := _get_next_image_info(team, cookie, info, SizeOf(info));
end;

{$endif}

{ this routine sets up the paramstr(0) string at startup }
procedure setupexecname;
var
 cookie: longint;
 image : image_info;
 index : byte;
 s : string;
begin
  cookie:=0;
  fillchar(image, sizeof(image_info), 0);
  if get_next_image_info(0, cookie, image) = B_OK then
  begin
    execpathstr := strpas(@image.name);
  end
  else
    execpathstr := '';
  { problem with Be 4.5 noted... path contains . character }
  { if file is directly executed in CWD                    }
  index:=pos('/./',execpathstr);
  if index <> 0 then
    begin
      { remove the /. characters }
      Delete(execpathstr,index, 2);
    end;
end;

function paramstr(l: longint) : string;
var
  s: string;
  s1: string;
begin
   
  { stricly conforming POSIX applications  }
  { have the executing filename as argv[0] }
  if l = 0 then
  begin
    paramstr := execpathstr;
  end
  else
  begin
    paramstr := '';
    paramstr:=strpas(argv[l]);
  end;
end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;

function GetProcessID:SizeUInt;
begin
{$WARNING To be corrected by platform maintainer}
 GetProcessID := 1;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in and messagebox }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

var
  s : string;
begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := Sptr - StackLength;
  SysInitStdIO;
{ Setup heap }
  myheapsize:=4096*1;// $ 20000;
  myheaprealsize:=4096*1;// $ 20000;
  heapstart:=nil;
  heapstartpointer := nil;
  heapstartpointer := Sbrk2(4096*1);
{$IFDEF FPC_USE_LIBC}  
//  heap_handle := create_area('fpcheap',heapstart,0,myheaprealsize,0,3);//!!
{$ELSE}
//  debugger('tata'#0);
//  heap_handle := create_area('fpcheap',longint(heapstartpointer),0,myheaprealsize,0,3);//!!
//  case heap_handle of
//    B_BAD_VALUE : WriteLn('B_BAD_VALUE');
//    B_PAGE_SIZE : WriteLn('B_PAGE_SIZE');
//    B_NO_MEMORY : WriteLn('B_NO_MEMORY');
//    B_ERROR : WriteLn('B_ERROR');
//  end;

  FillChar(heapstartpointer^, myheaprealsize, #0);
//  WriteLn('EndFillChar');
//    WriteLn('P : $' + Hexstr(longint(heapstartpointer), 8));        
//    WriteLn('heapstart : $' + Hexstr(longint(heapstartpointer^), 8));        
  heapstart := heapstartpointer;
{$ENDIF}
//  WriteLn('before InitHeap');
//  case heap_handle of
//    B_BAD_VALUE : WriteLn('B_BAD_VALUE');
//    B_PAGE_SIZE : WriteLn('B_PAGE_SIZE');
//    B_NO_MEMORY : WriteLn('B_NO_MEMORY');
//    B_ERROR : WriteLn('B_ERROR');
//  else
//    begin
//      WriteLn('ok');  
//      WriteLn('P : $' + Hexstr(longint(heapstartpointer), 8));        
//      WriteLn('heapstart : $' + Hexstr(longint(heapstartpointer^), 8));       
//      if heap_handle>0 then 
//      begin
        InitHeap;
//      end;
//    end;
//  end;
//  WriteLn('after InitHeap');
//  end else system_exit;
  SysInitExceptions;
//  WriteLn('after SysInitException');

{ Setup IO }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
  InitSystemThreads;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
  setupexecname;
end.
