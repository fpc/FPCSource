
function fpNice(N:cint):cint;
Function fpGetPriority(Which,Who:cint):cint;
Function fpSetPriority(Which,Who,What:cint):cint;
Function fpLstat(path:pchar;Info:pstat):cint;
Function fpLstat(Filename: PathStr;Info:pstat):cint;
Function fpSymlink(oldname,newname:pchar):cint;
Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;
Function fpReadLink(name:PathStr):PathStr;
Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; external name  'FPC_SYSC_MMAP';
Function Fpmunmap(start:pointer;len:size_t):cint;  external name 'FPC_SYSC_MUNMAP';
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

