
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';
function fpNice(N:cint):cint;
Function fpGetPriority(Which,Who:cint):cint;
Function fpSetPriority(Which,Who,What:cint):cint;
Function fpLstat(path:pchar;Info:pstat):cint;
Function fpLstat(Filename: PathStr;Info:pstat):cint;
Function fpSymlink(oldname,newname:pchar):cint;
Function fpReadLink(name,linkname:pchar;maxlen:size_t):cint;

