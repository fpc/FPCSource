program FreePasResourcePreprocessor;
{$ifdef win32}
{$APPTYPE CONSOLE}
{$endif}
{$ifndef fpc}{$N+}{$endif}
uses
 Comments,PasPrep,Expr
{$ifndef win32}
,DOS;
type
 str255=string[255];
{$else}
;
type
 str255=string[255];
function SearchPath(path,name,ext:pchar;size:longint;buf:pchar;var x:pointer):longint;stdcall;
 external 'kernel32.dll' name 'SearchPathA';
function FSearch(s,path:str255):Str255;
 var
  l:longint;
 procedure zeroterm(var s:str255);
  begin
   l:=length(s);
   move(s[1],s[0],l);
   s[l]:=#0;
  end;
 var
  buf:str255;
  aPtr:pointer;
  i:longint;
 begin
  zeroterm(path);
  zeroterm(s);
  i:=SearchPath(pchar(@path),pchar(@s),nil,255,pchar(@buf[1]),aPtr);
  if i<=255 then
   byte(buf[0]):=i
  else
   buf[0]:=#0;
  FSearch:=buf;
 end;
{$endif}

type
 pstring=^str255;
 PReplaceRec=^TReplaceRec;
 TReplaceRec=record
  next:PReplaceRec;
  CaseSentitive:longbool;
  oldvalue,newvalue:pstring;
 end;
 chars=array[1..2]of char;
 pchars=^chars;
const
 Chain:PReplaceRec=nil;
 ChainHdr:PReplaceRec=nil;
 Chainlen:longint=0;
var
 f:file;
 s:str255;
 size,nextpos:longint;
 buf:pchars;
 i:longint;
function Entry(buf:pchars;Size,fromPos:longint;const sample:str255;casesent:longbool):longbool;
 var
  i:longint;
  c:char;
 begin
  Entry:=false;
  if(fromPos>1)and(buf^[pred(frompos)]>#32)then
   exit;
  if fromPos+length(sample)-1>=size then
   exit;
  if buf^[fromPos+length(sample)]>#32 then
   exit;
  Entry:=true;
  for i:=1 to length(sample)do
   begin
    if pred(fromPos+i)>size then
     begin
      Entry:=false;
      exit;
     end;
    c:=buf^[pred(fromPos+i)];
    if not casesent then
     c:=UpCase(c);
    if c<>sample[i]then
     begin
      Entry:=false;
      exit;
     end;
    end;
 end;
function GetWord(buf:pchars;Size,fromPos:longint;var EndPos:longint):str255;
 var
  s:str255;
  i:longint;
  word_begin:longbool;
 begin
  s:='';
  i:=frompos;
  word_begin:=false;
  while i<size do
   begin
    if not word_begin then
     word_begin:=(buf^[i]>#32)and(buf^[i]<>';')and(buf^[i]<>'=');
    if word_begin then
     begin
      if not(buf^[i]in[#0..#32,';','='])then
       s:=s+buf^[i]
      else
       begin
        EndPos:=i;
        break;
       end;
     end;
    inc(i);
   end;
  GetWord:=s;
 end;
procedure excludeComments(buf:pchars;size:longint);
 var
  comment:longbool;
  i:longint;
 begin
  comment:=false;
  for i:=1 to pred(size)do
   begin
    if(buf^[i]='/')and(buf^[succ(i)]='*')then
     comment:=true;
    if comment then
     begin
      if(buf^[i]='*')and(buf^[succ(i)]='/')then
       begin
        comment:=false;
        buf^[succ(i)]:=' ';
       end;
      buf^[i]:=' ';
     end;
   end;
  comment:=false;
  for i:=1 to pred(size)do
   begin
    if(buf^[i]='/')and(buf^[succ(i)]='/')then
     comment:=true;
    if comment then
     begin
      if buf^[i]in[#10,#13]then
       comment:=false;
      buf^[i]:=' ';
     end;
   end;
 end;
function IsSwitch(const switch:str255):longbool;
 var
  i:longint;
 begin
  IsSwitch:=false;
  for i:=1 to ParamCount do
   if paramstr(i)='-'+switch then
    begin
     IsSwitch:=true;
     exit;
    end;
 end;
function GetSwitch(const switch:str255):str255;
 var
  i:longint;
 begin
  GetSwitch:='';
  for i:=1 to paramcount do
   if paramstr(i)='-'+switch then
    GetSwitch:=paramstr(succ(i));
 end;
procedure saveproc(const key,value:str255;CaseSent:longbool);{$ifndef fpc}far;{$endif}
 var
  c:pReplaceRec;
 begin
  new(c);
  c^.next:=nil;
  c^.CaseSentitive:=CaseSent;
  getmem(c^.oldvalue,succ(length(key)));
  c^.oldvalue^:=key;
  getmem(c^.newvalue,succ(length(value)));
  c^.newvalue^:=value;
  if chainhdr=nil then
   begin
    chain:=c;
    chainhdr:=chain;
    ChainLen:=1;
   end
  else
   begin
    chain^.next:=c;
    chain:=c;
    inc(ChainLen);
   end;
 end;
type
 Tlanguage=(L_C,L_Pascal);
function Language(s:str255):tLanguage;
 var
  s1,Lstr:str255;
  i,j:longint;
  found:longbool;
 type
  TLD=record
   x:string[3];
   l:tLanguage;
  end;
 const
  default:array[1..7]of TLD=(
   (x:'PAS';l:L_PASCAL),
   (x:'PP';l:L_PASCAL),
   (x:'P';l:L_PASCAL),
   (x:'DPR';l:L_PASCAL),
   (x:'IN?';l:L_PASCAL),
   (x:'C';l:L_C),
   (x:'H';l:L_C));
 begin
  Lstr:=GetSwitch('l');
  if lstr=''then
   Lstr:=GetSwitch('-language');
  for i:=1 to length(Lstr)do
   Lstr[i]:=UpCase(Lstr[i]);
  if Lstr='C'then
   begin
    Language:=L_C;
    exit;
   end
  else if(Lstr='PASCAL')or(Lstr='DELPHI')then
   begin
    Language:=L_PASCAL;
    exit;
   end
  else if (Lstr<>'')then
   writeln('Warning: unknown language ',Lstr);
  s1:='';
  for i:=length(s)downto 1 do
   begin
    if s[i]='.'then
     break;
    s1:=upcase(s[i])+s1;
   end;
  for i:=1 to 7 do
   begin
    found:=true;
    for j:=1 to length(s1)do
     if s1[j]<>default[i].x[j]then
      case default[i].x[j] of
       '?':
        ;
       else
        found:=false;
      end;
     if(found)and(s1<>'')then
      begin
       Language:=default[i].l;
       exit;
      end;
    end;
  Language:=L_PASCAL;
 end;
function Up(const s:str255):str255;
 var
  n:str255;
  i:longint;
 begin
  n:=s;
  for i:=1 to length(s)do
   n[i]:=upcase(s[i]);
  Up:=n;
 end;
procedure do_C(buf:pchars;size:longint;proc:pointer);
 type
  Tpushfunc=procedure(const key,value:str255;CaseSent:longBool);
 var
  position:longint;
  charconst,stringconst:longbool;
  s,s0:str255;
  afunc:Tpushfunc absolute proc;
 procedure read(var s:str255;toEOL:longbool);
  var
   i:longint absolute position;
  function EndOfWord:longbool;
   begin
    if toEOL then
     EndOfWord:=buf^[i]in[#10,#13]
    else
     EndOfWord:=buf^[i]<=#32;
   end;
  begin
   s:='';
   if i>size then
    exit;
   while buf^[i]<=#32 do
    begin
     if i>size then
      exit;
     inc(i);
    end;
   repeat
    if i>size then
     exit;
    if not stringConst then
     if buf^[i]=''''then
      charconst:=not charconst;
    if not charConst then
     if buf^[i]='"'then
      stringconst:=not stringconst;
    if(not charconst)and(not stringconst)and EndOfWord then
     exit;
    if buf^[i]>#32 then
     s:=s+buf^[i];
    inc(i);
   until false;
  end;
 begin
  ExcludeComments(buf,size);
  position:=1;
  charconst:=false;
  stringconst:=false;
  repeat
   read(s,false);
   if Up(s)='#DEFINE' then
    begin
     read(s,false);
     read(s0,true);
     Tpushfunc(afunc)(s,s0,true);
    end;
  until position>=size;
 end;
procedure expandname(var s:str255;path:str255);
 var
  astr:str255;
 begin
  astr:=fsearch(s,path);
  if astr<>''then
   s:={$ifndef Win32}FExpand{$endif}(astr);
 end;
function do_include(name:str255):longbool;
 var
  buf:pchars;
  f:file;
  size:longint;
  s1:str255;
 procedure trim;
  begin
   delete(name,1,1);
   dec(name[0]);
  end;
 begin
  if (name[1]='"')and(name[length(name)]='"')then
   trim
  else if (name[1]='<')and(name[length(name)]='>')then
   begin
    trim;
    s1:=GetSwitch('p');
    if s1=''then
     s1:=GetSwitch('-path');
    expandname(name,s1);
   end;
  assign(f,name);
  reset(f,1);
  size:=filesize(f);
  GetMem(buf,size);
  blockread(f,buf^,size);
  close(f);
  case Language(name)of
   L_C:
    do_C(buf,size,@saveProc);
   L_PASCAL:
    do_pascal(buf,size,@saveProc);
  end;
  FreeMem(buf,size);
  do_include:=true;
 end;
function CheckRight(const s:str255;pos:longint):longbool;
 begin
  CheckRight:=true;
  if pos>length(s)then
   CheckRight:=false
  else
   CheckRight:=not(s[succ(pos)]in['a'..'z','A'..'Z','0'..'9','_']);
 end;
function CheckLeft(const s:str255;pos:longint):longbool;
 begin
  CheckLeft:=true;
  if pos>1 then
   begin
    if pos>length(s)then
     CheckLeft:=false
    else
     CheckLeft:=not(s[pred(pos)]in['a'..'z','A'..'Z','0'..'9','_']);
   end;
 end;
function Evaluate(Equation:Str255):Str255;
 var
  x:double;
  Err:integer;
 begin
  Eval(Equation,x,Err);
  if(Err=0)and(frac(x)=0)then
   str(x:1:0,Equation)
  else
   Equation:='';
  Evaluate:=Equation;
 end;
type
 taccel=array[1..100]of pReplaceRec;
var
 accel:^taccel;
 c:pReplaceRec;
 j,kk:longint;
 sss,sst:str255;
 MustBeReplaced:longbool;
begin
 if(paramcount=0)or isSwitch('h')or isSwitch('-help')or((paramcount>1)and(GetSwitch('i')=''))then
  begin
   writeln('FPC CONSTANTS EXTRACTOR for resource scripts preprocessing');
   writeln('version 0.01');
   writeln('Usage: fprcp <file_name>');
   writeln('or:');
   writeln('fprcp -i <file_name> [-n] [-C] [-l PASCAL|C] [-p <include_path>]');
   writeln('      -C type C header instead preprocessed resource script');
   writeln('      -l set programming language for include files');
   writeln('      -p set path to include files');
   writeln('      -n disable support of pascal comments nesting');
   halt;
  end;
 if ParamCount=1 then
  assign(f,paramstr(1))
 else
  assign(f,GetSwitch('i'));
 reset(f,1);
 size:=filesize(f);
 getmem(buf,size);
 blockread(f,buf^,size);
 close(f);
 if isSwitch('n')then
  PasNesting:=false;
 if isSwitch('-disable-nested-pascal-comments')then
  PasNesting:=false;
 excludeComments(buf,size);
 for i:=1 to size do
  begin
   if entry(buf,size,i,'#include',true)then
    do_include(GetWord(buf,size,i+length('#include'),nextpos));
  end;

 getmem(Accel,sizeof(pReplaceRec)*ChainLen);
 c:=ChainHdr;
 i:=0;
 while c<>nil do
  begin
   inc(i);
   Accel^[i]:=c;
   c:=c^.next;
  end;
 for i:=1 to pred(Chainlen)do
  for j:=succ(i)to Chainlen do
   if length(Accel^[j]^.newvalue^)>=length(Accel^[i]^.oldvalue^)then
    repeat
     MustBeReplaced:=false;
     for kk:=1 to length(Accel^[j]^.newvalue^)do
      begin
       sss:=copy(Accel^[j]^.newvalue^,kk,length(Accel^[i]^.oldvalue^));
       if length(sss)<>length(Accel^[i]^.oldvalue^)then
        break
       else if sss=Accel^[i]^.oldvalue^ then
        begin
         MustBeReplaced:=(CheckLeft(Accel^[j]^.newvalue^,kk)and CheckRight(Accel^[j]^.newvalue^,kk-1+
                             length(Accel^[i]^.oldvalue^)));
         if MustBeReplaced then
          break;
        end;
      end;
     if MustBeReplaced then
      begin
       sss:=Accel^[j]^.newvalue^;
       delete(sss,kk,length(Accel^[i]^.oldvalue^));
       insert(Accel^[i]^.newvalue^,sss,kk);
       freemem(Accel^[j]^.newvalue,length(Accel^[j]^.newvalue^));
       getmem(Accel^[j]^.newvalue,length(sss));
       Accel^[j]^.newvalue^:=sss;
      end;
    until not MustBeReplaced;
 for j:=1 to Chainlen do
  begin
   sss:=Evaluate(Accel^[j]^.newvalue^);
   freemem(Accel^[j]^.newvalue,length(Accel^[j]^.newvalue^));
   getmem(Accel^[j]^.newvalue,length(sss));
   Accel^[j]^.newvalue^:=sss;
  end;
 if isSwitch('C')or isSwitch('-Cheader')then
  for i:=1 to Chainlen do
   begin
    if Accel^[i]^.newvalue^<>''then
     writeln('#define ',Accel^[i]^.oldvalue^,' ',Accel^[i]^.newvalue^)
   end
 else
  begin
   sss:='';
   i:=1;
   sss:='';
   while i<=size do
    begin
     if buf^[i]<>#10 then
      sss:=sss+buf^[i]
     else
      begin
       while(sss<>'')and(sss[1]<=#32)do
        delete(sss,1,1);
       sst:=sss;
       for j:=1 to length(sst)do
        sst[j]:=upcase(sst[j]);
       if pos('#INCLUDE',sst)=0 then
        begin
         s:='';
         for kk:=1 to length(sss)do
          begin
           if sss[kk]>#32 then
            s:=s+sss[kk]
           else if s<>'' then
            begin
             for j:=1 to ChainLen do
              begin
               if accel^[j]^.casesentitive then
                begin
                 if(accel^[j]^.oldvalue^=s)and(accel^[j]^.newvalue^<>'')then
                  begin
                   s:=accel^[j]^.newvalue^;
                   break;
                  end;
                end
               else
                begin
                 if(accel^[j]^.oldvalue^=Up(s))and(accel^[j]^.newvalue^<>'')then
                  begin
                   s:=accel^[j]^.newvalue^;
                   break;
                  end;
                end;
              end;
             write(s,' ');
             s:='';
            end;
          end;
         writeln;
         sss:='';
        end
       else
        sss:='';
      end;
     inc(i);
    end;
  end;
 freemem(Accel,sizeof(pReplaceRec)*ChainLen);
 Chain:=ChainHdr;
 while Chain<>nil do
  begin
   c:=Chain;
   Chain:=Chain^.next;
   if c^.oldvalue<>nil then
    freemem(c^.oldvalue,succ(length(c^.oldvalue^)));
   if c^.newvalue<>nil then
    freemem(c^.newvalue,succ(length(c^.newvalue^)));
   dispose(c);
  end;
 freemem(buf,size);
end.
