unit uncgi;
{
  $Id$

  UNCGI UNIT 2.0.11
  ----------------
}
{$ASSERTIONS ON}
interface
uses
  strings
{$IFDEF OS2}
  ,DosCalls
{$ENDIF OS2}
  ;

{***********************************************************************}

const
  maxquery      = 100;
  uncgi_version = 'UNCGI 2.1.1';
  uncgi_year    = '1999';
  maintainer_name = 'Your Name Here';
  maintainer_email= 'your@email.address.here';

Type
  cgi_error_proc = procedure (Const Proc,Err : String);
  PCgiVar=^TCgiVar;
  TCgiVar=Record
    Name:PChar;
    NbrValues:LongInt;
    Value:PPChar
  end;
var
  cgiEnvC:LongInt;
  cgiEnvP:PCgiVar;
  get_nodata    : boolean;
  query_read    : Cardinal;
  uncgi_error   : cgi_error_proc;

{***********************************************************************}

{ FUNCTION

  This function returns the REQUEST METHOD of the CGI-BIN script

  Input         - Nothing
  Output        - [GET|POST]
}
function http_request_method: pchar;

{ FUNCTION

  This function returns the "referring" page. i.e. the page you followed
  the link to this CGI-BIN from

  Input         - Nothing
  Output        - [http://somewhere.a.tld]
}
function http_referer: pchar;

{ FUNCTION

  This function returns the IP address of the client.

  Input         - Nothing
  Output        - an internet IP address.
}
function http_remote: pchar;

{ FUNCTION

  This function returns the users's USER AGENT, the browser name etc.

  Input         - Nothing
  Output        - user agent string
}
function http_useragent: pchar;

{ FUNCTION

  This function returns a value from an id=value pair

  Input         - The identifier you want the value from
  Output        - If the identifier was found, the resulting value is
                  the output, otherwise the output is NIL
}


function get_value(id: pchar): pchar;

{ FUNCTION

  This function extracts array of values for the
  same variable (used in some checkbox forms)

  Use like in this example:

  v:=get_value('some_id');
  while v<>'' do begin
    Do_something_with(v);
    v:=get_next_value;
  end;
}
function get_next_value:PChar;

{ PROCEDURE

  This procedure writes the content-type to the screen

  Input         - The content type in MIME format
  Output        - Nothing

  Example       - set_content('text/plain');
                  set_content('text/html');
}
procedure set_content(const ctype: string);

{ Function to get the requested URL }

function http_url: pchar;



procedure cgi_init;
procedure cgi_deinit;

implementation

{$IFDEF OS2}
function GetEnv (EnvVar: string): PChar;

var P: PChar;

begin
 EnvVar := EnvVar + #0;
 if DosScanEnv (@EnvVar [1], P) = 0 then GetEnv := P else GetEnv := nil;
end;
{$ENDIF OS2}

{$ifdef win32}
Var EnvP : PChar;
    EnvLen : Longint;
    OldExitProc : Pointer;

function GetEnvironmentStrings : pchar; external 'kernel32' name 'GetEnvironmentStringsA';
function FreeEnvironmentStrings(p : pchar) : longbool; external 'kernel32' name 'FreeEnvironmentStringsA';

Procedure FInitWin32CGI;
begin
  { Free memory }
  FreeMem (EnvP,EnvLen);
  ExitProc:=OldExitProc;
end;

Procedure InitWin32CGI;
var s : String;
    i,len : longint;
    hp,p : pchar;

begin
  { Make a local copy of environment}
  p:=GetEnvironmentStrings;
  hp:=p;
  envp:=Nil;
  envlen:=0;
  while hp[0]<>#0 do
    begin
    len:=strlen(hp);
    hp:=hp+len+1;
    EnvLen:=Envlen+len+1;
    end;
  GetMem(EnvP,Envlen);
  Move(P^,EnvP^,EnvLen);
  FreeEnvironmentStrings(p);
  OldExitProc:=ExitProc;
  ExitProc:=@FinitWin32CGI;
end;

Function GetEnv(envvar: string): pchar;
{ Getenv that can return environment vars of length>255 }
var s : String;
    i,len : longint;
    hp : pchar;

begin
  s:=Envvar+#0;
  getenv:=Nil;
  hp:=envp;
  while hp[0]<>#0 do
    begin
    len:=strlen(hp);
    i:=Longint(strscan(hp,'='))-longint(hp);
    if StrLIComp(@s[1],HP,i-1)=0 then
       begin
       Len:=Len-i;
       getmem (getenv,len);
       Move(HP[I+1],getenv^,len+1);
       break;
       end;
    { next string entry}
    hp:=hp+len+1;
    end;
end;
{$endif}

{$ifdef GO32V2}
Function  GetEnv(envvar: string): pchar;
var
  hp    : ppchar;
  hs    : string;
  eqpos : longint;
begin
  envvar:=upcase(envvar);
  hp:=envp;
  getenv:=nil;
  while assigned(hp^) do
   begin
     hs:=strpas(hp^);
     eqpos:=pos('=',hs);
     if copy(hs,1,eqpos-1)=envvar then
      begin
        getenv:=hp^+eqpos;
        exit;
      end;
     inc(hp);
   end;
end;
{$endif}

{$ifdef unix}
Function GetEnv(P:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  found : boolean;
Begin
  p:=p+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        if strlcomp(@p[1],(ep^),length(p))=0 then
         found:=true
        else
         inc(ep);
      end;
   end;
  if found then
   getenv:=ep^+length(p)
  else
   getenv:=nil;
end;
{$endif unix}


var
  done_init     : boolean;

procedure set_content(const ctype: string);
begin
  writeln('Content-Type: ',ctype);
  writeln;
end;

function http_request_method: pchar;
begin
  http_request_method :=getenv('REQUEST_METHOD');
end;

function http_referer: pchar;
begin
  http_referer :=getenv('HTTP_REFERER');
end;

function http_useragent: pchar;
begin
  http_useragent :=getenv('HTTP_USER_AGENT');
end;

function hexconv(h1,h2: char): char;
  function h2c(c:char):byte;
    begin
      case c of
        '0'..'9':h2c := ord(c) - ord('0');
        'A'..'F':h2c := 10 + (ord(UpCase(c)) - ord('A'));
      end;
    end;
begin
  HexConv:=Chr(h2c(h1)*16+h2c(h2));
end;

procedure def_uncgi_error(const pname,perr: string);
begin
  set_content('text/html');
  writeln('<html><head><title>UNCGI ERROR</title></head>');
  writeln('<body>');
  writeln('<center><hr><h1>UNCGI ERROR</h1><hr></center><br><br>');
  writeln('UnCgi encountered the following error: <br>');
  writeln('<ul><br>');
  writeln('<li> procedure: ',pname,'<br>');
  writeln('<li> error: ',perr,'<br><hr>');
  writeln(
   '<h5><p><i>uncgi (c) ',uncgi_year,' ',maintainer_name,
{ skelet fix }
   '<a href="mailto:',maintainer_email,'">',
   maintainer_email,'</a></i></p></h5>');
  writeln('</body></html>');
  halt;
end;

var
  gv_cnt,gv_cnt_n:LongInt;
function get_next_value:PChar;
begin
  Assert(done_init,'Please call cgi_init() first');
  if gv_cnt>=cgiEnvC
  then
    Exit(Nil);
  with cgiEnvP[gv_cnt] do
    begin
      if gv_cnt_n>=NbrValues
      then
        Exit(Nil);
      get_next_value:=Value[gv_cnt_n];
    end;
    Inc(gv_cnt_n);
end;

function get_value(id: pchar): pchar;
 begin
  Assert(done_init,'Please call cgi_init() first');
  gv_cnt:=0;
  gv_cnt_n:=0;
  while(gv_cnt<cgiEnvC)and(StrComp(id,cgiEnvP[gv_cnt].Name)<>0)do
    Inc(gv_cnt);
  get_value:=get_next_value;
 end;

Function UnEscape(QueryString: PChar): PChar;
var
   qunescaped    : pchar;
   sptr          : longint;
   cnt           : word;
   qslen         : longint;
begin
   qslen:=strlen(QueryString);
   if qslen=0 then
     begin
     Unescape:=Nil;
     get_nodata:=true;
     exit;
     end
   else
     get_nodata :=false;
{ skelet fix }
{Escaped chain is usually longer than the unescaped chain}
   GetMem(qunescaped,qslen+1);
   if qunescaped=nil
   then
    uncgi_error('UnEscape()','Could not allocate memory');
   sptr :=0;

{  for cnt := 0 to qslen do  +++++ use while instead of for }
   cnt:=0;
   while cnt<qslen do
   begin
     case querystring[cnt] of
       '+': qunescaped[sptr]:=' ';
       '%': begin
            qunescaped[sptr]:=hexconv(querystring[cnt+1], querystring[cnt+2]);
            inc(cnt,2); { <--- not allowed in for loops in pascal }
            end;
     else
       qunescaped[sptr] := querystring[cnt];
     end;
     inc(sptr);
{ skelet fix }
     qunescaped[sptr]:=#0;
     inc(cnt);               { <-- don't forget to increment }
   end;
   UnEscape:=StrNew(qunescaped);
   FreeMem(qunescaped,qsLen+1);
end;

Function Chop(QueryString:PChar):Cardinal;
  var
    VarName,VarValue,name_pos,value_pos:PChar;
    sz,EnvCC:LongInt;
    p:Pointer;
  begin
    GetMem(cgiEnvP,MaxQuery*SizeOf(TCgiVar));
    name_pos:=QueryString;
    value_pos:=QueryString;
    repeat
      value_pos:=StrScan(name_pos,'=');
      if value_pos=Nil
      then
        value_pos:=StrEnd(name_pos)
      else
        Inc(value_pos);
      sz:=value_pos-name_pos-1;
      VarName:=StrAlloc(sz+1);
      StrLCopy(VarName,name_pos,sz);
      name_pos:=StrScan(name_pos,'&');
      if name_pos=Nil
      then
        sz:=StrLen(value_pos)
      else
        begin
          Inc(name_pos);
          sz:=name_pos-value_pos-1;
        end;
      VarValue:=StrAlloc(sz+1);
      StrLCopy(VarValue,value_pos,sz);
      EnvCC:=0;
      repeat
        with cgiEnvP[EnvCC] do
          begin
            if EnvCC=cgiEnvC
            then
              begin
                if cgiEnvC>=MaxQuery
                then
                  uncgi_error('cgi_read_get_query()','Your are trying to use more than max varaibles allowed! Please change value of "MaxQuery" and recompile your program')
                else
                  begin
                    Name:=UnEscape(VarName);
                    GetMem(Value,MaxQuery*SizeOf(PChar));
                    NbrValues:=0;
                    Inc(cgiEnvC);
                  end;
              end;
            if StrComp(VarName,Name)=0
            then
              begin
                if NbrValues>=MaxQuery
                then
                  uncgi_error('cgi_read_get_query()','Your are trying to use more than max values allowed for a given variable! Please change value of "MaxQuery" and recompile your program')
                else
                  begin
                    Value[NbrValues]:=UnEscape(VarValue);
                    Inc(NbrValues);
                  end;
                StrDispose(VarName);
                StrDispose(VarValue);
                break;
              end;
          end;
        Inc(EnvCC);
      until false;
    until name_pos=Nil;
    for EnvCC:=0 to cgiEnvC-1 do
      with cgiEnvP[EnvCC] do
        begin
          p:=Value;
          sz:=NbrValues*SizeOf(PChar);
          GetMem(Value,sz);
          Move(p^,Value^,sz);
          FreeMem(p,MaxQuery*SizeOf(PChar));
        end;
    p:=cgiEnvP;
    sz:=cgiEnvC*SizeOf(TCgiVar);
    GetMem(cgiEnvP,sz);
    Move(p^,cgiEnvP^,sz);
    FreeMem(p,MaxQuery*SizeOf(TCgiVar));
    Chop:=Abs(cgiEnvC);
  end;

procedure cgi_read_get_query;
var
  querystring   : pchar;
  qslen         : longint;
begin
  querystring :=strnew(getenv('QUERY_STRING'));
  if querystring<>NIL
  then
    begin
      qslen :=strlen(querystring);
      if qslen=0
      then
        begin
          get_nodata :=true;
          exit;
        end
      else
        get_nodata :=false;
      query_read:=Chop(QueryString);
    end;
  StrDispose(QueryString);
end;

procedure cgi_read_post_query;
var
  querystring   : pchar;
  qslen         : longint;
  sptr          : longint;
  clen          : string;
  ch            : char;

begin
  if getenv('CONTENT_LENGTH')<>Nil then
    begin
    clen:=strpas (getenv('CONTENT_LENGTH'));
    val(clen,qslen);
    if (upcase(strpas(getenv('CONTENT_TYPE')))='APPLICATION/X-WWW-FORM-URLENCODED')
       or (upcase(strpas(getenv('CONTENT_TYPE')))='TEXT/PLAIN')
      then
      begin
      getmem(querystring,qslen+1);
      sptr :=0;
      while sptr<>qslen do
        begin
        read(ch);
        QueryString[sptr]:=ch;
        inc(sptr);
        end;
      { !!! force null-termination }
      QueryString[sptr]:=#0;
      query_read:=Chop(QueryString);
      end;
    end;
end;

procedure cgi_init;
var
  rmeth : pchar;
begin
  Assert(NOT done_init,'cgi_init() was already called');
  query_read:=0;
  rmeth :=http_request_method;
  if rmeth=nil then
    begin
    uncgi_error('cgi_init()','No REQUEST_METHOD passed from server!');
    exit;
    end;
  if strcomp('POST',rmeth)=0 then cgi_read_post_query else
  if strcomp('GET',rmeth)=0 then cgi_read_get_query else
  uncgi_error('cgi_init()','No REQUEST_METHOD passed from server!');
  done_init :=true;
end;

procedure cgi_deinit;
var
  i,j:LongInt;
begin
  Assert(done_init,'Please call cgi_init() first');
  if cgiEnvC=0
  then
    Exit;
  for i:=0 to cgiEnvC-1 do
    with cgiEnvP[i] do
      begin
        StrDispose(Name);
        for j:=0 to NbrValues-1 do
          StrDispose(Value[j]);
        FreeMem(Value,NbrValues*SizeOf(PChar));
      end;
  FreeMem(cgiEnvP,cgiEnvC*SizeOf(TCgiVar));
  cgiEnvC:=0;
end;


Function http_url: pchar;
begin
  http_url:=getenv('REQUEST_URI');
end;

function http_remote: pchar;
begin
  http_remote :=getenv('REMOTE_ADDR');
end;

begin
  {$ifdef win32}
  InitWin32CGI;
  {$endif}
  uncgi_error:=@def_uncgi_error;
  done_init :=false;
end.


{
  HISTORY
  $Log$
  Revision 1.13  2003-09-27 12:07:31  peter
    * unix getenv added

  Revision 1.12  2003/07/16 12:56:03  mazen
  + using Assert to monitor done_init and get state of
    un_cgi initailization
  * renaming EnvP and EnvC to cgiEnvP and cgiEnvP
    to avoid confusion with regular EnvP and EnvC
    varaibles especially under win32 target
  * set_contents get parameter by address (const)

  Revision 1.11  2003/05/29 08:58:45  michael
  + Fixed inline error when building

  Revision 1.10  2003/05/27 20:50:18  mazen
  * New implemtation of HexConv
  * New implementation of Chop to fix an incompatibilty
    bug with SysUtils.
  * Replacing quary_array (static) by EnvP(dynamic)

  Revision 1.9  2002/10/24 17:25:36  sg
  * Fixed parsing of empty URL arguments (with missing "=")

  Revision 1.8  2002/10/18 05:43:53  michael
  + Fix of invalid pointer bug in unescape, from U. Maeder

  Revision 1.7  2002/10/10 05:48:20  michael
  Added http_remote and fixed determining of input method. Fix courtesy of Antal <antal@carmelcomputer.com>

  Revision 1.6  2002/09/12 16:24:59  michael
  + Added http_url function from Michael Weinert

  Revision 1.5  2002/09/07 15:43:06  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/05/31 11:54:33  marco
  * Renamefest for 1.0, many 1.1.x spots patched also.

  Revision 1.3  2002/03/04 17:57:17  peter
    * updated example in comment

  Revision 1.2  2002/03/01 10:57:03  peter

    * get_next_value patch from Skelet

  Revision 1.1  2002/01/29 17:55:23  peter
    * splitted to base and extra

}
