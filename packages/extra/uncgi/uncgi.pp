unit uncgi;
{
  $Id$

  UNCGI UNIT 2.0.11
  ----------------
}

interface
uses
  strings
 {$ifdef Unix}
  {$ifdef ver1_0}
    ,Linux
  {$else}
    ,Unix
  {$endif}
 {$endif}
{$IFDEF OS2}
  ,DosCalls
{$ENDIF OS2}
  ;

{***********************************************************************}

const
  maxquery      = 100;
  hextable      : array[0..15] of char=('0','1','2','3','4','5','6','7',
                                        '8','9','A','B','C','D','E','F');
  uncgi_version = 'UNCGI 2.0.11';
  uncgi_year    = '1999';
  maintainer_name = 'Your Name Here';
  maintainer_email= 'your@email.address.here';

Type cgi_error_proc = procedure (Const Proc,Err : String);

var
  get_nodata    : boolean;
  query_read    : word;
  query_array   : array[1..2,1..maxquery] of pchar;
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
    v:=get_next_value('some_id');
  end;
}
function get_next_value(id: pchar): pchar;

{ PROCEDURE

  This procedure writes the content-type to the screen

  Input         - The content type in MIME format
  Output        - Nothing

  Example       - set_content('text/plain');
                  set_content('text/html');
}
procedure set_content(ctype: string);

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


var
  done_init     : boolean;

procedure set_content(ctype: string);
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
var
  cnt   : byte;
  thex  : byte;
begin
  for cnt :=0 to 15 do if upcase(h1)=hextable[cnt] then thex := cnt * 16;
  for cnt :=0 to 15 do if upcase(h2)=hextable[cnt] then thex := thex + cnt;
  hexconv := chr(thex);
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

var gv_cnt:word;

function get_next_value(id: pchar): pchar;
var
  cnt   : word;
begin
  if gv_cnt<=query_read then inc(gv_cnt);
  get_next_value:=Nil;
  if done_init then
    for cnt :=gv_cnt to query_read do
      if strcomp(strupper(id),strupper(query_array[1,cnt]))=0 then begin
        get_next_value := query_array[2,cnt];
        gv_cnt:=cnt;
        exit;
      end;
end;

function get_value(id: pchar): pchar;
 begin
  gv_cnt:=0;
  get_value:=get_next_value(id)
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
    Unescape:=#0;
    get_nodata:=true;
    exit;
    end
  else
    get_nodata :=false;
{ skelet fix }
  getmem(qunescaped,qslen+1);
  if qunescaped=nil then
    begin
    writeln ('Oh-oh');
    halt;
    end;
  sptr :=0;
  for cnt := 0 to qslen do
    begin
    case querystring[cnt] of
      '+': qunescaped[sptr] := ' ';
      '%': begin
           qunescaped[sptr] :=
               hexconv(querystring[cnt+1], querystring[cnt+2]);
           inc(cnt,2);
           end;
    else
      qunescaped[sptr] := querystring[cnt];
    end;
    inc(sptr);
{ skelet fix }
    qunescaped[sptr]:=#0;
    end;
  UnEscape:=qunescaped;
end;

Function Chop(QunEscaped : PChar) : Longint;
var
  qptr          : word;
  cnt           : word;
  qslen : longint;

begin
  qptr := 1;
  qslen:=strlen(QUnescaped);
  query_array[1,qptr] := qunescaped;
  for cnt := 0 to qslen-1 do
    case qunescaped[cnt] of
      '=': begin
             qunescaped[cnt] := #0;
             { save address }
             query_array[2,qptr] := @qunescaped[cnt+1];
           end;
      '&': begin
             qunescaped[cnt] := #0;
             { Unescape previous one. }
             query_array[2,qptr]:=unescape(query_array[2,qptr]);
             inc(qptr);
             query_array[1,qptr] := @qunescaped[cnt+1];
           end;
    end; { Case }
  { Unescape last one. }
  query_array[2,qptr]:=unescape(query_array[2,qptr]);
  Chop :=qptr;
end;

procedure cgi_read_get_query;
var
  querystring   : pchar;
  qslen         : longint;
begin
  querystring :=strnew(getenv('QUERY_STRING'));
  if querystring<>NIL then
    begin
    qslen :=strlen(querystring);
    if qslen=0 then
      begin
      get_nodata :=true;
      exit;
      end
    else
      get_nodata :=false;
    query_read:=Chop(QueryString);
    end;
  done_init :=true;
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
        pchar(longint(querystring)+sptr)^ :=ch;
        inc(sptr);
        end;
      { !!! force null-termination }
      pchar(longint(querystring)+sptr)^ :=#0;
      query_read:=Chop(QueryString);
      end;
    end;
  done_init :=true;
end;

procedure cgi_init;
var
  rmeth : pchar;
begin
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
end;

procedure cgi_deinit;
begin
  done_init :=false;
  query_read :=0;
  fillchar(query_array,sizeof(query_array),0);
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
  fillchar(query_array,sizeof(query_array),0);
end.

  
{
  HISTORY
  $Log$
  Revision 1.7  2002-10-10 05:48:20  michael
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
