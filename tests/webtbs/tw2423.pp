{ %target=win32 }
{ Source provided for Free Pascal Bug Report 2423 }
{ Submitted by "Pavel V. Ozerski" on  2003-03-18 }
{ e-mail: ozerski@list.ru }
{$ifdef fpc}
{$mode objfpc}
{$endif}
{ $define BugAvoid}
type
 pVariant=^Variant;
function ShowHTMLDialog(const hwndParent:longint;const pmk:pointer;
                                const pvarArgIn:Variant;const pchOptions:{pwidechar}pointer;
                                pvarArgOut:pVariant):longint;stdcall;
 external 'MSHTML.DLL';
function CreateURLMoniker(const pmkContext:pointer;const szURL:pWideChar;var ppmk:pointer):longint;stdcall;
 external 'URLMON.DLL';

{$ifdef BugAvoid}

function SysAllocStringLen(psz:pointer;len:Integer):pointer;stdcall;
 external 'oleaut32.dll' name 'SysAllocStringLen';

procedure SysFreeString(bstr:pointer);stdcall;
 external 'oleaut32.dll' name 'SysFreeString';

function MultiByteToWideChar(CodePage:cardinal;dwFlags:cardinal;
                             lpMultiByteStr:pChar;cchMultiByte:longint;
                             lpWideCharStr:pointer;cchWideChar:longint
                             ):longint;stdcall;
 external 'kernel32.dll';

function MakeWide(const s:ansistring):pointer;
 var
  l:cardinal;
 begin
  l:=succ(length(s));
  Result:=SysAllocStringLen(nil,l);
  MultiByteToWideChar(0,1,@s[1],length(s),Result,l);
 end;

{$endif}

var
 buf:pointer;
const
 Htm:AnsiString='<HTML><SCRIPT language="JavaScript">document.write(window.dialogArguments);'+
 'setTimeout("window.close();",1000);</SCRIPT></HTML>';
var
 t:file;
 ws:widestring;
 s:ansistring;
 pmk:pointer;

 {$ifdef BugAvoid}

  InParam_data:TVarData;
  InParam:variant absolute InParam_data;

 {$else}

 inparam:variant;

 {$endif}

 i:longint;
begin
 s:=paramstr(0);
 for i:=length(s) downto 1 do
  if s[i]='\'then
   begin
    setlength(s,i);
    break;
   end;
 s:=s+'demo.htm';
 assign(t,s);
 rewrite(t,1);
 blockwrite(t,Htm[1],length(Htm));
 close(t);
 ws:=s;


{$ifdef BugAvoid}

 buf:=MakeWide(s);


{$else}

 buf:=pWideChar(ws);

{$endif}

 CreateURLMoniker(nil,buf,pmk);

{$ifdef BugAvoid}

 InParam_data.VType:=8;
 InParam_data.VPointer:=buf;

{$else}

 InParam:=ws;

{$endif}

 ShowHTMLDialog(0,pmk,InParam,nil,nil);
end.
