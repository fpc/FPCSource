{ Source provided for Free Pascal Bug Report 2968 }
{ Submitted by "Marco (gory bugs)" on  2004-02-10 }
{ e-mail:  }
uses
  dos;

function ArrayStringToPPchar(const S:Array of AnsiString;reserveone:boolean):ppchar; // const ?
// ReserveOne:=True -> one extra pchar is allocated and the first ( p[0]) is left for commandline

var p   : ppchar;
    Res,
    i   : LongInt;
begin
 if High(s)<Low(s) Then Exit(NIL);
 Res:=0;
 If ReserveOne Then
   Res:=1 ;
 Getmem(p,1+sizeof(pchar)*(high(s)-low(s)));  // one more for NIL, one more
                                              // for cmd
 for i:=low(s) to high(s) do
    p[i+Res]:=@pchar(@s[i])[1];
//  p[i+Res]:=@ansistring(s[i])[1];
 p[high(s)+1+Res]:=nil;
 ArrayStringToPPchar:=p;
end;

function intFpExec (Const PathName:AnsiString;const S:Array Of AnsiString):longint;

Var
  p       : ppchar;
  NewCmd  : ansistring;
  ThePath : String;

begin
  p:=ArrayStringToPPchar(s,True);
  newcmd:=PathName;
{  If SearchPath Then
   Begin}
     Thepath:=getenv('PATH');
     if thepath='' then
      thepath:='.';
     newcmd:=FSearch(newcmd,thepath);
     writeln(newcmd);
{   End;}
//  p^:=@NewCmd[1];
//  IntFpExec:=fpExecVE(p^,p,MyEnv);
end;


function FpExecLP(Const PathName:AnsiString;const S:Array Of AnsiString):longint;

Begin
  FpExecLP:=intFPExec(PathName,S);
End;


begin
 fpexeclp('sh',['-c','echo hello']);
end.
