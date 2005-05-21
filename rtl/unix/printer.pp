{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{   Change Log
   ----------

   Started by Michael Van Canneyt, 1996
   (michael@tfdec1.fys.kuleuven.ac.be)

   Current version is 0.9

   Date          Version          Who         Comments
   1999-2000 by         0.8              Michael     Initial implementation
   11/97         0.9              Peter Vreman <pfv@worldonline.nl>
                                              Unit now depends on the
                                              linux unit only.
                                              Cleaned up code.

  ---------------------------------------------------------------------}

Unit printer;

Interface

{.$DEFINE PRINTERDEBUG}

{$I printerh.inc}

Procedure AssignLst ( Var F : text; ToFile : string);
{
 Assigns to F a printing device. ToFile is a string with the following form:
 '|filename options'  : This sets up a pipe with the program filename,
                        with the given options
 'filename' : Prints to file filename. Filename can contain the string 'PID'
              (No Quotes), which will be replaced by the PID of your program.
              When closing lst, the file will be sent to lpr and deleted.
              (lpr should be in PATH)

 'filename|' Idem as previous, only the file is NOT sent to lpr, nor is it
             deleted.
             (useful for opening /dev/printer or for later printing)

 Lst is set up using '/tmp/PID.lst'. You can change this behaviour at
 compile time, setting the DefFile constant.
}

Implementation
Uses Unix,BaseUnix,Strings;

{$I printer.inc}

{
  include definition of textrec
}
{$i textrec.inc}


Const
  P_TOF   = 1; { Print to file }
  P_TOFNP = 2; { Print to File, don't spool }
  P_TOP   = 3; { Print to Pipe }

Var
  Lpr      : String[255]; { Contains path to lpr binary, including null char }

Procedure PrintAndDelete (f:string);
var
  i,j  : longint;
  p,pp : ppchar;
begin
  f:=f+#0;
  if lpr='' then
   exit;
  i:=fpFork;
  if i<0 then
   exit; { No printing was done. We leave the file where it is.}
  if i=0 then
   begin
   { We're in the child }
     getmem(p,12);
     if p=nil then
      halt(127);
     pp:=p;
     pp^:=@lpr[1];
     inc(pp);
     pp^:=@f[1];
     inc(pp);
     pp^:=nil;
     fpExecve(lpr,p,envp);
     { In trouble here ! }
     halt(128)
   end
  else
   begin
   { We're in the parent. }
     fpwaitpid (i,@j,0);
     if j<>0 then
      exit;
   { Erase the file }
     fpUnlink(f);
   end;
end;



Procedure OpenLstPipe ( Var F : Text);
begin
  POpen (f,StrPas(textrec(f).name),'W');
end;



Procedure OpenLstFile ( Var F : Text);
var
  i : longint;
begin
{$IFDEF PRINTERDEBUG}
  writeln ('Printer : In OpenLstFile');
{$ENDIF}
 If textrec(f).mode <> fmoutput then
  exit;
 textrec(f).userdata[15]:=0; { set Zero length flag }
 i:=fpOpen(StrPas(textrec(f).name),(Open_WrOnly or Open_Creat), 438);
 if i<0 then
  textrec(f).mode:=fmclosed
 else
  textrec(f).handle:=i;
end;



Procedure CloseLstFile ( Var F : Text);
begin
{$IFDEF PRINTERDEBUG}
  writeln ('Printer : In CloseLstFile');
{$ENDIF}
  fpclose (textrec(f).handle);
{ In case length is zero, don't print : lpr would give an error }
  if (textrec(f).userdata[15]=0) and (textrec(f).userdata[16]=P_TOF) then
   begin
     fpUnlink(StrPas(textrec(f).name));
     exit
   end;
{ Non empty : needs printing ? }
  if (textrec(f).userdata[16]=P_TOF) then
   PrintAndDelete (strpas(textrec(f).name));
  textrec(f).mode:=fmclosed
end;



Procedure InOutLstFile ( Var F : text);
begin
{$IFDEF PRINTERDEBUG}
  writeln ('Printer : In InOutLstFile');
{$ENDIF}
  If textrec(f).mode<>fmoutput then
   exit;
  if textrec(f).bufpos<>0 then
   textrec(f).userdata[15]:=1; { Set it is not empty. Important when closing !!}
  fpwrite(textrec(f).handle,textrec(f).bufptr^,textrec(f).bufpos);
  textrec(f).bufpos:=0;
end;



function SubstPidInName (const S: string): string;
var
  i    : longint;
  temp : string[8];
begin
  i:=pos('PID',s);
  if i=0 then
   SubstPidInName := S
  else
   begin
    Str (fpGetPid, Temp);
    SubstPidInName := Copy (S, 1, Pred (I)) + Temp +
                                           Copy (S, I + 3, Length (S) - I - 2);
{$IFDEF PRINTERDEBUG}
    writeln ('Print : Filename became : ', Result);
{$ENDIF}
   end;
end;



Procedure AssignLst ( Var F : text; ToFile : string);
begin
{$IFDEF PRINTERDEBUG}
  writeln ('Printer : In AssignLst');
{$ENDIF}
  If ToFile='' then
   exit;
  textrec(f).bufptr:=@textrec(f).buffer;
  textrec(f).bufsize:=128;
  ToFile := SubstPidInName (ToFile);
  if ToFile[1]='|' then
   begin
     Assign(f,Copy(ToFile,2,255));
     textrec(f).userdata[16]:=P_TOP;
     textrec(f).OpenFunc:=@OpenLstPipe;
   end
  else
   begin
    if Tofile[Length(ToFile)]='|' then
      begin
        Assign(f,Copy(ToFile,1,length(Tofile)-1));
        textrec(f).userdata[16]:=P_TOFNP;
      end
     else
      begin
        Assign(f,ToFile);
        textrec(f).userdata[16]:=P_TOF;
      end;
     textrec(f).OpenFunc:=@OpenLstFile;
     textrec(f).CloseFunc:=@CloseLstFile;
     textrec(f).InoutFunc:=@InoutLstFile;
     textrec(f).FlushFunc:=@InoutLstFile;
   end;
end;


begin
  InitPrinter (SubstPidInName ('/tmp/PID.lst'));
  SetPrinterExit;
  Lpr := '/usr/bin/lpr';
end.
