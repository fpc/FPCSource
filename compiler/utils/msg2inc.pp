{
    $Id$
    This program is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by Peter Vreman

    Convert a .msg file to an .inc file with a const array of char
    And for the lazy docwriters it can also generate some TeX output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program msg2inc;
uses strings;

const
  version='0.99.14';
{$ifdef linux}
  eollen=1;
{$else}
  eollen=2;
{$endif}
type
  TMode=(M_Char,M_Tex,M_Intel,M_String,M_Renumber);
var
  InFile,
  OutFile,
  OutName    : string;
  Mode       : TMode;
  TexOption,
  TexHeader,
  TexError   : boolean;

  MsgTxt     : pchar;
  EnumTxt    : pchar;
  enumsize,
  msgsize    : longint;

function XlatString(Var S : String):boolean;
{
  replaces \xxx in string S with #x, and \\ with \ (escaped)
  which can reduce size of string.
  Returns false when an error in the line exists
}
  Function GetNumber(Position:longint):longint;
  var
    C,Value,i : longint;
  begin
    I:=0;
    Value:=0;
    while i<3 do
     begin
       C:=ord(S[Position+I]);
       if (C>47) and (C<56) then
        dec(C,48)
       else
        begin
          GetNumber:=-1;
          exit;
        end;
       if I=0 then
        C:=C shl 6;
       if I=1 then
        C:=C SHL 3;
       inc(Value,C);
       inc(I);
     end;
    GetNumber:=Value;
  end;

var
  S2 : String;
  A,B,Value : longint;
begin
  A:=1;
  B:=1;
  while A<=Length(S) do
   begin
     if (S[A]='\') and (a<length(s)) then
      begin
        if S[A+1]='\' then
         begin
           S2[B]:='\';
           Inc(A,2);
           Inc(B);
         end
        else
         begin
           Value:=GetNumber(A+1);
           if Value=-1 then
            begin
              XlatString:=false;
              exit;
            end;
           S2[B]:=Chr(Value);
           inc(B);
           inc(A,4);
         end;
      end
     else
      begin
        S2[B]:=S[A];
        inc(A);
        inc(B);
      end;
   end;
  S2[0]:=Chr(B-1);
  S:=S2;
  XlatString:=true;
end;


procedure LoadMsgFile(const fn:string);
var
  f       : text;
  line,i  : longint;
  ptxt,
  penum   : pchar;
  s,s1    : string;
begin
  Writeln('Loading messagefile ',fn);
{Read the message file}
  assign(f,fn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+fn+' not found ***');
     exit;
   end;
{ First parse the file and count bytes needed }
  line:=0;
  msgsize:=0;
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if not XlatString(S) then
      S:='';
     if (s<>'') and not(s[1] in ['#',';','%']) then
      begin
        i:=pos('=',s);
        if i>0 then
         begin
           inc(msgsize,length(s)-i+1);
           inc(enumsize,i);
         end
        else
         writeln('error in line: ',line,' skipping');
      end;
   end;
{ now read the buffer in mem }
  getmem(msgtxt,msgsize);
  ptxt:=msgtxt;
  getmem(enumtxt,enumsize);
  penum:=enumtxt;
  reset(f);
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     if not XlatString(S) then
      S[0]:=#0;
     if (s<>'') and not(s[1] in ['#',';','%']) then
      begin
        i:=pos('=',s);
        if i>0 then
         begin
           {txt}
           s1:=Copy(s,i+1,255);
           { support <lf> for empty lines }
           if s1='<lf>' then
            begin
              s1:='';
              { update the msgsize also! }
              dec(msgsize,4);
            end;
           move(s1[1],ptxt^,length(s1));
           inc(ptxt,length(s1));
           ptxt^:=#0;
           inc(ptxt);
           {enum}
           move(s[1],penum^,i-1);
           inc(penum,i-1);
           penum^:=#0;
           inc(penum);
         end;
      end;
   end;
  close(f);
end;


{*****************************************************************************
                               WriteEnumFile
*****************************************************************************}

procedure WriteEnumFile(const fn,typename:string);
var
  t : text;
  i : longint;
  p : pchar;
  start : boolean;
begin
  writeln('Writing enumfile '+fn);
{Open textfile}
  assign(t,fn);
  rewrite(t);
  writeln(t,'type t',typename,'=(');
{Parse buffer in msgbuf and create indexs}
  p:=enumtxt;
  start:=true;
  for i:=1to enumsize do
   begin
     if start then
      begin
        write(t,'  ');
        start:=false;
      end;
     if p^=#0 then
      begin
        writeln(t,',');
        start:=true;
      end
     else
      write(t,p^);
     inc(p);
   end;
  writeln(t,'end',typename);
  writeln(t,');');
  close(t);
end;


{*****************************************************************************
                               WriteStringFile
*****************************************************************************}

procedure WriteStringFile(const fn,constname:string);
const
  maxslen=240; { to overcome aligning problems }

  function l0(l:longint):string;
  var
    s : string[16];
  begin
    str(l,s);
    while (length(s)<5) do
     s:='0'+s;
    l0:=s;
  end;

var
  t      : text;
  f      : file;
  slen,
  len,i  : longint;
  p      : pchar;
  s      : string;
  start,
  quote  : boolean;
begin
  writeln('Writing stringfile ',fn);
{Open textfile}
  assign(t,fn);
  rewrite(t);
  writeln(t,'{$ifdef Delphi}');
  writeln(t,'const '+constname+' : array[0..000000] of string[',maxslen,']=(');
  writeln(t,'{$else Delphi}');
  writeln(t,'const '+constname+' : array[0..000000,1..',maxslen,'] of char=(');
  write(t,'{$endif Delphi}');
{Parse buffer in msgbuf and create indexs}
  p:=msgtxt;
  slen:=0;
  len:=0;
  quote:=false;
  start:=true;
  for i:=1 to msgsize do
   begin
     if slen>=maxslen then
      begin
        if quote then
         begin
           write(t,'''');
           quote:=false;
         end;
        write(t,',');
        slen:=0;
        inc(len);
      end;
     if (len>70) or (start) then
      begin
        if quote then
         begin
           write(t,'''');
           quote:=false;
         end;
        if slen>0 then
          writeln(t,'+')
        else
          writeln(t);
        len:=0;
        start:=false;
      end;
     if (len=0) then
      write(t,'  ');
     if (ord(p^)>=32) and (p^<>#39) then
      begin
        if not quote then
         begin
           write(t,'''');
           quote:=true;
           inc(len);
         end;
        write(t,p^);
        inc(len);
      end
     else
      begin
        if quote then
         begin
           write(t,'''');
           inc(len);
           quote:=false;
         end;
        write(t,'#'+chr(ord(p^) div 100+48)+chr((ord(p^) mod 100) div 10+48)+chr(ord(p^) mod 10+48));
        inc(len,3);
      end;
     if p^=#0 then
      start:=true;
     inc(slen);
     inc(p);
   end;
  if quote then
   write(t,'''');
  writeln(t,'');
  writeln(t,');');
  close(t);
{update arraysize}
  s:=l0(msgsize div maxslen); { we start with 0 }
  assign(f,fn);
  reset(f,1);
  seek(f,34+eollen+length(constname));
  blockwrite(f,s[1],5);
  seek(f,90+3*eollen+2*length(constname));
  blockwrite(f,s[1],5);
  close(f);
end;


{*****************************************************************************
                               WriteCharFile
*****************************************************************************}

procedure WriteCharFile(const fn,constname:string);

  function l0(l:longint):string;
  var
    s : string[16];
  begin
    str(l,s);
    while (length(s)<5) do
     s:='0'+s;
    l0:=s;
  end;

  function createconst(b:byte):string;
  begin
    if (b in [32..127]) and (b<>39) then
     createconst:=''''+chr(b)+''''
    else
     createconst:='#'+chr(b div 100+48)+chr((b mod 100) div 10+48)+chr(b mod 10+48)
  end;

var
  t       : text;
  f       : file;
  cidx,i  : longint;
  p       : pchar;
  s       : string;
begin
  writeln('Writing charfile '+fn);
{Open textfile}
  assign(t,fn);
  rewrite(t);
  writeln(t,'const ',constname,' : array[1..00000] of char=(');
{Parse buffer in msgbuf and create indexs}
  p:=msgtxt;
  cidx:=0;
  for i:=1to msgsize do
   begin
     if cidx=15 then
      begin
        if cidx>0 then
         writeln(t,',')
        else
         writeln(t,'');
        write(t,'  ');
        cidx:=0;
      end
     else
      if cidx>0 then
        write(t,',')
      else
        write(t,'  ');
     write(t,createconst(ord(p^)));
     inc(cidx);
     inc(p);
   end;
  writeln(t,');');
  close(t);
{update arraysize}
  s:=l0(msgsize);
  assign(f,fn);
  reset(f,1);
  seek(f,18+length(constname));
  blockwrite(f,s[1],5);
  close(f);
end;


{*****************************************************************************
                               WriteIntelFile
*****************************************************************************}

procedure WriteIntelFile(const fn,constname:string);
var
  t      : text;
  len,i  : longint;
  p      : pchar;
  start,
  quote  : boolean;
begin
  writeln('Writing Intelfile ',fn);
{Open textfile}
  assign(t,fn);
  rewrite(t);
  writeln(t,'procedure '+constname+';assembler;');
  writeln(t,'asm');
{Parse buffer in msgbuf and create indexs}
  p:=msgtxt;
  len:=0;
  start:=true;
  quote:=false;
  for i:=1to msgsize do
   begin
     if len>70 then
      begin
        if quote then
         begin
           write(t,'''');
           quote:=false;
         end;
        writeln(t,'');
        start:=true;
      end;
     if start then
      begin
        write(t,'  db ''');
        len:=0;
        quote:=true;
      end;
     if (ord(p^)>=32) and (p^<>#39) then
      begin
        if not quote then
         begin
           write(t,',''');
           quote:=true;
           inc(len);
         end;
        write(t,p^);
        inc(len);
      end
     else
      begin
        if quote then
         begin
           write(t,'''');
           inc(len);
           quote:=false;
         end;
        write(t,','+chr(ord(p^) div 100+48)+chr((ord(p^) mod 100) div 10+48)+chr(ord(p^) mod 10+48));
        inc(len,4);
      end;
     inc(p);
   end;
  if quote then
   write(t,'''');
  writeln(t,'');
  writeln(t,'end;');
  close(t);
end;


{*****************************************************************************
                                RenumberFile
*****************************************************************************}

procedure RenumberFile(const fn,name:string);
var
  f,t  : text;
  i    : longint;
  s,s1 : string;
begin
  Writeln('Renumbering ',fn);
{Read the message file}
  assign(f,fn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+fn+' not found ***');
     exit;
   end;
  assign(t,'msg2inc.$$$');
  rewrite(t);
  i:=0;
  while not eof(f) do
   begin
     readln(f,s);
     if (copy(s,1,length(Name))=Name) and (s[3] in ['0'..'9']) then
      begin
        inc(i);
        str(i,s1);
        while length(s1)<3 do
         s1:='0'+s1;
        writeln(t,Name+s1+Copy(s,6,255));
      end
     else
      writeln(t,s);
   end;
  close(t);
  close(f);
{ rename new file }
  erase(f);
  rename(t,fn);
end;


{*****************************************************************************
                                WriteTexFile
*****************************************************************************}

Function EscapeString (Const S : String) : String;
Var
  I  : longint;
  hs : string;
begin
  hs:='';
  for i:=1 to length(s) do
    if S[i]='$' then
      hs:=hs+'arg'
    else
      hs:=hs+s[i];
  EscapeString:=hs;
end;

procedure WriteTexFile(const infn,outfn:string);
var
  t,f   : text;
  line,
  i,k   : longint;
  s,s1  : string;
  texoutput : boolean;
begin
  Writeln('Loading messagefile ',infn);
  writeln('Writing TeXfile ',outfn);
{ Open infile }
  assign(f,infn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     WriteLn('*** message file '+infn+' not found ***');
     exit;
   end;
{ Open outfile }
  assign(t,outfn);
  rewrite(t);
  If texheader then
    begin
    writeln (t,'\documentclass{article}');
    writeln (t,'\usepackage{html}');
    writeln (t,'\usepackage{fpc}');
    writeln (t,'\begin{document}');
    end;
{ Parse }
  line:=0;
  TexOutput:=False;
  while not eof(f) do
   begin
     readln(f,s);
     inc(line);
     If Pos ('# BeginOfTeX',S)=1 then
       TexOutPut:=True
     else if pos ('# EndOfTeX',S)=1 then
       TexOutPut:=False;
     if (s<>'') and not(s[1] in ['#',';']) and TeXOutPut then
      begin
        if s[1]='%' then
         begin
           Delete(s,1,1);
           writeln(t,s);
         end
        else
         begin
           i:=pos('=',s);
           if i>0 then
            begin
              inc(i);
              s1:='';
              k:=0;
              while (k<5) and (s[i+k]<>'_') do
               begin
                 case s[i+k] of
                  'W' : s1:='Warning: ';
                  'E' : s1:='Error: ';
                  'F' : s1:='Fatal: ';
                  'N' : s1:='Note: ';
                  'I' : s1:='Info: ';
                  'H' : s1:='Hint: ';
                 end;
                 inc(k);
               end;
              if s[i+k]='_' then
               inc(i,k+1);
              writeln(t,'\item ['+s1+escapestring(Copy(s,i,255))+']');
            end
           else
            writeln('error in line: ',line,' skipping');
         end;
      end;
   end;
  If TexHeader then
    writeln (t,'\end{document}');
  close(t);
  close(f);
end;


{*****************************************************************************
                                Main Program
*****************************************************************************}

procedure getpara;
var
  ch      : char;
  para    : string;
  files,i : word;

  procedure helpscreen;
  begin
    writeln('usage : msg2inc [Options] <msgfile> <incfile> <constname>');
    writeln('<Options> can be : -TE    Create .doc TeX file (error style)');
    writeln('                   -TO    Create .doc TeX file (options style)');
    writeln('                   -TS    Create .doc TeX file (stand-alone)');
    writeln('                   -I     Intel style asm output');
    writeln('                   -S     array of string');
    writeln('                   -C     array of char');
    writeln('                   -R     renumber section <incfile>');
    writeln('                   -V     Show version');
    writeln('             -? or -H     This HelpScreen');
    halt(1);
  end;

begin
  Mode:=M_String;
  FIles:=0;
  for i:=1to paramcount do
   begin
     para:=paramstr(i);
     if (para[1]='-') then
      begin
        ch:=upcase(para[2]);
        delete(para,1,2);
        case ch of
         'T' : begin
                 case upcase(para[1]) of
                  'O' : TexOption:=true;
                  'E' : TexError:=true;
                  'S' : TexHeader:=True;
                 end;
                 Mode:=M_Tex;
               end;
         'I' : Mode:=M_Intel;
         'S' : Mode:=M_String;
         'C' : Mode:=M_Char;
         'R' : Mode:=M_Renumber;
         'V' : begin
                 Writeln('Msg2Inc ',version,' for Free Pascal (C) 1998 Peter Vreman');
                 Writeln;
                 Halt;
               end;
     '?','H' : helpscreen;
        end;
     end
    else
     begin
       inc(Files);
       if Files>3 then
        HelpScreen;
       case Files of
        1 : InFile:=Para;
        2 : OutFile:=Para;
        3 : OutName:=Para;
       end;
     end;
   end;
  case Mode of
   M_Renumber,
        M_Tex : if Files<2 then
                 Helpscreen;
  else
   if FIles<3 then
    HelpScreen;
  end;
end;


begin
  GetPara;
  case Mode of
   M_Renumber : begin
                  Renumberfile(Infile,OutFile);
                end;
        M_Tex : begin
                  WriteTexFile(InFile,OutFile+'.tex');
                end;
      M_Intel : begin
                  Loadmsgfile(InFile);
                  WriteEnumFile(OutFile+'idx.inc',OutName+'const');
                  WriteIntelFile(OutFile+'txt.inc',OutName+'txt');
                end;
     M_String : begin
                  Loadmsgfile(InFile);
                  WriteEnumFile(OutFile+'idx.inc',OutName+'const');
                  WriteStringFile(OutFile+'txt.inc',OutName+'txt');
                end;
       M_Char : begin
                  Loadmsgfile(InFile);
                  WriteEnumFile(OutFile+'idx.inc',OutName+'const');
                  WriteCharFile(OutFile+'txt.inc',OutName+'txt');
                end;
  end;
end.
{
  $Log$
  Revision 1.5  2000-02-09 13:23:11  peter
    * log truncated

  Revision 1.4  2000/01/27 11:29:15  peter
    * version 0.99.14

  Revision 1.3  2000/01/07 01:15:00  peter
    * updated copyright to 2000

}
