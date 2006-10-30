program FixTab;
uses Dos;

const
  {Files}
  InputExt='';
  OutputExt='*';


var
{General}
  InFile,
  OutFile   : string[80];
  ParaFile  : word;
{Specific}
const
  TabSize   : longint=8;
  DosEol    : boolean=false;
  Verbose   : boolean=false;

{****************************************************************************
                                 Routines
****************************************************************************}

const
{$IFDEF LINUX}
  PathCh='/';
{$ELSE}
  PathCh='\';
{$ENDIF}

Function SplitPath(Const HStr:String):String;
var
  i : byte;
begin
  i:=Length(Hstr);
  while (i>0) and (Hstr[i]<>PathCh) do
   dec(i);
  SplitPath:=Copy(Hstr,1,i);
end;



Function SplitFileName(Const HStr:String):String;
var
  i : byte;
begin
  i:=Length(Hstr);
  while (i>0) and (Hstr[i]<>PathCh) do
   dec(i);
  SplitFileName:=Copy(Hstr,i+1,255);
end;



Function SplitName(Const HStr:String):String;
var
  i,j : byte;
begin
  i:=Length(Hstr);
  j:=i;
  while (i>0) and (Hstr[i]<>PathCh) do
   dec(i);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j<=i then
   j:=255;
  SplitName:=Copy(Hstr,i+1,j-(i+1));
end;



Function SplitExtension(Const HStr:String):String;
var
  j,i : byte;
begin
  i:=Length(Hstr);
  j:=i;
  while (i>0) and (Hstr[i]<>PathCh) do
   dec(i);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j<=i then
   j:=254;
  SplitExtension:=Copy(Hstr,j+1,255);
end;



Function ChangeFileExt(Const HStr,ext:String):String;
begin
  if (Ext<>'') and (SplitExtension(HStr)='') then
   ChangeFileExt:=Hstr+'.'+Ext
  else
   ChangeFileExt:=Hstr;
end;



Function ForceExtension(Const HStr,ext:String):String;
var
  j : byte;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext;
end;


function UCase(Const Hstr:string):string;
var
  i : byte;
begin
  for i:=1to Length(Hstr) do
   UCase[i]:=Upcase(Hstr[i]);
  UCase[0]:=chr(Length(Hstr));
end;



Function ESpace(HStr:String;len:byte):String;
begin
  while length(Hstr)<Len do
   begin
     inc(byte(Hstr[0]));
     Hstr[Length(Hstr)]:=' ';
   end;
  ESpace:=Hstr;
end;

{****************************************************************************
                                 Main Stuff
****************************************************************************}

var
  Done  : array[0..1023] of string[32];
  Total : word;
Function FileDone(const fn:string):boolean;
var
  i : word;
begin
  i:=0;
  while (i<Total) and (Done[i]<>fn) do
   inc(i);
  if Done[i]=fn then
   FileDone:=true
  else
   begin
     Done[Total]:=fn;
     inc(Total);
     FileDone:=false;
   end;
end;



procedure Convert(fn,nfn:string);
type
  inbuftype=array[0..31999] of char;
  outbuftype=array[0..63999] of char;
var
  f,g    : file;
  inbuf  : ^inbuftype;
  outbuf : ^outbuftype;
  Curr,
  TabCol,
  col,
  i,last,
  innum,
  outnum : longint;

  procedure WriteBuf;
  begin
    if i>last then
     begin
       move(InBuf^[last],OutBuf^[OutNum],i-last);
       inc(OutNum,(i-last));
     end;
    Last:=i+1;
  end;

begin
{Create New FileName}
  if SplitExtension(nfn)='*' then
   nfn:=ChangeFileExt(SplitPath(nfn)+SplitName(nfn),SplitExtension(fn));
  if SplitName(nfn)='*' then
   begin
     if SplitPath(nfn)='' then
      nfn:=ChangeFileExt(SplitPath(fn)+SplitName(fn),SplitExtension(nfn))
     else
      nfn:=ChangeFileExt(SplitPath(nfn)+SplitName(fn),SplitExtension(nfn));
   end;
{Done?}
  if FileDone(nfn) then
   exit;
{Open Files}
  Write('Converting '+ESpace(fn,30)+' ');
  if fn=nfn then
   assign(g,ForceExtension(fn,'$T$'))
  else
   begin
     Write('-> '+ESpace(nfn,30)+' ');
     assign(g,nfn);
   end;
  new(inbuf);
  new(outbuf);
  assign(f,fn);
  {$I-}
   reset(f,1);
  {$I+}
  if ioresult<>0 then
   exit;
  {$I-}
   rewrite(g,1);
  {$I+}
  if ioresult<>0 then
   begin
     close(f);
     exit;
   end;
  Curr:=0;
  col:=1;
  last:=0;
  repeat
    blockread(f,InBuf^,sizeof(InBufType),innum);
    outnum:=0;
    if innum>0 then
     begin
       i:=0;
       while (i<innum) do
        begin
          case InBuf^[i] of
           #9 : begin
                  WriteBuf;
                  OutBuf^[OutNum]:=' ';
                  inc(OutNum);
                  inc(Col);
                  TabCol:=(((Col-1) div TabSize)+1)*TabSize;
                  while (Col<TabCol) do
                   begin
                     OutBuf^[OutNum]:=' ';
                     inc(OutNum);
                     inc(Col);
                   end;
                end;
          #13 : begin
                  WriteBuf;
                  while (outnum>0) and (outbuf^[outnum-1] in [' ',#9]) do
                   dec(outnum);
                end;
          #10 : begin
                  WriteBuf;
                  while (outnum>0) and (outbuf^[outnum-1] in [' ',#9]) do
                   dec(outnum);
                  if DosEol then
                   begin
                     OutBuf^[OutNum]:=#13;
                     inc(OutNum);
                   end;
                  OutBuf^[OutNum]:=#10;
                  inc(OutNum);
                  col:=0;
                  inc(Curr);
                  if (curr and 31)=0 then
                   Write(Curr:5,#8#8#8#8#8);
                end;
           else
            inc(col);
           end;
          inc(i);
        end;
       WriteBuf;
       last:=0;
     end;
    blockwrite(g,OutBuf^,outnum);
  until innum=0;
  WriteLn(Curr,' Lines');
  close(g);
  close(f);
  if fn=nfn then
   begin
     erase(f);
     rename(g,fn);
   end;
  dispose(outbuf);
  dispose(inbuf);
end;


{****************************************************************************
                                General Stuff
****************************************************************************}

procedure getpara;
var
  ch   : char;
  para : string[128];
  i,j  : word;

  procedure helpscreen;
  begin
    writeln('Usage : '+SplitName(ParamStr(0))+' [Options] <InFile(s)>'#10);
    writeln('<Options> can be : -O<OutFile>  Specify OutFile Mask');
    WriteLn('                   -D           Use MsDos #13#10 Eols');
    writeln('                   -T<size>     Set Size of Tabs');
    writeln('                   -V           be more verbose');
    writeln('             -? or -H           This HelpScreen');
    halt(1);
  end;

begin
  for i:=1to paramcount do
   begin
     para:=ucase(paramstr(i));
     if (para[1]='-') then
      begin
        ch:=para[2];
        delete(para,1,2);
        case ch of
         'O' : OutFile:=ChangeFileExt(Para,OutputExt);
         'D' : DosEol:=true;
         'T' : Val(Para,TabSize,j);
         'V' : verbose:=true;
     '?','H' : helpscreen;
        end;
     end
    else
     begin
       if ParaFile=0 then
        ParaFile:=i;
     end;
   end;
  if (ParaFile=0) then
   HelpScreen;
  if OutFile='' then
   OutFile:=ForceExtension('*',OutPutExt);
end;



var
  Dir : SearchRec;
  i   : word;
begin
  GetPara;
{Main}
  if Verbose then
   begin
     Writeln('fixtab v1.01 (C) 1999-2002 Peter Vreman');
     Writeln('TabSize ',TabSize);
     if DosEol then
      WriteLn('Using MsDos Eols');
   end;
  for i:=ParaFile to ParamCount do
   begin
     InFile:=ChangeFileExt(ParamStr(i),InputExt);
     FindFirst(InFile,$20,Dir);
     while (DosError=0) do
      begin
        Convert(SplitPath(InFile)+Dir.Name,OutFile);
        FindNext(Dir);
      end;
   end;
end.
