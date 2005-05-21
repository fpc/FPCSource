{
    This program is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Program to create a depend makefile for a program with multiple units }

program ppdep;
uses Dos;

{.$define debug}

const
{$ifdef unix}
  exeext='';
{$else}
  exeext='.EXE';
{$endif}

type
  PUses=^TUses;
  TUses=record
    Name : string[32];
    Next : PUses;
  end;
  PUnit=^TUnit;
  TUnit=record
    UsesList : PUses;
    PasFn,
    Name     : string[32];
    IsUnit   : boolean;
    Next     : PUnit;
  end;
  PDefine=^TDefine;
  TDefine = Record
    Name : String[32];
    Next : PDefine;
    end;


var
  UnitList         : PUnit;
  Define           : PDefine;
  ParaFile         : string;
  Verbose          : boolean;
  AddCall          : byte;
  CallLine,
  OutFile          : String;
  UnitExt          : String;

{****************************************************************************
                           Handy Routines
****************************************************************************}

function UCase(Const Hstr:string):string;
var
  i : longint;
begin
  for i:=1to Length(Hstr) do
   UCase[i]:=Upcase(Hstr[i]);
  UCase[0]:=chr(Length(Hstr));
end;


function FixFn(const s:string):string;
var
  i      : longint;
  NoPath : boolean;
begin
  NoPath:=true;
  for i:=length(s) downto 1 do
   begin
     case s[i] of
 {$ifdef unix}
  '/','\' : begin
              FixFn[i]:='/';
              NoPath:=false; {Skip lowercasing path: 'X11'<>'x11' }
            end;
 'A'..'Z' : if NoPath then
             FixFn[i]:=char(byte(s[i])+32)
            else
             FixFn[i]:=s[i];
 {$else}
      '/' : FixFn[i]:='\';
 'A'..'Z' : FixFn[i]:=char(byte(s[i])+32); { everything lowercase }
 {$endif}
     else
      FixFn[i]:=s[i];
     end;
   end;
  FixFn[0]:=Chr(Length(s));
end;


{****************************************************************************
                             Main Program
****************************************************************************}

Function SearchPas(const fn:string):string;
var
  Dir : SearchRec;
begin
  FindFirst(FixFn(fn+'.PP'),$20,Dir);
  if Doserror=0 then
   SearchPas:=FixFn(fn+'.PP')
  else
   SearchPas:=FixFn(fn+'.PAS')
end;


Function UnitDone(const fn:string):boolean;
var
  hp : PUnit;
begin
  hp:=UnitList;
  while not (hp=nil) do
   begin
     if hp^.Name=fn then
      begin
        UnitDone:=true;
        exit;
      end;
     hp:=hp^.Next;
   end;
  UnitDone:=false;
end;


Function CheckDefine(const s:string):boolean;
var
  ss : string[32];
  P : PDefine;
begin
  ss:=ucase(s);
  P:=Define;
  while (p<>Nil) do
   begin
     if ss=p^.name then
      begin
        CheckDefine:=true;
        exit;
      end;
     P:=P^.Next;
   end;
  CheckDefine:=false;
end;

Procedure AddDefine(Const S : String);
Var
  P : PDefine;
begin
  New(P);
  P^.Name:=Ucase(S);
  P^.Next:=Define;
  Define:=P;
end;


procedure RemoveSep(var fn:string);
var
  i : longint;
begin
  i:=0;
  while (i<length(fn)) and (fn[i+1] in [',',' ',#9]) do
   inc(i);
  Delete(fn,1,i);
end;


function GetName(var fn:string):string;
var
  i : longint;
begin
  i:=0;
  while (i<length(fn)) and (fn[i+1] in ['A'..'Z','0'..'9','_','-']) do
   inc(i);
  GetName:=Copy(fn,1,i);
  Delete(fn,1,i);
end;


procedure ListDepend(const fn:string);

{$ifndef FPC}
  procedure readln(var t:text;var s:string);
  var
    c : char;
    i : longint;
  begin
    c:=#0;
    i:=0;
    while (not eof(t)) and (c<>#10) do
     begin
       read(t,c);
       if c<>#10 then
        begin
          inc(i);
          s[i]:=c;
        end;
     end;
    if (i>0) and (s[i]=#13) then
     dec(i);
    s[0]:=chr(i);
  end;
{$endif}

const
  MaxLevel=200;
var
  f  : text;
  hs : ^string;
  curruses,lastuses : PUses;
  currunit,lastunit : PUnit;
  i,j : longint;
  UsesDone,
  OldComment,
  Done,Comment,
  InImplementation : boolean;
  Skip : array[0..MaxLevel] of boolean;
  Level : byte;
begin
  if UnitDone(fn) then
   exit;
  new(hs);
  new(currunit);
  currunit^.next:=nil;
  currunit^.Name:=fn;
  currunit^.IsUnit:=true;
  currunit^.PasFn:=SearchPas(fn);
  currunit^.useslist:=nil;
  assign(f,currunit^.PasFn);
  {$I-}
   reset(f);
  {$I+}
  if ioresult=0 then
   begin
     if verbose then
      Writeln('Processing ',currunit^.PasFn);
   {Add to Linked List}
     if unitlist=nil then
      unitlist:=currunit
     else
      begin
        lastunit:=UnitList;
        while not (lastunit^.Next=nil) do
         lastunit:=lastunit^.next;
        lastunit^.next:=currunit;
      end;
   {Parse file}
     InImplementation:=false;
     done:=false;
     usesdone:=true;
     Comment:=false;
     OldComment:=false;
     FillChar(skip,sizeof(Skip),0);
     hs^:='';
     Level:=0;
     while (not done) and (not Eof(f)) do
      begin
        repeat
          if hs^='' then
           begin
             ReadLn(f,hs^);
             hs^:=UCase(hs^);
           end;
          RemoveSep(hs^);
        until (hs^<>'') or Eof(f);
        if Comment then
         begin
           i:=pos('}',hs^);
           if (i>0) then
            begin
              j:=pos('{',hs^);
              if (j>0) and (j<i) then
               begin
                 Comment:=true;
                 Delete(hs^,1,j-1);
               end
              else
               begin
                 Comment:=false;
                 Delete(hs^,1,i-1);
               end;
            end
           else
            hs^:='';
         end;
        if (pos('(*',hs^)>0) or OldComment then
         begin
           i:=pos('*)',hs^);
           if (i>0) then
            begin
              OldComment:=false;
              Delete(hs^,1,i+1);
            end
           else
            begin
              OldComment:=true;
              hs^:='';
            end;
         end;
        if (hs^<>'') then
         begin
           case hs^[1] of
            '}' : begin
                    Comment:=false;
                    hs^:='';
                  end;
            '{' : begin
                    if (Copy(hs^,2,6)='$IFDEF') then
                     begin
                       Delete(hs^,1,7);
                       RemoveSep(hs^);
                       inc(Level);
                       if Level>=MaxLevel then
                        begin
                          Writeln('Too many IF(N)DEFs');
                          Halt(1);
                        end;
                       skip[level]:=skip[level-1] or (not CheckDefine(GetName(hs^)));
                       hs^:='';
                     end
                    else
                     if (Copy(hs^,2,7)='$IFNDEF') then
                      begin
                        Delete(hs^,1,7);
                        RemoveSep(hs^);
                        inc(Level);
                        if Level>=MaxLevel then
                         begin
                           Writeln('Too many IF(N)DEFs');
                           Halt(1);
                         end;
                        skip[level]:=skip[level-1] or (CheckDefine(GetName(hs^)));
                        hs^:='';
                      end
                    else
                     if (Copy(hs^,2,6)='$ELSE') then
                      begin
                        skip[level]:=skip[level-1] or (not skip[level]);
                        hs^:='';
                      end
                    else
                     if (Copy(hs^,2,6)='$ENDIF') then
                      begin
                        skip[level]:=false;
                        if Level=0 then
                         begin
                           Writeln('Too many ENDIFs');
                           Halt(1);
                         end;
                        dec(level);
                        hs^:='';
                      end
                    else
                     if (Copy(hs^,2,6)='$IFOPT') then
                      begin
                        inc(Level);
                        if Level>=MaxLevel then
                         begin
                           Writeln('Too many IF(N)DEFs');
                           Halt(1);
                         end;
                        skip[level]:=true;
                        hs^:='';
                      end
                    else
                     begin
                       i:=pos('}',hs^);
                       if i>0 then
                        begin
                          Delete(hs^,1,i);
                          Comment:=false;
                        end
                       else
                        Comment:=true;
                     end;
                  end;
            ';' : begin
                    UsesDone:=true;
                    Done:=(UsesDone and InImplementation);
                    hs^:='';
                  end;
           else
            begin
              if skip[level] then
               hs^:=''
              else
               begin
                 if (not UsesDone) then
                  begin
                    new(curruses);
                    curruses^.Name:=GetName(hs^);
                    curruses^.next:=nil;
                    if currunit^.useslist=nil then
                     currunit^.useslist:=curruses
                    else
                     begin
                       lastuses:=currunit^.useslist;
                       while not (lastuses^.Next=nil) do
                        lastuses:=lastuses^.next;
                       lastuses^.next:=curruses;
                     end;
   {$ifndef debug}
                    ListDepend(curruses^.Name);
   {$endif}
                    RemoveSep(hs^);
                  end
                 else
                  begin
                    if (Copy(hs^,1,4)='USES') and ((length(hs^)=4) or (hs^[5] in [' ',#9])) then
                     begin
                       Delete(hs^,1,4);
                       UsesDone:=false;
                     end
                    else
                     begin
                       if (hs^='IMPLEMENTATION') then
                        InImplementation:=true
                       else
                        if (Copy(hs^,1,7)='PROGRAM') then
                         begin
                           currunit^.IsUnit:=false;
                           InImplementation:=true; {there can be only 1 uses}
                         end
                       else
                         if InImplementation and ((copy(hs^,1,5)='CONST') or
                            (copy(hs^,1,3)='VAR') or (copy(hs^,1,5)='BEGIN')) then
                          done:=true;
                       hs^:='';
                     end;
                  end;
               end;
            end;
           end;
         end;
      end;
     Close(f);
   end
  else
   dispose(currunit);
  dispose(hs);
end;


procedure ShowDepend;
var
  currunit : PUnit;
  curruses : PUses;
  t        : text;
  P        : PDefine;
  First    : boolean;
begin
  if CallLine='' then
   begin
     CallLine:='ppc386 ';
     P:=Define;
     While P<>Nil do
      begin
        CallLine:=CallLine+' -d'+P^.Name;
        P:=P^.Next;
      end;
   end;
  assign(t,OutFile);
  rewrite(t);
  currunit:=UnitList;
  First:=true;
  while not (currunit=nil) do
   begin
     if currunit^.IsUnit then
      Write(t,FixFn(currunit^.Name+'.'+unitext)+': '+currunit^.PasFn)
     else
      Write(t,FixFn(currunit^.Name+exeext)+': '+currunit^.PasFn);
     curruses:=currunit^.useslist;
     while not (curruses=nil) do
      begin
{$ifndef debug}
        if UnitDone(curruses^.name) then
{$endif}
         begin
           writeln(t,' \');
           write(t,#9+FixFn(curruses^.name+'.'+unitext));
         end;
        curruses:=curruses^.next;
      end;
     writeln(t,'');
     If (AddCall=2) or (First and (AddCall=1)) then
      writeln(t,#9,CallLine,' ',currunit^.PasFn);
     writeln(t,'');
     currunit:=currunit^.next;
     First:=false;
   end;
  close(t);
end;


procedure getpara;
var
  ch   : char;
  para : string[128];
  i    : word;

  procedure helpscreen;
  begin
    writeln('ppdep [Options] <File>');
    Writeln;
    Writeln('Options can be: -D<define>   Define a symbol');
    Writeln('                -oFile       Write output to file');
    WRiteln('                             (default stdout)');
    Writeln('                -eext        Set unit extension to ext');
    Writeln('                             (default ppu)');
    Writeln('                -V           Be more verbose');
    Writeln('          -? or -H           This HelpScreen');
    Writeln('                -A[call]     Add compiler calls to makefile (all files)');
    Writeln('                -F[call]     Add compiler calls to makefile (only top file)');
    halt(1);
  end;

begin
  Define:=Nil;
  Outfile:='';
  AddCall:=0;
  Verbose:=False;
{$IFDEF Unix}
  UnitExt:='ppu';
{$ELSE}
  UnitExt:='PPU';
{$endif}
  for i:=1 to paramcount do
   begin
     para:=Paramstr(i);
     if (para[1]='-') then
      begin
        ch:=Upcase(para[2]);
        delete(para,1,2);
        case ch of
         'A' : begin
                 AddCall:=2;
                 CallLine:=Para;
               end;
         'F' : begin
                 AddCall:=1;
                 CallLine:=Para;
               end;
         'D' : AddDefine(para);
         'O' : OutFile:=Para;
         'E' : UnitExt:=Para;
         'V' : verbose:=true;
     '?','H' : helpscreen;
        end;
     end
    else
     begin
       ParaFile:=Para;
       if Pos('.',ParaFile)>0 then
        Delete(Parafile,Pos('.',ParaFile),255);
     end;
    end;
  if (ParaFile='') then
   HelpScreen;
end;


begin
  GetPara;
  ListDepend(ParaFile);
  ShowDepend;
end.
