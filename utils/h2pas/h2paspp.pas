{
    Copyright (c) 2000 by Peter Vreman

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
program h2paspp;

type
  PSymbol=^TSymbol;
  TSymbol=record
    name : string[32];
    next : PSymbol;
  end;
var
  Symbols : PSymbol;
  OutFile : string;


procedure def_symbol(const s:string);
var
  p : PSymbol;
begin
  new(p);
  p^.name:=s;
  p^.next:=Symbols;
  Symbols:=p;
end;

procedure undef_symbol(const s:string);
var
  p,plast : PSymbol;
begin
  p:=Symbols;
  plast:=nil;
  while assigned(p) do
   begin
     if p^.name=s then
      begin
        if assigned(plast) then
         plast^.next:=p^.next
        else
         Symbols:=p^.next;
        dispose(p);
        exit;
      end;
     p:=p^.next;
   end;
end;

function check_symbol(const s:string):boolean;
var
  p : PSymbol;
begin
  check_symbol:=false;
  p:=Symbols;
  while assigned(p) do
   begin
     if p^.name=s then
      begin
        check_symbol:=true;
        exit;
      end;
     p:=p^.next;
   end;
end;

procedure clear_symbols;
var
  hp : PSymbol;
begin
  while assigned(Symbols) do
   begin
     hp:=Symbols;
     Symbols:=Symbols^.next;
     dispose(hp);
   end;
end;

function dofile(const filename : string):boolean;

  procedure RemoveSpace(var fn:string);
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in [' ',#9]) do
      inc(i);
    Delete(fn,1,i);
    i:=length(fn);
    while (i>0) and (fn[i] in [' ',#9]) do
      dec(i);
    fn:=copy(fn,1,i);
  end;

  function GetName(var fn:string):string;
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in ['a'..'z','A'..'Z','0'..'9','_','-']) do
     inc(i);
    GetName:=Copy(fn,1,i);
    Delete(fn,1,i);
  end;

const
  maxlevel=16;
var
  f,g   : text;
  s,orgs,
  opts  : string;
  skip  : array[0..maxlevel-1] of boolean;
  level : longint;
begin
  dofile:=false;
{ open file }
  assign(f,filename);
  {$I-}
  reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     Writeln('Unable to open file ',filename);
     exit;
   end;
  if outfile='' then
   assign(g,'h2paspp.tmp')
  else
   assign(g,outfile);
  {$I-}
  rewrite(g);
  {$I+}
  if ioresult<>0 then
   begin
     Writeln('Unable to create file tmp');
     Close(f);
     exit;
   end;
  fillchar(skip,sizeof(skip),0);
  level:=0;
  while not eof(f) do
   begin
     readln(f,orgs);
     opts:=orgs;
     if (opts<>'') and (opts[1]='#') then
      begin
        Delete(opts,1,1);
        RemoveSpace(opts);
        s:=GetName(opts);
        if (s='ifdef') then
         begin
           RemoveSpace(opts);
           if Level>=maxlevel then
            begin
              Writeln('Too many ifdef levels');
              exit;
            end;
           inc(Level);
           skip[level]:=(skip[level-1] or (not check_symbol(GetName(opts))));
         end
        else
         if (s='if') then
          begin
            RemoveSpace(opts);
            if Level>=maxlevel then
             begin
               Writeln('Too many ifdef levels');
               exit;
             end;
            inc(Level);
            skip[level]:=(skip[level-1] or (not check_symbol(GetName(opts))));
          end
        else
         if (s='ifndef') then
          begin
            RemoveSpace(opts);
            if Level>=maxlevel then
             begin
               Writeln('Too many ifdef levels');
               exit;
             end;
            inc(Level);
            skip[level]:=(skip[level-1] or (check_symbol(GetName(opts))));
          end
        else
         if (s='else') then
          skip[level]:=skip[level-1] or (not skip[level])
        else
         if (s='endif') then
          begin
            skip[level]:=false;
            if Level=0 then
             begin
               Writeln('Too many endif found');
               exit;
             end;
            dec(level);
          end
        else
         if (not skip[level]) then
          begin
            if (s='define') then
             begin
               RemoveSpace(opts);
               def_symbol(GetName(opts));
             end
            else
             if (s='undef') then
              begin
                RemoveSpace(opts);
                undef_symbol(GetName(opts));
              end
            else
             if (s='include') then
              begin
                RemoveSpace(opts);
                Writeln('Uses include: ',opts);
                opts:='';
              end;
            { Add defines also to the output }
            if opts<>'' then
             writeln(g,orgs);
          end;
       end
      else
       begin
         if (not skip[level]) then
          writeln(g,orgs);
       end;
   end;
  if Level>0 then
   Writeln('Error: too less endif found');
  Close(f);
  Close(g);
  if outfile='' then
   begin
     Erase(f);
     Rename(g,filename);
   end;
  DoFile:=true;
end;


procedure Usage;
begin
  writeln('h2paspp [options] <file(s)>');
  writeln('options:');
  writeln('  -d<symbol>   define symbol');
  writeln('  -o<outfile>  output file');
  writeln('  -i           include also includes (default is to remove)');
  writeln('  -h or -?     this helpscreen');
  halt(0);
end;


var
  i,j : longint;
  s : string;
begin
{ process options }
  j:=0;
  for i:=1to paramcount do
   begin
     s:=paramstr(i);
     if s[1]='-' then
      begin
        case s[2] of
         'd' :
           def_symbol(Copy(s,3,255));
         'o' :
           outfile:=Copy(s,3,255);
         'h','?' :
           Usage;
        end;
      end
     else
      inc(j);
   end;
  { no files? }
  if j=0 then
   Usage;
{ process files }
  for i:=1to paramcount do
   begin
     s:=paramstr(i);
     if s[1]<>'-' then
      dofile(s);
   end;
end.
