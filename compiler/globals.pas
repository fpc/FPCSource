{
    $Id$
    Copyright (C) 1993-98 by Florian Klaempfl

    This unit implements some support functions and global variables

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

 ****************************************************************************
}

{$ifdef tp}
  {$E+,N+}
{$endif}

unit globals;

  interface

    uses
{$ifdef TP}
      objects,
{$endif}
{$ifdef Delphi4}
      dmisc,
      sysutils,
{$endif}
{$ifdef linux}
      linux,
{$endif}
      strings,dos,
      globtype,version,tokens,systems,cobjects;

    const
{$ifdef linux}
       DirSep = '/';
{$else}
  {$ifdef amiga}
       DirSep = '/';
  {$else}
       DirSep = '\';
  {$endif}
{$endif}

{$ifdef Splitheap}
       testsplit : boolean = false;
{$endif Splitheap}

       delphimodeswitches : tmodeswitches=
         [m_delphi,m_tp,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar];
       fpcmodeswitches    : tmodeswitches=
         [m_fpc,m_all,m_string_pchar,m_nested_comment,m_repeat_forward,
          m_cvar_support];
       objfpcmodeswitches : tmodeswitches=
         [m_fpc,m_all,m_objpas,m_class,m_result,m_string_pchar,m_nested_comment,
          m_repeat_forward,m_cvar_support];
       tpmodeswitches     : tmodeswitches=
         [m_tp,m_all,m_tp_procvar];
       gpcmodeswitches    : tmodeswitches=
         [m_gpc,m_all];

    var
       { specified inputfile }
       inputdir       : dirstr;
       inputfile      : namestr;
       inputextension : extstr;
       { specified outputfile with -o parameter }
       outputfile     : namestr;
       { specified with -FE or -FU }
       outputexedir   : dirstr;
       outputunitdir  : dirstr;

       { directory where the utils can be found (options -FD) }
       utilsdirectory : dirstr;

       { some flags for global compiler switches }
       do_build,
       do_make       : boolean;
       not_unit_proc : boolean;
       { path for searching units, different paths can be seperated by ; }
       exepath            : dirstr;  { Path to ppc }
       unitsearchpath,
       objectsearchpath,
       includesearchpath  : string;

       { deffile }
       usewindowapi  : boolean;
       description   : string;

       { current position }
       token,                        { current token being parsed }
       idtoken    : ttoken;          { holds the token if the pattern is a known word }
       tokenpos,                     { last postion of the read token }
       aktfilepos : tfileposinfo;    { current position }

       { type of currently parsed block }
       { isn't full implemented (FK)    }
       block_type : tblock_type;

       in_args : boolean;                { arguments must be checked especially }
       parsing_para_level : longint;     { parameter level, used to convert
                                             proc calls to proc loads in firstcalln }
       Must_be_valid : boolean;          { should the variable already have a value }
       compile_level : word;
       make_ref : boolean;
       use_esp_stackframe : boolean;     { to test for call with ESP as stack frame }

{$ifdef TP}
       use_big      : boolean;
{$endif}

     { commandline values }
       initdefines        : tlinkedlist;
       initglobalswitches : tglobalswitches;
       initmoduleswitches : tmoduleswitches;
       initlocalswitches  : tlocalswitches;
       initmodeswitches   : tmodeswitches;
       initpackenum,
       initpackrecords    : longint;
       initoutputformat   : tasm;
       initoptprocessor   : tprocessors;
       initasmmode        : tasmmode;
     { current state values }
       aktglobalswitches : tglobalswitches;
       aktmoduleswitches : tmoduleswitches;
       aktlocalswitches  : tlocalswitches;
       aktmodeswitches   : tmodeswitches;
       aktpackenum,
       aktpackrecords    : longint;
       aktoutputformat   : tasm;
       aktoptprocessor   : tprocessors;
       aktasmmode        : tasmmode;

     { Memory sizes }
       heapsize,
       maxheapsize,
       stacksize    : longint;

{$Ifdef EXTDEBUG}
       total_of_firstpass,
       firstpass_several : longint;
{$EndIf EXTDEBUG}
     { parameter switches }
{$Ifdef EXTDEBUG}
       debugstop,
       only_one_pass : boolean;
{$EndIf EXTDEBUG}
       { windows application type }
       apptype : tapptype;

    const
       RelocSection : boolean = false;
       DLLsource : boolean = false;
       { no binding needed for win32
         .edata written directly !! PM }
       bind_win32_dll : boolean = false;

       { should we allow non static members ? }
       allow_only_static : boolean = false;

       Inside_asm_statement : boolean = false;

    { for error info in pp.pas }
    const
       parser_current_file : string = '';

{$ifdef debug}
    { if the pointer don't point to the heap then write an error }
    function assigned(p : pointer) : boolean;
{$endif}
    function min(a,b : longint) : longint;
    function max(a,b : longint) : longint;
    function align(i,a:longint):longint;
    procedure Replace(var s:string;const s1,s2:string);
    function upper(const s : string) : string;
    function lower(const s : string) : string;
    function trimspace(const s:string):string;
    procedure uppervar(var s : string);
    function tostr(i : longint) : string;
    function tostr_with_plus(i : longint) : string;
    procedure valint(S : string;var V : longint;var code : integer);
    function is_number(const s : string) : boolean;
    function ispowerof2(value : longint;var power : longint) : boolean;
    { enable ansistring comparison }
    function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;
    function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;
    function bstoslash(const s : string) : string;
    procedure abstract;

    function getdatestr:string;
    function gettimestr:string;
    function filetimestring( t : longint) : string;

    function path_absolute(const s : string) : boolean;
    Function FileExists ( Const F : String) : Boolean;
    Function RemoveFile(const f:string):boolean;
    Function RemoveDir(d:string):boolean;
    Function GetFileTime ( Var F : File) : Longint;
    Function GetNamedFileTime ( Const F : String) : Longint;
    Function SplitFileName(const s:string):string;
    Function SplitName(const s:string):string;
    Function SplitExtension(Const HStr:String):String;
    Function AddExtension(Const HStr,ext:String):String;
    Function ForceExtension(Const HStr,ext:String):String;
    Function FixPath(s:string;allowdot:boolean):string;
    function FixFileName(const s:string):string;
    procedure AddPathToList(var list:string;s:string;first:boolean);
    function search(const f : string;path : string;var b : boolean) : string;
    procedure SynchronizeFileTime(const fn1,fn2:string);
    function FindExe(bin:string;var found:boolean):string;

   procedure InitGlobals;
   procedure DoneGlobals;

    procedure strdispose(var p : pchar);

  implementation

    procedure strdispose(var p : pchar);

      begin
         if assigned(p) then
           begin
              freemem(p,strlen(p)+1);
              p:=nil;
           end;
      end;


    function ngraphsearchvalue(const s1,s2 : string) : double;
      const
         n = 3;
      var
         equals,i,j : longint;
         hs : string;
      begin
         equals:=0;
         { is the string long enough ? }
         if min(length(s1),length(s2))-n+1<1 then
           begin
              ngraphsearchvalue:=0.0;
              exit;
           end;
         for i:=1 to length(s1)-n+1 do
           begin
              hs:=copy(s1,i,n);
              for j:=1 to length(s2)-n+1 do
                if hs=copy(s2,j,n) then
                  inc(equals);
           end;
{$ifdef fpc}
         ngraphsearchvalue:=equals/double(max(length(s1),length(s2))-n+1);
{$else}
         ngraphsearchvalue:=equals/(max(length(s1),length(s2))-n+1);
{$endif}
      end;


    function bstoslash(const s : string) : string;
    {
      return string s with all \ changed into /
    }
      var
         i : longint;
      begin
        for i:=1to length(s) do
         if s[i]='\' then
          bstoslash[i]:='/'
         else
          bstoslash[i]:=s[i];
         {$ifndef TP}
           {$ifopt H+}
             setlength(bstoslash,length(s));
           {$else}
             bstoslash[0]:=s[0];
           {$endif}
         {$else}
           bstoslash[0]:=s[0];
         {$endif}
      end;

{$ifdef debug}

    function assigned(p : pointer) : boolean;

{$ifndef FPC}
    {$ifndef DPMI}
      var
         lp : longint;
    {$endif DPMI}
{$endif FPC}
      begin
{$ifdef FPC}
          { Assigned is used for procvar and
            stack stored temp records !! PM }
         (* if (p<>nil) {and
            ((p<heaporg) or
            (p>heapptr))} then
           runerror(230); *)
{$else}
    {$ifdef DPMI}
         assigned:=(p<>nil);
         exit;
    {$else DPMI}
         if p=nil then
           lp:=0
         else
           lp:=longint(ptrrec(p).seg)*16+longint(ptrrec(p).ofs);
         if (lp<>0) and
            ((lp<longint(seg(heaporg^))*16+longint(ofs(heaporg^))) or
            (lp>longint(seg(heapptr^))*16+longint(ofs(heapptr^)))) then
           runerror(230);
    {$endif DPMI}
{$endif FPC}
         assigned:=(p<>nil);
      end;
{$endif}


    function min(a,b : longint) : longint;
    {
      return the minimal of a and b
    }
      begin
         if a>b then
           min:=b
         else
           min:=a;
      end;


    function max(a,b : longint) : longint;
    {
      return the maximum of a and b
    }
      begin
         if a<b then
           max:=b
         else
           max:=a;
      end;


    function align(i,a:longint):longint;
    {
      return value <i> aligned <a> boundary
    }
      begin
        align:=(i+a-1) and not(a-1);
      end;


    procedure Replace(var s:string;const s1,s2:string);
    {
      replace all s1 with s2 in string s
    }
      var
         i  : longint;
      begin
        repeat
          i:=pos(s1,s);
          if i>0 then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
           end;
        until i=0;
      end;


    function upper(const s : string) : string;
    {
      return uppercased string of s
    }
      var
         i  : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           upper[i]:=char(byte(s[i])-32)
          else
           upper[i]:=s[i];
         {$ifdef FPC}
           {$ifopt H+}
             setlength(upper,length(s));
           {$else}
             upper[0]:=s[0];
           {$endif}
         {$else}
           upper[0]:=s[0];
         {$endif}
      end;


    function lower(const s : string) : string;
    {
      return lowercased string of s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['A'..'Z'] then
           lower[i]:=char(byte(s[i])+32)
          else
           lower[i]:=s[i];
         {$ifndef TP}
           {$ifopt H+}
             setlength(lower,length(s));
           {$else}
             lower[0]:=s[0];
           {$endif}
         {$else}
           lower[0]:=s[0];
         {$endif}
      end;


    procedure uppervar(var s : string);
    {
      uppercase string s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           s[i]:=char(byte(s[i])-32);
      end;


   function trimspace(const s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimspace:=Copy(s,j,i-j+1);
     end;


   function tostr(i : longint) : string;
   {
     return string of value i
   }
     var
        hs : string;
     begin
        str(i,hs);
        tostr:=hs;
     end;


   function tostr_with_plus(i : longint) : string;
   {
     return string of value i, but always include a + when i>=0
   }
     var
        hs : string;
     begin
        str(i,hs);
        if i>=0 then
          tostr_with_plus:='+'+hs
        else
          tostr_with_plus:=hs;
     end;


    procedure valint(S : string;var V : longint;var code : integer);
    {
      val() with support for octal, which is not supported under tp7
    }
{$ifndef FPC}
      var
        vs : longint;
        c  : byte;
      begin
        if s[1]='%' then
          begin
             vs:=0;
             longint(v):=0;
             for c:=2 to length(s) do
               begin
                  if s[c]='0' then
                    vs:=vs shl 1
                  else
                  if s[c]='1' then
                    vs:=vs shl 1+1
                  else
                    begin
                      code:=c;
                      exit;
                    end;
               end;
             code:=0;
             longint(v):=vs;
          end
        else
         system.val(S,V,code);
      end;
{$else not FPC}
      begin
         system.val(S,V,code);
      end;
{$endif not FPC}


    function is_number(const s : string) : boolean;
    {
      is string a correct number ?
    }
      var
         w : integer;
         l : longint;
      begin
         valint(s,l,w);
         is_number:=(w=0);
      end;


    function ispowerof2(value : longint;var power : longint) : boolean;
    {
      return if value is a power of 2. And if correct return the power
    }
      var
         hl : longint;
         i : longint;
      begin
         hl:=1;
         ispowerof2:=true;
         for i:=0 to 31 do
           begin
              if hl=value then
                begin
                   power:=i;
                   exit;
                end;
              hl:=hl shl 1;
           end;
         ispowerof2:=false;
      end;


    { enable ansistring comparison }
    { 0 means equal }
    { 1 means p1 > p2 }
    { -1 means p1 < p2 }
    function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;

      var
         i,j : longint;
      begin
         compareansistrings:=0;
         j:=min(length1,length2);
         for i:=1  to j do
           begin
              if p1[i]>p2[i] then
                begin
                   compareansistrings:=1;
                   exit;
                end
              else if p1[i]<p2[i] then
                begin
                   compareansistrings:=-1;
                   exit;
                end;
           end;
         if length1>length2 then
          compareansistrings:=1
         else
          if length1<length2 then
           compareansistrings:=-1;
      end;


    function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;
      var
         p : pchar;
      begin
         getmem(p,length1+length2+1);
         move(p1[0],p[0],length1);
         move(p2[0],p[length1],length2+1);
         concatansistrings:=p;
      end;


{****************************************************************************
                               Time Handling
****************************************************************************}

    Function L0(l:longint):string;
    {
      return the string of value l, if l<10 then insert a zero, so
      the string is always at least 2 chars '01','02',etc
    }
      var
        s : string;
      begin
        Str(l,s);
        if l<10 then
         s:='0'+s;
        L0:=s;
      end;


   function gettimestr:string;
   {
     get the current time in a string HH:MM:SS
   }
      var
        hour,min,sec,hsec : word;
      begin
{$ifdef delphi}
        dmisc.gettime(hour,min,sec,hsec);
{$else delphi}
        dos.gettime(hour,min,sec,hsec);
{$endif delphi}
        gettimestr:=L0(Hour)+':'+L0(min)+':'+L0(sec);
      end;


   function getdatestr:string;
   {
     get the current date in a string YY/MM/DD
   }
      var
        Year,Month,Day,Wday : Word;
      begin
{$ifdef delphi}
        dmisc.getdate(year,month,day,wday);
{$else}
        dos.getdate(year,month,day,wday);
{$endif}
        getdatestr:=L0(Year)+'/'+L0(Month)+'/'+L0(Day);
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
     {$ifndef linux}
       DT : DateTime;
     {$endif}
       Year,Month,Day,Hour,Min,Sec : Word;
     begin
       if t=-1 then
        begin
          FileTimeString:='Not Found';
          exit;
        end;
     {$ifndef linux}
       unpacktime(t,DT);
       Year:=dT.year;month:=dt.month;day:=dt.day;
       Hour:=dt.hour;min:=dt.min;sec:=dt.sec;
     {$else}
       EpochToLocal (t,year,month,day,hour,min,sec);
     {$endif}
       filetimestring:=L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
     end;


{****************************************************************************
                               File Handling
****************************************************************************}

   function path_absolute(const s : string) : boolean;
   {
     is path s an absolute path?
   }
     begin
        path_absolute:=false;
{$ifdef linux}
        if (length(s)>0) and (s[1]='/') then
          path_absolute:=true;
{$else linux}
  {$ifdef amiga}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or (Pos(':',s) = length(s)) then
          path_absolute:=true;
  {$else}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or
           ((length(s)>2) and (s[2]=':') and ((s[3]='\') or (s[3]='/'))) then
          path_absolute:=true;
  {$endif amiga}
{$endif linux}
     end;

{$ifndef FPC}
    Procedure FindClose(var Info : SearchRec);
      Begin
      End;
{$endif not FPC}
{$ifdef delphi}
    Function FileExists ( Const F : String) : Boolean;

      begin
         FileExists:=sysutils.FileExists(f);
      end;

{$else}
    Function FileExists ( Const F : String) : Boolean;
      Var
      {$ifdef linux}
         Info : Stat;
      {$else}
         Info : SearchRec;
      {$endif}
      begin
      {$ifdef linux}
        FileExists:=FStat(F,info);
      {$else}
        findfirst(F,readonly+archive+hidden,info);
        FileExists:=(doserror=0);
        findclose(Info);
      {$endif}
      end;
{$endif}


    Function RemoveFile(const f:string):boolean;
      var
        g : file;
      begin
        assign(g,f);
        {$I-}
         erase(g);
        {$I+}
        RemoveFile:=(ioresult=0);
      end;


    Function RemoveDir(d:string):boolean;
      begin
        if d[length(d)]=DirSep then
         Delete(d,length(d),1);
        {$I-}
         rmdir(d);
        {$I+}
        RemoveDir:=(ioresult=0);
      end;


    Function SplitFileName(const s:string):string;
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
        FSplit(s,p,n,e);
        SplitFileName:=n+e;
      end;


    Function SplitName(const s:string):string;
      var
        i,j : longint;
      begin
        i:=Length(s);
        j:=Length(s);
        while (i>0) and not(s[i] in ['/','\']) do
         dec(i);
        while (j>0) and (s[j]<>'.') do
         dec(j);
        if j<=i then
         j:=255;
        SplitName:=Copy(s,i+1,j-(i+1));
      end;


    Function SplitExtension(Const HStr:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         begin
           if hstr[j]=DirSep then
            j:=0
           else
            dec(j);
         end;
        if j=0 then
         j:=254;
        SplitExtension:=Copy(Hstr,j,255);
      end;


    Function AddExtension(Const HStr,ext:String):String;
      begin
        if (Ext<>'') and (SplitExtension(HStr)='') then
         AddExtension:=Hstr+Ext
        else
         AddExtension:=Hstr;
      end;


    Function ForceExtension(Const HStr,ext:String):String;
      var
        j : longint;
      begin
        j:=length(Hstr);
        while (j>0) and (Hstr[j]<>'.') do
         dec(j);
        if j=0 then
         j:=255;
        ForceExtension:=Copy(Hstr,1,j-1)+Ext;
      end;


    Function FixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=DirSep;
        if (length(s)>0) and (s[length(s)]<>DirSep) and
           (s[length(s)]<>':') then
         s:=s+DirSep;
        if (not allowdot) and (s='.'+DirSep) then
         s:='';
        FixPath:=s;
      end;


   function FixFileName(const s:string):string;
     var
       i      : longint;
       NoPath : boolean;
     begin
       NoPath:=true;
       for i:=length(s) downto 1 do
        begin
          case s[i] of
      {$ifdef Linux}
       '/','\' : begin
                   FixFileName[i]:='/';
                   NoPath:=false; {Skip lowercasing path: 'X11'<>'x11' }
                 end;
      'A'..'Z' : if NoPath then
                  FixFileName[i]:=char(byte(s[i])+32)
                 else
                  FixFileName[i]:=s[i];
      {$else}
           '/' : FixFileName[i]:='\';
      'A'..'Z' : FixFileName[i]:=char(byte(s[i])+32);
      {$endif}
          else
           FixFileName[i]:=s[i];
          end;
        end;
       {$ifndef TP}
         {$ifopt H+}
           SetLength(FixFileName,length(s));
         {$else}
           FixFileName[0]:=s[0];
         {$endif}
       {$else}
         FixFileName[0]:=s[0];
       {$endif}
     end;


   procedure AddPathToList(var list:string;s:string;first:boolean);
     var
       LastAdd,
       starti,i,j : longint;
       Found    : boolean;
       CurrentDir,
       CurrPath,
       AddList  : string;
     begin
       if s='' then
        exit;
     {Fix List}
       if (length(list)>0) and (list[length(list)]<>';') then
        list:=list+';';
       GetDir(0,CurrentDir);
       CurrentDir:=FixPath(CurrentDir,false);
       AddList:='';
       LastAdd:=1;
       repeat
         j:=Pos(';',s);
         if j=0 then
          j:=255;
       {Get Pathname}
         CurrPath:=FixPath(Copy(s,1,j-1),false);
         if CurrPath='' then
          CurrPath:='.'+DirSep+';'
         else
          begin
            CurrPath:=FixPath(FExpand(CurrPath),false)+';';
            if (Copy(CurrPath,1,length(CurrentDir))=CurrentDir) then
             CurrPath:='.'+DirSep+Copy(CurrPath,length(CurrentDir)+1,255);
          end;
         Delete(s,1,j);
       {Check if already in path}
         found:=false;
         i:=0;
         starti:=1;
         while (not found) and (i<length(list)) do
          begin
            inc(i);
            if (list[i]=';') then
             begin
               found:=(CurrPath=Copy(List,starti,i-starti+1));
               if Found then
                begin
                  if First then
                   Delete(List,Starti,i-starti+1); {The new entry is placed first}
                end
               else
                starti:=i+1;
             end;
          end;
         if First then
          begin
            Insert(CurrPath,List,LastAdd);
            inc(LastAdd,Length(CurrPath));
          end
         else
          if not Found then
           List:=List+CurrPath
       until (s='');
     end;


   function search(const f : string;path : string;var b : boolean) : string;
      Var
        singlepathstring : string;
        i : longint;
     begin
     {$ifdef linux}
       for i:=1 to length(path) do
        if path[i]=':' then
       path[i]:=';';
     {$endif}
       b:=false;
       search:='';
       repeat
         i:=pos(';',path);
         if i=0 then
           i:=255;
         singlepathstring:=FixPath(copy(path,1,i-1),false);
         delete(path,1,i);
         If FileExists (singlepathstring+f) then
           begin
             Search:=singlepathstring;
             b:=true;
             exit;
           end;
       until path='';
     end;


   Function GetFileTime ( Var F : File) : Longint;
   Var
   {$ifdef linux}
     Info : Stat;
   {$endif}
     L : longint;
   begin
   {$ifdef linux}
     FStat (F,Info);
     L:=Info.Mtime;
   {$else}
     GetFTime(f,l);
   {$endif}
     GetFileTime:=L;
   end;


   Function GetNamedFileTime (Const F : String) : Longint;
   var
     L : Longint;
   {$ifndef linux}
     info : SearchRec;
   {$else}
     info : stat;
   {$endif}
   begin
     l:=-1;
   {$ifdef linux}
     if FStat (F,Info) then
      L:=info.mtime;
   {$else}
{$ifdef delphi}
     dmisc.FindFirst (F,archive+readonly+hidden,info);
{$else delphi}
     FindFirst (F,archive+readonly+hidden,info);
{$endif delphi}
     if DosError=0 then
      l:=info.time;
     {$ifdef Linux}
       FindClose(info);
     {$endif}
     {$ifdef Win32}
       FindClose(info);
     {$endif}
   {$endif}
     GetNamedFileTime:=l;
   end;


   {Touch Assembler and object time to ppu time is there is a ppufilename}
   procedure SynchronizeFileTime(const fn1,fn2:string);
   var
     f : file;
     l : longint;
   begin
     Assign(f,fn1);
     {$I-}
      reset(f,1);
     {$I+}
     if ioresult=0 then
      begin
        getftime(f,l);
        close(f);
        assign(f,fn2);
        {$I-}
         reset(f,1);
        {$I+}
        if ioresult=0 then
         begin
           setftime(f,l);
           close(f);
         end;
      end;
   end;

   function FindExe(bin:string;var found:boolean):string;
   begin
     bin:=FixFileName(bin)+source_os.exeext;
{$ifdef delphi}
     FindExe:=Search(bin,'.;'+exepath+';'+dmisc.getenv('PATH'),found)+bin;
{$else delphi}
     FindExe:=Search(bin,'.;'+exepath+';'+dos.getenv('PATH'),found)+bin;
{$endif delphi}
   end;

   procedure abstract;
     begin
        runerror(255);
     end;


 {****************************************************************************
                                    Init
 ****************************************************************************}

   procedure get_exepath;
     var
       hs1 : namestr;
       hs2 : extstr;
     begin
{$ifdef delphi}
       exepath:=dmisc.getenv('PPC_EXEC_PATH');
{$else delphi}
       exepath:=dos.getenv('PPC_EXEC_PATH');
{$endif delphi}
       if exepath='' then
        fsplit(FixFileName(paramstr(0)),exepath,hs1,hs2);
     {$ifdef linux}
       if exepath='' then
        fsearch(hs1,dos.getenv('PATH'));
     {$endif}
       exepath:=FixPath(exepath,false);
     end;



   procedure DoneGlobals;
     begin
        initdefines.done;
     end;

   procedure InitGlobals;
     begin
      { set global switches }
        do_build:=false;
        do_make:=true;
{$ifdef tp}
        use_big:=false;
{$endif tp}

      { Output }
        OutputFile:='';
        OutputExeDir:='';
        OutputUnitDir:='';

        { Utils directory }
        utilsdirectory:='';

      { Def file }
        usewindowapi:=false;
        description:='Compiled by FPC '+version_string+' - '+target_cpu_string;

      { Init values }
{$ifdef i386}
        initoptprocessor:=Class386;
        initlocalswitches:=[];
        initmoduleswitches:=[cs_extsyntax,cs_browser];
        initglobalswitches:=[cs_check_unit_name];
        initmodeswitches:=fpcmodeswitches;
        initpackenum:=4;
        initpackrecords:=2;
        initoutputformat:=target_asm.id;
        initasmmode:=asmmode_i386_att;
        initdefines.init;
{$else not i386}
  {$ifdef m68k}
        initoptprocessor:=MC68000;
        initlocalswitches:=[];
        initmoduleswitches:=[cs_extsyntax,cs_browser,cs_fp_emulation];
        initglobalswitches:=[cs_check_unit_name];
        initmodeswitches:=fpcmodeswitches;
        initpackenum:=4;
        initpackrecords:=2;
        initoutputformat:=as_m68k_as;
        initasmmode:=asmmode_m68k_mot;
        initdefines.init;
  {$endif m68k}
{$endif i386}

      { memory sizes, will be overriden by parameter or default for target
        in options or init_parser }
        stacksize:=0;
        heapsize:=0;
        maxheapsize:=0;

      { compile state }
        in_args:=false;
        must_be_valid:=true;
        not_unit_proc:=true;

        apptype:=at_cui;
     end;

begin
  get_exepath;
end.
{
  $Log$
  Revision 1.8.2.1  1999-07-07 07:53:21  michael
  + Merged patches from florian

  Revision 1.10  1999/07/06 21:48:16  florian
    * a lot bug fixes:
       - po_external isn't any longer necessary for procedure compatibility
       - m_tp_procvar is in -Sd now available
       - error messages of procedure variables improved
       - return values with init./finalization fixed
       - data types with init./finalization aren't any longer allowed in variant
         record

  Revision 1.9  1999/07/03 00:29:48  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.8  1999/05/27 19:44:29  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.7  1999/05/13 21:59:26  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.6  1999/05/05 10:05:50  florian
    * a delphi compiled compiler recompiles ppc

  Revision 1.5  1999/05/04 21:44:43  florian
    * changes to compile it with Delphi 4.0

  Revision 1.4  1999/04/26 13:31:32  peter
    * release storenumber,double_checksum

  Revision 1.3  1999/04/21 14:12:55  peter
    * default asm changed to att

  Revision 1.2  1999/04/16 09:56:05  pierre
   * unused local var commented

  Revision 1.1  1999/04/08 09:14:46  michael
  + Re-added;

  Revision 1.119  1999/04/07 14:15:53  pierre
   * assigned test for FPC removed, problems with stack variables

  Revision 1.118  1999/03/17 22:23:17  florian
    * a FPC compiled compiler checks now also in debug mode in assigned
      if a pointer points to the heap
    * when a symtable is loaded, there is no need to check for duplicate
      symbols. This leads to crashes because defowner isn't assigned
      in this case

  Revision 1.117  1999/03/04 13:55:42  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.116  1999/03/02 18:20:09  peter
    * fixed compareansistring which gave always -1 or 1 and never 0 :)

  Revision 1.115  1999/03/01 15:43:47  peter
    * synchronize also the objfile for ag386bin

  Revision 1.114  1999/02/25 21:02:36  peter
    * ag386bin updates
    + coff writer

  Revision 1.113  1999/02/22 13:06:50  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.111  1999/02/11 09:46:22  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.110  1999/01/27 13:05:42  pierre
   * give include file name on error

  Revision 1.109  1999/01/22 16:56:50  pierre
   * findclose problem fixed

  Revision 1.108  1999/01/22 10:08:19  daniel
  * Findclose is removed for go32v1 and go32v2 platforms, because this is
    TP incompatible. It is now only called for Linux and Win32.

  Revision 1.107  1999/01/12 14:25:27  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.106  1999/01/05 08:19:57  florian
    * mainly problem with invalid case ranges fixed (reported by Jonas)

  Revision 1.105  1998/12/28 23:26:16  peter
    + resource file handling ($R directive) for Win32

  Revision 1.104  1998/12/23 22:49:42  peter
    * forgot one conflict :(

  Revision 1.103  1998/12/23 22:48:41  peter
    * fixed findclose problem

  Revision 1.102  1998/12/23 14:26:30  jonas
    * patch from Peter: remove FindClose call in FileExists (caused error under Dos
      when opening the ppc386.cfg file)

  Revision 1.101  1998/12/23 14:02:00  peter
    * daniels patches against the latest versions

  Revision 1.99  1998/12/19 00:23:47  florian
    * ansistring memory leaks fixed

  Revision 1.98  1998/12/15 10:23:24  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.97  1998/12/11 00:03:17  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.96  1998/12/08 10:18:06  peter
    + -gh for heaptrc unit

  Revision 1.95  1998/12/03 10:17:30  peter
    * target_os.use_bound_instruction boolean

  Revision 1.94  1998/11/30 13:26:22  pierre
    * the code for ordering the exported procs/vars was buggy
    + added -WB to force binding (Ozerski way of creating DLL)
      this is off by default as direct writing of .edata section seems
      OK

  Revision 1.93  1998/11/30 09:43:10  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.92  1998/11/27 14:50:37  peter
    + open strings, $P switch support

  Revision 1.91  1998/11/26 21:33:08  peter
    * rangecheck updates

  Revision 1.90  1998/11/24 17:03:23  peter
    * removed obsolete version.inc

  Revision 1.89  1998/11/16 15:41:41  peter
    * tp7 didn't like my ifopt H+ :(

  Revision 1.88  1998/11/16 10:17:07  peter
    * fixed for H+ compilation

  Revision 1.87  1998/11/15 16:32:36  florian
    * some stuff of Pavel implement (win32 dll creation)
    * bug with ansistring function results fixed

  Revision 1.86  1998/11/05 12:02:43  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.85  1998/10/26 22:23:30  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.84  1998/10/25 23:30:15  peter
    * valint fix for tp7 to overcome overflow

  Revision 1.83  1998/10/22 17:54:02  florian
    + switch $APPTYPE for win32 added

  Revision 1.82  1998/10/22 11:56:44  pierre
    * FixPath handling of c: corrected

  Revision 1.81  1998/10/19 15:41:00  peter
    * better splitname to support glib-1.1.dll alike names

  Revision 1.80  1998/10/16 13:37:17  florian
    + switch -FD added to specify the path for utilities

  Revision 1.79  1998/10/16 08:51:46  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.78  1998/10/14 13:38:21  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.77  1998/10/13 14:01:09  peter
    * fixed -al

  Revision 1.76  1998/10/13 13:10:15  peter
    * new style for m68k/i386 infos and enums

  Revision 1.75  1998/10/13 08:19:32  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionnals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.74  1998/10/12 10:28:29  florian
    + auto dereferencing of pointers to structured types in delphi mode

  Revision 1.73  1998/10/12 09:49:56  florian
    + support of <procedure var type>:=<pointer> in delphi mode added

  Revision 1.72  1998/10/06 17:16:48  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.71  1998/09/30 19:53:54  michael
  + Upgraded to version 0.99.9

  Revision 1.70  1998/09/29 15:23:03  peter
    * remove also the end files for smartlinking

  Revision 1.69  1998/09/26 17:45:28  peter
    + idtoken and only one token table

  Revision 1.68  1998/09/24 23:49:04  peter
    + aktmodeswitches

  Revision 1.67  1998/09/22 17:13:46  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.66  1998/09/22 15:40:54  peter
    * some extra ifdef GDB

  Revision 1.65  1998/09/18 16:03:38  florian
    * some changes to compile with Delphi

  Revision 1.64  1998/09/10 15:25:29  daniel
  + Added maxheapsize.
  * Corrected semi-bug in calling the assembler and the linker

  Revision 1.63  1998/09/09 18:17:13  florian
    * version number changed to 0.99.8

  Revision 1.62  1998/09/07 17:36:59  florian
    * first fixes for published properties

  Revision 1.61  1998/09/03 11:21:52  peter
    * -al sets cs_asm_source

  Revision 1.60  1998/09/01 12:53:20  peter
    + aktpackenum

  Revision 1.59  1998/09/01 07:54:18  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.58  1998/08/31 12:26:25  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.57  1998/08/29 13:51:09  peter
    * moved get_exepath to globals
    + date_string const with the current date for 0.99.7+

  Revision 1.56  1998/08/26 15:35:31  peter
    * fixed scannerfiles for macros
    + $I %<environment>%

  Revision 1.55  1998/08/25 12:42:35  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.54  1998/08/19 18:04:53  peter
    * fixed current_module^.in_implementation flag

  Revision 1.53  1998/08/19 16:07:45  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.52  1998/08/19 10:06:13  peter
    * fixed filenames and removedir which supports slash at the end

  Revision 1.51  1998/08/17 09:17:46  peter
    * static/shared linking updates

  Revision 1.50  1998/08/14 21:56:33  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.49  1998/08/13 10:57:31  peter
    * constant sets are now written correctly to the ppufile

  Revision 1.48  1998/08/11 15:31:37  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.47  1998/08/10 14:49:59  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.46  1998/08/10 10:18:25  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.45  1998/07/24 22:16:56  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.44  1998/07/18 17:11:08  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.43  1998/07/14 21:46:42  peter
    * updated messages file

  Revision 1.42  1998/07/08 14:28:35  daniel
  * Fixed small TP incompatibility: Fsplit requires use of dirstr, namestr and
  extstr

  Revision 1.41  1998/07/07 11:19:56  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.40  1998/06/25 08:48:13  florian
    * first version of rtti support

  Revision 1.39  1998/06/17 14:10:12  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.38  1998/06/16 08:56:21  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.37  1998/06/13 00:10:06  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.36  1998/06/12 16:15:31  pierre
    * external name 'C_var';
      export name 'intern_C_var';
      cdecl;
      cdecl;external;
      are now supported only with -Sv switch

  Revision 1.34  1998/06/04 23:51:39  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.33  1998/06/03 22:48:54  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.32  1998/05/30 14:31:04  peter
    + $ASMMODE

  Revision 1.31  1998/05/28 14:40:24  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.30  1998/05/27 19:45:03  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.29  1998/05/25 17:11:39  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.28  1998/05/23 01:21:07  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.27  1998/05/20 09:42:33  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.26  1998/05/12 10:46:59  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.25  1998/05/11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.24  1998/05/08 09:21:20  michael
  * Added missing -Fl message to messages file.
  * Corrected mangling of file names when doing Linklib
  * -Fl now actually WORKS.
  * Librarysearchpath is now a field in linker object.

  Revision 1.23  1998/05/06 15:04:20  pierre
    + when trying to find source files of a ppufile
      check the includepathlist for included files
      the main file must still be in the same directory

  Revision 1.22  1998/05/06 08:38:39  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.21  1998/05/04 17:54:25  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.20  1998/05/01 07:43:53  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.19  1998/04/30 15:59:40  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.18  1998/04/29 10:33:52  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.17  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.16  1998/04/27 15:45:20  peter
    + -Xl for smartlink
    + target_info.arext = .a

  Revision 1.15  1998/04/22 21:06:50  florian
    * last fixes before the release:
      - veryyyy slow firstcall fixed

  Revision 1.14  1998/04/21 13:48:09  michael
  + Updated patch number

  Revision 1.13  1998/04/21 10:16:47  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.12  1998/04/09 14:28:06  jonas
    + basic k6 and 6x86 optimizing support (-O7 and -O8)

  Revision 1.11  1998/04/08 16:58:02  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.10  1998/04/08 11:34:22  peter
    * nasm works (linux only tested)

  Revision 1.9  1998/04/07 21:37:30  peter
    * fixed fixpath to also change / and \ slashes and better addpathtolist

  Revision 1.8  1998/04/07 13:19:44  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.7  1998/04/06 16:19:46  peter
    * fixed the -Up.. bug

}

