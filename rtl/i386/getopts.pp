{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit getopts;

{$I os.inc}

{ --------------------------------------------------------------------
  Getopt implementation for FPK pascal, modeled after GNU getopt.
  Tested under Linux.
  Tested under DOS
  Michael Van Canneyt, 1997

  *NOTE*
  The routines are a more or less straightforward conversion 
  of the GNU C implementation of getopt. One day they should be 
  replaced by some 'real pascal code'.
  --------------------------------------------------------------------
}

Interface

Const No_Argument       = 0;
      Required_Argument = 1;
      Optional_Argument = 2; 
      EndOfOptions      = #255;
    
Type Option = Record
       Name    : String;
       Has_arg : Integer;
       Flag    : ^char;
       Value   : Char;
      end;
     POption  = ^Option;
     Orderings = (require_order,permute,return_in_order);

Var OptArg : String;
    OptInd : Integer;
    OptErr : Boolean;
    OptOpt : Char;
      
Function GetOpt (ShortOpts : String) : char;
Function GetLongOpts (ShortOpts : String; 
                      LongOpts : POption; 
                      var Longind : Integer) : char;

Implementation

Var NextChar : integer;
    first_nonopt,last_nonopt,Nrargs : Integer;
    Ordering : orderings;
{$ifndef linux}
    argv : ^pchar;
{$endif}
{ Copied straight from strings.pp, avoids the 'uses strings'  }

function strpas(p : pchar) : string;

      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl %edi,%esi               
            movl $0xffffffff,%ecx        
            xorb %al,%al
            repne
            scasb
            notl %ecx
            decl %ecx
            movl 8(%ebp),%edi          
            movb %cl,%al
            stosb
            rep                         
            movsb                       
         end ['ECX','EAX','ESI','EDI'];
      end;

    
Procedure Exchange;

var bottom,middle,top,i,len : integer;
    temp : pchar;

begin
  bottom:=first_nonopt;
  middle:=last_nonopt;
  top:=optind;
  while (top>middle) and (middle>bottom) do
    begin
    if (top-middle>middle-bottom) then
      begin
      len:=middle-bottom;
      for i:=1 to len-1 do
        begin
        temp:=argv[bottom+i];
        argv[bottom+i]:=argv[top-(middle-bottom)+i];
        argv[top-(middle-bottom)+i]:=temp;
        end;
      top:=top-len;
      end
    else
      begin
      len:=top-middle;
      for i:=0 to len-1 do
        begin
        temp:=argv[bottom+i];
        argv[bottom+i]:=argv[middle+i];
        argv[middle+i]:=temp;
        end;
      bottom:=bottom+len;
      end;
    end;
  first_nonopt:=first_nonopt + optind-last_nonopt;
  last_nonopt:=optind;
end; { exchange }

procedure getopt_init (var opts : string);

begin
  { Initialize some defaults. }
  Optarg:='';
  Optind:=1;
  First_nonopt:=1;
  Last_nonopt:=1;
  OptOpt:='?';
  Nextchar:=0;
  if opts[1]='-' then 
    begin
    ordering:=return_in_order;
    delete(opts,1,1);
    end
  else if opts[1]='+' then
    begin
    ordering:=require_order;
    delete(opts,1,1);
    end
  else ordering:=permute;
end;
      
Function Internal_getopt (Var Optstring : string;
                          LongOpts : POption;
                          LongInd : pointer;
                          Long_only : boolean ) : char;

type pint=^integer; 

var temp,endopt,option_index : byte;
    indfound: integer;
    currentarg,optname : string;
    p,pfound : POption;
    exact,ambig : boolean;
    c : char; 
    
begin
optarg:='';
if optind=0 then getopt_init(optstring);
{ Check if We need the next argument. }
if optind<nrargs then currentarg:=strpas(argv[optind]) else currentarg:='';
if (nextchar=0) then 
  begin
  if ordering=permute then
    begin
    { If we processed options following non-options : exchange }
    if (first_nonopt<>last_nonopt) and (last_nonopt<>optind) then
      exchange
    else
      if last_nonopt<>optind then first_nonopt:=optind;
    while (optind<nrargs) and ((argv[optind][0]<>'-') 
                                or (length(strpas(argv[optind]))=1)) do
      begin
      inc(optind);
      end;
    last_nonopt:=optind;
    end;
  { Check for '--' argument }
  if optind<nrargs then currentarg:=strpas(argv[optind]) else currentarg:='';
  if (optind<>nrargs) and (currentarg='--') then
    begin
    inc(optind);
    if (first_nonopt<>last_nonopt) and (last_nonopt<>optind) then
      exchange
    else 
      if first_nonopt=last_nonopt then first_nonopt:=optind;
    last_nonopt:=nrargs;
    optind:=nrargs;
    end; 
  { Are we at the end of all arguments ? }
  if optind>=nrargs then
    begin
    if first_nonopt<>last_nonopt then
      optind:=first_nonopt;
    Internal_getopt:=EndOfOptions;
    exit;
    end;
  if optind<nrargs then currentarg:=strpas(argv[optind]) else currentarg:='';
  { Are we at a non-option ? }
  if (currentarg[1]<>'-') or (currentarg='-') then
    begin
    if ordering=require_order then
       begin
       Internal_getopt:=EndOfOptions;
       exit;
       end
    else
       begin
       optarg:=strpas(argv[optind]);
       inc(optind);
       Internal_getopt:=#1;
       exit;
       end;
    end;
  { At this point we're at an option ...}
  nextchar:=2;
  if (longopts<>nil) and (currentarg[2]='-') then inc(nextchar);
  { So, now nextchar points at the first character of an option }
  end;
{ Check if we have a long option }
if longopts<>nil then 
  if length(currentarg)>1 then
  if (currentarg[2]='-') or
    ((not long_only) and (pos(currentarg[2],optstring)<>0)) then
    begin
    { Get option name }
    endopt:=pos('=',currentarg);
    if endopt=0 then endopt:=length(currentarg)+1;
    optname:=copy(currentarg,nextchar,endopt-nextchar);
    { Match partial or full }
    p:=longopts;
    pfound:=nil;
    exact:=false;
    ambig:=false;
    option_index:=0;
    indfound:=0;
    while (p^.name<>'') and (not exact) do
      begin
      if pos(optname,p^.name)<>0 then
        begin
        if length(optname)=length(p^.name) then
          begin
          exact:=true;
          pfound:=p;
          indfound:=option_index;
          end
        else
          if pfound=nil then
            begin
            indfound:=option_index;
            pfound:=p
            end
          else
            ambig:=true;
        end;
      inc (longint(p),sizeof(option));
      inc (option_index);
      end;
    if ambig and not exact then 
      begin
      if opterr then
         writeln (paramstr(0),': option "',optname,'" is ambiguous');
      nextchar:=0;
      inc(optind);
      Internal_getopt:='?';
      end;
    if pfound<>nil then
      begin
      inc(optind);
      if endopt<=length(currentarg) then
        begin
        if pfound^.has_arg>0 then
          begin
          optarg:=copy(currentarg,endopt+1,length(currentarg)-endopt);
          end
        else
          begin
          if opterr then
            if currentarg[2]='-' then
              writeln (paramstr(0),': option "--',pfound^.name,'" doesn''t allow an argument')
            else  
              writeln (paramstr(0),': option "',currentarg[1],pfound^.name,'" doesn''t allow an argument');
          nextchar:=0;
          internal_getopt:='?';
          exit;
          end;
        end 
      else { argument in next paramstr...  } 
        begin
        if pfound^.has_arg=1 then
          begin
          if optind<nrargs then
            begin
            optarg:=strpas(argv[optind]);
            inc(optind);
            end { required argument }
          else
            begin { no req argument}
            if opterr then
              writeln (paramstr(0),': option ',pfound^.name,' requires an argument');
            nextchar:=0;
            if optstring[1]=':' then
              Internal_getopt:=':'
            else
              Internal_getopt:='?';
            exit;
            end;
          end; 
        end; { argument in next parameter end;}
        nextchar:=0;
      if longind<>nil then pint(longind)^:=indfound+1;
      if pfound^.flag<>nil then 
        begin
        pfound^.flag^:=pfound^.value;
        internal_getopt:=#0;
        exit
        end;
      internal_getopt:=pfound^.value;
      exit
      end; { pfound<>nil }
    { We didn't find it as an option }
    if (not long_only) or ((currentarg[2]='-') or 
                           (pos(CurrentArg[nextchar],optstring)=0)) then
      begin
      if opterr then
        if currentarg[2]='-' then
          writeln (paramstr(0),' unrecognized option "--',optname,'"')
        else
          writeln (paramstr(0),' unrecognized option "',currentarg[1],optname,'"');
      nextchar:=0;
      inc(optind);
      Internal_getopt:='?';
      exit;
      end;              
    end; { Of long options.}
{ We check for a short option. }
temp:=pos(currentarg[nextchar],optstring);
c:=currentarg[nextchar];
inc (nextchar);
if nextchar>length(currentarg) then 
  begin
  inc(optind);
  nextchar:=0;
  end;
if (temp=0) or (c=':') then
  begin
  if opterr then
    writeln (paramstr(0),': illegal option -- ',c);
  optopt:=currentarg[nextchar-1];
  internal_getopt:='?';
  exit;
  end;
Internal_getopt:=optstring[temp];
if optstring[temp+1]=':' then 
  if currentarg[temp+2]=':' then
    begin { optional argument }
      optarg:=copy (currentarg,nextchar,length(currentarg)-nextchar+1);
      nextchar:=0;
    end
  else
    begin { required argument }
    if nextchar>0 then 
      begin
      optarg:=copy (currentarg,nextchar,length(currentarg)-nextchar+1);
      inc(optind)
      end
    else if (optind=nrargs) then
      begin
      if opterr then
        writeln (paramstr(0),': option requires an argument -- ',optstring[temp]);
      optopt:=optstring[temp];
      if optstring[1]=':' then 
        Internal_getopt:=':'
      else
        Internal_Getopt:='?'  
      end
    else
      begin
      optarg:=strpas(argv[optind]);
      inc (optind)
      end;
    nextchar:=0;
    end; { End of required argument}
end; { End of internal getopt...}


Function GetOpt (ShortOpts : String) : char;

begin
  getopt:=internal_getopt (shortopts,nil,nil,false);
end;

Function GetLongOpts (ShortOpts : String; 
                      LongOpts : POption; 
                      var Longind : Integer) : char;
begin
  getlongopts:=internal_getopt ( shortopts,longopts,@longind,true);
end;

{$ifndef linux}
function args : pointer;

begin
  asm
  movl _args,%eax
  leave
  ret
  end ['EAX'];
end;
{$endif}                                                                                     
                                                                                     

begin
  { Needed to detect startup } 
  Opterr:=true;
  Optind:=0;
  nrargs:=paramcount+1;
{$ifndef linux}
  argv:=args;
{$endif}  
end.     
    
{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.3  1998/01/26 11:58:56  michael
  + Added log at the end


  
  Working file: rtl/i386/getopts.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:34:38;  author: michael;  state: Exp;  lines: +15 -2
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
