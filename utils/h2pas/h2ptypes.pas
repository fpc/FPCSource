unit h2ptypes;

// {$mode ObjFPC}
{$inline on}
{$modeswitch result}

interface

uses
  Classes, SysUtils;

type
   Char=system.ansichar;
   ttyp = (
      t_id,
      { p contains the string }
      t_arraydef,
      { }
      t_pointerdef,
      { p1 contains the definition
        if in type overrider
        or nothing for args
      }
      t_addrdef,

      t_void,
      { no field }
      t_dec,
      { }
      t_declist,
      { p1 is t_dec
        next if exists }
      t_memberdec,
      { p1 is type specifier
        p2 is declarator_list }
      t_structdef,
      { }
      t_memberdeclist,
      { p1 is memberdec
        next is next if it exist }
      t_procdef,
      { }
      t_uniondef,
      { }
      t_enumdef,
      { }
      t_enumlist,
      { }
      t_preop,
      { p contains the operator string
        p1 contains the right expr }
      t_bop,
      { p contains the operator string
        p1 contains the left expr
        p2 contains the right expr }
      t_arrayop,
      {
        p1 contains the array expr
        p2 contains the index expressions }
      t_callop,
      {
        p1 contains the proc expr
        p2 contains the index expressions }
      t_arg,
      {
        p1 contain the typedef
        p2 the declarator (t_dec)
      }
      t_arglist,
      { }
      t_funexprlist,
      { }
      t_exprlist,
      { p1 contains the expr
        next contains the next if it exists }
      t_ifexpr,
      { p1 contains the condition expr
        p2 contains the if branch
        p3 contains the else branch }
      t_funcname,
      { p1 contains the function dname
        p2 contains the funexprlist
        p3 possibly contains the return type }
      t_typespec,
      { p1 is the type itself
        p2 the typecast expr }
      t_size_specifier,
      { p1 expr for size }
      t_default_value,
      { p1 expr for value }
      t_statement_list,
      { p1 is the statement
        next is next if it exist }
      t_whilenode,
      t_fornode,
      t_dowhilenode,
      t_switchnode,
      t_gotonode,
      t_continuenode,
      t_breaknode
      );

const
  ttypstr: array[ttyp] of string =
  (
    't_id',
    't_arraydef',
    't_pointerdef',
    't_addrdef',
    't_void',
    't_dec',
    't_declist',
    't_memberdec',
    't_structdef',
    't_memberdeclist',
    't_procdef',
    't_uniondef',
    't_enumdef',
    't_enumlist',
    't_preop',
    't_bop',
    't_arrayop',
    't_callop',
    't_arg',
    't_arglist',
    't_funexprlist',
    't_exprlist',
    't_ifexpr',
    't_funcname',
    't_typespec',
    't_size_specifier',
    't_default_value',
    't_statement_list',
    't_whilenode',
    't_fornode',
    't_dowhilenode',
    't_switchnode',
    't_gotonode',
    't_continuenode',
    't_breaknode'
  );

type
  presobject = ^tresobject;
  tresobject = object
     typ : ttyp;
     p : pansichar;
     next : presobject;
     p1,p2,p3 : presobject;
     { name of int/real, then no T prefix is required }
     intname : boolean;
     constructor init_no(t : ttyp);
     constructor init_one(t : ttyp;_p1 : presobject);
     constructor init_two(t : ttyp;_p1,_p2 : presobject);
     constructor init_three(t : ttyp;_p1,_p2,_p3 : presobject);
     constructor init_id(const s : string);
     constructor init_intid(const s : string);
     constructor init_bop(const s : string;_p1,_p2 : presobject);
     constructor init_preop(const s : string;_p1 : presobject);
     procedure setstr(const s:string);
     function str : string;
     function strlength : byte;
     function get_copy : presobject;
     { can this ve considered as a constant ? }
     function is_const : boolean;
     destructor done;
  end;

  tblocktype = (bt_type,bt_const,bt_var,bt_func,bt_no);

Function NewUnaryOp(aop : string; aright : presobject) : presobject; inline;
Function NewBinaryOp(aop : string; aleft,aright : presobject) : presobject; inline;
Function NewVoid : presobject; inline;
Function NewID(aID : string) : presobject; inline;
Function NewType1(aType : ttyp; aID : presobject) : presobject; inline;
Function NewType2(aType : ttyp; aID,aID2 : presobject) : presobject; inline;
Function NewType3(aType : ttyp; aID,aID2,aID3 : presobject) : presobject; inline;
Function NewIntID(aIntID : string) : presobject; inline;
function strpnew(const s : ansistring) : pansichar; inline;

implementation

uses strings;


Function NewVoid : presobject;

begin
  Result:=new(presobject,init_no(t_void));
end;

Function NewBinaryOp(aop : string; aleft,aright : presobject) : presobject;

begin
  Result:=new(presobject,init_bop(aop,aleft,aright));
end;

Function NewUnaryOp(aop : string; aright : presobject) : presobject; inline;

begin
  Result:=new(presobject,init_preop(aop,aright));
end;


Function NewID(aID : string) : presobject;

begin
  Result:=new(presobject,init_id(aID));
end;

Function NewIntID(aIntID : string) : presobject;

begin
  Result:=new(presobject,init_intid(aIntID));
end;

Function NewType1(aType : ttyp; aID : presobject) : presobject; inline;

begin
  Result:=new(presobject,init_one(atype,aID));
end;

Function NewType2(aType : ttyp; aID,aID2 : presobject) : presobject; inline;

begin
  Result:=new(presobject,init_two(atype,aID,aID2));
end;

Function NewType3(aType : ttyp; aID,aID2,aID3 : presobject) : presobject; inline;

begin
  Result:=new(presobject,init_three(atype,aID,aID2,aID3));
end;

function strpnew(const s : ansistring) : pansichar;
var
  p : pansichar;
begin
 getmem(p,length(s)+1);
 strpcopy(p,s);
 strpnew:=p;
end;


constructor tresobject.init_preop(const s : string;_p1 : presobject);
  begin
     typ:=t_preop;
     p:=strpnew(s);
     p1:=_p1;
     p2:=nil;
     p3:=nil;
     next:=nil;
     intname:=false;
  end;

constructor tresobject.init_bop(const s : string;_p1,_p2 : presobject);
  begin
     typ:=t_bop;
     p:=strpnew(s);
     p1:=_p1;
     p2:=_p2;
     p3:=nil;
     next:=nil;
     intname:=false;
  end;

constructor tresobject.init_id(const s : string);
  begin
     typ:=t_id;
     p:=strpnew(s);
     p1:=nil;
     p2:=nil;
     p3:=nil;
     next:=nil;
     intname:=false;
  end;

constructor tresobject.init_intid(const s : string);
  begin
     typ:=t_id;
     p:=strpnew(s);
     p1:=nil;
     p2:=nil;
     p3:=nil;
     next:=nil;
     intname:=true;
  end;

constructor tresobject.init_two(t : ttyp;_p1,_p2 : presobject);
  begin
     typ:=t;
     p1:=_p1;
     p2:=_p2;
     p3:=nil;
     p:=nil;
     next:=nil;
     intname:=false;
  end;

constructor tresobject.init_three(t : ttyp;_p1,_p2,_p3 : presobject);
  begin
     typ:=t;
     p1:=_p1;
     p2:=_p2;
     p3:=_p3;
     p:=nil;
     next:=nil;
     intname:=false;
  end;

constructor tresobject.init_one(t : ttyp;_p1 : presobject);
  begin
     typ:=t;
     p1:=_p1;
     p2:=nil;
     p3:=nil;
     next:=nil;
     p:=nil;
     intname:=false;
  end;

constructor tresobject.init_no(t : ttyp);
  begin
     typ:=t;
     p:=nil;
     p1:=nil;
     p2:=nil;
     p3:=nil;
     next:=nil;
     intname:=false;
  end;

procedure tresobject.setstr(const s : string);
  begin
     if assigned(p) then
      strdispose(p);
     p:=strpnew(s);
  end;

function tresobject.str : string;
  begin
     str:=strpas(p);
  end;

function tresobject.strlength : byte;
  begin
     if assigned(p) then
       strlength:=strlen(p)
     else
       strlength:=0;
  end;

{ can this ve considered as a constant ? }
function tresobject.is_const : boolean;
  begin
     case typ of
       t_id,t_void :
         is_const:=true;
       t_preop  :
         is_const:= ((str='-') or (str=' not ')) and p1^.is_const;
       t_bop  :
         is_const:= p2^.is_const and p1^.is_const;
     else
       is_const:=false;
     end;
  end;

function tresobject.get_copy : presobject;
  var
     newres : presobject;
  begin
     newres:=new(presobject,init_no(typ));
     newres^.intname:=intname;
     if assigned(p) then
       newres^.p:=strnew(p);
     if assigned(p1) then
       newres^.p1:=p1^.get_copy;
     if assigned(p2) then
       newres^.p2:=p2^.get_copy;
     if assigned(p3) then
       newres^.p3:=p3^.get_copy;
     if assigned(next) then
       newres^.next:=next^.get_copy;
     get_copy:=newres;
  end;

destructor tresobject.done;
  begin
     (* writeln('disposing ',byte(typ)); *)
     if assigned(p)then strdispose(p);
     if assigned(p1) then
       dispose(p1,done);
     if assigned(p2) then
       dispose(p2,done);
     if assigned(p3) then
       dispose(p3,done);
     if assigned(next) then
       dispose(next,done);
  end;


end.

