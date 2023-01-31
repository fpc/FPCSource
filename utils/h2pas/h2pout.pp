unit h2pout;

interface

uses
  SysUtils, classes,
  h2poptions, h2pconst,h2plexlib,h2pyacclib, scanbase,h2ptypes;

procedure OpenOutputFiles;
procedure CloseTempFiles;

procedure WriteFileHeader(var headerfile: Text);
procedure WriteLibraryInitialization;

procedure write_statement_block(var outfile:text; p : presobject);
procedure write_type_specifier(var outfile:text; p : presobject);
procedure write_p_a_def(var outfile:text; p,simple_type : presobject);
procedure write_ifexpr(var outfile:text; p : presobject);
procedure write_funexpr(var outfile:text; p : presobject);
procedure write_def_params(var outfile:text; p : presobject);
procedure write_args(var outfile:text; p : presobject);
procedure write_packed_fields_info(var outfile:text; p : presobject; ph : string);
procedure write_expr(var outfile:text; p : presobject);

procedure emitignoreconst;
procedure emitignore(p : presobject);
procedure emitignoredefault(p : presobject);
procedure EmitAbstractIgnored;
procedure EmitWriteln(S : string);
procedure EmitPacked(aPack : integer);
procedure EmitAndOutput(S : string; aLine : integer);
procedure EmitErrorStart(S : string);

procedure shift(space_number : byte);
procedure popshift;
procedure resetshift;
function str(i : longint) : string;
function hexstr(i : cardinal) : string;
function uppercase(s : string) : string;
function PointerName(const s:string):string;
function IsACType(const s : String) : Boolean;
function NeedEllipsisOverload : Boolean;
function TypeName(const s:string):string;

Var
  No_pop   : boolean;
  implemfile  : text;  (* file for implementation headers extern procs *)
  in_args : boolean = false;
  old_in_args : boolean = false;
  must_write_packed_field : boolean;
  is_procvar : boolean = false;
  is_packed : boolean = false;
  if_nb : longint = 0;

implementation

var
  tempfile : text;
  space_array : array [0..255] of integer;
  space_index : integer;
  _NeedEllipsisOverload : boolean;
  typedef_level : longint = 0;

procedure EmitAndOutput(S : string; aLine : integer);

begin
  if yydebug then
    begin
    writeln(S,line_no);
    writeln(outfile,'(* ',S,' *)');
    end;
end;

procedure EmitErrorStart(S : string);

begin
  writeln(outfile,'(* error ');
  writeln(outfile,s);
end;

procedure EmitWriteln(S : string);

begin
  Writeln(outfile,S);
end;

procedure EmitPacked(aPack: integer);

var
  newpacked : boolean;

begin
  newpacked:=(aPack<>4);
  if (newpacked<>is_packed) and (not packrecords) then
    writeln(outfile,'{$PACKRECORDS ',aPack,'}');
  is_packed:=newpacked;
end;

procedure emitignoredefault(p : presobject);

begin
  if not stripinfo then
    writeln(outfile,'(* Warning : default value for ',p^.p,' ignored *)');
end;

procedure EmitAbstractIgnored;
begin
  if not stripinfo then
   writeln(outfile,'(* Const before abstract_declarator ignored *)');
end;

procedure emitignore(p : presobject);

begin
  if not stripinfo then
   writeln(outfile,aktspace,'(* ',p^.p,' ignored *)');
end;

procedure emitignoreconst;

begin
  if not stripinfo then
   writeln(outfile,'(* Const before declarator ignored *)');
end;

function NeedEllipsisOverload : Boolean;

begin
  NeedEllipsisOverload:=_NeedEllipsisOverload
end;



procedure shift(space_number : byte);

var
  i : byte;

begin
  space_array[space_index]:=space_number;
  inc(space_index);
  for i:=1 to space_number do
    aktspace:=aktspace+' ';
end;


procedure popshift;

begin
  dec(space_index);
  if space_index<0 then
    begin
    Writeln('Warning: atempt to decrease index below zero');
    space_index:=1;
    end
  else
    delete(aktspace,1,space_array[space_index]);
end;

procedure resetshift;
begin
  space_index:=1;
end;

function str(i : longint) : string;

var
  s : string;

begin
  system.str(i,s);
  str:=s;
end;


function hexstr(i : cardinal) : string;

const
  HexTbl : array[0..15] of char='0123456789ABCDEF';

var
  str : string;

begin
  str:='';
  while i<>0 do
  begin
    str:=hextbl[i and $F]+str;
    i:=i shr 4;
  end;
  if str='' then str:='0';
  hexstr:='$'+str;
end;

function uppercase(s : string) : string;

var
  i : byte;

begin
  for i:=1 to length(s) do
    s[i]:=UpCase(s[i]);
  uppercase:=s;
end;

{ This converts pascal reserved words to
the correct syntax.
}
function FixId(const s:string):string;

const
  maxtokens = 16;
  reservedid: array[1..maxtokens] of string[14] = (
    'CLASS',
    'DISPOSE',
    'FUNCTION',
    'FALSE',
    'LABEL',
    'NEW',
    'OUT',
    'PROPERTY',
    'PROCEDURE',
    'RECORD',
    'REPEAT',
    'STRING',
    'TYPE',
    'TRUE',
    'UNTIL',
    'VAR'
    );

var
  b : boolean;
  up : string;
  i: integer;
begin
  if s='' then
    begin
    FixId:='';
    exit;
    end;
  b:=false;
  up:=Uppercase(s);
  for i:=1 to maxtokens do
    begin
    if up=reservedid[i] then
      begin
        b:=true;
        break;
      end;
    end;
  if b then
    FixId:='_'+s
  else
    FixId:=s;
end;


function TypeName(const s:string):string;

var
  i : longint;

begin
  i:=1;
  if RemoveUnderScore and (length(s)>1) and (s[1]='_') then
    i:=2;
  if PrependTypes then
    TypeName:='T'+Copy(s,i,255)
  else
    TypeName:=Copy(s,i,255);
end;

function IsACType(const s : String) : Boolean;

var i : Integer;

begin
  IsACType := True;
  for i := 0 to MAX_CTYPESARRAY do
    begin
    if s = CTypesArray[i] then
      Exit;
    end;
  IsACType := False;
end;

function PointerName(const s:string):string;

var
  i : longint;

begin
  if UseCTypesUnit then
  begin
    if IsACType(s) then
    begin
    PointerName := 'p'+s;
    exit;
    end;
  end;
  i:=1;
  if RemoveUnderScore and (length(s)>1) and (s[1]='_') then
    i:=2;
  if UsePPointers then
  begin
    PointerName:='P'+Copy(s,i,255);
    PTypeList.Add(PointerName);
  end
  else
    PointerName:=Copy(s,i,255);
  if PointerPrefix then
      PTypeList.Add('P'+s);
end;


Function IsVarPara(P : presobject) : Boolean;

var
  varpara: boolean;

begin
  varpara:=usevarparas and
          assigned(p^.p1^.p1) and
          (p^.p1^.p1^.typ in [t_addrdef,t_pointerdef]) and
          assigned(p^.p1^.p1^.p1) and
          (p^.p1^.p1^.p1^.typ<>t_procdef);
  (* do not do it for char pointer !!               *)
  (* para : pchar; and var para : char; are         *)
  (* completely different in pascal                 *)
  (* here we exclude all typename containing char   *)
  (* is this a good method ??                       *)
  if varpara and
    (p^.p1^.p1^.typ=t_pointerdef) and
    (((p^.p1^.p1^.p1^.typ=t_id) and
      (pos('CHAR',uppercase(p^.p1^.p1^.p1^.str))<>0)) or
      ((p^.p1^.p1^.p1^.typ=t_void))
    ) then
    varpara:=false;
  IsVarPara:=varpara;
end;


procedure write_packed_fields_info(var outfile:text; p : presobject; ph : string);

var
    hp1,hp2,hp3 : presobject;
    is_sized : boolean;
    line : string;
    flag_index : longint;
    name : pansichar;
    ps : byte;

begin
  { write out the tempfile created }
  close(tempfile);
  reset(tempfile);
  is_sized:=false;
  flag_index:=0;
  writeln(outfile);
  writeln(outfile,aktspace,'const');
  shift(2);
  while not eof(tempfile) do
    begin
    readln(tempfile,line);
    ps:=pos('&',line);
    if ps>0 then
      line:=copy(line,1,ps-1)+ph+'_'+copy(line,ps+1,255);
    writeln(outfile,aktspace,line);
    end;
  writeln(outfile);
  close(tempfile);
  rewrite(tempfile);
  popshift;
  (* walk through all members *)
  hp1 := p^.p1;
  while assigned(hp1) do
    begin
    (* hp2 is t_memberdec *)
    hp2:=hp1^.p1;
    (*  hp3 is t_declist *)
    hp3:=hp2^.p2;
    while assigned(hp3) do
      begin
      if assigned(hp3^.p1^.p3) and
        (hp3^.p1^.p3^.typ = t_size_specifier) then
        begin
        is_sized:=true;
        name:=hp3^.p1^.p2^.p;
        { get function in interface }
        write(outfile,aktspace,'function ',name);
        write(outfile,'(var a : ',ph,') : ');
        shift(2);
        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
        writeln(outfile,';');
        popshift;
        { get function in implementation }
        write(implemfile,aktspace,'function ',name);
        write(implemfile,'(var a : ',ph,') : ');
        if not compactmode then
          shift(2);
        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
        writeln(implemfile,';');
        writeln(implemfile,aktspace,'begin');
        shift(2);
        write(implemfile,aktspace,name,':=(a.flag',flag_index);
        writeln(implemfile,' and bm_',ph,'_',name,') shr bp_',ph,'_',name,';');
        popshift;
        writeln(implemfile,aktspace,'end;');
        if not compactmode then
          popshift;
        writeln(implemfile,'');
        { set function in interface }
        write(outfile,aktspace,'procedure set_',name);
        write(outfile,'(var a : ',ph,'; __',name,' : ');
        shift(2);
        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
        writeln(outfile,');');
        popshift;
        { set function in implementation }
        write(implemfile,aktspace,'procedure set_',name);
        write(implemfile,'(var a : ',ph,'; __',name,' : ');
        if not compactmode then
          shift(2);
        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
        writeln(implemfile,');');
        writeln(implemfile,aktspace,'begin');
        shift(2);
        write(implemfile,aktspace,'a.flag',flag_index,':=');
        write(implemfile,'a.flag',flag_index,' or ');
        writeln(implemfile,'((__',name,' shl bp_',ph,'_',name,') and bm_',ph,'_',name,');');
        popshift;
        writeln(implemfile,aktspace,'end;');
        if not compactmode then
          popshift;
        writeln(implemfile,'');
        end
      else if is_sized then
        begin
        is_sized:=false;
        inc(flag_index);
        end;
      hp3:=hp3^.next;
      end;
    hp1:=hp1^.next;
    end;
  must_write_packed_field:=false;
  block_type:=bt_no;
end;


procedure write_expr(var outfile:text; p : presobject);

var
  DoFlush:Boolean;

begin
  if Not assigned(p) then
  begin
    writeln('Warning: attempt to write empty expression');
    exit;
  end;
  DoFlush:=True;
  case p^.typ of
    t_id,
    t_ifexpr :
      write(outfile,FixId(p^.p));
    t_funexprlist :
      write_funexpr(outfile,p);
    t_exprlist:
      begin
      if assigned(p^.p1) then
        write_expr(outfile,p^.p1);
      if assigned(p^.next) then
        begin
          write(', ');
          write_expr(outfile,p^.next);
        end;
      DoFlush:=False;
      end;
    t_preop:
      begin
      write(outfile,p^.p,'(');
      write_expr(outfile,p^.p1);
      write(outfile,')');
      end;
    t_typespec :
      begin
      write_type_specifier(outfile,p^.p1);
      write(outfile,'(');
      write_expr(outfile,p^.p2);
      write(outfile,')');
      end;
    t_bop :
      begin
      if p^.p1^.typ<>t_id then
        write(outfile,'(');
      write_expr(outfile,p^.p1);
      if p^.p1^.typ<>t_id then
      write(outfile,')');
      write(outfile,p^.p);
      if p^.p2^.typ<>t_id then
        write(outfile,'(');
      write_expr(outfile,p^.p2);
      if p^.p2^.typ<>t_id then
        write(outfile,')');
      end;
    t_arrayop :
      begin
      write_expr(outfile,p^.p1);
      write(outfile,p^.p,'[');
      write_expr(outfile,p^.p2);
      write(outfile,']');
      end;
    t_callop :
      begin
      write_expr(outfile,p^.p1);
      write(outfile,p^.p,'(');
      write_expr(outfile,p^.p2);
      write(outfile,')');
      end;
    else
      writeln(ord(p^.typ));
      internalerror(2);
      doFlush:=False;
  end;
  if DoFlush then
    Flush(OutFile);
end;


procedure write_ifexpr(var outfile:text; p : presobject);
begin
  flush(outfile);
  write(outfile,'if ');
  write_expr(outfile,p^.p1);
  writeln(outfile,' then');
  write(outfile,aktspace,'  ');
  write(outfile,p^.p);
  write(outfile,':=');
  write_expr(outfile,p^.p2);
  writeln(outfile);
  writeln(outfile,aktspace,'else');
  write(outfile,aktspace,'  ');
  write(outfile,p^.p);
  write(outfile,':=');
  write_expr(outfile,p^.p3);
  writeln(outfile,';');
  write(outfile,aktspace);
  flush(outfile);
end;


procedure write_all_ifexpr(var outfile:text; p : presobject);

begin
  if not assigned(p) then
  begin
    Writeln('Warning: writing empty ifexpr');
    exit;
  end;

  case p^.typ of
    t_id :;
    t_preop :
      write_all_ifexpr(outfile,p^.p1);
    t_callop,
    t_arrayop,
    t_bop :
      begin
      write_all_ifexpr(outfile,p^.p1);
      write_all_ifexpr(outfile,p^.p2);
      end;
    t_ifexpr :
      begin
      write_all_ifexpr(outfile,p^.p1);
      write_all_ifexpr(outfile,p^.p2);
      write_all_ifexpr(outfile,p^.p3);
      write_ifexpr(outfile,p);
      end;
    t_typespec :
      write_all_ifexpr(outfile,p^.p2);
    t_funexprlist,
    t_exprlist :
      begin
      if assigned(p^.p1) then
        write_all_ifexpr(outfile,p^.p1);
      if assigned(p^.next) then
        write_all_ifexpr(outfile,p^.next);
      end
    else
      internalerror(6);
  end;
end;

procedure write_funexpr(var outfile:text; p : presobject);
var
    i : longint;

begin
  if not assigned(p) then
  begin
    Writeln('Warning: attempt to write empty function expression');
    exit;
  end;
  case p^.typ of
    t_ifexpr :
      write(outfile,p^.p);
    t_exprlist :
      begin
      write_expr(outfile,p^.p1);
      if assigned(p^.next) then
        begin
        write(outfile,',');
        write_funexpr(outfile,p^.next);
        end
      end;
    t_funcname :
      begin
      if if_nb>0 then
        begin
            writeln(outfile,aktspace,'var');
            write(outfile,aktspace,'   ');
            for i:=1 to if_nb do
              begin
                write(outfile,'if_local',i);
                if i<if_nb then
                  write(outfile,', ')
                else
                  writeln(outfile,' : longint;');
              end;
            writeln(outfile,aktspace,'(* result types are not known *)');
            if_nb:=0;
        end;
      writeln(outfile,aktspace,'begin');
      shift(2);
      write(outfile,aktspace);
      write_all_ifexpr(outfile,p^.p2);
      write_expr(outfile,p^.p1);
      write(outfile,':=');
      write_funexpr(outfile,p^.p2);
      writeln(outfile,';');
      popshift;
      writeln(outfile,aktspace,'end;');
      if not compactmode then
        popshift;
      flush(outfile);
      end;
    t_funexprlist :
      begin
      if assigned(p^.p3) then
        begin
        write_type_specifier(outfile,p^.p3);
        write(outfile,'(');
        end;
      if assigned(p^.p1) then
        write_funexpr(outfile,p^.p1);
      if assigned(p^.p2) then
        begin
        write(outfile,'(');
        write_funexpr(outfile,p^.p2);
        write(outfile,')');
        end;
      if assigned(p^.p3) then
        write(outfile,')');
      end
    else
      internalerror(5);
  end;
end;

procedure write_args(var outfile:text; p : presobject);

var
    len,para : longint;
    old_in_args : boolean;
    varpara : boolean;
    lastp : presobject;
    hs : string;

begin
  _NeedEllipsisOverload:=false;
  para:=1;
  len:=0;
  lastp:=nil;
  old_in_args:=in_args;
  in_args:=true;
  write(outfile,'(');
  shift(2);

  (* walk through all arguments *)
  (* p must be of type t_arglist *)
  while assigned(p) do
    begin
    if p^.typ<>t_arglist then
      internalerror(10);
    (* is ellipsis ? *)
    if not assigned(p^.p1^.p1) and not assigned(p^.p1^.next) then
      begin
      write(outfile,'args:array of const');
      (* if variable number of args we must allways pop *)
      no_pop:=false;
      (* Needs 2 declarations, also one without args, becuase
        in C you can omit the second parameter. Default parameter
        doesn't help as that isn't possible with array of const *)
      _NeedEllipsisOverload:=true;
      (* Remove this para *)
      if assigned(lastp) then
      lastp^.next:=nil;
      dispose(p,done);
      (* leave the loop as p isnot valid anymore *)
      break;
      end
    (* we need to correct this in the pp file after *)
    else
      begin
      (* generate a call by reference parameter ?       *)
//     varpara:=usevarparas and
//              assigned(p^.p1^.p2^.p1) and
//              (p^.p1^.p2^.p1^.typ in [t_addrdef,t_pointerdef]) and
//               assigned(p^.p1^.p2^.p1^.p1) and
//               (p^.p1^.p2^.p1^.p1^.typ<>t_procdef);
      varpara:=IsVarPara(p);
      if varpara then
        begin
        write(outfile,'var ');
        inc(len,4);
        end;

      (* write new parameter name *)
      if assigned(p^.p1^.p2^.p2) then
        begin
          hs:=FixId(p^.p1^.p2^.p2^.p);
          write(outfile,hs);
          inc(len,length(hs));
        end
      else
        begin
          If removeUnderscore then
            begin
              Write (outfile,'para',para);
              inc(Len,5);
            end
          else
            begin
              write(outfile,'_para',para);
              inc(Len,6);
            end;
        end;
      write(outfile,':');
      if varpara then
      begin
        write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1^.p1);
      end
      else
        write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1);
      end;
    lastp:=p;
    p:=p^.next;
    if assigned(p) then
      begin
          write(outfile,'; ');
          { if len>40 then : too complicated to compute }
          if (para mod 5) = 0 then
            begin
              writeln(outfile);
              write(outfile,aktspace);
            end;
      end;
    inc(para);
    end;
  write(outfile,')');
  flush(outfile);
  in_args:=old_in_args;
  popshift;
end;


Procedure write_pointerdef(var outfile:text; p,simple_type : presobject);

var
  pointerwritten : Boolean;

begin
  (* procedure variable ? *)
  if assigned(p^.p1) and (p^.p1^.typ=t_procdef) then
    begin
    is_procvar:=true;
    (* distinguish between procedure and function *)
    if (simple_type^.typ=t_void) and (p^.p1^.p1=nil) then
      begin
      write(outfile,'procedure ');
      shift(10);
      (* write arguments *)
      if assigned(p^.p1^.p2) then
        write_args(outfile,p^.p1^.p2);
      flush(outfile);
      popshift;
      end
    else
      begin
      write(outfile,'function ');
      shift(9);
      (* write arguments *)
      if assigned(p^.p1^.p2) then
        write_args(outfile,p^.p1^.p2);
      write(outfile,':');
      flush(outfile);

      old_in_args:=in_args;
      (* write pointers as P.... instead of ^.... *)
      in_args:=true;
      write_p_a_def(outfile,p^.p1^.p1,simple_type);
      in_args:=old_in_args;
      popshift;
      end
    end
  else
    begin
    (* generate "pointer" ? *)
    if (simple_type^.typ=t_void) and (p^.p1=nil) then
      begin
        write(outfile,'pointer');
        flush(outfile);
      end
    else
      begin
      pointerwritten:=false;
      if (p^.p1=nil) and UsePPointers then
        begin
        if (simple_type^.typ=t_id) then
          begin
          write(outfile,PointerName(simple_type^.p));
          pointerwritten:=true;
          end
        { structure }
        else if (simple_type^.typ in [t_uniondef,t_structdef]) and
                (simple_type^.p1=nil) and (simple_type^.p2^.typ=t_id) then
          begin
          write(outfile,PointerName(simple_type^.p2^.p));
          pointerwritten:=true;
          end;
        end;
      if not pointerwritten then
        begin
        if in_args then
          begin
          write(outfile,'P');
          pointerprefix:=true;
          end
        else
          write(outfile,'^');
        write_p_a_def(outfile,p^.p1,simple_type);
        pointerprefix:=false;
        end;
      end;
    end;
end;

Procedure write_arraydef(var outfile:text; p,simple_type : presobject);

var
  constant : boolean;
  i, error : integer;

begin
  constant:=false;
  if assigned(p^.p2) then
    begin
    if p^.p2^.typ=t_id then
      begin
      val(p^.p2^.str,i,error);
      if error=0 then
        begin
          dec(i);
          constant:=true;
        end;
      end;
    if not constant then
      begin
      write(outfile,'array[0..(');
      write_expr(outfile,p^.p2);
      write(outfile,')-1] of ');
     end
    else
      write(outfile,'array[0..',i,'] of ');
    end
  else
    begin
    (* open array *)
    write(outfile,'array of ');
    end;
  flush(outfile);
  write_p_a_def(outfile,p^.p1,simple_type);
end;


procedure write_p_a_def(var outfile:text; p,simple_type : presobject);

begin
  if not(assigned(p)) then
    begin
    write_type_specifier(outfile,simple_type);
    exit;
    end;
  case p^.typ of
    t_pointerdef :
      Write_pointerdef(outfile,p,simple_type);
    t_arraydef :
      Write_arraydef(outfile,p,simple_type);
  else
    internalerror(1);
  end;
end;

procedure write_type_specifier_id(var outfile:text; p : presobject);

begin
  if pointerprefix then
    if UseCtypesUnit then
    begin
      if not IsACType(p^.p) then
      begin
        PTypeList.Add('P'+p^.str);
      end;
    end
    else
      begin
      PTypeList.Add('P'+p^.str);
      end;
  if p^.intname then
    write(outfile,p^.p)
  else
    write(outfile,TypeName(p^.p));
end;


procedure write_type_specifier_pointer(var outfile:text; p : presobject);

var
  pointerwritten : Boolean;

begin
  pointerwritten:=false;
  if (p^.p1^.typ=t_void) then
    begin
    write(outfile,'pointer');
    pointerwritten:=true;
    end
  else if UsePPointers then
    begin
    if (p^.p1^.typ=t_id) then
    begin
      write(outfile,PointerName(p^.p1^.p));
      pointerwritten:=true;
    end
    { structure }
    else if (p^.p1^.typ in [t_uniondef,t_structdef]) and
            (p^.p1^.p1=nil) and (p^.p1^.p2^.typ=t_id) then
    begin
      write(outfile,PointerName(p^.p1^.p2^.p));
      pointerwritten:=true;
    end;
    end;
  if not pointerwritten then
    begin
    if in_args then
      begin
      if UseCTypesUnit and IsACType(p^.p1^.p) then
        write(outfile,'p')
      else
        write(outfile,'P');
      pointerprefix:=true;
      end
    else
      begin
        if UseCTypesUnit and (IsACType(p^.p1^.p)=False) then
          write(outfile,'^')
        else
          write(outfile,'p');
      end;
    write_type_specifier(outfile,p^.p1);
    pointerprefix:=false;
    end;
end;

procedure write_enum_const(var outfile:text; hp1 : presobject; var lastexpr : presobject; var l : longint);

var
  error : integer;

begin
  write(outfile,aktspace,hp1^.p1^.p,' = ');
  if assigned(hp1^.p2) then
    begin
    write_expr(outfile,hp1^.p2);
    writeln(outfile,';');
    lastexpr:=hp1^.p2;
    if lastexpr^.typ=t_id then
      begin
      val(lastexpr^.str,l,error);
      if error=0 then
        begin
        inc(l);
        lastexpr:=nil;
        end
      else
        l:=1;
      end
    else
      l:=1;
    end
  else
  begin
    if assigned(lastexpr) then
      begin
      write(outfile,'(');
      write_expr(outfile,lastexpr);
      writeln(outfile,')+',l,';');
      end
    else
      writeln (outfile,l,';');
    inc(l);
    end;
end;

procedure write_type_specifier_enum(var outfile:text; p : presobject);

var
  hp1,lastexpr : presobject;
  l,w : longint;

begin
  if (typedef_level>1) and (p^.p1=nil) and (p^.p2^.typ=t_id) then
    begin
    if pointerprefix then
      if UseCTypesUnit and (IsACType( p^.p2^.p )=False) then
        PTypeList.Add('P'+p^.p2^.str);
    write(outfile,p^.p2^.p);
    end
  else if not EnumToConst then
    begin
    write(outfile,'(');
    hp1:=p^.p1;
    w:=length(aktspace);
    while assigned(hp1) do
      begin
      write(outfile,hp1^.p1^.p);
      if assigned(hp1^.p2) then
        begin
        write(outfile,' := ');
        write_expr(outfile,hp1^.p2);
        w:=w+6;(* strlen(hp1^.p); *)
        end;
      w:=w+length(hp1^.p1^.str);
      hp1:=hp1^.next;
      if assigned(hp1) then
        write(outfile,',');
      if w>40 then
        begin
        writeln(outfile);
        write(outfile,aktspace);
        w:=length(aktspace);
        end;
      flush(outfile);
      end;
    write(outfile,')');
    flush(outfile);
    end
  else
    begin
    Writeln (outfile,' Longint;');
    hp1:=p^.p1;
    lastexpr:=nil;
    l:=0;
    Writeln (outfile,copy(aktspace,1,length(aktspace)-2),'Const');
    while assigned(hp1) do
      begin
      write_enum_const(outfile,hp1,lastexpr,l);
      hp1:=hp1^.next;
      flush(outfile);
      end;
    block_type:=bt_const;
    end;
end;

procedure write_type_specifier_struct(var outfile:text; p : presobject);

var
  hp1,hp2,hp3 : presobject;
  i,l : longint;
  error : integer;
  current_power,
  mask : cardinal;
  flag_index : longint;
  current_level : byte;
    is_sized : boolean;

begin
  inc(typedef_level);
  flag_index:=-1;
  is_sized:=false;
  current_level:=0;
  if ((in_args) or (typedef_level>1)) and (p^.p1=nil) and (p^.p2^.typ=t_id) then
    begin
    if pointerprefix then
      if UseCTypesUnit and (IsACType(p^.p2^.str)=false) then
        PTypeList.Add('P'+p^.p2^.str);
    write(outfile,TypeName(p^.p2^.p));
    end
  else
    begin
      if packrecords then
        writeln(outfile,'packed record')
      else
        writeln(outfile,'record');
      shift(2);
      hp1:=p^.p1;

      (* walk through all members *)
      while assigned(hp1) do
        begin
        (* hp2 is t_memberdec *)
        hp2:=hp1^.p1;
        (*  hp3 is t_declist *)
        hp3:=hp2^.p2;
        while assigned(hp3) do
          begin
          if assigned(hp3^.p1) and
              (not assigned(hp3^.p1^.p3) or
              (hp3^.p1^.p3^.typ <> t_size_specifier)) then
            begin
            if is_sized then
              begin
                if current_level <= 16 then
                  writeln(outfile,'word;')
                else if current_level <= 32 then
                  writeln(outfile,'longint;')
                else
                  internalerror(11);
                is_sized:=false;
              end;

            write(outfile,aktspace,FixId(hp3^.p1^.p2^.p));
            write(outfile,' : ');
            shift(2);
            write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
            popshift;
            end;
          { size specifier  or default value ? }
          if assigned(hp3^.p1) and
              assigned(hp3^.p1^.p3) then
            begin
            { we could use mask to implement this }
            { because we need to respect the positions }
            if hp3^.p1^.p3^.typ = t_size_specifier then
              begin
              if not is_sized then
                begin
                current_power:=1;
                current_level:=0;
                inc(flag_index);
                write(outfile,aktspace,'flag',flag_index,' : ');
                end;
              must_write_packed_field:=true;
              is_sized:=true;
              { can it be something else than a constant ? }
              { it can be a macro !! }
              if hp3^.p1^.p3^.p1^.typ=t_id then
                begin
                val(hp3^.p1^.p3^.p1^.str,l,error);
                if error=0 then
                  begin
                  mask:=0;
                  for i:=1 to l do
                    begin
                      inc(mask,current_power);
                      current_power:=current_power*2;
                    end;
                  write(tempfile,'bm_&',hp3^.p1^.p2^.p);
                  writeln(tempfile,' = ',hexstr(mask),';');
                  write(tempfile,'bp_&',hp3^.p1^.p2^.p);
                  writeln(tempfile,' = ',current_level,';');
                  current_level:=current_level + l;
                  { go to next flag if 31 }
                  if current_level = 32 then
                    begin
                      write(outfile,'longint');
                      is_sized:=false;
                    end;
                  end;
                end;
              end
            else if hp3^.p1^.p3^.typ = t_default_value then
              begin
              write(outfile,'{=');
              write_expr(outfile,hp3^.p1^.p3^.p1);
              write(outfile,' ignored}');
              end;
            end;
          if not is_sized then
            begin
            if is_procvar then
              begin
              if not no_pop then
                write(outfile,';cdecl');
              is_procvar:=false;
              end;
            writeln(outfile,';');
            end;
          hp3:=hp3^.next;
          end;
        hp1:=hp1^.next;
        end;
      if is_sized then
        begin
        if current_level <= 16 then
          writeln(outfile,'word;')
        else if current_level <= 32 then
          writeln(outfile,'longint;')
        else
          internalerror(11);
        is_sized:=false;
        end;
      popshift;
      write(outfile,aktspace,'end');
      flush(outfile);
    end;
  dec(typedef_level);
end;

procedure write_type_specifier_union(var outfile:text; p : presobject);

var
  hp1,hp2,hp3 : presobject;
  l : integer;

begin
  inc(typedef_level);
  if (typedef_level>1) and (p^.p1=nil) and (p^.p2^.typ=t_id) then
    begin
    write(outfile,p^.p2^.p);
    end
  else
    begin
    inc(typedef_level);
    if packrecords then
      writeln(outfile,'packed record')
    else
      writeln(outfile,'record');
    shift(2);
    writeln(outfile,aktspace,'case longint of');
    shift(2);
    l:=0;
    hp1:=p^.p1;

    (* walk through all members *)
    while assigned(hp1) do
      begin
      (* hp2 is t_memberdec *)
      hp2:=hp1^.p1;
      (* hp3 is t_declist *)
      hp3:=hp2^.p2;
      while assigned(hp3) do
        begin
        write(outfile,aktspace,l,' : ( ');
        write(outfile,FixId(hp3^.p1^.p2^.p),' : ');
        shift(2);
        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
        popshift;
        writeln(outfile,' );');
        hp3:=hp3^.next;
        inc(l);
        end;
      hp1:=hp1^.next;
      end;
    popshift;
    write(outfile,aktspace,'end');
    popshift;
    flush(outfile);
    dec(typedef_level);
    end;
  dec(typedef_level);
end;

procedure write_type_specifier(var outfile:text; p : presobject);

begin
  case p^.typ of
  t_id :
    write_type_specifier_id(outfile,p);
  { what can we do with void defs  ? }
  t_void :
    write(outfile,'pointer');
  t_pointerdef :
    Write_type_specifier_pointer(outfile,p);
  t_enumdef :
    Write_type_specifier_enum(outfile,p);
  t_structdef :
    Write_type_specifier_struct(outfile,p);
  t_uniondef :
    Write_type_specifier_union(outfile,p);
  else
    internalerror(3);
  end;
end;

procedure write_def_params(var outfile:text; p : presobject);

var
  hp1 : presobject;

begin
  case p^.typ of
  t_enumdef:
    begin
    hp1:=p^.p1;
    while assigned(hp1) do
      begin
      write(outfile,FixId(hp1^.p1^.p));
      hp1:=hp1^.next;
      if assigned(hp1) then
        write(outfile,',')
      else
        write(outfile);
      flush(outfile);
      end;
    flush(outfile);
    end;
  else
    internalerror(4);
  end;
end;


procedure write_statement_block(var outfile:text; p : presobject);

begin
  writeln(outfile,aktspace,'begin');
  while assigned(p) do
    begin
    shift(2);
    if assigned(p^.p1) then
      case p^.p1^.typ of
        t_whilenode:
          begin
          write(outfile,aktspace,'while ');
          write_expr(outfile,p^.p1^.p1);
          writeln(outfile,' do');
          shift(2);
          write_statement_block(outfile,p^.p1^.p2);
          popshift;
          end;
      else
        write(outfile,aktspace);
        write_expr(outfile,p^.p1);
        writeln(outfile,';');
      end; // case
    p:=p^.next;
    popshift;
    end;
  writeln(outfile,aktspace,'end;');
end;



procedure WriteFileHeader(var headerfile: Text);
var
 i: integer;
 originalstr: string;
begin
{ write unit header }
  if not includefile then
   begin
     if createdynlib then
       writeln(headerfile,'{$mode objfpc}');
     writeln(headerfile,'unit ',unitname,';');
     writeln(headerfile,'interface');
     writeln(headerfile);
     if UseCTypesUnit then
     begin
       writeln(headerfile,'uses');
       writeln(headerfile,'  ctypes;');
       writeln(headerfile);
     end;
     writeln(headerfile,'{');
     writeln(headerfile,'  Automatically converted by H2Pas ',version,' from ',inputfilename);
     writeln(headerfile,'  The following command line parameters were used:');
     for i:=1 to paramcount do
       writeln(headerfile,'    ',paramstr(i));
     writeln(headerfile,'}');
     writeln(headerfile);
   end;
  if UseName then
   begin
     writeln(headerfile,aktspace,'const');
     writeln(headerfile,aktspace,'  External_library=''',libfilename,'''; {Setup as you need}');
     writeln(headerfile);
   end;
  if UsePPointers then
   begin
     Writeln(headerfile,aktspace,'{ Pointers to basic pascal types, inserted by h2pas conversion program.}');
     Writeln(headerfile,aktspace,'Type');
     Writeln(headerfile,aktspace,'  PLongint  = ^Longint;');
     Writeln(headerfile,aktspace,'  PSmallInt = ^SmallInt;');
     Writeln(headerfile,aktspace,'  PByte     = ^Byte;');
     Writeln(headerfile,aktspace,'  PWord     = ^Word;');
     Writeln(headerfile,aktspace,'  PDWord    = ^DWord;');
     Writeln(headerfile,aktspace,'  PDouble   = ^Double;');
     Writeln(headerfile);
   end;
  if PTypeList.count <> 0 then
   Writeln(headerfile,aktspace,'Type');
  for i:=0 to (PTypeList.Count-1) do
   begin
     originalstr:=copy(PTypelist[i],2,length(PTypeList[i]));
     if PrependTypes then
       originalstr:='T'+originalstr;
     Writeln(headerfile,aktspace,'  '+PTypeList[i],'  = ^',originalstr,';');
   end;
  if not packrecords then
   begin
      writeln(headerfile,'{$IFDEF FPC}');
      writeln(headerfile,'{$PACKRECORDS C}');
      writeln(headerfile,'{$ENDIF}');
   end;
  writeln(headerfile);
end;

procedure OpenOutputFiles;

begin
  { This is the intermediate output file }
  assign(outfile, 'ext3.tmp');
  {$I-}
  rewrite(outfile);
  {$I+}
  if ioresult<>0 then
   begin
     writeln('file ext3.tmp could not be created!');
     halt(1);
   end;
  writeln(outfile);
  { Open tempfiles }
  { This is where the implementation section of the unit shall be stored }

  Assign(implemfile,'ext.tmp');
  rewrite(implemfile);
  Assign(tempfile,'ext2.tmp');
  rewrite(tempfile);
end;

procedure CloseTempFiles;
begin
  close(implemfile);
  erase(implemfile);
  close(tempfile);
  erase(tempfile);
end;

procedure WriteLibraryInitialization;

var
 I : Integer;

begin
  writeln(outfile,'  uses');
  writeln(outfile,'    SysUtils, dynlibs;');
  writeln(outfile);
  writeln(outfile,'  var');
  writeln(outfile,'    hlib : tlibhandle;');
  writeln(outfile);
  writeln(outfile);
  writeln(outfile,'  procedure Free',unitname,';');
  writeln(outfile,'    begin');
  writeln(outfile,'      FreeLibrary(hlib);');

  for i:=0 to (freedynlibproc.Count-1) do
    Writeln(outfile,'      ',freedynlibproc[i]);

  writeln(outfile,'    end;');
  writeln(outfile);
  writeln(outfile);
  writeln(outfile,'  procedure Load',unitname,'(lib : pchar);');
  writeln(outfile,'    begin');
  writeln(outfile,'      Free',unitname,';');
  writeln(outfile,'      hlib:=LoadLibrary(lib);');
  writeln(outfile,'      if hlib=0 then');
  writeln(outfile,'        raise Exception.Create(format(''Could not load library: %s'',[lib]));');
  writeln(outfile);
  for i:=0 to (loaddynlibproc.Count-1) do
    Writeln(outfile,'      ',loaddynlibproc[i]);
  writeln(outfile,'    end;');

  writeln(outfile);
  writeln(outfile);

  writeln(outfile,'initialization');
  writeln(outfile,'  Load',unitname,'(''',unitname,''');');
  writeln(outfile,'finalization');
  writeln(outfile,'  Free',unitname,';');
end;


end.
