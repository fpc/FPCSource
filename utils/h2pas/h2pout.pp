unit h2pout;

{$modeswitch result}

interface

uses
  SysUtils, classes,
  h2poptions, h2pconst,h2plexlib,h2pyacclib, scanbase,h2ptypes;

procedure OpenOutputFiles;
procedure CloseTempFiles;

procedure WriteFileHeader(var headerfile: Text);
// Writes the uses clause of the implementation section for -P.
procedure WriteLibraryUses;
procedure WriteLibraryInitialization;

// This will write each pointer type only once.
function WritePointerTypeDef(var aFile : text; const PN,TN : AnsiString) : Boolean;
// Writes a marker line for type TN, replaced by the pointer types to TN when the unit is assembled.
procedure WritePointerMarker(var aFile : text; const TN : AnsiString);
// Declares the type aName for the function pointer element of the array or pointer declarator chain aChain with base type aType,
// and replaces the element by the type name; returns false when aChain is no array of or pointer to function pointers.
function HoistProcVarElement(const aName : AnsiString; aChain, aType : presobject) : Boolean;
// Applies HoistProcVarElement to the members of the struct or union aType, with type names aOwner_member.
procedure HoistStructProcVarElements(const aOwner : AnsiString; aType : presobject);
// Registers aName as a typedef of a function type: a pointer to it is the Pascal procedural type itself.
procedure RegisterFunctionType(const aName : AnsiString);
// Writes the pointer types for the marker line aLine; returns false when aLine is no marker.
function WriteMarkedPointers(var aFile : text; const aLine : AnsiString) : Boolean;
// Returns true when the type TN was declared before.
function IsDeclaredType(const TN : AnsiString) : Boolean;
// Writes a marker line for the struct TN that has no declaration yet, with its typedef alias AN ('' for none).
// When the unit is assembled, the marker becomes an empty record TN and the alias, unless TN is declared later:
// the alias then follows that declaration.
procedure WriteOpaqueMarker(var aFile : text; const TN, AN : AnsiString);
// Writes a marker line before the record TN, replaced by the pointer types to TN and its later aliases.
procedure WriteRecordMarker(var aFile : text; const TN : AnsiString);
// Returns true when the record TN with the definition aType can move to its opaque marker:
// all the types it refers to were declared before that marker.
function CanMoveRecord(const TN : AnsiString; aType : presobject) : Boolean;
// Writes the marker lines around the record TN that moves to its opaque marker.
procedure WriteMovedRecordStart(var aFile : text; const TN : AnsiString);
procedure WriteMovedRecordEnd(var aFile : text; const TN : AnsiString);
// Removes the lines of the moved records from aLines, for WriteMarkedPointers to write them at their opaque marker.
procedure CollectMovedRecords(aLines : TStringList);
// Writes a marker line: the declarations that follow are of kind aKind: T type, C constant, D constant that uses types,
// V variable, F function, I implementation.
procedure WriteSectionMarker(var aFile : text; aKind : char);
// Writes the section keyword aKeyword with indentation aIndent, as a marker line that -1 replaces.
procedure WriteSectionKeyword(var aFile : text; const aIndent, aKeyword : AnsiString);
// Returns true when the constant expression aExpr uses only literals, casts to base types and plain constants.
function IsPlainConstExpr(aExpr : presobject) : Boolean;
// Registers aName as a plain constant, which -1 writes before the types.
procedure RegisterPlainConst(const aName : AnsiString);
// Arranges the lines of the unit in aLines for -1: the constants, then all types in one section, then the rest.
procedure ArrangeSections(aLines : TStringList);

procedure write_statement_block(var outfile:text; p : presobject);
procedure write_type_specifier(var outfile:text; p : presobject);
// Writes the type p of a cast or macro result, with the pointer type names used for parameters.
procedure write_cast_type(var outfile:text; p : presobject);
procedure write_p_a_def(var outfile:text; p,simple_type : presobject);
procedure write_ifexpr(var outfile:text; p : presobject);
procedure write_funexpr(var outfile:text; p : presobject);
procedure write_def_params(var outfile:text; p : presobject);
// Writes the argument list p; with aSkipEllipsis the ellipsis argument is left out.
procedure write_args(var outfile:text; p : presobject; aSkipEllipsis : Boolean);
procedure write_packed_fields_info(var outfile:text; p : presobject; ph : string);
procedure write_expr(var outfile:text; p : presobject);
// Writes the directives of the procedure type just written: stdcall for aStdCall, else cdecl, and varargs.
procedure WriteProcVarDirectives(var aFile : text; aStdCall : Boolean);

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
function hexstr(i : qword) : string;
function uppercase(s : string) : string;
function PointerName(const s:string):string;
function IsACType(const s : String) : Boolean;
// Returns true when the argument list aArgs ends in an ellipsis.
function HasEllipsis(aArgs : presobject) : Boolean;
function TypeName(const s:string):string;
// Returns s with an underscore prefix when it is a Pascal reserved word.
function FixId(const s:string):string;

Var
  No_pop   : boolean;
  implemfile  : text;  (* file for implementation headers extern procs *)
  in_args : boolean = false;
  old_in_args : boolean = false;
  must_write_packed_field : boolean;
  is_procvar : boolean = false;
  // Set when the last procedure type written takes variable arguments.
  is_varargs : boolean = false;
  // Set when an array of const parameter was written.
  UsesArrayOfConst : boolean = false;
  is_packed : boolean = false;
  if_nb : longint = 0;

implementation

const
  PointerMarker = #1;
  OpaqueMarker = #2;
  RecordMarker = #3;
  MovedStartMarker = #4;
  MovedEndMarker = #5;
  SectionMarker = #6;
  KeywordMarker = #7;
  HeaderPointersMarker = #8;

var
  WrittenPointers : TStringList;
  DeclaredTypes : TStringList;
  // Structs written as an opaque marker, those declared later, and the aliases that follow them, as TN=AN.
  OpaqueTypes : TStringList;
  DefinedOpaqueTypes : TStringList;
  PendingAliases : TStringList;
  // The declared types in the order of declaration, and the number of them at each opaque marker, as TN=count.
  DeclaredSequence : TStringList;
  OpaqueSequence : TStringList;
  // The lines of the moved records, as objects of their names.
  MovedRecordLines : TStringList;
  // Constants of literals and other plain constants only.
  PlainConsts : TStringList;
  // Targets of pointer types to pointer types, as PPname=Pname.
  PointerTargets : TStringList;
  // Names of typedefs of a function type, without T prefix.
  FunctionTypes : TStringList;
  // Number of P prefixes written for the pointer type being written.
  pointer_level : Integer = 0;
  // Flag field index of each bit field of the record being written, as name=index.
  BitFieldFlags : TStringList;
  // Members of the enum types written so far.
  EnumMembers : TStringList;
  // Set while the value of an enum member is written.
  in_enum_value : boolean = false;
 tempfile : text;
  space_array : array [0..255] of integer;
  space_index : integer;
  typedef_level : longint = 0;

// Registers the pointer types P<aPointer>, PP<aPointer>, ... up to aLevels levels.
procedure RegisterPointerChain(const aPointer : AnsiString; aLevels : Integer); forward;
// Returns true when aType names a typedef of a function type.
function IsFunctionType(aType : presobject) : Boolean; forward;
// Returns true when the unit header needs pointer types to types that are not declared in the unit.
function HasHeaderPointers : Boolean; forward;
// Writes the pointer types of the unit header with indentation aIndent.
procedure WriteHeaderPointers(var aFile : text; const aIndent : AnsiString); forward;

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
    Writeln('Warning: attempt to decrease the indentation below zero');
    space_index:=0;
    end
  else
    delete(aktspace,1,space_array[space_index]);
end;

procedure resetshift;
begin
  space_index:=0;
  if compactmode then
    aktspace:=''
  else
    aktspace:='  ';
end;

function str(i : longint) : string;

var
  s : string;

begin
  system.str(i,s);
  str:=s;
end;


function hexstr(i : qword) : string;

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
  maxtokens = 68;
  reservedid: array[1..maxtokens] of string[14] = (
    'AND','ARRAY','AS','ASM','BEGIN','CASE','CLASS','CONST',
    'CONSTRUCTOR','DESTRUCTOR','DISPOSE','DIV','DO','DOWNTO','ELSE','END',
    'EXCEPT','EXPORTS','FALSE','FILE','FINALIZATION','FINALLY','FOR','FUNCTION',
    'GOTO','IF','IMPLEMENTATION','IN','INHERITED','INITIALIZATION','INTERFACE','IS',
    'LABEL','LIBRARY','MOD','NEW','NIL','NOT','OBJECT','OF',
    'OPERATOR','OR','OUT','PACKED','PROCEDURE','PROGRAM','PROPERTY','RAISE',
    'RECORD','REPEAT','RESOURCESTRING','SET','SHL','SHR','STRING','THEN',
    'THREADVAR','TO','TRUE','TRY','TYPE','UNIT','UNTIL','USES',
    'VAR','WHILE','WITH','XOR'
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

// Returns s as it follows the P of a pointer type name: without T prefix, and without leading underscore for -T.
function PointerBaseName(const s:string):string;

begin
  if RemoveUnderScore and (length(s)>1) and (s[1]='_') then
    PointerBaseName:=Copy(s,2,255)
  else
    PointerBaseName:=s;
end;


function PointerName(const s:string):string;

begin
  if UseCTypesUnit then
  begin
    if IsACType(s) then
    begin
    PointerName := 'p'+s;
    exit;
    end;
  end;
  if UsePPointers then
  begin
    PointerName:='P'+PointerBaseName(s);
    PTypeList.Add(PointerName);
  end
  else
    PointerName:=PointerBaseName(s);
  if PointerPrefix then
    PTypeList.Add('P'+PointerBaseName(s));
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
    line : string;
    flag_index : string;
    name : pansichar;
    ps : byte;

begin
  { write out the tempfile created }
  close(tempfile);
  reset(tempfile);
  writeln(outfile);
  WriteSectionMarker(outfile,'C');
  WriteSectionKeyword(outfile,aktspace,'const');
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
  WriteSectionMarker(outfile,'F');
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
      if assigned(hp3^.p1) and assigned(hp3^.p1^.p3) and
        (hp3^.p1^.p3^.typ = t_size_specifier) then
        begin
        name:=hp3^.p1^.p2^.p;
        flag_index:=BitFieldFlags.Values[hp3^.p1^.p2^.str];
        { get function in interface }
        write(outfile,aktspace,'function ',name);
        write(outfile,'(var __rec : ',ph,') : ');
        shift(2);
        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
        writeln(outfile,';');
        popshift;
        { get function in implementation }
        write(implemfile,aktspace,'function ',name);
        write(implemfile,'(var __rec : ',ph,') : ');
        if not compactmode then
          shift(2);
        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
        writeln(implemfile,';');
        writeln(implemfile,aktspace,'begin');
        shift(2);
        write(implemfile,aktspace,name,':=(__rec.flag',flag_index);
        writeln(implemfile,' and bm_',ph,'_',name,') shr bp_',ph,'_',name,';');
        popshift;
        writeln(implemfile,aktspace,'end;');
        if not compactmode then
          popshift;
        writeln(implemfile,'');
        { set function in interface }
        write(outfile,aktspace,'procedure set_',name);
        write(outfile,'(var __rec : ',ph,'; __',name,' : ');
        shift(2);
        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
        writeln(outfile,');');
        popshift;
        { set function in implementation }
        write(implemfile,aktspace,'procedure set_',name);
        write(implemfile,'(var __rec : ',ph,'; __',name,' : ');
        if not compactmode then
          shift(2);
        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
        writeln(implemfile,');');
        writeln(implemfile,aktspace,'begin');
        shift(2);
        write(implemfile,aktspace,'__rec.flag',flag_index,':=');
        write(implemfile,'(__rec.flag',flag_index,' and not bm_',ph,'_',name,') or ');
        writeln(implemfile,'((__',name,' shl bp_',ph,'_',name,') and bm_',ph,'_',name,');');
        popshift;
        writeln(implemfile,aktspace,'end;');
        if not compactmode then
          popshift;
        writeln(implemfile,'');
        end;
      hp3:=hp3^.next;
      end;
    hp1:=hp1^.next;
    end;
  BitFieldFlags.Clear;
  must_write_packed_field:=false;
  block_type:=bt_no;
end;


// Returns true when p is a character literal: 'x', '''' or #n.
function IsCharLiteral(p : presobject) : boolean;

var
  s : string;
  i : integer;

begin
  Result:=false;
  if not assigned(p) or (p^.typ<>t_id) then
    exit;
  s:=p^.str;
  if (s='''''''') or ((length(s)=3) and (s[1]='''') and (s[3]='''')) then
    exit(true);
  if (length(s)<2) or (s[1]<>'#') then
    exit;
  for i:=2 to length(s) do
    if not (s[i] in ['0'..'9']) then
      exit;
  Result:=true;
end;


// Returns true when the binary operator aOp takes the integer value of a character literal operand.
function IsIntegerOperator(const aOp : string) : boolean;

begin
  Result:=(aOp='+') or (aOp='-') or (aOp='*') or (aOp=' div ') or (aOp=' mod ') or (aOp=' shl ') or (aOp=' shr ')
          or (aOp=' and ') or (aOp=' or ') or (aOp=' xor ') or (aOp='=') or (aOp='<>') or (aOp='<') or (aOp='<=')
          or (aOp='>') or (aOp='>=');
end;


// Writes the operand p of the binary operator aOp, a character literal as its ordinal value when aOrd is set.
procedure write_operand(var outfile:text; p : presobject; aOrd : boolean);

begin
  if aOrd and IsCharLiteral(p) then
    write(outfile,'ord(',p^.p,')')
  else if p^.typ<>t_id then
    begin
    write(outfile,'(');
    write_expr(outfile,p);
    write(outfile,')');
    end
  else
    write_expr(outfile,p);
end;


procedure write_expr(var outfile:text; p : presobject);

var
  DoFlush:Boolean;
  lOrd : boolean;

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
      if in_enum_value and (p^.typ=t_id) and (EnumMembers.IndexOf(p^.p)>=0) then
        write(outfile,'ord(',FixId(p^.p),')')
      else if in_enum_value and IsCharLiteral(p) then
        write(outfile,'ord(',p^.p,')')
      else if p^.skiptprefix then
        write(outfile,p^.p)
      else
        write(outfile,FixId(p^.p));
    t_funexprlist :
      write_funexpr(outfile,p);
    t_exprlist:
      begin
      if assigned(p^.p1) then
        write_expr(outfile,p^.p1);
      if assigned(p^.next) then
        begin
          write(outfile,', ');
          write_expr(outfile,p^.next);
        end;
      DoFlush:=False;
      end;
    t_preop:
      if p^.str='^' then
        begin
        (* dereference is postfix in Pascal *)
        if p^.p1^.typ=t_id then
          write_expr(outfile,p^.p1)
        else
          begin
          write(outfile,'(');
          write_expr(outfile,p^.p1);
          write(outfile,')');
          end;
        write(outfile,'^');
        end
      else
        begin
        write(outfile,p^.p,'(');
        write_expr(outfile,p^.p1);
        write(outfile,')');
        end;
    t_typespec :
      begin
      write_cast_type(outfile,p^.p1);
      write(outfile,'(');
      write_expr(outfile,p^.p2);
      write(outfile,')');
      end;
    t_bop :
      begin
      (* C character literals are integers; Pascal compares two characters directly *)
      lOrd:=IsIntegerOperator(p^.p)
            and not ((p^.str[1] in ['=','<','>']) and IsCharLiteral(p^.p1) and IsCharLiteral(p^.p2));
      write_operand(outfile,p^.p1,lOrd);
      write(outfile,p^.p);
      write_operand(outfile,p^.p2,lOrd);
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
      flush(outfile);
      end;
    t_funexprlist :
      begin
      if assigned(p^.p3) then
        begin
        write_cast_type(outfile,p^.p3);
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

// Returns true when aArg (t_arg) is the ellipsis argument.
function IsEllipsisArg(aArg : presobject) : Boolean;

begin
  Result:=assigned(aArg) and not assigned(aArg^.p1) and not assigned(aArg^.next);
end;


function HasEllipsis(aArgs : presobject) : Boolean;

begin
  Result:=false;
  while assigned(aArgs) and not Result do
    begin
    Result:=IsEllipsisArg(aArgs^.p1);
    aArgs:=aArgs^.next;
    end;
end;


procedure write_args(var outfile:text; p : presobject; aSkipEllipsis : Boolean);

var
    len,para : longint;
    old_in_args : boolean;
    varpara, refpara, arraypara : boolean;
    lElement, lInner, lPointer : presobject;
    hs : string;

begin
  para:=1;
  len:=0;
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
    if IsEllipsisArg(p^.p1) then
      begin
      if not aSkipEllipsis then
        begin
        write(outfile,'args:array of const');
        UsesArrayOfConst:=true;
        end;
      (* if variable number of args we must always pop *)
      no_pop:=false;
      break;
      end
    (* we need to correct this in the pp file after *)
    else
      begin
      (* generate a call by reference parameter ?       *)
      varpara:=IsVarPara(p);
      (* C++ reference parameter *)
      refpara:=assigned(p^.p1^.p2) and assigned(p^.p1^.p2^.p1) and (p^.p1^.p2^.p1^.typ=t_addrdef);
      arraypara:=assigned(p^.p1^.p2) and assigned(p^.p1^.p2^.p1) and (p^.p1^.p2^.p1^.typ=t_arraydef);
      if arraypara then
        varpara:=false;
      if varpara or refpara then
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
      if refpara then
        write_p_a_def(outfile,p^.p1^.p2^.p1^.p1,p^.p1^.p1)
      else if arraypara then
        begin
        (* an array parameter is a pointer to its element *)
        lElement:=p^.p1^.p2^.p1^.p1;
        lInner:=lElement;
        while assigned(lInner) and (lInner^.typ<>t_arraydef) do
          lInner:=lInner^.p1;
        if assigned(lInner) then
          write(outfile,'pointer')
        else
          begin
          lPointer:=NewType1(t_pointerdef,lElement);
          write_p_a_def(outfile,lPointer,p^.p1^.p1);
          lPointer^.p1:=nil;
          dispose(lPointer,done);
          end;
        end
      else if varpara then
      begin
        write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1^.p1);
      end
      else
        write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1);
      end;
    p:=p^.next;
    if assigned(p) and not (aSkipEllipsis and IsEllipsisArg(p^.p1)) then
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



procedure WriteProcVarDirectives(var aFile : text; aStdCall : Boolean);

begin
  if not is_procvar then
    exit;
  if aStdCall then
    write(aFile,';stdcall')
  else
    write(aFile,';cdecl');
  if is_varargs then
    write(aFile,';varargs');
  is_procvar:=false;
  is_varargs:=false;
end;


Procedure write_pointerdef(var outfile:text; p,simple_type : presobject);

var
  pointerwritten : Boolean;
  lVarArgs : Boolean;
  lName : AnsiString;

begin
  (* procedure variable ? *)
  if assigned(p^.p1) and (p^.p1^.typ=t_procdef) then
    begin
    lVarArgs:=HasEllipsis(p^.p1^.p2);
    (* distinguish between procedure and function *)
    if (simple_type^.typ=t_void) and (p^.p1^.p1=nil) then
      begin
      write(outfile,'procedure ');
      shift(10);
      (* write arguments *)
      if assigned(p^.p1^.p2) then
        write_args(outfile,p^.p1^.p2,true);
      flush(outfile);
      popshift;
      end
    else
      begin
      write(outfile,'function ');
      shift(9);
      (* write arguments *)
      if assigned(p^.p1^.p2) then
        write_args(outfile,p^.p1^.p2,true);
      write(outfile,':');
      flush(outfile);

      old_in_args:=in_args;
      (* write pointers as P.... instead of ^.... *)
      in_args:=true;
      write_p_a_def(outfile,p^.p1^.p1,simple_type);
      in_args:=old_in_args;
      popshift;
      end;
    is_procvar:=true;
    is_varargs:=lVarArgs;
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
      if (p^.p1=nil) and IsFunctionType(simple_type) then
        begin
        write_type_specifier(outfile,simple_type);
        pointerwritten:=true;
        end;
      if not pointerwritten and (p^.p1=nil) and UsePPointers then
        begin
        if (simple_type^.typ=t_id) then
          begin
          lName:=PointerName(simple_type^.p);
          write(outfile,lName);
          RegisterPointerChain(lName,pointer_level);
          pointerwritten:=true;
          end
        { structure }
        else if (simple_type^.typ in [t_uniondef,t_structdef]) and
                (simple_type^.p1=nil) and (simple_type^.p2^.typ=t_id) then
          begin
          lName:=PointerName(simple_type^.p2^.p);
          write(outfile,lName);
          RegisterPointerChain(lName,pointer_level);
          pointerwritten:=true;
          end;
        end;
      if not pointerwritten then
        begin
        if in_args then
          begin
          write(outfile,'P');
          pointerprefix:=true;
          Inc(pointer_level);
          end
        else
          write(outfile,'^');
        write_p_a_def(outfile,p^.p1,simple_type);
        if in_args then
          Dec(pointer_level);
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
    t_pointerdef,
    t_addrdef :
      Write_pointerdef(outfile,p,simple_type);
    t_arraydef :
      Write_arraydef(outfile,p,simple_type);
    t_id :
      write_type_specifier(outfile,p);
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
        PTypeList.Add('P'+PointerBaseName(p^.str));
        RegisterPointerChain('P'+PointerBaseName(p^.str),pointer_level-1);
      end
      else
        RegisterPointerChain('p'+p^.str,pointer_level-1);
    end
    else
      begin
      PTypeList.Add('P'+PointerBaseName(p^.str));
      RegisterPointerChain('P'+PointerBaseName(p^.str),pointer_level-1);
      end;
  if p^.skiptprefix then
    write(outfile,p^.p)
  else if pointerprefix then
    write(outfile,PointerBaseName(p^.str))
  else
    write(outfile,TypeName(p^.p));
end;


procedure write_type_specifier_pointer(var outfile:text; p : presobject);

var
  pointerwritten : Boolean;
  lName : AnsiString;

begin
  pointerwritten:=false;
  if (p^.p1^.typ=t_void) then
    begin
    write(outfile,'pointer');
    pointerwritten:=true;
    end
  else if IsFunctionType(p^.p1) then
    begin
    write_type_specifier(outfile,p^.p1);
    pointerwritten:=true;
    end
  else if UsePPointers then
    begin
    if (p^.p1^.typ=t_id) then
    begin
      lName:=PointerName(p^.p1^.p);
      write(outfile,lName);
      RegisterPointerChain(lName,pointer_level);
      pointerwritten:=true;
    end
    { structure }
    else if (p^.p1^.typ in [t_uniondef,t_structdef]) and
            (p^.p1^.p1=nil) and (p^.p1^.p2^.typ=t_id) then
    begin
      lName:=PointerName(p^.p1^.p2^.p);
      write(outfile,lName);
      RegisterPointerChain(lName,pointer_level);
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
      Inc(pointer_level);
      end
    else
      begin
        if UseCTypesUnit and (IsACType(p^.p1^.p)=False) then
          write(outfile,'^')
        else
          write(outfile,'p');
      end;
    write_type_specifier(outfile,p^.p1);
    if in_args then
      Dec(pointer_level);
    pointerprefix:=false;
    end;
end;

procedure write_enum_const(var outfile:text; hp1 : presobject; var lastexpr : presobject; var l : longint);

var
  error : integer;

begin
  write(outfile,aktspace,FixId(hp1^.p1^.p),' = ');
  RegisterPlainConst(hp1^.p1^.str);
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
      write(outfile,FixId(hp1^.p1^.p));
      if assigned(hp1^.p2) then
        begin
        write(outfile,' := ');
        in_enum_value:=true;
        write_expr(outfile,hp1^.p2);
        in_enum_value:=false;
        w:=w+6;(* strlen(hp1^.p); *)
        end;
      EnumMembers.Add(hp1^.p1^.p);
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
    WriteSectionMarker(outfile,'C');
    WriteSectionKeyword(outfile,copy(aktspace,1,length(aktspace)-2),'Const');
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
  mask : qword;
  flag_index : longint;
  current_level : longint;
    is_sized : boolean;

  // Ends the current flag field with the type that holds current_level bits.
  procedure CloseFlag;

  begin
    if current_level <= 16 then
      writeln(outfile,'word;')
    else if current_level <= 32 then
      writeln(outfile,'dword;')
    else
      writeln(outfile,'qword;');
    is_sized:=false;
  end;

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
          if assigned(hp3^.p1) and assigned(hp3^.p1^.p2) and
              (not assigned(hp3^.p1^.p3) or
              (hp3^.p1^.p3^.typ <> t_size_specifier)) then
            begin
            if is_sized then
              CloseFlag;

            write(outfile,aktspace,FixId(hp3^.p1^.p2^.p));
            write(outfile,' : ');
            shift(2);
            is_procvar:=false;
            (* a flexible array member becomes an array of one element *)
            if assigned(hp3^.p1^.p1) and (hp3^.p1^.p1^.typ=t_pointerdef) and hp3^.p1^.p1^.openarray then
              begin
              write(outfile,'array[0..0] of ');
              write_p_a_def(outfile,hp3^.p1^.p1^.p1,hp2^.p1);
              end
            else
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
              l:=0;
              error:=1;
              { can it be something else than a constant ? }
              { it can be a macro !! }
              if hp3^.p1^.p3^.p1^.typ=t_id then
                val(hp3^.p1^.p3^.p1^.str,l,error);
              (* a field that does not fit starts a new flag: 32 bits, 64 for wider fields *)
              if is_sized and (error=0) and (current_level+l>32)
                 and ((l<=32) or (current_level+l>64)) then
                CloseFlag;
              if not is_sized then
                begin
                current_power:=1;
                current_level:=0;
                inc(flag_index);
                write(outfile,aktspace,'flag',flag_index,' : ');
                end;
              must_write_packed_field:=true;
              is_sized:=true;
              BitFieldFlags.Values[hp3^.p1^.p2^.str]:=IntToStr(flag_index);
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
            WriteProcVarDirectives(outfile,false);
            writeln(outfile,';');
            end;
          hp3:=hp3^.next;
          end;
        hp1:=hp1^.next;
        end;
      if is_sized then
        CloseFlag;
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
        if assigned(hp3^.p1) and assigned(hp3^.p1^.p2) then
          begin
          write(outfile,aktspace,l,' : ( ');
          write(outfile,FixId(hp3^.p1^.p2^.p),' : ');
          shift(2);
          write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
          popshift;
          writeln(outfile,' );');
          inc(l);
          end;
        hp3:=hp3^.next;
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

procedure write_cast_type(var outfile:text; p : presobject);

var
  lOldInArgs : boolean;

begin
  lOldInArgs:=in_args;
  in_args:=true;
  write_type_specifier(outfile,p);
  in_args:=lOldInArgs;
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

function MayWritePointerTypeDef(const PN: AnsiString): Boolean;

begin
  Result:=WrittenPointers.IndexOf(PN)=-1;
end;

// Writes PN = ^TN with indentation aIndent unless PN was written before.
function WriteIndentedPointerTypeDef(var aFile : text; const aIndent, PN, TN: AnsiString): Boolean;

begin
  Result:=MayWritePointerTypeDef(PN);
  if Result then
    begin
    WrittenPointers.Add(PN);
    Writeln(aFile,aIndent,PN,' = ^',TN,';');
   end;
end;


function WritePointerTypeDef(var aFile : text; const PN, TN: AnsiString): Boolean;

begin
  Result:=WriteIndentedPointerTypeDef(aFile,aktspace,PN,TN);
end;


// Returns the Pascal name of the type the pointer type PN points to.
function PointerTarget(const PN : AnsiString) : AnsiString;

begin
  Result:=PointerTargets.Values[PN];
  if Result='' then
    Result:=TypeName(Copy(PN,2,Length(PN)-1));
end;


procedure RegisterPointerChain(const aPointer : AnsiString; aLevels : Integer);

var
  lName, lTarget : AnsiString;
  lLevel : Integer;

begin
  lTarget:=aPointer;
  for lLevel:=1 to aLevels do
    begin
    lName:='P'+lTarget;
    PTypeList.Add(lName);
    if PointerTargets.IndexOfName(lName)=-1 then
      PointerTargets.Values[lName]:=lTarget;
    lTarget:=lName;
    end;
end;


function IsFunctionType(aType : presobject) : Boolean;

begin
  Result:=assigned(aType) and (aType^.typ=t_id)
          and (FunctionTypes.IndexOf(PointerBaseName(aType^.str))<>-1);
end;


procedure RegisterFunctionType(const aName : AnsiString);

begin
  FunctionTypes.Add(PointerBaseName(aName));
end;


// Writes the pointer types to aTarget, and recursively the pointer types to those.
procedure WritePointersTo(var aFile : text; const aIndent, aTarget : AnsiString);

var
  lIndex : Integer;
  lName : AnsiString;

begin
  for lIndex:=0 to PTypeList.Count-1 do
    begin
    lName:=PTypeList[lIndex];
    if SameText(PointerTarget(lName),aTarget) then
      begin
      WriteIndentedPointerTypeDef(aFile,aIndent,lName,aTarget);
      WritePointersTo(aFile,aIndent,lName);
      end;
    end;
end;


function HoistProcVarElement(const aName : AnsiString; aChain, aType : presobject) : Boolean;

var
  lArray, lElement : presobject;
  lName : AnsiString;

begin
  Result:=false;
  lArray:=aChain;
  if not (assigned(lArray) and (lArray^.typ in [t_arraydef,t_pointerdef])) then
    exit;
  repeat
    lElement:=lArray^.p1;
    if not assigned(lElement) or not (lElement^.typ in [t_arraydef,t_pointerdef]) then
      exit;
    if (lElement^.typ=t_pointerdef) and assigned(lElement^.p1) and (lElement^.p1^.typ=t_procdef) then
      break;
    lArray:=lElement;
  until false;
  lName:=TypeName(aName);
  WriteSectionMarker(outfile,'T');
  if block_type<>bt_type then
    begin
    if not compactmode then
      writeln(outfile);
    WriteSectionKeyword(outfile,aktspace,'type');
    block_type:=bt_type;
    end;
  shift(2);
  write(outfile,aktspace,lName,' = ');
  write_p_a_def(outfile,lElement,aType);
  WriteProcVarDirectives(outfile,false);
  writeln(outfile,';');
  is_procvar:=false;
  WritePointerMarker(outfile,lName);
  popshift;
  dispose(lElement,done);
  lArray^.p1:=NewID(aName);
  Result:=true;
end;


procedure HoistStructProcVarElements(const aOwner : AnsiString; aType : presobject);

var
  lMembers, lMember, lDecls : presobject;

begin
  if not (assigned(aType) and (aType^.typ in [t_structdef,t_uniondef])) then
    exit;
  lMembers:=aType^.p1;
  while assigned(lMembers) do
    begin
    lMember:=lMembers^.p1;
    if assigned(lMember) and (lMember^.typ=t_memberdec) then
      begin
      lDecls:=lMember^.p2;
      while assigned(lDecls) do
        begin
        if assigned(lDecls^.p1) and assigned(lDecls^.p1^.p2) and assigned(lDecls^.p1^.p2^.p) then
          begin
          HoistStructProcVarElements(aOwner+'_'+lDecls^.p1^.p2^.str,lMember^.p1);
          HoistProcVarElement(aOwner+'_'+lDecls^.p1^.p2^.str,lDecls^.p1^.p1,lMember^.p1);
          end;
        lDecls:=lDecls^.next;
        end;
      end;
    lMembers:=lMembers^.next;
    end;
end;


procedure WritePointerMarker(var aFile : text; const TN : AnsiString);

begin
  if block_type<>bt_type then
    exit;
  if OpaqueTypes.IndexOf(TN)>=0 then
    DefinedOpaqueTypes.Add(TN);
  DeclaredTypes.Add(TN);
  DeclaredSequence.Add(TN);
  Writeln(aFile,PointerMarker,aktspace,TN);
end;


// Returns false when a type name in the tree aNode other than TN was declared at or after position aLimit.
function RefersToEarlierTypes(aNode : presobject; const TN : AnsiString; aLimit : Integer) : Boolean;

var
  lIndex : Integer;

begin
  Result:=true;
  if not assigned(aNode) then
    exit;
  if (aNode^.typ=t_id) and not aNode^.skiptprefix and not SameText(TypeName(aNode^.str),TN) then
    begin
    lIndex:=DeclaredSequence.IndexOf(TypeName(aNode^.str));
    if lIndex>=aLimit then
      exit(false);
    end;
  Result:=RefersToEarlierTypes(aNode^.p1,TN,aLimit) and RefersToEarlierTypes(aNode^.p2,TN,aLimit)
          and RefersToEarlierTypes(aNode^.p3,TN,aLimit) and RefersToEarlierTypes(aNode^.next,TN,aLimit);
end;


function CanMoveRecord(const TN : AnsiString; aType : presobject) : Boolean;

begin
  Result:=not OneTypeSection and (OpaqueSequence.IndexOfName(TN)>=0) and (DefinedOpaqueTypes.IndexOf(TN)<0)
          and RefersToEarlierTypes(aType^.p1,TN,StrToInt(OpaqueSequence.Values[TN]));
end;


procedure WriteMovedRecordStart(var aFile : text; const TN : AnsiString);

begin
  Writeln(aFile,MovedStartMarker,TN);
end;


procedure WriteMovedRecordEnd(var aFile : text; const TN : AnsiString);

begin
  Writeln(aFile,MovedEndMarker,TN);
end;


procedure CollectMovedRecords(aLines : TStringList);

var
  i : Integer;
  lName : AnsiString;
  lRecord : TStringList;

begin
  lRecord:=nil;
  i:=0;
  while i<aLines.Count do
    begin
    if (aLines[i]<>'') and (aLines[i][1]=MovedStartMarker) then
      begin
      lName:=Copy(aLines[i],2,Length(aLines[i])-1);
      lRecord:=TStringList.Create;
      MovedRecordLines.AddObject(lName,lRecord);
      aLines.Delete(i);
      end
    else if (aLines[i]<>'') and (aLines[i][1]=MovedEndMarker) then
      begin
      lRecord:=nil;
      aLines.Delete(i);
      end
    else if assigned(lRecord) then
      begin
      lRecord.Add(aLines[i]);
      aLines.Delete(i);
      end
    else
      inc(i);
    end;
end;


procedure WriteSectionMarker(var aFile : text; aKind : char);

begin
  Writeln(aFile,SectionMarker,aKind);
end;


procedure WriteSectionKeyword(var aFile : text; const aIndent, aKeyword : AnsiString);

begin
  Writeln(aFile,KeywordMarker,aIndent,aKeyword);
end;


function IsPlainConstExpr(aExpr : presobject) : Boolean;

var
  lStr : AnsiString;

begin
  Result:=true;
  if not assigned(aExpr) then
    exit;
  case aExpr^.typ of
    t_typespec :
      (* a cast to a base type *)
      exit(assigned(aExpr^.p1) and (aExpr^.p1^.typ=t_id) and aExpr^.p1^.skiptprefix and IsPlainConstExpr(aExpr^.p2));
    t_id :
      if not aExpr^.skiptprefix then
        begin
        lStr:=aExpr^.str;
        if (lStr='') or not ((lStr[1] in ['0'..'9','''','#','$','&']) or (PlainConsts.IndexOf(lStr)>=0)) then
          exit(false);
        end;
    t_funexprlist :
      exit(false);
  end;
  Result:=IsPlainConstExpr(aExpr^.p1) and IsPlainConstExpr(aExpr^.p2) and IsPlainConstExpr(aExpr^.p3)
          and IsPlainConstExpr(aExpr^.next);
end;


procedure RegisterPlainConst(const aName : AnsiString);

begin
  PlainConsts.Add(aName);
end;


// Returns true when the line aTrimmed, without leading spaces, is a whole comment line or starts a comment;
// aClose is set to the text that closes the comment when it continues on the next lines, '' otherwise.
function IsCommentLine(const aTrimmed : AnsiString; var aClose : AnsiString) : Boolean;

var
  lEnd : Integer;

begin
  aClose:='';
  Result:=false;
  if (aTrimmed<>'') and (aTrimmed[1]='{') and (Copy(aTrimmed,1,2)<>'{$') then
    begin
    lEnd:=Pos('}',aTrimmed);
    if lEnd=0 then
      aClose:='}';
    Result:=(lEnd=0) or (lEnd=Length(TrimRight(aTrimmed)));
    end
  else if Copy(aTrimmed,1,2)='(*' then
    begin
    lEnd:=Pos('*)',aTrimmed);
    if lEnd=0 then
      aClose:='*)';
    Result:=(lEnd=0) or (lEnd=Length(TrimRight(aTrimmed))-1);
    end;
end;


procedure ArrangeSections(aLines : TStringList);

var
  lConsts, lTypes, lRest, lPending : TStringList;
  lKind, lRestKind : char;
  lConstKeyword, lHasTypes, lHasConsts : Boolean;
  i : Integer;
  lLine, lTrimmed, lClose, lIndent : AnsiString;

  // Adds the pending comments and aLine to aStream.
  procedure AddContent(aStream : TStringList; const aLine : AnsiString);

  begin
    aStream.AddStrings(lPending);
    lPending.Clear;
    aStream.Add(aLine);
  end;

begin
  lConsts:=TStringList.Create;
  lTypes:=TStringList.Create;
  lRest:=TStringList.Create;
  lPending:=TStringList.Create;
  if compactmode then
    lIndent:=''
  else
    lIndent:='  ';
  lKind:='F';
  lRestKind:=#0;
  lConstKeyword:=false;
  lHasTypes:=false;
  lHasConsts:=false;
  lClose:='';
  for i:=0 to aLines.Count-1 do
    begin
    lLine:=aLines[i];
    lTrimmed:=TrimLeft(lLine);
    if lClose<>'' then
      begin
      (* inside a comment of several lines *)
      lPending.Add(lLine);
      if Pos(lClose,lLine)>0 then
        lClose:='';
      end
    else if lTrimmed='' then
      lPending.Add(lLine)
    else if lLine[1]=SectionMarker then
      lKind:=lLine[2]
    else if lLine[1]=KeywordMarker then
      (* the sections are written again below *)
    else if (Copy(lTrimmed,1,2)='{$') and not SameText(Copy(lTrimmed,1,4),'{$i ')
            and not SameText(Copy(lTrimmed,1,9),'{$include') then
      begin
      (* conditions and switches apply to every section *)
      lConsts.Add(lLine);
      lTypes.Add(lLine);
      lRest.Add(lLine);
      lConstKeyword:=false;
      lRestKind:=#0;
      end
    else if IsCommentLine(lTrimmed,lClose) then
      lPending.Add(lLine)
    else if lKind='T' then
      begin
      lHasTypes:=true;
      AddContent(lTypes,lLine);
      end
    else if lKind='C' then
      begin
      if not lConstKeyword then
        lConsts.Add(lIndent+'const');
      lConstKeyword:=true;
      lHasConsts:=true;
      AddContent(lConsts,lLine);
      end
    else
      begin
      if lKind<>lRestKind then
        if lKind='D' then
          lRest.Add(lIndent+'const')
        else if lKind='V' then
          lRest.Add(lIndent+'var');
      lRestKind:=lKind;
      AddContent(lRest,lLine);
      end;
    end;
  lRest.AddStrings(lPending);
  aLines.Clear;
  if lHasConsts then
    aLines.AddStrings(lConsts);
  (* one type keyword: forward pointers are resolved within one type section *)
  if lHasTypes or HasHeaderPointers then
    begin
    aLines.Add(lIndent+'type');
    aLines.Add(HeaderPointersMarker+lIndent+'  ');
    aLines.AddStrings(lTypes);
    end;
  aLines.AddStrings(lRest);
  lPending.Free;
  lRest.Free;
  lTypes.Free;
  lConsts.Free;
end;


function IsDeclaredType(const TN : AnsiString) : Boolean;

begin
  Result:=DeclaredTypes.IndexOf(TN)>=0;
end;


procedure WriteOpaqueMarker(var aFile : text; const TN, AN : AnsiString);

begin
  OpaqueTypes.Add(TN);
  OpaqueSequence.Values[TN]:=IntToStr(DeclaredSequence.Count);
  DeclaredTypes.Add(TN);
  if AN<>'' then
    DeclaredTypes.Add(AN);
  Writeln(aFile,OpaqueMarker,aktspace,TN,' ',AN);
end;


procedure WriteRecordMarker(var aFile : text; const TN : AnsiString);

begin
  if block_type=bt_type then
    Writeln(aFile,RecordMarker,aktspace,TN);
end;


// Writes the alias AN = TN with the pointer types to AN.
procedure WriteAlias(var aFile : text; const aIndent, AN, TN : AnsiString);

begin
  Writeln(aFile,aIndent,AN,' = ',TN,';');
  WritePointersTo(aFile,aIndent,AN);
end;


function WriteMarkedPointers(var aFile : text; const aLine : AnsiString) : Boolean;

var
  lIndent, lTN, lAN : AnsiString;
  lPos, lIndex : Integer;
  lLines : TStringList;

begin
  Result:=(aLine<>'') and (aLine[1] in [PointerMarker,OpaqueMarker,RecordMarker,SectionMarker,KeywordMarker,
                                        HeaderPointersMarker]);
  if not Result then
    exit;
  case aLine[1] of
    SectionMarker :
      exit;
    KeywordMarker :
      begin
      Writeln(aFile,Copy(aLine,2,Length(aLine)-1));
      exit;
      end;
    HeaderPointersMarker :
      begin
      WriteHeaderPointers(aFile,Copy(aLine,2,Length(aLine)-1));
      exit;
      end;
  end;
  lPos:=2;
  while (lPos<=Length(aLine)) and (aLine[lPos]=' ') do
    Inc(lPos);
  lIndent:=Copy(aLine,2,lPos-2);
  lTN:=Copy(aLine,lPos,Length(aLine)-lPos+1);
  if aLine[1]=RecordMarker then
    begin
    (* pointers before the record, for the fields that refer to it *)
    WritePointersTo(aFile,lIndent,lTN);
    for lIndex:=0 to PendingAliases.Count-1 do
      if SameText(PendingAliases.Names[lIndex],lTN) then
        WritePointersTo(aFile,lIndent,PendingAliases.ValueFromIndex[lIndex]);
    exit;
    end;
  if aLine[1]=OpaqueMarker then
    begin
    lAN:=Trim(Copy(lTN,Pos(' ',lTN)+1,Length(lTN)));
    lTN:=Copy(lTN,1,Pos(' ',lTN)-1);
    lIndex:=MovedRecordLines.IndexOf(lTN);
    if lIndex>=0 then
      begin
      (* the record declared later, moved here *)
      if not OneTypeSection then
        Writeln(aFile,Copy(lIndent,1,Length(lIndent)-2),'type');
      if lAN<>'' then
        WritePointersTo(aFile,lIndent,lAN);
      lLines:=TStringList(MovedRecordLines.Objects[lIndex]);
      for lPos:=0 to lLines.Count-1 do
        if not WriteMarkedPointers(aFile,lLines[lPos]) then
          Writeln(aFile,lLines[lPos]);
      if lAN<>'' then
        WriteAlias(aFile,lIndent,lAN,lTN);
      exit;
      end;
    if DefinedOpaqueTypes.IndexOf(lTN)>=0 then
      begin
      if lAN<>'' then
        PendingAliases.Add(lTN+'='+lAN);
      exit;
      end;
    if not OneTypeSection then
      Writeln(aFile,Copy(lIndent,1,Length(lIndent)-2),'type');
    if PackRecords then
      Writeln(aFile,lIndent,lTN,' = packed record')
    else
      Writeln(aFile,lIndent,lTN,' = record');
    Writeln(aFile,lIndent,'    {undefined structure}');
    Writeln(aFile,lIndent,'  end;');
    WritePointersTo(aFile,lIndent,lTN);
    if lAN<>'' then
      WriteAlias(aFile,lIndent,lAN,lTN);
    exit;
    end;
  WritePointersTo(aFile,lIndent,lTN);
  for lIndex:=0 to PendingAliases.Count-1 do
    if SameText(PendingAliases.Names[lIndex],lTN) then
      WriteAlias(aFile,lIndent,PendingAliases.ValueFromIndex[lIndex],lTN);
end;


// Returns true when the pointer type PN belongs in the pointer list of the unit header.
function IsHeaderPointer(const PN : AnsiString) : Boolean;

var
  lBase : AnsiString;

begin
  lBase:=PointerTarget(PN);
  while (PTypeList.IndexOf(lBase)<>-1) and not SameText(lBase,PN) do
    lBase:=PointerTarget(lBase);
  Result:=MayWritePointerTypeDef(PN) and (DeclaredTypes.IndexOf(lBase)=-1);
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

procedure WritePointerList(var headerfile: Text);

var
  I : Integer;
  MustWritePointers : Boolean;

begin
  I:=PTypeList.count-1;
  MustWritePointers:=False;
  While (Not MustWritePointers) and (I>=0) do
    begin
    MustWritePointers:=IsHeaderPointer(PTypelist[i]);
    Dec(I);
    end;
  if not MustWritePointers then
    exit;
  Writeln(headerfile,'Type');
  for i:=0 to (PTypeList.Count-1) do
    if IsHeaderPointer(PTypeList[i]) then
      WritePointerTypeDef(HeaderFile,PTypeList[i],PointerTarget(PTypeList[i]));
end;


function HasHeaderPointers : Boolean;

var
  i : Integer;

begin
  Result:=false;
  for i:=0 to PTypeList.Count-1 do
    if IsHeaderPointer(PTypeList[i]) or (OneTypeSection and MayWritePointerTypeDef(PTypeList[i])) then
      exit(true);
end;


procedure WriteHeaderPointers(var aFile : text; const aIndent : AnsiString);

var
  i : Integer;

begin
  (* with one type section, all pointer types come first, as forward pointers *)
  for i:=0 to PTypeList.Count-1 do
    if IsHeaderPointer(PTypeList[i]) or (OneTypeSection and MayWritePointerTypeDef(PTypeList[i])) then
      WriteIndentedPointerTypeDef(aFile,aIndent,PTypeList[i],PointerTarget(PTypeList[i]));
end;

procedure WriteFileHeader(var headerfile: Text);
var
 i: integer;
 originalstr: string;
begin
{ write unit header }
  if not includefile then
   begin
     if createdynlib or UsesArrayOfConst then
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
     writeln(headerfile,'const');
     writeln(headerfile,'  External_library=''',libfilename,'''; {Setup as you need}');
     writeln(headerfile);
   end;
  if (PTypeList.count <> 0) and not OneTypeSection then
    WritePointerList(headerfile);
  writeln(headerfile);
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

procedure WriteLibraryUses;

begin
  writeln(outfile,'  uses');
  writeln(outfile,'    SysUtils, dynlibs;');
  writeln(outfile);
end;


procedure WriteLibraryInitialization;

var
 I : Integer;

begin
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

initialization
  WrittenPointers:=TStringList.Create;
  WrittenPointers.Sorted:=true;
  // We must never write these, they are defined in the system unit
  WrittenPointers.Add('pansichar');
  WrittenPointers.Add('pchar');
  WrittenPointers.Add('pdouble');
  WrittenPointers.Add('plongint');
  WrittenPointers.Add('psmallint');
  WrittenPointers.Add('pshortint');
  WrittenPointers.Add('pbyte');
  WrittenPointers.Add('pint64');
  WrittenPointers.Add('pword');
  WrittenPointers.Add('pqword');
  WrittenPointers.Add('pextended');
  WrittenPointers.Add('plongword');
  WrittenPointers.Add('psizeuint');
  WrittenPointers.Add('psizeint');
  WrittenPointers.Add('pptrint');
  WrittenPointers.Add('pptruint');
  WrittenPointers.Add('pboolean');
  WrittenPointers.Add('pwidechar');
  WrittenPointers.Add('pucs4char');
  WrittenPointers.Add('ppointer');
  WrittenPointers.Add('ppansichar');
  WrittenPointers.Add('ppchar');
  WrittenPointers.Add('ppbyte');
  WrittenPointers.Add('ppdouble');
  WrittenPointers.Add('pplongint');
  WrittenPointers.Add('pppansichar');
  WrittenPointers.Add('pppchar');
  WrittenPointers.Add('pppointer');
  PointerTargets:=TStringList.Create;
  BitFieldFlags:=TStringList.Create;
  EnumMembers:=TStringList.Create;
  OpaqueTypes:=TStringList.Create;
  DefinedOpaqueTypes:=TStringList.Create;
  PendingAliases:=TStringList.Create;
  DeclaredSequence:=TStringList.Create;
  OpaqueSequence:=TStringList.Create;
  MovedRecordLines:=TStringList.Create;
  MovedRecordLines.OwnsObjects:=true;
  PlainConsts:=TStringList.Create;
  FunctionTypes:=TStringList.Create;
  FunctionTypes.Sorted:=true;
  FunctionTypes.Duplicates:=dupIgnore;
  DeclaredTypes:=TStringList.Create;
  DeclaredTypes.Sorted:=true;
  DeclaredTypes.Duplicates:=dupIgnore;

finalization
  DeclaredTypes.Free;
  FunctionTypes.Free;
  PointerTargets.Free;
  BitFieldFlags.Free;
  EnumMembers.Free;
  OpaqueTypes.Free;
  DefinedOpaqueTypes.Free;
  PendingAliases.Free;
  DeclaredSequence.Free;
  OpaqueSequence.Free;
  MovedRecordLines.Free;
  PlainConsts.Free;
  WrittenPointers.Free;
end.
