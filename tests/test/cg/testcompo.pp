{
  mips-linux had a bug that was only having an effect on
  the jvm compiler built on mips-linux system.

  In a_cmp_ref_reg_label method of Thlcgcpu in jvm/hlcgcpu.pas unit,
  in line 1310, a temp is allocated of size 56,
  corresponding to a temporary partial copy of the reference record
  for the part that is put on stack at $sp+16
  when a_load_ref_stack is called,
  but the temp is never copied back to coorect location $sp+16
.Lj518:
# Temp 192,56 allocated
	# Register a0 allocated
	.stabn 68,0,1310,.Ll540 - HLCGCPU$_$THLCGJVM_$__$$_A_CMP_REF_REG_LABEL$TASMLIST$TDEF$TOPCMP$TREFERENCE$TREGISTER$TASMLABEL
.Ll540:
# [1310] a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
	addiu	$a0,$fp,16
	# Register a1 allocated
	addiu	$a1,$sp,192
	# Register v0 allocated
	addiu	$v0,$zero,14

  This test tries to generate a simple example displaying the same problem
}

{$mode objfpc}

const 
  NR_NO=0;
  NR_EVAL_STACK_BASE=$55443322;
  NR_STACK_POINTER_REG=$55010101;
  NR_BASE=$BA;
  NR_INDEX=$1D;
  test_offset = $ABCDEF;

type

  TCGInt = Int64;
  Tai = class
    next : tai;
  end;

  Tdef = class
      size : longint;
      constructor create;
    end;

  TOrdDef = class(Tdef)
      high : TCgint;
    end;

  TSymbol = class
    end;
  TAsmLabel = class
    end;

  topcmp = (op_none,op_eq,op_gt,op_lt,op_ge,op_le,
    op_ne,op_and,op_or,op_xor);

  tregister = longint;

  treference = record
    base : ptruint;
    index : ptruint;
    offset : TCGInt;
    symbol : tsymbol;
  end;

  tasmop = (a_none,a_mov,a_swap,a_dup,a_putfield,a_getfield,
            a_checkcast,a_putstatic,a_getstatic,
            a_iload,a_istore);

  TAsmList = class
    first, last : tai;
    constructor create;
    procedure concat(next : tai);
  end;

  TaiCPU = class(tai)
    constructor op_none(op : tasmop);
    constructor op_reg(op : tasmop; reg : tregister);
    constructor op_ref(op : tasmop; ref : treference);
    constructor op_const(op : tasmop; val : tcgint);
  end;

  thlcgjvm = class
    fstackpos : longint;
    constructor create;
    function loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: tcgint): tasmop;
    procedure incstack(list: TasmList;slots: longint);
    function prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
    procedure a_load_reg_stack(list : TAsmList;size: tdef;reg: tregister);
    procedure a_load_ref_stack(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
    procedure a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    function loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: tcgint): tasmop;
  end;

var
  voidpointertype : torddef;
  gsym : tsymbol;
const
  test_count : longint = 0;
  error_count : longint = 0;

  procedure test(ref : treference);
    begin
      if (ref.base<>NR_BASE) or (ref.index <>NR_INDEX) or
         (ref.offset<>test_offset) or (ref.symbol<>gsym) then
        begin
          writeln('Error in generated code');
          inc(error_count);
        end;
      inc(test_count);
    end;

  procedure internalerror(v : longint);
    begin
      writeln('Internal error ',v);
    end;

  constructor tdef.create;
    begin
      size:=4;
    end;

   constructor tasmlist.create;
    begin
      first:=nil;
      last:=nil;
    end;
 
  procedure tasmlist.concat(next : tai);
    begin
      if not assigned(first) then
        begin
          first:=next;
          last:=next;
        end
      else
        begin
          last.next:=next;
          last:=next;
        end;
    end;

  constructor TaiCPU.op_none(op : tasmop);
    begin
    end;

  constructor TaiCPU.op_reg(op : tasmop; reg : tregister);
    begin
    end;

  constructor TaiCPU.op_ref(op : tasmop; ref : treference);
    begin
    end;

  constructor TaiCPU.op_const(op : tasmop; val : tcgint);
    begin
    end;


   procedure a_op_const_stack(list : tasmlist;op : topcmp;size:tdef;finishandval : TCGInt);
     begin
       list.concat(taicpu.op_const(a_none,finishandval));
     end;


  constructor thlcgjvm.create;
    begin
    end;

  procedure thlcgjvm.incstack(list: TasmList;slots: longint);
    begin
      if slots=0 then
        exit;
      inc(fstackpos,slots);
    end;


   procedure thlcgjvm.a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    var
      extraslots : longint; 
    begin
      writeln('a_cmp_ref_reg_label');
      a_load_reg_stack(list,size,reg);
      { First separated }
      extraslots:=prepare_stack_for_ref(list,ref,false);
      a_load_ref_stack(list,size,ref,extraslots);
      a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
    end;

  function thlcgjvm.loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: tcgint): tasmop;
    var
      size: longint;
    begin
      finishandval:=-1;
      size:=def.size;
      if isload then
        result:=a_iload
      else
        result:=a_istore;
    end;

  procedure thlcgjvm.a_load_reg_stack(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      writeln('a_load_reg_stack');
      opc:=loadstoreopc(size,true,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      incstack(list,1+ord(size.size>4));
    end;

  procedure thlcgjvm.a_load_ref_stack(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      writeln('a_load_ref_stack');
      test(ref);
      opc:=loadstoreopcref(size,true,ref,finishandval);
      incstack(list,1+ord(size.size>4)-extra_slots);
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
    end;

  function thlcgjvm.prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
    var
      href: treference;
    begin
      writeln('prepare_stack_for_ref');
      result:=0;
      test(ref);
        begin
          if (ref.base<>NR_NO) then
            begin
              if (ref.base<>NR_STACK_POINTER_REG) then
                begin
                  { regular field -> load self on the stack }
                  a_load_reg_stack(list,voidpointertype,ref.base);
                  if dup then
                    begin
                      list.concat(taicpu.op_none(a_dup));
                      incstack(list,1);
                    end;
                  result:=1;
                end
              else
                begin
                  if assigned(ref.symbol) then
                    internalerror(2010120523);
                end;
            end
          else
            begin
              { static field -> nothing to do here, except for validity check }
              if not assigned(ref.symbol) or
                 (ref.offset<>0) then
                internalerror(2010120525);
            end;
        end;
    end;

  function thlcgjvm.loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: tcgint): tasmop;
    const
                     { isload  static }
      getputopc: array[boolean,boolean] of tasmop =
        ((a_putfield,a_putstatic),
         (a_getfield,a_getstatic));
    begin
      if assigned(ref.symbol) then
        begin
          result:=getputopc[isload,ref.base=NR_NO];
          finishandval:=-1;
            case def.size of
              1: if (torddef(def).high>127) then
                   finishandval:=255;
              2: if (torddef(def).high>32767) then
                   finishandval:=65535;
            end;
        end;
    end;

var
  ref : treference;
  size : torddef;
  cmp_op : topcmp;
  reg : tregister;
  l : TAsmLabel;
  hlcg : thlcgjvm;
  list : TAsmlist;
begin
  writeln('Start of test');
  gsym:=tsymbol.create;
  ref.base:=NR_BASE;
  ref.index:=NR_INDEX;
  ref.offset:=test_offset;
  ref.symbol:=gsym;
  list:=tasmlist.create;
  l:=tasmlabel.create;
  size:=torddef.create;
  size.size:=1;
  size.high:=255; 
  voidpointertype:=torddef.create;
  voidpointertype.size:=sizeof(ptrint);
  voidpointertype.high:=TCGInt(high(ptrint)); 
  hlcg:=thlcgjvm.create;
  hlcg.a_cmp_ref_reg_label(list,size, cmp_op, ref, reg, l);
  writeln('End of test, number of test: ',test_count);
  if error_count > 0 then
    begin
      writeln(error_count,' errors found');
      halt(1);
    end;
end.

