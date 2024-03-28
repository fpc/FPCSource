{
    Copyright (c) 2019 by Free Pascal and Lazarus foundation

    Contains the assembler object for the WebAssembly

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,globals,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  cgbase,cgutils,cpubase,cpuinfo,ogbase,
  symtype,
  widestr;

    { fake, there are no "mov reg,reg" instructions here }
    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type
      TWasmBasicTypeList = array of TWasmBasicType;

      { TWasmGlobalAsmSymbol }

      TWasmGlobalAsmSymbol = class(TAsmSymbol)
      private
        FWasmGlobalType: TWasmBasicType;
        procedure SetWasmGlobalType(AValue: TWasmBasicType);
      public
        property WasmGlobalType: TWasmBasicType read FWasmGlobalType write SetWasmGlobalType;
      end;

      { TWasmValueStack }

      TWasmValueStack = class
      private
        FValStack: array of TWasmBasicType;
        function GetCount: Integer;
        function GetItems(AIndex: Integer): TWasmBasicType;
        procedure SetCount(AValue: Integer);
        procedure SetItems(AIndex: Integer; AValue: TWasmBasicType);
      public
        procedure Push(wbt: TWasmBasicType);
        function Pop: TWasmBasicType;
        property Items[AIndex: Integer]: TWasmBasicType read GetItems write SetItems; default;
        property Count: Integer read GetCount write SetCount;
      end;

      { TWasmControlFrame }

      PWasmControlFrame = ^TWasmControlFrame;
      TWasmControlFrame = record
        opcode: tasmop;
        start_types: TWasmBasicTypeList;
        end_types: TWasmBasicTypeList;
        height: Integer;
        unreachable: Boolean;
      end;

      { TWasmControlStack }

      TWasmControlStack = class
      private
        FControlStack: array of TWasmControlFrame;
        function GetCount: Integer;
        function GetItems(AIndex: Integer): TWasmControlFrame;
        function GetPItems(AIndex: Integer): PWasmControlFrame;
        procedure SetItems(AIndex: Integer; const AValue: TWasmControlFrame);
      public
        procedure Push(const wcf: TWasmControlFrame);
        function Pop: TWasmControlFrame;
        property Items[AIndex: Integer]: TWasmControlFrame read GetItems write SetItems; default;
        property PItems[AIndex: Integer]: PWasmControlFrame read GetPItems;
        property Count: Integer read GetCount;
      end;

      taicpu = class;

      TGetLocalTypeProc = function(localidx: Integer): TWasmBasicType of object;

      { TWasmValidationStacks }

      TWasmValidationStacks = class
      private
        FValueStack: TWasmValueStack;
        FCtrlStack: TWasmControlStack;
        FGetLocalType: TGetLocalTypeProc;
        FFuncType: TWasmFuncType;
        FEndFunctionReached: Boolean;
      public
        constructor Create(AGetLocalType: TGetLocalTypeProc; AFuncType: TWasmFuncType);
        destructor Destroy; override;

        procedure PushVal(vt: TWasmBasicType);
        function PopVal: TWasmBasicType;
        function PopVal(expect: TWasmBasicType): TWasmBasicType;
        function PopVal_RefType: TWasmBasicType;
        procedure PushVals(vals: TWasmBasicTypeList);
        function PopVals(vals: TWasmBasicTypeList): TWasmBasicTypeList;

        procedure PushCtrl(_opcode: tasmop; _in, _out: TWasmBasicTypeList);
        function PopCtrl: TWasmControlFrame;

        function label_types(const frame: TWasmControlFrame): TWasmBasicTypeList;
        procedure Unreachable;

        procedure Validate(a: taicpu);
      end;

      twasmstruc_stack = class;
      TAsmMapFuncResultType = (amfrtNoChange, amfrtNewAi, amfrtNewList, amfrtDeleteAi);
      TAsmMapFuncResult = record
        case typ: TAsmMapFuncResultType of
          amfrtNoChange: ();
          amfrtNewAi: (newai: tai);
          amfrtNewList: (newlist: TAsmList);
          amfrtDeleteAi: ();
      end;
      TAsmMapFunc = function(ai: tai; blockstack: twasmstruc_stack): TAsmMapFuncResult of object;
      TWasmLocalAllocator = function(wbt: TWasmBasicType): Integer of object;

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         is_br_generated_by_goto: boolean;

         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : aint);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);

         constructor op_sym_const(op : tasmop;_op1 : tasmsymbol;_op2 : aint);
         constructor op_sym_functype(op : tasmop;_op1 : tasmsymbol;_op2 : TWasmFuncType);

         constructor op_single(op : tasmop;_op1 : single);
         constructor op_double(op : tasmop;_op1 : double);

         constructor op_functype(op : tasmop; _op1: TWasmFuncType);

         procedure loadfunctype(opidx:longint;ft:TWasmFuncType);
         procedure loadsingle(opidx:longint;f:single);
         procedure loaddouble(opidx:longint;d:double);

         { register allocation }
         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;

         function Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
      end;

      taiwstype = (
        aitws_if,
        aitws_block,
        aitws_loop,
        aitws_try_delegate,
        aitws_try_catch
      );

      { taicpu_wasm_structured_instruction }

      taicpu_wasm_structured_instruction = class(tai)
      protected
        FLabel: TAsmLabel;
        FLabelIsNew: Boolean;
      public
        wstyp: taiwstype;

        constructor Create;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);virtual;abstract;
        procedure ConvertToFlatList(l: TAsmList);virtual;abstract;
        function getlabel: TAsmLabel;
      end;

      { tai_wasmstruc_if }

      tai_wasmstruc_if = class(taicpu_wasm_structured_instruction)
        if_instr: taicpu;
        then_asmlist: TAsmList;
        else_asmlist: TAsmList;

        constructor create_from(a_if_instr: taicpu; srclist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);override;
        procedure ConvertToFlatList(l: TAsmList);override;
        procedure ConvertToBrIf(list: TAsmList; local_alloc: TWasmLocalAllocator);
      end;

      { tai_wasmstruc_block }

      tai_wasmstruc_block = class(taicpu_wasm_structured_instruction)
        block_instr: taicpu;
        inner_asmlist: TAsmList;

        constructor create_from(a_block_instr: taicpu; srclist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);override;
        procedure ConvertToFlatList(l: TAsmList);override;
      end;

      { tai_wasmstruc_loop }

      tai_wasmstruc_loop = class(taicpu_wasm_structured_instruction)
        loop_instr: taicpu;
        inner_asmlist: TAsmList;

        constructor create_from(a_loop_instr: taicpu; srclist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);override;
        procedure ConvertToBr(list: TAsmList);
        procedure ConvertToFlatList(l: TAsmList);override;
      end;

      { tai_wasmstruc_try }

      tai_wasmstruc_try = class(taicpu_wasm_structured_instruction)
      private
        class function create_from(srclist: TAsmList): tai_wasmstruc_try;
      public
        try_asmlist: TAsmList;

        constructor internal_create(a_try_asmlist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure ConvertToFlatList(l: TAsmList);override;
      end;

      { tai_wasmstruc_try_delegate }

      tai_wasmstruc_try_delegate = class(tai_wasmstruc_try)
        delegate_instr: taicpu;

        constructor internal_create(first_ins: taicpu; a_try_asmlist, srclist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);override;
        procedure ConvertToFlatList(l: TAsmList);override;
      end;

      { tai_wasmstruc_try_catch }

      tai_wasmstruc_try_catch = class(tai_wasmstruc_try)
        catch_list: array of record
          catch_instr: taicpu;
          asmlist: TAsmList;
        end;
        catch_all_asmlist: TAsmList;

        constructor internal_create(first_ins: taicpu; a_try_asmlist, srclist: TAsmList);
        destructor Destroy; override;
        function getcopy:TLinkedListItem;override;
        procedure Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);override;
        procedure ConvertToFlatList(l: TAsmList);override;
      end;

      { twasmstruc_stack }

      twasmstruc_stack = class
      private
        FStack: array of taicpu_wasm_structured_instruction;
        function Get(Index: Integer): taicpu_wasm_structured_instruction;
      public
        procedure push(ins: taicpu_wasm_structured_instruction);
        procedure pop;
        property Items [Index: Integer]: taicpu_wasm_structured_instruction read Get;default;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

      TImpExpType= (
        ie_Func,   // functions
        ie_Table,  // tables (arrays of methods)
        ie_Memory, // memory reference
        ie_Global  // global variables
      );

      { tai_export_name }

      tai_export_name = class(tai)
        extname : ansistring; // external name
        intname : ansistring; // internal name
        symstype: TImpExpType;
        constructor create(const aextname, aintname: ansistring; asymtype: timpexptype);
      end;

      // local variable declaration

      { tai_local }

      tai_local = class(tai)
        bastyp: TWasmBasicType;
        name : string;
        first: boolean;
        last: boolean;
        constructor create(abasictype: TWasmBasicType; const aname: string = '');
      end;

      { tai_globaltype }

      tai_globaltype = class(tai)
        globalname: string;
        gtype: TWasmBasicType;
        immutable: boolean;
        is_external: boolean;
        is_global: boolean;
        sym       : TWasmGlobalAsmSymbol;
        constructor create(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean);
        constructor create_local(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
        constructor create_global(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      end;

      { tai_functype }

      tai_functype = class(tai)
        funcname: string;
        functype: TWasmFuncType;
        constructor create(const afuncname: string; afunctype: TWasmFuncType);
        destructor destroy;override;
      end;

      { tai_tagtype }

      tai_tagtype = class(tai)
        tagname: string;
        params: TWasmResultType;
        constructor create(const atagname: string; aparams: TWasmResultType);
      end;

      { tai_import_module }

      tai_import_module = class(tai)
        symname: string;
        importmodule: string;
        constructor create(const asymname, aimportmodule: string);
      end;

      { tai_import_name }

      tai_import_name = class(tai)
        symname: string;
        importname: string;
        constructor create(const asymname, aimportname: string);
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    procedure wasm_convert_to_structured_asmlist(var asmlist: TAsmList);
    procedure wasm_convert_to_flat_asmlist(var asmlist: TAsmList);
    procedure map_structured_asmlist(l: TAsmList; f: TAsmMapFunc);

implementation

uses
  ogwasm;

    function wasm_convert_first_item_to_structured(srclist: TAsmList): tai; forward;
    procedure map_structured_asmlist_inner(l: TAsmList; f: TAsmMapFunc; blockstack: twasmstruc_stack); forward;

    { TWasmGlobalAsmSymbol }

    procedure TWasmGlobalAsmSymbol.SetWasmGlobalType(AValue: TWasmBasicType);
      begin
        if FWasmGlobalType=AValue then
          Exit;
        if FWasmGlobalType<>wbt_Unknown then
          Internalerror(2024022503);
        FWasmGlobalType:=AValue;
      end;

    { TWasmValueStack }

    function TWasmValueStack.GetItems(AIndex: Integer): TWasmBasicType;
      var
        I: Integer;
      begin
        I:=High(FValStack)-AIndex;
        if (I<Low(FValStack)) or (I>High(FValStack)) then
          internalerror(2024011702);
        Result:=FValStack[I];
      end;

    procedure TWasmValueStack.SetCount(AValue: Integer);
      begin
        SetLength(FValStack,AValue);
      end;

    function TWasmValueStack.GetCount: Integer;
      begin
        Result:=Length(FValStack);
      end;

    procedure TWasmValueStack.SetItems(AIndex: Integer; AValue: TWasmBasicType);
      var
        I: Integer;
      begin
        I:=High(FValStack)-AIndex;
        if (I<Low(FValStack)) or (I>High(FValStack)) then
          internalerror(2024011703);
        FValStack[I]:=AValue;
      end;

    procedure TWasmValueStack.Push(wbt: TWasmBasicType);
      begin
        SetLength(FValStack,Length(FValStack)+1);
        FValStack[High(FValStack)]:=wbt;
      end;

    function TWasmValueStack.Pop: TWasmBasicType;
      begin
        if Length(FValStack)=0 then
          internalerror(2024011701);
        Result:=FValStack[High(FValStack)];
        SetLength(FValStack,Length(FValStack)-1);
      end;

    { TWasmControlStack }

    function TWasmControlStack.GetItems(AIndex: Integer): TWasmControlFrame;
      var
        I: Integer;
      begin
        I:=High(FControlStack)-AIndex;
        if (I<Low(FControlStack)) or (I>High(FControlStack)) then
          internalerror(2024013101);
        Result:=FControlStack[I];
      end;

    function TWasmControlStack.GetPItems(AIndex: Integer): PWasmControlFrame;
      var
        I: Integer;
      begin
        I:=High(FControlStack)-AIndex;
        if (I<Low(FControlStack)) or (I>High(FControlStack)) then
          internalerror(2024013101);
        Result:=@(FControlStack[I]);
      end;

    function TWasmControlStack.GetCount: Integer;
      begin
        Result:=Length(FControlStack);
      end;

    procedure TWasmControlStack.SetItems(AIndex: Integer; const AValue: TWasmControlFrame);
      var
        I: Integer;
      begin
        I:=High(FControlStack)-AIndex;
        if (I<Low(FControlStack)) or (I>High(FControlStack)) then
          internalerror(2024013102);
        FControlStack[I]:=AValue;
      end;

    procedure TWasmControlStack.Push(const wcf: TWasmControlFrame);
      begin
        SetLength(FControlStack,Length(FControlStack)+1);
        FControlStack[High(FControlStack)]:=wcf;
      end;

    function TWasmControlStack.Pop: TWasmControlFrame;
      begin
        if Length(FControlStack)=0 then
          internalerror(2024013103);
        Result:=FControlStack[High(FControlStack)];
        SetLength(FControlStack,Length(FControlStack)-1);
      end;

    { TWasmValidationStacks }

    constructor TWasmValidationStacks.Create(AGetLocalType: TGetLocalTypeProc; AFuncType: TWasmFuncType);
      begin
        FEndFunctionReached:=False;
        FGetLocalType:=AGetLocalType;
        FValueStack:=TWasmValueStack.Create;
        FCtrlStack:=TWasmControlStack.Create;
        FFuncType:=AFuncType;
        PushCtrl(a_block,[],[]);
      end;

    destructor TWasmValidationStacks.Destroy;
      begin
        FValueStack.Free;
        FCtrlStack.Free;
        inherited Destroy;
      end;

    procedure TWasmValidationStacks.PushVal(vt: TWasmBasicType);
      begin
        FValueStack.Push(vt);
      end;

    function TWasmValidationStacks.PopVal: TWasmBasicType;
      begin
        if FValueStack.Count = FCtrlStack[0].height then
          begin
            Result:=wbt_Unknown;
            if not FCtrlStack[0].unreachable then
              internalerror(2024013104);
          end
        else
          Result:=FValueStack.Pop;
      end;

    function TWasmValidationStacks.PopVal(expect: TWasmBasicType): TWasmBasicType;
      begin
        Result:=wbt_Unknown;
        Result:=PopVal();
        if (Result<>expect) and (Result<>wbt_Unknown) and (expect<>wbt_Unknown) then
          internalerror(2024013105);
      end;

    function TWasmValidationStacks.PopVal_RefType: TWasmBasicType;
      begin
        Result:=wbt_Unknown;
        Result:=PopVal;
        if not (Result in (WasmReferenceTypes + [wbt_Unknown])) then
          internalerror(2024020501);
      end;

    procedure TWasmValidationStacks.PushVals(vals: TWasmBasicTypeList);
      var
        v: TWasmBasicType;
      begin
        for v in vals do
          PushVal(v);
      end;

    function TWasmValidationStacks.PopVals(vals: TWasmBasicTypeList): TWasmBasicTypeList;
      var
        I: Integer;
      begin
        Result:=nil;
        SetLength(Result,Length(vals));
        for I:=High(vals) downto Low(Vals) do
          Result[I]:=PopVal(vals[I]);
      end;

    procedure TWasmValidationStacks.PushCtrl(_opcode: tasmop; _in, _out: TWasmBasicTypeList);
      var
        frame: TWasmControlFrame;
      begin
        FillChar(frame,SizeOf(frame),0);
        with frame do
          begin
            opcode:=_opcode;
            start_types:=Copy(_in);
            end_types:=Copy(_out);
            height:=FValueStack.Count;
            unreachable:=False;
          end;
        FCtrlStack.Push(frame);
      end;

    function TWasmValidationStacks.PopCtrl: TWasmControlFrame;
      begin
        Result:=Default(TWasmControlFrame);
        if FCtrlStack.Count=0 then
          internalerror(2024013106);
        Result:=FCtrlStack[0];
        PopVals(Result.end_types);
        if FValueStack.Count<>Result.height then
          internalerror(2024013107);
        FCtrlStack.Pop;
      end;

    function TWasmValidationStacks.label_types(const frame: TWasmControlFrame): TWasmBasicTypeList;
      begin
        if frame.opcode=a_loop then
          Result:=frame.start_types
        else
          Result:=frame.end_types;
      end;

    procedure TWasmValidationStacks.Unreachable;
      var
        c: PWasmControlFrame;
      begin
        c:=FCtrlStack.PItems[0];
        FValueStack.Count:=c^.height;
        c^.unreachable:=true;
      end;

    procedure TWasmValidationStacks.Validate(a: taicpu);

      function GetLocalIndex: Integer;
        begin
          Result:=-1;
          with a do
            begin
              if ops<>1 then
                internalerror(2024020801);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        internalerror(2024020802);
                      if ref^.base<>NR_STACK_POINTER_REG then
                        internalerror(2024020803);
                      if ref^.index<>NR_NO then
                        internalerror(2024020804);
                      Result:=ref^.offset;
                    end;
                  top_const:
                    Result:=val;
                  else
                    internalerror(2024020805);
                end;
            end;
        end;

      var
        frame: TWasmControlFrame;
        n: TCGInt;
      begin
        if FEndFunctionReached then
          internalerror(2024022602);
        case a.opcode of
          a_nop:
            ;
          a_i32_const:
            PushVal(wbt_i32);
          a_i64_const:
            PushVal(wbt_i64);
          a_f32_const:
            PushVal(wbt_f32);
          a_f64_const:
            PushVal(wbt_f64);
          a_i32_add,
          a_i32_sub,
          a_i32_mul,
          a_i32_div_s, a_i32_div_u,
          a_i32_rem_s, a_i32_rem_u,
          a_i32_and,
          a_i32_or,
          a_i32_xor,
          a_i32_shl,
          a_i32_shr_s, a_i32_shr_u,
          a_i32_rotl,
          a_i32_rotr:
            begin
              PopVal(wbt_i32);
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_add,
          a_i64_sub,
          a_i64_mul,
          a_i64_div_s, a_i64_div_u,
          a_i64_rem_s, a_i64_rem_u,
          a_i64_and,
          a_i64_or,
          a_i64_xor,
          a_i64_shl,
          a_i64_shr_s, a_i64_shr_u,
          a_i64_rotl,
          a_i64_rotr:
            begin
              PopVal(wbt_i64);
              PopVal(wbt_i64);
              PushVal(wbt_i64);
            end;
          a_f32_add,
          a_f32_sub,
          a_f32_mul,
          a_f32_div,
          a_f32_min,
          a_f32_max,
          a_f32_copysign:
            begin
              PopVal(wbt_f32);
              PopVal(wbt_f32);
              PushVal(wbt_f32);
            end;
          a_f64_add,
          a_f64_sub,
          a_f64_mul,
          a_f64_div,
          a_f64_min,
          a_f64_max,
          a_f64_copysign:
            begin
              PopVal(wbt_f64);
              PopVal(wbt_f64);
              PushVal(wbt_f64);
            end;
          a_i32_clz,
          a_i32_ctz,
          a_i32_popcnt:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_clz,
          a_i64_ctz,
          a_i64_popcnt:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_i64);
            end;
          a_f32_abs,
          a_f32_neg,
          a_f32_sqrt,
          a_f32_ceil,
          a_f32_floor,
          a_f32_trunc,
          a_f32_nearest:
            begin
              PopVal(wbt_f32);
              PushVal(wbt_f32);
            end;
          a_f64_abs,
          a_f64_neg,
          a_f64_sqrt,
          a_f64_ceil,
          a_f64_floor,
          a_f64_trunc,
          a_f64_nearest:
            begin
              PopVal(wbt_f64);
              PushVal(wbt_f64);
            end;
          a_i32_eqz:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_eqz:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_i32);
            end;
          a_i32_eq,
          a_i32_ne,
          a_i32_lt_s, a_i32_lt_u,
          a_i32_gt_s, a_i32_gt_u,
          a_i32_le_s, a_i32_le_u,
          a_i32_ge_s, a_i32_ge_u:
            begin
              PopVal(wbt_i32);
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_eq,
          a_i64_ne,
          a_i64_lt_s, a_i64_lt_u,
          a_i64_gt_s, a_i64_gt_u,
          a_i64_le_s, a_i64_le_u,
          a_i64_ge_s, a_i64_ge_u:
            begin
              PopVal(wbt_i64);
              PopVal(wbt_i64);
              PushVal(wbt_i32);
            end;
          a_f32_eq,
          a_f32_ne,
          a_f32_lt,
          a_f32_gt,
          a_f32_le,
          a_f32_ge:
            begin
              PopVal(wbt_f32);
              PopVal(wbt_f32);
              PushVal(wbt_i32);
            end;
          a_f64_eq,
          a_f64_ne,
          a_f64_lt,
          a_f64_gt,
          a_f64_le,
          a_f64_ge:
            begin
              PopVal(wbt_f64);
              PopVal(wbt_f64);
              PushVal(wbt_i32);
            end;
          a_i32_extend8_s,
          a_i32_extend16_s:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_extend8_s,
          a_i64_extend16_s,
          a_i64_extend32_s:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_i64);
            end;
          a_i32_wrap_i64:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_i32);
            end;
          a_i64_extend_i32_s,
          a_i64_extend_i32_u:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i64);
            end;
          a_i32_trunc_f32_s,
          a_i32_trunc_f32_u,
          a_i32_trunc_sat_f32_s,
          a_i32_trunc_sat_f32_u:
            begin
              PopVal(wbt_f32);
              PushVal(wbt_i32);
            end;
          a_i32_trunc_f64_s,
          a_i32_trunc_f64_u,
          a_i32_trunc_sat_f64_s,
          a_i32_trunc_sat_f64_u:
            begin
              PopVal(wbt_f64);
              PushVal(wbt_i32);
            end;
          a_i64_trunc_f32_s,
          a_i64_trunc_f32_u,
          a_i64_trunc_sat_f32_s,
          a_i64_trunc_sat_f32_u:
            begin
              PopVal(wbt_f32);
              PushVal(wbt_i64);
            end;
          a_i64_trunc_f64_s,
          a_i64_trunc_f64_u,
          a_i64_trunc_sat_f64_s,
          a_i64_trunc_sat_f64_u:
            begin
              PopVal(wbt_f64);
              PushVal(wbt_i64);
            end;
          a_f32_demote_f64:
            begin
              PopVal(wbt_f64);
              PushVal(wbt_f32);
            end;
          a_f64_promote_f32:
            begin
              PopVal(wbt_f32);
              PushVal(wbt_f64);
            end;
          a_f32_convert_i32_s,
          a_f32_convert_i32_u:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_f32);
            end;
          a_f32_convert_i64_s,
          a_f32_convert_i64_u:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_f32);
            end;
          a_f64_convert_i32_s,
          a_f64_convert_i32_u:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_f64);
            end;
          a_f64_convert_i64_s,
          a_f64_convert_i64_u:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_f64);
            end;
          a_i32_reinterpret_f32:
            begin
              PopVal(wbt_f32);
              PushVal(wbt_i32);
            end;
          a_i64_reinterpret_f64:
            begin
              PopVal(wbt_f64);
              PushVal(wbt_i64);
            end;
          a_f32_reinterpret_i32:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_f32);
            end;
          a_f64_reinterpret_i64:
            begin
              PopVal(wbt_i64);
              PushVal(wbt_f64);
            end;
          a_ref_null_externref:
            PushVal(wbt_externref);
          a_ref_null_funcref:
            PushVal(wbt_funcref);
          a_ref_is_null:
            begin
              PopVal_RefType;
              PushVal(wbt_i32);
            end;
          a_drop:
            PopVal;
          a_unreachable:
            Unreachable;
          a_i32_load,
          a_i32_load16_s, a_i32_load16_u,
          a_i32_load8_s, a_i32_load8_u:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_i64_load,
          a_i64_load32_s, a_i64_load32_u,
          a_i64_load16_s, a_i64_load16_u,
          a_i64_load8_s, a_i64_load8_u:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i64);
            end;
          a_f32_load:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_f32);
            end;
          a_f64_load:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_f64);
            end;
          a_i32_store,
          a_i32_store16,
          a_i32_store8:
            begin
              PopVal(wbt_i32);
              PopVal(wbt_i32);
            end;
          a_i64_store,
          a_i64_store32,
          a_i64_store16,
          a_i64_store8:
            begin
              PopVal(wbt_i64);
              PopVal(wbt_i32);
            end;
          a_f32_store:
            begin
              PopVal(wbt_f32);
              PopVal(wbt_i32);
            end;
          a_f64_store:
            begin
              PopVal(wbt_f64);
              PopVal(wbt_i32);
            end;
          a_memory_size:
            PushVal(wbt_i32);
          a_memory_grow:
            begin
              PopVal(wbt_i32);
              PushVal(wbt_i32);
            end;
          a_memory_fill,
          a_memory_copy:
            begin
              PopVal(wbt_i32);
              PopVal(wbt_i32);
              PopVal(wbt_i32);
            end;
          a_local_get:
            PushVal(FGetLocalType(GetLocalIndex));
          a_local_set:
            PopVal(FGetLocalType(GetLocalIndex));
          a_local_tee:
            begin
              PopVal(FGetLocalType(GetLocalIndex));
              PushVal(FGetLocalType(GetLocalIndex));
            end;
          a_global_get,
          a_global_set:
            begin
              if a.ops<>1 then
                internalerror(2024022504);
              if a.oper[0]^.typ<>top_ref then
                internalerror(2024022505);
              if not assigned(a.oper[0]^.ref^.symbol) then
                internalerror(2024022506);
              if (a.oper[0]^.ref^.base<>NR_NO) or (a.oper[0]^.ref^.index<>NR_NO) or (a.oper[0]^.ref^.offset<>0) then
                internalerror(2024022507);
              if a.oper[0]^.ref^.symbol.typ<>AT_WASM_GLOBAL then
                internalerror(2024022508);
              case a.opcode of
                a_global_get:
                  PushVal(TWasmGlobalAsmSymbol(a.oper[0]^.ref^.symbol).WasmGlobalType);
                a_global_set:
                  PopVal(TWasmGlobalAsmSymbol(a.oper[0]^.ref^.symbol).WasmGlobalType);
                else
                  internalerror(2024022509);
              end;
            end;
          a_call:
            begin
              if a.ops<>2 then
                internalerror(2024022501);
              if a.oper[1]^.typ<>top_functype then
                internalerror(2024022502);
              PopVals(a.oper[1]^.functype.params);
              PushVals(a.oper[1]^.functype.results);
            end;
          a_call_indirect:
            begin
              if a.ops<>1 then
                internalerror(2024022401);
              if a.oper[0]^.typ<>top_functype then
                internalerror(2024022402);
              PopVal(wbt_i32);
              PopVals(a.oper[0]^.functype.params);
              PushVals(a.oper[0]^.functype.results);
            end;
          a_if,
          a_block,
          a_loop,
          a_try:
            begin
              if a.opcode=a_if then
                PopVal(wbt_i32);
              if a.ops>1 then
                internalerror(2024022510);
              if a.ops=0 then
                PushCtrl(a.opcode,[],[])
              else
                begin
                  if a.oper[0]^.typ<>top_functype then
                    internalerror(2024022511);
                  PopVals(a.oper[0]^.functype.params);
                  PushCtrl(a.opcode,a.oper[0]^.functype.params,a.oper[0]^.functype.results);
                end;
            end;
          a_else:
            begin
              frame:=PopCtrl;
              if frame.opcode<>a_if then
                internalerror(2024022512);
              PushCtrl(a_else,frame.start_types,frame.end_types);
            end;
          a_catch:
            begin
              frame:=PopCtrl;
              if (frame.opcode<>a_try) and (frame.opcode<>a_catch) then
                internalerror(2024022701);
              PushCtrl(a_catch,frame.start_types,frame.end_types);
            end;
          a_end_if:
            begin
              frame:=PopCtrl;
              if (frame.opcode<>a_if) and (frame.opcode<>a_else) then
                internalerror(2024022513);
              PushVals(frame.end_types);
            end;
          a_end_block:
            begin
              frame:=PopCtrl;
              if frame.opcode<>a_block then
                internalerror(2024022514);
              PushVals(frame.end_types);
            end;
          a_end_loop:
            begin
              frame:=PopCtrl;
              if frame.opcode<>a_loop then
                internalerror(2024022515);
              PushVals(frame.end_types);
            end;
          a_end_try:
            begin
              frame:=PopCtrl;
              if (frame.opcode<>a_try) and (frame.opcode<>a_catch) then
                internalerror(2024022702);
              PushVals(frame.end_types);
            end;
          a_br:
            begin
              if a.ops<>1 then
                internalerror(2024022516);
              if a.oper[0]^.typ<>top_const then
                internalerror(2024022517);
              n:=a.oper[0]^.val;
              if FCtrlStack.Count < n then
                internalerror(2024022518);
              PopVals(label_types(FCtrlStack[n]));
              Unreachable;
            end;
          a_br_if:
            begin
              if a.ops<>1 then
                internalerror(2024022519);
              if a.oper[0]^.typ<>top_const then
                internalerror(2024022520);
              n:=a.oper[0]^.val;
              if FCtrlStack.Count < n then
                internalerror(2024022521);
              PopVal(wbt_i32);
              PopVals(label_types(FCtrlStack[n]));
              PushVals(label_types(FCtrlStack[n]));
            end;
          a_throw:
            Unreachable;
          a_rethrow:
            Unreachable;
          a_return:
            begin
              PopVals(FFuncType.results);
              Unreachable;
            end;
          a_end_function:
            FEndFunctionReached:=True;
          else
            internalerror(2024030502);
        end;
      end;

    { twasmstruc_stack }

    function twasmstruc_stack.Get(Index: Integer): taicpu_wasm_structured_instruction;
      begin
{$push}{$r+,q+}
        Result:=FStack[High(FStack)-Index];
{$pop}
      end;

    procedure twasmstruc_stack.push(ins: taicpu_wasm_structured_instruction);
      begin
        SetLength(FStack,Length(FStack)+1);
        FStack[High(FStack)]:=ins;
      end;

    procedure twasmstruc_stack.pop;
      begin
        SetLength(FStack,Length(FStack)-1);
      end;

    { taicpu_wasm_structured_instruction }

    constructor taicpu_wasm_structured_instruction.Create;
      begin
        inherited;
        typ:=ait_wasm_structured_instruction;
      end;

    function taicpu_wasm_structured_instruction.getlabel: TAsmLabel;
      begin
        if not assigned(FLabel) then
          begin
            current_asmdata.getjumplabel(FLabel);
            FLabelIsNew:=true;
          end;
        result:=FLabel;
      end;

    { tai_wasmstruc_if }

    constructor tai_wasmstruc_if.create_from(a_if_instr: taicpu; srclist: TAsmList);
      var
        p: tai;
        ThenDone, ElsePresent, ElseDone: Boolean;
      begin
        wstyp:=aitws_if;
        inherited Create;
        if assigned(a_if_instr.Previous) or assigned(a_if_instr.Next) then
          internalerror(2023100301);
        if_instr:=a_if_instr;

        then_asmlist:=TAsmList.Create;

        ThenDone:=False;
        ElsePresent:=False;
        repeat
          p:=tai(srclist.First);
          if not assigned(p) then
            internalerror(2023100302);
          if (p.typ=ait_instruction) and (taicpu(p).opcode in [a_else,a_end_if,a_end_block,a_end_loop,a_end_try,a_catch,a_catch_all,a_delegate]) then
            begin
              srclist.Remove(p);
              case taicpu(p).opcode of
                a_else:
                  begin
                    ThenDone:=True;
                    ElsePresent:=True;
                  end;
                a_end_if:
                  ThenDone:=True;
                else
                  internalerror(2023100501);
              end;
            end
          else
            then_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
        until ThenDone;

        if ElsePresent then
          begin
            else_asmlist:=TAsmList.Create;
            ElseDone:=False;
            repeat
              p:=tai(srclist.First);
              if not assigned(p) then
                internalerror(2023100303);
              if (p.typ=ait_instruction) and (taicpu(p).opcode=a_end_if) then
                begin
                  srclist.Remove(p);
                  ElseDone:=True;
                end
              else
                else_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
            until ElseDone;
          end;
      end;

    destructor tai_wasmstruc_if.Destroy;
      begin
        then_asmlist.free;
        else_asmlist.free;
        if_instr.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_if.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_if;
      begin
        p:=tai_wasmstruc_if(inherited getcopy);
        if assigned(if_instr) then
          p.if_instr:=taicpu(if_instr.getcopy);
        if assigned(then_asmlist) then
          begin
            p.then_asmlist:=TAsmList.Create;
            p.then_asmlist.concatListcopy(then_asmlist);
          end;
        if assigned(else_asmlist) then
          begin
            p.else_asmlist:=TAsmList.Create;
            p.else_asmlist.concatListcopy(else_asmlist);
          end;
        getcopy:=p;
      end;

    procedure tai_wasmstruc_if.Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);
      begin
        blockstack.push(self);
        map_structured_asmlist_inner(then_asmlist,f,blockstack);
        map_structured_asmlist_inner(else_asmlist,f,blockstack);
        blockstack.pop;
      end;

    procedure tai_wasmstruc_if.ConvertToFlatList(l: TAsmList);
      begin
        l.Concat(if_instr);
        if_instr:=nil;
        l.concatList(then_asmlist);
        if assigned(else_asmlist) then
          begin
            l.Concat(taicpu.op_none(A_ELSE));
            l.concatList(else_asmlist);
          end;
        l.Concat(taicpu.op_none(a_end_if));
        if FLabelIsNew then
          l.concat(tai_label.create(FLabel));
      end;

    procedure tai_wasmstruc_if.ConvertToBrIf(list: TAsmList; local_alloc: TWasmLocalAllocator);
      var
        res_ft: TWasmFuncType;
        save_if_reg: Integer;
        save_param_reg: array of Integer;
        save_result_reg: array of Integer;

        procedure AllocateLocalsForSavingParamsAndResult;
          var
            i: Integer;
          begin
            if not assigned(res_ft) then
              exit;
            if length(res_ft.params)<>0 then
              begin
                save_if_reg:=local_alloc(wbt_i32);
                SetLength(save_param_reg,length(res_ft.params));
                for i:=low(res_ft.params) to high(res_ft.params) do
                  save_param_reg[i]:=local_alloc(res_ft.params[i]);
              end;
            if length(res_ft.results)<>0 then
              begin
                SetLength(save_result_reg,length(res_ft.results));
                for i:=low(res_ft.results) to high(res_ft.results) do
                  save_result_reg[i]:=local_alloc(res_ft.results[i]);
              end;
          end;

        procedure SaveParams;
          var
            i: Integer;
          begin
            if (not assigned(res_ft)) or (length(res_ft.params)=0) then
              exit;
            list.concat(taicpu.op_const(a_local_set,save_if_reg));
            for i:=high(res_ft.params) downto low(res_ft.params) do
              list.concat(taicpu.op_const(a_local_set,save_param_reg[i]));
            list.concat(taicpu.op_const(a_local_get,save_if_reg));
          end;

        procedure RestoreParams;
          var
            i: Integer;
          begin
            if (not assigned(res_ft)) or (length(res_ft.params)=0) then
              exit;
            for i:=low(res_ft.params) to high(res_ft.params) do
              list.concat(taicpu.op_const(a_local_get,save_param_reg[i]));
          end;

        procedure SaveResults;
          var
            i: Integer;
          begin
            if (not assigned(res_ft)) or (length(res_ft.results)=0) then
              exit;
            for i:=high(res_ft.results) downto low(res_ft.results) do
              list.concat(taicpu.op_const(a_local_set,save_result_reg[i]));
          end;

        procedure RestoreResults;
          var
            i: Integer;
          begin
            if (not assigned(res_ft)) or (length(res_ft.results)=0) then
              exit;
            for i:=low(res_ft.results) to high(res_ft.results) do
              list.concat(taicpu.op_const(a_local_get,save_result_reg[i]));
          end;

      var
        then_label: TAsmLabel;
      begin
        if if_instr.ops>1 then
          internalerror(2023101701);
        if (if_instr.ops=1) and (if_instr.oper[0]^.typ=top_functype) then
          res_ft:=if_instr.oper[0]^.functype
        else
          res_ft:=nil;
        AllocateLocalsForSavingParamsAndResult;

        current_asmdata.getjumplabel(then_label);
        SaveParams;
        list.concat(taicpu.op_sym(a_br_if,then_label));
        RestoreParams;
        if assigned(else_asmlist) then
          list.concatList(else_asmlist);
        SaveResults;
        list.concat(taicpu.op_sym(a_br, GetLabel));
        list.concat(tai_label.create(then_label));
        RestoreParams;
        list.concatList(then_asmlist);
        SaveResults;
        list.concat(tai_label.create(GetLabel));
        RestoreResults;
      end;

    { tai_wasmstruc_block }

    constructor tai_wasmstruc_block.create_from(a_block_instr: taicpu; srclist: TAsmList);
      var
        Done: Boolean;
        p: tai;
      begin
        wstyp:=aitws_block;
        inherited Create;
        if assigned(a_block_instr.Previous) or assigned(a_block_instr.Next) then
          internalerror(2023100304);
        block_instr:=a_block_instr;

        inner_asmlist:=TAsmList.Create;

        Done:=False;
        repeat
          p:=tai(srclist.First);
          if not assigned(p) then
            internalerror(2023100305);
          if (p.typ=ait_instruction) and (taicpu(p).opcode=a_end_block) then
            begin
              srclist.Remove(p);
              Done:=True;
            end
          else
            inner_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
        until Done;
      end;

    destructor tai_wasmstruc_block.Destroy;
      begin
        inner_asmlist.free;
        block_instr.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_block.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_block;
      begin
        p:=tai_wasmstruc_block(inherited getcopy);
        if assigned(block_instr) then
          p.block_instr:=taicpu(block_instr.getcopy);
        if assigned(inner_asmlist) then
          begin
            p.inner_asmlist:=TAsmList.Create;
            p.inner_asmlist.concatListcopy(inner_asmlist);
          end;
        getcopy:=p;
      end;

    procedure tai_wasmstruc_block.Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);
      begin
        blockstack.push(self);
        map_structured_asmlist_inner(inner_asmlist,f,blockstack);
        blockstack.pop;
      end;

    procedure tai_wasmstruc_block.ConvertToFlatList(l: TAsmList);
      begin
        l.Concat(block_instr);
        block_instr:=nil;
        l.concatList(inner_asmlist);
        l.Concat(taicpu.op_none(a_end_block));
        if FLabelIsNew then
          l.concat(tai_label.create(FLabel));
      end;

    { tai_wasmstruc_loop }

    constructor tai_wasmstruc_loop.create_from(a_loop_instr: taicpu; srclist: TAsmList);
      var
        Done: Boolean;
        p: tai;
      begin
        wstyp:=aitws_loop;
        inherited Create;
        if assigned(a_loop_instr.Previous) or assigned(a_loop_instr.Next) then
          internalerror(2023100306);
        loop_instr:=a_loop_instr;

        inner_asmlist:=TAsmList.Create;

        Done:=False;
        repeat
          p:=tai(srclist.First);
          if not assigned(p) then
            internalerror(2023100307);
          if (p.typ=ait_instruction) and (taicpu(p).opcode=a_end_loop) then
            begin
              srclist.Remove(p);
              Done:=True;
            end
          else
            inner_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
        until Done;
      end;

    destructor tai_wasmstruc_loop.Destroy;
      begin
        inner_asmlist.free;
        loop_instr.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_loop.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_loop;
      begin
        p:=tai_wasmstruc_loop(inherited getcopy);
        if assigned(loop_instr) then
          p.loop_instr:=taicpu(loop_instr.getcopy);
        if assigned(inner_asmlist) then
          begin
            p.inner_asmlist:=TAsmList.Create;
            p.inner_asmlist.concatListcopy(inner_asmlist);
          end;
        getcopy:=p;
      end;

    procedure tai_wasmstruc_loop.Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);
      begin
        blockstack.push(self);
        map_structured_asmlist_inner(inner_asmlist,f,blockstack);
        blockstack.pop;
      end;

    procedure tai_wasmstruc_loop.ConvertToBr(list: TAsmList);
      begin
        list.concat(tai_label.create(GetLabel));
        list.concatList(inner_asmlist);
        list.concat(taicpu.op_sym(a_br,GetLabel));
      end;

    procedure tai_wasmstruc_loop.ConvertToFlatList(l: TAsmList);
      begin
        l.Concat(loop_instr);
        loop_instr:=nil;
        if FLabelIsNew then
          l.concat(tai_label.create(FLabel));
        l.concatList(inner_asmlist);
        l.Concat(taicpu.op_none(a_end_loop));
      end;

    { tai_wasmstruc_try }

    class function tai_wasmstruc_try.create_from(srclist: TAsmList): tai_wasmstruc_try;
      var
        Done: Boolean;
        p: tai;
        tmp_asmlist: TAsmList;
      begin
        result:=nil;
        tmp_asmlist:=TAsmList.Create;

        Done:=False;
        repeat
          p:=tai(srclist.First);
          if not assigned(p) then
            internalerror(2023100308);
          if (p.typ=ait_instruction) and (taicpu(p).opcode in [a_end_try,a_catch,a_catch_all,a_delegate]) then
            begin
              srclist.Remove(p);
              Done:=True;
            end
          else
            tmp_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
        until Done;
        case taicpu(p).opcode of
          a_end_try,a_catch,a_catch_all:
            result:=tai_wasmstruc_try_catch.internal_create(taicpu(p),tmp_asmlist,srclist);
          a_delegate:
            result:=tai_wasmstruc_try_delegate.internal_create(taicpu(p),tmp_asmlist,srclist);
          else
            internalerror(2023100502);
        end;
      end;

    constructor tai_wasmstruc_try.internal_create(a_try_asmlist: TAsmList);
      begin
        inherited Create;
        try_asmlist:=a_try_asmlist;
      end;

    destructor tai_wasmstruc_try.Destroy;
      begin
        try_asmlist.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_try.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_try;
      begin
        p:=tai_wasmstruc_try(inherited getcopy);
        if assigned(try_asmlist) then
          begin
            p.try_asmlist:=TAsmList.Create;
            p.try_asmlist.concatListcopy(try_asmlist);
          end;
        getcopy:=p;
      end;

    procedure tai_wasmstruc_try.ConvertToFlatList(l: TAsmList);
      begin
        l.Concat(taicpu.op_none(A_TRY));
        l.concatList(try_asmlist);
      end;

    { tai_wasmstruc_try_catch }

    constructor tai_wasmstruc_try_catch.internal_create(first_ins: taicpu; a_try_asmlist, srclist: TAsmList);
      var
        p: tai;

        procedure parse_next_catch_block;
          var
            new_catch_index: Integer;
            al: TAsmList;
            Done: Boolean;
            pp: tai;
          begin
            SetLength(catch_list,Length(catch_list)+1);
            new_catch_index:=High(catch_list);
            catch_list[new_catch_index].catch_instr:=taicpu(p);
            al:=TAsmList.Create;
            catch_list[new_catch_index].asmlist:=al;
            Done:=False;
            repeat
              pp:=tai(srclist.First);
              if (pp.typ=ait_instruction) and (taicpu(pp).opcode in [a_catch,a_catch_all,a_end_try]) then
                Done:=True
              else
                al.Concat(wasm_convert_first_item_to_structured(srclist));
            until Done;
          end;

        procedure parse_catch_all;
          var
            Done: Boolean;
            pp: tai;
          begin
            catch_all_asmlist:=TAsmList.Create;
            Done:=False;
            repeat
              pp:=tai(srclist.First);
              if (pp.typ=ait_instruction) and (taicpu(pp).opcode=a_end_try) then
                begin
                  srclist.Remove(pp);
                  Done:=True;
                end
              else
                catch_all_asmlist.Concat(wasm_convert_first_item_to_structured(srclist));
            until Done;
          end;

      var
        Done: Boolean;
      begin
        wstyp:=aitws_try_catch;
        inherited internal_create(a_try_asmlist);
        if assigned(first_ins.Previous) or assigned(first_ins.Next) then
          internalerror(2023100310);
        Done:=False;
        p:=first_ins;
        repeat
          if p.typ=ait_instruction then
            case taicpu(p).opcode of
              a_catch:
                begin
                  parse_next_catch_block;
                  p:=tai(srclist.First);
                  srclist.Remove(p);
                end;
              a_catch_all:
                begin
                  parse_catch_all;
                  Done:=True;
                end;
              a_end_try:
                Done:=True;
              else
                internalerror(2023100311);
            end
          else
            internalerror(2023100312);
        until Done;
      end;

    destructor tai_wasmstruc_try_catch.Destroy;
      var
        i: Integer;
      begin
        for i:=low(catch_list) to high(catch_list) do
          begin
            catch_list[i].asmlist.free;
            catch_list[i].catch_instr.free;
          end;
        catch_all_asmlist.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_try_catch.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_try_catch;
        i: Integer;
      begin
        p:=tai_wasmstruc_try_catch(inherited getcopy);
        p.catch_list:=Copy(catch_list);
        for i:=0 to length(catch_list)-1 do
          begin
            if assigned(catch_list[i].asmlist) then
              begin
                p.catch_list[i].asmlist:=TAsmList.Create;
                p.catch_list[i].asmlist.concatListcopy(catch_list[i].asmlist);
              end;
            if assigned(catch_list[i].catch_instr) then
              p.catch_list[i].catch_instr:=taicpu(catch_list[i].catch_instr.getcopy);
          end;
        if assigned(catch_all_asmlist) then
          begin
            p.catch_all_asmlist:=TAsmList.Create;
            p.catch_all_asmlist.concatListcopy(catch_all_asmlist);
          end;
        getcopy:=p;
      end;

    procedure tai_wasmstruc_try_catch.Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);
      var
        i: Integer;
      begin
        blockstack.push(self);
        map_structured_asmlist_inner(try_asmlist,f,blockstack);
        for i:=low(catch_list) to high(catch_list) do
          map_structured_asmlist_inner(catch_list[i].asmlist,f,blockstack);
        map_structured_asmlist_inner(catch_all_asmlist,f,blockstack);
        blockstack.pop;
      end;

    procedure tai_wasmstruc_try_catch.ConvertToFlatList(l: TAsmList);
      var
        i: Integer;
      begin
        inherited ConvertToFlatList(l);
        for i:=low(catch_list) to high(catch_list) do
          begin
            l.Concat(catch_list[i].catch_instr);
            catch_list[i].catch_instr:=nil;
            l.concatList(catch_list[i].asmlist);
          end;
        if assigned(catch_all_asmlist) then
          begin
            l.Concat(taicpu.op_none(a_catch_all));
            l.concatList(catch_all_asmlist);
          end;
        l.Concat(taicpu.op_none(a_end_try));
        if FLabelIsNew then
          l.concat(tai_label.create(FLabel));
      end;

    { tai_wasmstruc_try_delegate }

    constructor tai_wasmstruc_try_delegate.internal_create(first_ins: taicpu; a_try_asmlist, srclist: TAsmList);
      begin
        wstyp:=aitws_try_delegate;
        inherited internal_create(a_try_asmlist);
        if assigned(first_ins.Previous) or assigned(first_ins.Next) then
          internalerror(2023100309);
        delegate_instr:=first_ins;
      end;

    destructor tai_wasmstruc_try_delegate.Destroy;
      begin
        delegate_instr.free;
        inherited Destroy;
      end;

    function tai_wasmstruc_try_delegate.getcopy: TLinkedListItem;
      var
        p: tai_wasmstruc_try_delegate;
      begin
        p:=tai_wasmstruc_try_delegate(inherited getcopy);
        if assigned(delegate_instr) then
          p.delegate_instr:=taicpu(delegate_instr.getcopy);
        getcopy:=p;
      end;

    procedure tai_wasmstruc_try_delegate.Map(f: TAsmMapFunc; blockstack: twasmstruc_stack);
      begin
        blockstack.push(self);
        map_structured_asmlist_inner(try_asmlist,f,blockstack);
        blockstack.pop;
      end;

    procedure tai_wasmstruc_try_delegate.ConvertToFlatList(l: TAsmList);
      begin
        inherited ConvertToFlatList(l);
        l.Concat(delegate_instr);
        delegate_instr:=nil;
        if FLabelIsNew then
          l.concat(tai_label.create(FLabel));
      end;

    { tai_globaltype }

    constructor tai_globaltype.create(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean);
      begin
        inherited Create;
        sym:=TWasmGlobalAsmSymbol(current_asmdata.RefAsmSymbolByClass(TWasmGlobalAsmSymbol,aglobalname,AT_WASM_GLOBAL));
        sym.WasmGlobalType:=atype;
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=true;
        is_global:=false;
      end;

    constructor tai_globaltype.create_local(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      begin
        inherited Create;
        sym:=TWasmGlobalAsmSymbol(current_asmdata.DefineAsmSymbolByClass(TWasmGlobalAsmSymbol,aglobalname,AB_LOCAL,AT_WASM_GLOBAL,def));
        sym.WasmGlobalType:=atype;
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=false;
        is_global:=false;
      end;

    constructor tai_globaltype.create_global(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      begin
        inherited Create;
        sym:=TWasmGlobalAsmSymbol(current_asmdata.DefineAsmSymbolByClass(TWasmGlobalAsmSymbol,aglobalname,AB_GLOBAL,AT_WASM_GLOBAL,def));
        sym.WasmGlobalType:=atype;
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=false;
        is_global:=true;
      end;

    { tai_import_name }

    constructor tai_import_name.create(const asymname, aimportname: string);
      begin
        inherited Create;
        typ:=ait_import_name;
        symname:=asymname;
        importname:=aimportname;
      end;

    { tai_import_module }

    constructor tai_import_module.create(const asymname, aimportmodule: string);
      begin
        inherited Create;
        typ:=ait_import_module;
        symname:=asymname;
        importmodule:=aimportmodule;
      end;

    { tai_functype }

    constructor tai_functype.create(const afuncname: string; afunctype: TWasmFuncType);
      begin
        inherited Create;
        typ:=ait_functype;
        funcname:=afuncname;
        functype:=afunctype;
      end;


    destructor tai_functype.destroy;
      begin
        functype.free;
        inherited;
      end;

    { tai_tagtype }

    constructor tai_tagtype.create(const atagname: string; aparams: TWasmResultType);
      begin
        typ:=ait_tagtype;
        tagname:=atagname;
        params:=aparams;
      end;

    { tai_local }

    constructor tai_local.create(abasictype: TWasmBasicType; const aname: string);
      begin
        inherited Create;
        bastyp := abasictype;
        typ := ait_local;
        name := aname;
      end;

    { timpexp_ai }

      constructor tai_export_name.create(const aextname, aintname: ansistring;
          asymtype: timpexptype);
        begin
          inherited create;
          typ := ait_export_name;
          extname := aextname;
          intname := aintname;
          symstype:= asymtype;
        end;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
        inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
        inherited create(op);
        ops:=1;
{$ifdef EXTDEBUG}
        if getregtype(_op1)=R_INVALIDREGISTER then
          InternalError(2021011901);
{$endif EXTDEBUG}
        loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
        inherited create(op);
        ops:=1;
        loadref(0,_op1);
        if op in [a_local_get,a_local_set,a_local_tee] then
          begin
            if (_op1.base<>NR_LOCAL_STACK_POINTER_REG) or (_op1.index<>NR_NO) then
              internalerror(2021010201);
          end
        else
          begin
            if (_op1.base<>NR_NO) or (_op1.index<>NR_NO) then
              internalerror(2021010202);
          end;
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : aint);
      begin
        inherited create(op);
        ops:=1;
        loadconst(0,_op1);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
        inherited create(op);
        ops:=1;
        loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_const(op: tasmop; _op1: tasmsymbol; _op2: aint);
      begin
        inherited create(op);
        ops:=2;
        loadsymbol(0,_op1,0);
        loadconst(1,_op2);
      end;


    constructor taicpu.op_sym_functype(op : tasmop;_op1 : tasmsymbol;_op2 : TWasmFuncType);
      begin
        inherited create(op);
        ops:=2;
        loadsymbol(0,_op1,0);
        loadfunctype(1,_op2);
      end;


    constructor taicpu.op_single(op: tasmop; _op1: single);
      begin
        inherited create(op);
        ops:=1;
        loadsingle(0,_op1);
      end;


    constructor taicpu.op_double(op: tasmop; _op1: double);
      begin
        inherited create(op);
        ops:=1;
        loaddouble(0,_op1);
      end;

    constructor taicpu.op_functype(op: tasmop; _op1: TWasmFuncType);
      begin
       inherited create(op);
       ops:=1;
       loadfunctype(0,_op1);
      end;

    procedure taicpu.loadfunctype(opidx: longint; ft: TWasmFuncType);
      begin
       allocate_oper(opidx+1);
       with oper[opidx]^ do
        begin
          if typ<>top_functype then
            clearop(opidx);
          functype:=ft;
          typ:=top_functype;
        end;
      end;


    procedure taicpu.loadsingle(opidx:longint;f:single);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_single then
             clearop(opidx);
           sval:=f;
           typ:=top_single;
         end;
      end;


    procedure taicpu.loaddouble(opidx: longint; d: double);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_double then
             clearop(opidx);
           dval:=d;
           typ:=top_double;
         end;
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=false;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        if opcode in AsmOp_Store then
          result:=operand_write
        else
          result:=operand_read;
      end;


    function taicpu.Pass1(objdata: TObjData): longint;

        function SlebSize(v: tcgint): longint;
          begin
            result:=0;
            repeat
              v:=SarInt64(v,7);
              Inc(result);
            until ((v=0) and ((byte(v) and 64)=0)) or ((v=-1) and ((byte(v) and 64)<>0));
          end;

        function UlebSize(v: tcgint): longint;
          begin
            result:=0;
            repeat
              v:=v shr 7;
              Inc(result);
            until v=0;
          end;

      begin
        result:=0;
        case opcode of
          a_unreachable,
          a_nop,
          a_return,
          a_drop,
          a_i32_eqz,
          a_i32_eq,
          a_i32_ne,
          a_i32_lt_s,
          a_i32_lt_u,
          a_i32_gt_s,
          a_i32_gt_u,
          a_i32_le_s,
          a_i32_le_u,
          a_i32_ge_s,
          a_i32_ge_u,
          a_i64_eqz,
          a_i64_eq,
          a_i64_ne,
          a_i64_lt_s,
          a_i64_lt_u,
          a_i64_gt_s,
          a_i64_gt_u,
          a_i64_le_s,
          a_i64_le_u,
          a_i64_ge_s,
          a_i64_ge_u,
          a_f32_eq,
          a_f32_ne,
          a_f32_lt,
          a_f32_gt,
          a_f32_le,
          a_f32_ge,
          a_f64_eq,
          a_f64_ne,
          a_f64_lt,
          a_f64_gt,
          a_f64_le,
          a_f64_ge,
          a_i32_clz,
          a_i32_ctz,
          a_i32_popcnt,
          a_i32_add,
          a_i32_sub,
          a_i32_mul,
          a_i32_div_s,
          a_i32_div_u,
          a_i32_rem_s,
          a_i32_rem_u,
          a_i32_and,
          a_i32_or,
          a_i32_xor,
          a_i32_shl,
          a_i32_shr_s,
          a_i32_shr_u,
          a_i32_rotl,
          a_i32_rotr,
          a_i64_clz,
          a_i64_ctz,
          a_i64_popcnt,
          a_i64_add,
          a_i64_sub,
          a_i64_mul,
          a_i64_div_s,
          a_i64_div_u,
          a_i64_rem_s,
          a_i64_rem_u,
          a_i64_and,
          a_i64_or,
          a_i64_xor,
          a_i64_shl,
          a_i64_shr_s,
          a_i64_shr_u,
          a_i64_rotl,
          a_i64_rotr,
          a_f32_abs,
          a_f32_neg,
          a_f32_ceil,
          a_f32_floor,
          a_f32_trunc,
          a_f32_nearest,
          a_f32_sqrt,
          a_f32_add,
          a_f32_sub,
          a_f32_mul,
          a_f32_div,
          a_f32_min,
          a_f32_max,
          a_f32_copysign,
          a_f64_abs,
          a_f64_neg,
          a_f64_ceil,
          a_f64_floor,
          a_f64_trunc,
          a_f64_nearest,
          a_f64_sqrt,
          a_f64_add,
          a_f64_sub,
          a_f64_mul,
          a_f64_div,
          a_f64_min,
          a_f64_max,
          a_f64_copysign,
          a_i32_wrap_i64,
          a_i32_trunc_f32_s,
          a_i32_trunc_f32_u,
          a_i32_trunc_f64_s,
          a_i32_trunc_f64_u,
          a_i64_extend_i32_s,
          a_i64_extend_i32_u,
          a_i64_trunc_f32_s,
          a_i64_trunc_f32_u,
          a_i64_trunc_f64_s,
          a_i64_trunc_f64_u,
          a_f32_convert_i32_s,
          a_f32_convert_i32_u,
          a_f32_convert_i64_s,
          a_f32_convert_i64_u,
          a_f32_demote_f64,
          a_f64_convert_i32_s,
          a_f64_convert_i32_u,
          a_f64_convert_i64_s,
          a_f64_convert_i64_u,
          a_f64_promote_f32,
          a_i32_reinterpret_f32,
          a_i64_reinterpret_f64,
          a_f32_reinterpret_i32,
          a_f64_reinterpret_i64,
          a_i32_extend8_s,
          a_i32_extend16_s,
          a_i64_extend8_s,
          a_i64_extend16_s,
          a_i64_extend32_s,
          a_else,
          a_end_block,
          a_end_if,
          a_end_loop,
          a_end_try,
          a_catch_all,
          a_ref_is_null:
            result:=1;
          a_i32_trunc_sat_f32_s,
          a_i32_trunc_sat_f32_u,
          a_i32_trunc_sat_f64_s,
          a_i32_trunc_sat_f64_u,
          a_i64_trunc_sat_f32_s,
          a_i64_trunc_sat_f32_u,
          a_i64_trunc_sat_f64_s,
          a_i64_trunc_sat_f64_u,
          a_memory_size,
          a_memory_grow,
          a_ref_null_funcref,
          a_ref_null_externref:
            result:=2;
          a_memory_copy:
            result:=4;
          a_memory_fill,
          a_atomic_fence:
            result:=3;
          a_i32_const:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        result:=6
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          result:=1+SlebSize(longint(ref^.offset));
                        end;
                    end;
                  top_const:
                    result:=1+SlebSize(longint(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_i64_const:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        result:=6
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          result:=1+SlebSize(int64(ref^.offset));
                        end;
                    end;
                  top_const:
                    result:=1+SlebSize(int64(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_f32_const:
            result:=5;
          a_f64_const:
            result:=9;
          a_local_get,
          a_local_set,
          a_local_tee:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        internalerror(2021092005);
                      if ref^.base<>NR_STACK_POINTER_REG then
                        internalerror(2021092006);
                      if ref^.index<>NR_NO then
                        internalerror(2021092007);
                      result:=1+UlebSize(ref^.offset);
                    end;
                  top_const:
                    result:=1+UlebSize(val);
                  else
                    internalerror(2021092008);
                end;
            end;
          a_global_get,
          a_global_set:
            begin
              if ops<>1 then
                internalerror(2021092010);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) then
                        internalerror(2021092012);
                      if (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092013);
                      result:=6;
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            result:=0;
          a_block,
          a_loop,
          a_if,
          a_try:
            begin
              if ops=0 then
                result:=2
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  with oper[0]^ do
                    case typ of
                      top_functype:
                        begin
                          if (length(functype.params)=0) and (length(functype.results)<=1) then
                            result:=2
                          else
                            { more complex blocktypes are not yet implemented }
                            internalerror(2021092621);
                        end;
                      else
                        internalerror(2021092620);
                    end;
                end;
            end;
          a_i32_load,
          a_i64_load,
          a_f32_load,
          a_f64_load,
          a_i32_load8_s,
          a_i32_load8_u,
          a_i32_load16_s,
          a_i32_load16_u,
          a_i64_load8_s,
          a_i64_load8_u,
          a_i64_load16_s,
          a_i64_load16_u,
          a_i64_load32_s,
          a_i64_load32_u,
          a_i32_store,
          a_i64_store,
          a_f32_store,
          a_f64_store,
          a_i32_store8,
          a_i32_store16,
          a_i64_store8,
          a_i64_store16,
          a_i64_store32:
            begin
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          Result:=1+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            5;  { relocation, fixed size = 5 bytes }
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          Result:=1+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            UlebSize(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      Result:=1+
                        UlebSize(natural_alignment_for_load_store(opcode))+
                        UlebSize(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_memory_atomic_notify,
          a_memory_atomic_wait32,
          a_memory_atomic_wait64,
          a_i32_atomic_load,
          a_i64_atomic_load,
          a_i32_atomic_load8_u,
          a_i32_atomic_load16_u,
          a_i64_atomic_load8_u,
          a_i64_atomic_load16_u,
          a_i64_atomic_load32_u,
          a_i32_atomic_store,
          a_i64_atomic_store,
          a_i32_atomic_store8,
          a_i32_atomic_store16,
          a_i64_atomic_store8,
          a_i64_atomic_store16,
          a_i64_atomic_store32,
          a_i32_atomic_rmw_add,
          a_i64_atomic_rmw_add,
          a_i32_atomic_rmw8_add_u,
          a_i32_atomic_rmw16_add_u,
          a_i64_atomic_rmw8_add_u,
          a_i64_atomic_rmw16_add_u,
          a_i64_atomic_rmw32_add_u,
          a_i32_atomic_rmw_sub,
          a_i64_atomic_rmw_sub,
          a_i32_atomic_rmw8_sub_u,
          a_i32_atomic_rmw16_sub_u,
          a_i64_atomic_rmw8_sub_u,
          a_i64_atomic_rmw16_sub_u,
          a_i64_atomic_rmw32_sub_u,
          a_i32_atomic_rmw_and,
          a_i64_atomic_rmw_and,
          a_i32_atomic_rmw8_and_u,
          a_i32_atomic_rmw16_and_u,
          a_i64_atomic_rmw8_and_u,
          a_i64_atomic_rmw16_and_u,
          a_i64_atomic_rmw32_and_u,
          a_i32_atomic_rmw_or,
          a_i64_atomic_rmw_or,
          a_i32_atomic_rmw8_or_u,
          a_i32_atomic_rmw16_or_u,
          a_i64_atomic_rmw8_or_u,
          a_i64_atomic_rmw16_or_u,
          a_i64_atomic_rmw32_or_u,
          a_i32_atomic_rmw_xor,
          a_i64_atomic_rmw_xor,
          a_i32_atomic_rmw8_xor_u,
          a_i32_atomic_rmw16_xor_u,
          a_i64_atomic_rmw8_xor_u,
          a_i64_atomic_rmw16_xor_u,
          a_i64_atomic_rmw32_xor_u,
          a_i32_atomic_rmw_xchg,
          a_i64_atomic_rmw_xchg,
          a_i32_atomic_rmw8_xchg_u,
          a_i32_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw8_xchg_u,
          a_i64_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw32_xchg_u,
          a_i32_atomic_rmw_cmpxchg,
          a_i64_atomic_rmw_cmpxchg,
          a_i32_atomic_rmw8_cmpxchg_u,
          a_i32_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw8_cmpxchg_u,
          a_i64_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw32_cmpxchg_u:
            begin
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          Result:=2+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            5;  { relocation, fixed size = 5 bytes }
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          Result:=2+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            UlebSize(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      Result:=2+
                        UlebSize(natural_alignment_for_load_store(opcode))+
                        UlebSize(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_call:
            begin
              if ops<>2 then
                internalerror(2021092021);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092023);
                      result:=6;
                    end;
                  else
                    internalerror(2021092022);
                end;
            end;
          a_call_indirect:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_functype:
                    begin
                      TWasmObjData(objdata).FuncTypes.AddOrGetFuncType(functype);
                      result:=6+
                        UlebSize(0);
                    end;
                  else
                    internalerror(2021092611);
                end;
            end;
          a_br,
          a_br_if,
          a_rethrow,
          a_delegate:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=1+
                      UlebSize(val);
                  else
                    internalerror(2021092625);
                end;
            end;
          a_catch,
          a_throw:
            begin
              if ops<>1 then
                internalerror(2021092709);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092711);
                      result:=6;
                    end;
                  else
                    internalerror(2021092710);
                end;
            end;
          a_memory_init:
            begin
              if ops<>1 then
                internalerror(2022052802);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=3+UlebSize(val);
                  else
                    internalerror(2022052803);
                end;
            end;
          a_data_drop:
            begin
              if ops<>1 then
                internalerror(2022052804);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=2+UlebSize(val);
                  else
                    internalerror(2022052805);
                end;
            end;
          else
            internalerror(2021092623);
        end;
      end;


    procedure taicpu.Pass2(objdata: TObjData);

        procedure WriteByte(b: byte);
          begin
            objdata.writebytes(b,1);
          end;

{$ifdef FPC_LITTLE_ENDIAN}
        procedure WriteSingle(s: single);
          begin
            objdata.writebytes(s,4);
          end;

        procedure WriteDouble(d: double);
          begin
            objdata.writebytes(d,8);
          end;
{$else FPC_LITTLE_ENDIAN}
        procedure WriteSingle(s: single);
          var
            l: longword;
          begin
            Move(s,l,4);
            l:=SwapEndian(l);
            objdata.writebytes(l,4);
          end;

        procedure WriteDouble(d: double);
          var
            q: qword;
          begin
            Move(d,q,8);
            q:=SwapEndian(q);
            objdata.writebytes(q,8);
          end;
{$endif FPC_LITTLE_ENDIAN}

        procedure WriteSleb(v: tcgint);
          var
            b: byte;
            Done: Boolean=false;
          begin
            repeat
              b:=byte(v) and 127;
              v:=SarInt64(v,7);
              if ((v=0) and ((b and 64)=0)) or ((v=-1) and ((b and 64)<>0)) then
                Done:=true
              else
                b:=b or 128;
              objdata.writebytes(b,1);
            until Done;
          end;

        procedure WriteUleb(v: tcgint);
          var
            b: byte;
          begin
            repeat
              b:=byte(v) and 127;
              v:=v shr 7;
              if v<>0 then
                b:=b or 128;
              objdata.writebytes(b,1);
            until v=0;
          end;

      begin
        case opcode of
          a_unreachable:
            WriteByte($00);
          a_nop:
            WriteByte($01);
          a_return:
            WriteByte($0F);
          a_drop:
            WriteByte($1A);
          a_memory_size:
            begin
              WriteByte($3F);
              WriteByte($00);
            end;
          a_memory_grow:
            begin
              WriteByte($40);
              WriteByte($00);
            end;
          a_memory_copy:
            begin
              WriteByte($FC);
              WriteUleb(10);
              WriteByte($00);
              WriteByte($00);
            end;
          a_memory_fill:
            begin
              WriteByte($FC);
              WriteUleb(11);
              WriteByte($00);
            end;
          a_atomic_fence:
            begin
              WriteByte($FE);
              WriteByte($03);
              WriteByte($00);
            end;
          a_i32_eqz:
            WriteByte($45);
          a_i32_eq:
            WriteByte($46);
          a_i32_ne:
            WriteByte($47);
          a_i32_lt_s:
            WriteByte($48);
          a_i32_lt_u:
            WriteByte($49);
          a_i32_gt_s:
            WriteByte($4A);
          a_i32_gt_u:
            WriteByte($4B);
          a_i32_le_s:
            WriteByte($4C);
          a_i32_le_u:
            WriteByte($4D);
          a_i32_ge_s:
            WriteByte($4E);
          a_i32_ge_u:
            WriteByte($4F);
          a_i64_eqz:
            WriteByte($50);
          a_i64_eq:
            WriteByte($51);
          a_i64_ne:
            WriteByte($52);
          a_i64_lt_s:
            WriteByte($53);
          a_i64_lt_u:
            WriteByte($54);
          a_i64_gt_s:
            WriteByte($55);
          a_i64_gt_u:
            WriteByte($56);
          a_i64_le_s:
            WriteByte($57);
          a_i64_le_u:
            WriteByte($58);
          a_i64_ge_s:
            WriteByte($59);
          a_i64_ge_u:
            WriteByte($5A);
          a_f32_eq:
            WriteByte($5B);
          a_f32_ne:
            WriteByte($5C);
          a_f32_lt:
            WriteByte($5D);
          a_f32_gt:
            WriteByte($5E);
          a_f32_le:
            WriteByte($5F);
          a_f32_ge:
            WriteByte($60);
          a_f64_eq:
            WriteByte($61);
          a_f64_ne:
            WriteByte($62);
          a_f64_lt:
            WriteByte($63);
          a_f64_gt:
            WriteByte($64);
          a_f64_le:
            WriteByte($65);
          a_f64_ge:
            WriteByte($66);
          a_i32_clz:
            WriteByte($67);
          a_i32_ctz:
            WriteByte($68);
          a_i32_popcnt:
            WriteByte($69);
          a_i32_add:
            WriteByte($6A);
          a_i32_sub:
            WriteByte($6B);
          a_i32_mul:
            WriteByte($6C);
          a_i32_div_s:
            WriteByte($6D);
          a_i32_div_u:
            WriteByte($6E);
          a_i32_rem_s:
            WriteByte($6F);
          a_i32_rem_u:
            WriteByte($70);
          a_i32_and:
            WriteByte($71);
          a_i32_or:
            WriteByte($72);
          a_i32_xor:
            WriteByte($73);
          a_i32_shl:
            WriteByte($74);
          a_i32_shr_s:
            WriteByte($75);
          a_i32_shr_u:
            WriteByte($76);
          a_i32_rotl:
            WriteByte($77);
          a_i32_rotr:
            WriteByte($78);
          a_i64_clz:
            WriteByte($79);
          a_i64_ctz:
            WriteByte($7A);
          a_i64_popcnt:
            WriteByte($7B);
          a_i64_add:
            WriteByte($7C);
          a_i64_sub:
            WriteByte($7D);
          a_i64_mul:
            WriteByte($7E);
          a_i64_div_s:
            WriteByte($7F);
          a_i64_div_u:
            WriteByte($80);
          a_i64_rem_s:
            WriteByte($81);
          a_i64_rem_u:
            WriteByte($82);
          a_i64_and:
            WriteByte($83);
          a_i64_or:
            WriteByte($84);
          a_i64_xor:
            WriteByte($85);
          a_i64_shl:
            WriteByte($86);
          a_i64_shr_s:
            WriteByte($87);
          a_i64_shr_u:
            WriteByte($88);
          a_i64_rotl:
            WriteByte($89);
          a_i64_rotr:
            WriteByte($8A);
          a_f32_abs:
            WriteByte($8B);
          a_f32_neg:
            WriteByte($8C);
          a_f32_ceil:
            WriteByte($8D);
          a_f32_floor:
            WriteByte($8E);
          a_f32_trunc:
            WriteByte($8F);
          a_f32_nearest:
            WriteByte($90);
          a_f32_sqrt:
            WriteByte($91);
          a_f32_add:
            WriteByte($92);
          a_f32_sub:
            WriteByte($93);
          a_f32_mul:
            WriteByte($94);
          a_f32_div:
            WriteByte($95);
          a_f32_min:
            WriteByte($96);
          a_f32_max:
            WriteByte($97);
          a_f32_copysign:
            WriteByte($98);
          a_f64_abs:
            WriteByte($99);
          a_f64_neg:
            WriteByte($9A);
          a_f64_ceil:
            WriteByte($9B);
          a_f64_floor:
            WriteByte($9C);
          a_f64_trunc:
            WriteByte($9D);
          a_f64_nearest:
            WriteByte($9E);
          a_f64_sqrt:
            WriteByte($9F);
          a_f64_add:
            WriteByte($A0);
          a_f64_sub:
            WriteByte($A1);
          a_f64_mul:
            WriteByte($A2);
          a_f64_div:
            WriteByte($A3);
          a_f64_min:
            WriteByte($A4);
          a_f64_max:
            WriteByte($A5);
          a_f64_copysign:
            WriteByte($A6);
          a_i32_wrap_i64:
            WriteByte($A7);
          a_i32_trunc_f32_s:
            WriteByte($A8);
          a_i32_trunc_f32_u:
            WriteByte($A9);
          a_i32_trunc_f64_s:
            WriteByte($AA);
          a_i32_trunc_f64_u:
            WriteByte($AB);
          a_i64_extend_i32_s:
            WriteByte($AC);
          a_i64_extend_i32_u:
            WriteByte($AD);
          a_i64_trunc_f32_s:
            WriteByte($AE);
          a_i64_trunc_f32_u:
            WriteByte($AF);
          a_i64_trunc_f64_s:
            WriteByte($B0);
          a_i64_trunc_f64_u:
            WriteByte($B1);
          a_f32_convert_i32_s:
            WriteByte($B2);
          a_f32_convert_i32_u:
            WriteByte($B3);
          a_f32_convert_i64_s:
            WriteByte($B4);
          a_f32_convert_i64_u:
            WriteByte($B5);
          a_f32_demote_f64:
            WriteByte($B6);
          a_f64_convert_i32_s:
            WriteByte($B7);
          a_f64_convert_i32_u:
            WriteByte($B8);
          a_f64_convert_i64_s:
            WriteByte($B9);
          a_f64_convert_i64_u:
            WriteByte($BA);
          a_f64_promote_f32:
            WriteByte($BB);
          a_i32_reinterpret_f32:
            WriteByte($BC);
          a_i64_reinterpret_f64:
            WriteByte($BD);
          a_f32_reinterpret_i32:
            WriteByte($BE);
          a_f64_reinterpret_i64:
            WriteByte($BF);
          a_i32_extend8_s:
            WriteByte($C0);
          a_i32_extend16_s:
            WriteByte($C1);
          a_i64_extend8_s:
            WriteByte($C2);
          a_i64_extend16_s:
            WriteByte($C3);
          a_i64_extend32_s:
            WriteByte($C4);
          a_end_block,
          a_end_if,
          a_end_loop,
          a_end_try:
            WriteByte($0B);
          a_catch_all:
            WriteByte($19);
          a_i32_const:
            begin
              WriteByte($41);
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB)
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteSleb(longint(ref^.offset));
                        end;
                    end;
                  top_const:
                    WriteSleb(longint(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_i64_const:
            begin
              WriteByte($42);
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB)
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteSleb(int64(ref^.offset));
                        end;
                    end;
                  top_const:
                    WriteSleb(int64(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_f32_const:
            begin
              if ops<>1 then
                internalerror(2021092619);
              WriteByte($43);
              with oper[0]^ do
                case typ of
                  top_single:
                    WriteSingle(sval);
                  else
                    internalerror(2021092618);
                end;
            end;
          a_f64_const:
            begin
              if ops<>1 then
                internalerror(2021092616);
              WriteByte($44);
              with oper[0]^ do
                case typ of
                  top_double:
                    WriteDouble(dval);
                  else
                    internalerror(2021092617);
                end;
            end;
          a_local_get,
          a_local_set,
          a_local_tee:
            begin
              case opcode of
                a_local_get:
                  WriteByte($20);
                a_local_set:
                  WriteByte($21);
                a_local_tee:
                  WriteByte($22);
                else
                  internalerror(2021092003);
              end;
              if ops<>1 then
                internalerror(2021092004);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        internalerror(2021092005);
                      if ref^.base<>NR_STACK_POINTER_REG then
                        internalerror(2021092006);
                      if ref^.index<>NR_NO then
                        internalerror(2021092007);
                      WriteUleb(ref^.offset);
                    end;
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2021092008);
                end;
            end;
          a_global_get,
          a_global_set:
            begin
              case opcode of
                a_global_get:
                  WriteByte($23);
                a_global_set:
                  WriteByte($24);
                else
                  internalerror(2021092009);
              end;
              if ops<>1 then
                internalerror(2021092010);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) then
                        internalerror(2021092012);
                      if (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092013);
                      objdata.writeReloc(0,5,TWasmObjData(ObjData).globalref(ref^.symbol),RELOC_GLOBAL_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            ;
          a_block,
          a_loop,
          a_if,
          a_try:
            begin
              case opcode of
                a_block:
                  WriteByte($02);
                a_loop:
                  WriteByte($03);
                a_if:
                  WriteByte($04);
                a_try:
                  WriteByte($06);
                else
                  internalerror(2021092626);
              end;
              if ops=0 then
                WriteByte($40)
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  with oper[0]^ do
                    case typ of
                      top_functype:
                        begin
                          if (length(functype.params)=0) and (length(functype.results)<=1) then
                            begin
                              if length(functype.results)=1 then
                                WriteByte(encode_wasm_basic_type(functype.results[0]))
                              else
                                WriteByte($40);
                            end
                          else
                            { more complex blocktypes are not yet implemented }
                            internalerror(2021092621);
                        end;
                      else
                        internalerror(2021092620);
                    end;
                end;
            end;
          a_else:
            WriteByte($05);
          a_i32_load,
          a_i64_load,
          a_f32_load,
          a_f64_load,
          a_i32_load8_s,
          a_i32_load8_u,
          a_i32_load16_s,
          a_i32_load16_u,
          a_i64_load8_s,
          a_i64_load8_u,
          a_i64_load16_s,
          a_i64_load16_u,
          a_i64_load32_s,
          a_i64_load32_u,
          a_i32_store,
          a_i64_store,
          a_f32_store,
          a_f64_store,
          a_i32_store8,
          a_i32_store16,
          a_i64_store8,
          a_i64_store16,
          a_i64_store32:
            begin
              case opcode of
                a_i32_load:
                  WriteByte($28);
                a_i64_load:
                  WriteByte($29);
                a_f32_load:
                  WriteByte($2A);
                a_f64_load:
                  WriteByte($2B);
                a_i32_load8_s:
                  WriteByte($2C);
                a_i32_load8_u:
                  WriteByte($2D);
                a_i32_load16_s:
                  WriteByte($2E);
                a_i32_load16_u:
                  WriteByte($2F);
                a_i64_load8_s:
                  WriteByte($30);
                a_i64_load8_u:
                  WriteByte($31);
                a_i64_load16_s:
                  WriteByte($32);
                a_i64_load16_u:
                  WriteByte($33);
                a_i64_load32_s:
                  WriteByte($34);
                a_i64_load32_u:
                  WriteByte($35);
                a_i32_store:
                  WriteByte($36);
                a_i64_store:
                  WriteByte($37);
                a_f32_store:
                  WriteByte($38);
                a_f64_store:
                  WriteByte($39);
                a_i32_store8:
                  WriteByte($3A);
                a_i32_store16:
                  WriteByte($3B);
                a_i64_store8:
                  WriteByte($3C);
                a_i64_store16:
                  WriteByte($3D);
                a_i64_store32:
                  WriteByte($3E);
                else
                  internalerror(2021092019);
              end;
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_LEB);
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          WriteUleb(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      WriteUleb(natural_alignment_for_load_store(opcode));
                      WriteUleb(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_memory_atomic_notify,
          a_memory_atomic_wait32,
          a_memory_atomic_wait64,
          a_i32_atomic_load,
          a_i64_atomic_load,
          a_i32_atomic_load8_u,
          a_i32_atomic_load16_u,
          a_i64_atomic_load8_u,
          a_i64_atomic_load16_u,
          a_i64_atomic_load32_u,
          a_i32_atomic_store,
          a_i64_atomic_store,
          a_i32_atomic_store8,
          a_i32_atomic_store16,
          a_i64_atomic_store8,
          a_i64_atomic_store16,
          a_i64_atomic_store32,
          a_i32_atomic_rmw_add,
          a_i64_atomic_rmw_add,
          a_i32_atomic_rmw8_add_u,
          a_i32_atomic_rmw16_add_u,
          a_i64_atomic_rmw8_add_u,
          a_i64_atomic_rmw16_add_u,
          a_i64_atomic_rmw32_add_u,
          a_i32_atomic_rmw_sub,
          a_i64_atomic_rmw_sub,
          a_i32_atomic_rmw8_sub_u,
          a_i32_atomic_rmw16_sub_u,
          a_i64_atomic_rmw8_sub_u,
          a_i64_atomic_rmw16_sub_u,
          a_i64_atomic_rmw32_sub_u,
          a_i32_atomic_rmw_and,
          a_i64_atomic_rmw_and,
          a_i32_atomic_rmw8_and_u,
          a_i32_atomic_rmw16_and_u,
          a_i64_atomic_rmw8_and_u,
          a_i64_atomic_rmw16_and_u,
          a_i64_atomic_rmw32_and_u,
          a_i32_atomic_rmw_or,
          a_i64_atomic_rmw_or,
          a_i32_atomic_rmw8_or_u,
          a_i32_atomic_rmw16_or_u,
          a_i64_atomic_rmw8_or_u,
          a_i64_atomic_rmw16_or_u,
          a_i64_atomic_rmw32_or_u,
          a_i32_atomic_rmw_xor,
          a_i64_atomic_rmw_xor,
          a_i32_atomic_rmw8_xor_u,
          a_i32_atomic_rmw16_xor_u,
          a_i64_atomic_rmw8_xor_u,
          a_i64_atomic_rmw16_xor_u,
          a_i64_atomic_rmw32_xor_u,
          a_i32_atomic_rmw_xchg,
          a_i64_atomic_rmw_xchg,
          a_i32_atomic_rmw8_xchg_u,
          a_i32_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw8_xchg_u,
          a_i64_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw32_xchg_u,
          a_i32_atomic_rmw_cmpxchg,
          a_i64_atomic_rmw_cmpxchg,
          a_i32_atomic_rmw8_cmpxchg_u,
          a_i32_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw8_cmpxchg_u,
          a_i64_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw32_cmpxchg_u:
            begin
              WriteByte($FE);
              case opcode of
                a_memory_atomic_notify:
                  WriteByte($00);
                a_memory_atomic_wait32:
                  WriteByte($01);
                a_memory_atomic_wait64:
                  WriteByte($02);
                a_i32_atomic_load:
                  WriteByte($10);
                a_i64_atomic_load:
                  WriteByte($11);
                a_i32_atomic_load8_u:
                  WriteByte($12);
                a_i32_atomic_load16_u:
                  WriteByte($13);
                a_i64_atomic_load8_u:
                  WriteByte($14);
                a_i64_atomic_load16_u:
                  WriteByte($15);
                a_i64_atomic_load32_u:
                  WriteByte($16);
                a_i32_atomic_store:
                  WriteByte($17);
                a_i64_atomic_store:
                  WriteByte($18);
                a_i32_atomic_store8:
                  WriteByte($19);
                a_i32_atomic_store16:
                  WriteByte($1A);
                a_i64_atomic_store8:
                  WriteByte($1B);
                a_i64_atomic_store16:
                  WriteByte($1C);
                a_i64_atomic_store32:
                  WriteByte($1D);
                a_i32_atomic_rmw_add:
                  WriteByte($1E);
                a_i64_atomic_rmw_add:
                  WriteByte($1F);
                a_i32_atomic_rmw8_add_u:
                  WriteByte($20);
                a_i32_atomic_rmw16_add_u:
                  WriteByte($21);
                a_i64_atomic_rmw8_add_u:
                  WriteByte($22);
                a_i64_atomic_rmw16_add_u:
                  WriteByte($23);
                a_i64_atomic_rmw32_add_u:
                  WriteByte($24);
                a_i32_atomic_rmw_sub:
                  WriteByte($25);
                a_i64_atomic_rmw_sub:
                  WriteByte($26);
                a_i32_atomic_rmw8_sub_u:
                  WriteByte($27);
                a_i32_atomic_rmw16_sub_u:
                  WriteByte($28);
                a_i64_atomic_rmw8_sub_u:
                  WriteByte($29);
                a_i64_atomic_rmw16_sub_u:
                  WriteByte($2A);
                a_i64_atomic_rmw32_sub_u:
                  WriteByte($2B);
                a_i32_atomic_rmw_and:
                  WriteByte($2C);
                a_i64_atomic_rmw_and:
                  WriteByte($2D);
                a_i32_atomic_rmw8_and_u:
                  WriteByte($2E);
                a_i32_atomic_rmw16_and_u:
                  WriteByte($2F);
                a_i64_atomic_rmw8_and_u:
                  WriteByte($30);
                a_i64_atomic_rmw16_and_u:
                  WriteByte($31);
                a_i64_atomic_rmw32_and_u:
                  WriteByte($32);
                a_i32_atomic_rmw_or:
                  WriteByte($33);
                a_i64_atomic_rmw_or:
                  WriteByte($34);
                a_i32_atomic_rmw8_or_u:
                  WriteByte($35);
                a_i32_atomic_rmw16_or_u:
                  WriteByte($36);
                a_i64_atomic_rmw8_or_u:
                  WriteByte($37);
                a_i64_atomic_rmw16_or_u:
                  WriteByte($38);
                a_i64_atomic_rmw32_or_u:
                  WriteByte($39);
                a_i32_atomic_rmw_xor:
                  WriteByte($3A);
                a_i64_atomic_rmw_xor:
                  WriteByte($3B);
                a_i32_atomic_rmw8_xor_u:
                  WriteByte($3C);
                a_i32_atomic_rmw16_xor_u:
                  WriteByte($3D);
                a_i64_atomic_rmw8_xor_u:
                  WriteByte($3E);
                a_i64_atomic_rmw16_xor_u:
                  WriteByte($3F);
                a_i64_atomic_rmw32_xor_u:
                  WriteByte($40);
                a_i32_atomic_rmw_xchg:
                  WriteByte($41);
                a_i64_atomic_rmw_xchg:
                  WriteByte($42);
                a_i32_atomic_rmw8_xchg_u:
                  WriteByte($43);
                a_i32_atomic_rmw16_xchg_u:
                  WriteByte($44);
                a_i64_atomic_rmw8_xchg_u:
                  WriteByte($45);
                a_i64_atomic_rmw16_xchg_u:
                  WriteByte($46);
                a_i64_atomic_rmw32_xchg_u:
                  WriteByte($47);
                a_i32_atomic_rmw_cmpxchg:
                  WriteByte($48);
                a_i64_atomic_rmw_cmpxchg:
                  WriteByte($49);
                a_i32_atomic_rmw8_cmpxchg_u:
                  WriteByte($4A);
                a_i32_atomic_rmw16_cmpxchg_u:
                  WriteByte($4B);
                a_i64_atomic_rmw8_cmpxchg_u:
                  WriteByte($4C);
                a_i64_atomic_rmw16_cmpxchg_u:
                  WriteByte($4D);
                a_i64_atomic_rmw32_cmpxchg_u:
                  WriteByte($4E);
                else
                  internalerror(2022052101);
              end;
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_LEB);
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          WriteUleb(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      WriteUleb(natural_alignment_for_load_store(opcode));
                      WriteUleb(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_call:
            begin
              if ops<>2 then
                internalerror(2021092021);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092023);
                      WriteByte($10);
                      objdata.writeReloc(0,5,ObjData.symbolref(ref^.symbol),RELOC_FUNCTION_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092022);
                end;
            end;
          a_call_indirect:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_functype:
                    begin
                      WriteByte($11);
                      objdata.writeReloc(TWasmObjData(objdata).FuncTypes.AddOrGetFuncType(functype),5,nil,RELOC_TYPE_INDEX_LEB);
                      WriteUleb(0);
                    end;
                  else
                    internalerror(2021092611);
                end;
            end;
          a_br,
          a_br_if,
          a_rethrow,
          a_delegate:
            begin
              case opcode of
                a_br:
                  WriteByte($0C);
                a_br_if:
                  WriteByte($0D);
                a_rethrow:
                  WriteByte($09);
                a_delegate:
                  WriteByte($18);
                else
                  internalerror(2021092622);
              end;
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2021092625);
                end;
            end;
          a_catch,
          a_throw:
            begin
              case opcode of
                a_catch:
                  WriteByte($07);
                a_throw:
                  WriteByte($08);
                else
                  internalerror(2021092708);
              end;
              if ops<>1 then
                internalerror(2021092709);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092711);
                      objdata.writeReloc(0,5,TWasmObjData(ObjData).ExceptionTagRef(ref^.symbol),RELOC_TAG_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092710);
                end;
            end;
          a_memory_init:
            begin
              WriteByte($FC);
              WriteByte($08);
              if ops<>1 then
                internalerror(2022052806);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2022052807);
                end;
              WriteByte($00);
            end;
          a_data_drop:
            begin
              WriteByte($FC);
              WriteByte($09);
              if ops<>1 then
                internalerror(2022052808);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2022052809);
                end;
            end;
          a_i32_trunc_sat_f32_s:
            begin
              WriteByte($FC);
              WriteByte($00);
            end;
          a_i32_trunc_sat_f32_u:
            begin
              WriteByte($FC);
              WriteByte($01);
            end;
          a_i32_trunc_sat_f64_s:
            begin
              WriteByte($FC);
              WriteByte($02);
            end;
          a_i32_trunc_sat_f64_u:
            begin
              WriteByte($FC);
              WriteByte($03);
            end;
          a_i64_trunc_sat_f32_s:
            begin
              WriteByte($FC);
              WriteByte($04);
            end;
          a_i64_trunc_sat_f32_u:
            begin
              WriteByte($FC);
              WriteByte($05);
            end;
          a_i64_trunc_sat_f64_s:
            begin
              WriteByte($FC);
              WriteByte($06);
            end;
          a_i64_trunc_sat_f64_u:
            begin
              WriteByte($FC);
              WriteByte($07);
            end;
          a_ref_null_funcref:
            begin
              WriteByte($D0);
              WriteByte($70);
            end;
          a_ref_null_externref:
            begin
              WriteByte($D0);
              WriteByte($6F);
            end;
          a_ref_is_null:
            WriteByte($D1);
          else
            internalerror(2021092624);
        end;
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
       internalerror(2010122614);
       result:=nil;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
       internalerror(2010122615);
       result:=nil;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


    function wasm_convert_first_item_to_structured(srclist: TAsmList): tai;
      begin
        result:=tai(srclist.First);
        if result<>nil then
          begin
            srclist.Remove(result);
            if result.typ=ait_instruction then
              case taicpu(result).opcode of
                a_if:
                  result:=tai_wasmstruc_if.create_from(taicpu(result),srclist);
                a_block:
                  result:=tai_wasmstruc_block.create_from(taicpu(result),srclist);
                a_loop:
                  result:=tai_wasmstruc_loop.create_from(taicpu(result),srclist);
                a_try:
                  result:=tai_wasmstruc_try.create_from(srclist);
                a_else,a_end_if,a_end_block,a_end_loop,a_end_try,a_catch,a_catch_all,a_delegate:
                  internalerror(2023100503);
                else
                  ;
              end;
          end;
      end;


    procedure wasm_convert_to_structured_asmlist_internal(srclist, destlist: TAsmList);
      begin
        while not srclist.Empty do
          destlist.Concat(wasm_convert_first_item_to_structured(srclist));
      end;


    procedure wasm_convert_to_structured_asmlist(var asmlist: TAsmList);
      var
        tmplist: TAsmList;
      begin
        tmplist:=TAsmList.Create;
        wasm_convert_to_structured_asmlist_internal(asmlist,tmplist);
        asmlist.Free;
        asmlist:=tmplist;
      end;

    procedure wasm_convert_to_flat_asmlist_internal(srclist, destlist: TAsmList);
      var
        p: tai;
        tmplist: TAsmList;
      begin
        tmplist:=TAsmList.Create;
        while not srclist.Empty do
          begin
            p:=tai(srclist.First);
            srclist.Remove(p);
            if p.typ=ait_wasm_structured_instruction then
              begin
                taicpu_wasm_structured_instruction(p).ConvertToFlatList(tmplist);
                srclist.insertList(tmplist);
              end
            else
              destlist.Concat(p);
          end;
        tmplist.free;
      end;


    procedure wasm_convert_to_flat_asmlist(var asmlist: TAsmList);
      var
        tmplist: TAsmList;
      begin
        tmplist:=TAsmList.Create;
        wasm_convert_to_flat_asmlist_internal(asmlist,tmplist);
        asmlist.Free;
        asmlist:=tmplist;
      end;


    procedure map_structured_asmlist_inner(l: TAsmList; f: TAsmMapFunc; blockstack: twasmstruc_stack);
      var
        p, q: tai;
        mapres: TAsmMapFuncResult;
      begin
        if not assigned(l) then
          exit;
        p:=tai(l.First);
        while p<>nil do
          begin
            if p.typ=ait_wasm_structured_instruction then
              begin
                mapres:=f(p,blockstack);
                case mapres.typ of
                  amfrtNoChange:
                    begin
                      taicpu_wasm_structured_instruction(p).Map(f,blockstack);
                      p:=tai(p.next);
                    end;
                  amfrtNewAi:
                    begin
                      q:=mapres.newai;
                      if q<>p then
                        begin
                          l.InsertAfter(q,p);
                          l.Remove(p);
                          p:=q;
                        end;
                      p:=tai(p.next);
                    end;
                  amfrtNewList:
                    begin
                      q:=tai(mapres.newlist.First);
                      l.insertListAfter(p,mapres.newlist);
                      mapres.newlist.free;
                      l.Remove(p);
                      p:=q;
                    end;
                  amfrtDeleteAi:
                    begin
                      q:=p;
                      p:=tai(p.next);
                      l.Remove(q);
                    end;
                end;
              end
            else
              begin
                mapres:=f(p,blockstack);
                case mapres.typ of
                  amfrtNoChange:
                    p:=tai(p.next);
                  amfrtNewAi:
                    begin
                      q:=mapres.newai;
                      if q<>p then
                        begin
                          l.InsertAfter(q,p);
                          l.Remove(p);
                          p:=tai(q.next);
                        end
                      else
                        p:=tai(p.next);
                    end;
                  amfrtNewList:
                    begin
                      q:=tai(mapres.newlist.First);
                      l.insertListAfter(p,mapres.newlist);
                      mapres.newlist.free;
                      l.Remove(p);
                      p:=q;
                    end;
                  amfrtDeleteAi:
                    begin
                      q:=p;
                      p:=tai(p.next);
                      l.Remove(q);
                    end;
                end;
              end;
          end;
      end;


    procedure map_structured_asmlist(l: TAsmList; f: TAsmMapFunc);
      var
        blockstack: twasmstruc_stack;
      begin
        blockstack:=twasmstruc_stack.create;
        map_structured_asmlist_inner(l,f,blockstack);
        blockstack.free;
      end;


initialization
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
