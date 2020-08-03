unit wasmnormalize;

interface

uses
  SysUtils, Classes,
  wasmmodule, wasmbin, wasmbincode, wasmlink;

procedure Normalize(m: TWasmModule);

implementation

procedure PopulateRelocData(module: TWasmModule; ci: TWasmInstr);
var
  idx : integer;
begin
  case INST_FLAGS[ci.code].Param of
    ipi32OrFunc:
      if (ci.operandText<>'') and (ci.operandText[1]='$') then begin
        //if not ci.hasRelocIdx then
        idx := RegisterfuncInElem(module, ci.operandText);
        //AddReloc(rt, dst.Position+ofsAddition, idx);
        ci.operandNum := idx;
        ci.SetReloc(INST_RELOC_FLAGS[ci.code].relocType, idx);
      end;

    ipLeb:
       if (INST_RELOC_FLAGS[ci.code].doReloc) then begin
         ci.SetReloc(INST_RELOC_FLAGS[ci.code].relocType, ci.operandNum);
       end;

    ipCallType:
      if Assigned(ci.insttype) then
        ci.SetReloc(INST_RELOC_FLAGS[ci.code].relocType, ci.insttype.typeNum);
  end;
end;


// searching back in the labels stack.
// returning the "number" of steps to jump back to the label
function GetJumpLabelIndex(const JumpToLbl: string; LblStack: TStrings): Integer;
var
  i : integer;
begin
  i:=LblStack.Count-1;
  while (i>=0) and (LblStack[i]<>JumpToLbl) do
    dec(i);
  Result := LblStack.Count-i-1;
end;

procedure NormalizeOfsAlign(ci: TWasmInstr);
const
  ALIGN_STR : array [0..3] of string = ('1','2','4','8');
begin
  if (ci.offsetText = '') then ci.offsetText := '0';
  if (ci.alignText = '') then
    ci.alignText := ALIGN_STR[INST_FLAGS[ci.code].align];
end;

// Normalizing instruction list, popuplating index reference ($index)
// with the actual numbers. (params, locals, globals, memory, functions index)
//
// pass "f" as nil, if instruction list doesn't belong to a function
function NormalizeInst(m: TWasmModule; f: TWasmFunc; l: TWasmInstrList; checkEnd: boolean = true): Boolean;
var
  i   : integer;
  j   : integer;
  ci  : TWasmInstr;
  endNeed : Integer;
  lbl     : TStringList;
const
  ValidResTypes = [VALTYPE_NONE,VALTYPE_I32,VALTYPE_I64,VALTYPE_F32,VALTYPE_F64];
begin
  Result := true;
  endNeed := 1;
  lbl := TStringList.Create;
  try
    for i:=0 to l.Count-1 do begin
      ci:=l[i];

      case INST_FLAGS[ci.code].Param of
        ipResType:
        begin
          inc(endNeed);
          if not (byte(ci.operandNum) in ValidResTypes) then
            ci.operandNum := VALTYPE_NONE;

          lbl.Add(ci.jumplabel);
        end;
        ipOfsAlign:
          NormalizeOfsAlign( ci );
      end;

      case ci.code of
        INST_local_get, INST_local_set, INST_local_tee:
        begin
          if not Assigned(f) then begin
            Result:=false;
            Exit;
          end;

          if (ci.operandIdx<>'') and (ci.operandNum<0) then begin
            j:=FindParam(f.functype.params, ci.operandIdx);
            if j<0 then begin
              j:=FindParam(f.locals, ci.operandIdx);
              if j>=0 then inc(j, f.functype.ParamCount);
            end;
            ci.operandNum:=j;
          end;
        end;

        INST_global_get, INST_global_set:
        begin
          if not Assigned(f) then begin
            Result:=false;
            Exit;
          end;

          if (ci.operandIdx<>'') and (ci.operandNum<0) then begin
            j:=FindGlobal(m, ci.operandIdx);
            ci.operandNum:=j;
          end;
        end;

        INST_call:
        begin
          if (ci.operandIdx<>'') and (ci.operandNum<0) then
            ci.operandNum:=FindFunc(m,ci.operandIdx);
        end;

        INST_call_indirect:
        begin
          if Assigned(ci.insttype) and (ci.insttype.typeNum<0) then begin
            if ci.insttype.typeIdx <> '' then
              ci.insttype.typeNum:=FindFuncType(m, ci.insttype.typeIdx)
            else
              ci.insttype.typeNum:=RegisterFuncType(m, ci.insttype);
          end;
        end;

        INST_br, INST_br_if, INST_br_table: begin

          if ci.code = INST_br_table then
            for j:=0 to ci.vecTableCount-1 do
              if ci.vecTable[j].id <> '' then
                ci.vecTable[j].idNum:=GetJumpLabelIndex(ci.vecTable[j].id, lbl);

          if ci.operandIdx<>'' then
            ci.operandNum:=GetJumpLabelIndex(ci.operandIdx, lbl);
        end;

        INST_END: begin
          dec(endNeed);
          if lbl.Count>0 then lbl.Delete(lbl.Count-1);
        end;
      end;

      PopulateRelocData(m, ci);
    end;

    // adding end instruction
    if checkEnd and (endNeed>0) then
      l.AddInstr(INST_END);
  finally
    lbl.Free;
  end;
end;


procedure NormalizeFuncType(m: TWasmModule; fn : TWasmFuncType);
begin
  if fn.isNumOrIdx then begin
    if fn.typeIdx<>'' then
      fn.typeNum:=FindFuncType(m, fn.typeIdx);
  end else
    fn.typeNum:=RegisterFuncType(m, fn);
end;

procedure NormalizeImport(m: TWasmModule; var fnIdx: Integer);
var
  i  : integer;
  im : TWasmImport;
begin
  fnIdx := 0;
  for i:=0 to m.ImportCount-1 do begin
    im := m.GetImport(i);
    if Assigned(im.fn) then begin
      im.fn.idNum:=fnIdx;
      NormalizeFuncType(m, im.fn.functype);
      inc(fnIdx);
    end;
  end;
end;

procedure NormalizeGlobals(m: TWasmModule);
var
  i : integer;
begin
  for i:=0 to m.GlobalCount-1 do
    m.GetGlobal(i).id.idNum:=i;
end;

procedure NormalizeTable(m: TWasmModule);
var
  i  : integer;
  j  : integer;
  t  : TWasmTable;
  se : TWasmElement;
  de : TWasmElement;
begin
  for i:=0 to m.TableCount-1 do begin
    t := m.GetTable(i);
    t.id.idNum:=i; // todo: is it safe?
  end;

  for i:=0 to m.TableCount-1 do begin
    t := m.GetTable(i);
    if not Assigned(t.elem) then continue;
    se:=t.elem;
    de := m.AddElement;
    de.tableIdx := t.id.idNum;

    de.funcCount:=se.funcCount;
    if se.funcCount>0 then begin
      SetLength(de.funcs, de.funcCount);
      for j:=0 to de.funcCount-1 do begin
        de.funcs[j].id := se.funcs[j].id;
        de.funcs[j].idNum := se.funcs[j].idNum;
      end;
    end;
  end;
end;

procedure NormalizeTableLimit(m: TWasmModule);
var
  i       : integer;
  elCount : integer;
  t       : TWasmTable;
begin
  elCount:=0;
  for i:=0 to m.ElementCount-1 do
    inc(elCount, m.GetElement(i).funcCount );

  for i:=0 to m.TableCount-1 do begin
    t := m.GetTable(i);
    if (t.min=0) and (t.max=0) then begin
      t.min := elCount;
      t.max := elCount;
      Break;
    end;
  end;
end;

procedure NormalizeElems(m: TWasmModule);
var
  i : integer;
  e : TWasmElement;
  l : TWasmInstrList;
begin
  //todo: resolve offsets
  for i:=0 to m.ElementCount-1 do begin
    e := m.GetElement(i);
    l := e.AddOffset;
    if (l.Count=0) then l.AddInstr(INST_i32_const).operandText:='0';
    NormalizeInst( m, nil, l);
  end;
end;

// normalizing reference
procedure Normalize(m: TWasmModule);
var
  i     : integer;
  f     : TWasmFunc;
  x     : TWasmExport;
  fnIdx : Integer;
begin
  fnIdx := 0;
  NormalizeGlobals(m);
  NormalizeTable(m);
  NormalizeElems(m);
  NormalizeImport(m, fnIdx);
  NormalizeTableLimit(m);

  for i:=0 to m.FuncCount-1 do begin
    f:=m.GetFunc(i);
    f.idNum := fnIdx;

    NormalizeFuncType(m, f.functype);

    inc(fnIdx);
  end;

  for i:=0 to m.GlobalCount-1 do
    NormalizeInst(m, nil, m.GetGlobal(i).StartValue);

  // normalizing function body
  for i:=0 to m.FuncCount-1 do begin
    f:=m.GetFunc(i);
    // finding the reference in functions
    // populating "nums" where string "index" is used
    NormalizeInst(m, f, f.instr);
  end;

  // normalizing exports
  for i:=0 to m.ExportCount-1 do begin
    x:=m.GetExport(i);
    if x.exportNum<0 then
      case x.exportType of
        EXPDESC_FUNC:
          if x.exportIdx<>'' then
            x.exportNum := FindFunc(m, x.exportIdx);
      end;
  end;
end;


end.

