unit wasmmodule;

interface

uses
  Classes, SysUtils;

type
  TWasmParam = class(TObject)
    id : string;
    tp : byte;
  end;

  { TWasmType }

  TWasmType = class(TObject)
  private
    params  : TList;
    results : TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddResult(tp: byte = 0): TWasmParam;
    function AddParam(tp: byte = 0; const id: string = ''): TWasmParam;
    function GetParam(i: integer): TWasmParam;
    function GetResult(i: integer): TWasmParam; overload;
    function GetResult: TWasmParam;  overload;
    function ResultCount: Integer;
    function ParamCount: Integer;
  end;

  TWasmInstr = class(TObject)
    code        : byte;
    operandIdx  : string;
    operandNum  : integer;
    operandText : string;
  end;

  { TWasmInstrList }

  TWasmInstrList = class(TObject)
  private
    items: TList;
    function GetItem(i: integer): TWasmInstr;
  public
    constructor Create;
    destructor Destroy; override;
    function AddInstr(acode: byte = 0): TWasmInstr;
    function Count: Integer;
    property Item[i: integer]: TWasmInstr read GetItem; default;
  end;

  { TWasmFunc }

  TWasmFunc = class(TObject)
  private
    finlineType: TWasmType;
    locals:  TList;
  public
    id : string;
    typeIdx : Integer; // if Idx < 0 then type is declared from typeDef
    typeId  : string;  // if tpyeID='' then type is declared from typeDef
    instr   : TWasmInstrList;
    constructor Create;
    destructor Destroy; override;
    function GetInlineType: TWasmType;
    function AddLocal: TWasmParam;
    function LocalsCount: integer;
  end;

  { TWasmExport }

  TWasmExport = class(TObject)
    name       : string;
    exportType : byte;
    exportNum  : integer;
    exportIdx  : string;
  end;

  { TWasmModule }

  TWasmModule = class(TObject)
  private
    types   : TList;
    funcs   : TList;
    exp     : TList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddFunc: TWasmFunc;
    function GetFunc(i: integer): TWasmFunc;
    function FuncCount: integer;

    function AddType: TWasmType;
    function GetTypes(i: integer): TWasmType;
    function TypesCount: integer;

    function AddExport: TWasmExport;
    function GetExport(i: integer): TWasmExport;
    function ExportCount: integer;
  end;

implementation

procedure ClearList(l: TList);
var
  i : integer;
begin
  for i:=0 to l.Count-1 do
    TObject(l[i]).Free;
  l.Clear;
end;

{ TWasmInstrList }

function TWasmInstrList.GetItem(i: integer): TWasmInstr;
begin
  if (i>=0) and (i < items.Count) then
    Result:=TWasmInstr(items[i])
  else
    Result:=nil;
end;

constructor TWasmInstrList.Create;
begin
  inherited Create;
  items:=TList.Create;
end;

destructor TWasmInstrList.Destroy;
begin
  ClearList(items);
  items.Free;
  inherited Destroy;
end;

function TWasmInstrList.AddInstr(acode: byte = 0): TWasmInstr;
begin
  Result:=TWasmInstr.Create;
  Result.code:=acode;
  items.Add(Result);
end;

function TWasmInstrList.Count: Integer;
begin
  Result:=items.Count;
end;

{ TWasmType }

constructor TWasmType.Create;
begin
  inherited Create;
  params:=Tlist.Create;
  results:=Tlist.Create;
end;

destructor TWasmType.Destroy;
begin
  ClearList(params);
  ClearList(results);
  params.free;
  results.free;
  inherited Destroy;
end;

function TWasmType.AddResult(tp: byte): TWasmParam;
begin
  Result:=TWasmParam.Create;
  Result.tp:=tp;
  results.Add(Result);
end;

function TWasmType.AddParam(tp: byte; const id: string): TWasmParam;
begin
  Result:=TWasmParam.Create;
  Result.tp:=tp;
  Result.id:=id;
  params.Add(Result);
end;

function TWasmType.GetParam(i: integer): TWasmParam;
begin
  if (i>=0) and (i<params.Count) then
    Result:=TWasmParam(params[i])
  else
    Result:=nil;
end;

function TWasmType.GetResult(i: integer): TWasmParam;
begin
  if (i>=0) and (i<results.Count) then
    Result:=TWasmParam(results[i])
  else
    Result:=nil;
end;

function TWasmType.GetResult: TWasmParam;
begin
  Result:=GetResult(0);
end;

function TWasmType.ResultCount: Integer;
begin
  Result:=results.Count;
end;

function TWasmType.ParamCount: Integer;
begin
  Result:=params.Count;
end;

{ TWasmModule }

constructor TWasmModule.Create;
begin
  inherited Create;
  types := TList.Create;
  funcs := TList.Create;
  exp := TList.Create;
end;

destructor TWasmModule.Destroy;
begin
  ClearList(exp);
  exp.Free;
  ClearList(types);
  types.Free;
  ClearList(funcs);
  funcs.Free;
  inherited Destroy;
end;

function TWasmModule.AddFunc: TWasmFunc;
begin
  Result:=TWasmFunc.Create;
  funcs.Add(Result);
end;

function TWasmModule.AddType: TWasmType;
begin
  Result:=TWasmType.Create;
  types.Add(Result);
end;

function TWasmModule.GetFunc(i: integer): TWasmFunc;
begin
  if (i>=0) and (i<funcs.Count) then
    Result:=TWasmFunc(funcs[i])
  else
    Result:=nil;
end;

function TWasmModule.FuncCount: integer;
begin
  Result:=funcs.Count;
end;

function TWasmModule.GetTypes(i: integer): TWasmType;
begin
  if (i>=0) and (i<types.Count) then
    Result:=TWasmType(types[i])
  else
    Result:=nil;
end;

function TWasmModule.TypesCount: integer;
begin
  Result:=types.Count;
end;

function TWasmModule.AddExport: TWasmExport;
begin
  Result:=TWasmExport.Create;
  exp.add(Result);
end;

function TWasmModule.GetExport(i: integer): TWasmExport;
begin
  if (i>=0) and (i<exp.Count) then
    Result:=TWasmExport(exp[i])
  else
    Result:=nil;
end;

function TWasmModule.ExportCount: integer;
begin
  Result:=exp.Count;
end;

{ TWasmFunc }

constructor TWasmFunc.Create;
begin
  inherited;
  typeIdx:=-1;
  locals:=TList.Create;
  instr:=TWasmInstrList.Create;
end;

destructor TWasmFunc.Destroy;
begin
  ClearList(locals);
  locals.Free;
  finlineType.Free;
  instr.Free;
  inherited Destroy;
end;

function TWasmFunc.GetInlineType: TWasmType;
begin
  if not Assigned(fInlineType) then finlineType:=TWasmType.Create;
  Result:=finlineType;
end;

function TWasmFunc.AddLocal: TWasmParam;
begin
  Result:=TWasmParam.Create;
  locals.AdD(Result);
end;

function TWasmFunc.LocalsCount: integer;
begin
  result:=locals.Count;
end;

end.
