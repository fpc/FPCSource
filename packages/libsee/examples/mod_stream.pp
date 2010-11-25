unit mod_stream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libsee;

Procedure RegisterStreamModule;
Procedure RegisterWriteModule;

implementation

{ ---------------------------------------------------------------------
  General auxiliary functions
  ---------------------------------------------------------------------}

Function ValueToString(V : TSee_Value) : string;

Var
  PS : Ptcuint;
  PD : PChar;
  I : Integer;

begin
  SetLength(Result,v.u._string^.length);
  If Length(Result)<>0 then
    begin
    PD:=PChar(Result);
    PS:=v.u._string^.data;
    For I:=0 to length(Result)-1 do
      begin
      PD^:=Char(PS^ and $ff);
      Inc(PD);
      Inc(PS);
      end;
    end;
end;

Procedure CreateJSObject(Interp : PSEE_Interpreter; Parent : PSEE_Object;AName : PSEE_String; Obj : PSee_Object);

var
  V : PSEE_Value;

begin
  v:=new_see_value;
  see_set_object(V,Obj);
  see_object_put(interp,parent,AName,V,SEE_ATTR_DEFAULT);
end;

Procedure CreateJSNumber(Interp : PSEE_Interpreter; Obj : PSee_Object; AName : PSEE_String; AValue : TSEE_number_t);

var
  V : PSEE_Value;

begin
  v:=new_SEE_value;
  see_set_number(V,AValue);
  see_object_put(Interp,Obj,AName,v,SEE_ATTR_DEFAULT);
end;

Procedure CreateJSFunction(Interp : PSEE_Interpreter; Obj : PSee_Object; Func : TSEE_call_fn_t; AName : PSEE_String; Len : Integer);

var
  V : PSEE_Value;

begin
  v:=new_SEE_value;
  see_set_object(V,see_cfunction_make(interp,Func,AName,len));
  see_object_put(Interp,Obj,AName,v,SEE_ATTR_DEFAULT);
end;

{ ---------------------------------------------------------------------
  Stream module support
  ---------------------------------------------------------------------}
Var
  StreamModule : TSEE_module;
  StreamObjectDef,
  StreamPrototypeDef : PSEE_objectclass;

  WriteModule : TSEE_module;

Type
  TStreamModuleData = record
    Stream : PSEE_object;
    Prototype :  PSEE_object;
    Error : PSEE_object;
  end;
  PStreamModuleData = ^TStreamModuleData;

  TStreamObject = record
    native : TSEE_native;
    Stream : TStream;
  end;
  PSTreamObject = ^TStreamObject;

Var
  GStreamRead,
  GStreamWrite,
  GStreamSeek,
  GStreamSize,
  GStreamPosition,
  GStreamFree,
  GStreamfmCreate,
  GStreamfmOpenRead,
  GStreamfmOpenWrite,
  GStreamfmOpenReadWrite,
  GStreamStream,
  GStreamError,
  GStreamPrototype : PSEE_String;

Procedure StreamAlloc(Interp : PSEE_Interpreter); cdecl;

begin
  PPointer(see_module_private(Interp,@StreamModule))^:=new(PStreamModuleData);
end;

Function PrivateData(Interp : PSEE_Interpreter) : PStreamModuleData;
begin
  Result:=PStreamModuleData((see_module_private(Interp,@StreamModule))^)
end;

Function AsFile(i:PTSEE_interpreter; obj:PTSEE_object) : PStreamObject;

begin
  If (Not Assigned(obj)) or (Obj^.objectclass<>StreamPrototypeDef) then
      SEE_error__throw0(i,I^.TypeError,Nil);
  Result:=PStreamObject(Obj)
end;

procedure StreamSize (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,Nil);
  SEE_SET_NUMBER(res,S^.Stream.Size);
end;


procedure StreamWrite (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;
  v : TSEE_Value;
  t : string;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,'File is closed');
  if (ArgC=0) then
    SEE_error__throw0(i,I^.RangeError,'Missing argument');
  SEE_ToString(i,argv[0], @v);
  T:=ValueToString(V);
  If Length(T)>0 then
    S^.Stream.Write(T[1],Length(T));
end;

procedure StreamPosition (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;
  v : TSEE_Value;
  t : string;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,'File is closed');
  SEE_SET_NUMBER(res,S^.Stream.Position);
end;

procedure StreamSeek (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;
  v : TSEE_Value;
  newpos : integer;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,'File is closed');
  if (ArgC=0) then
    SEE_error__throw0(i,I^.RangeError,'Missing argument');
  newpos:=SEE_ToUint32(i,argv[0]);
  SEE_SET_NUMBER(res,S^.Stream.Seek(soFromBeginning,newpos));
end;

procedure StreamRead (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;
  r : PSEE_String;
  j,maxlen : integer;
  c : char;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,'File is closed');
  if (ArgC=0) then
    maxlen:=1024
  else
    maxlen:=see_touint32(I,argv[0]);
  r:=see_string_new(I,maxlen);
  For j:=0 to maxLen-1 do
    begin
    S^.stream.Read(c,sizeOf(c));
    SEE_string_addch(R,ord(c));
    end;
  SEE_SET_STRING(Res,r);
end;

procedure StreamFree (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  S : PStreamObject;
  v : TSEE_Value;
  t : string;

begin
  S:=AsFile(I,ThisObj);
  If (S^.Stream=Nil) then
    SEE_error__throw0(i,PrivateData(I)^.Error,'File is closed');
  FreeAndNil(S^.Stream);
  SEE_SET_UNDEFINED(Res);
end;

procedure StreamFinalize ( i:PTSEE_interpreter; p:pointer; closure:pointer);cdecl;

begin
  FreeAndNil(PStreamObject(P)^.Stream);
end;

procedure StreamConstruct (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  P : PChar;
  fm : Integer;
  S : TStream;
  Err : String;
  R : PTSEE_Object;

begin
  SEE_parse_args(i,argc,argv,'Z|i',@p,@fm);
  If (P=Nil) then
    SEE_error__throw0(i,I^.RangeError,'Missing argument');
  Err:='';
  try
    S:=TFileStream.Create(strpas(p),fm);
  except
    On E : Exception do
      Err:=E.Message;
  end;
  If (Err<>'') then
    SEE_error__throw0(i,PrivateData(I)^.Error,PChar(Err));
  R:=PTSEE_Object(SEE_malloc_finalize(I,SizeOf(TStreamObject),@StreamFinalize,Nil));
  SEE_Native_init(PSEE_Native(R),I,StreamPrototypeDef,PrivateData(I)^.Prototype);
  PStreamObject(r)^.Stream:=S;
  SEE_SET_OBJECT(Res,R);
end;



Procedure StreamInit(Interp : PSEE_Interpreter); cdecl;

Var
  Stream,
  StreamPrototype,
  StreamError : PSee_object;
begin
//  writeln('Initializing stream');
  // Construct Stream.prototype object
//  writeln('Creating Stream Prototype ');
  StreamPrototype:=PSEE_object(SEE_malloc(Interp,SizeOf(TSTreamObject)));
  See_native_init(PSEE_native(StreamProtoType),Interp,StreamPrototypeDef,interp^.Object_prototype);
  PSTreamObject(StreamPrototype)^.stream:=Nil;
  createJSFUnction(Interp,StreamPrototype,@StreamRead,GStreamRead,0);
  createJSFUnction(Interp,StreamPrototype,@StreamWrite,GStreamWrite,0);
  createJSFUnction(Interp,StreamPrototype,@StreamSize,GStreamSize,0);
  createJSFUnction(Interp,StreamPrototype,@StreamPosition,GStreamPosition,0);
  createJSFUnction(Interp,StreamPrototype,@StreamSeek,GStreamSeek,0);
  createJSFUnction(Interp,StreamPrototype,@StreamFree,GStreamFree,0);
//  writeln('Creating Stream');
  // Construct Stream object
  Stream:=PSEE_object(new_see_native);
  See_native_init(PSEE_native(Stream),Interp,StreamObjectDef,interp^.Object_prototype);
  CreateJSObject(Interp,Interp^.Global,GStreamStream,Stream);
  CreateJSObject(Interp,Stream,GStreamprototype,StreamPrototype);
  CreateJSNumber(Interp,Stream,GStreamfmCreate,fmCreate);
  CreateJSNumber(Interp,Stream,GStreamfmOpenRead,fmOpenRead);
  CreateJSNumber(Interp,Stream,GStreamfmOpenWrite,fmOpenWrite);
  CreateJSNumber(Interp,Stream,GStreamfmOpenReadWrite,fmOpenReadWrite);
  StreamError:=SEE_Error_make(interp, GSTreamError);
  PrivateData(Interp)^.Stream:=STream;
  PrivateData(Interp)^.Prototype:=StreamPrototype;
  PrivateData(Interp)^.Error:=StreamError;
//  writeln('Done initializing stream');
end;

Procedure AllocateStreamStrings;

begin
  GStreamRead:=SEE_intern_global('Read');
  GStreamWrite:=SEE_intern_global('Write');
  GStreamSeek:=SEE_intern_global('Seek');
  GStreamSize:=SEE_intern_global('Size');
  GStreamPosition:=SEE_intern_global('Position');
  GStreamFree:=SEE_intern_global('Free');
  GStreamfmCreate:=SEE_intern_global('fmCreate');
  GStreamfmOpenRead:=SEE_intern_global('fmOpenRead');
  GStreamfmOpenWrite:=SEE_intern_global('fmOpenWrite');
  GStreamfmOpenReadWrite:=SEE_intern_global('fmOpenReadWrite');
  GStreamStream:=SEE_intern_global('Stream');
  GStreamError:=SEE_intern_global('Error');
  GStreamPrototype:=SEE_intern_global('prototype');
end;

Function StreamInitModule : Integer; cdecl;

begin
//  writeln('Initializing module');
  StreamPrototypeDef:=new_SEE_objectclass;
  With StreamPrototypeDef^ do
    begin
    _Class:='Stream';
    get:=SEE_native_get;
    put:=SEE_native_put;
    canput:=SEE_native_canput;
    hasproperty:=SEE_native_hasproperty;
    Delete:=SEE_native_delete;
    DefaultValue:=SEE_native_defaultvalue;
    ENumerator:=SEE_native_enumerator;
    Construct:=Nil;
    Call:=Nil;
    HasInstance:=Nil;
    end;
  StreamObjectDef:=new_SEE_objectclass;
  With StreamObjectDef^ do
    begin
    _Class:='Stream';
    get:=SEE_native_get;
    put:=SEE_native_put;
    get:=SEE_native_get;
    put:=SEE_native_put;
    canput:=SEE_native_canput;
    hasproperty:=SEE_native_hasproperty;
    Delete:=SEE_native_delete;
    DefaultValue:=SEE_native_defaultvalue;
    ENumerator:=SEE_native_enumerator;
    Construct:=@StreamConstruct;
    Call:=Nil;
    HasInstance:=Nil;
    end;
  AllocateStreamStrings;
//  writeln('Done Initializing module');
  Result:=0;
end;

Procedure RegisterStreamModule;

begin
//  writeln('Registering stream module');
//  StreamModule:=new_SEE_module;
  With StreamModule do
    begin
    magic:=SEE_MODULE_MAGIC;
    name:='Stream';
    version:='1.0';
    Index:=0;
    Mod_init:=@StreamInitModule;
    alloc:=@StreamAlloc;
    init:=@StreamInit
    end;
  SEE_module_add(@StreamModule);
end;

{ ---------------------------------------------------------------------
  Write(ln) module support
  ---------------------------------------------------------------------}

procedure WriteWrite (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;

Var
  a,C : Integer;
  t : string;
  v : TSEE_Value;

begin
  if (ArgC=0) then
    SEE_error__throw0(i,I^.RangeError,'Missing argument');
  C:=0;
  For A:=0 to Argc-1 do
    begin
    SEE_ToString(i,argv[a], @v);
    T:=ValueToString(V);
    If Length(T)>0 then
      begin
      Write(T);
      C:=C+Length(T);
      end;
    end;
  SEE_SET_NUMBER(Res,C);
end;

procedure WriteWriteln (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value;
             res:PTSEE_value);cdecl;


begin
  if (Argc>0) then
    WriteWrite(i,obj,thisobj,argc,argv,res)
  else
    SEE_SET_NUMBER(Res,0);
  Writeln;
end;

Var
  GWriteWrite   : PSEE_STRING;
  GWriteWriteln : PSEE_STRING;

Procedure WriteInit(Interp : PSEE_Interpreter); cdecl;

begin
//  writeln('Initializing write');
  createJSFUnction(Interp,Interp^.Global,@WriteWrite,GWriteWrite,1);
  createJSFUnction(Interp,Interp^.Global,@WriteWriteln,GWriteWriteln,1);
//  writeln('Done initializing write');
end;

Procedure AllocateWriteStrings;

begin
  GWriteWrite:=SEE_intern_global('write');
  GWriteWriteln:=SEE_intern_global('writeln');
end;

Function WriteInitModule : Integer; cdecl;
begin
  Result:=0;
end;

Procedure RegisterWriteModule;

begin
//  writeln('Registering write module');
//  StreamModule:=new_SEE_module;
  With WriteModule do
    begin
    magic:=SEE_MODULE_MAGIC;
    name:='Write';
    version:='1.0';
    Index:=0;
    Mod_init:=@WriteInitModule;
    alloc:=Nil;
    init:=@WriteInit
    end;
  AllocateWriteStrings;
  SEE_module_add(@WriteModule);
end;


end.

