program testarray;
{$mode objfpc}
{$h+}
uses typinfo;

Procedure SetPointerProp(Instance : TObject;PropInfo : PPropInfo;Value : Pointer);

type
  TObjectArray = Array of tobject;
  TSetPointerProcIndex=procedure(index : longint;p:pointer) of object;
  TSetPointerProc=procedure(P : Pointer) of object;

var
  DataSize: Integer;
  AMethod : TMethod;
begin
  DataSize:=Length(TObjectArray(Value));
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
        PPointer(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetPointerProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetPointerProc(AMethod)(Value);
      end;
  end;
end;

{$M+}
Type
  TMyArrayObject = Class(TObject);
  TMyArrayObjectArray = Array of TMyArrayObject;

  { TMyObject }

  TMyObject = Class(TObject)
  private
    FMyArray : TMyArrayObjectArray;
    procedure SetMyArray(AIndex: Integer; AValue: TMyArrayObjectArray);virtual;
  Published
    Property MyArray : TMyArrayObjectArray Index 8 Read FMyArray Write SetMyArray;
  end;

{ TMyObject }

procedure TMyObject.SetMyArray(AIndex: Integer; AValue: TMyArrayObjectArray);
Var
  ALength : Integer;

begin
  ALength:=Length(AValue);
  If FMyArray=AValue then exit;
  FMyArray:=AValue;
end;

Var
  O : TMyObject;
  A : TMyArrayObjectArray;

begin
  SetLength(A,117);
  O:=TMyObject.Create;
  // SetObjProp(O,GetPropInfo(O,'MyArray'),TObject(A));
  SetPointerProp(O,GetPropInfo(O,'MyArray'),Pointer(A));
  If Length(O.MyArray)<>Length(A) then
    Writeln('Wrong!!')
end.
