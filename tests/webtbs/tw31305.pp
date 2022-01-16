program tw31305;

{$MODE DELPHI}

type
  TInterfaceStubLog = object
    TimeStamp64: Int64;
    WasError: boolean;
    Method: Pointer;
    Params: UTF8String;
    CustomResults: UTF8String;
  end;

var
  a,b: TInterfaceStubLog;
begin
  a.Method := Pointer($1);
  CopyArray(@b, @a, TypeInfo(TInterfaceStubLog), 1);
  if not Assigned(b.Method) then
    Halt(1);
end.
