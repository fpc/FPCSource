{ %NORUN }

program tw39869;

{$mode objfpc}{$H+}

uses
  TypInfo, SysUtils;

{$DEFINE _WORKING}
{$DEFINE NOTWORKING}

type
{$IFDEF NOTWORKING}
  generic TCallProcStdCall<T> = procedure(aArg1:T;aArg2:Integer;aArg3:Integer) of object;stdcall;
  TGenericCallProcIntegerStdCall = specialize TCallProcStdCall<Integer>;
{$ENDIF}

  { TTest }
  generic TGenericTest<T> = class
{$IFDEF WORKING}
  type
    TCallProcStdCall = procedure(aArg1:T;aArg2:Integer;aArg3:Integer) of object;stdcall;
{$ENDIF}
  public
    procedure StdCalling(aArg1:T;aArg2:Integer;aArg3:Integer);stdcall;
  end;

  TIntTest = specialize TGenericTest<Integer>;

{ TTest }
procedure TGenericTest.StdCalling(aArg1:T;aArg2:Integer;aArg3:Integer); stdcall;
begin
  WriteLn('Self=0x'+IntToHex(IntPtr(self),SizeOf(self)*2)+
          ' Arg1='+IntToStr(PtrInt(aArg1))+
          ' Arg2='+IntToStr(aArg2)+
          ' Arg3='+IntToStr(aArg3));
end;

var
  obj       : TIntTest;
{$IFDEF NOTWORKING}
  stdCallPtr: TGenericCallProcIntegerStdCall;
{$ENDIF}
{$IFDEF WORKING}
  stdCallPtr: specialize TGenericTest<integer>.TCallProcStdCall;
{$ENDIF}
begin
  obj := TIntTest.Create;
  try
    //project1.lpr(51,23) Error:
    //Incompatible types:
    //got      "<procedure variable type of procedure(LongInt;LongInt;LongInt) of object;StdCall>"
    //expected "<procedure variable type of procedure(LongInt;LongInt;LongInt) of object;Register>"
    stdCallPtr := @obj.StdCalling;

    obj.StdCalling(1,2,3);

    //call is made with wrong calling convention
    stdCallPtr(1,2,3);

    //readln;
  finally
    obj.Free;
  end;
end.

