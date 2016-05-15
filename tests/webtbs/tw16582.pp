program Project1;

{$mode objfpc}{$H+}
{$inline on}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils;

type

  { T1 }

  generic T1<_T> = class
    private
      i:_T;
      procedure SetF(v:_T);
      function GetF:_T;
  end;

  TPointerList = specialize T1<Pointer>;

  { TPointerList2 }

  generic TPointerList2<_T2> = class(TPointerList)
    public
      procedure SetF(v:_T2);//inline; //when uncommented gives error - Illegal expression.
      procedure WriteLn;
  end;

  TPointerListInt = specialize TPointerList2<PInteger>;
  TPointerListDouble = specialize TPointerList2<PDouble>;

  { T1 }

procedure T1.SetF(v: _T);
begin
  i:=v;
end;

function T1.GetF: _T;
begin
  Result:=i;
end;

{ TPointerList2 }

procedure TPointerList2.SetF(v: _T2); inline;
begin
  inherited SetF( Pointer(v) );
end;

procedure TPointerList2.WriteLn;
var S:string;
begin
  S:=Format('%P', [i] );
  System.WriteLn(S);
end;

var IntO:TPointerListInt;
    DoubleO:TPointerListDouble;
begin
  IntO:=TPointerListInt.Create;
  IntO.SetF( PInteger(nil) );
  IntO.WriteLn;
  IntO.Free;

  DoubleO:=TPointerListDouble.Create;
  DoubleO.SetF( PDouble(nil) );
  DoubleO.WriteLn;
  DoubleO.Free;
end.

