{ %NORUN }

program tw32111;
{$MODE OBJFPC}{$H+}{$B-}
uses
  SysUtils;

type generic
  TDynArray<T> = array of T;

type generic
  TSorter<T, C> = class
  protected
  var
    Tmp: C;
    procedure _Sort(constref Index: specialize TDynArray<C>;
                     constref Values: specialize TDynArray<T>;
                     StartIndex, EndIndex: Int32); virtual;
  public
    procedure Sort(constref Index: specialize TDynArray<C>;
                     constref Values: specialize TDynArray<T>;
                     StartIndex, EndIndex: Int32); virtual;
  end;


procedure TSorter.Sort(constref Index: specialize TDynArray<C>;
                       constref Values: specialize TDynArray<T>;
                       StartIndex, EndIndex: Int32);
var
  Len: Int32;
  I : C;

begin
  // some code
  Len:= System.Length(Values);
  if (Len = 0) or ( Assigned(Index) and (Len <> System.Length(Index)) ) then Exit;
  if Assigned(Index)
  then begin
    for I:= C(Startindex) to C(EndIndex) do Index[I]:= I;
    I:= 1;
  end;
  Self._Sort(Index, Values, StartIndex, EndIndex);
end;

procedure TSorter._Sort(constref Index: specialize TDynArray<C>;
                        constref Values: specialize TDynArray<T>;
                        StartIndex, EndIndex: Int32);
begin
  // some code
end;

var
  s: specialize TSorter<Unicodestring, Int32>;

begin

  s:= specialize TSorter<Unicodestring, Int32>.Create;

end.
