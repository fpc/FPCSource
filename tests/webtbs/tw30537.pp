{ %NORUN }

{$mode objfpc}
program tw30537;
uses sysutils;

type
  generic TRange<_T, V> = object
    public
    Range:_T;
    function GetCount:NativeInt;
    function GetHigh:NativeInt;
    function GetItem(i:NativeInt):V;
    procedure SetItem(i:NativeInt; const E:V);

    property Count:NativeInt read GetCount;
    property High:NativeInt read GetHigh;
    property Item[i:NativeInt]:V read GetItem write SetItem;default;
  end;

  generic TRangeOffset<_T> = object
    private type
      TPtr=^_T;
    public
    
    P:TPtr;
    Count:NativeInt;

    private
      function GetCount:NativeInt;
      function GetHigh:NativeInt;
      function GetItem(i:NativeInt):_T;
      procedure SetItem(i:NativeInt; const E:_T);
  end;

  generic TRangePeriod<R, _T> = object
    public
     
    Range:R; 
    i1, i2:NativeInt;

    private
      function GetIndex(i:NativeInt):NativeInt;
      function GetCount:NativeInt;
      function GetHigh:NativeInt;
      function GetItem(i:NativeInt):_T;
      procedure SetItem(i:NativeInt; const E:_T);
  end;

function TRange.GetCount:NativeInt;
begin
  Result:=Range.GetCount;
end;

function TRange.GetHigh:NativeInt;
begin
  Result:=Range.GetHigh;
end;

function TRange.GetItem(i:NativeInt):V;
begin
  Result:=Range.GetItem(i);
end;

procedure TRange.SetItem(i:NativeInt; const E:V);
begin
  Range.SetItem(i, E);
end;

function TRangeOffset.GetCount:NativeInt;
begin
  Result:=Count;
end;

function TRangeOffset.GetHigh:NativeInt;
begin
  Result:=Count-1;
end;

function TRangeOffset.GetItem(i:NativeInt):_T;
begin
   Result:=(P+i)^;
end;

procedure TRangeOffset.SetItem(i:NativeInt; const E:_T);
begin
  (P+i)^:=E;
end;

function TRangePeriod.GetCount:NativeInt;
begin
  Result:=abs(i2-i1)+1;
end;

function TRangePeriod.GetHigh:NativeInt;
begin
  Result:=GetCount-1;
end;

function TRangePeriod.GetIndex(i:NativeInt):NativeInt;
begin
   if i1 <= i2 then
      Result:=i1+i
   else
      Result:=i1-i;
end;

function TRangePeriod.GetItem(i:NativeInt):_T;
begin
   Result:=Range.GetItem( GetIndex(i) );
end;

procedure TRangePeriod.SetItem(i:NativeInt; const E:_T);
begin
  Range.SetItem( GetIndex(i), E );
end;

generic function Find<T, V>(const R:T; const Val:V):NativeInt;
var i:NativeInt;
begin
  Result:=-1;
  for i:=0 to R.High do  // triggers Internal error 200204175 here
    if R[i] = Val then
      Exit(i);
end;

generic function Find<T>(const R:T; const Val:Char):NativeInt;
var i:NativeInt;
begin
  Result:=-1;
  for i:=0 to R.High do // triggers Internal error 200204175 here
    if R[i] = Val then
      Exit(i);
end;

type
  TRangeOffsetString = specialize TRangeOffset<Char>;
  TRangeString=specialize TRange<TRangeOffsetString, Char>;

  TRangePeriodString = specialize TRangePeriod<TRangeOffsetString, Char>;
  TRangeString2=specialize TRange<TRangePeriodString, Char>;

var
  RO:TRangeOffsetString;
  RP:TRangePeriodString;
  RS:TRangeString;
  RS2:TRangeString2;
  s:string;
  i:Integer;
begin
  s:='as123';
  RO.P:=@s[1];
  RO.Count:=Length(s);
  RS.Range:=RO;
  
  i:=specialize Find<TRangeString, Char>(RS, '2');
  WriteLn(i);

  i:=specialize Find<TRangeString>(RS, '2');
  WriteLn(i);

  s:='abbs33sd';
  RP.Range.P:=@s[1];
  RP.Range.Count:=Length(s);
  RP.i1:=2;
  RP.i2:=7;
  RS2.Range:=RP;
  
  i:=specialize Find<TRangeString2>(RS2, '2');
  WriteLn(i);
end.
