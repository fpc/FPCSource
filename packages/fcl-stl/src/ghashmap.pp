  {
     This file is part of the Free Pascal FCL library.
     BSD parts (c) 2011 Vlado Boza

     See the file COPYING.FPC, included in this distribution,
     for details about the copyright.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY;without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}
  {$mode objfpc}

  { $define STL_INTERFACE_EXT}

  unit ghashmap;

  interface
  uses gvector, gutil, garrayutils;

  const
    baseFDataSize = 8;             // must be > 0
    maxLoadingFactor = 1.0;

  {
    THash should have the class functions
      hash(a: TKey, n: SizeUInt): SizeUInt;
      	      return uniformly distributed i value in range <0,n-1> base only on arguments,
	      n will be always power of 2
      equal(const AKey1, AKey2: TKey): Boolean;            [when STL_INTERFACE_EXT is defined]
              return the boolean test for equality of the two keys.  Typically this is operator=,
              but it doesn't have to be (e.g. case-insensitive string comparison)
  }

  type
    generic THashmapIterator<TKey, TValue, T, TTable>=class
      public
      type PValue=^TValue;
      var
        Fh,Fp:SizeUInt;
        FData:TTable;
        function Next:boolean;inline;
        function Prev:boolean;inline;
        function GetData:T;inline;
        function GetKey:TKey;inline;
        function GetValue:TValue;inline;
        function GetMutable:PValue;inline;
        procedure SetValue(value:TValue);inline;
        property Data:T read GetData;
        property Key:TKey read GetKey;
        property Value:TValue read GetValue write SetValue;
        property MutableValue:PValue read GetMutable;
    end;

    generic THashmap<TKey, TValue, Thash>=class
      public
      type
        TPair=record
          Value:TValue;
          Key:TKey;
        end;
      var
      private 
      type
        TContainer = specialize TVector<TPair>;
        TTable = specialize TVector<TContainer>;
      var 
        FData:TTable;
        FDataSize:SizeUInt; 
        procedure EnlargeTable;
      public 
      type
        TIterator = specialize THashmapIterator<TKey, TValue, TPair, TTable>;
        constructor create;
        destructor destroy;override;
        procedure insert(key:TKey;value:TValue);inline;
        function contains(key:TKey):boolean;inline;
        function size:SizeUInt;inline;
        procedure delete(key:TKey);inline;
        procedure erase(iter:TIterator);inline;
        function IsEmpty:boolean;inline;
        function GetData(key:TKey):TValue;inline;
        function GetValue(key:TKey;out value:TValue):boolean;inline;

        property Items[i : TKey]: TValue read GetData write Insert; default;

        function Iterator:TIterator;
  end;

implementation

function THashmap.Size: SizeUInt;
begin
  Size:=FDataSize;
end;

destructor THashmap.Destroy;
var i:SizeUInt;
begin
  i:=0;
  while i < FData.size do
  begin
    (FData[i]).Destroy;
    inc(i);
  end;
  FData.Destroy;
end;

function THashmap.IsEmpty(): boolean;
begin
  IsEmpty := Size()=0;
end;

procedure THashmap.EnlargeTable;
var i,j,h,oldDataSize:SizeUInt; 
    curbucket:TContainer;
    value:TPair;
begin
  //Assert(oldDataSize>0);
  oldDataSize:=FData.size;
  FData.resize(FData.size*2);
  for i:=oldDataSize to FData.size-1 do
    FData[i] := TContainer.create;
  for i:=oldDataSize-1 downto 0 do begin
    curbucket:=FData[i];
    j := 0;
    while j < curbucket.size do begin
      h:=THash.hash(curbucket[j].key,FData.size);
      if (h <> i) then begin
        if (j+1) < curbucket.size then begin
          value:=curbucket[j];
          curbucket[j]:= curbucket.back;
          (FData[h]).pushback(value);
        end else
          (FData[h]).pushback(curbucket[j]);
        curbucket.popback;
      end else
        inc(j);
    end;
  end;
end;

constructor THashmap.create;
var i: SizeUInt;
begin
  FDataSize:=0;
  FData:=TTable.create;
  FData.resize(baseFDataSize);
  for i:=0 to baseFDataSize-1 do
    FData[i]:=TContainer.create;
end;

function THashmap.contains(key: TKey): boolean;
var i,bs:SizeUInt;
    curbucket:TContainer;
begin
  curbucket:=FData[THash.hash(key,FData.size)];
  bs:=curbucket.size;
  i:=0;
  while i < bs do begin
{$ifdef STL_INTERFACE_EXT}
    if THash.equal(curbucket[i].Key, key) then exit(true);
{$else}
    if (curbucket[i].Key = key) then exit(true);
{$endif}
    inc(i);
  end;
  exit(false);
end;

function THashmap.GetData(key: TKey): TValue;
var i,bs:SizeUInt;
    curbucket:TContainer;
begin
  curbucket:=FData[THash.hash(key,FData.size)];
  bs:=curbucket.size;
  i:=0;
  while i < bs do begin
{$ifdef STL_INTERFACE_EXT}
    if THash.equal(curbucket[i].Key, key) then exit(curbucket[i].Value);
{$else}
    if (curbucket[i].Key = key) then exit(curbucket[i].Value);
{$endif}
    inc(i);
  end;
  // exception?
end;

function THashmap.GetValue(key: TKey; out value: TValue): boolean;
var i,bs:SizeUInt;
    curbucket:TContainer;
begin
  curbucket:=FData[THash.hash(key,FData.size)];
  bs:=curbucket.size;
  i:=0;
  while i < bs do begin
{$ifdef STL_INTERFACE_EXT}
    if THash.equal(curbucket[i].Key, key) then begin
{$else}
    if (curbucket[i].Key = key) then begin
{$endif}
      value:=curbucket[i].Value;
      exit(true);
    end;
    inc(i);
  end;
  exit(false);
end;

procedure THashmap.insert(key: TKey; value: TValue);
var pair:TPair;
    i,bs:SizeUInt;
    curbucket:TContainer;
begin
  curbucket:=FData[THash.hash(key,FData.size)];
  bs:=curbucket.size;
  i:=0;
  while i < bs do begin
{$ifdef STL_INTERFACE_EXT}
    if THash.equal(curbucket[i].Key, key) then begin
{$else}
    if (curbucket[i].Key = key) then begin
{$endif}
      (curbucket.mutable[i])^.value := value;
      exit;
    end;
    inc(i);
  end;
  pair.Key := key;
  pair.Value := value;
  inc(FDataSize);
  curbucket.pushback(pair);

  if (FDataSize > maxLoadingFactor*FData.size) then
    EnlargeTable;
end;

procedure THashmap.delete(key: TKey);
var i,bs:SizeUInt;
    curbucket:TContainer;
begin
  curbucket:=FData[THash.hash(key,FData.size)];
  bs:=curbucket.size;
  i:=0;
  while i < bs do begin
{$ifdef STL_INTERFACE_EXT}
    if THash.equal(curbucket[i].Key, key) then begin
{$else}
    if (curbucket[i].Key = key) then begin
{$endif}
      //if (i+1) < bs then
        curbucket[i] := curbucket.back;
      curbucket.popback;
      dec(FDataSize);
      exit;
    end;
    inc(i);
  end;
end;

procedure THashmap.erase(iter: TIterator);
var curbucket:TContainer;
begin
  curbucket:=FData[iter.Fh];
  //if (iter.Fp+1) < curbucket.size then
    curbucket[iter.Fp] := curbucket.back;
  curbucket.popback;
  dec(FDataSize);
  iter.Prev;
end;

function THashmapIterator.Next: boolean;
begin
  Assert(Fh < FData.size);      // assumes FData.size>0 (i.e. buckets don't shrink) and cannot call Next again after reaching end
  inc(Fp);
  if (Fp < (FData[Fh]).size) then
    exit(true);
  Fp:=0; Inc(Fh);
  while Fh < FData.size do begin
    if ((FData[Fh]).size > 0) then
      exit(true);
    Inc(Fh);
  end;
  //Assert((Fp = 0) and (Fh = FData.size));
  exit(false);
end;

function THashmapIterator.Prev: boolean;
var bs:SizeUInt;
begin
  if (Fp > 0) then begin
    dec(Fp);
    exit(true);
  end;
  while Fh > 0 do begin
    Dec(Fh);
    bs:=(FData[Fh]).size;
    if (bs > 0) then begin
      Fp:=bs-1;
      exit(true);
    end;
  end;
  //Assert((Fp = 0) and (Fh = 0));
  exit(false);
end;

function THashmapIterator.GetData: T;
begin
  GetData:=(FData[Fh])[Fp];
end;

function THashmap.Iterator: TIterator;
var h,p:SizeUInt;
begin
  h:=0;
  p:=0;
  while h < FData.size do begin
    if ((FData[h]).size > 0) then break;
    inc(h);
  end;
  if (h = FData.size) then exit(nil);
  Iterator := TIterator.create;
  Iterator.Fh := h;
  Iterator.Fp := p;
  Iterator.FData := FData;
end;

function THashmapIterator.GetKey: TKey;
begin
  GetKey:=((FData[Fh])[Fp]).Key;
end;

function THashmapIterator.GetValue: TValue;
begin
  GetValue:=((FData[Fh])[Fp]).Value;
end;

function THashmapIterator.GetMutable: PValue;
begin
  GetMutable:=@((FData[Fh]).Mutable[Fp]^.Value);
end;

procedure THashmapIterator.SetValue(value:TValue);
begin
  ((FData[Fh]).mutable[Fp])^.Value := value;
end;

end.
