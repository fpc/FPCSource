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

unit gmap;

interface

uses gset;

type
  generic TMapCompare<TPair, TKeyCompare>=class
    class function c(a,b :TPair):boolean;
  end;

  generic TMap<TKey, TValue, TCompare>=class
  public
  type
    TPair=record
      Key:TKey;
      Value:TValue;
    end;
    TMCompare = specialize TMapCompare<TPair, TCompare>;
    TMSet = specialize TSet<TPair, TMCompare>;
    PTValue = ^TValue;
    PTPair = ^TPair;
  var
  private
    FSet:TMSet;
  public
    function Find(key:TKey):TMSet.PNode;inline;
    function FindLess(key:TKey):TMSet.PNode;inline;
    function FindLessEqual(key:TKey):TMSet.PNode;inline;
    function FindGreater(key:TKey):TMSet.PNode;inline;
    function FindGreaterEqual(key:TKey):TMSet.PNode;inline;
    function GetValue(key:TKey):TValue;inline;
    procedure Insert(key:TKey; value:TValue);inline;
    function Min:TMSet.PNode;inline;
    function Max:TMSet.PNode;inline;
    function Next(x:TMSet.PNode):TMSet.PNode;inline;
    function Prev(x:TMSet.PNode):TMSet.PNode;inline;
    procedure Delete(key:TKey);inline;
    function Size:SizeUInt;inline;
    function IsEmpty:boolean;inline;
    constructor Create;
    destructor Destroy;override;
    property Items[i : TKey]: TValue read GetValue write Insert; default;
  end;

implementation

class function TMapCompare.c(a,b: TPair):boolean;
begin
  c:= TKeyCompare.c(a.Key, b.Key);
end;

constructor TMap.Create;
begin
  FSet:=TMSet.Create;
end;

destructor TMap.Destroy;
begin
  FSet.Destroy;
end;

procedure TMap.Delete(key:TKey);inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FSet.Delete(Pair);
end;

function TMap.Find(key:TKey):TMSet.PNode;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  Find:=FSet.Find(Pair);
end;

function TMap.FindLess(key:TKey):TMSet.PNode;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FindLess:=FSet.FindLess(Pair);
end;

function TMap.FindLessEqual(key:TKey):TMSet.PNode;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FindLessEqual:=FSet.FindLessEqual(Pair);
end;

function TMap.FindGreater(key:TKey):TMSet.PNode;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FindGreater:=FSet.FindGreater(Pair);
end;

function TMap.FindGreaterEqual(key:TKey):TMSet.PNode;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FindGreaterEqual:=FSet.FindGreaterEqual(Pair);
end;

function TMap.GetValue(key:TKey):TValue;inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  GetValue:=FSet.Find(Pair)^.Data.Value;
end;

procedure TMap.Insert(key:TKey; value:TValue);inline;
var Pair:TPair;
begin
  Pair.Key:=key;
  FSet.Insert(Pair)^.Data.Value := value;
end;

function TMap.Min:TMSet.PNode;inline;
begin
  Min:=FSet.Min;
end;

function TMap.Max:TMSet.PNode;inline;
begin
  Max:=FSet.Max;
end;

function TMap.Next(x:TMSet.PNode):TMSet.PNode;inline;
begin
  Next:=FSet.Next(x);
end;

function TMap.Prev(x:TMSet.PNode):TMSet.PNode;inline;
begin
  Prev:=FSet.Prev(x);
end;

function TMap.Size:SizeUInt;inline;
begin
  Size:=FSet.Size;
end;

function TMap.IsEmpty:boolean;inline;
begin
  IsEmpty:=FSet.IsEmpty;
end;

end.
