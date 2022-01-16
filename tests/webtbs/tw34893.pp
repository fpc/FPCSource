unit tw34893;
{$ifdef fpc}
  {$mode delphi}
{$endif}

{$scopedenums on}

interface

uses
  Classes, SysUtils;

type TPasGLTFSizeInt=SizeInt;

     TPasGLTFSizeUInt=SizeUInt;

     TPasGLTFObjectList<T:class>=class
      private
       type TValueEnumerator=record
             private
              fObjectList:TPasGLTFObjectList<T>;
              fIndex:TPasGLTFSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aObjectList:TPasGLTFObjectList<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TPasGLTFSizeInt;
       fAllocated:TPasGLTFSizeInt;
       fOwnsObjects:boolean;
       function RoundUpToPowerOfTwoSizeUInt(x:TPasGLTFSizeUInt):TPasGLTFSizeUInt;
       procedure SetCount(const pNewCount:TPasGLTFSizeInt);
       function GetItem(const pIndex:TPasGLTFSizeInt):T;
       procedure SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function IndexOf(const pItem:T):TPasGLTFSizeInt;
       function Add(const pItem:T):TPasGLTFSizeInt;
       procedure Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
       procedure Delete(const pIndex:TPasGLTFSizeInt);
       procedure Remove(const pItem:T);
       procedure Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt);
       function GetEnumerator:TValueEnumerator;
       property Count:TPasGLTFSizeInt read fCount write SetCount;
       property Allocated:TPasGLTFSizeInt read fAllocated;
       property Items[const pIndex:TPasGLTFSizeInt]:T read GetItem write SetItem; default;
       property OwnsObjects:boolean read fOwnsObjects write fOwnsObjects;
     end;

implementation

constructor TPasGLTFObjectList<T>.TValueEnumerator.Create(const aObjectList:TPasGLTFObjectList<T>);
begin
end;

function TPasGLTFObjectList<T>.TValueEnumerator.MoveNext:boolean;
begin
end;

function TPasGLTFObjectList<T>.TValueEnumerator.GetCurrent:T;
begin
end;

constructor TPasGLTFObjectList<T>.Create;
begin
end;

destructor TPasGLTFObjectList<T>.Destroy;
begin
end;

function TPasGLTFObjectList<T>.RoundUpToPowerOfTwoSizeUInt(x:TPasGLTFSizeUInt):TPasGLTFSizeUInt;
begin
end;

procedure TPasGLTFObjectList<T>.Clear;
var Index:TPasGLTFSizeInt;
begin
end;

procedure TPasGLTFObjectList<T>.SetCount(const pNewCount:TPasGLTFSizeInt);
var Index,NewAllocated:TPasGLTFSizeInt;
begin
end;

function TPasGLTFObjectList<T>.GetItem(const pIndex:TPasGLTFSizeInt):T;
begin
end;

procedure TPasGLTFObjectList<T>.SetItem(const pIndex:TPasGLTFSizeInt;const pItem:T);
begin
end;

function TPasGLTFObjectList<T>.IndexOf(const pItem:T):TPasGLTFSizeInt;
var Index:TPasGLTFSizeInt;
begin
end;

function TPasGLTFObjectList<T>.Add(const pItem:T):TPasGLTFSizeInt;
begin
end;

procedure TPasGLTFObjectList<T>.Insert(const pIndex:TPasGLTFSizeInt;const pItem:T);
var OldCount:TPasGLTFSizeInt;
begin
   System.Move(fItems[pIndex],fItems[pIndex+1],(OldCount-pIndex)*SizeOf(T));
end;

procedure TPasGLTFObjectList<T>.Delete(const pIndex:TPasGLTFSizeInt);
var Old:T;
begin

end;

procedure TPasGLTFObjectList<T>.Remove(const pItem:T);
var Index:TPasGLTFSizeInt;
begin
end;

procedure TPasGLTFObjectList<T>.Exchange(const pIndex,pWithIndex:TPasGLTFSizeInt);
var Temporary:T;
begin
end;

function TPasGLTFObjectList<T>.GetEnumerator:TPasGLTFObjectList<T>.TValueEnumerator;
begin
end;

end.

