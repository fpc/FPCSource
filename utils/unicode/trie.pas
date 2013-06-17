{   Simple TRIE implementation.

    Copyright (c) 2012 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }
unit trie;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  MAX_CHILD_COUNT = 256;
type
  TKeyType = Cardinal;
  TDataType = Integer;
  PTrieNode = ^TTrieNode;
  TTrieNode = packed record
    Key        : TKeyType;
    DataNode   : Boolean;
    Data       : TDataType;
    ChildCount : Byte;
    Children   : array[0..(MAX_CHILD_COUNT-1)] of PTrieNode;
  end;

  function CreateNode(
    const AKey      : TKeyType;
    const AData     : TDataType
  ) : PTrieNode; overload;
  function CreateNode(const AKey : TKeyType) : PTrieNode;overload;
  procedure FreeNode(ANode : PTrieNode);
  function InsertWord(
    const ARoot   : PTrieNode;
    const AWord   : array of TKeyType;
    const AValue  : TDataType
  ) : Boolean;overload;
  function InsertWord(
    const ARoot   : PTrieNode;
    const AWord   : TKeyType;
    const AValue  : TDataType
  ) : Boolean;overload;

implementation

function CreateNode(
  const AKey      : TKeyType;
  const AData     : TDataType
) : PTrieNode;
begin
  New(Result);
  Result^.Key := AKey;
  Result^.DataNode := True;
  Result^.Data := AData;
  Result^.ChildCount := 0;
end;

function CreateNode(const AKey : TKeyType) : PTrieNode;
begin
  New(Result);
  Result^.Key := AKey;
  Result^.DataNode := False;
  Result^.ChildCount := 0;
end;

procedure FreeNode(ANode : PTrieNode);
var
  p : PTrieNode;
  i : Integer;
begin
  if (ANode = nil) then
    exit;
  p := ANode;
  for i := 0 to p^.ChildCount - 1 do
    FreeNode(p^.Children[i]);
  Dispose(p);
end;

function InsertWord(
  const ARoot   : PTrieNode;
  const AWord   : TKeyType;
  const AValue  : TDataType
) : Boolean;
begin
  Result := InsertWord(ARoot,[AWord],AValue);
end;

function InsertWord(
  const ARoot   : PTrieNode;
  const AWord   : array of TKeyType;
  const AValue  : TDataType
) : Boolean;
var
  p : PTrieNode;
  i, k, c : Integer;
  searching : TKeyType;
  found : Boolean;
begin
  Result := False;
  if (ARoot^.Key <> AWord[0]) then
    exit;
  p := ARoot;
  i := 1;
  c := Length(AWord);
  while (i < c) do begin
    searching := AWord[i];
    found := False;
    for k := 0 to p^.ChildCount - 1 do begin
      if (p^.Children[k]^.Key = searching) then begin
        p :=  p^.Children[k];
        found := True;
        Break;
      end;
    end;
    if not found then
      Break;
    Inc(i);
  end;
  if (i < c) then begin
    if (i = c) then
      i := i - 1;
    for i := i to c - 2 do begin
      k := p^.ChildCount;
      p^.Children[k] := CreateNode(AWord[i]);
      p^.ChildCount := k + 1;
      p := p^.Children[k];
    end;
    i := c - 1;
    k := p^.ChildCount;
    p^.Children[k] := CreateNode(AWord[i],AValue);
    p^.ChildCount := k + 1;
    Result := True;
  end;
end;



end.

