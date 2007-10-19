program testarray3;

{$mode objfpc}{$H+}

type
  TFixedString15 = array[1..15] of char;
  generic TLinkedList<T> = class
    type
      PNode = ^TNode;
      TNode = record
      key: T;
      value : dword;
      next : PNode;
    end;
    var
      first: PNode;
  end;

var
  MyLinkedList: specialize TLinkedList<TFixedString15>;

begin
end.
