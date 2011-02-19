{$ifdef NDS_INTERFACE}

type
  PLinkedList = ^LinkedList;
  PPLinkedList = ^PLinkedList;
  LinkedList = record
    next: PLinkedList;
    prev: PLinkedList;
    data: pointer;
  end;
  TLinkedList = LinkedList;

function linkedlistAdd(front: PLinkedList; data: pointer): PLinkedList; cdecl; external;
procedure linkedlistRemove(node: PLinkedList); cdecl; external;

{$endif NDS_INTERFACE}

