(*---------------------------------------------------------------------------------

	A simple linked list data structure

  Copyright (C) 2008
			Jason Rogers (dovoto)

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you
     must not claim that you wrote the original software. If you use
     this software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and
     must not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source
     distribution.


---------------------------------------------------------------------------------*)

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

function linkedlistAdd(front: PLinkedList; data: pointer): PLinkedList; inline;
procedure linkedlistRemove(node: PLinkedList); inline;

{$endif NDS_INTERFACE}


{$ifdef NDS_IMPLEMENTATION}
function linkedlistAdd(front: PLinkedList; data: pointer): PLinkedList; inline;
var
  node: PLinkedList;
begin
	GetMem(node, sizeof(LinkedList));

	node^.prev := nil;

	if (front = nil) then
	begin	
		node^.next := nil;

		node^.data := data;

		front := node;

		result := node;
	end;
    
	node^.next := front;
	
	front^.prev := node;

	result := node;
end;


procedure linkedlistRemove(node: PLinkedList); inline;
begin
	if (node = nil) then exit;

	if (node^.prev <> nil) then
		node^.prev^.next := node^.next;

	if (node^.next <> nil) then
		node^.next^.prev := node^.prev;

	FreeMem(node);
end;

{$endif NDS_IMPLEMENTATION}
