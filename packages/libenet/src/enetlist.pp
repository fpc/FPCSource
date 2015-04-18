{$mode objfpc}{$H+}
unit enetlist;

{
  ENet - Reliable UDP networking library

  FreePascal DLL header: enetlist.pp
  Copyright (c) 2015 Dmitry D. Chernov aka Black Doomer

  Original file: list.h
  Copyright (c) 2002-2014 Lee Salzman

  Version 1 for 1.3.12: 25.02.2015

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

interface
                                  
type

{$PACKRECORDS C}

  pENetListNode = ^ENetListNode;
  ENetListNode = record
    next     : pENetListNode;
    previous : pENetListNode;
  end;

  ENetListIterator = pENetListNode;

  pENetList = ^TENetList;
  TENetList = record
    sentinel : ENetListNode;
  end;

{$PACKRECORDS DEFAULT}

//inline macros
function enet_list_begin( list: pENetList ): ENetListIterator; inline;
function enet_list_end( list: pENetList ): ENetListIterator; inline;

function enet_list_empty( list: pENetList ): Boolean; inline;

function enet_list_next( iterator: ENetListIterator ): ENetListIterator; inline;
function enet_list_previous( iterator: ENetListIterator ): ENetListIterator; inline;

function enet_list_front( list: pENetList ): Pointer; inline;
function enet_list_back( list: pENetList ): Pointer; inline;

implementation

function enet_list_begin( list: pENetList ): ENetListIterator; inline;
   begin Result := list^.sentinel.next;
     end;
function enet_list_end( list: pENetList ): ENetListIterator; inline;
   begin Result := @( list^.sentinel );
     end;

function enet_list_empty( list: pENetList ): Boolean; inline;
   begin Result := enet_list_begin(list) = enet_list_end(list);
     end;

function enet_list_next( iterator: ENetListIterator ): ENetListIterator; inline;
   begin Result := iterator^.next;
     end;
function enet_list_previous( iterator: ENetListIterator ): ENetListIterator; inline;
   begin Result := iterator^.previous;
     end;

function enet_list_front( list: pENetList ): Pointer; inline;
   begin Result := Pointer( list^.sentinel.next );
     end;
function enet_list_back( list: pENetList ): Pointer; inline;
   begin Result := Pointer( list^.sentinel.previous );
     end;

end.

