{$mode objfpc}{$H+}
unit enetplatform;

{
  ENet - Reliable UDP networking library

  FreePascal DLL header: enetplatform
  Copyright (c) 2015 Dmitry D. Chernov aka Black Doomer

  Original files: win32.h & unix.h
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

uses enettypes, // used only for size_t
     {$IFDEF WINDOWS} WinSock2 {$ELSE} BaseUnix, Sockets {$ENDIF};

const
  ENET_SOCKET_NULL = {$IFDEF WINDOWS} INVALID_SOCKET; {$ELSE} -1; {$ENDIF}
//  ENET_BUFFER_MAXIMUM = MSG_MAXIOVLEN; //is it forgotten in win32.h ?

type
  ENetSocket = {$IFDEF WINDOWS} TSocket {$ELSE} enet_int {$ENDIF};

  ENetSocketSet = TFDSet;
  pENetSocketSet = ^ENetSocketSet;

{$PACKRECORDS C}

  pENetBuffer = ^ENetBuffer;
  ENetBuffer = record
    {$IFDEF WINDOWS}
    dataLength : enet_size_t;
    data       : Pointer;
    {$ELSE}
    data       : Pointer;
    dataLength : enet_size_t;
    {$ENDIF}
  end;

{$PACKRECORDS DEFAULT}

//inline macros
function ENET_HOST_TO_NET_16( const value: Word ): Word; inline;
function ENET_HOST_TO_NET_32( const value: LongWord ): LongWord; inline;

function ENET_NET_TO_HOST_16( const value: Word ): Word; inline;
function ENET_NET_TO_HOST_32( const value: LongWord ): LongWord; inline;

procedure ENET_SOCKETSET_EMPTY( var sockset: ENetSocketSet ); inline;
procedure ENET_SOCKETSET_ADD( var sockset: ENetSocketSet; socket: ENetSocket ); inline;
procedure ENET_SOCKETSET_REMOVE( var sockset: ENetSocketSet; socket: ENetSocket ); inline;
function  ENET_SOCKETSET_CHECK( var sockset: ENetSocketSet; socket: ENetSocket ): Boolean; inline;

implementation

function ENET_HOST_TO_NET_16( const value: Word ): Word; inline;
   begin Result := htons(value);
     end;
function ENET_HOST_TO_NET_32( const value: LongWord ): LongWord; inline;
   begin Result := htonl(value);
     end;

function ENET_NET_TO_HOST_16( const value: Word ): Word; inline;
   begin Result := ntohs(value);
     end;
function ENET_NET_TO_HOST_32( const value: LongWord ): LongWord; inline;
   begin Result := ntohl(value);
     end;

procedure ENET_SOCKETSET_EMPTY( var sockset: ENetSocketSet ); inline;
    begin {$IFNDEF WINDOWS}fpFD_ZERO{$ELSE}FD_ZERO{$ENDIF}( sockset );
      end;
procedure ENET_SOCKETSET_ADD( var sockset: ENetSocketSet; socket: ENetSocket ); inline;
    begin {$IFNDEF WINDOWS}fpFD_SET{$ELSE}FD_SET{$ENDIF}( socket, sockset );
      end;
procedure ENET_SOCKETSET_REMOVE( var sockset: ENetSocketSet; socket: ENetSocket ); inline;
    begin {$IFNDEF WINDOWS}fpFD_CLR{$ELSE}FD_CLR{$ENDIF}( socket, sockset );
      end;

function ENET_SOCKETSET_CHECK( var sockset: ENetSocketSet; socket: ENetSocket ): Boolean; inline;
   begin Result := {$IFNDEF WINDOWS} fpFD_ISSET( socket, sockset ) <> 0
                               {$ELSE}   FD_ISSET( socket, sockset ) {$ENDIF};
     end;

end.

