{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
// msgqueue.h - Message queue point-to-point.
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit MsgQueue;

{$CALLING cdecl}

interface

uses Windows;

// Declarations of constants and structures transferred from winbase.h.

const
      MSGQUEUE_NOPRECOMMIT            = $00000001;
      MSGQUEUE_ALLOW_BROKEN           = $00000002;
      
      MSGQUEUE_MSGALERT               = $00000001;


type
     MSGQUEUEOPTIONS_OS = record
       dwSize:DWORD;                           // size of the structure
       dwFlags:DWORD;                          // behavior of message queue
       dwMaxMessages:DWORD;                    // max # of msgs in queue
       cbMaxMessage:DWORD;                     // max size of msg
       bReadAccess:BOOL;                      // read access requested
     end;
     MSGQUEUEOPTIONS = MSGQUEUEOPTIONS_OS;
     LPMSGQUEUEOPTIONS = ^MSGQUEUEOPTIONS_OS;
     PMSGQUEUEOPTIONS = ^MSGQUEUEOPTIONS_OS;

type
     MSGQUEUEINFO = record
       dwSize:DWORD;                           // size of structure
       dwFlags:DWORD;                          // behavior of message queue
       dwMaxMessages:DWORD;                    // max # of msgs in queue
       cbMaxMessage:DWORD;                     // max size of msg
       dwCurrentMessages:DWORD;                // # of message in queue currently
       dwMaxQueueMessages:DWORD;               // high water mark of queue
       wNumReaders:word;                      // # of readers
       wNumWriters:word;                      // # of writes
     end;
     PMSGQUEUEINFO = ^MSGQUEUEINFO;
     LPMSGQUEUEINFO = ^MSGQUEUEINFO;

     
function CreateMsgQueue(lpName:LPCWSTR; lpOptions:LPMSGQUEUEOPTIONS):HANDLE; external KernelDLL name 'CreateMsgQueue'; // index 111
function OpenMsgQueue(hSrcProc:HANDLE; hMsgQ:HANDLE; lpOptions:LPMSGQUEUEOPTIONS):HANDLE; external KernelDLL name 'OpenMsgQueue'; // index 116
function ReadMsgQueue(hMsgQ:HANDLE;
                      lpBuffer:LPVOID;
                      cbBufferSize:DWORD;
                      lpNumberOfBytesRead:LPDWORD;
                      dwTimeout:DWORD;
                      pdwFlags:LPDWORD):BOOL; external KernelDLL name 'ReadMsgQueue'; // index 112
function WriteMsgQueue(hMsgQ:HANDLE;
                       lpBuffer:LPVOID;
                       cbDataSize:DWORD;
                       dwTimeout:DWORD;
                       dwFlags:DWORD):BOOL; external KernelDLL name 'WriteMsgQueue'; // index 113
function GetMsgQueueInfo(hMsgQ:HANDLE; lpInfo:LPMSGQUEUEINFO):BOOL; external KernelDLL name 'GetMsgQueueInfo'; // index 114
function CloseMsgQueue(hMsgQ:HANDLE):BOOL; external KernelDLL name 'CloseMsgQueue'; // index 115

implementation

end.