(*
  $Id: ipc.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifdef NDS_INTERFACE}
type
  sTransferSoundData = record
    data: pointer;//pcint;
    len: cuint32; 
    rate: cuint32; 
    vol: cuint8; 
    pan: cuint8; 
    format: cuint8; 
    PADDING: cuint8; 
  end;
  TransferSoundData = sTransferSoundData; 
  TTransferSoundData = TransferSoundData;
  PTransferSoundData = ^sTransferSoundData; 

  sTransferRegion = record
  	touchX,   touchY: cint16;
  	touchXpx, touchYpx: cint16;
  	touchZ1,  touchZ2: cint16;
 
  	buttons: cuint16;

    unixTime: cint32;
  end;
  TransferRegion = sTransferRegion; 
  TTransferRegion = TransferRegion;
  PTransferRegion = ^sTransferRegion; 


const
  //IPC : pTransferRegion = pointer($027FF000);

//---------------------------------------------------------------------------------
// Synchronization register
//---------------------------------------------------------------------------------
  REG_IPC_SYNC	: pcuint16 = pointer($04000180);

type
  IPC_SYNC_BITS = longint;
const
  IPC_SYNC_IRQ_ENABLE: IPC_SYNC_BITS =	(1 shl 14);
  IPC_SYNC_IRQ_REQUEST: IPC_SYNC_BITS	=	(1 shl 13);
  
  
function getIPC(): PTransferRegion; inline;
function IPC(): PTransferRegion; inline;
procedure IPC_SendSync(sync: cuint); inline; 
function IPC_GetSync(): cint; inline;

const
  REG_IPC_FIFO_TX		: pcuint32 = pointer($04000188);
  REG_IPC_FIFO_RX		: pcuint32 = pointer($04100000);
  REG_IPC_FIFO_CR		: pcuint16 = pointer($04000184);

type
  IPC_CONTROL_BITS = cint;

const
	IPC_FIFO_SEND_EMPTY  : IPC_CONTROL_BITS = (1 shl 0);
	IPC_FIFO_SEND_FULL   : IPC_CONTROL_BITS = (1 shl 1);
	IPC_FIFO_SEND_IRQ    : IPC_CONTROL_BITS = (1 shl 2);
	IPC_FIFO_SEND_CLEAR  : IPC_CONTROL_BITS = (1 shl 3);
	IPC_FIFO_RECV_EMPTY  : IPC_CONTROL_BITS	=	(1 shl 8);
	IPC_FIFO_RECV_FULL   : IPC_CONTROL_BITS = (1 shl 9);
	IPC_FIFO_RECV_IRQ    : IPC_CONTROL_BITS = (1 shl 10);
	IPC_FIFO_ERROR       : IPC_CONTROL_BITS = (1 shl 14);
	IPC_FIFO_ENABLE      : IPC_CONTROL_BITS = (1 shl 15);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function getIPC(): PTransferRegion; inline;
begin
	getIPC := PTransferRegion(pointer($027FF000));
end;

function IPC(): PTransferRegion; inline;
begin
	IPC := getIPC();
end;
  
procedure IPC_SendSync(sync: cuint); inline; 
begin
	REG_IPC_SYNC^ := (REG_IPC_SYNC^ and $f0ff) or (((sync) and $0f) shl 8) or IPC_SYNC_IRQ_REQUEST;
end;

function IPC_GetSync(): cint; inline;
begin
	IPC_GetSync := REG_IPC_SYNC^ and $0f;
end;
{$endif NDS_IMPLEMENTATION}
