(*
  $Id: sound.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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
var
  Snd: TTransferSound;
  SndDat: TTransferSoundData; //  =		{ (void *)0 , 0, 11025, 64, 64, 1 };

procedure playSound(ASound: pTransferSoundData);
procedure setGenericSound(rate: cuint32; vol: cuint8; pan: cuint8; format: cuint8);
procedure playGenericSound(data: pointer; length: cuint32);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure playSoundBlock(ASnd: PTransferSound);
begin
	DC_FlushRange(ASnd, sizeof(TTransferSound));
	IPC^.soundData := ASnd;
end;

procedure setGenericSound(rate: cuint32; vol: cuint8; pan: cuint8; format: cuint8);
begin
	SndDat.rate		:= rate;
	SndDat.vol		:= vol;
	SndDat.pan		:= pan;
	SndDat.format	:= format;
end;

procedure playSound(ASound: pTransferSoundData);
begin
	Snd.count := 1;
	Move(ASound^,  Snd.data[0], sizeof(TTransferSoundData) );
	playSoundBlock(@Snd);
end;

procedure playGenericSound(data: pointer; length: cuint32);
begin
	Snd.count := 1;
  move(SndDat, Snd.data, sizeof(TTransferSoundData) );
	Snd.data[0].data := data;
	Snd.data[0].len := length;
	playSoundBlock(@Snd);
end;


{$endif NDS_IMPLEMENTATION}
