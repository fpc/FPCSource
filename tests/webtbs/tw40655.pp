{ %NORUN }

program tw40655;

type

  MIDITimeStamp = UInt64;

{$push}
{$packrecords 4}
  MIDIPacket = record
	  timeStamp: MIDITimeStamp;
	  length: UInt16;
	  data: packed array [0..255] of Byte;
  end;
  MIDIPacketPtr = ^MIDIPacket;
{$pop}


{$push}
{$packrecords 4}
  MIDIPacketList = record
	  numPackets: UInt32;
	  packet: array [0..0] of MIDIPacket;
  end;
{$pop}

{$if SizeOf(MIDIPacket) <> 268}
{$message fatal 'Size of MIDIPacket is not 268'}
{$endif}

{$if SizeOf(MIDIPacketList) <> 272}
{$message fatal 'Size of MIDIPacketList is not 272'}
{$endif}

begin
end.
