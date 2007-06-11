{$mode macpas}

program FatalError_2006082312;

type
  UInt16 = Word;
  UInt64 = QWord;

  CoreMidiPacket = packed record
    timeStamp: UInt64;
    length: UInt16;
    data: packed array [0..255] of byte
  end;

procedure test(var gCoreMidiPacket: CoreMidiPacket);
begin
  with gCoreMidiPacket do
    begin
      timeStamp := high(int64);
      length := $2345;
    end
end;

var
  gcmp: CoreMidiPacket;
begin
  test(gcmp);
  if (gcmp.timestamp <> high(int64)) or
     (gcmp.length<>$2345) then
    halt(1);
end.
