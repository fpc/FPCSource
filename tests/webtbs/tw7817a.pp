{ %norun }

{$mode macpas}

{$inline on}

unit tw7817a;

interface

type
        CFByteOrder = longint;
        UInt8Ptr = ^byte;
        UInt32 = cardinal;
const
        CFByteOrderUnknown = 0;
        CFByteOrderLittleEndian = 1;
        CFByteOrderBigEndian = 2;


function CFByteOrderGetCurrent: CFByteOrder; inline;

implementation

function CFByteOrderGetCurrent: CFByteOrder; inline;
        var
                x: UInt32 = (CFByteOrderBigEndian shl 24) or CFByteOrderLittleEndian;
begin
        CFByteOrderGetCurrent := CFByteOrder(UInt8Ptr(@x)^);
end;

end.

