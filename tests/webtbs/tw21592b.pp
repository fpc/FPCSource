unit tw21592b;

{$MODE DELPHI}

interface

type TBytesOverlay<T> = array [0..SizeOf(T) - 1] of Byte;

implementation

end.
