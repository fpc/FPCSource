program SetSizeWrong;

{$IFDEF FPC}
  {$mode delphi}

  {$packenum 1}
  {$packset 1}
{$ENDIF}

type
  { the flags that are sent with every message }
  TnxMessageHeaderFlag = (
    {the message header is followed by a string}
    mhfErrorMessage,
    { reserver for future use }
    mhfReserved1,
    { reserver for future use }
    mhfReserved2,
    { reserver for future use }
    mhfReserved3,
    { reserver for future use }
    mhfReserved4,
    { reserver for future use }
    mhfReserved5,
    { reserver for future use }
    mhfReserved6,
    { reserver for future use }
    mhfReserved7
  );

  { set of Message flags }
  TnxMessageHeaderFlags = set of TnxMessageHeaderFlag;

begin
  if SizeOf(TnxMessageHeaderFlag)<>1 then
    halt(1);
  WriteLn(SizeOf(TnxMessageHeaderFlag)); // should be 1, is 1
  WriteLn(SizeOf(TnxMessageHeaderFlags)); // should be 1, is 4
  if SizeOf(TnxMessageHeaderFlags)<>1 then
    halt(1);
end.
