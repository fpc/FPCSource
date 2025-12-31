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
    { reserved for future use }
    mhfReserved1,
    { reserved for future use }
    mhfReserved2,
    { reserved for future use }
    mhfReserved3,
    { reserved for future use }
    mhfReserved4,
    { reserved for future use }
    mhfReserved5,
    { reserved for future use }
    mhfReserved6,
    { reserved for future use }
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
