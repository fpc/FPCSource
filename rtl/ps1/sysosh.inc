type
  THandle = DWord;
  TThreadID = THandle;
  TOSTimestamp = LongInt;

  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = record
    Locked: Boolean;
  end;
