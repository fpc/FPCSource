    const
       maxsmallint = high(smallint);
       { error codes }
       grOk =  0;
       grNoInitGraph = -1;
       grNotDetected = -2;
       grFileNotFound = -3;
       grInvalidDriver = -4;
       grNoLoadMem = -5;
       grNoScanMem = -6;
       grNoFloodMem = -7;
       grFontNotFound = -8;
       grNoFontMem = -9;
       grInvalidMode = -10;
       grError = -11;
       grIOerror = -12;
       grInvalidFont = -13;
       grInvalidFontNum = -14;
       grInvalidVersion = -18;

function GraphErrorMsg(ErrorCode: smallint): string;
Begin
 GraphErrorMsg:='';
 case ErrorCode of
  grOk,grFileNotFound,grInvalidDriver: exit;
  grNoInitGraph: GraphErrorMsg:='Graphics driver not installed';
  grNotDetected: GraphErrorMsg:='Graphics hardware not detected';
  grNoLoadMem,grNoScanMem,grNoFloodMem: GraphErrorMsg := 'Not enough memory for graphics';
  grNoFontMem: GraphErrorMsg := 'Not enough memory to load font';
  grFontNotFound: GraphErrorMsg:= 'Font file not found';
  grInvalidMode: GraphErrorMsg := 'Invalid graphics mode';
  grError: GraphErrorMsg:='Graphics error';
  grIoError: GraphErrorMsg:='Graphics I/O error';
  grInvalidFont,grInvalidFontNum: GraphErrorMsg := 'Invalid font';
  grInvalidVersion: GraphErrorMsg:='Invalid driver version';
 end;
end;

begin
  if GraphErrorMsg(grNoInitGraph) <> 'Graphics driver not installed' then
    halt(1);
end.
