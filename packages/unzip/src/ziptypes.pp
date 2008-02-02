{
}
UNIT ziptypes;
{
Type definitions for UNZIP
  * original version by Christian Ghisler
  * extended
    and
    amended for Win32 by Dr Abimbola Olowofoyeku (The African Chief)
 Homepage: http://ourworld.compuserve.com/homepages/African_Chief
  * extended by Tomas Hajny, XHajT03@mbox.vol.cz to support other 32-bit
    compilers/platforms (OS/2, GO32, ...); search for (* TH ... *)
}

{$IFDEF FPC}
 {$DEFINE BIT32}
{$ENDIF}

{$IFDEF OS2}
 {$DEFINE BIT32}
{$ENDIF}

{$IFDEF WIN32}
 {$DEFINE BIT32}
{$ENDIF}


INTERFACE

{$ifdef BIT32}
TYPE
  nWord   = longint;
  Integer = Longint; {Default Integer is 16 bit!}
{$else BIT32}
TYPE
  nWord = Word;
{$endif BIT32}

CONST
  tBufSize = {$ifdef BIT32}256{$else}63{$endif} * 1024;   {buffer size}
  tFSize   = {$ifdef BIT32}259{$else}79{$endif};          {filename length}

{$IFDEF OS2}
  AllFiles = '*';
{$ELSE}
  {$ifdef unix}
  AllFiles = '*';
  {$else}
  AllFiles = '*.*';
  {$endif}
{$ENDIF}

{$ifdef unix}
  DirSep='/';
{$else}
  DirSep='\';
{$endif}

TYPE
  { Record for UNZIP }
  buftype  = ARRAY [ 0..tBufSize ] of char;
  TDirtype = ARRAY [ 0..tFSize ] of char;
  TZipRec = PACKED RECORD
       buf : ^buftype;        {please}         {buffer containing central dir}
       bufsize,               {do not}         {size of buffer}
       localstart : word;     {change these!}  {start pos in buffer}
       Time,
       Size,
       CompressSize,
       headeroffset : Longint;
       FileName : tdirtype;
       PackMethod : word;
       Attr : Byte;
  END; { TZipRec }

  { record for callback progress Reports, etc. }
  pReportRec = ^TReportRec;     {passed to callback functions}
  TReportRec = PACKED RECORD
       FileName : tdirtype;   {name of individual file}
       Time,                  {date and time stamp of individual file}
       Size,                  {uncompressed and time stamp of individual file}
       CompressSize : Longint;{compressed and time stamp of individual file}
       Attr : integer;        {file attribute of individual file}
       PackMethod : Word;     {compression method of individual file}
       Ratio : byte;          {compression ratio of individual file}
       Status : longint;      {callback status code to show where we are}
       IsaDir : Boolean;      {is this file a directory?}
  END; {TReportRec}

{ callback status codes }
CONST
  file_starting    = -1000;  {beginning the unzip process; file}
  file_unzipping   = -1001;  {continuing the unzip process; file}
  file_completed   = -1002;  {completed the unzip process; file}
  file_Failure     = -1003;  {failure in unzipping file}
  unzip_starting   = -1004;  {starting with a new ZIP file}
  unzip_completed  = -1005;  {completed this ZIP file}


{ procedural types for callbacks }
TYPE
  UnzipReportProc  = PROCEDURE ( Retcode : longint;Rec : pReportRec );{$ifdef Delphi32}STDCALL;{$endif}
{ procedural type for "Report" callback: the callback function
  (if any) is called several times during the unzip process

  Error codes are sent to the callback in "Retcode". Other
  details are sent in the record pointed to by "Rec".
  * Note particularly Rec^.Status - this contains information about
  the current status or stage of the unzip process. It can have
  any of the following values;
  (archive status)
    unzip_starting   = starting with a new ZIP archive (rec^.filename)
    unzip_completed  = finished with the ZIP archive (rec^.filename)

  (file status)
    file_starting    = starting to unzip (extract) a file (from archive)
    file_unzipping   = continuing to unzip a file (from archive)
        (when this status value is reported, the actual number of
         bytes written to the file are reported in "Retcode"; this is
         valuable for updating any progress bar)

    file_completed   = finshed  unzip a file (from archive)
    file_Failure     = could not extract the file (from archive)
}

UnzipQuestionProc = FUNCTION ( Rec : pReportRec ) : Boolean;
{$ifdef Delphi32}STDCALL;{$endif}
{ procedural type for "Question" callback:if a file already
  exists, the callback (if any) will be called to ask whether
  the file should be overwritten by the one in the ZIP file;

  the details of the file in the ZIP archive are supplied in the
  record pointed to by "Rec"

 in your callback function, you should;
   return TRUE  if you want the existing file to be overwritten
   return FALSE is you want the existing file to be skipped
}


{Error codes returned by the main unzip functions}
CONST
  unzip_Ok             =  0;
  unzip_CRCErr         = -1;
  unzip_WriteErr       = -2;
  unzip_ReadErr        = -3;
  unzip_ZipFileErr     = -4;
  unzip_UserAbort      = -5;
  unzip_NotSupported   = -6;
  unzip_Encrypted      = -7;
  unzip_InUse          = -8;
  unzip_InternalError  = -9;    {Error in zip format}
  unzip_NoMoreItems    = -10;
  unzip_FileError      = -11;   {Error Accessing file}
  unzip_NotZipfile     = -12;   {not a zip file}
  unzip_SeriousError   = -100;  {serious error}
  unzip_MissingParameter = -500; {missing parameter}


{ the various unzip methods }
CONST
Unzipmethods : ARRAY [ 0..9 ] of pchar =
  ( 'stored', 'shrunk', 'reduced 1', 'reduced 2', 'reduced 3',
   'reduced 4', 'imploded', 'tokenized', 'deflated', 'skipped' );

{ unzip actions being undertaken }
CONST
UnzipActions : ARRAY [ 0..9 ] of pchar =
  ( 'copying', 'unshrinking', 'unreducing 1', 'unreducing 2', 'unreducing 3',
   'unreducing 4', 'exploding', 'un-tokenizing', 'inflating', 'skipping' );

{ rudimentary "uppercase" function }
FUNCTION Upper ( s : String ) : String;

{ remove path and return filename only }
FUNCTION StripPath ( CONST s : String ) : String;

IMPLEMENTATION

FUNCTION Upper ( s : String ) : String;
VAR i : integer;
BEGIN
   FOR i := 1 TO length ( s ) DO s [ i ] := Upcase ( s [ i ] );
   Upper := s;
END;

FUNCTION StripPath ( CONST s : String ) : String;
VAR
i, j : Word;
BEGIN
   StripPath := s;
   j := length ( s );
   FOR i := j DOWNTO 1 DO BEGIN
       IF s [ i ] in [ '\', ':', '/' ] THEN BEGIN
          StripPath := Copy ( s, succ ( i ), j -i );
          exit;
       END;
   END;
END;

END.
