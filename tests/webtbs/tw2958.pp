{ Source provided for Free Pascal Bug Report 2958 }
{ Submitted by "Marco ( Gory Bugs Department)" on  2004-02-08 }
{ e-mail:  }

{$mode Delphi}
Uses Classes;

type

  TIdHash = class(TObject);

  TIdHash32 = class(TIdHash)
  public
    function HashValue(const ASrc: string): LongWord; overload;
    function HashValue(AStream: TStream): LongWord; overload; virtual; abstract;
  end;

  TIdHashCRC32 = class( TIdHash32 )
  public
    function HashValue( AStream: TStream ) : LongWord; override;
    function HashValue( AStream: TStream; const ABeginPos: Cardinal{=0}; const AEndPos : Cardinal{=0} ) : LongWord; overload;
  end;


function tidhash32.hashvalue(const ASrc:string):longword;

begin
end;

function tidhashCRC32.hashvalue(AStream:TStream):longword;

begin
end;

function tidhashcrc32.hashvalue(AStream:TStream;const ABeginPos: Cardinal{=0}; const AEndPos : Cardinal{=0} ) : LongWord;

begin
end;

begin
end.
