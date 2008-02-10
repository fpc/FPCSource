unit tw10807;

interface
{$mode delphi}

uses
  Classes, SysUtils;

const maxword = 65535;

type
  PClrStreamHeader = ^TClrStreamHeader;
  TClrStreamHeader = packed record
    Name: array [0..MaxWord] of Char;
  end;
  TJclClrStream = class(TObject)
   constructor Create(const AMetadata: Tobject;      AHeader: PClrStreamHeader); virtual;
    end;
   TJclClrStreamClass = class of TJclClrStream;

  tobjectlist = class
    procedure add(c: tobject);
  end;

  tJclPeImage=class(tobject)
               end;
  TJclPeMetadata = class(TObject)
  private
    FStreams: TObjectList;
    constructor Create(const AImage: TJclPeImage);
  end;

implementation

procedure tobjectlist.add(c: tobject);
begin
end;

constructor TJclPeMetadata.Create(const AImage: TJclPeImage);

  function GetStreamClass(const Name: string): TJclClrStreamClass;
  begin
  end;

  procedure UpdateStreams;
  var
    pStream: PClrStreamHeader;
    I: Integer;
  begin
     FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));
  end;

begin
end;

constructor TJclClrStream.Create(const AMetadata: Tobject;      AHeader: PClrStreamHeader);
begin
end;

end.

