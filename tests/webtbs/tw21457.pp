unit tw21457;
{$mode objfpc}
interface
uses Classes;

Type
  TFileStreamHelper = class helper for TFileStream
  public
    constructor CreateRetry(const AFileName: string; Mode: Word; Rights: Cardinal);
  end;


implementation

{ TFileStreamHelper }

constructor TFileStreamHelper.CreateRetry(const AFileName:string; Mode:Word; Rights: Cardinal);
begin
  //TODO
  //=> internal error 200305103
end;


end.
