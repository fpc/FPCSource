program Test;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$APPTYPE CONSOLE}

type
  TwbSignature = array[0..3] of AnsiChar;

  TwbConflictPriority = (
    cpIgnore,
    cpBenign,
    cpTranslate,
    cpNormal,
    cpCritical,
    cpFormID
  );

  IwbElement = interface
    ['{F4B4637D-C794-415F-B5C7-587EAA4095B3}']
  end;

  TwbDontShowCallback = function(const aElement: IwbElement): Boolean;

  IwbSubRecordDef = interface
    ['{D848E426-8768-45F4-B192-4DEFBE34D40A}']
  end;

  IwbByteArrayDef = interface
    ['{3069E1AC-4307-421B-93E4-797E18075EF9}']
  end;

function wbByteArray(const aName : string = 'Unknown';
                           aSize : Cardinal = 0;
                           aPriority : TwbConflictPriority = cpNormal;
                           aRequired : Boolean = False;
                           aDontShow : TwbDontShowCallback = nil)
                                     : IwbByteArrayDef; overload;
begin
  Result := nil;
end;

function wbByteArray(const aSignature : TwbSignature;
                     const aName : string = 'Unknown';
                           aSize : Cardinal = 0;
                           aPriority : TwbConflictPriority = cpNormal;
                           aRequired : Boolean = False;
                           aSizeMatch : Boolean = False;
                           aDontShow : TwbDontShowCallback = nil)
                                      : IwbSubRecordDef; overload;
begin
  Result := nil;
  halt(2);
end;

function wbUnknown(aPriority : TwbConflictPriority = cpNormal;
                   aRequired : Boolean = False;
                   aDontShow : TwbDontShowCallback = nil)
                             : IwbByteArrayDef;
begin
  Result := wbByteArray('Unknown', 0, aPriority, aRequired, aDontShow);
end;

function cb(const aElement: IwbElement): Boolean;
begin
  halt(1);
end;

begin
  wbUnknown(cpNormal,False,cb);
end. 
