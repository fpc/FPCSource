type
  MBHelpPtr = pointer;
  MenuRecord = record end;
  MenuItemsPtr = pointer;
  MenuIconHandle = pointer;
  MenuRef = pointer;
  int16 = word;
  int32 = longint;
  OSStatus = int32;
  Str255 = ShortString;

 function MacMenuAddItemInternal
        ( var theMenuRecord           : MenuRecord;
              theMenuOrSubMenuID      : Int32;
              theOptBeforeItemIndex   : Int32;
          var theMenuRef              : MenuRef;
          var theItemsPtr             : MenuItemsPtr;
        const theItemStr              : Str255;
              theItemIconHandle       : MenuIconHandle;
              theEnableFlag           : boolean;
              theCheckFlag            : boolean;
              theCommandChar          : char;
              theCommandModifiers     : Int16;
              theItemCmdID            : Int32;
        const theItemAppStr           : AnsiString;
          var theNewItemIndex         : Int32): OSStatus;
begin
end;

 function MBMenuAddItemInternal
        ( var theMenuRecord           : MenuRecord;
              theMenuOrSubMenuID      : Int32;
              theOptBeforeItemIndex   : Int32;
          var theMenuRef              : MenuRef;
          var theItemsPtr             : MenuItemsPtr;
        const theItemStr              : Str255;
              theItemIconHandle       : MenuIconHandle;
              theEnableFlag           : boolean;
              theCheckFlag            : boolean;
              theCommandChar          : char;
              theCommandGlyph         : Int16; { unused here }
              theCommandModifiers     : Int16;
              theItemCmdID            : Int32;
        const theItemAppStr           : AnsiString;
              theItemHelpPtr          : MBHelpPtr; { unused here }
          var theNewItemIndex         : Int32): OSStatus;
      var
        theErr                        : OSStatus;
    begin
      theItemsPtr                     := nil;
      theNewItemIndex                 := 0;
      theErr                          := MacMenuAddItemInternal
        ( theMenuRecord, theMenuOrSubMenuID, theOptBeforeItemIndex, theMenuRef, theItemsPtr,
          theItemStr, theItemIconHandle, theEnableFlag, theCheckFlag, theCommandChar,
          theCommandModifiers, theItemCmdID, theItemAppStr, theNewItemIndex);
      MBMenuAddItemInternal           := theErr
    end;

var
  theMenuRecord: MenuRecord;
  theMenuRef: MenuRef;
  theItemsPtr: MenuItemsPtr;
  theNewItemIndex: Int32;
begin
  MBMenuAddItemInternal(theMenuRecord,1,2,theMenuRef,theItemsPtr,'abc',nil,true,false,'b',3,4,5,'def',nil,theNewItemIndex);
end.
