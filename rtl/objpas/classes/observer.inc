{ TObservers }

function TObservers.CanObserve(const aID: Integer): Boolean;
begin
  Result:=Assigned(FCanObserve) and FCanObserve(aID);
end;

procedure TObservers.AddObserver(const aID: Integer; const aIntf: IInterface);

var
  I : integer;
  P : PIDArray;
  O : IObserver;
  E : IEditLinkObserver;

begin
  if not Supports(aIntf,IObserver,O) then
    raise EObserverException.Create(SErrNotIObserverInterface);
  if not CanObserve(aID) then
    raise EObserverException.Create(SErrUnsupportedObserver);
  P:=FList.GetIDArrayFromID(aId);
  if P=Nil then
    FList.AddInterface(aID,aIntf)
  else
    begin
    if not Supports(aIntf,ISingleCastObserver) then
      P^.Add(aIntf)
    else
      begin
      if Supports(aIntf,IEditLinkObserver,E) and Not E.IsReadOnly then
        begin
        // There can be only one editing link observer.
        For I:=0 to P^.Count-1 do
          if Supports(P^.List[I],IEditLinkObserver,E) then
            if not E.IsReadOnly then
              Raise EObserverException.Create(SErrOnlyOneEditingObserverAllowed)
        end;
      P^.Add(aIntf)
      end;
    end;
  if Assigned(FObserverAdded) then
   FObserverAdded(aId,O);
end;

procedure TObservers.AddObserver(const aIDs: array of Integer; const aIntf: IInterface);

var
  aID : integer;

begin
  for aID in aIDs do
    AddObserver(aId,aIntf);
end;

procedure TObservers.RemoveObserver(const aID: Integer; const aIntf: IInterface);

var
  P : PIDArray;

begin
  P:=FList.GetIDArrayFromID(aID);
  if P=Nil then
    exit;
  P^.Remove(aIntf);
end;

procedure TObservers.RemoveObserver(const aIDs: array of Integer; const aIntf: IInterface);

var
  aID : integer;

begin
  for aID in aIDs do
    RemoveObserver(aId,aIntf);
end;

function TObservers.IsObserving(const aID: Integer): Boolean;

var
  O : IInterface;

begin
  Result:=TryIsObserving(aID,O);
end;

function TObservers.TryIsObserving(const aID: Integer; out aIntf: IInterface): Boolean;

var
  P : PIDArray;

begin
  aIntf:=Nil;
  Result:=False;
  P:=FList.GetIDArrayFromID(aID);
  if P=Nil then
    exit;
  aIntf:=P^.GetActive;
  Result:=aIntf<>Nil;
end;

function TObservers.GetSingleCastObserver(const aID: Integer): IInterface;

var
  P : PIDArray;

begin
  Result:=Nil;
  P:=FList.GetIDArrayFromID(aID);
  if P<>Nil then
    Result:=P^.GetSingleCast;
  if Result=Nil then
    raise EObserverException.CreateFmt(SErrObserverNoSinglecast, [aID]);
end;

function TObservers.GetMultiCastObserverArray(const aID: Integer): TIInterfaceArray;

var
  aCount, I : Integer;
  P : PIDArray;
  O : IObserver;

begin
  Result:=[];
  P:=FList.GetIDArrayFromId(aId);
  if P=Nil then
    exit;
  SetLength(Result,P^.Count);
  aCount:=0;
  for I:=0 to P^.Count-1 do
    if Supports(P^.List[I],IMultiCastObserver,O) then
      if O.Active then
        begin
        Result[aCount]:=O;
        Inc(aCount);
        end;
  SetLength(Result,aCount);
  if aCount=0 then
    raise EObserverException.CreateFmt(SerrObserverNoMulticastFound, [aID]);
end;

function TObservers.GetMultiCastObserver(const aID: Integer): IInterfaceList;

Var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;

begin
  Result:=TInterfaceList.Create;
  IntfArray:=GetMultiCastObserverArray(aId);
  For Intf in IntfArray do
    Result.Add(Intf);
end;

{ TObservers.TIDArray }

procedure TObservers.TIDArray.Add(const aInterface: IInterface);
begin
  if Count=Length(List) then
    SetLength(List,Count+10);
  List[Count]:=aInterface;
  Inc(Count);
end;

procedure TObservers.TIDArray.Remove(const aInterface: IInterface);

var
  I : Integer;

begin
  I:=Count-1;
  While (I>=0) and (List[i]<>aInterface) do
    Dec(I);
  if (I>=0) then
    List[i]:=Nil;
end;

function TObservers.TIDArray.GetActive: IObserver;

var
  I : integer;

begin
  Result:=Nil;
  I:=Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    if Supports(List[I],IObserver,Result) then
      if Not Result.Active then
        Result:=nil;
    Dec(I);
    end;
end;

function TObservers.TIDArray.GetSingleCast: ISingleCastObserver;

var
  I : Integer;
  E : IEditLinkObserver;

begin
  Result:=Nil;
  I:=Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=nil;
    if Supports(List[I],ISingleCastObserver,Result) then
      begin
      if Not (Result.Active
              and Supports(Result,IEditLinkObserver,E)
              and not E.IsReadOnly) then
        Result:=nil;
      end;
    Dec(I);
    end;
end;

{ TObservers.TIDArrayList }

function TObservers.TIDArrayList.IndexOfID(aId: Integer): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (List[Result].ID<>aID) do
    Dec(Result);
end;

function TObservers.TIDArrayList.AddID(aId: Integer): Integer;
begin
  if Count=Length(List) then
    SetLength(List,Count+10);
  List[Count].ID:=aID;
  Result:=Count;
  Inc(Count);
end;

procedure TObservers.TIDArrayList.AddInterface(aID: integer;
  aInterFace: IInterface);

var
  Idx : Integer;
  P : PIDarray;

begin
  Idx:=AddID(aID);
  P:=GetIDArray(Idx);
  P^.Add(aInterface);
end;

function TObservers.TIDArrayList.GetIDArray(aIdx: Integer): PIDArray;
begin
  Result:=@List[aIdx];
end;

function TObservers.TIDArrayList.GetIDArrayFromID(aId: Integer): PIDArray;

var
  Idx : Integer;

begin
  Result:=Nil;
  Idx:=IndexOfID(aId);
  if Idx<>-1 then
    Result:=GetIDArray(Idx);
end;

{ TLinkObservers }

class function TLinkObservers.CheckObserving(const aObservers: TObservers; aID : Integer) : Integer;

begin
  Result:=aID;
  if Not aObservers.IsObserving(aID) then
    raise EObserverException.CreateFmt(SErrObserverNotAvailable,[aID]);
end;

class function TLinkObservers.GetEditGridLink(const aObservers: TObservers): IEditGridLinkObserver;

var
  aId: Integer;

begin
  aId:=CheckObserving(aObservers, TObserverMapping.EditGridLinkID);
  Result:=aObservers.GetSingleCastObserver(aID) as IEditGridLinkObserver;
end;

class function TLinkObservers.GetEditLink(const aObservers: TObservers): IEditLinkObserver;

var
  aId: Integer;

begin
  aId:=CheckObserving(aObservers,TObserverMapping.EditLinkID);
  Result:=aObservers.GetSingleCastObserver(aID) as IEditLinkObserver;
end;

class procedure TLinkObservers.EditLinkUpdate(const aObservers: TObservers);
begin
  GetEditLink(AObservers).Update;
end;

class function TLinkObservers.EditLinkTrackUpdate(const aObservers: TObservers): Boolean;

var
  E : IEditLinkObserver;
  T : IObserverTrack;

begin
  Result:=False;
  E:=GetEditLink(aObservers);
  if Supports(E,IObserverTrack,T) then
    if T.Track then
      begin
      Result:=true;
      E.Update;
      end;
end;

class procedure TLinkObservers.EditLinkReset(const aObservers: TObservers);
begin
  GetEditLink(AObservers).Reset;
end;

class procedure TLinkObservers.EditLinkModified(aObservers: TObservers);
begin
  GetEditLink(aObservers).Modified;
end;

class function TLinkObservers.EditLinkIsModified(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditLink(aObservers).IsModified;
end;

class function TLinkObservers.EditLinkIsValidChar(const aObservers: TObservers; aKey: Char): Boolean;
begin
  Result:=GetEditLink(aObservers).IsValidChar(aKey);
end;

class function TLinkObservers.EditLinkIsEditing(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditLink(aObservers).IsEditing;
end;

class function TLinkObservers.EditLinkEdit(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditLink(aObservers).Edit;
end;

class procedure TLinkObservers.EditLinkSetIsReadOnly(const aObservers: TObservers; AValue: Boolean);
begin
  GetEditLink(aObservers).IsReadOnly:=aValue;
end;

class function TLinkObservers.EditLinkIsReadOnly(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditLink(aObservers).IsReadOnly;
end;

class procedure TLinkObservers.EditGridLinkUpdate(const aObservers: TObservers);
begin
  GetEditGridLink(aObservers).Update;
end;

class procedure TLinkObservers.EditGridLinkReset(const aObservers: TObservers);
begin
  GetEditGridLink(aObservers).Reset;
end;

class procedure TLinkObservers.EditGridLinkModified(const aObservers: TObservers
  );
begin
  GetEditGridLink(aObservers).Modified;
end;

class function TLinkObservers.EditGridLinkIsModified(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditGridLink(aObservers).IsModified;
end;

class function TLinkObservers.EditGridLinkIsValidChar(const aObservers: TObservers; aKey: Char): Boolean;
begin
  Result:=GetEditGridLink(aObservers).IsValidChar(aKey);
end;

class function TLinkObservers.EditGridLinkIsEditing(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditGridLink(aObservers).IsEditing
end;

class function TLinkObservers.EditGridLinkEdit(const aObservers: TObservers): Boolean;
begin
  Result:=GetEditGridLink(aObservers).Edit;
end;

class function TLinkObservers.EditGridLinkIsReadOnly(
  const aObservers: TObservers): Boolean;
begin
  Result:=GetEditGridLink(aObservers).IsReadOnly;
end;

class procedure TLinkObservers.EditGridLinkSetIsReadOnly(const aObservers: TObservers; aValue: Boolean);

begin
  GetEditGridLink(aObservers).IsReadOnly:=aValue
end;

class procedure TLinkObservers.PositionLinkPosChanged(const aObservers: TObservers);

var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;
  PL: IPositionLinkObserver;
  PL170: IPositionLinkObserver170;

begin
  if Not aObservers.IsObserving(TObserverMapping.PositionLinkID) then
    Exit;
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.PositionLinkID);
  for Intf in IntfArray do
    begin
    if Supports(Intf,IPositionLinkObserver,PL) then
      PL.PosChanged;
    if Supports(Intf,IPositionLinkObserver170,PL170) then
      PL.PosChanged;
    end;
end;

class procedure TLinkObservers.PositionLinkPosChanging(const aObservers: TObservers);
var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;
  PL: IPositionLinkObserver;
  PL170: IPositionLinkObserver170;

begin
  if Not aObservers.IsObserving(TObserverMapping.PositionLinkID) then
    Exit;
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.PositionLinkID);
  for Intf in IntfArray do
    begin
    if Supports(Intf,IPositionLinkObserver,PL) then
      PL.PosChanging;
    if Supports(Intf,IPositionLinkObserver170,PL170) then
      PL.PosChanging;
    end;
end;

class procedure TLinkObservers.ListSelectionChanged(const aObservers: TObservers);

begin
  if AObservers.IsObserving(TObserverMapping.EditLinkID) then
    if not TLinkObservers.EditLinkIsEditing(aObservers) then
      EditLinkReset(AObservers)
    else
      begin
      EditLinkModified(aObservers);
      EditLinkTrackUpdate(aObservers);
      PositionLinkPosChanged(aObservers);
      end;
  if aObservers.IsObserving(TObserverMapping.ControlValueID) then
    begin
    ControlValueModified(aObservers);
    ControlValueTrackUpdate(aObservers);
    end;
  if aObservers.IsObserving(TObserverMapping.PositionLinkID) then
    PositionLinkPosChanged(aObservers);
end;

class procedure TLinkObservers.ControlValueUpdate(aObservers: TObservers);

var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;
  O: IControlValueObserver;

begin
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.ControlValueID);
  for Intf in IntfArray do
    if Supports(Intf,IControlValueObserver,O) then
      O.ValueUpdate;
end;

class procedure TLinkObservers.ControlValueModified(aObservers: TObservers);

var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;
  O: IControlValueObserver;

begin
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.ControlValueID);
  for Intf in IntfArray do
    if Supports(Intf,IControlValueObserver,O) then
      O.ValueModified;
end;

class function TLinkObservers.ControlValueTrackUpdate(const aObservers: TObservers): Boolean;
var
  IntfArray : TIInterfaceArray;
  Intf : IInterface;
  O: IControlValueObserver;
  T : IObserverTrack;

begin
  Result:=False;
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.ControlValueID);
  for Intf in IntfArray do
    if Supports(Intf,IControlValueObserver,O)
       And Supports(O,IObserverTrack,T) then
         if T.Track then
           begin
           Result:=true;
           O.ValueUpdate;
           end;

end;

class function TLinkObservers.AllowControlChange(const aObservers: TObservers): Boolean;

begin
  if aObservers.IsObserving(TObserverMapping.EditLinkID) then
    Result:=TLinkObservers.EditLinkEdit(aObservers)
  else
    Result := True;
end;

class procedure TLinkObservers.ControlChanged(const aObservers: TObservers);

begin
  if (aObservers.IsObserving(TObserverMapping.EditLinkID))
     and EditLinkEdit(aObservers) then
    begin
    EditLinkModified(aObservers);
    EditLinkUpdate(aObservers);
    end;
  if aObservers.IsObserving(TObserverMapping.ControlValueID) then
    begin
    ControlValueModified(aObservers);
    ControlValueUpdate(aObservers);
    end;
  if aObservers.IsObserving(TObserverMapping.PositionLinkID) then
    PositionLinkPosChanged(aObservers);
end;

class function TLinkObservers.AllowControlChange(const aControl: TComponent): Boolean;
begin
  AllowControlChange(aControl.Observers);
end;

class procedure TLinkObservers.ControlChanged(const aControl: TComponent);
begin
  ControlChanged(aControl.Observers);
end;

class procedure TLinkObservers.IteratorLinkUpdateControlComponent(const aObservers: TObservers; aControl: TComponent);
var
  O : IIteratorLinkObserver;
  IntfArray : TIInterfaceArray;
  Intf : IInterface;

begin
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.IteratorLinkID);
  for Intf in IntfArray do
    if Supports(Intf,IIteratorLinkObserver,O) then
      O.UpdateControlComponent(aControl);
end;

class procedure TLinkObservers.IteratorLinkStartFrom(const aObservers: TObservers; aPosition: Integer);

var
  O : IIteratorLinkObserver;
  IntfArray : TIInterfaceArray;
  Intf : IInterface;

begin
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.IteratorLinkID);
  for Intf in IntfArray do
    if Supports(Intf,IIteratorLinkObserver,O) then
      O.StartFrom(aPosition);
end;

class function TLinkObservers.IteratorLinkMoveNext(const aObservers: TObservers): Boolean;

var
  O : IIteratorLinkObserver;
  IntfArray : TIInterfaceArray;
  Intf : IInterface;

begin
  Result:=false;
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.IteratorLinkID);
  for Intf in IntfArray do
    if Supports(Intf,IIteratorLinkObserver,O) then
      Result:=Result or O.MoveNext;
end;

class procedure TLinkObservers.IteratorLinkFinish(const aObservers: TObservers);

var
  O : IIteratorLinkObserver;
  IntfArray : TIInterfaceArray;
  Intf : IInterface;

begin
  IntfArray:=aObservers.GetMultiCastObserverArray(TObserverMapping.IteratorLinkID);
  for Intf in IntfArray do
    if Supports(Intf,IIteratorLinkObserver,O) then
      O.Finish;
end;

{ TObserverMapping }


constructor TObserverMapping.Create;
begin
  FList:=TStringList.Create;
  // Don't use sorted, as it will change the IDs as more records are added
end;

destructor TObserverMapping.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

class constructor TObserverMapping.Init;
begin
  _Instance:=TObserverMapping.Create;
end;

class destructor TObserverMapping.Done;
begin
  FreeAndNil(_Instance)
end;

class function TObserverMapping.GetObserverID(const aKey: string): Integer;
begin
  Result:=Instance.List.Indexof(aKey);
  if Result=-1 then
    Result:=Instance.List.Add(aKey);
  Result:=Result+MinPublicID;
end;

class procedure TObserverMapping.Clear;
begin
  Instance.List.Clear;
end;

constructor ObservableMemberAttribute.Create(const aMemberName, aFramework: string; aTrack: Boolean);
begin
  inherited Create;
  FFramework := AFramework;
  FMemberName := AMemberName;
  FTrack := ATrack;
end;

constructor ObservableMemberAttribute.Create(const aMemberName: string; aTrack: Boolean);
begin
  inherited Create;
  FMemberName := AMemberName;
  FTrack := ATrack;
end;

constructor ObservableMemberAttribute.Create(const aMemberName: string);
begin
  inherited Create;
  FMemberName := AMemberName;
end;


