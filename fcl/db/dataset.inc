{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by Michael Van Canneyt, member of the 
    Free Pascal development team

    Dataset implementation
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
    TDataSet
  ---------------------------------------------------------------------}

Const
  DefaultBufferCount = 10;

constructor TDataSet.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FFieldDefs:=TFieldDefs.Create(Self);
  FFieldList:=TFields.Create(Self);
end;



destructor TDataSet.Destroy;

begin
  Active:=False;
  FFieldDefs.Free;
  FFieldList.Free;
  Inherited Destroy;
end;


procedure TDataset.ActivateBuffers; 

begin
  FBOF:=False;
  FEOF:=False;
  FRecordCount:=1;
  FActiveRecord:=0;
end;

procedure TDataset.UpdateFieldDefs; 

begin
  //!! To be implemented
end;

procedure TDataset.BindFields(Binding: Boolean);

Var I : longint;

begin
  {
     Here some magic will be needed later; for now just simply set
     Just set fieldno from listindex...
     Later we should take it from the fielddefs.
  }   
  For I:=0 to FFieldList.Count-1 do
    FFieldList[i].FFieldNo:=I;
end;

function TDataset.BookmarkAvailable: Boolean;

Const BookmarkStates = [dsBrowse,dsEdit,dsInsert];

begin
  Result:=(Not IsEmpty) and (State in BookmarkStates) 
          and (getBookMarkFlag(ActiveBuffer)=bfCurrent);
end;

procedure TDataset.CalculateFields(Buffer: PChar); 

begin
  //!! To be implemented
end;

procedure TDataset.CheckActive; 

begin
  If Not Active then
    DataBaseError(SInactiveDataset);
end;

procedure TDataset.CheckInactive; 

begin
  If Active then
    DataBaseError(SActiveDataset);
end;

procedure TDataset.ClearBuffers; 

begin
  FRecordCount:=0;
  FactiveRecord:=0;
  FCurrentRecord:=-1;
  FBOF:=True;
  FEOF:=True;
end;

procedure TDataset.ClearCalcFields(Buffer: PChar); 

begin
  //!! To be implemented
end;

procedure TDataset.CloseBlob(Field: TField); 

begin
  //!! To be implemented
end;

procedure TDataset.CloseCursor; 

begin
  //!! To be implemented
end;

procedure TDataset.CreateFields;

Var I : longint;

begin
{$ifdef DSDebug}
  Writeln ('Creating fields');
{$endif}
  For I:=0 to fielddefs.Count-1 do
    With Fielddefs.Items[I] do
      If DataType<>ftUnknown then
        CreateField(self);
end;

procedure TDataset.DataEvent(Event: TDataEvent; Info: Longint); 

begin
  //!! To be implemented
end;

procedure TDataset.DestroyFields; 

begin
  FFieldList.Clear;
end;

procedure TDataset.DoAfterCancel; 

begin
 If assigned(FAfterCancel) then 
   FAfterCancel(Self);
end;

procedure TDataset.DoAfterClose; 

begin
 If assigned(FAfterClose) then 
   FAfterClose(Self);
end;

procedure TDataset.DoAfterDelete; 

begin
 If assigned(FAfterDelete) then 
   FAfterDelete(Self);
end;

procedure TDataset.DoAfterEdit; 

begin
 If assigned(FAfterEdit) then 
   FAfterEdit(Self);
end;

procedure TDataset.DoAfterInsert; 

begin
 If assigned(FAfterInsert) then 
   FAfterInsert(Self);
end;

procedure TDataset.DoAfterOpen; 

begin
 If assigned(FAfterOpen) then 
   FAfterOpen(Self);
end;

procedure TDataset.DoAfterPost; 

begin
 If assigned(FAfterPost) then 
   FAfterPost(Self);
end;

procedure TDataset.DoAfterScroll; 

begin
 If assigned(FAfterScroll) then 
   FAfterScroll(Self);
end;

procedure TDataset.DoBeforeCancel; 

begin
 If assigned(FBeforeCancel) then 
   FBeforeCancel(Self);
end;

procedure TDataset.DoBeforeClose; 

begin
 If assigned(FBeforeClose) then 
   FBeforeClose(Self);
end;

procedure TDataset.DoBeforeDelete; 

begin
 If assigned(FBeforeDelete) then 
   FBeforeDelete(Self);
end;

procedure TDataset.DoBeforeEdit; 

begin
 If assigned(FBeforeEdit) then 
   FBeforeEdit(Self);
end;

procedure TDataset.DoBeforeInsert; 

begin
 If assigned(FBeforeInsert) then 
   FBeforeInsert(Self);
end;

procedure TDataset.DoBeforeOpen; 

begin
 If assigned(FBeforeOpen) then 
   FBeforeOpen(Self);
end;

procedure TDataset.DoBeforePost; 

begin
 If assigned(FBeforePost) then 
   FBeforePost(Self);
end;

procedure TDataset.DoBeforeScroll; 

begin
 If assigned(FBeforeScroll) then 
   FBeforeScroll(Self);
end;

Procedure TDataset.DoInternalOpen;

begin
  FBufferCount:=0;
  FDefaultFields:=FieldCount=0;
  DoBeforeOpen;
  Try
    InternalOpen;
    FBOF:=True;
    SetState(dsBrowse);
    SetBufListSize(DefaultBufferCount);
    GetNextRecords;
    DoAfterOpen;
    DoAfterScroll;
  except 
    SetState(dsInactive);   
    DoInternalClose;
    raise;
  end;
end;

Function TDataset.RequiredBuffers : longint;
{
  If later some datasource requires more buffers (grids etc)
  then it should be taken into account here...
} 

begin
  Result:=0;
end;

Procedure TDataset.DoInternalClose;

begin
  FreeFieldBuffers;
  ClearBuffers;
  SetState(dsInactive);
  InternalClose;
end;

procedure TDataset.DoOnCalcFields; 

begin
 If assigned(FOnCalcfields) then 
   FOnCalcFields(Self);
end;

procedure TDataset.DoOnNewRecord; 

begin
 If assigned(FOnNewRecord) then 
   FOnNewRecord(Self);
end;

function TDataset.FieldByNumber(FieldNo: Longint): TField;

begin
  Result:=FFieldList.FieldByNumber(FieldNo);
end;

function TDataset.FindRecord(Restart, GoForward: Boolean): Boolean; 

begin
  //!! To be implemented
end;

procedure TDataset.FreeFieldBuffers; 

Var I : longint;

begin
  For I:=0 to FFieldList.Count-1 do
    FFieldList[i].FreeBuffers;
end;

function TDataset.GetBookmarkStr: TBookmarkStr; 

begin
  Result:='';
  If BookMarkAvailable then
    begin
    SetLength(Result,FBookMarkSize);
    GetBookMarkData(ActiveBuffer,Pointer(Result));
    end
end;

Function TDataset.GetBuffer (Index : longint) : Pchar;  

begin
  Result:=FBuffers[Index];
end;

procedure TDataset.GetCalcFields(Buffer: PChar); 

begin
  //!! To be implemented
end;

function TDataset.GetCanModify: Boolean; 

begin
  //!! To be implemented
end;

procedure TDataset.GetChildren(Proc: TGetChildProc; Root: TComponent); 

begin
  //!! To be implemented
end;

Function TDataset.GetField (Index : Longint) : TField;

begin
  Result:=FFIeldList[index];
end;

{
  This is not yet allowed, FPC doesn't allow typed consts of Classes...

Const 
  DefFieldClasses : Array [TFieldType] of TFieldClass =
    ( { ftUnknown} Tfield,
      { ftString} TStringField,
      { ftSmallint} TLongIntField,
      { ftInteger} TLongintField,
      { ftWord} TLongintField,
      { ftBoolean} TBooleanField,
      { ftFloat} TFloatField,
      { ftDate} TDateField,
      { ftTime} TTimeField,
      { ftDateTime} TDateTimeField,
      { ftBytes} TBytesField,
      { ftVarBytes} TVarBytesField,
      { ftAutoInc} TAutoIncField,
      { ftBlob} TBlobField,
      { ftMemo} TMemoField,
      { ftGraphic} TGraphicField,
      { ftFmtMemo} TMemoField,
      { ftParadoxOle} Nil,
      { ftDBaseOle} Nil,
      { ftTypedBinary} Nil,
      { ftCursor} Nil
    );
}

function TDataset.GetFieldClass(FieldType: TFieldType): TFieldClass; 

begin
  Case FieldType of 
     ftUnknown : Result:=Tfield;
     ftString: Result := TStringField;
     ftSmallint: Result := TLongIntField;
     ftInteger: Result := TLongintField;
     ftWord: Result := TLongintField;
     ftBoolean: Result := TBooleanField;
     ftFloat: Result := TFloatField;
     ftDate: Result := TDateField;
     ftTime: Result := TTimeField;
     ftDateTime: Result := TDateTimeField;
     ftBytes: Result := TBytesField;
     ftVarBytes: Result := TVarBytesField;
     ftAutoInc: Result := TAutoIncField;
     ftBlob: Result := TBlobField;
     ftMemo: Result := TMemoField;
     ftGraphic: Result := TGraphicField;
     ftFmtMemo: Result := TMemoField;
     ftParadoxOle: Result := Nil;
     ftDBaseOle: Result := Nil;
     ftTypedBinary: Result := Nil;
     ftCursor: Result := Nil;
  end;
end;

function TDataset.GetIsIndexField(Field: TField): Boolean; 

begin
  //!! To be implemented
end;

function TDataset.GetNextRecord: Boolean; 

Var Shifted : Boolean;

begin
{$ifdef dsdebug}
  Writeln ('Getting next record. Internal RecordCount : ',FRecordCount);
{$endif}  
  Shifted:=FRecordCount=FBufferCount;
  If Shifted then
    begin
    ShiftBuffers(1);
    Dec(FRecordCount);
    end;
{$ifdef dsdebug}
  Writeln ('Getting data into buffer : ',FRecordCount);
{$endif}  
  Result:=GetRecord(FBuffers[FRecordCount],gmNext,True)=grOK;
  If Result then
    begin
    If FRecordCount=0 then 
      ActivateBuffers
    else
      If FRecordCount<FBufferCount then
        Inc(FRecordCount);
    FCurrentRecord:=FRecordCount;  
    end
  else
    begin
    if shifted then
      begin
      ShiftBuffers(-1);
      inc(FRecordCount);
      end;
    CursorPosChanged;
    end;
{$ifdef dsdebug}
  Writeln ('Result getting next record : ',Result);  
{$endif}  
end;

function TDataset.GetNextRecords: Longint; 

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting next record(s), need :',FBufferCount);
{$endif}  
  While (FRecordCount<FBufferCount) and GetNextRecord do
    Inc(Result);
{$ifdef dsdebug}
  Writeln ('Result Getting next record(s), GOT :',RESULT);
{$endif}  
end;

function TDataset.GetPriorRecord: Boolean; 

Var Shifted : boolean;

begin
{$ifdef dsdebug}
  Writeln ('Getting previous record');
{$endif}  
  Shifted:=FRecordCount>0;
  If Shifted Then 
    begin
    SetCurrentRecord(0);
    ShiftBuffers(-1);
    end;
  Result:=GetRecord(FBuffers[0],gmPrior,True)=grOK;
  If Result then
    begin
    If FRecordCount=0 then 
      ActivateBuffers
    else
      begin
      If FrecordCount<FBufferCount then
        Inc(FRecordCount);
      end;
    FCurrentRecord:=0;    
    end
  else
    begin
    If Shifted then 
      begin
      ShiftBuffers(1);
      end;
    CursorPosChanged;
    end;
end;

function TDataset.GetPriorRecords: Longint; 

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting previous record(s), need :',FBufferCount);
{$endif}  
  While (FRecordCount<FbufferCount) and GetPriorRecord do
    Inc(Result);
end;

function TDataset.GetRecNo: Longint; 

begin
  //!! To be implemented
end;

function TDataset.GetRecordCount: Longint; 

begin
  //!! To be implemented
end;

procedure TDataset.InitFieldDefs; 

begin
  //!! To be implemented
end;

procedure TDataset.InitRecord(Buffer: PChar); 

begin
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
end;

procedure TDataset.InternalCancel; 

begin
  //!! To be implemented
end;

procedure TDataset.InternalEdit; 

begin
  //!! To be implemented
end;

procedure TDataset.InternalRefresh; 

begin
  //!! To be implemented
end;

procedure TDataset.Loaded; 

begin
  //!! To be implemented
end;

procedure TDataset.OpenCursor(InfoQuery: Boolean); 

begin
  //!! To be implemented
end;

procedure TDataset.RefreshInternalCalcFields(Buffer: PChar); 

begin
  //!! To be implemented
end;

procedure TDataset.RestoreState(const Value: TDataSetState);

begin
  //!! To be implemented
end;

Procedure TDataset.SetActive (Value : Boolean);

begin
  If Value<>Factive then
    If Value then
      DoInternalOpen
    else
      DoInternalClose;
  FActive:=Value;
end;

procedure TDataset.SetBookmarkStr(const Value: TBookmarkStr); 

begin
  GotoBookMark(Pointer(Value))
end;

procedure TDataset.SetBufListSize(Value: Longint);

Var I : longint;

begin
  If Value=FBufferCount Then exit;
  I:=RequiredBuffers; // Save 1 call.
  If Value<I Then
    Value:=I;
  If Value>FBufferCount then
    begin
    ReAllocMem(FBuffers,(FBufferCount+1)*SizeOf(PChar),(Value+1)*SizeOf(PChar));
    FillChar(FBuffers[FBufferCount+1],(Value-FBufferCount)*SizeOF(Pchar),#0);
    Try
      For I:=FBufferCount to Value do
        FBuffers[i]:=AllocRecordBuffer;
    except
      I:=FBufferCount;
      While (I<=Value) and (FBuffers[i]<>Nil) do
        begin
        FreeRecordBuffer(FBuffers[i]);
        Inc(i);
        end;
      raise;          
    end;   
    end
  else
    begin
    For I:=Value+1 to FBufferCount do
      FreeRecordBuffer(FBuffers[i]);
    ReAllocMem(FBuffers,FBufferCount*SizeOf(Pchar),Value*SizeOf(Pchar));  
    end;
  FBufferCount:=Value;   
end;

procedure TDataset.SetChildOrder(Component: TComponent; Order: Longint); 

begin
  //!! To be implemented
end;

procedure TDataset.SetCurrentRecord(Index: Longint); 

begin
  If FCurrentRecord<>Index then
    begin
    Writeln ('Setting current record to',index);
    Case GetBookMarkFlag(FBuffers[Index]) of
      bfCurrent : InternalSetToRecord(FBuffers[Index]);
      bfBOF : InternalFirst;
      bfEOF : InternalLast;
      end; 
    FCurrentRecord:=index;
    end;
end;

Procedure TDataset.SetField (Index : Longint;Value : TField);

begin
  //!! To be implemented
end;

procedure TDataset.SetFilterOptions(Value: TFilterOptions); 

begin
  //!! To be implemented
end;

procedure TDataset.SetFilterText(const Value: string); 

begin
  //!! To be implemented
end;

procedure TDataset.SetFiltered(Value: Boolean); 

begin
  //!! To be implemented
end;

procedure TDataset.SetFound(const Value: Boolean);

begin
  //!! To be implemented
end;

procedure TDataset.SetModified(Value: Boolean);

begin
  //!! To be implemented
end;

procedure TDataset.SetName(const Value: TComponentName); 

begin
  //!! To be implemented
end;

procedure TDataset.SetOnFilterRecord(const Value: TFilterRecordEvent); 

begin
  //!! To be implemented
end;

procedure TDataset.SetRecNo(Value: Longint); 

begin
  //!! To be implemented
end;

procedure TDataset.SetState(Value: TDataSetState);

begin
  If Value<>FState then
    begin
    FState:=Value;
    end;
end;

function TDataset.SetTempState(const Value: TDataSetState): TDataSetState;

begin
  //!! To be implemented
end;

function TDataset.TempBuffer: PChar;

begin
  //!! To be implemented
end;

procedure TDataset.UpdateIndexDefs; 

begin
  //!! To be implemented
end;



function TDataset.ControlsDisabled: Boolean;


begin
  //!! To be implemented
end;

function TDataset.ActiveBuffer: PChar;


begin
{$ifdef dsdebug}
//  Writeln ('Active buffer requested. Returning:',ActiveRecord);
{$endif}  
  Result:=FBuffers[ActiveRecord];
end;

procedure TDataset.Append;


begin
  //!! To be implemented
end;

procedure TDataset.AppendRecord(const Values: array of const);


begin
  //!! To be implemented
end;

function TDataset.BookmarkValid(ABookmark: TBookmark): Boolean; 
{
  Should be overridden by descendant objects.
}
begin
  Result:=False
end;

procedure TDataset.Cancel; 


begin
  //!! To be implemented
end;

procedure TDataset.CheckBrowseMode;

begin
  CheckActive;
  If State In [dsedit,dsinsert] then
    begin
    UpdateRecord;
    If Modified then
      Post
    else
      Cancel;
    end;
end;

procedure TDataset.ClearFields;


begin
  //!! To be implemented
end;

procedure TDataset.Close;


begin
  Active:=False;
end;

function TDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; 


begin
  Result:=0;
end;

function TDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; 


begin
  Result:=Nil;
end;

procedure TDataset.CursorPosChanged;


begin
  FCurrentRecord:=-1;
end;

procedure TDataset.Delete;


begin
  //!! To be implemented
end;

procedure TDataset.DisableControls;


begin
  //!! To be implemented
end;

procedure TDataset.Edit;


begin
  //!! To be implemented
end;

procedure TDataset.EnableControls;


begin
  //!! To be implemented
end;

function TDataset.FieldByName(const FieldName: string): TField;


begin
  Result:=FindField(FieldName);
  If Result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[FieldName],Self);
end;

function TDataset.FindField(const FieldName: string): TField;


begin
  Result:=FFieldList.FindField(FieldName);
end;

function TDataset.FindFirst: Boolean;


begin
  //!! To be implemented
end;

function TDataset.FindLast: Boolean;


begin
  //!! To be implemented
end;

function TDataset.FindNext: Boolean;


begin
  //!! To be implemented
end;

function TDataset.FindPrior: Boolean;


begin
  //!! To be implemented
end;

procedure TDataset.First;


begin
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalFirst;
    GetNextRecords;
  finally
    FBOF:=True;
    DoAfterScroll;
  end;
end;

procedure TDataset.FreeBookmark(ABookmark: TBookmark); 


begin
  FreeMem(ABookMark,FBookMarkSize);
end;

function TDataset.GetBookmark: TBookmark; 


begin
  if BookmarkAvailable then 
    begin
    GetMem (Result,FBookMarkSize);
    GetBookMarkdata(ActiveBuffer,Result);
    end
  else
    Result:=Nil;
end;

function TDataset.GetCurrentRecord(Buffer: PChar): Boolean; 


begin
  Result:=False;
end;

procedure TDataset.GetFieldList(List: TList; const FieldNames: string);


begin
  
end;

procedure TDataset.GetFieldNames(List: TStrings);


begin
  FFieldList.GetFieldNames(List);
end;

procedure TDataset.GotoBookmark(ABookmark: TBookmark);


begin
  If Assigned(ABookMark) then
    begin
    CheckBrowseMode;
    DoBeforeScroll;
    InternalGotoBookMark(ABookMark);
    Resync([rmExact,rmCenter]);
    DoAfterScroll;
    end;
end;

procedure TDataset.Insert;


begin
  //!! To be implemented
end;

procedure TDataset.InsertRecord(const Values: array of const);


begin
  //!! To be implemented
end;

function TDataset.IsEmpty: Boolean;

begin
  Result:=(Bof and Eof);
end;

function TDataset.IsSequenced: Boolean; 

begin
  //!! To be implemented
end;

procedure TDataset.Last;

begin
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalLast;
    GetPriorRecords;
    FActiveRecord:=FRecordCount-1;
  finally
    FEOF:=true;
    DoAfterScroll;
  end;
end;

function TDataset.MoveBy(Distance: Longint): Longint;

  Procedure Scrollforward;
  
  begin
{$ifdef dsdebug}
    Writeln('Scrolling forward :',Distance);
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
{$endif}    
    While (Distance>0) and not FEOF do
      begin
      If FActiveRecord<FRecordCount-1 then
        begin
        Inc(FActiveRecord);
        Dec(Distance)
        end
      else
        begin
       {$ifdef dsdebug}
           Writeln('Moveby : need next record');
       {$endif}    
        If GetNextRecord then
          Dec(Distance)
        else
          FEOF:=true;
        end;
      end
  end;

  Procedure ScrollBackward;
  
  begin

{$ifdef dsdebug}
    Writeln('Scrolling backward:',Abs(Distance));
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
{$endif}    
    While (Distance<0) and not FBOF do
      begin
      If FActiveRecord>0 then
        begin
        Dec(FActiveRecord);
        Inc(Distance)
        end
      else
        begin
       {$ifdef dsdebug}
           Writeln('Moveby : need next record');
       {$endif}    
        If GetPriorRecord then
          Inc(Distance)
        else
          FBOF:=true;
        end;
      end
  end;

begin
  CheckBrowseMode;
  Result:=0;
  DoBeforeScroll;
  If ((Distance>0) and FEOF) or
     ((Distance<0) and FBOF) then
    exit;
  Try
    If Distance>0 then 
      ScrollForward
    else 
      ScrollBackward;
  finally
     DoAfterScroll;
  end;
end;

procedure TDataset.Next;

begin
  MoveBy(1);
end;

procedure TDataset.Open;

begin
  Active:=True;
end;

procedure TDataset.Post; 

begin
  //!! To be implemented
end;

procedure TDataset.Prior;

begin
  MoveBy(-1);
end;

procedure TDataset.Refresh;

begin
  //!! To be implemented
end;

procedure TDataset.Resync(Mode: TResyncMode); 

Var Count,ShiftCount : Longint;

begin
  // See if we can find the requested record.
  If rmExact in Mode then
    begin
    { throw an exception if not found.
      Normally the descendant should do this if DoCheck is true. }  
    If GetRecord(Fbuffers[FRecordCount-1],gmcurrent,True)<>grOk Then
      DatabaseError(SNoSuchRecord,Self);
    end
  else
    { Can we find a record in the neighbourhood ?
      Use Shortcut evaluation for this, or we'll have some funny results. }
    If (GetRecord(Fbuffers[FRecordCount-1],gmcurrent,True)<>grOk) and
       (GetRecord(Fbuffers[FRecordCount-1],gmprior,True)<>grOk) and 
       (GetRecord(Fbuffers[FRecordCount-1],gmprior,True)<>grOk) then
       begin
       // nothing found, invalidate buffer and bail out.
       ClearBuffers;
       Exit;
       end; 
  If (rmCenter in Mode) then     
    ShiftCount:=FbufferCount div 2
  else
    // keep current position.
    ShiftCount:=FActiveRecord;  
  // Reposition on 0
  ShiftBuffers(FRecordCount-1);
  ActivateBuffers;
  Count:=0;
  Writeln ('Getting previous',ShiftCount,' records');
  While (Count<ShiftCount) and GetPriorRecord do Inc(Count);
  FActiveRecord:=Count;
  // fill rest of buffers, adjust ActiveBuffer.
  SetCurrentRecord(FRecordCount-1);
  GetNextRecords;
  Inc(FActiveRecord,GetPriorRecords);
end;

procedure TDataset.SetFields(const Values: array of const);

Var I  : longint;

begin
  For I:=0 to high(Values) do
    Case Values[I].vtype of
      vtInteger : FieldByNumber(i).AsLongInt:=Values[I].VInteger;
      // needs Completion..
    end;
end;

procedure TDataset.Translate(Src, Dest: PChar; ToOem: Boolean); 

begin
  //!! To be implemented
end;

procedure TDataset.UpdateCursorPos;

begin
  //!! To be implemented
end;

procedure TDataset.UpdateRecord;

begin
  //!! To be implemented
end;

Procedure TDataset.RemoveField (Field : TField);

begin
  //!! To be implemented
end;

Function TDataset.Getfieldcount : Longint;

begin
  Result:=FFieldList.Count;
end;

Procedure TDataset.ShiftBuffers (Distance : longint);

Var Temp : Pointer;
    MoveSize : Longint;
      
  Procedure ShiftBuffersUp;
  begin
    {$ifdef DSDEBUG}
    writeln ('Shifting buffers up with distance :',Distance);
    writeln ('Moving ',(FBufferCount-Distance), ' Buffers at ',Distance);
    {$endif}
    Move(FBuffers[0],Temp^,MoveSize);
    Move(FBuffers[Distance],FBuffers[0],(FBufferCount-Distance)*SizeOf(Pchar));
    Move(Temp^,FBuffers[FBufferCount-Distance],MoveSize);
  end;

  Procedure ShiftBuffersDown;
  
  begin
    // Distance is NEGATIVE
    {$ifdef DSDEBUG}
    writeln ('Shifting buffers down with distance :',Abs(Distance));
    writeln ('Moving ',Movesize div 4,' Buffers at ',FBufferCount+Distance);
    {$endif}
    Move(FBuffers[FbufferCount+Distance],Temp^ ,MoveSize);
    Move(FBuffers[0],FBuffers[Abs(Distance)],(FBufferCount+Distance)*SizeOf(Pchar));
    Move(Temp^ ,FBuffers[0],MoveSize);
  end;

begin
  If Abs(Distance)>=BufferCount then Exit;
  try
    MoveSize:=SizeOf(Pchar)*Abs(Distance);
    GetMem(Temp,MoveSize);
    If Distance<0 Then
      ShiftBuffersDown
    else If Distance>0 then
      ShiftBuffersUp;
  Finally  
    FreeMem(temp);
  end;
end;

{
  $Log$
  Revision 1.2  1999-10-24 17:07:54  michael
  + Added copyright header

}
