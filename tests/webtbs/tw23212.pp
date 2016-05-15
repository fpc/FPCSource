program TestCase;

{$MODE DELPHI}

Type

  TSomeRec1 = Packed Record
    A : Integer; // Changing this to Byte oddly enough will work, but Integer does not..
    B : Byte;
  End; { Record }

  TSomeRecord = Packed Record
    Case x : Cardinal OF
      0 : (A : TSomeRec1);
  End; { Record }

  TBaseList<T> = Class

    Private

      // Fields //

      FItems : Array OF T;

    Protected

      // Methods //

      Function  GetItem(Index : Integer) : T; Virtual;
      Procedure SetItem(Index : Integer; Const Value : T); Virtual;

    Public

      // Methods //

      Constructor Create;
      Destructor  Destroy; Override;

      // Properties //

      Property Items[Index : Integer] : T Read GetItem Write SetItem; Default;

  End; { Class }

  TSomeList = TBaseList<TSomeRecord>;

  TSomeClass = Class

    Private

      // Fields //

      FItems : TSomeList;

    Public

      // Methods //

      Constructor Create;
      Destructor  Destroy; Override;

      Procedure GetRec(Index : Integer; Out Rec : TSomeRecord);
      Procedure SetRec(Index : Integer; Const Rec : TSomeRecord);

  End; { Class }

//****************************************************************************//
//****************************************************************************//
//********** TBaseList Class *************************************************//
//****************************************************************************//
//****************************************************************************//

//========== Protected Methods ===============================================//

Function  TBaseList<T>.GetItem(Index : Integer) : T;
Begin
  Result := FItems[Index];
End; { Function }

Procedure TBaseList<T>.SetItem(Index : Integer; Const Value : T);
Begin
  IF Index >= High(FItems) Then SetLength(FItems, Index + 1);
  FItems[Index] := Value;
End; { Procedure }

//========== Public Methods ==================================================//

Constructor TBaseList<T>.Create;
Begin
  Inherited;
End; { Constructor }

Destructor  TBaseList<T>.Destroy;
Begin
  Finalize(FItems);
  Inherited;
End; { Destructor }

//****************************************************************************//
//****************************************************************************//
//********** TSomeClass Class ************************************************//
//****************************************************************************//
//****************************************************************************//

//========== Public Methods ==================================================//

Constructor TSomeClass.Create;
Begin
  Inherited;
  FItems := TSomeList.Create;
End; { Constructor }

Destructor  TSomeClass.Destroy;
Begin
  FItems.Free;
  Inherited;
End; { Destructor }

Procedure TSomeClass.GetRec(Index : Integer; Out Rec : TSomeRecord);
Begin
  Rec := FItems[Index];
End; { Procedure }

Procedure TSomeClass.SetRec(Index : Integer; Const Rec : TSomeRecord);
Begin
  FItems[Index] := Rec;
End; { Procedure }

//========== Global Variables ================================================//

Var

  C   : TSomeClass;
  Rec : TSomeRecord;

Begin

  C := TSomeClass.Create;

  Rec.A.A := 42;
  Rec.A.B := 5;

  C.SetRec(0, Rec);
  C.GetRec(0, Rec);

  Writeln(Rec.A.A, ',', Rec.A.B);

  C.Free;
  if (rec.a.a<>42) or (rec.a.b<>5) then
    halt(1);

End.

