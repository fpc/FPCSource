{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    LINQ-style operator-based criteria builder.
    Thin wrapper over TOPCriteria using record operator overloading.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
     Usage:
      uses Opf.Linq;
      OPManager.Load(List, TPerson,
        (Prop('Active') = True) and ((Prop('Age') > 18) or (Prop('VIP') = True)));
}

unit Opf.Linq;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

{ COM interfaces provide automatic reference counting for the internal
  TOPCriteria wrappers. Each record operator creates temporaries that must
  be freed when no longer referenced. FPC's management operators
  (Initialize/Finalize/AddRef) do NOT reliably finalize all temporaries
  in complex operator chains, causing memory leaks. COM interface
  refcounting is deeply integrated into the FPC code generator and
  correctly handles every temporary. This directive is local to this
  unit - the rest of the codebase uses CORBA interfaces. }
{$INTERFACES COM}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, DB,
{$ENDIF}
  Opf.Manager, Opf.Criteria;

type
  { IOPLinqCtx - refcounted wrapper owning a TOPCriteria }
  IOPLinqCtx = interface
    ['{8E4A9B23-C5D7-4F6A-AB2E-3D9F7A1C4E85}']
    { The criteria held by this wrapper }
    function GetCriteria: TOPCriteria;
    { Takes the criteria out of this wrapper, handing ownership to the caller }
    function DetachCriteria: TOPCriteria;
  end;

  { TOPLinqExpr - lightweight record wrapping a refcounted TOPCriteria }

  TOPLinqExpr = record
    FCtx: IOPLinqCtx;
    { Condition that is true when both conditions are true }
    class operator and(A, B: TOPLinqExpr): TOPLinqExpr;
    { Condition that is true when either condition is true }
    class operator or(A, B: TOPLinqExpr): TOPLinqExpr;
    { Condition that is true when the given condition is false }
    class operator not(A: TOPLinqExpr): TOPLinqExpr;
  end;

  { TOPLinqProp - field reference with typed comparison operators }

  TOPLinqProp = record
    FName: UTF8String;
    { Integer }
    { Condition: field equals the whole number }
    class operator =(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Condition: field differs from the integer }
    class operator <>(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Condition: field is greater than the integer }
    class operator >(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Condition: field is less than the integer }
    class operator <(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Condition: field is greater than or equal to the integer }
    class operator >=(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Condition: field is less than or equal to the integer }
    class operator <=(A: TOPLinqProp; B: Integer): TOPLinqExpr;
    { Int64 }
    { Condition: field equals the large whole number }
    class operator =(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { Condition: field differs from the 64-bit integer }
    class operator <>(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { Condition: field is greater than the 64-bit integer }
    class operator >(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { Condition: field is less than the 64-bit integer }
    class operator <(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { Condition: field is greater than or equal to the 64-bit integer }
    class operator >=(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { Condition: field is less than or equal to the 64-bit integer }
    class operator <=(A: TOPLinqProp; B: Int64): TOPLinqExpr;
    { String }
    { Condition: field equals the text }
    class operator =(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Condition: field differs from the text }
    class operator <>(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Condition: field sorts after the text }
    class operator >(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Condition: field sorts before the text }
    class operator <(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Condition: field sorts after or equals the text }
    class operator >=(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Condition: field sorts before or equals the text }
    class operator <=(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr;
    { Double }
    { Condition: field equals the decimal number }
    class operator =(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Condition: field differs from the floating-point number }
    class operator <>(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Condition: field is greater than the floating-point number }
    class operator >(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Condition: field is less than the floating-point number }
    class operator <(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Condition: field is greater than or equal to the floating-point number }
    class operator >=(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Condition: field is less than or equal to the floating-point number }
    class operator <=(A: TOPLinqProp; B: Double): TOPLinqExpr;
    { Boolean }
    { Condition: field equals the true/false value }
    class operator =(A: TOPLinqProp; B: Boolean): TOPLinqExpr;
    { Condition: field differs from the boolean value }
    class operator <>(A: TOPLinqProp; B: Boolean): TOPLinqExpr;
    { TDateTime }
    { Condition: field equals the date/time }
    class operator =(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Condition: field differs from the date/time }
    class operator <>(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Condition: field is later than the date/time }
    class operator >(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Condition: field is earlier than the date/time }
    class operator <(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Condition: field is later than or equal to the date/time }
    class operator >=(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Condition: field is earlier than or equal to the date/time }
    class operator <=(A: TOPLinqProp; B: TDateTime): TOPLinqExpr;
    { Special conditions }
    { Condition: field is NULL }
    function IsNull: TOPLinqExpr;
    { Condition: field has a value (is not null) }
    function IsNotNull: TOPLinqExpr;
    { Condition: field matches the LIKE pattern }
    function Like(const APattern: UTF8String): TOPLinqExpr;
    { Condition: field does not match the LIKE pattern }
    function NotLike(const APattern: UTF8String): TOPLinqExpr;
    { Condition: field contains the given text }
    function Contains(const AValue: UTF8String): TOPLinqExpr;
  end;

  { TOPManagerLinqHelper - adds Load(TOPLinqExpr) without creating
    a dependency from opf.manager on opf.linq }

  TOPManagerLinqHelper = class helper for TOPManager
    { Loads into the list all objects of the class that match the expression }
    Procedure Load(aList: TObject; aClass: TClass; aExpr: TOPLinqExpr); overload;
  end;

{ Create a property reference for use in LINQ expressions }
function Prop(const AName: UTF8String): TOPLinqProp;

implementation

uses Opf.Types, Opf.Consts;

type
  { TOPLinqCtx:
    Owns a TOPCriteria, freed when refcount hits zero.
    DetachCriteria transfers ownership to the caller; it is no longer used
    by the operators, which copy their operands instead. }

  TOPLinqCtx = class(TInterfacedObject, IOPLinqCtx)
  private
    FCriteria: TOPCriteria;
  public
    constructor Create(ACriteria: TOPCriteria);
    destructor Destroy; override;
    function GetCriteria: TOPCriteria;
    function DetachCriteria: TOPCriteria;
  end;

constructor TOPLinqCtx.Create(ACriteria: TOPCriteria);
begin
  inherited Create;
  FCriteria := ACriteria;
end;

destructor TOPLinqCtx.Destroy;
begin
  FreeAndNil(FCriteria);
  inherited Destroy;
end;

function TOPLinqCtx.GetCriteria: TOPCriteria;
begin
  Result := FCriteria;
end;

function TOPLinqCtx.DetachCriteria: TOPCriteria;
begin
  Result := FCriteria;
  FCriteria := nil;
end;

{ --- Helpers --- }

function Wrap(ACriteria: TOPCriteria): TOPLinqExpr;
begin
  Result.FCtx := TOPLinqCtx.Create(ACriteria);
end;

{ The criteria of an expression; raises for an unassigned expression }

function ExprCriteria(const aExpr: TOPLinqExpr): TOPCriteria;
begin
  Result := Nil;
  if Assigned(aExpr.FCtx) then
    Result := aExpr.FCtx.GetCriteria;
  if Result = Nil then
    raise EOPF.Create(SErrLinqEmptyExpression);
end;

{ --- TOPLinqExpr logical operators ---
  The operands are never modified: the result is built on a copy of A's
  criteria, with a copy of B's conditions added as a parenthesized group.
  This allows operands to be reused, e.g. E and E, or (E and F) or E. }

class operator TOPLinqExpr.and(A, B: TOPLinqExpr): TOPLinqExpr;
var
  Base: TOPCriteria;
begin
  Base := ExprCriteria(A).Clone;
  try
    Base.AddAndCopy(ExprCriteria(B));
  except
    Base.Free;
    raise;
  end;
  Result := Wrap(Base);
end;

class operator TOPLinqExpr.or(A, B: TOPLinqExpr): TOPLinqExpr;
var
  Base: TOPCriteria;
begin
  Base := ExprCriteria(A).Clone;
  try
    Base.AddOrCopy(ExprCriteria(B));
  except
    Base.Free;
    raise;
  end;
  Result := Wrap(Base);
end;

class operator TOPLinqExpr.not(A: TOPLinqExpr): TOPLinqExpr;
var
  Base: TOPCriteria;
begin
  Base := ExprCriteria(A).Clone;
  Base.Not_;
  Result := Wrap(Base);
end;

{ --- TOPLinqProp comparison operators ---
  Each creates a fresh TOPCriteria with a single Where call. }

{$MACRO ON}

{$DEFINE IMPL_COMP_INT := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{$DEFINE IMPL_COMP_INT64 := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{$DEFINE IMPL_COMP_STR := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{$DEFINE IMPL_COMP_DBL := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{$DEFINE IMPL_COMP_BOOL := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{$DEFINE IMPL_COMP_DT := var Cr: TOPCriteria;
begin Cr := TOPCriteria.Create; Cr.Where(A.FName, OP, B); Result := Wrap(Cr); end}

{ Integer }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;
{$DEFINE OP := opGreater}
class operator TOPLinqProp.>(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;
{$DEFINE OP := opLess}
class operator TOPLinqProp.<(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;
{$DEFINE OP := opGreaterOrEqual}
class operator TOPLinqProp.>=(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;
{$DEFINE OP := opLessOrEqual}
class operator TOPLinqProp.<=(A: TOPLinqProp; B: Integer): TOPLinqExpr; IMPL_COMP_INT;

{ Int64 }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;
{$DEFINE OP := opGreater}
class operator TOPLinqProp.>(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;
{$DEFINE OP := opLess}
class operator TOPLinqProp.<(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;
{$DEFINE OP := opGreaterOrEqual}
class operator TOPLinqProp.>=(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;
{$DEFINE OP := opLessOrEqual}
class operator TOPLinqProp.<=(A: TOPLinqProp; B: Int64): TOPLinqExpr; IMPL_COMP_INT64;

{ String }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;
{$DEFINE OP := opGreater}
class operator TOPLinqProp.>(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;
{$DEFINE OP := opLess}
class operator TOPLinqProp.<(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;
{$DEFINE OP := opGreaterOrEqual}
class operator TOPLinqProp.>=(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;
{$DEFINE OP := opLessOrEqual}
class operator TOPLinqProp.<=(A: TOPLinqProp; const B: UTF8String): TOPLinqExpr; IMPL_COMP_STR;

{ Double }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;
{$DEFINE OP := opGreater}
class operator TOPLinqProp.>(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;
{$DEFINE OP := opLess}
class operator TOPLinqProp.<(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;
{$DEFINE OP := opGreaterOrEqual}
class operator TOPLinqProp.>=(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;
{$DEFINE OP := opLessOrEqual}
class operator TOPLinqProp.<=(A: TOPLinqProp; B: Double): TOPLinqExpr; IMPL_COMP_DBL;

{ Boolean }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; B: Boolean): TOPLinqExpr; IMPL_COMP_BOOL;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; B: Boolean): TOPLinqExpr; IMPL_COMP_BOOL;

{ TDateTime }
{$DEFINE OP := opEqual}
class operator TOPLinqProp.=(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;
{$DEFINE OP := opNotEqual}
class operator TOPLinqProp.<>(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;
{$DEFINE OP := opGreater}
class operator TOPLinqProp.>(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;
{$DEFINE OP := opLess}
class operator TOPLinqProp.<(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;
{$DEFINE OP := opGreaterOrEqual}
class operator TOPLinqProp.>=(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;
{$DEFINE OP := opLessOrEqual}
class operator TOPLinqProp.<=(A: TOPLinqProp; B: TDateTime): TOPLinqExpr; IMPL_COMP_DT;

{$MACRO OFF}

{ --- Special conditions --- }

function TOPLinqProp.IsNull: TOPLinqExpr;
var
  Cr: TOPCriteria;
begin
  Cr := TOPCriteria.Create;
  Cr.WhereIsNull(FName);
  Result := Wrap(Cr);
end;

function TOPLinqProp.IsNotNull: TOPLinqExpr;
var
  Cr: TOPCriteria;
begin
  Cr := TOPCriteria.Create;
  Cr.WhereIsNotNull(FName);
  Result := Wrap(Cr);
end;

function TOPLinqProp.Like(const APattern: UTF8String): TOPLinqExpr;
var
  Cr: TOPCriteria;
begin
  Cr := TOPCriteria.Create;
  Cr.Where(FName, opLike, APattern);
  Result := Wrap(Cr);
end;

function TOPLinqProp.NotLike(const APattern: UTF8String): TOPLinqExpr;
var
  Cr: TOPCriteria;
begin
  Cr := TOPCriteria.Create;
  Cr.Where(FName, opNotLike, APattern);
  Result := Wrap(Cr);
end;

function TOPLinqProp.Contains(const AValue: UTF8String): TOPLinqExpr;
var
  Cr: TOPCriteria;
begin
  Cr := TOPCriteria.Create;
  Cr.Where(FName, opContains, AValue);
  Result := Wrap(Cr);
end;

{ --- Public functions --- }

function Prop(const AName: UTF8String): TOPLinqProp;
begin
  Result.FName := AName;
end;

{ --- Class helper --- }

procedure TOPManagerLinqHelper.Load(aList: TObject; aClass: TClass; aExpr: TOPLinqExpr);
begin
  Load(aList, aClass, ExprCriteria(aExpr));
end;

end.
