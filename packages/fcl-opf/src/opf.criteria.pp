{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Parameterized criteria API for type-safe query building

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Criteria;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, DB,
{$ENDIF}
  Opf.Fields;

type

  { TOPCompareOp - comparison operators for WHERE expressions }

  TOPCompareOp = (
    opEqual,
    opNotEqual,
    opLess,
    opLessOrEqual,
    opGreater,
    opGreaterOrEqual,
    opLike,
    opNotLike,
    opContains
  );

  { TOPLogicalOp - logical connectors }

  TOPLogicalOp = (
    loAnd,
    loOr
  );

  { TOPNodeKind - discriminator for expression nodes }

  TOPNodeKind = (
    nkComparison,
    nkLogical,
    nkNot,
    nkIsNull,
    nkIsNotNull,
    nkIn,
    nkNotIn,
    nkBetween,
    nkGroup,
    nkRaw
  );

  { TOPExprNode - variant record for all expression node types }

  TOPExprNode = record
    Kind: TOPNodeKind;
    Field: UTF8String;
    // opContains: the text to search for, unescaped
    Text: UTF8String;
    case TOPNodeKind of
      nkComparison: (
        CompareOp: TOPCompareOp;
        ParamIndex: Integer;
      );
      nkLogical: (
        LogicalOp: TOPLogicalOp;
        LeftIndex: Integer;
        RightIndex: Integer;
      );
      nkNot: (
        NotChildIndex: Integer;
      );
      nkIsNull: ();
      nkIsNotNull: ();
      nkIn: (
        InParamCount: Integer;
        InParamStart: Integer;
      );
      nkNotIn: (
        NotInParamCount: Integer;
        NotInParamStart: Integer;
      );
      nkBetween: (
        BetweenLowParam: Integer;
        BetweenHighParam: Integer;
      );
      nkGroup: (
        GroupChildIndex: Integer;
      );
      nkRaw: (
        // Field holds the SQL fragment with ? placeholders
        RawParamStart: Integer;
        RawParamCount: Integer;
      );
  end;

  TOPExprNodeArray = array of TOPExprNode;

  { One ORDER BY item }

  TOPOrderByItem = record
    Field: UTF8String;
    Descending: Boolean;
  end;
  TOPOrderByItemArray = array of TOPOrderByItem;

  { TOPCriteria:
    fluent, recursive expression builder for parameterized
    WHERE clauses with ordering and pagination support.
    Users specify property names; these are resolved to database field names
    at SQL generation time via a TOPFieldMapList. A name that is neither a
    mapped property nor a mapped column raises an exception: use WhereRaw
    for SQL expressions.
    Logical nodes are parenthesized where AND and OR meet, so the generated
    SQL follows the order in which the criteria were built:
    Where(a).AddOr(b).AddAnd(c) means (a OR b) AND c. }

  TOPCriteria = class(TObject)
  private
    FNodes: TOPExprNodeArray;
    FParams: TParams;
    FParamCounter: Integer;
    FRootNode: Integer;
    FOrderBy: TOPOrderByItemArray;
    FLimitValue: Integer;
    FOffsetValue: Integer;
    FIncludeDeleted: Boolean;
    FOwnedCriteria: TFPList;
    FOwner: TOPCriteria;              // The criteria that owns this one (AddAnd/AddOr)
    FFormatter: TOPIdentifierFormatter; // Only set during SQL generation
    FLikeEscaper: TOPLikeEscaper;     // Only set during SQL generation
    function AddNode(aNode: TOPExprNode): Integer;
    function ChildSQL(aChild: Integer; aParentOp: TOPLogicalOp; aFieldMap: TOPFieldMapList): UTF8String;
    function RawToSQL(const aNode: TOPExprNode): UTF8String;
    function InternalAddRaw(const aSQL: UTF8String; const aValues: array of const): Integer;
    function InternalAddSub(aSub: TOPCriteria; aOp: TOPLogicalOp; aTakeOwnership: Boolean): TOPCriteria;
    procedure CheckSubSettings(aSub: TOPCriteria);
    procedure MergeSubSettings(aSub: TOPCriteria);
    procedure TakeExpression(aSource: TOPCriteria);
    function NextParamName: UTF8String;
    function AddParamInteger(aValue: Integer): Integer;
    function AddParamInt64(aValue: Int64): Integer;
    function AddParamString(const aValue: UTF8String): Integer;
    function AddParamFloat(aValue: Double): Integer;
    function AddParamBoolean(aValue: Boolean): Integer;
    function AddParamDateTime(aValue: TDateTime): Integer;
    function AddParamFromVarRec(const aVarRec: TVarRec): Integer;
    function NodeToSQL(aIndex: Integer; aFieldMap: TOPFieldMapList): UTF8String;
    function ResolveFieldName(const aPropName: UTF8String; aFieldMap: TOPFieldMapList): UTF8String;
    function ResolveFieldName(const aPropName: UTF8String; aFieldMap: TOPFieldMapList;
      aParamStart, aParamCount: Integer): UTF8String;
    procedure MergeSubCriteria(aSub: TOPCriteria);
    function CombineWithLogical(aOp: TOPLogicalOp; aRightNodeIndex: Integer): TOPCriteria;
    function InternalAddComparison(const aField: UTF8String; aOp: TOPCompareOp; aParamIdx: Integer): Integer;
    function InternalAddIn(const aField: UTF8String; aNotIn: Boolean; const aValues: array of const): Integer;
    function InternalAddInDateTime(const aField: UTF8String; aNotIn: Boolean; const aValues: array of TDateTime): Integer;
  public
    // Creates an empty criteria object with no conditions, ordering or paging
    constructor Create;
    // Frees the criteria object, its parameters and any criteria it owns
    destructor Destroy; override;

    { Initial WHERE - typed overloads.
      When a Where... call raises, the conditions set before are kept. }
    // Replaces all conditions with one comparing a field with a whole number
    function Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria; overload;
    // Replaces all conditions with one comparing a field with a large whole number
    function Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria; overload;
    // Replaces all conditions with one comparing a field with a text value
    function Where(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria; overload;
    // Replaces all conditions with one comparing a field with a decimal number
    function Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria; overload;
    // Replaces all conditions with one comparing a field with a true/false value
    function Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria; overload;
    // Replaces all conditions with one comparing a field with a date/time value
    function Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria; overload;
    // Replaces all conditions with one requiring the field to be empty (NULL)
    function WhereIsNull(const aField: UTF8String): TOPCriteria;
    // Replaces all conditions with one requiring the field to have a value (not NULL)
    function WhereIsNotNull(const aField: UTF8String): TOPCriteria;
    { Replaces all conditions with one requiring the field to equal one of the given values.
      Array of const variants (In, NotIn, Between): a TDateTime value arrives
      as a float; it is bound as a date/time when aField is mapped to a
      date/time property, as a float otherwise. }
    function WhereIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Replaces all conditions with one requiring the field to equal one of the given dates
    function WhereIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Replaces all conditions with one requiring the field to differ from all given values
    function WhereNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Replaces all conditions with one requiring the field to differ from all given dates
    function WhereNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Replaces all conditions with a range check; aValues holds exactly the low and high value
    function WhereBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Replaces all conditions with a check that the field lies between two dates
    function WhereBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria; overload;
    { Raw SQL fragment, always parenthesized. Each ? outside quotes is replaced
      by a parameter holding the matching value. Identifiers in aSQL are used
      as-is: never build aSQL from user input.
      A TDateTime in aValues cannot be told apart from a Double and is bound as
      a float: compare dates with Where/AddAnd(field, op, TDateTime) instead,
      or cast in the SQL. }
    function WhereRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;
    // Adds a raw SQL condition with AND, like WhereRaw
    function AddAndRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;
    // Adds a raw SQL condition with OR, like WhereRaw
    function AddOrRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;

    { AND composition - typed overloads }
    // Adds an AND condition comparing a field with a whole number
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria; overload;
    // Adds an AND condition comparing a field with a large whole number
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria; overload;
    // Adds an AND condition comparing a field with a text value
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria; overload;
    // Adds an AND condition comparing a field with a decimal number
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria; overload;
    // Adds an AND condition comparing a field with a true/false value
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria; overload;
    // Adds an AND condition comparing a field with a date/time value
    function AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria; overload;
    { Takes ownership of aSub: it is freed with this criteria, or at once when
      the merge raises (e.g. conflicting paging), so the caller must not use or
      free aSub after the call.
      A criteria can have only one owner: adding it to a second criteria, or
      adding a criteria to one of its own sub-criteria, raises. In these misuse
      cases (and for aSub=Self) aSub is not freed, because another criteria
      still owns it.
      Use AddAndCopy to combine the same conditions several times. }
    function AddAnd(aSub: TOPCriteria): TOPCriteria; overload;
    // Copies the conditions of aSub; aSub remains owned by the caller
    function AddAndCopy(aSub: TOPCriteria): TOPCriteria;
    // Adds an AND condition requiring the field to be empty (NULL)
    function AddAndIsNull(const aField: UTF8String): TOPCriteria;
    // Adds an AND condition requiring the field to have a value (not NULL)
    function AddAndIsNotNull(const aField: UTF8String): TOPCriteria;
    // Adds an AND condition requiring the field to equal one of the given values
    function AddAndIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an AND condition requiring the field to equal one of the given dates
    function AddAndIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Adds an AND condition requiring the field to differ from all given values
    function AddAndNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an AND condition requiring the field to differ from all given dates
    function AddAndNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Adds an AND range check; aValues holds exactly the low and high value
    function AddAndBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an AND condition that the field lies between two dates
    function AddAndBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria; overload;

    { OR composition - typed overloads }
    // Adds an OR condition comparing a field with a whole number
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria; overload;
    // Adds an OR condition comparing a field with a large whole number
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria; overload;
    // Adds an OR condition comparing a field with a text value
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria; overload;
    // Adds an OR condition comparing a field with a decimal number
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria; overload;
    // Adds an OR condition comparing a field with a true/false value
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria; overload;
    // Adds an OR condition comparing a field with a date/time value
    function AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria; overload;
    // Takes ownership of aSub, like AddAnd(aSub)
    function AddOr(aSub: TOPCriteria): TOPCriteria; overload;
    // Copies the conditions of aSub; aSub remains owned by the caller
    function AddOrCopy(aSub: TOPCriteria): TOPCriteria;
    // Adds an OR condition requiring the field to be empty (NULL)
    function AddOrIsNull(const aField: UTF8String): TOPCriteria;
    // Adds an OR condition requiring the field to have a value (not NULL)
    function AddOrIsNotNull(const aField: UTF8String): TOPCriteria;
    // Adds an OR condition requiring the field to equal one of the given values
    function AddOrIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an OR condition requiring the field to equal one of the given dates
    function AddOrIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Adds an OR condition requiring the field to differ from all given values
    function AddOrNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an OR condition requiring the field to differ from all given dates
    function AddOrNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria; overload;
    // Adds an OR range check; aValues holds exactly the low and high value
    function AddOrBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria; overload;
    // Adds an OR condition that the field lies between two dates
    function AddOrBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria; overload;

    { Negates the whole condition built so far }
    function Not_: TOPCriteria;

    { Also loads objects that are marked as deleted }
    function IncludeDeleted: TOPCriteria;

    { Ordering and pagination.
      aField is a field name, optionally followed by ASC or DESC.
      A comma-separated list of such items is also accepted. }
    function OrderBy(const aField: UTF8String; aDescending: Boolean = False): TOPCriteria;
    { -1 (the default) means no limit / no offset. Limit(0) loads no rows. }
    function Limit(aLimit: Integer): TOPCriteria;
    // Sets how many rows to skip before the first row is returned
    function Offset(aOffset: Integer): TOPCriteria;

    { Independent deep copy of conditions, parameters, ordering and paging }
    function Clone: TOPCriteria;

    { SQL generation - aFieldMap resolves property names to DB field names,
      aFormatter (optional) quotes or transforms the resulting identifiers. }
    function ToWhereSQL(aFieldMap: TOPFieldMapList): UTF8String; overload;
    // Returns the text of the WHERE condition, with field names passed through aFormatter
    function ToWhereSQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter): UTF8String; overload;
    // aLikeEscaper: dialect escaping for opContains (Nil: escape ! % _ with !)
    function ToWhereSQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter;
      aLikeEscaper: TOPLikeEscaper): UTF8String; overload;
    // Returns the text of the ORDER BY list
    function ToOrderBySQL(aFieldMap: TOPFieldMapList): UTF8String; overload;
    // Returns the text of the ORDER BY list, with field names passed through aFormatter
    function ToOrderBySQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter): UTF8String; overload;
    // True when at least one condition was added
    function HasWhereClause: Boolean;
    // True when soft-deleted rows should also be loaded
    function HasIncludeDeleted: Boolean;
    // Returns the maximum number of rows to load, -1 for no limit
    function GetLimitValue: Integer;
    // Returns the number of rows to skip, -1 for none
    function GetOffsetValue: Integer;

    // The parameter values used by the conditions
    property Params: TParams read FParams;
  end;

implementation

uses Opf.Types, Opf.Consts, Opf.FieldValue;

Resourcestring
  SErrCriteriaOwnedElsewhere = 'Sub-criteria is already owned by another criteria: use AddAndCopy or AddOrCopy';
  SErrCriteriaOwnerCycle = 'Sub-criteria owns this criteria: adding it would create an ownership cycle';

{ Helpers: build expression nodes }

function MakeComparisonNode(const aField: UTF8String; aOp: TOPCompareOp; aParamIndex: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.Kind:=nkComparison;
  Result.Field:=aField;
  Result.CompareOp:=aOp;
  Result.ParamIndex:=aParamIndex;
end;

function MakeLogicalNode(aOp: TOPLogicalOp; aLeft, aRight: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.Kind:=nkLogical;
  Result.Field:='';
  Result.LogicalOp:=aOp;
  Result.LeftIndex:=aLeft;
  Result.RightIndex:=aRight;
end;

function MakeNotNode(aChild: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.Kind:=nkNot;
  Result.Field:='';
  Result.NotChildIndex:=aChild;
end;

function MakeNullNode(const aField: UTF8String; aIsNotNull: Boolean): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  if aIsNotNull then
    Result.Kind:=nkIsNotNull
  else
    Result.Kind:=nkIsNull;
  Result.Field:=aField;
end;

function MakeInNode(const aField: UTF8String; aNotIn: Boolean; aParamStart, aParamCount: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  if aNotIn then
  begin
    Result.Kind:=nkNotIn;
    Result.Field:=aField;
    Result.NotInParamStart:=aParamStart;
    Result.NotInParamCount:=aParamCount;
  end
  else
  begin
    Result.Kind:=nkIn;
    Result.Field:=aField;
    Result.InParamStart:=aParamStart;
    Result.InParamCount:=aParamCount;
  end;
end;

function MakeBetweenNode(const aField: UTF8String; aLowParam, aHighParam: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.Kind:=nkBetween;
  Result.Field:=aField;
  Result.BetweenLowParam:=aLowParam;
  Result.BetweenHighParam:=aHighParam;
end;

function MakeGroupNode(aChild: Integer): TOPExprNode;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.Kind:=nkGroup;
  Result.Field:='';
  Result.GroupChildIndex:=aChild;
end;

{ TOPCriteria }

constructor TOPCriteria.Create;
begin
  inherited Create;
  FParams:=TParams.Create(nil);
  FParamCounter:=0;
  FRootNode:=-1;
  FOrderBy:=Nil;
  FLimitValue:=-1;
  FOffsetValue:=-1;
  FIncludeDeleted:=False;
  FOwnedCriteria:=TFPList.Create;
end;

destructor TOPCriteria.Destroy;
var
  I: Integer;
begin
  for I:=0 to FOwnedCriteria.Count - 1 do
    TObject(FOwnedCriteria[I]).Free;
  FreeAndNil(FOwnedCriteria);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TOPCriteria.AddNode(aNode: TOPExprNode): Integer;
var
  Len: Integer;
begin
  Len:=Length(FNodes);
  SetLength(FNodes, Len + 1);
  FNodes[Len]:=aNode;
  Result:=Len;
end;

function TOPCriteria.NextParamName: UTF8String;
begin
  Inc(FParamCounter);
  Result:='p' + IntToStr(FParamCounter);
end;

function TOPCriteria.AddParamInteger(aValue: Integer): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftInteger;
  P.AsInteger:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamInt64(aValue: Int64): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftLargeint;
  P.AsLargeInt:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamString(const aValue: UTF8String): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftString;
  P.AsString:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamFloat(aValue: Double): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftFloat;
  P.AsFloat:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamBoolean(aValue: Boolean): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftBoolean;
  P.AsBoolean:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamDateTime(aValue: TDateTime): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  P.Name:=NextParamName;
  P.DataType:=ftDateTime;
  P.AsDateTime:=aValue;
  Result:=FParams.Count - 1;
end;

function TOPCriteria.AddParamFromVarRec(const aVarRec: TVarRec): Integer;
var
  P: TParam;
begin
  P:=TParam(FParams.Add);
  try
    P.Name:=NextParamName;
    OPFAssignVarRec(aVarRec, P);
  except
    P.Free;
    Dec(FParamCounter);
    raise;
  end;
  Result:=FParams.Count - 1;
end;

{ True if aName is a plain identifier, optionally qualified: name or table.name }

function IsPlainIdentifier(const aName: UTF8String): Boolean;
var
  I: Integer;
  AtStart: Boolean;
begin
  Result:=False;
  if aName='' then
    Exit;
  AtStart:=True;
  for I:=1 to Length(aName) do
    case aName[I] of
      'A'..'Z', 'a'..'z', '_':
        AtStart:=False;
      '0'..'9', '$':
        if AtStart then
          Exit;
      '.':
        begin
          if AtStart then
            Exit;
          AtStart:=True;
        end;
    else
      Exit;
    end;
  Result:=not AtStart;
end;

{ Resolve a property name to its column name. Mapped column names are accepted
  as well. Without a field map, only plain identifiers are accepted. }

function TOPCriteria.ResolveFieldName(const aPropName: UTF8String; aFieldMap: TOPFieldMapList): UTF8String;
begin
  Result:=ResolveFieldName(aPropName, aFieldMap, 0, 0);
end;

{ Also give the aParamCount parameters from aParamStart the type of the field:
  a date in an array of const arrives as a float (vtExtended), which is
  compared numerically, or rejected (PostgreSQL), against a date column. }

function TOPCriteria.ResolveFieldName(const aPropName: UTF8String; aFieldMap: TOPFieldMapList;
  aParamStart, aParamCount: Integer): UTF8String;
var
  F: Opf.Fields.TOPFieldMap;
  P: TParam;
  I: Integer;
begin
  if aFieldMap = Nil then
  begin
    if not IsPlainIdentifier(aPropName) then
      raise EOPF.CreateFmt(SErrCriteriaInvalidField, [aPropName]);
    Result:=aPropName;
  end
  else
  begin
    F:=aFieldMap.FindByPropertyName(aPropName);
    if F=Nil then
      F:=aFieldMap.FindByFieldName(aPropName);
    if F=Nil then
      raise EOPF.CreateFmt(SErrCriteriaUnknownField, [aPropName]);
    Result:=F.FieldName;
    if OPFIsNativeDateTime(F) then
      for I:=aParamStart to aParamStart+aParamCount-1 do
      begin
        P:=FParams[I];
        if (P.DataType=ftFloat) and not P.IsNull then
          OPFSetFloatParam(P, F, P.AsFloat);
      end;
  end;
  if Assigned(FFormatter) then
    Result:=FFormatter(Result);
end;

{ SQL for a child of a logical node: a logical child with a different operator
  is parenthesized, so the tree structure is preserved in the SQL. }

function TOPCriteria.ChildSQL(aChild: Integer; aParentOp: TOPLogicalOp; aFieldMap: TOPFieldMapList): UTF8String;
begin
  Result:=NodeToSQL(aChild, aFieldMap);
  if (FNodes[aChild].Kind=nkLogical) and (FNodes[aChild].LogicalOp<>aParentOp) then
    Result:='(' + Result + ')';
end;

{ Replace each ? outside quoted strings and quoted identifiers by its parameter }

function TOPCriteria.RawToSQL(const aNode: TOPExprNode): UTF8String;
var
  I, P: Integer;
  Quote: Char;
  S: UTF8String;
begin
  S:=aNode.Field;
  Result:='';
  P:=aNode.RawParamStart;
  Quote:=#0;
  for I:=1 to Length(S) do
  begin
    if Quote<>#0 then
    begin
      if S[I]=Quote then
        Quote:=#0;
      Result:=Result+S[I];
    end
    else if S[I] in ['''', '"', '`'] then
    begin
      Quote:=S[I];
      Result:=Result+S[I];
    end
    else if S[I]='?' then
    begin
      Result:=Result+':'+FParams[P].Name;
      Inc(P);
    end
    else
      Result:=Result+S[I];
  end;
  Result:='(' + Result + ')';
end;

function CountRawPlaceholders(const aSQL: UTF8String): Integer;
var
  I: Integer;
  Quote: Char;
begin
  Result:=0;
  Quote:=#0;
  for I:=1 to Length(aSQL) do
    if Quote<>#0 then
    begin
      if aSQL[I]=Quote then
        Quote:=#0;
    end
    else if aSQL[I] in ['''', '"', '`'] then
      Quote:=aSQL[I]
    else if aSQL[I]='?' then
      Inc(Result);
end;

function TOPCriteria.NodeToSQL(aIndex: Integer; aFieldMap: TOPFieldMapList): UTF8String;
var
  Node: TOPExprNode;
  ParamStart, ParamCount, I: Integer;
  S, FN: UTF8String;
begin
  Node:=FNodes[aIndex];
  case Node.Kind of
    nkComparison:
    begin
      FN:=ResolveFieldName(Node.Field, aFieldMap, Node.ParamIndex, 1);
      case Node.CompareOp of
        opEqual:
          Result:=FN + ' = :' + FParams[Node.ParamIndex].Name;
        opNotEqual:
          Result:=FN + ' <> :' + FParams[Node.ParamIndex].Name;
        opLess:
          Result:=FN + ' < :' + FParams[Node.ParamIndex].Name;
        opLessOrEqual:
          Result:=FN + ' <= :' + FParams[Node.ParamIndex].Name;
        opGreater:
          Result:=FN + ' > :' + FParams[Node.ParamIndex].Name;
        opGreaterOrEqual:
          Result:=FN + ' >= :' + FParams[Node.ParamIndex].Name;
        opLike:
          Result:=FN + ' LIKE :' + FParams[Node.ParamIndex].Name;
        opNotLike:
          Result:=FN + ' NOT LIKE :' + FParams[Node.ParamIndex].Name;
        opContains:
          begin
            // % and _ in the text match themselves
            if Assigned(FLikeEscaper) then
              FParams[Node.ParamIndex].AsString:='%'+FLikeEscaper(Node.Text)+'%'
            else
              FParams[Node.ParamIndex].AsString:='%'+OPFEscapeLikeText(Node.Text)+'%';
            Result:=FN + ' LIKE :' + FParams[Node.ParamIndex].Name + ' ESCAPE ''!''';
          end;
      end;
    end;
    nkLogical:
    begin
      case Node.LogicalOp of
        loAnd:
          Result:=ChildSQL(Node.LeftIndex, loAnd, aFieldMap) + ' AND ' + ChildSQL(Node.RightIndex, loAnd, aFieldMap);
        loOr:
          Result:=ChildSQL(Node.LeftIndex, loOr, aFieldMap) + ' OR ' + ChildSQL(Node.RightIndex, loOr, aFieldMap);
      end;
    end;
    nkNot:
      Result:='NOT (' + NodeToSQL(Node.NotChildIndex, aFieldMap) + ')';
    nkIsNull:
      Result:=ResolveFieldName(Node.Field, aFieldMap) + ' IS NULL';
    nkIsNotNull:
      Result:=ResolveFieldName(Node.Field, aFieldMap) + ' IS NOT NULL';
    nkIn:
    begin
      ParamStart:=Node.InParamStart;
      ParamCount:=Node.InParamCount;
      FN:=ResolveFieldName(Node.Field, aFieldMap, ParamStart, ParamCount);
      S:='';
      for I:=0 to ParamCount - 1 do
      begin
        if I > 0 then
          S:=S + ',';
        S:=S + ':' + FParams[ParamStart + I].Name;
      end;
      // IN () is invalid SQL: an empty list matches nothing
      if ParamCount=0 then
        Result:='1=0'
      else
        Result:=FN + ' IN (' + S + ')';
    end;
    nkNotIn:
    begin
      ParamStart:=Node.NotInParamStart;
      ParamCount:=Node.NotInParamCount;
      FN:=ResolveFieldName(Node.Field, aFieldMap, ParamStart, ParamCount);
      S:='';
      for I:=0 to ParamCount - 1 do
      begin
        if I > 0 then
          S:=S + ',';
        S:=S + ':' + FParams[ParamStart + I].Name;
      end;
      // NOT IN () is invalid SQL: an empty list excludes nothing
      if ParamCount=0 then
        Result:='1=1'
      else
        Result:=FN + ' NOT IN (' + S + ')';
    end;
    nkBetween:
    begin
      FN:=ResolveFieldName(Node.Field, aFieldMap, Node.BetweenLowParam, 1);
      ResolveFieldName(Node.Field, aFieldMap, Node.BetweenHighParam, 1);
      Result:=FN + ' BETWEEN :' + FParams[Node.BetweenLowParam].Name +
                ' AND :' + FParams[Node.BetweenHighParam].Name;
    end;
    nkGroup:
      Result:='(' + NodeToSQL(Node.GroupChildIndex, aFieldMap) + ')';
    nkRaw:
      Result:=RawToSQL(Node);
  end;
end;

procedure TOPCriteria.MergeSubCriteria(aSub: TOPCriteria);
var
  NodeOffset, ParamOffset: Integer;
  I: Integer;
  Node: TOPExprNode;
  P, SrcP: TParam;
begin
  NodeOffset:=Length(FNodes);
  ParamOffset:=FParams.Count;

  { Copy params (type, value and null state) with new names }
  for I:=0 to aSub.FParams.Count - 1 do
  begin
    SrcP:=aSub.FParams[I];
    P:=TParam(FParams.Add);
    P.Assign(SrcP);
    P.Name:=NextParamName;
  end;

  { Copy nodes, adjusting indices }
  for I:=0 to Length(aSub.FNodes) - 1 do
  begin
    Node:=aSub.FNodes[I];
    case Node.Kind of
      nkComparison:
        Node.ParamIndex:=Node.ParamIndex + ParamOffset;
      nkLogical:
      begin
        Node.LeftIndex:=Node.LeftIndex + NodeOffset;
        Node.RightIndex:=Node.RightIndex + NodeOffset;
      end;
      nkNot:
        Node.NotChildIndex:=Node.NotChildIndex + NodeOffset;
      nkIsNull,
      nkIsNotNull:
        ;
      nkIn:
        Node.InParamStart:=Node.InParamStart + ParamOffset;
      nkNotIn:
        Node.NotInParamStart:=Node.NotInParamStart + ParamOffset;
      nkBetween:
      begin
        Node.BetweenLowParam:=Node.BetweenLowParam + ParamOffset;
        Node.BetweenHighParam:=Node.BetweenHighParam + ParamOffset;
      end;
      nkGroup:
        Node.GroupChildIndex:=Node.GroupChildIndex + NodeOffset;
      nkRaw:
        Node.RawParamStart:=Node.RawParamStart + ParamOffset;
    end;
    AddNode(Node);
  end;
end;

{ Replace the conditions and their parameters by those of aSource: Where after
  Where replaces the conditions. Ordering, paging and IncludeDeleted are kept. }

procedure TOPCriteria.TakeExpression(aSource: TOPCriteria);
begin
  FNodes:=Copy(aSource.FNodes, 0, Length(aSource.FNodes));
  FParams.Assign(aSource.FParams);
  FParamCounter:=aSource.FParamCounter;
  FRootNode:=aSource.FRootNode;
end;

{ Raise if the paging of aSub conflicts with ours, before anything is changed }

procedure TOPCriteria.CheckSubSettings(aSub: TOPCriteria);
begin
  if (aSub.FLimitValue>=0) and (FLimitValue>=0) and (aSub.FLimitValue<>FLimitValue) then
    raise EOPF.CreateFmt(SErrCriteriaConflictingPaging, ['limit', aSub.FLimitValue, 'limit', FLimitValue]);
  if (aSub.FOffsetValue>=0) and (FOffsetValue>=0) and (aSub.FOffsetValue<>FOffsetValue) then
    raise EOPF.CreateFmt(SErrCriteriaConflictingPaging, ['offset', aSub.FOffsetValue, 'offset', FOffsetValue]);
end;

{ Take over the ordering, paging and IncludeDeleted of aSub:
  ordering is appended (fields already ordered on are skipped),
  IncludeDeleted is combined with OR, paging is taken when not set here. }

procedure TOPCriteria.MergeSubSettings(aSub: TOPCriteria);
var
  I, J, Len: Integer;
  Found: Boolean;
begin
  for I:=0 to High(aSub.FOrderBy) do
  begin
    Found:=False;
    for J:=0 to High(FOrderBy) do
      if SameText(FOrderBy[J].Field, aSub.FOrderBy[I].Field) then
        Found:=True;
    if not Found then
    begin
      Len:=Length(FOrderBy);
      SetLength(FOrderBy, Len+1);
      FOrderBy[Len]:=aSub.FOrderBy[I];
    end;
  end;
  FIncludeDeleted:=FIncludeDeleted or aSub.FIncludeDeleted;
  if FLimitValue<0 then
    FLimitValue:=aSub.FLimitValue;
  if FOffsetValue<0 then
    FOffsetValue:=aSub.FOffsetValue;
end;

{ Combine the conditions of aSub with aOp as a parenthesized group.
  An empty aSub adds no condition, but its settings are still taken over. }

function TOPCriteria.InternalAddSub(aSub: TOPCriteria; aOp: TOPLogicalOp; aTakeOwnership: Boolean): TOPCriteria;
var
  SubRootOffset, GroupIdx: Integer;
  Owner: TOPCriteria;
begin
  Result:=Self;
  if aSub=Nil then
    Exit;
  if aSub=Self then
    raise EOPF.Create(SErrCriteriaSelf);
  if aTakeOwnership then
  begin
    // One owner only: a second owner would free it twice, and an owner
    // cycle would recurse endlessly in the destructor
    if aSub.FOwner=Self then
      raise EOPF.Create(SErrCriteriaAlreadyOwned);
    if aSub.FOwner<>Nil then
      raise EOPF.Create(SErrCriteriaOwnedElsewhere);
    Owner:=FOwner;
    while Owner<>Nil do
    begin
      if Owner=aSub then
        raise EOPF.Create(SErrCriteriaOwnerCycle);
      Owner:=Owner.FOwner;
    end;
  end;
  // A valid transfer of ownership frees aSub when the merge fails, so that
  // AddAnd(TOPCriteria.Create...) never leaks
  try
    CheckSubSettings(aSub);
  except
    if aTakeOwnership then
      aSub.Free;
    raise;
  end;
  if aTakeOwnership then
  begin
    FOwnedCriteria.Add(aSub);
    aSub.FOwner:=Self;
  end;
  MergeSubSettings(aSub);
  if aSub.FRootNode<0 then
    Exit;
  SubRootOffset:=aSub.FRootNode + Length(FNodes);
  MergeSubCriteria(aSub);
  GroupIdx:=AddNode(MakeGroupNode(SubRootOffset));
  Result:=CombineWithLogical(aOp, GroupIdx);
end;

function TOPCriteria.InternalAddRaw(const aSQL: UTF8String; const aValues: array of const): Integer;
var
  I, ParamStart, Count: Integer;
  Node: TOPExprNode;
begin
  Count:=CountRawPlaceholders(aSQL);
  if Count<>Length(aValues) then
    raise EOPF.CreateFmt(SErrCriteriaRawParamCount, [aSQL, Count, Length(aValues)]);
  ParamStart:=FParams.Count;
  for I:=0 to High(aValues) do
    AddParamFromVarRec(aValues[I]);
  FillChar(Node{%H-}, SizeOf(Node), 0);
  Node.Kind:=nkRaw;
  Node.Field:=aSQL;
  Node.RawParamStart:=ParamStart;
  Node.RawParamCount:=Length(aValues);
  Result:=AddNode(Node);
end;

function TOPCriteria.WhereRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndRaw(aSQL, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.AddAndRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddRaw(aSQL, aValues));
end;

function TOPCriteria.AddOrRaw(const aSQL: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddRaw(aSQL, aValues));
end;

function TOPCriteria.CombineWithLogical(aOp: TOPLogicalOp; aRightNodeIndex: Integer): TOPCriteria;
var
  LogNode: Integer;
begin
  if FRootNode < 0 then
    FRootNode:=aRightNodeIndex
  else
  begin
    LogNode:=AddNode(MakeLogicalNode(aOp, FRootNode, aRightNodeIndex));
    FRootNode:=LogNode;
  end;
  Result:=Self;
end;

function TOPCriteria.InternalAddComparison(const aField: UTF8String; aOp: TOPCompareOp; aParamIdx: Integer): Integer;
var
  Node: TOPExprNode;
begin
  // LIKE needs text: a number or date would give invalid SQL
  if (aOp in [opLike, opNotLike, opContains]) and (FParams[aParamIdx].DataType<>ftString) then
  begin
    FParams[aParamIdx].Free;
    Dec(FParamCounter);
    raise EOPF.CreateFmt(SErrCriteriaLikeNeedsText, [aField]);
  end;
  Node:=MakeComparisonNode(aField, aOp, aParamIdx);
  if aOp=opContains then
    Node.Text:=FParams[aParamIdx].AsString;
  Result:=AddNode(Node);
end;

function TOPCriteria.InternalAddIn(const aField: UTF8String; aNotIn: Boolean; const aValues: array of const): Integer;
var
  I, ParamStart: Integer;
begin
  ParamStart:=FParams.Count;
  for I:=0 to High(aValues) do
    AddParamFromVarRec(aValues[I]);
  Result:=AddNode(MakeInNode(aField, aNotIn, ParamStart, Length(aValues)));
end;

function TOPCriteria.InternalAddInDateTime(const aField: UTF8String; aNotIn: Boolean; const aValues: array of TDateTime): Integer;
var
  I, ParamStart: Integer;
begin
  ParamStart:=FParams.Count;
  for I:=0 to High(aValues) do
    AddParamDateTime(aValues[I]);
  Result:=AddNode(MakeInNode(aField, aNotIn, ParamStart, Length(aValues)));
end;

{ --- Initial WHERE - typed overloads ---
  Each Where builds the new condition on an empty criteria first: when that
  raises, the conditions set before are kept (no unfiltered criteria). }

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.Where(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAnd(aField, aOp, aValue);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereIsNull(const aField: UTF8String): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndIsNull(aField);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereIsNotNull(const aField: UTF8String): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndIsNotNull(aField);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndIn(aField, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndNotIn(aField, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndBetween(aField, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndIn(aField, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndNotIn(aField, aValues);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

function TOPCriteria.WhereBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria;
var
  N: TOPCriteria;
begin
  N:=TOPCriteria.Create;
  try
    N.AddAndBetween(aField, aLow, aHigh);
    TakeExpression(N);
  finally
    N.Free;
  end;
  Result:=Self;
end;

{ --- AND composition - typed overloads --- }

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, AddParamInteger(aValue)));
end;

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, AddParamInt64(aValue)));
end;

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria;
var
  Idx: Integer;
begin
  // opContains: the escaped pattern is set when the SQL is generated
  Idx:=AddParamString(aValue);
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, Idx));
end;

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, AddParamFloat(aValue)));
end;

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, AddParamBoolean(aValue)));
end;

function TOPCriteria.AddAnd(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddComparison(aField, aOp, AddParamDateTime(aValue)));
end;

function TOPCriteria.AddAnd(aSub: TOPCriteria): TOPCriteria;
begin
  Result:=InternalAddSub(aSub, loAnd, True);
end;

function TOPCriteria.AddAndCopy(aSub: TOPCriteria): TOPCriteria;
begin
  Result:=InternalAddSub(aSub, loAnd, False);
end;

function TOPCriteria.AddAndIsNull(const aField: UTF8String): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, AddNode(MakeNullNode(aField, False)));
end;

function TOPCriteria.AddAndIsNotNull(const aField: UTF8String): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, AddNode(MakeNullNode(aField, True)));
end;

function TOPCriteria.AddAndIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddIn(aField, False, aValues));
end;

function TOPCriteria.AddAndNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddIn(aField, True, aValues));
end;

function TOPCriteria.AddAndBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria;
var
  LowIdx, HighIdx, NodeIdx: Integer;
begin
  if Length(aValues) <> 2 then
    raise EOPF.CreateFmt(SErrCriteriaBetweenCount, [Length(aValues)]);
  LowIdx:=AddParamFromVarRec(aValues[0]);
  HighIdx:=AddParamFromVarRec(aValues[1]);
  NodeIdx:=AddNode(MakeBetweenNode(aField, LowIdx, HighIdx));
  Result:=CombineWithLogical(loAnd, NodeIdx);
end;

function TOPCriteria.AddAndIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddInDateTime(aField, False, aValues));
end;

function TOPCriteria.AddAndNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loAnd, InternalAddInDateTime(aField, True, aValues));
end;

function TOPCriteria.AddAndBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria;
var
  LowIdx, HighIdx, NodeIdx: Integer;
begin
  LowIdx:=AddParamDateTime(aLow);
  HighIdx:=AddParamDateTime(aHigh);
  NodeIdx:=AddNode(MakeBetweenNode(aField, LowIdx, HighIdx));
  Result:=CombineWithLogical(loAnd, NodeIdx);
end;

{ --- OR composition - typed overloads --- }

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Integer): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, AddParamInteger(aValue)));
end;

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Int64): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, AddParamInt64(aValue)));
end;

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; const aValue: UTF8String): TOPCriteria;
var
  Idx: Integer;
begin
  // opContains: the escaped pattern is set when the SQL is generated
  Idx:=AddParamString(aValue);
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, Idx));
end;

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Double): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, AddParamFloat(aValue)));
end;

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: Boolean): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, AddParamBoolean(aValue)));
end;

function TOPCriteria.AddOr(const aField: UTF8String; aOp: TOPCompareOp; aValue: TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddComparison(aField, aOp, AddParamDateTime(aValue)));
end;

function TOPCriteria.AddOr(aSub: TOPCriteria): TOPCriteria;
begin
  Result:=InternalAddSub(aSub, loOr, True);
end;

function TOPCriteria.AddOrCopy(aSub: TOPCriteria): TOPCriteria;
begin
  Result:=InternalAddSub(aSub, loOr, False);
end;

function TOPCriteria.AddOrIsNull(const aField: UTF8String): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, AddNode(MakeNullNode(aField, False)));
end;

function TOPCriteria.AddOrIsNotNull(const aField: UTF8String): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, AddNode(MakeNullNode(aField, True)));
end;

function TOPCriteria.AddOrIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddIn(aField, False, aValues));
end;

function TOPCriteria.AddOrNotIn(const aField: UTF8String; const aValues: array of const): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddIn(aField, True, aValues));
end;

function TOPCriteria.AddOrBetween(const aField: UTF8String; const aValues: array of const): TOPCriteria;
var
  LowIdx, HighIdx, NodeIdx: Integer;
begin
  if Length(aValues) <> 2 then
    raise EOPF.CreateFmt(SErrCriteriaBetweenCount, [Length(aValues)]);
  LowIdx:=AddParamFromVarRec(aValues[0]);
  HighIdx:=AddParamFromVarRec(aValues[1]);
  NodeIdx:=AddNode(MakeBetweenNode(aField, LowIdx, HighIdx));
  Result:=CombineWithLogical(loOr, NodeIdx);
end;

function TOPCriteria.AddOrIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddInDateTime(aField, False, aValues));
end;

function TOPCriteria.AddOrNotIn(const aField: UTF8String; const aValues: array of TDateTime): TOPCriteria;
begin
  Result:=CombineWithLogical(loOr, InternalAddInDateTime(aField, True, aValues));
end;

function TOPCriteria.AddOrBetween(const aField: UTF8String; aLow, aHigh: TDateTime): TOPCriteria;
var
  LowIdx, HighIdx, NodeIdx: Integer;
begin
  LowIdx:=AddParamDateTime(aLow);
  HighIdx:=AddParamDateTime(aHigh);
  NodeIdx:=AddNode(MakeBetweenNode(aField, LowIdx, HighIdx));
  Result:=CombineWithLogical(loOr, NodeIdx);
end;

{ --- Negation --- }

function TOPCriteria.Not_: TOPCriteria;
begin
  if FRootNode >= 0 then
    FRootNode:=AddNode(MakeNotNode(FRootNode));
  Result:=Self;
end;

{ --- Soft-delete control --- }

function TOPCriteria.IncludeDeleted: TOPCriteria;
begin
  FIncludeDeleted:=True;
  Result:=Self;
end;

{ --- Ordering and pagination --- }

function TOPCriteria.OrderBy(const aField: UTF8String; aDescending: Boolean): TOPCriteria;
var
  Parts, Words: TStringArray;
  Item: TOPOrderByItem;
  S, Dir: String;
  Len: Integer;
begin
  S:=aField;
  Parts:=S.Split([',']);
  for S in Parts do
  begin
    Words:=Trim(S).Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    if (Length(Words)<1) or (Length(Words)>2) then
      raise EOPF.CreateFmt(SErrCriteriaInvalidOrderBy, [Trim(S)]);
    Item.Field:=Words[0];
    Item.Descending:=aDescending;
    if Length(Words)=2 then
    begin
      Dir:=UpperCase(Words[1]);
      if Dir='DESC' then
        Item.Descending:=True
      else if Dir='ASC' then
        Item.Descending:=False
      else
        raise EOPF.CreateFmt(SErrCriteriaInvalidOrderBy, [Trim(S)]);
    end;
    Len:=Length(FOrderBy);
    SetLength(FOrderBy, Len+1);
    FOrderBy[Len]:=Item;
  end;
  Result:=Self;
end;

function TOPCriteria.Clone: TOPCriteria;
var
  I: Integer;
  P: TParam;
begin
  Result:=TOPCriteria.Create;
  try
    Result.FNodes:=Copy(FNodes, 0, Length(FNodes));
    for I:=0 to FParams.Count-1 do
    begin
      P:=TParam(Result.FParams.Add);
      P.Assign(FParams[I]);
    end;
    Result.FParamCounter:=FParamCounter;
    Result.FRootNode:=FRootNode;
    Result.FOrderBy:=Copy(FOrderBy, 0, Length(FOrderBy));
    Result.FLimitValue:=FLimitValue;
    Result.FOffsetValue:=FOffsetValue;
    Result.FIncludeDeleted:=FIncludeDeleted;
  except
    Result.Free;
    raise;
  end;
end;

function TOPCriteria.Limit(aLimit: Integer): TOPCriteria;
begin
  if aLimit<-1 then
    raise EOPF.CreateFmt(SErrCriteriaInvalidPaging, ['limit', aLimit, 'limit']);
  FLimitValue:=aLimit;
  Result:=Self;
end;

function TOPCriteria.Offset(aOffset: Integer): TOPCriteria;
begin
  if aOffset<-1 then
    raise EOPF.CreateFmt(SErrCriteriaInvalidPaging, ['offset', aOffset, 'offset']);
  FOffsetValue:=aOffset;
  Result:=Self;
end;

{ --- SQL generation --- }

function TOPCriteria.ToWhereSQL(aFieldMap: TOPFieldMapList): UTF8String;
begin
  Result:=ToWhereSQL(aFieldMap, Nil);
end;

function TOPCriteria.ToWhereSQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter): UTF8String;
begin
  Result:=ToWhereSQL(aFieldMap, aFormatter, Nil);
end;

function TOPCriteria.ToWhereSQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter;
  aLikeEscaper: TOPLikeEscaper): UTF8String;
begin
  FFormatter:=aFormatter;
  FLikeEscaper:=aLikeEscaper;
  try
    if FRootNode >= 0 then
      Result:=NodeToSQL(FRootNode, aFieldMap)
    else
      Result:='';
  finally
    FFormatter:=Nil;
    FLikeEscaper:=Nil;
  end;
end;

function TOPCriteria.ToOrderBySQL(aFieldMap: TOPFieldMapList): UTF8String;
begin
  Result:=ToOrderBySQL(aFieldMap, Nil);
end;

function TOPCriteria.ToOrderBySQL(aFieldMap: TOPFieldMapList; aFormatter: TOPIdentifierFormatter): UTF8String;
var
  I: Integer;
begin
  Result:='';
  FFormatter:=aFormatter;
  try
    for I:=0 to Length(FOrderBy)-1 do
    begin
      if Result <> '' then
        Result:=Result + ', ';
      Result:=Result + ResolveFieldName(FOrderBy[I].Field, aFieldMap);
      if FOrderBy[I].Descending then
        Result:=Result + ' DESC';
    end;
  finally
    FFormatter:=Nil;
  end;
  if Result <> '' then
    Result:='ORDER BY ' + Result;
end;

function TOPCriteria.HasWhereClause: Boolean;
begin
  Result:=FRootNode >= 0;
end;

function TOPCriteria.HasIncludeDeleted: Boolean;
begin
  Result:=FIncludeDeleted;
end;

function TOPCriteria.GetLimitValue: Integer;
begin
  Result:=FLimitValue;
end;

function TOPCriteria.GetOffsetValue: Integer;
begin
  Result:=FOffsetValue;
end;

end.
