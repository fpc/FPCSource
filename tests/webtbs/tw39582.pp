unit tw39582;

{$mode Delphi}{$H+}

// примитивный множественный обрабочтчик событий, Pub/Sub.
//
// универсальный класс можно найти, например, в Spring4Delphi
// но там очень сложная библиотека с вуду типа генерации кода
// в рантайме по RTTI, также с расширенными контейнерными
// библиотеками и спользованием генериков так глубоко, что
// Delphi XE2 часто ломается на сборке

interface
uses Classes, SysUtils, Generics.Collections;

// события только типа procedure (Sender: TObject; ... params ...; var Cancel: boolean) of object
// другие типы не поддерживаются, тип не проверяется (или S4D со всеми наворотами)
// параметры событий хранятся внутри объекта во время цикла
// следовательно, никакой рекурсии и многопоточности
// процедуры вызова и передачи параметров придётся хардкодить и копипастить (или S4D с их вуду)

type
  TSimpleMultiEvent<P> = class;

  ISimpleMultiEvent<P> = interface
     procedure AddHandler(const Proc: P);
     procedure RemoveHandler(const Proc: P); overload;
     procedure RemoveHandler(const Obj:  TObject); overload;
     procedure RemoveHandler(const Method: TMethod); overload;

//     procedure RunEventOnBorrowedData(const RootEvent: TSimpleMultiEvent<P>);  -- FPC bug 39681
     procedure RunEventOnBorrowedData(const RootEvent: TObject);
  end;

  ESimpleMultiEvent = class(Exception) end;

  { TSimpleMultiEvent }

  TSimpleMultiEvent<P> = class(TInterfacedObject, ISimpleMultiEvent<P>)
  protected  // for debug tests if nothing better

     // 0 - default; +1, +2 - вложенные вызовы при запуске события; -1, -2 - при удалении/добавлении обработчиков
     MEState: integer;
     MEHandlers: TList<P>;

     type TPredicate = function(var Value; var Data): boolean of object;
  private
     function PredMeth(var Handler; var Data): Boolean;
     function PredObj (var Handler; var Data): Boolean;
     function PredType(var Handler; var Data): Boolean;
  protected

     // переменные события
     EV_Sender: TObject;
     EV_StopEventLoop: boolean;

     // цикл отработки событий после сохранения переменных;
     // возвращает количество вызванных обработчиков
     function InvokeAllHandlers: integer;

     // вызывает один обработчик, с нужными параметрами и сохраняет флаг остановки цикла
     procedure InvokeOneHandler(const Handler: P); virtual; abstract;

     // забирает данные из "корневого" события
     procedure BorrowDataFrom(const RootEvent: TSimpleMultiEvent<P>); virtual;

     // вызывается после обработчиков - зачистка ref-counted переменных
     procedure ClearEventVars; virtual;

     // дополнительные функции, например "показать часики" перед обработчиками
     // события и убрать после, или записать в журнал или еще что.
     // не должны бросать наружу исключения!
     procedure BeforeEvents; virtual;
     procedure AfterEvents; virtual;

     // FPC bug 39581, param should be TSimpleMultiEvent<P>
     procedure RunEventOnBorrowedData(const RootEvent: TObject);
  private
     procedure ClearAllHandlers;
     procedure InternalRemoveHandlers(const Where: TPredicate; var Data);

     procedure StartDoingEvent;
     procedure EndDoingEvent;
     procedure StartManagingHandlers;
     procedure EndManagingHandlers;
  public
     procedure AddHandler(const Proc: P);
     procedure RemoveHandler(const Proc: P); overload;
     procedure RemoveHandler(const Obj:  TObject); overload;
     procedure RemoveHandler(const Method: TMethod); overload;

     procedure BeforeDestruction; override;
     procedure AfterConstruction; override;

     var EventAfter, EventBefore: ISimpleMultiEvent<P>;
     // procedure RunEvent(Self, ......) - копипастить в каждом классе
  end;


  IMultiNotifyEvent = interface(ISimpleMultiEvent<TNotifyEvent>)
     function RunEvent(const Sender: TObject): integer;
  end;

  TMultiNotifyEvent = class (TSimpleMultiEvent<TNotifyEvent>, IMultiNotifyEvent)
  protected
    procedure InvokeOneHandler(const Handler: TNotifyEvent); override;
    function RunEvent(const Sender: TObject): integer;
  end;

  TSingleEvent = procedure (const NewValue: single) of object;

  IMultiSingleValEvent = interface(ISimpleMultiEvent<TSingleEvent>)
     function RunEvent(const NewVal: single): integer;
  end;

  { TMultiSingleValEvent }

  TMultiSingleValEvent = class (TSimpleMultiEvent<TSingleEvent>, IMultiSingleValEvent)
  private
    EV_Val: single;
  protected
    procedure InvokeOneHandler(const Handler: TSingleEvent); override;
    function RunEvent(const NewVal: single): integer;
  end;


implementation

type TMethodEq = record helper for TMethod
        function EqualTo(const M2: TMethod): boolean; inline;
     end;

function TMethodEq.EqualTo(const M2: TMethod): boolean;
begin
  Result := (Self.Data = M2.Data) and (Self.Code = M2.Code);
end;


{ TSimpleMultiEvent<P> }

// вызывается после сохранения всех параметров события
function TSimpleMultiEvent<P>.InvokeAllHandlers: integer;
var Handler: P;
begin
  StartDoingEvent;
  try
    Result := 0;
    EV_StopEventLoop := False;

    for Handler in MEHandlers do begin
       InvokeOneHandler(Handler);
       Inc(Result);
       if EV_StopEventLoop then break; // событие отработано, дальше не вызываем
    end;
  finally
    try
      EndDoingEvent;
    finally
      ClearEventVars;
    end;
  end;
end;

procedure TSimpleMultiEvent<P>.BorrowDataFrom(const RootEvent: TSimpleMultiEvent
  <P>);
begin
  Self.EV_Sender := RootEvent.EV_Sender;
end;

procedure TSimpleMultiEvent<P>.ClearEventVars;
begin
  EV_StopEventLoop := False; // готовимся к следующему циклу
  EV_Sender := nil;
end;

procedure TSimpleMultiEvent<P>.BeforeEvents;
begin
  // to be used by derived subclasses
end;

procedure TSimpleMultiEvent<P>.AfterEvents;
begin
  // to be used by derived subclasses

  if Assigned(EventAfter) then
     // EventAfter.Run
end;

procedure TSimpleMultiEvent<P>.RunEventOnBorrowedData(
  const RootEvent: TObject);
begin
  BorrowDataFrom(RootEvent as TSimpleMultiEvent<P>);
  InvokeAllHandlers;
end;

// Inc/Dec можно заменить на многопоточный System.SyncObjs.TInterlocked.Increment

procedure TSimpleMultiEvent<P>.StartDoingEvent;
begin
  // >0 тоже нельзя, переменные сохраняются - не реентерабельно!
  if MEState <> 0 then
     raise ESimpleMultiEvent.Create('Can not invoke ' + QualifiedClassName + ' while its handlers are changing.');

  Inc(MEState);

  BeforeEvents;
end;

procedure TSimpleMultiEvent<P>.EndDoingEvent;
begin
  if MEState > 0 then
     Dec(MEState);

  if MEState = 0 then
     AfterEvents;
end;

procedure TSimpleMultiEvent<P>.StartManagingHandlers;
begin
  if MEState > 0 then
     raise ESimpleMultiEvent.Create('Can not change handlers of ' + QualifiedClassName + ' while the event is running.');

  Dec(MEState);
end;

procedure TSimpleMultiEvent<P>.EndManagingHandlers;
begin
  if MEState < 0 then
     Inc(MEState);
end;

procedure TSimpleMultiEvent<P>.ClearAllHandlers;
begin
  StartManagingHandlers;
  try
    if MEHandlers <> nil then
       MEHandlers.Clear;
  finally
    EndManagingHandlers
  end;
end;

(*** возникает вопрос, как передать значение.
var M: TNotifyEvent absolute Proc; - проще всего и быстрее всего, но...
   [DCC Fatal Error] F2084 Internal Error: AV084D2278-W00000014-1

/*union*/  record case...  0:(Method: procedure of object); 1:(Arg: P); end;
   [DCC Error] E2569 Type parameter 'P' may need finalization - not allowed in variant record.  Consider using RECORD constraint

Приходится делать Move, и даже там "хардкодить" размер переменной :-/ ***)

procedure TSimpleMultiEvent<P>.AddHandler(const Proc: P);
var M: procedure of object; // const Sz = SizeOf(M) == 0 !!! expression типа результат вызова void-функции;
begin
  StartManagingHandlers;
  try
    M := nil;
    Move(Proc, M, SizeOf(TNotifyEvent));
    if Assigned(M) then
      MEHandlers.Add(Proc);
  finally
    EndManagingHandlers
  end;
end;

procedure TSimpleMultiEvent<P>.InternalRemoveHandlers(
  const Where: TPredicate; var Data);
var i: integer; H: P;
begin
  if MEHandlers = nil then exit;
  i := MEHandlers.Count;
  if i <= 0 then exit;

  StartManagingHandlers;
  try
    while i > 0 do begin
      Dec(i);
      H := MEHandlers[i];
      if Where(H, Data) then
         MEHandlers.Delete(i);
    end;
  finally
    EndManagingHandlers
  end;
end;

function TSimpleMultiEvent<P>.PredMeth(var Handler; var Data): Boolean;
var M: TMethod absolute Handler; //  - ICE on "absolute P"!
    N: TMethod absolute Data;
begin
//  Move(Handler, M, SizeOf(TNotifyEvent));
  Result := ( M.EqualTo( N ) );
end;

procedure TSimpleMultiEvent<P>.RemoveHandler(const Method: TMethod);
var LMethod: TMethod; // generic lambdas can not capture const-param record
begin
  StartManagingHandlers;
  try
    LMethod := Method;
    InternalRemoveHandlers(
      PredMeth, LMethod
    );
  finally
    EndManagingHandlers
  end;
end;

function TSimpleMultiEvent<P>.PredObj(var Handler; var Data): Boolean;
var M: ^TMethod;
    Obj: TObject absolute Data;
begin
  M := Pointer(@Handler);
  Result := M^.Data = Obj;
end;

procedure TSimpleMultiEvent<P>.RemoveHandler(const Obj: TObject);
var LObj: TObject;
begin
  StartManagingHandlers;
  try
    LObj := Obj;
    InternalRemoveHandlers(
      PredObj, LObj
    );
  finally
    EndManagingHandlers
  end;
end;

function TSimpleMultiEvent<P>.PredType(var Handler; var Data): Boolean;
var MH: TMethod absolute Handler;
    MD: TMethod absolute Data;
begin
//  Move(Handler, MH, SizeOf(TNotifyEvent));
  Result := MH.EqualTo(MD);
end;

procedure TSimpleMultiEvent<P>.RemoveHandler(const Proc: P);
var MP: TMethod; // generic lambdas can not capture const-param record
begin
  StartManagingHandlers;
  try
    MP.Code := nil;
    Move(Proc, MP, SizeOf(TNotifyEvent));
    InternalRemoveHandlers(
       PredType, MP
    );
  finally
    EndManagingHandlers
  end;
end;

procedure TSimpleMultiEvent<P>.BeforeDestruction;
begin
  Assert( MEState = 0, 'MultiEvent is busy, can not destroy!' );
  ClearAllHandlers;
  ClearEventVars;
  MEHandlers.Free;
  inherited;
end;

procedure TSimpleMultiEvent<P>.AfterConstruction;
begin
  MEHandlers := TList<P>.Create;
  MEHandlers.Capacity := 4;
  inherited;
end;

{ TMultiNotifyEvent }

procedure TMultiNotifyEvent.InvokeOneHandler(const Handler: TNotifyEvent);
begin
  Handler(EV_Sender);
end;

function TMultiNotifyEvent.RunEvent(const Sender: TObject): integer;
begin
  EV_Sender := Sender;
  Result := InvokeAllHandlers;
end;

{ TMultiSingleValEvent }

procedure TMultiSingleValEvent.InvokeOneHandler(const Handler: TSingleEvent);
begin
  Handler( EV_Val );
end;

function TMultiSingleValEvent.RunEvent(const NewVal: single): integer;
begin
  EV_Val := NewVal;
  Result := InvokeAllHandlers;
end;

{ TMethodEq }


(****
// а вот тут копи-паста для использования
type
  iMultiEventExample = interface(iSimpleMultiEvent<TNotifyEvent>)
    function RunEvent(const Sender: TObject): integer;
  end;

  TMultiEventExample = class(TSimpleMultiEvent<TNotifyEvent>, iMultiEventExample)
  public
    function RunEvent(const Sender: TObject): integer;
    procedure InvokeOneHandler(const Handler: TNotifyEvent); override;
  end;

//  // использовать - через интерфейс
//  THostingComponent = class ....
//  private MyEvent: iMultiEventExample;
//
//  // чтобы другие объекты могли подписываться
//  public property Event: iMultiEventExample read MyEvent;
//  end;
//
//  procedure THostingComponent.DoEvent; begin
//    MyEvent.RunEvent( Self, ....);
//  end.


{ TMultiEventExample }

function TMultiEventExample.RunEvent(const Sender: TObject): integer;
begin
  EV_Sender := Sender;

  Result := InvokeAllHandlers();
end;

procedure TMultiEventExample.InvokeOneHandler(const Handler: TNotifyEvent);
begin
  Handler(EV_Sender);
end;


  // ищем у подписантов строку типа Apple=20, возвращаем значение
type
  TSomeSearchEvent = procedure (const Sender: TObject; const Name: string; var Data: integer; Var Found_Stop_Now: boolean ) of object;

  iMEvExample2 = interface(iSimpleMultiEvent<TSomeSearchEvent>)
    function RunEvent(const Sender: TObject; const Name: string; var Data: integer): integer;
  end;

  TMEvExample2 = class(TSimpleMultiEvent<TSomeSearchEvent>, iMEvExample2)
  private
    EV_Data: ^integer;
    EV_Name: string;
  protected
    procedure ClearEventVars; override;
  public
    function RunEvent(const Sender: TObject; const Name: string; var Data: integer): integer;
    procedure InvokeOneHandler(const Handler: TSomeSearchEvent); override;
  end;

{ TMEvExample2 }

procedure TMEvExample2.InvokeOneHandler(const Handler: TSomeSearchEvent);
begin
  Handler( EV_Sender, EV_Name, EV_Data^, EV_StopEventLoop );
end;

procedure TMEvExample2.ClearEventVars;
begin
  EV_Name := '';  // освободить ARC-Объект
 //  EV_Data := nil; - перестраховка для отладки

  inherited;
end;

function TMEvExample2.RunEvent(const Sender: TObject; const Name: string;
  var Data: integer): integer;
begin
  EV_Sender := Sender;
  EV_Data := @Data;
  EV_Name := Name;

  Result := InvokeAllHandlers();
end;

(****

***)

end.

