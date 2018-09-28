unit collector;

{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, ezthreads, gdax.api.types, fgl;

type

  TTickState = (tsStopped,tsStarted);

  { ITickCollector }
  (*
    ITickCollector is responsible for managing what products
    are collected during a poll and holding the raw data
  *)
  ITickCollector = interface
    ['{0D59189E-5244-49DD-A3F7-0C417DD1ED83}']
    //property methods
    function GetAvailableProducts: TStringArray;
    function GetCollectedProducts: TStringArray;
    function GetPollInterval: Cardinal;
    function GetState: TTickState;
    function GetAuth: IGDAXAuthenticator;
    procedure SetAuth(Const AValue: IGDAXAuthenticator);
    procedure SetPollInterval(Const AValue: Cardinal);

    //properties
    property Authenticator : IGDAXAuthenticator read GetAuth write SetAuth;
    property PollInterval : Cardinal read GetPollInterval write SetPollInterval;
    property AvailableProducts : TStringArray read GetAvailableProducts;
    property CollectedProducts : TStringArray read GetCollectedProducts;
    property State : TTickState read GetState;

    //methods
    function UpdatePollInterval(Const AInterval:Cardinal) : ITickCollector;
    function Collect(Const AProduct:String) : ITickCollector;
    function Remove(Const AProduct:String) : ITickCollector;
    function Data(Const AProduct:String;Out Data:TStringArray):ITickCollector;
    function Flush(Const AProduct:String=''):ITickCollector;
    procedure Start;
    procedure Stop;
  end;

  { TTickCollectorImpl }
  (*
    base tick collector implementation
  *)
  TTickCollectorImpl = class(TInterfacedObject,ITickCollector)
  protected
    type
      TDataMap = TFPGMapObject<String,TStringList>;
  strict private
    FMap: TDataMap;
    FPollInterval: Cardinal;
    FState: TTickState;
    FAuth: IGDAXAuthenticator;
    FProducts: IGDAXProducts;
    FCollected: TStringList;
    FThread: IEZThread;
    FCanStart,
    FRequestedStop: Boolean;
    function GetAuth: IGDAXAuthenticator;
    function GetAvailableProducts: TStringArray;
    function GetCollectedProducts: TStringArray;
    function GetPollInterval: Cardinal;
    function GetState: TTickState;
    procedure SetAuth(Const AValue: IGDAXAuthenticator);
    procedure SetPollInterval(Const AValue: Cardinal);
    procedure CollectMethod(Const AThread:IEZThread);
    procedure StopCollect(Const AThread:IEZThread);
    procedure Failure(Const AThread:IEZThread);
  strict protected
  public
    property Authenticator : IGDAXAuthenticator read GetAuth write SetAuth;
    property PollInterval : Cardinal read GetPollInterval write SetPollInterval;
    property AvailableProducts : TStringArray read GetAvailableProducts;
    property CollectedProducts : TStringArray read GetCollectedProducts;
    property State : TTickState read GetState;

    function UpdatePollInterval(Const AInterval:Cardinal) : ITickCollector;
    function Collect(Const AProduct:String) : ITickCollector;
    function Remove(Const AProduct:String) : ITickCollector;
    function Data(Const AProduct:String;Out Data:TStringArray):ITickCollector;
    function Flush(Const AProduct:String=''):ITickCollector;
    procedure Start;
    procedure Stop;
    constructor Create;virtual;
    destructor Destroy; override;
  end;

implementation
uses
  syncobjs, gdax.api.authenticator, gdax.api.products, gdax.api.ticker,
  dateutils;
var
  Critical : TCriticalSection;

{ TTickCollectorImpl }

function TTickCollectorImpl.GetAvailableProducts: TStringArray;
var
  LContent,
  LError:String;
  I:Integer;
begin
  //auth may have changed since last time calling this, reset every time
  FProducts.Authenticator:=FAuth;

  //for now just raise the exception, may want to handle this better
  if not FProducts.Get(LContent,LError) then
    raise Exception.Create(LError);

  //set length to the count of products we have
  SetLength(Result,FProducts.Products.Count);

  //store ids in result
  for I:=0 to Pred(FProducts.Products.Count) do
    Result[I]:=FProducts.Products[0].ID;
end;

function TTickCollectorImpl.GetAuth: IGDAXAuthenticator;
begin
  Result:=FAuth;
end;

function TTickCollectorImpl.GetCollectedProducts: TStringArray;
var
  I:Integer;
begin
  Critical.Enter;
  try
    SetLength(Result,FCollected.Count);
    for I:=0 to Pred(FCollected.Count) do
      Result[I]:=FCollected[I];
  finally
    Critical.Leave;
  end;
end;

function TTickCollectorImpl.GetPollInterval: Cardinal;
begin
  Result:=FPollInterval;
end;

function TTickCollectorImpl.GetState: TTickState;
begin
  Result:=FState;
end;

procedure TTickCollectorImpl.SetAuth(const AValue: IGDAXAuthenticator);
begin
  FAuth:=nil;
  FAuth:=AValue;
end;

procedure TTickCollectorImpl.SetPollInterval(const AValue: Cardinal);
begin
  if State<>tsStopped then
    raise Exception.Create('stop tick collector first');
  FPollInterval:=AValue;
end;

procedure TTickCollectorImpl.CollectMethod(const AThread: IEZThread);
var
  LStart:TDateTime;
  LTicker:IGDAXTicker;
  LProduct:IGDAXProduct;
  LProducts:TStringArray;
  I:Integer;
  LList:TStringList;
  LContent,
  LError: String;
  LDiff: Cardinal;
begin
  //setup the product/ticker
  LProduct:=TGDAXProductImpl.Create;
  LProduct.Authenticator:=FAuth;
  LTicker:=TGDAXTickerImpl.Create;
  LTicker.Product:=LProduct;
  LTicker.Authenticator:=FAuth;

  //one time fetch the products we are collecting
  LProducts:=CollectedProducts;
  while not FRequestedStop do
  begin
    //get start time so we know how long to sleep
    LStart:=Now;

    //iterate the product ids and fetch ticker information for each one
    for I:=0 to High(LProducts) do
    begin
      try
        //assign current id to product
        LProduct.ID:=LProducts[I];

        //attempt to get ticker info
        if LTicker.Get(LContent,LError) then
        begin
          //if success, store the content in the map
          LList:=FMap.Data[FMap.IndexOf(LProducts[I])];

          //aquire lock before recording content
          Critical.Enter;
          try
            LList.Add(LContent);
          finally
            Critical.Leave;
          end;
        end;
      except on E:Exception do
        //todo - handle exception/log
      end;
    end;

    if PollInterval > 0 then
    begin
      //find the difference between start of polling and now
      LDiff:=MilliSecondsBetween(Now,LStart);
      if LDiff >= FPollInterval then
        Continue;

      //sleep remainder
      Sleep(FPollInterval - LDiff);
    end;
  end;
end;

procedure TTickCollectorImpl.StopCollect(const AThread: IEZThread);
begin
  FState:=tsStopped;
end;

procedure TTickCollectorImpl.Failure(const AThread: IEZThread);
begin
  FCanStart:=True;
  FRequestedStop:=False;
  FState:=tsStopped;
end;

function TTickCollectorImpl.UpdatePollInterval(const AInterval: Cardinal): ITickCollector;
begin
  SetPollInterval(AInterval);
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Collect(const AProduct: String): ITickCollector;
begin
  if FCollected.IndexOf(AProduct) < 0 then
  begin
    Critical.Enter;
    try
      //add the product to collected, as well as the map for data
      FCollected.Add(AProduct);
      FMap.Add(AProduct,TStringList.Create);
    finally
      Critical.Leave;
    end;
  end;
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Remove(const AProduct: String): ITickCollector;
var
  I:Integer;
begin
  //if the product exists remove it, and then flush data
  I:=FCollected.IndexOf(AProduct);
  if I < 0 then
  begin
    Critical.Enter;
    try
      FCollected.Delete(I);
      FMap.Remove(AProduct);
    finally
      Critical.Leave;
    end;
  end;
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Data(const AProduct: String;
  out Data: TStringArray): ITickCollector;
begin
  if FCollected.IndexOf(AProduct) >= 0 then
    Data:=FMap.Data[FMap.IndexOf(AProduct)].Text.Split(sLineBreak);
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Flush(const AProduct: String): ITickCollector;
var
  I:Integer;
begin
  Critical.Enter;
  try
    I:=FMap.IndexOf(AProduct);

    //clear the data without freeing object
    if I >= 0 then
      FMap.Data[I].Clear;
  finally
    Critical.Leave;
  end;

  Result:=Self as ITickCollector;
end;

procedure TTickCollectorImpl.Start;
begin
  if (FState<>tsStopped) or ((FState=tsStopped) and not FCanStart) then
    Exit;
  if not Assigned(FAuth) then
    raise Exception.Create('please assign a valid authenticator');
  if FAuth.Key.IsEmpty or FAuth.Passphrase.IsEmpty or FAuth.Secret.IsEmpty then
    raise Exception.Create('secret/passphrase/key are required in the authenticator');

  FThread
    .Setup(CollectMethod,Failure,nil)
    .Events
      .UpdateOnStop(StopCollect)
      .Thread
    .Start;
  FCanStart:=False;
end;

procedure TTickCollectorImpl.Stop;
begin
  FRequestedStop:=True;

  //block until thread is stopped
  while FState=tsStarted do
    Continue;

  FCanStart:=True;
end;

constructor TTickCollectorImpl.Create;
begin
  FAuth:=TGDAXAuthenticatorImpl.Create;
  FProducts:=TGDAXProductsImpl.Create;
  FCollected:=TStringList.Create;
  FMap:=TDataMap.Create(True);
  FThread:=TEZThreadImpl.Create;
  FCanStart:=True;
  FRequestedStop:=False;
end;

destructor TTickCollectorImpl.Destroy;
begin
  if State=tsStarted then
    Stop;
  FAuth:=nil;
  FProducts:=nil;
  FCollected.Free;
  FMap.Free;
  FThread:=nil;
  inherited Destroy;
end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  Critical.Free;
end.

