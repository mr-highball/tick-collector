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
    function GetAuth: IGDAXAuthenticator;
    function GetAvailableProducts: TStringArray;
    function GetCollectedProducts: TStringArray;
    function GetPollInterval: Cardinal;
    function GetState: TTickState;
    procedure SetAuth(Const AValue: IGDAXAuthenticator);
    procedure SetPollInterval(Const AValue: Cardinal);
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
  syncobjs, gdax.api.authenticator, gdax.api.products;
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
begin
  //todo
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
  if FState<>tsStopped then
    Exit;
  if not Assigned(FAuth) then
    raise Exception.Create('please assign a valid authenticator');
  if FAuth.Key.IsEmpty or FAuth.Passphrase.IsEmpty or FAuth.Secret.IsEmpty then
    raise Exception.Create('secret/passphrase/key are required in the authenticator');
  //todo - start the collector
end;

procedure TTickCollectorImpl.Stop;
begin
  //todo - stop the collector
end;

constructor TTickCollectorImpl.Create;
begin
  FAuth:=TGDAXAuthenticatorImpl.Create;
  FProducts:=TGDAXProductsImpl.Create;
  FCollected:=TStringList.Create;
  FMap:=TDataMap.Create(True);
end;

destructor TTickCollectorImpl.Destroy;
begin
  FAuth:=nil;
  FProducts:=nil;
  FCollected.Free;
  FMap.Free;
  inherited Destroy;
end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  Critical.Free;
end.

