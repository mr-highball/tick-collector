unit collector;

{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils;

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
    procedure SetPollInterval(Const AValue: Cardinal);

    //properties
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
  strict private
    FPollInterval: Cardinal;
    FState: TTickState;
    function GetAvailableProducts: TStringArray;
    function GetCollectedProducts: TStringArray;
    function GetPollInterval: Cardinal;
    function GetState: TTickState;
    procedure SetPollInterval(Const AValue: Cardinal);
  strict protected
  public
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
  end;

implementation

{ TTickCollectorImpl }

function TTickCollectorImpl.GetAvailableProducts: TStringArray;
begin
  //todo
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
  //todo - add product id to a list if not exists
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Remove(const AProduct: String): ITickCollector;
begin
  //todo - if the product exists remove it, and then flush data
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Data(const AProduct: String;
  out Data: TStringArray): ITickCollector;
begin
  //todo - check if exists, if so return data, otherwise empty
  Result:=Self as ITickCollector;
end;

function TTickCollectorImpl.Flush(const AProduct: String): ITickCollector;
begin
  //todo - check if exists, then remove data
  Result:=Self as ITickCollector;
end;

procedure TTickCollectorImpl.Start;
begin
  if FState<>tsStopped then
    Exit;
  //todo - start the collector
end;

procedure TTickCollectorImpl.Stop;
begin
  //todo - stop the collector
end;

end.

