unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, CheckLst, collector;

type

  { TMainForm }
  (*
    main form used to select products for the tick collector and to
    configure any additional settings
  *)
  TMainForm = class(TForm)
    btn_multi: TButton;
    chk_list_products: TCheckListBox;
    lbl_list_products: TLabel;
    pctrl_main: TPageControl;
    scroll_products: TScrollBox;
    line_list_products: TShape;
    ts_products: TTabSheet;
    ts_auth: TTabSheet;
    procedure btn_multiClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    const
      BTN_MULTI_START = 'Start';
      BTN_MULTI_STOP = 'Stop';
  private
    FCollector: ITickCollector;
    procedure MultiAction;
    procedure CheckValidAuth;
  public
    procedure StartCollector;
    procedure StopCollector;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCollector:=TTickCollectorImpl.Create;
  StopCollector;
  CheckValidAuth;
end;

procedure TMainForm.btn_multiClick(Sender: TObject);
begin
  MultiAction;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCollector:=nil;
end;

procedure TMainForm.MultiAction;
begin
  if FCollector.State = tsStarted then
    StopCollector
  else
    StartCollector;
end;

procedure TMainForm.CheckValidAuth;
begin
  //todo - checks to see if we have valid auth info if not switches focus
end;

procedure TMainForm.StartCollector;
begin
  //if FCollector.State = tsStopped then
  //  FCollector.Start;
  btn_multi.Caption:=BTN_MULTI_START;
  chk_list_products.Enabled:=False;
end;

procedure TMainForm.StopCollector;
begin
  if FCollector.State = tsStarted then
    FCollector.Stop;
  btn_multi.Caption:=BTN_MULTI_STOP;
  chk_list_products.Enabled:=True;
end;

end.

