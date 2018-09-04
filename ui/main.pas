unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  collector;

type

  { TMainForm }
  (*
    main form used to select products for the tick collector and to
    configure any additional settings
  *)
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCollector: ITickCollector;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCollector:=TTickCollectorImpl.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCollector:=nil;
end;

end.

