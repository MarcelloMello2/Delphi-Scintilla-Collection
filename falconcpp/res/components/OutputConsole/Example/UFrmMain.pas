unit UFrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, Menus, CommandConsole, OutputConsole;

type
  TFrmMain = class(TForm)
    MemoOut: TMemo;
    EditFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditParams: TEdit;
    BtnStart: TButton;
    BtnStop: TButton;
    XPManifest1: TXPManifest;
    PopupMenu1: TPopupMenu;
    Clear1: TMenuItem;
    Label3: TLabel;
    EditPath: TEdit;
    OutputConsole: TOutputConsole;
    procedure OutputConsoleFinish(Sender: TObject; FileName,
      Params: String; ConsoleOut: TStrings; ExitCode: Integer);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure OutputConsoleStart(Sender: TObject; const FileName,
      Params: String);
    procedure OutputConsoleProgress(Sender: TObject; const FileName, Params,
      OutLine: String);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.OutputConsoleFinish(Sender: TObject; FileName,
  Params: String; ConsoleOut: TStrings; ExitCode: Integer);
begin
  BtnStart.Enabled := True;
  BtnStop.Enabled := False;
  Caption := Format('Process terminated ExitCode: %d', [ExitCode]);
end;

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  Caption := 'Output Console';
  OutputConsole.FileName := EditFileName.Text;
  OutputConsole.Params := EditParams.Text;
  if Length(EditPath.Text) = 0 then
    EditPath.Text := ExtractFilePath(Application.ExeName);

  OutputConsole.Directory := EditPath.Text;
  OutputConsole.Start;
end;

procedure TFrmMain.BtnStopClick(Sender: TObject);
begin
  OutputConsole.Stop;
end;

procedure TFrmMain.Clear1Click(Sender: TObject);
begin
  MemoOut.Clear;
end;

procedure TFrmMain.OutputConsoleStart(Sender: TObject; const FileName,
  Params: String);
begin
  BtnStart.Enabled := False;
  BtnStop.Enabled := True;
  MemoOut.Clear;
end;

procedure TFrmMain.OutputConsoleProgress(Sender: TObject; const FileName,
  Params, OutLine: String);
begin
  if Pos('', OutLine) > 0 then
    MemoOut.Clear
  else
    MemoOut.Text := MemoOut.Text + OutLine;
  MemoOut.SelStart := Length(MemoOut.Text);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  BtnStart.Click;
end;

end.
