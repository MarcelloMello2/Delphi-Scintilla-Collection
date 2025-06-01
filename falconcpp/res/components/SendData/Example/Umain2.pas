unit Umain2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SendData, XPMan, ExtDlgs,JPeg;

type
  infodata = packed record
    Name:string[255];
    Index:integer;
  end;

  TFrmMain2 = class(TForm)
    rgs: TRadioGroup;
    ednom: TEdit;
    lname: TLabel;
    btsend: TButton;
    SendData1: TSendData;
    XPManifest1: TXPManifest;
    Panel1: TPanel;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure SendData1CopyData(var Msg: TWMCopyData); message WM_CopyData;
    procedure btsendClick(Sender: TObject);
    procedure SendData1ReceivedRecord(Sender: TObject; Value: Pointer);
    procedure Image1DblClick(Sender: TObject);
    procedure SendData1ReceivedStream(Sender: TObject;
      Value: TMemoryStream);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain2: TFrmMain2;

implementation

{$R *.dfm}

procedure TFrmMain2.SendData1CopyData(var Msg: TWMCopyData);
begin
  SendData1.Action(Msg);
end;

procedure TFrmMain2.btsendClick(Sender: TObject);
var
  ifdata:infodata;
  imgs:TMemoryStream;
begin
  ifdata.Name := ednom.Text;
  ifdata.Index := rgs.ItemIndex;
  SendData1.SendRecord(SizeOf(ifdata),@ifdata);
  if Assigned(Image1.Picture) then
  begin
    imgs := TMemoryStream.Create;
    Image1.Picture.Bitmap.SaveToStream(imgs);
    SendData1.SendStream(imgs);
    imgs.Free;
  end;
end;

procedure TFrmMain2.SendData1ReceivedRecord(Sender: TObject;
  Value: Pointer);
var
  ifdata:infodata;
begin
  ifdata.Name := infodata(Value^).Name;
  ifdata.Index := infodata(Value^).Index;
  ednom.Text := ifdata.Name;
  rgs.ItemIndex := ifdata.Index;
end;

procedure TFrmMain2.Image1DblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    if FileExists(OpenPictureDialog1.FileName) then
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName)
    else
      Image1.Picture := nil;
end;

procedure TFrmMain2.SendData1ReceivedStream(Sender: TObject;
  Value: TMemoryStream);
begin
  Image1.Picture.Bitmap.LoadFromStream(Value);
end;

end.
