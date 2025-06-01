unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IconDialog, StdCtrls, ExtCtrls, XPMan;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Button1: TButton;
    IconDialog1: TIconDialog;
    XPManifest1: TXPManifest;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if IconDialog1.Execute then
  begin
    Image1.Picture := nil;
    Edit1.Text := IconDialog1.FileName;
    Edit2.Text := IntToStr(IconDialog1.IconIndex);
    if ExtractFileExt(IconDialog1.FileName) = '.ico' then
      Image1.Picture.LoadFromFile(IconDialog1.FileName)
    else
      Image1.Picture.Icon := IconDialog1.Icon;
  end;
end;

end.
