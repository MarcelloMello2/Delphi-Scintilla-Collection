unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPPanels,  ComCtrls, XPMan, ExtCtrls, ModernTabs, StdCtrls,
  ImgList;

type
  TFormSplitterPanel = class(TForm)
    XPManifest1: TXPManifest;
    ImageList1: TImageList;
    ModernPageControl1: TModernPageControl;
    Panel1: TPanel;
    Button2: TButton;
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ModernPageControl1PageChange(Sender: TObject;
      TabIndex: Integer);
    procedure ModernPageControl1Close(Sender: TObject; TabIndex: Integer;
      var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSplitterPanel: TFormSplitterPanel;

implementation

{$R *.dfm}

procedure TFormSplitterPanel.Button4Click(Sender: TObject);
begin
  ModernPageControl1.SelectNextPage(False);
end;

procedure TFormSplitterPanel.Button3Click(Sender: TObject);
begin
  ModernPageControl1.SelectNextPage(True);
end;

procedure TFormSplitterPanel.Button5Click(Sender: TObject);
begin
  ModernPageControl1.ActivePage := nil;
end;

procedure TFormSplitterPanel.ModernPageControl1PageChange(Sender: TObject;
  TabIndex: Integer);
begin
  Beep;
  Caption := 'PageChangeIndex: ' + IntToStr(TabIndex);
end;

procedure TFormSplitterPanel.ModernPageControl1Close(Sender: TObject;
  TabIndex: Integer; var CanClose: Boolean);
begin
  MessageBeep(64);
  CanClose := True;
  if TabIndex = 0 then
    Close;
end;

procedure TFormSplitterPanel.Button2Click(Sender: TObject);
begin
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Nova aba ' + IntToStr(ModernPageControl1.PageCount + 1);
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
end;

procedure TFormSplitterPanel.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormSplitterPanel.FormCreate(Sender: TObject);
begin
  ModernPageControl1.LockAnimation;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Basic';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Audio';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Documment';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Internet';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Multimedia';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Database';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  with TModernTabSheet.Create(ModernPageControl1) do
  begin
    Caption := 'Compression';
    PageControl := ModernPageControl1;
    ModernPageControl1.ActivePageIndex := PageIndex;
  end;
  ModernPageControl1.TabIndex := 0;
  ModernPageControl1.UnlockAnimation;
end;

end.
