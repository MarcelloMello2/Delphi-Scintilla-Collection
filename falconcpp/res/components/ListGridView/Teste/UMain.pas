unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Grids, ListGridView, XPMan, DBGrids, ThemeDBGrid;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    XPManifest1: TXPManifest;
    StringGrid1: TStringGrid;
    ListGridView1: TListGridView;
    procedure FormCreate(Sender: TObject);
    procedure ListGridView1EditColumn(Sender: TObject; ACol, ARow: Integer;
      var CanEdit: Boolean; var EditType: TEditType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListGridView1.Cells[0, 0] := 'Caption';
  ListGridView1.Cells[1, 0] := 'Caption 2';
  ListGridView1.Cells[2, 0] := 'Caption 3';
  ListGridView1.Cells[3, 0] := 'Caption 4';
  ListGridView1.Cells[4, 0] := 'Caption 5';
end;

procedure TForm1.ListGridView1EditColumn(Sender: TObject; ACol,
  ARow: Integer; var CanEdit: Boolean; var EditType: TEditType);
begin
  CanEdit := not (ACol in [2,4]);
  if (ACol = 3) then
    EditType := etInteger;
  if (ACol = 1) then
    EditType := etCurrency;
end;

end.
