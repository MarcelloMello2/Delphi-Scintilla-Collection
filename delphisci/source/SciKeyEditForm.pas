//CE_Desc_Include(helpdescriptions.txt)
{
 Adopted from the DelphiWrapper at http://www.pnotepad.com/scintilla
		 $Id: SciKeyEditForm.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $
 History:   29/09/2004 Initial Release with Delphi Scintilla Interface Components
            13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                       generate the help.
}
unit SciKeyEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls, ExtCtrls, Buttons;

type

  TKeyEditForm = class(TForm)
    pnlAlign: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cmbCommand: TComboBox;
    HotKey: THotKey;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
{$R *.dfm}

Uses
  SciKeyBindings;

procedure TKeyEditForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
  cmbCommand.Clear;
  cmbCommand.Items.BeginUpdate;
  try
    for i := 0 to High(Sci_KeyboardCommandMap) do
      cmbCommand.AddItem(Sci_KeyboardCommandMap[i].Name, TObject(Sci_KeyboardCommandMap[i].Value));
    cmbCommand.ItemIndex := -1;
  finally
    cmbCommand.Items.EndUpdate;
  end;
end;

end.
