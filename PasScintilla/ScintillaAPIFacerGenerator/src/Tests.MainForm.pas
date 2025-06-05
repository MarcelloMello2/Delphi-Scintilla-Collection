unit Tests.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Seven.Scintilla.CustomTextEditor;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FScintillaTextEditor: TCustomSciTextEditor;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if Assigned(FScintillaTextEditor) then
    Exit;


  FScintillaTextEditor := TCustomSciTextEditor.Create(Self);
  FScintillaTextEditor.Align := alClient;
  FScintillaTextEditor.AlignWithMargins := True;
  FScintillaTextEditor.Visible := True;
  FScintillaTextEditor.Parent := Self;
end;

end.
