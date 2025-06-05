unit Tests.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Seven.Scintilla.CustomTextEditor;

type

  TCustomSciTextEditor = class(Seven.Scintilla.CustomTextEditor.TCustomSciTextEditor)
  public
    constructor Create(AOwner: TComponent); override;
    property ViewEOL;
    property ViewWS;
    property TabDrawMode;
    property ControlCharSymbol;
  end;

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
  FScintillaTextEditor.ViewEOL := True;
  FScintillaTextEditor.ControlCharSymbol := 1;
end;

{ TCustomSciTextEditor }

constructor TCustomSciTextEditor.Create(AOwner: TComponent);
begin
  inherited;
end;

end.
