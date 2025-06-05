program Tests;

uses
  Vcl.Forms,
  Tests.MainForm in 'Tests.MainForm.pas' {Form2},
  Seven.Scintilla.BaseTextEditor in 'Seven.Scintilla.BaseTextEditor.pas',
  Seven.Scintilla.CustomTextEditor in 'Seven.Scintilla.CustomTextEditor.pas',
  Seven.Scintilla.Types in 'Seven.Scintilla.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
