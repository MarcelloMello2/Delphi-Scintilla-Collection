program Seven.Scintilla.Tests;

uses
  Vcl.Forms,
  Seven.Scintilla.Tests.MainForm in 'Seven.Scintilla.Tests.MainForm.pas' {Form1},
  Seven.Scintilla.Types in 'Seven.Scintilla.Types.pas',
  Seven.Scintilla.BaseTextEditor in 'Seven.Scintilla.BaseTextEditor.pas',
  Seven.Scintilla.CustomTextEditor in 'Seven.Scintilla.CustomTextEditor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
