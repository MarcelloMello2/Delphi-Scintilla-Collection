program SplitterPanel;

uses
  Forms,
  UMain in 'UMain.pas' {FormSplitterPanel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSplitterPanel, FormSplitterPanel);
  Application.Run;
end.
