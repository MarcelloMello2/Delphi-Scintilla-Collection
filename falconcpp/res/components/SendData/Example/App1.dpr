program App1;

uses
  Forms,
  Umain in 'Umain.pas' {FrmMain1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain1, FrmMain1);
  Application.Run;
end.
