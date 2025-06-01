program App2;

uses
  Forms,
  Umain2 in 'Umain2.pas' {FrmMain2};
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain2, FrmMain2);
  Application.Run;
end.
