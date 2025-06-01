program Test;

uses
  Forms,
  UFrmTest in 'UFrmTest.pas' {FrmTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTest, FrmTest);
  Application.Run;
end.
