program ScintillaFacerGenerator;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  ScintillaFacerGenerator.Face in 'ScintillaFacerGenerator.Face.pas',
  ScintillaFacerGenerator.FileGenerator in 'ScintillaFacerGenerator.FileGenerator.pas',
  ScintillaFacerGenerator.HFacer in 'ScintillaFacerGenerator.HFacer.pas',
  ScintillaFacerGenerator.ScintillaAPIFacer in 'ScintillaFacerGenerator.ScintillaAPIFacer.pas',
  ScintillaFacerGenerator.PascalGenerator in 'ScintillaFacerGenerator.PascalGenerator.pas';

begin
  try
    RunGenerator();
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      WriteLn('');
      WriteLn('Press ENTER to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  end;
end.
