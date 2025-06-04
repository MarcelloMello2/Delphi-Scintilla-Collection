program Seven.Scintilla.IfaceGeneratorApp;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  SysUtils,
  Classes,
  StrUtils,
  Seven.Scintilla.IfaceGenerator in 'Seven.Scintilla.IfaceGenerator.pas';

// Programa principal
var
  Generator: TScintillaGenerator;
  InputFile, OutputFile: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Uso: ScintillaIfaceGenerator <arquivo.iface> [arquivo_saida.pas]');
    WriteLn('');
    WriteLn('Exemplo: ScintillaIfaceGenerator Scintilla.iface ScintillaWrapper.pas');
    Exit;
  end;

  InputFile := ParamStr(1);
  if not FileExists(InputFile) then
  begin
    WriteLn('Erro: Arquivo não encontrado: ' + InputFile);
    Exit;
  end;

  if ParamCount >= 2 then
    OutputFile := ParamStr(2)
  else
    OutputFile := 'ScintillaWrapper.pas';

  Generator := TScintillaGenerator.Create;
  try
    WriteLn('Scintilla Interface Generator v1.0');
    WriteLn('===================================');
    WriteLn('');
    WriteLn('Parseando arquivo: ' + InputFile);
    Generator.ParseIfaceFile(InputFile);

    WriteLn('Gerando unit Delphi...');
    Generator.GenerateDelphiUnit(OutputFile);

    WriteLn('');
    WriteLn('Concluído com sucesso!');
  finally
    Generator.Free;
  end;

  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.
