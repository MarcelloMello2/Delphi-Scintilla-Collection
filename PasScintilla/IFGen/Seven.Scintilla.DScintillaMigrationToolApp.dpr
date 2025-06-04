program Seven.Scintilla.DScintillaMigrationToolApp;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, DScintillaMigration;

procedure ShowUsage;
begin
  WriteLn('DScintilla Migration Tool');
  WriteLn('========================');
  WriteLn('');
  WriteLn('Uso: DScintillaMigrationTool <wrapper_antigo> <wrapper_novo> [opcoes]');
  WriteLn('');
  WriteLn('Opcoes:');
  WriteLn('  -o <dir>    Diretório de saída (padrão: diretório atual)');
  WriteLn('  -compat     Gera unit de compatibilidade');
  WriteLn('  -report     Gera relatório detalhado');
  WriteLn('  -all        Gera todos os arquivos');
  WriteLn('');
  WriteLn('Exemplo:');
  WriteLn('  DScintillaMigrationTool DScintilla_old.pas DScintilla_new.pas -all');
end;

procedure RunMigration(const OldFile, NewFile: string;
  GenerateCompat, GenerateReport: Boolean; const OutputDir: string);
var
  MigrationHelper: TDScintillaMigrationHelper;
  CompatFile, ReportFile: string;
begin
  if not FileExists(OldFile) then
  begin
    WriteLn('ERRO: Arquivo antigo não encontrado: ', OldFile);
    Exit;
  end;

  if not FileExists(NewFile) then
  begin
    WriteLn('ERRO: Arquivo novo não encontrado: ', NewFile);
    Exit;
  end;

  MigrationHelper := TDScintillaMigrationHelper.Create;
  try
    WriteLn('Iniciando análise de migração...');
    WriteLn('');

    // Analisa o wrapper antigo
    MigrationHelper.AnalyzeExistingWrapper(OldFile);

    // Analisa o wrapper novo
    MigrationHelper.AnalyzeNewWrapper(NewFile);

    WriteLn('');
    WriteLn('Análise concluída!');
    WriteLn('');

    // Gera relatório de migração
    if GenerateReport then
    begin
      ReportFile := IncludeTrailingPathDelimiter(OutputDir) + 'MigrationReport.txt';
      WriteLn('Gerando relatório de migração...');
      MigrationHelper.GenerateMigrationReport(ReportFile);
    end;

    // Gera unit de compatibilidade
    if GenerateCompat then
    begin
      CompatFile := IncludeTrailingPathDelimiter(OutputDir) + 'DScintillaCompat.pas';
      WriteLn('Gerando unit de compatibilidade...');
      MigrationHelper.GenerateCompatibilityUnit(CompatFile);
    end;

    WriteLn('');
    WriteLn('Processo de migração concluído!');

    if GenerateCompat then
    begin
      WriteLn('');
      WriteLn('Para usar a compatibilidade em seu projeto:');
      WriteLn('1. Adicione DScintillaCompat à cláusula uses');
      WriteLn('2. Substitua TScintilla por TDScintillaCompat');
      WriteLn('3. Compile e teste sua aplicação');
      WriteLn('4. Verifique o relatório para atualizar código obsoleto');
    end;
  finally
    MigrationHelper.Free;
  end;
end;

// Programa principal
var
  OldFile, NewFile, OutputDir: string;
  GenerateCompat, GenerateReport: Boolean;
  I: Integer;
begin
  if ParamCount < 2 then
  begin
    ShowUsage;
    Exit;
  end;

  // Parâmetros obrigatórios
  OldFile := ParamStr(1);
  NewFile := ParamStr(2);

  // Valores padrão
  OutputDir := ExtractFilePath(ParamStr(0));
  GenerateCompat := False;
  GenerateReport := False;

  // Processa opções
  I := 3;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '-o' then
    begin
      Inc(I);
      if I <= ParamCount then
        OutputDir := ParamStr(I);
    end
    else if ParamStr(I) = '-compat' then
      GenerateCompat := True
    else if ParamStr(I) = '-report' then
      GenerateReport := True
    else if ParamStr(I) = '-all' then
    begin
      GenerateCompat := True;
      GenerateReport := True;
    end;

    Inc(I);
  end;

  // Executa a migração
  try
    RunMigration(OldFile, NewFile, GenerateCompat, GenerateReport, OutputDir);
  except
    on E: Exception do
    begin
      WriteLn('ERRO: ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.
