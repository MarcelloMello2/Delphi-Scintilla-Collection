﻿program Seven.Scintilla.CompleteDScintillaMigrationApp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  Windows,
  ShellAPI;

const
  VERSION = '1.0.0';

type
  TMigrationConfig = record
    ScintillaIfaceFile: string;
    OldWrapperFile: string;
    OutputDir: string;
    BackupOldWrapper: Boolean;
    GenerateNewWrapper: Boolean;
    GenerateCompatLayer: Boolean;
    GenerateReport: Boolean;
    GenerateTests: Boolean;
    CompileDemo: Boolean;
  end;

var
  Config: TMigrationConfig;

procedure ShowBanner;
begin
  WriteLn('=====================================');
  WriteLn('DScintilla Complete Migration Tool');
  WriteLn('Version ', VERSION);
  WriteLn('=====================================');
  WriteLn('');
end;

procedure InitializeConfig;
begin
  with Config do
  begin
    ScintillaIfaceFile := 'Scintilla.iface';
    OldWrapperFile := 'DScintilla.pas';
    OutputDir := 'Output';
    BackupOldWrapper := True;
    GenerateNewWrapper := True;
    GenerateCompatLayer := True;
    GenerateReport := True;
    GenerateTests := True;
    CompileDemo := True;
  end;
end;

function ExecuteAndWait(const Command: string; const Params: string = ''): Integer;
var
  StartInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
begin
  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartInfo.wShowWindow := SW_HIDE;

  if CreateProcess(nil, PChar(Command + ' ' + Params), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    Result := ExitCode;
  end
  else
    Result := -1;
end;

procedure Step(const Description: string);
begin
  WriteLn('[*] ', Description);
end;

procedure Success(const Message: string);
begin
  WriteLn('[+] ', Message);
end;

procedure Error(const Message: string);
begin
  WriteLn('[!] ERRO: ', Message);
end;

procedure BackupOldWrapper;
var
  BackupFile: string;
begin
  if not Config.BackupOldWrapper then Exit;

  Step('Fazendo backup do wrapper antigo...');

  if FileExists(Config.OldWrapperFile) then
  begin
    BackupFile := ChangeFileExt(Config.OldWrapperFile,
      '.backup.' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.pas');

    if CopyFile(PChar(Config.OldWrapperFile), PChar(BackupFile), True) then
      Success('Backup criado: ' + BackupFile)
    else
      Error('Falha ao criar backup');
  end;
end;

procedure GenerateNewWrapper;
var
  ExitCode: Integer;
  NewWrapperFile: string;
begin
  if not Config.GenerateNewWrapper then Exit;

  Step('Gerando novo wrapper do Scintilla...');

  // Verifica se o gerador existe
  if not FileExists('ScintillaIfaceGenerator.exe') then
  begin
    Step('Compilando gerador...');
    ExitCode := ExecuteAndWait('fpc', 'ScintillaIfaceGenerator.pas -O3');

    if ExitCode <> 0 then
    begin
      Error('Falha ao compilar gerador');
      Exit;
    end;
  end;

  // Executa o gerador
  NewWrapperFile := IncludeTrailingPathDelimiter(Config.OutputDir) + 'DScintilla.pas';
  ExitCode := ExecuteAndWait('ScintillaIfaceGenerator.exe',
    Config.ScintillaIfaceFile + ' ' + NewWrapperFile);

  if ExitCode = 0 then
    Success('Novo wrapper gerado: ' + NewWrapperFile)
  else
    Error('Falha ao gerar wrapper');
end;

procedure RunMigrationAnalysis;
var
  ExitCode: Integer;
  OldFile, NewFile: string;
  Params: string;
begin
  if not (Config.GenerateCompatLayer or Config.GenerateReport) then Exit;

  Step('Executando análise de migração...');

  // Verifica se a ferramenta de migração existe
  if not FileExists('DScintillaMigrationTool.exe') then
  begin
    Step('Compilando ferramenta de migração...');
    ExitCode := ExecuteAndWait('fpc', 'DScintillaMigrationTool.pas -O3');

    if ExitCode <> 0 then
    begin
      Error('Falha ao compilar ferramenta de migração');
      Exit;
    end;
  end;

  OldFile := Config.OldWrapperFile;
  NewFile := IncludeTrailingPathDelimiter(Config.OutputDir) + 'DScintilla.pas';

  Params := OldFile + ' ' + NewFile + ' -o ' + Config.OutputDir;

  if Config.GenerateCompatLayer and Config.GenerateReport then
    Params := Params + ' -all'
  else if Config.GenerateCompatLayer then
    Params := Params + ' -compat'
  else if Config.GenerateReport then
    Params := Params + ' -report';

  ExitCode := ExecuteAndWait('DScintillaMigrationTool.exe', Params);

  if ExitCode = 0 then
  begin
    if Config.GenerateCompatLayer then
      Success('Camada de compatibilidade gerada');
    if Config.GenerateReport then
      Success('Relatório de migração gerado');
  end
  else
    Error('Falha na análise de migração');
end;

procedure GenerateTests;
var
  SL: TStringList;
  TestFile: string;
begin
  if not Config.GenerateTests then Exit;

  Step('Gerando testes unitários...');

  TestFile := IncludeTrailingPathDelimiter(Config.OutputDir) + 'DScintillaTests.pas';

  SL := TStringList.Create;
  try
    SL.Add('unit DScintillaTests;');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  TestFramework, DScintilla, DScintillaCompat;');
    SL.Add('');
    SL.Add('type');
    SL.Add('  TDScintillaTest = class(TTestCase)');
    SL.Add('  private');
    SL.Add('    FScintilla: TScintilla;');
    SL.Add('  protected');
    SL.Add('    procedure SetUp; override;');
    SL.Add('    procedure TearDown; override;');
    SL.Add('  published');
    SL.Add('    procedure TestCreate;');
    SL.Add('    procedure TestBasicText;');
    SL.Add('    procedure TestConstants;');
    SL.Add('    procedure TestCompatibility;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.SetUp;');
    SL.Add('begin');
    SL.Add('  FScintilla := TScintilla.Create(nil);');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.TearDown;');
    SL.Add('begin');
    SL.Add('  FScintilla.Free;');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.TestCreate;');
    SL.Add('begin');
    SL.Add('  CheckNotNull(FScintilla, ''Scintilla deve ser criado'');');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.TestBasicText;');
    SL.Add('begin');
    SL.Add('  FScintilla.SetText(''Hello World'');');
    SL.Add('  CheckEquals(11, FScintilla.GetLength, ''Tamanho do texto'');');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.TestConstants;');
    SL.Add('begin');
    SL.Add('  // Testa se constantes importantes existem');
    SL.Add('  Check(SCI_ADDTEXT > 0, ''SCI_ADDTEXT deve existir'');');
    SL.Add('  Check(SCLEX_PASCAL >= 0, ''SCLEX_PASCAL deve existir'');');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TDScintillaTest.TestCompatibility;');
    SL.Add('var');
    SL.Add('  Compat: TDScintillaCompat;');
    SL.Add('begin');
    SL.Add('  Compat := TDScintillaCompat.Create(nil);');
    SL.Add('  try');
    SL.Add('    // Testa métodos de compatibilidade');
    SL.Add('    CheckNotNull(Compat, ''Compatibilidade deve funcionar'');');
    SL.Add('  finally');
    SL.Add('    Compat.Free;');
    SL.Add('  end;');
    SL.Add('end;');
    SL.Add('');
    SL.Add('initialization');
    SL.Add('  RegisterTest(TDScintillaTest.Suite);');
    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(TestFile);
    Success('Testes unitários gerados: ' + TestFile);
  finally
    SL.Free;
  end;
end;

procedure GenerateSummary;
var
  SL: TStringList;
  SummaryFile: string;
begin
  Step('Gerando resumo da migração...');

  SummaryFile := IncludeTrailingPathDelimiter(Config.OutputDir) + 'MIGRATION_SUMMARY.txt';

  SL := TStringList.Create;
  try
    SL.Add('RESUMO DA MIGRAÇÃO DSCINTILLA');
    SL.Add('==============================');
    SL.Add('Data: ' + DateTimeToStr(Now));
    SL.Add('');
    SL.Add('Arquivos Gerados:');
    SL.Add('-----------------');

    if Config.BackupOldWrapper then
      SL.Add('- Backup do wrapper original');

    if Config.GenerateNewWrapper then
    begin
      SL.Add('- DScintilla.pas (novo wrapper)');
      SL.Add('- Scintilla.Consts.inc');
      SL.Add('- Scintilla.Types.inc');
    end;

    if Config.GenerateCompatLayer then
      SL.Add('- DScintillaCompat.pas (compatibilidade)');

    if Config.GenerateReport then
      SL.Add('- MigrationReport.txt (relatório detalhado)');

    if Config.GenerateTests then
      SL.Add('- DScintillaTests.pas (testes unitários)');

    SL.Add('');
    SL.Add('Próximos Passos:');
    SL.Add('----------------');
    SL.Add('1. Revise o relatório de migração em MigrationReport.txt');
    SL.Add('2. Atualize seu projeto para usar DScintillaCompat temporariamente');
    SL.Add('3. Execute os testes unitários');
    SL.Add('4. Gradualmente migre para o novo DScintilla');
    SL.Add('5. Remova DScintillaCompat quando a migração estiver completa');
    SL.Add('');
    SL.Add('Exemplo de Migração Gradual:');
    SL.Add('---------------------------');
    SL.Add('// Passo 1: Use compatibilidade');
    SL.Add('uses DScintilla, DScintillaCompat;');
    SL.Add('var Editor: TDScintillaCompat;');
    SL.Add('');
    SL.Add('// Passo 2: Migre gradualmente');
    SL.Add('uses DScintilla;');
    SL.Add('var Editor: TScintilla;');

    SL.SaveToFile(SummaryFile);
    Success('Resumo gerado: ' + SummaryFile);
  finally
    SL.Free;
  end;
end;

// Programa principal
begin
  ShowBanner;
  InitializeConfig;

  try
    // Cria diretório de saída
    if not DirectoryExists(Config.OutputDir) then
    begin
      Step('Criando diretório de saída...');
      CreateDir(Config.OutputDir);
    end;

    // Executa as etapas
    BackupOldWrapper;
    GenerateNewWrapper;
    RunMigrationAnalysis;
    GenerateTests;
    GenerateSummary;

    WriteLn('');
    WriteLn('=====================================');
    WriteLn('Migração concluída com sucesso!');
    WriteLn('Verifique os arquivos em: ', Config.OutputDir);
    WriteLn('=====================================');
  except
    on E: Exception do
    begin
      Error(E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.
