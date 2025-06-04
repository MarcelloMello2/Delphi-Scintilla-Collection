unit Seven.Scintilla.DScintillaMigration;

interface

uses
  SysUtils, Classes, StrUtils;

type
  TDScintillaMigrationHelper = class
  private
    FOldConstants: TStringList;
    FNewConstants: TStringList;
    FDeprecatedList: TStringList;

    procedure LoadOldConstants(const FileName: string);
    procedure CompareConstants;
    procedure GenerateCompatibilityLayer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AnalyzeExistingWrapper(const OldWrapperFile: string);
    procedure GenerateMigrationReport(const OutputFile: string);
    procedure GenerateCompatibilityUnit(const OutputFile: string);
  end;

implementation

constructor TDScintillaMigrationHelper.Create;
begin
  inherited;
  FOldConstants := TStringList.Create;
  FNewConstants := TStringList.Create;
  FDeprecatedList := TStringList.Create;
end;

destructor TDScintillaMigrationHelper.Destroy;
begin
  FOldConstants.Free;
  FNewConstants.Free;
  FDeprecatedList.Free;
  inherited;
end;

procedure TDScintillaMigrationHelper.LoadOldConstants(const FileName: string);
var
  SL: TStringList;
  I: Integer;
  Line, ConstName: string;
  EqualPos: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);

    for I := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);

      // Procura por constantes no formato: CONST_NAME = valor;
      if ContainsStr(Line, '=') and EndsStr(';', Line) then
      begin
        EqualPos := Pos('=', Line);
        ConstName := Trim(Copy(Line, 1, EqualPos - 1));

        // Verifica se é uma constante Scintilla
        if StartsStr('SCI_', ConstName) or StartsStr('SC_', ConstName) or
           StartsStr('SCEN_', ConstName) or StartsStr('SCN_', ConstName) then
        begin
          FOldConstants.Add(ConstName);
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TDScintillaMigrationHelper.AnalyzeExistingWrapper(const OldWrapperFile: string);
var
  SL: TStringList;
  I: Integer;
  Line: string;
begin
  WriteLn('Analisando wrapper existente: ', OldWrapperFile);

  LoadOldConstants(OldWrapperFile);

  SL := TStringList.Create;
  try
    SL.LoadFromFile(OldWrapperFile);

    // Analisa métodos
    for I := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);

      // Procura por funções/procedures
      if StartsStr('function ', Line) or StartsStr('procedure ', Line) then
      begin
        // Extrai nome do método
        // Adicionar à lista de métodos existentes
      end;
    end;
  finally
    SL.Free;
  end;

  WriteLn('Encontradas ', FOldConstants.Count, ' constantes antigas');
end;

procedure TDScintillaMigrationHelper.CompareConstants;
var
  I: Integer;
  ConstName: string;
begin
  // Identifica constantes obsoletas
  for I := 0 to FOldConstants.Count - 1 do
  begin
    ConstName := FOldConstants[I];
    if FNewConstants.IndexOf(ConstName) = -1 then
    begin
      FDeprecatedList.Add(ConstName);
    end;
  end;
end;

procedure TDScintillaMigrationHelper.GenerateMigrationReport(const OutputFile: string);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('DScintilla Migration Report');
    SL.Add('==========================');
    SL.Add('Generated: ' + DateTimeToStr(Now));
    SL.Add('');

    SL.Add('Deprecated Constants (' + IntToStr(FDeprecatedList.Count) + '):');
    SL.Add('--------------------');
    for I := 0 to FDeprecatedList.Count - 1 do
    begin
      SL.Add('  - ' + FDeprecatedList[I]);
    end;
    SL.Add('');

    SL.Add('Migration Steps:');
    SL.Add('---------------');
    SL.Add('1. Backup your existing DScintilla unit');
    SL.Add('2. Generate new wrapper using ScintillaWrapperBuilder');
    SL.Add('3. Include DScintillaCompat unit for backward compatibility');
    SL.Add('4. Test your application thoroughly');
    SL.Add('5. Gradually update deprecated constants and methods');

    SL.SaveToFile(OutputFile);
    WriteLn('Relatório de migração salvo em: ', OutputFile);
  finally
    SL.Free;
  end;
end;

procedure TDScintillaMigrationHelper.GenerateCompatibilityLayer;
begin
  // Implementar geração de camada de compatibilidade
end;

procedure TDScintillaMigrationHelper.GenerateCompatibilityUnit(const OutputFile: string);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('unit DScintillaCompat;');
    SL.Add('');
    SL.Add('{ Compatibility layer for old DScintilla code }');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  DScintilla;');
    SL.Add('');
    SL.Add('const');
    SL.Add('  { Deprecated constants - mapped to new values }');

    // Adiciona mapeamentos de constantes antigas
    for I := 0 to FDeprecatedList.Count - 1 do
    begin
      // Tenta mapear para nova constante
      SL.Add(Format('  %s = %s; // DEPRECATED',
        [FDeprecatedList[I], 'SCI_UNDEFINED']));
    end;

    SL.Add('');
    SL.Add('type');
    SL.Add('  { Compatibility class }');
    SL.Add('  TDScintillaCompat = class(TScintilla)');
    SL.Add('  public');
    SL.Add('    { Old method signatures for compatibility }');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(OutputFile);
    WriteLn('Unit de compatibilidade gerada: ', OutputFile);
  finally
    SL.Free;
  end;
end;

end.
