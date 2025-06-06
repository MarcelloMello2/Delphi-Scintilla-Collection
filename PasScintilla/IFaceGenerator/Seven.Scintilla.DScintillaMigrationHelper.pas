﻿unit Seven.Scintilla.DScintillaMigrationHelper;

interface

uses
  SysUtils, Classes, StrUtils;

type

  TDScintillaMigrationHelper = class
  private
    FOldConstants: TStringList;
    FNewConstants: TStringList;
    FDeprecatedList: TStringList;
    FRenamedConstants: TStringList; // Formato: OLD_NAME=NEW_NAME
    FOldMethods: TStringList;
    FNewMethods: TStringList;

    procedure LoadOldConstants(const FileName: string);
    procedure LoadNewConstants(const FileName: string);
    procedure CompareConstants;
    procedure GenerateCompatibilityLayer(SL: TStringList);
    procedure FindRenamedConstants;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AnalyzeExistingWrapper(const OldWrapperFile: string);
    procedure AnalyzeNewWrapper(const NewWrapperFile: string);
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
  FRenamedConstants := TStringList.Create;
  FOldMethods := TStringList.Create;
  FNewMethods := TStringList.Create;

  // StringLists ordenadas para busca binária
  FOldConstants.Sorted := True;
  FNewConstants.Sorted := True;
  FOldMethods.Sorted := True;
  FNewMethods.Sorted := True;
end;

destructor TDScintillaMigrationHelper.Destroy;
begin
  FOldConstants.Free;
  FNewConstants.Free;
  FDeprecatedList.Free;
  FRenamedConstants.Free;
  FOldMethods.Free;
  FNewMethods.Free;
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

procedure TDScintillaMigrationHelper.LoadNewConstants(const FileName: string);
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

      if ContainsStr(Line, '=') and EndsStr(';', Line) then
      begin
        EqualPos := Pos('=', Line);
        ConstName := Trim(Copy(Line, 1, EqualPos - 1));

        if StartsStr('SCI_', ConstName) or StartsStr('SC_', ConstName) or
           StartsStr('SCEN_', ConstName) or StartsStr('SCN_', ConstName) then
        begin
          FNewConstants.Add(ConstName);
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
  Line, MethodName: string;
  SpacePos: Integer;
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
        SpacePos := Pos('(', Line);
        if SpacePos = 0 then
          SpacePos := Pos(':', Line);
        if SpacePos = 0 then
          SpacePos := Pos(';', Line);

        if SpacePos > 0 then
        begin
          MethodName := Copy(Line, 1, SpacePos - 1);
          // Remove 'function ' ou 'procedure '
          if StartsStr('function ', MethodName) then
            Delete(MethodName, 1, 9)
          else if StartsStr('procedure ', MethodName) then
            Delete(MethodName, 1, 10);

          MethodName := Trim(MethodName);
          if Pos('.', MethodName) > 0 then
            MethodName := Copy(MethodName, Pos('.', MethodName) + 1, Length(MethodName));

          FOldMethods.Add(MethodName);
        end;
      end;
    end;
  finally
    SL.Free;
  end;

  WriteLn('Encontradas ', FOldConstants.Count, ' constantes antigas');
  WriteLn('Encontrados ', FOldMethods.Count, ' métodos antigos');
end;

procedure TDScintillaMigrationHelper.AnalyzeNewWrapper(const NewWrapperFile: string);
var
  SL: TStringList;
  I: Integer;
  Line, MethodName: string;
  SpacePos: Integer;
begin
  WriteLn('Analisando novo wrapper: ', NewWrapperFile);

  LoadNewConstants(NewWrapperFile);

  SL := TStringList.Create;
  try
    SL.LoadFromFile(NewWrapperFile);

    // Analisa métodos novos
    for I := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);

      if StartsStr('function ', Line) or StartsStr('procedure ', Line) then
      begin
        SpacePos := Pos('(', Line);
        if SpacePos = 0 then
          SpacePos := Pos(':', Line);
        if SpacePos = 0 then
          SpacePos := Pos(';', Line);

        if SpacePos > 0 then
        begin
          MethodName := Copy(Line, 1, SpacePos - 1);
          if StartsStr('function ', MethodName) then
            Delete(MethodName, 1, 9)
          else if StartsStr('procedure ', MethodName) then
            Delete(MethodName, 1, 10);

          MethodName := Trim(MethodName);
          if Pos('.', MethodName) > 0 then
            MethodName := Copy(MethodName, Pos('.', MethodName) + 1, Length(MethodName));

          FNewMethods.Add(MethodName);
        end;
      end;
    end;
  finally
    SL.Free;
  end;

  WriteLn('Encontradas ', FNewConstants.Count, ' constantes novas');
  WriteLn('Encontrados ', FNewMethods.Count, ' métodos novos');

  // Agora compara as constantes
  CompareConstants;
  FindRenamedConstants;
end;

procedure TDScintillaMigrationHelper.CompareConstants;
var
  I: Integer;
  ConstName: string;
begin
  FDeprecatedList.Clear;

  // Identifica constantes obsoletas (que existiam mas não existem mais)
  for I := 0 to FOldConstants.Count - 1 do
  begin
    ConstName := FOldConstants[I];
    if FNewConstants.IndexOf(ConstName) = -1 then
    begin
      FDeprecatedList.Add(ConstName);
    end;
  end;

  WriteLn('Identificadas ', FDeprecatedList.Count, ' constantes obsoletas');
end;

procedure TDScintillaMigrationHelper.FindRenamedConstants;
var
  I, J: Integer;
  OldName, NewName: string;
  OldBase, NewBase: string;
  //SimilarityScore: Integer;
begin
  // Tenta identificar constantes que foram renomeadas
  // Procura por padrões similares

  for I := 0 to FDeprecatedList.Count - 1 do
  begin
    OldName := FDeprecatedList[I];

    // Remove prefixo comum para comparação
    if StartsStr('SCI_', OldName) then
      OldBase := Copy(OldName, 5, Length(OldName))
    else if StartsStr('SC_', OldName) then
      OldBase := Copy(OldName, 4, Length(OldName))
    else if StartsStr('SCN_', OldName) then
      OldBase := Copy(OldName, 5, Length(OldName))
    else
      OldBase := OldName;

    // Procura por constantes similares nas novas
    for J := 0 to FNewConstants.Count - 1 do
    begin
      NewName := FNewConstants[J];

      // Remove prefixo para comparação
      if StartsStr('SCI_', NewName) then
        NewBase := Copy(NewName, 5, Length(NewName))
      else if StartsStr('SC_', NewName) then
        NewBase := Copy(NewName, 4, Length(NewName))
      else if StartsStr('SCN_', NewName) then
        NewBase := Copy(NewName, 5, Length(NewName))
      else
        NewBase := NewName;

      // Calcula similaridade simples
      if (OldBase = NewBase) or
         (Pos(OldBase, NewBase) > 0) or
         (Pos(NewBase, OldBase) > 0) then
      begin
        FRenamedConstants.Add(OldName + '=' + NewName);
        Break;
      end;
    end;
  end;

  WriteLn('Identificadas ', FRenamedConstants.Count, ' possíveis renomeações');
end;

procedure TDScintillaMigrationHelper.GenerateMigrationReport(const OutputFile: string);
var
  SL: TStringList;
  I: Integer;
  Parts: TArray<string>;
begin
  SL := TStringList.Create;
  try
    SL.Add('DScintilla Migration Report');
    SL.Add('==========================');
    SL.Add('Generated: ' + DateTimeToStr(Now));
    SL.Add('');

    SL.Add('Summary:');
    SL.Add('--------');
    SL.Add('Old constants: ' + IntToStr(FOldConstants.Count));
    SL.Add('New constants: ' + IntToStr(FNewConstants.Count));
    SL.Add('Deprecated constants: ' + IntToStr(FDeprecatedList.Count));
    SL.Add('Renamed constants: ' + IntToStr(FRenamedConstants.Count));
    SL.Add('Old methods: ' + IntToStr(FOldMethods.Count));
    SL.Add('New methods: ' + IntToStr(FNewMethods.Count));
    SL.Add('');

    if FDeprecatedList.Count > 0 then
    begin
      SL.Add('Deprecated Constants:');
      SL.Add('--------------------');
      for I := 0 to FDeprecatedList.Count - 1 do
      begin
        SL.Add('  - ' + FDeprecatedList[I]);
      end;
      SL.Add('');
    end;

    if FRenamedConstants.Count > 0 then
    begin
      SL.Add('Possibly Renamed Constants:');
      SL.Add('--------------------------');
      for I := 0 to FRenamedConstants.Count - 1 do
      begin
        Parts := FRenamedConstants[I].Split(['=']);
        if Length(Parts) = 2 then
          SL.Add('  - ' + Parts[0] + ' -> ' + Parts[1]);
      end;
      SL.Add('');
    end;

    // Métodos removidos
    SL.Add('Removed Methods:');
    SL.Add('---------------');
    for I := 0 to FOldMethods.Count - 1 do
    begin
      if FNewMethods.IndexOf(FOldMethods[I]) = -1 then
        SL.Add('  - ' + FOldMethods[I]);
    end;
    SL.Add('');

    // Novos métodos
    SL.Add('New Methods:');
    SL.Add('-----------');
    for I := 0 to FNewMethods.Count - 1 do
    begin
      if FOldMethods.IndexOf(FNewMethods[I]) = -1 then
        SL.Add('  + ' + FNewMethods[I]);
    end;
    SL.Add('');

    SL.Add('Migration Steps:');
    SL.Add('---------------');
    SL.Add('1. Backup your existing DScintilla unit');
    SL.Add('2. Generate new wrapper using ScintillaWrapperBuilder');
    SL.Add('3. Include DScintillaCompat unit for backward compatibility');
    SL.Add('4. Test your application thoroughly');
    SL.Add('5. Gradually update deprecated constants and methods');
    SL.Add('');
    SL.Add('Use the generated DScintillaCompat.pas unit to maintain');
    SL.Add('backward compatibility during migration.');

    SL.SaveToFile(OutputFile);
    WriteLn('Relatório de migração salvo em: ', OutputFile);
  finally
    SL.Free;
  end;
end;

procedure TDScintillaMigrationHelper.GenerateCompatibilityLayer(SL: TStringList);
var
  I: Integer;
  Parts: TArray<string>;
  OldName, NewName, Value: string;
begin
  // Gera a camada de compatibilidade

  SL.Add('const');
  SL.Add('  { Deprecated constants - mapped to new values }');

  // Primeiro, adiciona constantes renomeadas
  for I := 0 to FRenamedConstants.Count - 1 do
  begin
    Parts := FRenamedConstants[I].Split(['=']);
    if Length(Parts) = 2 then
    begin
      OldName := Parts[0];
      NewName := Parts[1];
      SL.Add(Format('  %s = %s; // RENAMED from %s',
        [OldName, NewName, OldName]));
    end;
  end;

  if FRenamedConstants.Count > 0 then
    SL.Add('');

  // Depois, adiciona constantes obsoletas sem mapeamento
  SL.Add('  { Obsolete constants - no direct replacement }');
  for I := 0 to FDeprecatedList.Count - 1 do
  begin
    OldName := FDeprecatedList[I];

    // Verifica se já foi mapeada como renomeada
    if FRenamedConstants.IndexOfName(OldName) = -1 then
    begin
      // Tenta adivinhar um valor padrão baseado no nome
      if ContainsStr(OldName, 'MARGIN') then
        Value := '0'
      else if ContainsStr(OldName, 'STYLE') then
        Value := 'STYLE_DEFAULT'
      else if ContainsStr(OldName, 'MARKER') then
        Value := '0'
      else
        Value := '0';

      SL.Add(Format('  %s = %s; // OBSOLETE - check migration guide',
        [OldName, Value]));
    end;
  end;

  SL.Add('');
  SL.Add('  { Helper constants for migration }');
  SL.Add('  DSCINTILLA_COMPAT_VERSION = 1;');
  SL.Add('');
end;

procedure TDScintillaMigrationHelper.GenerateCompatibilityUnit(const OutputFile: string);
var
  SL: TStringList;
  I: Integer;
  MethodName: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('unit DScintillaCompat;');
    SL.Add('');
    SL.Add('{ Compatibility layer for old DScintilla code }');
    SL.Add('{ This unit provides backward compatibility for migrating }');
    SL.Add('{ from older versions of DScintilla to the new auto-generated version }');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Windows, Messages, SysUtils, Classes, DScintilla;');
    SL.Add('');

    // Gera a camada de compatibilidade de constantes
    GenerateCompatibilityLayer(SL);

    SL.Add('type');
    SL.Add('  { Compatibility class with old method signatures }');
    SL.Add('  TDScintillaCompat = class(TScintilla)');
    SL.Add('  private');
    SL.Add('    procedure ShowDeprecatedWarning(const MethodName: string);');
    SL.Add('  public');

    // Adiciona métodos obsoletos com avisos
    for I := 0 to FOldMethods.Count - 1 do
    begin
      MethodName := FOldMethods[I];
      if FNewMethods.IndexOf(MethodName) = -1 then
      begin
        // Método foi removido, cria um stub
        SL.Add(Format('    procedure %s; deprecated ''Method removed in new Scintilla version'';',
          [MethodName]));
      end;
    end;

    SL.Add('  end;');
    SL.Add('');

    // Helpers para facilitar migração
    SL.Add('  { Helper functions for migration }');
    SL.Add('  function MigrateConstant(const OldConstant: string): Integer;');
    SL.Add('  procedure ShowMigrationHint(const OldCode, NewCode: string);');
    SL.Add('');

    SL.Add('implementation');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Dialogs;');
    SL.Add('');

    // Implementação da classe de compatibilidade
    SL.Add('{ TDScintillaCompat }');
    SL.Add('');
    SL.Add('procedure TDScintillaCompat.ShowDeprecatedWarning(const MethodName: string);');
    SL.Add('begin');
    SL.Add('  {$IFDEF DEBUG}');
    SL.Add('  OutputDebugString(PChar(''DScintilla: Deprecated method called: '' + MethodName));');
    SL.Add('  {$ENDIF}');
    SL.Add('end;');
    SL.Add('');

    // Implementa stubs para métodos removidos
    for I := 0 to FOldMethods.Count - 1 do
    begin
      MethodName := FOldMethods[I];
      if FNewMethods.IndexOf(MethodName) = -1 then
      begin
        SL.Add(Format('procedure TDScintillaCompat.%s;', [MethodName]));
        SL.Add('begin');
        SL.Add(Format('  ShowDeprecatedWarning(''%s'');', [MethodName]));
        SL.Add('  // This method no longer exists in the new Scintilla version');
        SL.Add('end;');
        SL.Add('');
      end;
    end;

    // Funções helper
    SL.Add('function MigrateConstant(const OldConstant: string): Integer;');
    SL.Add('begin');
    SL.Add('  // Esta função pode ser expandida para mapear constantes antigas');
    SL.Add('  Result := 0;');
    SL.Add('  ');
    SL.Add('  {$IFDEF DEBUG}');
    SL.Add('  OutputDebugString(PChar(''DScintilla: Unknown constant: '' + OldConstant));');
    SL.Add('  {$ENDIF}');
    SL.Add('end;');
    SL.Add('');

    SL.Add('procedure ShowMigrationHint(const OldCode, NewCode: string);');
    SL.Add('begin');
    SL.Add('  {$IFDEF DEBUG}');
    SL.Add('  ShowMessage(''DScintilla Migration Hint:'' + #13#10 +');
    SL.Add('    ''Old: '' + OldCode + #13#10 +');
    SL.Add('    ''New: '' + NewCode);');
    SL.Add('  {$ENDIF}');
    SL.Add('end;');
    SL.Add('');

    SL.Add('initialization');
    SL.Add('  {$IFDEF DEBUG}');
    SL.Add('  OutputDebugString(''DScintillaCompat: Compatibility layer loaded'');');
    SL.Add('  {$ENDIF}');
    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(OutputFile);
    WriteLn('Unit de compatibilidade gerada: ', OutputFile);
  finally
    SL.Free;
  end;
end;

end.
