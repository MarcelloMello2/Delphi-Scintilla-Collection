unit ScintillaFacerGenerator.PascalGenerator;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  ScintillaFacerGenerator.Face,
  ScintillaFacerGenerator.FileGenerator,
  ScintillaFacerGenerator.HFacer,
  ScintillaFacerGenerator.ScintillaAPIFacer;

type
  TDelphiGenerator = class
  private
    FFace: TFace;
    FConstants: TStringList;
    FTypes: TStringList;
    FMessages: TStringList;
    FDeclarations: TStringList;
    FImplementations: TStringList;
    FProperties: TStringList;
    
    procedure GenerateConstants;
    procedure GenerateTypes;
    procedure GenerateMessages;
    procedure GenerateMethods;
    procedure GenerateProperties;
    
    function ConvertCppTypeToDelphi(const CppType: string): string;
    function GenerateWParam(const Feature: TFeature): string;
    function GenerateLParam(const Feature: TFeature): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadIfaceFile(const FileName: string);
    procedure GenerateDelphiUnit(const OutputFile: string);
    
    property Constants: TStringList read FConstants;
    property Declarations: TStringList read FDeclarations;
    property Properties: TStringList read FProperties;
  end;

procedure RunGenerator;

implementation

constructor TDelphiGenerator.Create;
begin
  inherited;
  FFace := TFace.Create;
  FConstants := TStringList.Create;
  FTypes := TStringList.Create;
  FMessages := TStringList.Create;
  FDeclarations := TStringList.Create;
  FImplementations := TStringList.Create;
  FProperties := TStringList.Create;
end;

destructor TDelphiGenerator.Destroy;
begin
  FFace.Free;
  FConstants.Free;
  FTypes.Free;
  FMessages.Free;
  FDeclarations.Free;
  FImplementations.Free;
  FProperties.Free;
  inherited;
end;

function TDelphiGenerator.ConvertCppTypeToDelphi(const CppType: string): string;
begin
  if CppType = 'void' then Result := ''
  else if CppType = 'int' then Result := 'Integer'
  else if CppType = 'bool' then Result := 'Boolean'
  else if CppType = 'position' then Result := 'Integer'
  else if CppType = 'line' then Result := 'Integer'
  else if CppType = 'colour' then Result := 'TColor'
  else if CppType = 'colouralpha' then Result := 'TColorAlpha'
  else if CppType = 'string' then Result := 'PAnsiChar'
  else if CppType = 'stringresult' then Result := 'PAnsiChar'
  else if CppType = 'cells' then Result := 'PAnsiChar'
  else if CppType = 'pointer' then Result := 'Pointer'
  else if CppType = 'textrange' then Result := 'PTextRange'
  else if CppType = 'textrangefull' then Result := 'PTextRangeFull'
  else if CppType = 'findtext' then Result := 'PFindText'
  else if CppType = 'findtextfull' then Result := 'PFindTextFull'
  else if CppType = 'keymod' then Result := 'Integer'
  else if CppType = 'formatrange' then Result := 'PFormatRange'
  else if CppType = 'formatrangefull' then Result := 'PFormatRangeFull'
  else Result := 'Integer';
end;

procedure TDelphiGenerator.LoadIfaceFile(const FileName: string);
begin
  FFace.ReadFromFile(FileName);
  
  GenerateConstants;
  GenerateTypes;
  GenerateMessages;
  GenerateMethods;
  GenerateProperties;
end;

procedure TDelphiGenerator.GenerateConstants;
var
  I: Integer;
  Name: string;
  Feature: TFeature;
  LastEnum: string;
  AllEnumPrefixes: TStringList;
  Prefix: string;
//  HasPrefix: Boolean;
begin
  AllEnumPrefixes := TStringList.Create;
  try
    // Prefixos padrão
    AllEnumPrefixes.Add('SCE_');
    AllEnumPrefixes.Add('SCI_');
    AllEnumPrefixes.Add('SCEN_');
    
    // Coleta prefixos de enumerações
    for Name in FFace.Features.Keys do
    begin
      if FFace.Features.TryGetValue(Name, Feature) then
      begin
        if (Feature.Category <> 'Deprecated') and (Feature.FeatureType = ftEnu) then
        begin
          AllEnumPrefixes.Delimiter := ' ';
          AllEnumPrefixes.StrictDelimiter := True;
          AllEnumPrefixes.DelimitedText := AllEnumPrefixes.DelimitedText + ' ' + Feature.Value;
        end;
      end;
    end;
    
    FConstants.Add('const');
    
    LastEnum := '';
    for I := 0 to FFace.Order.Count - 1 do
    begin
      Name := FFace.Order[I];
      if FFace.Features.TryGetValue(Name, Feature) then
      begin
        if Feature.Category <> 'Deprecated' then
        begin
          if Feature.FeatureType = ftVal then
          begin
            // Verifica se pertence a alguma enumeração
//            HasPrefix := False;
            for Prefix in AllEnumPrefixes do
            begin
              if StartsStr(Prefix, Name) then
              begin
//                HasPrefix := True;

                // Adiciona comentário para nova enumeração se necessário
                if (Prefix <> LastEnum) and (Prefix <> 'SCI_') and (Prefix <> 'SCE_') and (Prefix <> 'SCEN_') then
                begin
                  FConstants.Add('');
                  FConstants.Add('  // ' + Prefix + ' enumeration');
                  LastEnum := Prefix;
                end;
                Break;
              end;
            end;
            
            FConstants.Add('  ' + Name + ' = ' + Feature.Value + ';');
          end;
        end;
      end;
    end;
  finally
    AllEnumPrefixes.Free;
  end;
end;

procedure TDelphiGenerator.GenerateTypes;
begin
  FTypes.Add('type');
  FTypes.Add('  TSciPosition = Integer;');
  FTypes.Add('  TSciLine = Integer;');
  FTypes.Add('  TColorAlpha = Cardinal;');
  FTypes.Add('');
  FTypes.Add('  PTextRange = ^TTextRange;');
  FTypes.Add('  TTextRange = record');
  FTypes.Add('    chrg: TCharRange;');
  FTypes.Add('    lpstrText: PAnsiChar;');
  FTypes.Add('  end;');
  FTypes.Add('');
  FTypes.Add('  PTextRangeFull = ^TTextRangeFull;');
  FTypes.Add('  TTextRangeFull = record');
  FTypes.Add('    cpMin: Int64;');
  FTypes.Add('    cpMax: Int64;');
  FTypes.Add('    lpstrText: PAnsiChar;');
  FTypes.Add('  end;');
  FTypes.Add('');
  FTypes.Add('  PFindText = ^TFindText;');
  FTypes.Add('  TFindText = record');
  FTypes.Add('    chrg: TCharRange;');
  FTypes.Add('    lpstrText: PAnsiChar;');
  FTypes.Add('    chrgText: TCharRange;');
  FTypes.Add('  end;');
  FTypes.Add('');
  FTypes.Add('  PFindTextFull = ^TFindTextFull;');
  FTypes.Add('  TFindTextFull = record');
  FTypes.Add('    cpMin: Int64;');
  FTypes.Add('    cpMax: Int64;');
  FTypes.Add('    lpstrText: PAnsiChar;');
  FTypes.Add('    cpMinText: Int64;');
  FTypes.Add('    cpMaxText: Int64;');
  FTypes.Add('  end;');
end;

procedure TDelphiGenerator.GenerateMessages;
var
  I: Integer;
  Name: string;
  Feature: TFeature;
begin
  FMessages.Add('  // Scintilla messages');
  
  for I := 0 to FFace.Order.Count - 1 do
  begin
    Name := FFace.Order[I];
    if FFace.Features.TryGetValue(Name, Feature) then
    begin
      if (Feature.Category <> 'Deprecated') and 
         (Feature.FeatureType in [ftFun, ftGet, ftSet]) then
      begin
        FMessages.Add('  SCI_' + UpperCase(Name) + ' = ' + Feature.Value + ';');
      end;
    end;
  end;
  
  FMessages.Add('');
  FMessages.Add('  // Scintilla notifications');
  
  for I := 0 to FFace.Order.Count - 1 do
  begin
    Name := FFace.Order[I];
    if FFace.Features.TryGetValue(Name, Feature) then
    begin
      if (Feature.Category <> 'Deprecated') and (Feature.FeatureType = ftEvt) then
      begin
        FMessages.Add('  SCN_' + UpperCase(Name) + ' = ' + Feature.Value + ';');
      end;
    end;
  end;
end;

function TDelphiGenerator.GenerateWParam(const Feature: TFeature): string;
begin
  if Feature.Param1.Name <> '' then
    Result := Feature.Param1.Name
  else
    Result := '0';
end;

function TDelphiGenerator.GenerateLParam(const Feature: TFeature): string;
begin
  if Feature.Param2.Name <> '' then
  begin
    if (Feature.Param2.ParamType = 'string') or 
       (Feature.Param2.ParamType = 'stringresult') then
      Result := 'LPARAM(' + Feature.Param2.Name + ')'
    else if Feature.Param2.ParamType = 'pointer' then
      Result := 'LPARAM(' + Feature.Param2.Name + ')'
    else
      Result := Feature.Param2.Name;
  end
  else
    Result := '0';
end;

procedure TDelphiGenerator.GenerateMethods;
var
  I: Integer;
  Name: string;
  Feature: TFeature;
  DelphiRetType: string;
  ParamStr: string;
  LastCategory: string;
begin
  LastCategory := '';
  
  for I := 0 to FFace.Order.Count - 1 do
  begin
    Name := FFace.Order[I];
    if FFace.Features.TryGetValue(Name, Feature) then
    begin
      if (Feature.Category <> 'Deprecated') and 
         (Feature.FeatureType in [ftFun, ftGet, ftSet]) then
      begin
        // Adiciona comentário de categoria
        if Feature.Category <> LastCategory then
        begin
          FDeclarations.Add('');
          FDeclarations.Add('    // ' + Feature.Category);
          LastCategory := Feature.Category;
        end;
        
        DelphiRetType := ConvertCppTypeToDelphi(Feature.ReturnType);
        
        // Monta string de parâmetros
        ParamStr := '';
        if Feature.Param1.Name <> '' then
        begin
          ParamStr := Feature.Param1.Name + ': ' + 
            ConvertCppTypeToDelphi(Feature.Param1.ParamType);
        end;
        
        if Feature.Param2.Name <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + Feature.Param2.Name + ': ' + 
            ConvertCppTypeToDelphi(Feature.Param2.ParamType);
        end;
        
        // Gera declaração
        if DelphiRetType <> '' then
        begin
          FDeclarations.Add('    function ' + Name + '(' + ParamStr + '): ' + DelphiRetType + ';');
        end
        else
        begin
          FDeclarations.Add('    procedure ' + Name + '(' + ParamStr + ');');
        end;
        
        // Gera implementação
        FImplementations.Add('');
        if DelphiRetType <> '' then
        begin
          FImplementations.Add('function TScintilla.' + Name + '(' + ParamStr + '): ' + DelphiRetType + ';');
          FImplementations.Add('begin');
          FImplementations.Add('  Result := SendMessage(Handle, SCI_' + UpperCase(Name) + ', ' +
            GenerateWParam(Feature) + ', ' + GenerateLParam(Feature) + ');');
          FImplementations.Add('end;');
        end
        else
        begin
          FImplementations.Add('procedure TScintilla.' + Name + '(' + ParamStr + ');');
          FImplementations.Add('begin');
          FImplementations.Add('  SendMessage(Handle, SCI_' + UpperCase(Name) + ', ' +
            GenerateWParam(Feature) + ', ' + GenerateLParam(Feature) + ');');
          FImplementations.Add('end;');
        end;
      end;
    end;
  end;
end;

procedure TDelphiGenerator.GenerateProperties;
var
  GetterSetterPairs: TDictionary<string, TPair<string, string>>;
  I: Integer;
  Name, PropName: string;
  Feature: TFeature;
  GetterName, SetterName: string;
  Pair: TPair<string, string>;
begin
  GetterSetterPairs := TDictionary<string, TPair<string, string>>.Create;
  try
    // Primeiro, identifica pares Get/Set
    for I := 0 to FFace.Order.Count - 1 do
    begin
      Name := FFace.Order[I];
      if FFace.Features.TryGetValue(Name, Feature) then
      begin
        if (Feature.Category <> 'Deprecated') and 
           (Feature.FeatureType = ftGet) and 
           StartsStr('Get', Name) and
           (Feature.Param1.Name = '') and
           (Feature.Param2.Name = '') then
        begin
          PropName := Copy(Name, 4, MaxInt);
          GetterName := Name;
          
          // Procura setter correspondente
          SetterName := 'Set' + PropName;
          if FFace.Features.ContainsKey(SetterName) then
          begin
            GetterSetterPairs.Add(PropName, TPair<string, string>.Create(GetterName, SetterName));
          end
          else
          begin
            // Property somente leitura
            GetterSetterPairs.Add(PropName, TPair<string, string>.Create(GetterName, ''));
          end;
        end;
      end;
    end;
    
    // Gera properties
    if GetterSetterPairs.Count > 0 then
    begin
      FProperties.Add('  published');
      FProperties.Add('    // Auto-generated properties');
      
      for PropName in GetterSetterPairs.Keys do
      begin
        Pair := GetterSetterPairs[PropName];
        GetterName := Pair.Key;
        SetterName := Pair.Value;
        
        if FFace.Features.TryGetValue(GetterName, Feature) then
        begin
          if SetterName <> '' then
            FProperties.Add('    property ' + PropName + ': ' + 
              ConvertCppTypeToDelphi(Feature.ReturnType) + 
              ' read ' + GetterName + ' write ' + SetterName + ';')
          else
            FProperties.Add('    property ' + PropName + ': ' + 
              ConvertCppTypeToDelphi(Feature.ReturnType) + 
              ' read ' + GetterName + ';');
        end;
      end;
    end;
  finally
    GetterSetterPairs.Free;
  end;
end;

procedure TDelphiGenerator.GenerateDelphiUnit(const OutputFile: string);
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    // Cabeçalho
    Output.Add('unit DScintilla;');
    Output.Add('');
    Output.Add('{');
    Output.Add('  Delphi wrapper for Scintilla editor');
    Output.Add('  Auto-generated from Scintilla.iface');
    Output.Add('  Generation date: ' + DateTimeToStr(Now));
    Output.Add('}');
    Output.Add('');
    Output.Add('interface');
    Output.Add('');
    Output.Add('uses');
    Output.Add('  Windows, Messages, SysUtils, Classes, Graphics, Controls;');
    Output.Add('');
    
    // Adiciona constantes
    Output.AddStrings(FConstants);
    Output.Add('');
    
    // Adiciona mensagens
    Output.AddStrings(FMessages);
    Output.Add('');
    
    // Adiciona tipos
    Output.AddStrings(FTypes);
    Output.Add('');
    
    // Classe TScintilla
    Output.Add('  TScintilla = class(TWinControl)');
    Output.Add('  private');
    Output.Add('    FDirectPtr: Pointer;');
    Output.Add('    FDirectFunction: Pointer;');
    Output.Add('  protected');
    Output.Add('    procedure CreateWnd; override;');
    Output.Add('    procedure DestroyWnd; override;');
    Output.Add('    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;');
    Output.Add('    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;');
    Output.Add('  public');
    Output.Add('    constructor Create(AOwner: TComponent); override;');
    Output.Add('    destructor Destroy; override;');
    
    // Adiciona declarações de métodos
    Output.AddStrings(FDeclarations);
    
    // Adiciona properties
    if FProperties.Count > 0 then
    begin
      Output.Add('');
      Output.AddStrings(FProperties);
    end;
    
    Output.Add('  end;');
    Output.Add('');
    Output.Add('procedure Register;');
    Output.Add('');
    Output.Add('implementation');
    Output.Add('');
    Output.Add('const');
    Output.Add('  ScintillaClassName = ''Scintilla'';');
    Output.Add('  ScintillaDLL = ''SciLexer.dll'';');
    Output.Add('');
    
    // Registro do componente
    Output.Add('procedure Register;');
    Output.Add('begin');
    Output.Add('  RegisterComponents(''Scintilla'', [TScintilla]);');
    Output.Add('end;');
    Output.Add('');
    
    // Implementações básicas
    Output.Add('{ TScintilla }');
    Output.Add('');
    Output.Add('constructor TScintilla.Create(AOwner: TComponent);');
    Output.Add('begin');
    Output.Add('  inherited Create(AOwner);');
    Output.Add('  Width := 300;');
    Output.Add('  Height := 200;');
    Output.Add('  TabStop := True;');
    Output.Add('  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];');
    Output.Add('end;');
    Output.Add('');
    Output.Add('destructor TScintilla.Destroy;');
    Output.Add('begin');
    Output.Add('  inherited Destroy;');
    Output.Add('end;');
    Output.Add('');
    Output.Add('procedure TScintilla.CreateWnd;');
    Output.Add('var');
    Output.Add('  LoadResult: THandle;');
    Output.Add('begin');
    Output.Add('  LoadResult := LoadLibrary(ScintillaDLL);');
    Output.Add('  if LoadResult = 0 then');
    Output.Add('    raise Exception.Create(''Failed to load '' + ScintillaDLL);');
    Output.Add('');
    Output.Add('  CreateWindowEx(0, ScintillaClassName, nil,');
    Output.Add('    WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_CLIPCHILDREN,');
    Output.Add('    0, 0, Width, Height, Handle, 0, HInstance, nil);');
    Output.Add('');
    Output.Add('  // Get direct access for better performance');
    Output.Add('  FDirectPtr := Pointer(SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0));');
    Output.Add('  FDirectFunction := Pointer(SendMessage(Handle, SCI_GETDIRECTFUNCTION, 0, 0));');
    Output.Add('end;');
    Output.Add('');
    Output.Add('procedure TScintilla.DestroyWnd;');
    Output.Add('begin');
    Output.Add('  FDirectPtr := nil;');
    Output.Add('  FDirectFunction := nil;');
    Output.Add('  inherited DestroyWnd;');
    Output.Add('end;');
    Output.Add('');
    Output.Add('procedure TScintilla.WMGetDlgCode(var Message: TWMGetDlgCode);');
    Output.Add('begin');
    Output.Add('  inherited;');
    Output.Add('  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;');
    Output.Add('end;');
    Output.Add('');
    Output.Add('procedure TScintilla.WMEraseBkgnd(var Message: TWMEraseBkgnd);');
    Output.Add('begin');
    Output.Add('  Message.Result := 1; // Prevent flicker');
    Output.Add('end;');
    
    // Adiciona implementações dos métodos
    Output.AddStrings(FImplementations);
    
    Output.Add('');
    Output.Add('end.');
    
    // Salva o arquivo
    Output.SaveToFile(OutputFile);
    WriteLn('Generated: ' + OutputFile);
  finally
    Output.Free;
  end;
end;

// Procedure principal que é chamada pelo programa
procedure RunGenerator;
var
  Generator: TDelphiGenerator;
  InputFile, OutputFile: string;
  RootPath: string;
begin
  WriteLn('Scintilla Wrapper Generator v3.0');
  WriteLn('===================================');
  WriteLn('');
  
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' <Scintilla.iface> [output.pas]');
    WriteLn('');
    WriteLn('Or to regenerate all files like the Python scripts:');
    WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' --regenerate-all <root-path>');
    Exit;
  end;
  
  if ParamStr(1) = '--regenerate-all' then
  begin
    // Modo compatível com os scripts Python
    if ParamCount < 2 then
    begin
      WriteLn('Error: root path required');
      Exit;
    end;
    
    RootPath := ParamStr(2);
    WriteLn('Regenerating all files from: ' + RootPath);
    
    // Chama o regenerador que simula o comportamento dos scripts Python
    ScintillaFacerGenerator.ScintillaAPIFacer.RegenerateAll(RootPath);
    
    // Agora gera o wrapper Delphi
    Generator := TDelphiGenerator.Create;
    try
      InputFile := IncludeTrailingPathDelimiter(RootPath) + 'include' + PathDelim + 'Scintilla.iface';
      OutputFile := IncludeTrailingPathDelimiter(RootPath) + 'DScintilla.pas';
      
      WriteLn('');
      WriteLn('Generating Delphi wrapper...');
      Generator.LoadIfaceFile(InputFile);
      Generator.GenerateDelphiUnit(OutputFile);
      
      WriteLn('');
      WriteLn('Generation complete!');
    finally
      Generator.Free;
    end;
  end
  else
  begin
    // Modo simples - apenas gera o wrapper Delphi
    InputFile := ParamStr(1);
    
    if ParamCount >= 2 then
      OutputFile := ParamStr(2)
    else
      OutputFile := 'DScintilla.pas';
    
    if not FileExists(InputFile) then
    begin
      WriteLn('Error: File not found: ' + InputFile);
      Exit;
    end;
    
    Generator := TDelphiGenerator.Create;
    try
      WriteLn('Loading: ' + InputFile);
      Generator.LoadIfaceFile(InputFile);
      
      WriteLn('Generating: ' + OutputFile);
      Generator.GenerateDelphiUnit(OutputFile);
      
      WriteLn('');
      WriteLn('Statistics:');
      WriteLn('  Constants: ' + IntToStr(Generator.Constants.Count));
      WriteLn('  Functions: ' + IntToStr(Generator.Declarations.Count));
      WriteLn('  Properties: ' + IntToStr(Generator.Properties.Count));
      
      WriteLn('');
      WriteLn('Generation complete!');
    finally
      Generator.Free;
    end;
  end;
  
  WriteLn('');
  WriteLn('Press ENTER to exit...');
  ReadLn;
end;

end.