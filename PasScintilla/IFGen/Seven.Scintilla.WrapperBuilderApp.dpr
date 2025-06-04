program Seven.Scintilla.WrapperBuilderApp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  StrUtils,
  Seven.Scintilla.IfaceParser in 'Seven.Scintilla.IfaceParser.pas',
  Seven.Scintilla.DScintillaMigrationHelper in 'Seven.Scintilla.DScintillaMigrationHelper.pas';

type
  TScintillaWrapperGenerator = class
  private
    FParser: TScintillaParser;
    FOutputPath: string;
    FUnitTargetName: string;
    FGenerateProperties: Boolean;
    FGenerateEvents: Boolean;
    FSplitFiles: Boolean;

    procedure GenerateConstantsInc;
    procedure GenerateTypesInc;
    procedure GenerateEventsInc;
    procedure GenerateMainUnit;
    procedure GenerateDocumentation;

    function FindPropertyPairs: TStringList;
    function GeneratePropertyDeclaration(const PropName, GetterName,
      SetterName, PropType: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadIfaceFile(const FileName: string);
    procedure GenerateWrapper;

    property OutputPath: string read FOutputPath write FOutputPath;
    property UnitTargetName: string read FUnitTargetName write FUnitTargetName;
    property GenerateProperties: Boolean read FGenerateProperties write FGenerateProperties;
    property GenerateEvents: Boolean read FGenerateEvents write FGenerateEvents;
    property SplitFiles: Boolean read FSplitFiles write FSplitFiles;
  end;

constructor TScintillaWrapperGenerator.Create;
begin
  inherited;
  FParser := TScintillaParser.Create;
  FOutputPath := '';
  FUnitTargetName := 'DScintilla';
  FGenerateProperties := True;
  FGenerateEvents := True;
  FSplitFiles := True;
end;

destructor TScintillaWrapperGenerator.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure TScintillaWrapperGenerator.LoadIfaceFile(const FileName: string);
begin
  FParser.ParseFile(FileName);
  WriteLn(Format('Carregado: %d funções, %d constantes, %d eventos',
    [Length(FParser.Functions), Length(FParser.Constants), Length(FParser.Events)]));
end;

procedure TScintillaWrapperGenerator.GenerateConstantsInc;
var
  SL: TStringList;
  I: Integer;
  LastEnum: string;
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(FOutputPath) + 'Scintilla.Consts.inc';

  SL := TStringList.Create;
  try
    SL.Add('{ Scintilla Constants }');
    SL.Add('{ Generated from Scintilla.iface }');
    SL.Add('');

    LastEnum := '';
    for I := 0 to Length(FParser.Constants) - 1 do
    begin
      if (FParser.Constants[I].EnumName <> '') and
         (FParser.Constants[I].EnumName <> LastEnum) then
      begin
        LastEnum := FParser.Constants[I].EnumName;
        SL.Add('');
        SL.Add('  { ' + LastEnum + ' }');
      end;

      SL.Add(Format('  %s = %s;',
        [FParser.Constants[I].Name, FParser.Constants[I].Value]));
    end;

    SL.SaveToFile(FileName);
    WriteLn('Gerado: ' + FileName);
  finally
    SL.Free;
  end;
end;

procedure TScintillaWrapperGenerator.GenerateTypesInc;
var
  SL: TStringList;
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(FOutputPath) + 'Scintilla.Types.inc';

  SL := TStringList.Create;
  try
    SL.Add('{ Scintilla Types }');
    SL.Add('{ Generated from Scintilla.iface }');
    SL.Add('');

    // Tipos básicos
    SL.Add('type');
    SL.Add('  TSciPosition = NativeInt;');
    SL.Add('  TSciCell = NativeInt;');
    SL.Add('  TSciColour = Integer;');
    SL.Add('');

    // Estruturas
    SL.Add('  PSciTextRange = ^TSciTextRange;');
    SL.Add('  TSciTextRange = record');
    SL.Add('    chrg: TCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('  end;');
    SL.Add('');

    SL.Add('  PSciTextToFind = ^TSciTextToFind;');
    SL.Add('  TSciTextToFind = record');
    SL.Add('    chrg: TCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('    chrgText: TCharRange;');
    SL.Add('  end;');
    SL.Add('');

    SL.Add('  PSciRectangle = ^TSciRectangle;');
    SL.Add('  TSciRectangle = record');
    SL.Add('    left: Integer;');
    SL.Add('    top: Integer;');
    SL.Add('    right: Integer;');
    SL.Add('    bottom: Integer;');
    SL.Add('  end;');
    SL.Add('');

    SL.Add('  PSciRangeToFormat = ^TSciRangeToFormat;');
    SL.Add('  TSciRangeToFormat = record');
    SL.Add('    hdc: HDC;');
    SL.Add('    hdcTarget: HDC;');
    SL.Add('    rc: TSciRectangle;');
    SL.Add('    rcPage: TSciRectangle;');
    SL.Add('    chrg: TCharRange;');
    SL.Add('  end;');
    SL.Add('');

    // Estrutura de notificação
    SL.Add('  PSCNotification = ^TSCNotification;');
    SL.Add('  TSCNotification = record');
    SL.Add('    nmhdr: TNMHdr;');
    SL.Add('    position: TSciPosition;');
    SL.Add('    ch: Integer;');
    SL.Add('    modifiers: Integer;');
    SL.Add('    modificationType: Integer;');
    SL.Add('    text: PAnsiChar;');
    SL.Add('    length: TSciPosition;');
    SL.Add('    linesAdded: TSciPosition;');
    SL.Add('    message: Integer;');
    SL.Add('    wParam: WPARAM;');
    SL.Add('    lParam: LPARAM;');
    SL.Add('    line: TSciPosition;');
    SL.Add('    foldLevelNow: Integer;');
    SL.Add('    foldLevelPrev: Integer;');
    SL.Add('    margin: Integer;');
    SL.Add('    listType: Integer;');
    SL.Add('    x: Integer;');
    SL.Add('    y: Integer;');
    SL.Add('    token: Integer;');
    SL.Add('    annotationLinesAdded: TSciPosition;');
    SL.Add('    updated: Integer;');
    SL.Add('  end;');

    SL.SaveToFile(FileName);
    WriteLn('Gerado: ' + FileName);
  finally
    SL.Free;
  end;
end;

procedure TScintillaWrapperGenerator.GenerateEventsInc;
var
  SL: TStringList;
  FileName: string;
  I: Integer;
begin
  if Length(FParser.Events) = 0 then Exit;

  FileName := IncludeTrailingPathDelimiter(FOutputPath) + 'Scintilla.Events.inc';

  SL := TStringList.Create;
  try
    SL.Add('{ Scintilla Event Types and Handlers }');
    SL.Add('{ Generated from Scintilla.iface }');
    SL.Add('');

    // Tipos de eventos
    SL.Add('type');
    SL.Add('  TScintillaNotifyEvent = procedure(Sender: TObject; const SCN: TSCNotification) of object;');
    SL.Add('  TScintillaCharAddedEvent = procedure(Sender: TObject; Ch: AnsiChar) of object;');
    SL.Add('  TScintillaPositionEvent = procedure(Sender: TObject; Position: TSciPosition) of object;');
    SL.Add('  TScintillaModificationEvent = procedure(Sender: TObject; const SCN: TSCNotification; const Text: AnsiString) of object;');
    SL.Add('');

    // Declarações de eventos privados
    SL.Add('private');
    SL.Add('  { Event handlers }');
    for I := 0 to Length(FParser.Events) - 1 do
    begin
      SL.Add(Format('  FOn%s: TScintillaNotifyEvent;', [FParser.Events[I].Name]));
    end;
    SL.Add('');

    // Properties publicadas
    SL.Add('published');
    SL.Add('  { Events }');
    for I := 0 to Length(FParser.Events) - 1 do
    begin
      SL.Add(Format('  property On%s: TScintillaNotifyEvent read FOn%s write FOn%s;',
        [FParser.Events[I].Name, FParser.Events[I].Name, FParser.Events[I].Name]));
    end;

    SL.SaveToFile(FileName);
    WriteLn('Gerado: ' + FileName);
  finally
    SL.Free;
  end;
end;

function TScintillaWrapperGenerator.FindPropertyPairs: TStringList;
var
  I, J: Integer;
  PropName, GetterName, SetterName: string;
  PropInfo: string;
begin
  Result := TStringList.Create;

  for I := 0 to Length(FParser.Functions) - 1 do
  begin
    if (FParser.Functions[I].FuncType = 'get') and
       (StartsStr('Get', FParser.Functions[I].Name)) and
       (Length(FParser.Functions[I].Params) = 0) and
       (FParser.Functions[I].ReturnType <> '') then
    begin
      GetterName := FParser.Functions[I].Name;
      PropName := Copy(GetterName, 4, Length(GetterName));
      SetterName := 'Set' + PropName;

      // Procura setter correspondente
      for J := 0 to Length(FParser.Functions) - 1 do
      begin
        if (FParser.Functions[J].FuncType = 'set') and
           (FParser.Functions[J].Name = SetterName) then
        begin
          // Formato: PropName=Getter,Setter,Type
          PropInfo := Format('%s=%s,%s,%s',
            [PropName, GetterName, SetterName, FParser.Functions[I].ReturnType]);
          Result.Add(PropInfo);
          Break;
        end;
      end;
    end;
  end;
end;

function TScintillaWrapperGenerator.GeneratePropertyDeclaration(
  const PropName, GetterName, SetterName, PropType: string): string;
begin
  if SetterName <> '' then
    Result := Format('    property %s: %s read %s write %s;',
      [PropName, PropType, GetterName, SetterName])
  else
    Result := Format('    property %s: %s read %s;',
      [PropName, PropType, GetterName]);
end;

procedure TScintillaWrapperGenerator.GenerateMainUnit;
var
  SL: TStringList;
  I, J: Integer;
  FileName, ParamStr, WParam, LParam: string;
  Props: TStringList;
begin
  FileName := IncludeTrailingPathDelimiter(FOutputPath) + FUnitTargetName + '.pas';
  Props := nil;

  SL := TStringList.Create;
  try
    // Cabeçalho
    SL.Add('unit ' + FUnitTargetName + ';');
    SL.Add('');
    SL.Add('{');
    SL.Add('  Delphi Wrapper for Scintilla');
    SL.Add('  Generated from Scintilla.iface');
    SL.Add('  Generation date: ' + DateTimeToStr(Now));
    SL.Add('}');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;');
    SL.Add('');

    // Includes
    if FSplitFiles then
    begin
      SL.Add('const');
      SL.Add('  {$I Scintilla.Consts.inc}');
      SL.Add('');
      SL.Add('{$I Scintilla.Types.inc}');
      SL.Add('');
    end;

    // Classe
    SL.Add('type');
    SL.Add('  TScintilla = class(TCustomControl)');
    SL.Add('  private');
    SL.Add('    FDirectPtr: Pointer;');
    SL.Add('    FDirectFunction: Pointer;');

    if FGenerateEvents and FSplitFiles then
      SL.Add('    {$I Scintilla.Events.inc}');

    SL.Add('  protected');
    SL.Add('    procedure CreateWnd; override;');
    SL.Add('    procedure DestroyWnd; override;');
    SL.Add('    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;');
    SL.Add('    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;');
    SL.Add('    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;');
    SL.Add('    function SendEditor(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;');
    SL.Add('  public');
    SL.Add('    constructor Create(AOwner: TComponent); override;');
    SL.Add('    destructor Destroy; override;');
    SL.Add('');

    // Funções públicas
    SL.Add('    { Scintilla Methods }');
    for I := 0 to Length(FParser.Functions) - 1 do
    begin
      ParamStr := '';
      for J := 0 to Length(FParser.Functions[I].Params) - 1 do
      begin
        if FParser.Functions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FParser.Functions[I].Params[J].ParamName + ': ' +
                      FParser.Functions[I].Params[J].ParamType;
        end;
      end;

      if FParser.Functions[I].ReturnType <> '' then
        SL.Add(Format('    function %s(%s): %s;',
          [FParser.Functions[I].Name, ParamStr, FParser.Functions[I].ReturnType]))
      else
        SL.Add(Format('    procedure %s(%s);',
          [FParser.Functions[I].Name, ParamStr]));
    end;

    // Properties
    if FGenerateProperties then
    begin
      Props := FindPropertyPairs;
      if Props.Count > 0 then
      begin
        SL.Add('');
        SL.Add('  published');
        SL.Add('    { Properties }');
        for I := 0 to Props.Count - 1 do
        begin
          // Parse property info
          var Parts := Props[I].Split(['=', ',']);
          if Length(Parts) >= 4 then
            SL.Add(GeneratePropertyDeclaration(Parts[0], Parts[1], Parts[2], Parts[3]));
        end;
      end;
    end;

    SL.Add('  end;');
    SL.Add('');
    SL.Add('procedure Register;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');

    // DLL
    SL.Add('const');
    SL.Add('  ScintillaDefaultDLL = ''SciLexer.dll'';');
    SL.Add('');

    // Implementação
    SL.Add('procedure Register;');
    SL.Add('begin');
    SL.Add('  RegisterComponents(''Scintilla'', [TScintilla]);');
    SL.Add('end;');
    SL.Add('');

    // Métodos básicos
    SL.Add('{ TScintilla }');
    SL.Add('');
    SL.Add('constructor TScintilla.Create(AOwner: TComponent);');
    SL.Add('begin');
    SL.Add('  inherited Create(AOwner);');
    SL.Add('  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks];');
    SL.Add('  Width := 300;');
    SL.Add('  Height := 200;');
    SL.Add('  TabStop := True;');
    SL.Add('  FDirectPtr := nil;');
    SL.Add('  FDirectFunction := nil;');
    SL.Add('end;');
    SL.Add('');

    // Implementações das funções
    for I := 0 to Length(FParser.Functions) - 1 do
    begin
      ParamStr := '';
      WParam := '0';
      LParam := '0';

      for J := 0 to Length(FParser.Functions[I].Params) - 1 do
      begin
        if FParser.Functions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FParser.Functions[I].Params[J].ParamName + ': ' +
                      FParser.Functions[I].Params[J].ParamType;

          // Define WParam e LParam
          if J = 0 then
            WParam := FParser.Functions[I].Params[J].ParamName
          else if J = 1 then
            LParam := 'LPARAM(' + FParser.Functions[I].Params[J].ParamName + ')';
        end;
      end;

      SL.Add('');
      if FParser.Functions[I].ReturnType <> '' then
      begin
        SL.Add(Format('function TScintilla.%s(%s): %s;',
          [FParser.Functions[I].Name, ParamStr, FParser.Functions[I].ReturnType]));
        SL.Add('begin');
        SL.Add(Format('  Result := SendEditor(%s, %s, %s);',
          [FParser.Functions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end
      else
      begin
        SL.Add(Format('procedure TScintilla.%s(%s);',
          [FParser.Functions[I].Name, ParamStr]));
        SL.Add('begin');
        SL.Add(Format('  SendEditor(%s, %s, %s);',
          [FParser.Functions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end;
    end;

    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(FileName);
    WriteLn('Gerado: ' + FileName);
  finally
    SL.Free;
    Props.Free;
  end;
end;

procedure TScintillaWrapperGenerator.GenerateDocumentation;
var
  SL: TStringList;
  I, J: Integer;
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(FOutputPath) + 'ScintillaAPI.html';

  SL := TStringList.Create;
  try
    SL.Add('<!DOCTYPE html>');
    SL.Add('<html>');
    SL.Add('<head>');
    SL.Add('  <title>Scintilla Delphi API</title>');
    SL.Add('  <style>');
    SL.Add('    body { font-family: Arial, sans-serif; }');
    SL.Add('    .function { margin: 10px 0; padding: 10px; background: #f0f0f0; }');
    SL.Add('    .constant { margin: 5px 0; }');
    SL.Add('    h2 { color: #333; }');
    SL.Add('  </style>');
    SL.Add('</head>');
    SL.Add('<body>');
    SL.Add('  <h1>Scintilla Delphi Wrapper API</h1>');

    // Constantes
    SL.Add('  <h2>Constants</h2>');
    for I := 0 to Length(FParser.Constants) - 1 do
    begin
      SL.Add(Format('  <div class="constant"><b>%s</b> = %s</div>',
        [FParser.Constants[I].Name, FParser.Constants[I].Value]));
    end;

    // Funções
    SL.Add('  <h2>Functions</h2>');
    for I := 0 to Length(FParser.Functions) - 1 do
    begin
      SL.Add('  <div class="function">');
      SL.Add(Format('    <b>%s</b> (Message: %s)<br>',
        [FParser.Functions[I].Name, FParser.Functions[I].MessageID]));
      SL.Add('    Parameters: ');
      for J := 0 to Length(FParser.Functions[I].Params) - 1 do
      begin
        if FParser.Functions[I].Params[J].ParamName <> '' then
          SL.Add(Format('%s: %s ',
            [FParser.Functions[I].Params[J].ParamName,
             FParser.Functions[I].Params[J].ParamType]));
      end;
      if FParser.Functions[I].ReturnType <> '' then
        SL.Add('<br>Returns: ' + FParser.Functions[I].ReturnType);
      SL.Add('  </div>');
    end;

    SL.Add('</body>');
    SL.Add('</html>');

    SL.SaveToFile(FileName);
    WriteLn('Gerado: ' + FileName);
  finally
    SL.Free;
  end;
end;

procedure TScintillaWrapperGenerator.GenerateWrapper;
begin
  if FSplitFiles then
  begin
    GenerateConstantsInc;
    GenerateTypesInc;
    if FGenerateEvents then
      GenerateEventsInc;
  end;

  GenerateMainUnit;
  GenerateDocumentation;
end;

// Programa principal
// ScintillaWrapperBuilderApp. Scintilla.iface -o C:\Output -u MyScintilla -single
var
  Generator: TScintillaWrapperGenerator;
  InputFile: string;
begin
  WriteLn('Scintilla Wrapper Generator v2.0');
  WriteLn('=================================');
  WriteLn('');

  if ParamCount < 1 then
  begin
    WriteLn('Uso: ScintillaWrapperBuilder <arquivo.iface> [opcoes]');
    WriteLn('');
    WriteLn('Opcoes:');
    WriteLn('  -o <path>     Diretório de saída');
    WriteLn('  -u <name>     Nome da unit (padrão: DScintilla)');
    WriteLn('  -single       Gera arquivo único (não split)');
    WriteLn('  -noprops      Não gera properties');
    WriteLn('  -noevents     Não gera eventos');
    Exit;
  end;

  InputFile := ParamStr(1);
  if not FileExists(InputFile) then
  begin
    WriteLn('Erro: Arquivo não encontrado: ' + InputFile);
    Exit;
  end;

  Generator := TScintillaWrapperGenerator.Create;
  try
    // Parse argumentos
    var I: Integer;
    I := 2;
    while I <= ParamCount do
    begin
      if ParamStr(I) = '-o' then
      begin
        Inc(I);
        if I <= ParamCount then
          Generator.OutputPath := ParamStr(I);
      end
      else if ParamStr(I) = '-u' then
      begin
        Inc(I);
        if I <= ParamCount then
          Generator.UnitTargetName := ParamStr(I);
      end
      else if ParamStr(I) = '-single' then
        Generator.SplitFiles := False
      else if ParamStr(I) = '-noprops' then
        Generator.GenerateProperties := False
      else if ParamStr(I) = '-noevents' then
        Generator.GenerateEvents := False;

      Inc(I);
    end;

    WriteLn('Carregando: ' + InputFile);
    Generator.LoadIfaceFile(InputFile);

    WriteLn('Gerando arquivos...');
    Generator.GenerateWrapper;

    WriteLn('');
    WriteLn('Concluído com sucesso!');
  finally
    Generator.Free;
  end;

  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.
