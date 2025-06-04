unit Seven.Scintilla.IfaceGenerator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.IOUtils,
  System.Character,
  System.RegularExpressions;


type
  TParamInfo = record
    ParamType: string;
    ParamName: string;
  end;

  TFunctionInfo = record
    FuncType: string;      // fun, get, set
    ReturnType: string;
    Name: string;
    MessageID: string;
    Params: TArray<TParamInfo>;
    Category: string;
  end;

  TConstantInfo = record
    Name: string;
    Value: string;
    EnumName: string;
  end;

  TEventInfo = record
    ReturnType: string;
    Name: string;
    EventID: string;
    Params: string;
  end;

  TScintillaGenerator = class
  private
    FFunctions: array of TFunctionInfo;
    FConstants: array of TConstantInfo;
    FEvents: array of TEventInfo;
    FCurrentCategory: string;
    FCurrentEnum: string;

    function ConvertType(const CppType: string): string;
    function ParseFunctionLine(const Line: string): Boolean;
    function ParseConstantLine(const Line: string): Boolean;
    function ParseEnumLine(const Line: string): Boolean;
    function ParseEventLine(const Line: string): Boolean;
    procedure ParseParameters(const ParamStr: string; var Params: TArray<TParamInfo>);

    function GenerateConstants: string;
    function GenerateFunctionDeclarations: string;
    function GenerateFunctionImplementations: string;
    function GenerateProperties: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseIfaceFile(const FileName: string);
    procedure GenerateDelphiUnit(const OutputFile: string);
  end;

implementation

constructor TScintillaGenerator.Create;
begin
  inherited;
  SetLength(FFunctions, 0);
  SetLength(FConstants, 0);
  SetLength(FEvents, 0);
  FCurrentCategory := '';
  FCurrentEnum := '';
end;

destructor TScintillaGenerator.Destroy;
begin
  SetLength(FFunctions, 0);
  SetLength(FConstants, 0);
  SetLength(FEvents, 0);
  inherited;
end;

function TScintillaGenerator.ConvertType(const CppType: string): string;
begin
  if CppType = 'void' then Result := ''
  else if CppType = 'int' then Result := 'Integer'
  else if CppType = 'bool' then Result := 'Boolean'
  else if CppType = 'position' then Result := 'Integer'
  else if CppType = 'colour' then Result := 'TColor'
  else if CppType = 'string' then Result := 'PAnsiChar'
  else if CppType = 'stringresult' then Result := 'PAnsiChar'
  else if CppType = 'cells' then Result := 'PAnsiChar'
  else if CppType = 'textrange' then Result := 'PSciTextRange'
  else if CppType = 'findtext' then Result := 'PSciFindText'
  else if CppType = 'keymod' then Result := 'Integer'
  else if CppType = 'formatrange' then Result := 'PSciFormatRange'
  else Result := 'Integer'; // Padrão
end;

procedure TScintillaGenerator.ParseParameters(const ParamStr: string; var Params: TArray<TParamInfo>);
var
  ParamList: TStringList;
  I, SpacePos: Integer;
  Param, PType, PName: string;
begin
  ParamList := TStringList.Create;
  try
    ParamList.Delimiter := ',';
    ParamList.StrictDelimiter := True;
    ParamList.DelimitedText := ParamStr;

    SetLength(Params, ParamList.Count);

    for I := 0 to ParamList.Count - 1 do
    begin
      Param := Trim(ParamList[I]);
      if Param <> '' then
      begin
        SpacePos := LastDelimiter(' ', Param);
        if SpacePos > 0 then
        begin
          PType := Copy(Param, 1, SpacePos - 1);

          PName := Trim(Copy(Param, SpacePos + 1, Length(Param)));
          PName[1] := PName[1].ToUpper();

          if Length(PName) > 1 then
            PName := 'A' + PName;


          Params[I].ParamType := ConvertType(Trim(PType));
          Params[I].ParamName := PName;
        end
        else
        begin
          Params[I].ParamType := ConvertType(Param);
          Params[I].ParamName := '';
        end;
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

function TScintillaGenerator.ParseFunctionLine(const Line: string): Boolean;
var
  Match: TMatch;
  FuncInfo: TFunctionInfo;
  Index: Integer;
begin
  Result := False;

  // Pattern: (fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)
  Match := TRegEx.Match(Line, '(fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)');

  if Match.Success then
  begin
    FuncInfo.FuncType := Match.Groups[1].Value;
    FuncInfo.ReturnType := ConvertType(Match.Groups[2].Value);
    FuncInfo.Name := Match.Groups[3].Value;
    FuncInfo.MessageID := Match.Groups[4].Value;
    FuncInfo.Category := FCurrentCategory;

    // Parse parameters
    ParseParameters(Match.Groups[5].Value, FuncInfo.Params);

    // Adiciona à lista
    Index := Length(FFunctions);
    SetLength(FFunctions, Index + 1);
    FFunctions[Index] := FuncInfo;

    Result := True;
  end;
end;

function TScintillaGenerator.ParseConstantLine(const Line: string): Boolean;
var
  Match: TMatch;
  ConstInfo: TConstantInfo;
  Index: Integer;
begin
  Result := False;

  // Pattern: val\s+(\w+)=(.+)
  Match := TRegEx.Match(Line, 'val\s+(\w+)=(.+)');

  if Match.Success then
  begin
    ConstInfo.Name := Match.Groups[1].Value;
    ConstInfo.Value := Match.Groups[2].Value;
    ConstInfo.EnumName := FCurrentEnum;

    Index := Length(FConstants);
    SetLength(FConstants, Index + 1);
    FConstants[Index] := ConstInfo;

    Result := True;
  end;
end;

function TScintillaGenerator.ParseEnumLine(const Line: string): Boolean;
var
  Match: TMatch;
begin
  Result := False;

  // Pattern: enu\s+(\w+)=(\w+)
  Match := TRegEx.Match(Line, 'enu\s+(\w+)=(\w+)');

  if Match.Success then
  begin
    FCurrentEnum := Match.Groups[1].Value;
    Result := True;
  end;
end;

function TScintillaGenerator.ParseEventLine(const Line: string): Boolean;
var
  Match: TMatch;
  EventInfo: TEventInfo;
  Index: Integer;
begin
  Result := False;

  // Pattern: evt\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)
  Match := TRegEx.Match(Line, 'evt\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)');

  if Match.Success then
  begin
    EventInfo.ReturnType := Match.Groups[1].Value;
    EventInfo.Name := Match.Groups[2].Value;
    EventInfo.EventID := Match.Groups[3].Value;
    EventInfo.Params := Match.Groups[4].Value;

    Index := Length(FEvents);
    SetLength(FEvents, Index + 1);
    FEvents[Index] := EventInfo;

    Result := True;
  end;
end;

procedure TScintillaGenerator.ParseIfaceFile(const FileName: string);
var
  FileLines: TStringList;
  I: Integer;
  Line: string;
begin
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FileName);

    for I := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[I]);

      // Ignora linhas vazias e comentários
      if (Line = '') or (Line[1] = '#') then
        Continue;

      // Categoria
      if StartsStr('cat ', Line) then
      begin
        FCurrentCategory := Copy(Line, 5, Length(Line));
        Continue;
      end;

      // Tenta fazer parse de cada tipo
      if StartsStr('fun ', Line) or StartsStr('get ', Line) or StartsStr('set ', Line) then
        ParseFunctionLine(Line)
      else if StartsStr('val ', Line) then
        ParseConstantLine(Line)
      else if StartsStr('enu ', Line) then
        ParseEnumLine(Line)
      else if StartsStr('evt ', Line) then
        ParseEventLine(Line);
    end;

    WriteLn(Format('Parseado: %d funções, %d constantes, %d eventos',
      [Length(FFunctions), Length(FConstants), Length(FEvents)]));
  finally
    FileLines.Free;
  end;
end;

function TScintillaGenerator.GenerateConstants: string;
var
  SL: TStringList;
  I: Integer;
  LastEnum: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('const');

    LastEnum := '';
    for I := 0 to Length(FConstants) - 1 do
    begin
      // Adiciona comentário para nova enumeração
      if (FConstants[I].EnumName <> '') and (FConstants[I].EnumName <> LastEnum) then
      begin
        LastEnum := FConstants[I].EnumName;
        SL.Add('');
        SL.Add('  // ' + LastEnum);
      end;

      SL.Add(Format('  %s = %s;', [FConstants[I].Name, FConstants[I].Value.Replace('0x', '$')]));
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TScintillaGenerator.GenerateFunctionDeclarations: string;
var
  SL: TStringList;
  I, J: Integer;
  ParamStr: string;
  LastCategory: string;
begin
  SL := TStringList.Create;
  try
    LastCategory := '';

    for I := 0 to Length(FFunctions) - 1 do
    begin
      // Adiciona comentário de categoria
      if FFunctions[I].Category <> LastCategory then
      begin
        LastCategory := FFunctions[I].Category;
        SL.Add('');
        SL.Add('    // ' + LastCategory);
      end;

      // Monta string de parâmetros
      ParamStr := '';
      for J := 0 to Length(FFunctions[I].Params) - 1 do
      begin
        if FFunctions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FFunctions[I].Params[J].ParamName + ': ' +
                      FFunctions[I].Params[J].ParamType;
        end;
      end;

      // Gera declaração
      if FFunctions[I].ReturnType <> '' then
        SL.Add(Format('    function %s(%s): %s;',
          [FFunctions[I].Name, ParamStr, FFunctions[I].ReturnType]))
      else
        SL.Add(Format('    procedure %s(%s);',
          [FFunctions[I].Name, ParamStr]));
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TScintillaGenerator.GenerateFunctionImplementations: string;
var
  SL: TStringList;
  I, J: Integer;
  ParamStr, WParam, LParam: string;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Length(FFunctions) - 1 do
    begin
      // Monta string de parâmetros
      ParamStr := '';
      for J := 0 to Length(FFunctions[I].Params) - 1 do
      begin
        if FFunctions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FFunctions[I].Params[J].ParamName + ': ' +
                      FFunctions[I].Params[J].ParamType;
        end;
      end;

      // Determina WParam e LParam
      WParam := '0';
      LParam := '0';

      if Length(FFunctions[I].Params) >= 1 then
        if FFunctions[I].Params[0].ParamName <> '' then
          WParam := 'WPARAM(' + FFunctions[I].Params[0].ParamName + ')';

      if Length(FFunctions[I].Params) >= 2 then
        if FFunctions[I].Params[1].ParamName <> '' then
          LParam := 'LPARAM(' + FFunctions[I].Params[1].ParamName + ')';

      // Gera implementação
      SL.Add('');
      if FFunctions[I].ReturnType <> '' then
      begin
        SL.Add(Format('function TCustomSciTextEditor.%s(%s): %s;', [FFunctions[I].Name, ParamStr, FFunctions[I].ReturnType]));
        SL.Add('begin');

        if SameText(FFunctions[I].ReturnType, 'Boolean') then
          SL.Add(Format('  Result := Boolean(SendScintillaEditorMessage(%s, %s, %s));', [FFunctions[I].MessageID, WParam, LParam]))
        else
          SL.Add(Format('  Result := SendScintillaEditorMessage(%s, %s, %s);', [FFunctions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end
      else
      begin
        SL.Add(Format('procedure TCustomSciTextEditor.%s(%s);',
          [FFunctions[I].Name, ParamStr]));
        SL.Add('begin');
        SL.Add(Format('  SendScintillaEditorMessage(%s, %s, %s);',
          [FFunctions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end;
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TScintillaGenerator.GenerateProperties: string;
var
  SL: TStringList;
  I, J: Integer;
  PropName, GetterName, SetterName: string;
  HasGetter, HasSetter: Boolean;
  GetterFunc, SetterFunc: TFunctionInfo;
begin
  SL := TStringList.Create;
  try
    SL.Add('  published');
    SL.Add('    // Properties geradas automaticamente de funções Get/Set');

    // Procura pares de get/set
    for I := 0 to Length(FFunctions) - 1 do
    begin
      if (FFunctions[I].FuncType = 'get') and
         (StartsStr('Get', FFunctions[I].Name)) then
      begin
        GetterFunc := FFunctions[I];
        GetterName := GetterFunc.Name;
        PropName := Copy(GetterName, 4, Length(GetterName)); // Remove 'Get'

        // Procura setter correspondente
        SetterName := 'Set' + PropName;
        HasSetter := False;

        for J := 0 to Length(FFunctions) - 1 do
        begin
          if (FFunctions[J].FuncType = 'set') and
             (FFunctions[J].Name = SetterName) then
          begin
            SetterFunc := FFunctions[J];
            HasSetter := True;
            Break;
          end;
        end;

        // Se não tem parâmetros no getter e tem tipo de retorno
        if (Length(GetterFunc.Params) = 0) and (GetterFunc.ReturnType <> '') then
        begin
          if HasSetter then
            SL.Add(Format('    property %s: %s read %s write %s;',
              [PropName, GetterFunc.ReturnType, GetterName, SetterName]))
          else
            SL.Add(Format('    property %s: %s read %s;',
              [PropName, GetterFunc.ReturnType, GetterName]));
        end;
      end;
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TScintillaGenerator.GenerateDelphiUnit(const OutputFile: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Format('unit %s;', [TPath.GetFileNameWithoutExtension(OutputFile)]));
    SL.Add('');
    SL.Add('{');
    SL.Add('  Wrapper Delphi para Scintilla');
    SL.Add('  Gerado automaticamente por ' + ExtractFileName(ParamStr(0)));
    SL.Add('  Data: ' + DateTimeToStr(Now));
    SL.Add('}');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Winapi.Windows,');
    SL.Add('  Winapi.Messages,');
    SL.Add('  System.SysUtils,');
    SL.Add('  System.Classes,');
    SL.Add('  Vcl.Graphics,');
    SL.Add('  Vcl.Controls,');
    SL.Add('  Seven.Scintilla.SciTypes,');
    SL.Add('  Seven.Scintilla.BaseTextEditor;');
    SL.Add('');

    // Constantes
    SL.Add(GenerateConstants);
    SL.Add('');

    // Tipos
    SL.Add('type');
    SL.Add('  // Tipos auxiliares');
    SL.Add('');
    SL.Add('  TSciCharRange = Record');
    SL.Add('    cpMin : Longint;');
    SL.Add('	  cpMax : Longint;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('  PSciTextRange = ^TSciTextRange;');
    SL.Add('  TSciTextRange = record');
    SL.Add('    chrg: TSciCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('  PSciFindText = ^TSciFindText;');
    SL.Add('  TSciFindText = record');
    SL.Add('    chrg: TSciCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('  PSciFormatRange = ^TSciFormatRange;');
    SL.Add('  TSciFormatRange = record');
    SL.Add('    hdc: HDC;');
    SL.Add('    hdcTarget: HDC;');
    SL.Add('    rc: TRect;');
    SL.Add('    rcPage: TRect;');
    SL.Add('    chrg: TSciCharRange;');
    SL.Add('  end;');
    SL.Add('');

    // Eventos
    if Length(FEvents) > 0 then
    begin
      SL.Add('  // Tipos de eventos');
      SL.Add('  TSciCharEvent = procedure(Sender: TObject; Ch: AnsiChar) of object;');
      SL.Add('  TSciPositionEvent = procedure(Sender: TObject; Position: Integer) of object;');
      SL.Add('');
    end;

    // Classe principal
    SL.Add('  TCustomSciTextEditor = class(TBaseSciTextEditor)');
    SL.Add('  private');

    // Eventos privados
    if Length(FEvents) > 0 then
    begin
      SL.Add('    // Eventos');
      SL.Add('    FOnStyleNeeded: TSciPositionEvent;');
      SL.Add('    FOnCharAdded: TSciCharEvent;');
      SL.Add('    FOnModifyAttemptRO: TNotifyEvent;');
      SL.Add('');
    end;

    SL.Add('  protected');
    SL.Add('    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;');
    SL.Add('  public');
    SL.Add('    constructor Create(AOwner: TComponent); override;');

    // Declarações de funções
    SL.Add(GenerateFunctionDeclarations);

    // Properties
    SL.Add('');
    SL.Add(GenerateProperties);

    // Eventos publicados
    if Length(FEvents) > 0 then
    begin
      SL.Add('');
      SL.Add('    // Eventos');
      SL.Add('    property OnStyleNeeded: TSciPositionEvent read FOnStyleNeeded write FOnStyleNeeded;');
      SL.Add('    property OnCharAdded: TSciCharEvent read FOnCharAdded write FOnCharAdded;');
      SL.Add('    property OnModifyAttemptRO: TNotifyEvent read FOnModifyAttemptRO write FOnModifyAttemptRO;');
    end;

    SL.Add('  end;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');

    // Implementações básicas
    SL.Add('constructor TCustomSciTextEditor.Create(AOwner: TComponent);');
    SL.Add('begin');
    SL.Add('  inherited Create(AOwner);');
    SL.Add('  Width := 300;');
    SL.Add('  Height := 200;');
    SL.Add('  TabStop := True;');
    SL.Add('end;');
    SL.Add('');

    SL.Add('procedure TCustomSciTextEditor.CNNotify(var Message: TWMNotify);');
    SL.Add('//var');
    SL.Add('//  SCN: PSCNotification;');
    SL.Add('begin');
    SL.Add('  inherited;');
    SL.Add('  // Implementar tratamento de notificações aqui');
    SL.Add('  {');
    SL.Add('  SCN := PSCNotification(Message.NMHdr);');
    SL.Add('  case SCN^.nmhdr.code of');
    SL.Add('    SCN_STYLENEEDED:');
    SL.Add('      if Assigned(FOnStyleNeeded) then');
    SL.Add('        FOnStyleNeeded(Self, SCN^.position);');
    SL.Add('    SCN_CHARADDED:');
    SL.Add('      if Assigned(FOnCharAdded) then');
    SL.Add('        FOnCharAdded(Self, AnsiChar(SCN^.ch));');
    SL.Add('  end;');
    SL.Add('  }');
    SL.Add('end;');

    // Implementações das funções
    SL.Add(GenerateFunctionImplementations);

    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(OutputFile, TEncoding.UTF8);
    WriteLn('Arquivo ' + GetCurrentDir() + '\' + OutputFile + ' gerado com sucesso!');
  finally
    SL.Free;
  end;
end;

end.
