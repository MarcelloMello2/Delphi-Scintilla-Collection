unit Seven.Scintilla.IfaceParser;

interface

uses
  SysUtils, Classes, StrUtils;

type
  TParamInfo = record
    ParamType: string;
    ParamName: string;
  end;

  TFunctionInfo = record
    FuncType: string;
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

  TScintillaParser = class
  private
    FFunctions: TArray<TFunctionInfo>;
    FConstants: TArray<TConstantInfo>;
    FEvents: TArray<TEventInfo>;
    FCurrentCategory: string;
    FCurrentEnum: string;

    function ExtractBetween(const S, StartDelim, EndDelim: string;
      StartPos: Integer = 1): string;
    function SplitParams(const ParamStr: string): TStringList;
    function ConvertType(const CppType: string): string;

    function ParseFunctionLine(const Line: string): Boolean;
    function ParseConstantLine(const Line: string): Boolean;
    function ParseEnumLine(const Line: string): Boolean;
    function ParseEventLine(const Line: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseLine(const Line: string);
    procedure ParseFile(const FileName: string);

    property Functions: TArray<TFunctionInfo> read FFunctions;
    property Constants: TArray<TConstantInfo> read FConstants;
    property Events: TArray<TEventInfo> read FEvents;
  end;

implementation

constructor TScintillaParser.Create;
begin
  inherited;
  Clear;
end;

destructor TScintillaParser.Destroy;
begin
  Clear;
  inherited;
end;

procedure TScintillaParser.Clear;
begin
  SetLength(FFunctions, 0);
  SetLength(FConstants, 0);
  SetLength(FEvents, 0);
  FCurrentCategory := '';
  FCurrentEnum := '';
end;

function TScintillaParser.ExtractBetween(const S, StartDelim, EndDelim: string;
  StartPos: Integer): string;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := PosEx(StartDelim, S, StartPos);
  if P1 > 0 then
  begin
    Inc(P1, Length(StartDelim));
    P2 := PosEx(EndDelim, S, P1);
    if P2 > 0 then
      Result := Copy(S, P1, P2 - P1);
  end;
end;

function TScintillaParser.SplitParams(const ParamStr: string): TStringList;
var
  I, ParenLevel: Integer;
  CurrentParam: string;
begin
  Result := TStringList.Create;
  CurrentParam := '';
  ParenLevel := 0;

  for I := 1 to Length(ParamStr) do
  begin
    case ParamStr[I] of
      '(': Inc(ParenLevel);
      ')': Dec(ParenLevel);
      ',': if ParenLevel = 0 then
           begin
             Result.Add(Trim(CurrentParam));
             CurrentParam := '';
             Continue;
           end;
    end;
    CurrentParam := CurrentParam + ParamStr[I];
  end;

  if Trim(CurrentParam) <> '' then
    Result.Add(Trim(CurrentParam));
end;

function TScintillaParser.ConvertType(const CppType: string): string;
var
  CleanType: string;
begin
  CleanType := Trim(CppType);

  if CleanType = 'void' then Result := ''
  else if CleanType = 'int' then Result := 'Integer'
  else if CleanType = 'bool' then Result := 'Boolean'
  else if CleanType = 'position' then Result := 'Integer'
  else if CleanType = 'colour' then Result := 'TColor'
  else if CleanType = 'string' then Result := 'PAnsiChar'
  else if CleanType = 'stringresult' then Result := 'PAnsiChar'
  else if CleanType = 'cells' then Result := 'PAnsiChar'
  else if CleanType = 'textrange' then Result := 'PTextRange'
  else if CleanType = 'findtext' then Result := 'PFindText'
  else if CleanType = 'keymod' then Result := 'Integer'
  else if CleanType = 'formatrange' then Result := 'PFormatRange'
  else if CleanType = 'char' then Result := 'AnsiChar'
  else if CleanType = 'long' then Result := 'LongInt'
  else if CleanType = 'uptr_t' then Result := 'NativeUInt'
  else if CleanType = 'sptr_t' then Result := 'NativeInt'
  else if CleanType = 'line' then Result := 'Integer'
  else Result := 'Integer';
end;

function TScintillaParser.ParseFunctionLine(const Line: string): Boolean;
var
  FuncInfo: TFunctionInfo;
  Words: TStringList;
  RestOfLine, ParamsStr: string;
  EqualPos, ParenPos, SpacePos: Integer;
  ParamList: TStringList;
  I: Integer;
  Param: string;
begin
  Result := False;

  // Verifica se é uma linha de função
  if not (StartsStr('fun ', Line) or StartsStr('get ', Line) or
          StartsStr('set ', Line)) then
    Exit;

  Words := TStringList.Create;
  ParamList := nil;
  try
    // Extrai o tipo de função (fun, get, set)
    SpacePos := Pos(' ', Line);
    if SpacePos = 0 then Exit;

    FuncInfo.FuncType := Copy(Line, 1, SpacePos - 1);
    RestOfLine := Copy(Line, SpacePos + 1, Length(Line));

    // Encontra a posição do '='
    EqualPos := Pos('=', RestOfLine);
    if EqualPos = 0 then Exit;

    // Parte antes do '=' contém tipo de retorno e nome
    Words.Delimiter := ' ';
    Words.DelimitedText := Trim(Copy(RestOfLine, 1, EqualPos - 1));

    if Words.Count < 2 then Exit;

    FuncInfo.ReturnType := ConvertType(Words[0]);
    FuncInfo.Name := Words[1];

    // Extrai o ID da mensagem
    RestOfLine := Copy(RestOfLine, EqualPos + 1, Length(RestOfLine));
    ParenPos := Pos('(', RestOfLine);
    if ParenPos = 0 then Exit;

    FuncInfo.MessageID := Trim(Copy(RestOfLine, 1, ParenPos - 1));

    // Extrai os parâmetros
    ParamsStr := ExtractBetween(RestOfLine, '(', ')');
    ParamList := SplitParams(ParamsStr);

    SetLength(FuncInfo.Params, ParamList.Count);
    for I := 0 to ParamList.Count - 1 do
    begin
      Param := Trim(ParamList[I]);
      if Param <> '' then
      begin
        SpacePos := LastDelimiter(' ', Param);
        if SpacePos > 0 then
        begin
          FuncInfo.Params[I].ParamType := ConvertType(Copy(Param, 1, SpacePos - 1));
          FuncInfo.Params[I].ParamName := Copy(Param, SpacePos + 1, Length(Param));
        end
        else
        begin
          FuncInfo.Params[I].ParamType := ConvertType(Param);
          FuncInfo.Params[I].ParamName := '';
        end;
      end;
    end;

    FuncInfo.Category := FCurrentCategory;

    // Adiciona à lista
    SetLength(FFunctions, Length(FFunctions) + 1);
    FFunctions[High(FFunctions)] := FuncInfo;

    Result := True;
  finally
    Words.Free;
    ParamList.Free;
  end;
end;

function TScintillaParser.ParseConstantLine(const Line: string): Boolean;
var
  ConstInfo: TConstantInfo;
  EqualPos: Integer;
begin
  Result := False;

  if not StartsStr('val ', Line) then
    Exit;

  EqualPos := Pos('=', Line);
  if EqualPos = 0 then Exit;

  ConstInfo.Name := Trim(Copy(Line, 5, EqualPos - 5));
  ConstInfo.Value := Trim(Copy(Line, EqualPos + 1, Length(Line)));
  ConstInfo.EnumName := FCurrentEnum;

  SetLength(FConstants, Length(FConstants) + 1);
  FConstants[High(FConstants)] := ConstInfo;

  Result := True;
end;

function TScintillaParser.ParseEnumLine(const Line: string): Boolean;
var
  EqualPos: Integer;
begin
  Result := False;

  if not StartsStr('enu ', Line) then
    Exit;

  EqualPos := Pos('=', Line);
  if EqualPos > 0 then
  begin
    FCurrentEnum := Trim(Copy(Line, 5, EqualPos - 5));
    Result := True;
  end;
end;

function TScintillaParser.ParseEventLine(const Line: string): Boolean;
var
  EventInfo: TEventInfo;
  Words: TStringList;
  RestOfLine: string;
  EqualPos, ParenPos, SpacePos: Integer;
begin
  Result := False;

  if not StartsStr('evt ', Line) then
    Exit;

  Words := TStringList.Create;
  try
    SpacePos := Pos(' ', Line);
    if SpacePos = 0 then Exit;

    RestOfLine := Copy(Line, SpacePos + 1, Length(Line));

    EqualPos := Pos('=', RestOfLine);
    if EqualPos = 0 then Exit;

    Words.Delimiter := ' ';
    Words.DelimitedText := Trim(Copy(RestOfLine, 1, EqualPos - 1));

    if Words.Count < 2 then Exit;

    EventInfo.ReturnType := Words[0];
    EventInfo.Name := Words[1];

    RestOfLine := Copy(RestOfLine, EqualPos + 1, Length(RestOfLine));
    ParenPos := Pos('(', RestOfLine);
    if ParenPos = 0 then Exit;

    EventInfo.EventID := Trim(Copy(RestOfLine, 1, ParenPos - 1));
    EventInfo.Params := ExtractBetween(RestOfLine, '(', ')');

    SetLength(FEvents, Length(FEvents) + 1);
    FEvents[High(FEvents)] := EventInfo;

    Result := True;
  finally
    Words.Free;
  end;
end;

procedure TScintillaParser.ParseLine(const Line: string);
var
  TrimmedLine: string;
begin
  TrimmedLine := Trim(Line);

  // Ignora linhas vazias e comentários
  if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
    Exit;

  // Categoria
  if StartsStr('cat ', TrimmedLine) then
  begin
    FCurrentCategory := Copy(TrimmedLine, 5, Length(TrimmedLine));
    Exit;
  end;

  // Tenta fazer parse de cada tipo
  if not ParseFunctionLine(TrimmedLine) then
    if not ParseConstantLine(TrimmedLine) then
      if not ParseEnumLine(TrimmedLine) then
        ParseEventLine(TrimmedLine);
end;

procedure TScintillaParser.ParseFile(const FileName: string);
var
  FileLines: TStringList;
  I: Integer;
begin
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FileName);

    for I := 0 to FileLines.Count - 1 do
      ParseLine(FileLines[I]);
  finally
    FileLines.Free;
  end;
end;

end.
