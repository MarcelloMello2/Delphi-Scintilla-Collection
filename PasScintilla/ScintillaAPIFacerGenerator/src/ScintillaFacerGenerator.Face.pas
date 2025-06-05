unit ScintillaFacerGenerator.Face;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Character,
  System.Generics.Collections;

type
  TFeatureType = (ftFun, ftGet, ftSet, ftVal, ftEvt, ftEnu, ftLex, ftAli, ftCat);
  
  TParam = record
    ParamType: string;
    Name: string;
    Value: string;
  end;
  
  TFeature = class
  public
    FeatureType: TFeatureType;
    ReturnType: string;
    Name: string;
    Value: string;
    Param1: TParam;
    Param2: TParam;
    Category: string;
    Comment: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TFace = class
  private
    FOrder: TStringList;
    FFeatures: TObjectDictionary<string, TFeature>;
    FValues: TDictionary<string, Boolean>;
    FEvents: TDictionary<string, Boolean>;
    FAliases: TDictionary<string, string>;
    
    function SanitiseLine(const Line: string): string;
    procedure DecodeFunction(const FeatureVal: string; out RetType, Name, Value: string;
      out Param1, Param2: TParam);
    procedure DecodeEvent(const FeatureVal: string; out RetType, Name, Value: string);
    procedure DecodeParam(const P: string; out Param: TParam);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ReadFromFile(const FileName: string);
    
    property Order: TStringList read FOrder;
    property Features: TObjectDictionary<string, TFeature> read FFeatures;
    property Aliases: TDictionary<string, string> read FAliases;
    property Values: TDictionary<string, Boolean> read FValues;
    property Events: TDictionary<string, Boolean> read FEvents;
  end;

function IsEnumeration(const T: string): Boolean;
function PascalCase(const S: string): string;

implementation

function IsEnumeration(const T: string): Boolean;
begin
  if T = '' then
    Exit(False);
  Result := CharInSet(T[1], ['A'..'Z']);
end;

function PascalCase(const S: string): string;
var
  I: Integer;
  Capitalized: string;
  CharPrevious, Character, CharNext: Char;
begin
  // Converte para Title Case
  Capitalized := '';
  for I := 1 to Length(S) do
  begin
    if (I = 1) or (S[I-1] = '_') then
      Capitalized := Capitalized + UpCase(S[I])
    else
      Capitalized := Capitalized + LowerCase(S[I]);
  end;
  
  // Remove '_' exceto entre dígitos
  Result := '';
  CharPrevious := ' ';
  
  for I := 1 to Length(Capitalized) - 1 do
  begin
    Character := Capitalized[I];
    CharNext := Capitalized[I + 1];
    
    if (Character <> '_') or 
       CharInSet(CharPrevious, ['0'..'9']) and CharInSet(CharNext, ['0'..'9']) then
    begin
      Result := Result + Character;
    end;
    
    CharPrevious := Character;
  end;
  
  // Adiciona último caractere
  if Length(Capitalized) > 0 then
    Result := Result + Capitalized[Length(Capitalized)];
end;

{ TFeature }

constructor TFeature.Create;
begin
  inherited;
  Comment := TStringList.Create;
end;

destructor TFeature.Destroy;
begin
  Comment.Free;
  inherited;
end;

{ TFace }

constructor TFace.Create;
begin
  inherited;
  FOrder := TStringList.Create;
  FFeatures := TObjectDictionary<string, TFeature>.Create([doOwnsValues]);
  FValues := TDictionary<string, Boolean>.Create;
  FEvents := TDictionary<string, Boolean>.Create;
  FAliases := TDictionary<string, string>.Create;
end;

destructor TFace.Destroy;
begin
  FOrder.Free;
  FFeatures.Free;
  FValues.Free;
  FEvents.Free;
  FAliases.Free;
  inherited;
end;

function TFace.SanitiseLine(const Line: string): string;
var
  P: Integer;
begin
  Result := TrimRight(Line);
  
  // Remove comentários ##
  P := Pos('##', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
    
  Result := Trim(Result);
end;

procedure TFace.DecodeParam(const P: string; out Param: TParam);
var
  TrimmedP: string;
  SpacePos, EqualPos: Integer;
  TypeAndName: string;
begin
  TrimmedP := Trim(P);
  Param.ParamType := '';
  Param.Name := '';
  Param.Value := '';
  
  if Pos(' ', TrimmedP) > 0 then
  begin
    SpacePos := Pos(' ', TrimmedP);
    Param.ParamType := Copy(TrimmedP, 1, SpacePos - 1);
    TypeAndName := Copy(TrimmedP, SpacePos + 1, Length(TrimmedP));

    TypeAndName[1] := TypeAndName[1].ToUpper();

    if Length(TypeAndName) > 1 then
      TypeAndName := 'A' + TypeAndName;
    
    EqualPos := Pos('=', TypeAndName);
    if EqualPos > 0 then
    begin
      Param.Name := Copy(TypeAndName, 1, EqualPos - 1);
      Param.Value := Copy(TypeAndName, EqualPos + 1, Length(TypeAndName));
    end
    else
      Param.Name := TypeAndName;
  end;
end;

procedure TFace.DecodeFunction(const FeatureVal: string; out RetType, Name, Value: string;
  out Param1, Param2: TParam);
var
  Parts: TStringList;
  SpacePos, EqualPos, ParenPos: Integer;
  Rest, NameIdent, Params: string;
  ParamList: TStringList;
begin
  Parts := TStringList.Create;
  ParamList := TStringList.Create;
  try
    // Extrai tipo de retorno
    SpacePos := Pos(' ', FeatureVal);
    if SpacePos = 0 then
      raise Exception.Create('Invalid function format');
      
    RetType := Copy(FeatureVal, 1, SpacePos - 1);
    Rest := Copy(FeatureVal, SpacePos + 1, Length(FeatureVal));
    
    // Extrai nome e ID
    ParenPos := Pos('(', Rest);
    if ParenPos = 0 then
      raise Exception.Create('Invalid function format - no parenthesis');
      
    NameIdent := Copy(Rest, 1, ParenPos - 1);
    
    EqualPos := Pos('=', NameIdent);
    if EqualPos = 0 then
      raise Exception.Create('Invalid function format - no equals');
      
    Name := Copy(NameIdent, 1, EqualPos - 1);
    Value := Copy(NameIdent, EqualPos + 1, Length(NameIdent));
    
    // Extrai parâmetros
    Params := Copy(Rest, ParenPos + 1, Length(Rest));
    ParenPos := Pos(')', Params);
    if ParenPos > 0 then
      Params := Copy(Params, 1, ParenPos - 1);
      
    ParamList.Delimiter := ',';
    ParamList.StrictDelimiter := True;
    ParamList.DelimitedText := Params;
    
    if ParamList.Count >= 1 then
      DecodeParam(ParamList[0], Param1);
    if ParamList.Count >= 2 then
      DecodeParam(ParamList[1], Param2);
  finally
    Parts.Free;
    ParamList.Free;
  end;
end;

procedure TFace.DecodeEvent(const FeatureVal: string; out RetType, Name, Value: string);
var
  SpacePos, EqualPos, ParenPos: Integer;
  Rest, NameIdent: string;
begin
  // Similar a DecodeFunction mas mais simples
  SpacePos := Pos(' ', FeatureVal);
  if SpacePos = 0 then
    raise Exception.Create('Invalid event format');
    
  RetType := Copy(FeatureVal, 1, SpacePos - 1);
  Rest := Copy(FeatureVal, SpacePos + 1, Length(FeatureVal));
  
  ParenPos := Pos('(', Rest);
  if ParenPos = 0 then
    raise Exception.Create('Invalid event format - no parenthesis');
    
  NameIdent := Copy(Rest, 1, ParenPos - 1);
  
  EqualPos := Pos('=', NameIdent);
  if EqualPos = 0 then
    raise Exception.Create('Invalid event format - no equals');
    
  Name := Copy(NameIdent, 1, EqualPos - 1);
  Value := Copy(NameIdent, EqualPos + 1, Length(NameIdent));
end;

procedure TFace.ReadFromFile(const FileName: string);
var
  Lines: TStringList;
  I: Integer;
  Line, FeatureTypeStr, FeatureVal: string;
  CurrentCategory: string;
  CurrentComment: TStringList;
  CurrentCommentFinished: Boolean;
  Feature: TFeature;
  SpacePos: Integer;
  NameValue: TStringList;
begin
  CurrentCategory := '';
  CurrentComment := TStringList.Create;
  CurrentCommentFinished := False;
  Lines := TStringList.Create;
  NameValue := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := SanitiseLine(Lines[I]);
      
      if Line <> '' then
      begin
        if Line[1] = '#' then
        begin
          // Comentário
          if (Length(Line) > 1) and (Line[2] = ' ') then
          begin
            if CurrentCommentFinished then
            begin
              CurrentComment.Clear;
              CurrentCommentFinished := False;
            end;
            CurrentComment.Add(Copy(Line, 3, Length(Line)));
          end;
        end
        else
        begin
          // Não é comentário
          CurrentCommentFinished := True;
          
          SpacePos := Pos(' ', Line);
          if SpacePos > 0 then
          begin
            FeatureTypeStr := Copy(Line, 1, SpacePos - 1);
            FeatureVal := Copy(Line, SpacePos + 1, Length(Line));
            
            Feature := TFeature.Create;
            Feature.Category := CurrentCategory;
            Feature.Comment.Assign(CurrentComment);
            
            try
              if (FeatureTypeStr = 'fun') or (FeatureTypeStr = 'get') or 
                 (FeatureTypeStr = 'set') then
              begin
                if FeatureTypeStr = 'fun' then
                  Feature.FeatureType := ftFun
                else if FeatureTypeStr = 'get' then
                  Feature.FeatureType := ftGet
                else
                  Feature.FeatureType := ftSet;
                  
                DecodeFunction(FeatureVal, Feature.ReturnType, Feature.Name, 
                  Feature.Value, Feature.Param1, Feature.Param2);
                  
                if FValues.ContainsKey(Feature.Value) then
                  raise Exception.CreateFmt('Duplicate value %s %s', [Feature.Value, Feature.Name]);
                  
                FValues.Add(Feature.Value, True);
                FFeatures.Add(Feature.Name, Feature);
                FOrder.Add(Feature.Name);
                CurrentComment.Clear;
              end
              else if FeatureTypeStr = 'evt' then
              begin
                Feature.FeatureType := ftEvt;
                DecodeEvent(FeatureVal, Feature.ReturnType, Feature.Name, Feature.Value);
                
                if FEvents.ContainsKey(Feature.Value) then
                  raise Exception.CreateFmt('Duplicate event %s %s', [Feature.Value, Feature.Name]);
                  
                FEvents.Add(Feature.Value, True);
                FFeatures.Add(Feature.Name, Feature);
                FOrder.Add(Feature.Name);
              end
              else if FeatureTypeStr = 'cat' then
              begin
                CurrentCategory := FeatureVal;
                Feature.Free;
              end
              else if FeatureTypeStr = 'val' then
              begin
                Feature.FeatureType := ftVal;
                
                NameValue.Clear;
                NameValue.Delimiter := '=';
                NameValue.StrictDelimiter := True;
                NameValue.DelimitedText := FeatureVal;
                
                if NameValue.Count >= 2 then
                begin
                  Feature.Name := NameValue[0];
                  Feature.Value := NameValue[1];
                  
                  FFeatures.Add(Feature.Name, Feature);
                  FOrder.Add(Feature.Name);
                end
                else
                  Feature.Free;
              end
              else if (FeatureTypeStr = 'enu') or (FeatureTypeStr = 'lex') then
              begin
                if FeatureTypeStr = 'enu' then
                  Feature.FeatureType := ftEnu
                else
                  Feature.FeatureType := ftLex;
                  
                NameValue.Clear;
                NameValue.Delimiter := '=';
                NameValue.StrictDelimiter := True;
                NameValue.DelimitedText := FeatureVal;
                
                if NameValue.Count >= 2 then
                begin
                  Feature.Name := NameValue[0];
                  Feature.Value := NameValue[1];
                  
                  FFeatures.Add(Feature.Name, Feature);
                  FOrder.Add(Feature.Name);
                  CurrentComment.Clear;
                end
                else
                  Feature.Free;
              end
              else if FeatureTypeStr = 'ali' then
              begin
                // Alias de enumeração
                NameValue.Clear;
                NameValue.Delimiter := '=';
                NameValue.StrictDelimiter := True;
                NameValue.DelimitedText := FeatureVal;
                
                if NameValue.Count >= 2 then
                  FAliases.Add(NameValue[0], NameValue[1]);
                  
                CurrentComment.Clear;
                Feature.Free;
              end
              else
                Feature.Free;
            except
              Feature.Free;
              raise;
            end;
          end;
        end;
      end;
    end;
  finally
    Lines.Free;
    CurrentComment.Free;
    NameValue.Free;
  end;
end;

end.