unit SendData;

interface

uses
  Windows, SysUtils, Classes, Messages, Dialogs;

type
  TCopyDataType = (cdtString = 0, cdtStream = 1, cdtRecord = 2);
  TSendType = (stSend, stPost);

  TReceivedStringEvent = procedure(Sender: TObject; Value: String) of object;
  TReceivedStreamEvent = procedure(Sender: TObject;
    Value: TMemoryStream) of object;
  TReceivedRecordEvent = procedure(Sender: TObject; Value: Pointer) of object;
  TWMCopyDataEvent = procedure(var Msg: TWMCopyData) of object;

  TSendData = class(TComponent)
  private
    { Private declarations }
    FClassNameSD: string;
    FClassTextSD: String;
    FSendType: TSendType;
    FOnReceivedString: TReceivedStringEvent;
    FOnReceivedStream: TReceivedStreamEvent;
    FOnReceivedRecord: TReceivedRecordEvent;
    FOnCopyData: TWMCopyDataEvent;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(Msg: TWMCopyData);
    function SendString(Value: String): Integer;
    function SendStream(Value: TMemoryStream): Integer;
    function SendRecord(Size: Cardinal; Value: Pointer): Integer;
    function SendStringA(Wnd: HWND; Value: String): Integer;
    function SendStreamA(Wnd: HWND; Value: TMemoryStream): Integer;
    function SendRecordA(Wnd: HWND; Size: Cardinal; Value: Pointer): Integer;
  published
    { Published declarations }
    property ClassNamed: string read FClassNameSD write FClassNameSD;
    property ClassText: string read FClassTextSD write FClassTextSD;
    property SendType: TSendType read FSendType write FSendType;
    property OnReceivedString
      : TReceivedStringEvent read FOnReceivedString write
      FOnReceivedString;
    property OnReceivedStream
      : TReceivedStreamEvent read FOnReceivedStream write
      FOnReceivedStream;
    property OnReceivedRecord
      : TReceivedRecordEvent read FOnReceivedRecord write
      FOnReceivedRecord;
    property OnCopyData: TWMCopyDataEvent read FOnCopyData write FOnCopyData;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MZSW Components', [TSendData]);
end;

constructor TSendData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSendType := stPost;
end;

destructor TSendData.Destroy;
begin
  inherited Destroy;
end;

procedure TSendData.Action(Msg: TWMCopyData);
var
  copyDataType: TCopyDataType;
  ms: TMemoryStream;
begin
  copyDataType := TCopyDataType(Msg.CopyDataStruct.dwData);
  case copyDataType of
    cdtString:
      if Assigned(FOnReceivedString) then
        FOnReceivedString(self, PChar(Msg.CopyDataStruct.lpData));
    cdtStream:
      if Assigned(FOnReceivedStream) then
      begin
        ms := TMemoryStream.Create;
        try
          ms.Write(Msg.CopyDataStruct.lpData^, Msg.CopyDataStruct.cbData);
          ms.Position := 0;
          FOnReceivedStream(self, ms);
        finally
          ms.Free;
        end;
      end;
    cdtRecord:
      if Assigned(FOnReceivedRecord) then
        FOnReceivedRecord(self, Msg.CopyDataStruct.lpData)
  end;
end;

function TSendData.SendString(Value: String): Integer;
var
  Wnd: HWND;
  Cnsd: PChar;
  Ctsd: PChar;
begin
  if (Length(FClassNameSD) = 0) then
    Cnsd := nil
  else
    Cnsd := PChar(FClassNameSD);
  if (Length(FClassTextSD) = 0) then
    Ctsd := nil
  else
    Ctsd := PChar(FClassTextSD);
  Wnd := FindWindow(Cnsd, Ctsd);
  if (Wnd > 0) then
    Result := SendStringA(Wnd, Value)
  else
    Result := -1;
end;

function TSendData.SendStream(Value: TMemoryStream): Integer;
var
  Wnd: HWND;
  Cnsd: PChar;
  Ctsd: PChar;
begin
  if (Length(FClassNameSD) = 0) then
    Cnsd := nil
  else
    Cnsd := PChar(FClassNameSD);
  if (Length(FClassTextSD) = 0) then
    Ctsd := nil
  else
    Ctsd := PChar(FClassTextSD);
  Wnd := FindWindow(Cnsd, Ctsd);
  if (Wnd > 0) then
    Result := SendStreamA(Wnd, Value)
  else
    Result := -1;
end;

function TSendData.SendRecord(Size: Cardinal; Value: Pointer): Integer;
var
  Wnd: HWND;
  Cnsd: PChar;
  Ctsd: PChar;
begin
  if (Length(FClassNameSD) = 0) then
    Cnsd := nil
  else
    Cnsd := PChar(FClassNameSD);
  if (Length(FClassTextSD) = 0) then
    Ctsd := nil
  else
    Ctsd := PChar(FClassTextSD);
  Wnd := FindWindow(Cnsd, Ctsd);
  if (Wnd > 0) then
    Result := SendRecordA(Wnd, Size, Value)
  else
    Result := -1;
end;

function TSendData.SendStringA(Wnd: HWND; Value: String): Integer;
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := Integer(cdtString);
  CopyDataStruct.cbData := (1 + Length(Value)) * SizeOf(Char);
  CopyDataStruct.lpData := PChar(Value);
  case SendType of
    stSend:
      Result := SendMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
  else
    PostMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
    Result := 0;
  end;
end;

function TSendData.SendStreamA(Wnd: HWND; Value: TMemoryStream): Integer;
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := Integer(cdtStream);
  CopyDataStruct.cbData := Value.Size;
  CopyDataStruct.lpData := Value.Memory;
  case SendType of
    stSend:
      Result := SendMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
  else
    PostMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
    Result := 0;
  end;
end;

function TSendData.SendRecordA(Wnd: HWND; Size: Cardinal;
  Value: Pointer): Integer;
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := Integer(cdtRecord);
  CopyDataStruct.cbData := Size;
  CopyDataStruct.lpData := Value;
  case SendType of
    stSend:
      Result := SendMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
  else
    PostMessage(Wnd, WM_COPYDATA, 0, Integer(@CopyDataStruct));
    Result := 0;
  end;
end;

end.
