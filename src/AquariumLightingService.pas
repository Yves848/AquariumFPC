program AquariumLightingService;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, DateUtils;

const
  CONFIG_FILE = '/etc/aquarium/config.json'; // À adapter
  API_BASE_URL = 'http://192.168.50.202';      // IP ou nom DNS de ton ESP32
  SLEEP_MS = 60000; // 1 minute

type
  TServiceMode = (smAuto, smManual);

procedure CallAPI(const Endpoint: String);
var
  Response: String;
begin
  try
    Response := TFPHTTPClient.SimpleGet(API_BASE_URL + Endpoint);
    WriteLn(Format('[%s] Called %s -> %s',
      [FormatDateTime('hh:nn:ss', Now), Endpoint, Response]));
  except
    on E: Exception do
      WriteLn(Format('[%s] HTTP Error: %s', [FormatDateTime('hh:nn:ss', Now), E.Message]));
  end;
end;

procedure LoadConfig(out Mode: TServiceMode; out ManualState, OnTime, OffTime: String);
var
  JSON: TJSONData;
  FileContent: String;
  ModeStr: String;
  Stream: TFileStream;
begin
  if not FileExists(CONFIG_FILE) then
    raise Exception.Create('Config file not found');

  Stream := TFileStream.Create(CONFIG_FILE, fmOpenRead);
  try
    SetLength(FileContent, Stream.Size);
    Stream.ReadBuffer(FileContent[1], Stream.Size);
  finally
    Stream.Free;
  end;

  JSON := GetJSON(FileContent);
  try
    ModeStr := LowerCase(JSON.FindPath('mode').AsString);
    if ModeStr = 'manual' then
      Mode := smManual
    else
      Mode := smAuto;

    ManualState := JSON.FindPath('manual_state').AsString;
    OnTime := JSON.FindPath('on_time').AsString;
    OffTime := JSON.FindPath('off_time').AsString;
  finally
    JSON.Free;
  end;
end;

procedure RunService;
var
  Mode: TServiceMode;
  ManualState, OnTime, OffTime: String;
  CurrentTime, LastCommand: String;
begin
  LastCommand := ''; // Pour éviter de répéter le même appel
  WriteLn('[Service] Aquarium Lighting Service started.');
  while True do
  begin
    try
      LoadConfig(Mode, ManualState, OnTime, OffTime);
      CurrentTime := FormatDateTime('HH:NN', Now);

      case Mode of
        smManual:
          begin
            if ManualState = 'day' then
            begin
              if LastCommand <> 'day' then
              begin
                CallAPI('/day');
                LastCommand := 'day';
              end;
            end
            else if ManualState = 'night' then
            begin
              if LastCommand <> 'night' then
              begin
                CallAPI('/night');
                LastCommand := 'night';
              end;
            end;
          end;
        smAuto:
          begin
            if (CurrentTime = OnTime) and (LastCommand <> 'day') then
            begin
              CallAPI('/day');
              LastCommand := 'day';
            end
            else if (CurrentTime = OffTime) and (LastCommand <> 'night') then
            begin
              CallAPI('/night');
              LastCommand := 'night';
            end;
          end;
      end;
    except
      on E: Exception do
        WriteLn(Format('[%s] Error: %s', [FormatDateTime('hh:nn:ss', Now), E.Message]));
    end;

    Sleep(SLEEP_MS);
  end;
end;

begin
  RunService;
end.