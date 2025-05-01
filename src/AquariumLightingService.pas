program AquariumLightingService;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, DateUtils,
  fphttpserver, HTTPDefs;

type
  TServiceMode = (smAuto,smManual);

var
  CurrentServiceMode : TServiceMode;
  ManualState, OnTime, OffTime, LastCommand : string;

const
  CONFIG_FILE = '/etc/aquarium/config.json'; // À adapter
  API_BASE_URL = 'http://192.168.50.202';      // IP ou nom DNS de ton ESP32
  SLEEP_MS = 60000; // 1 minute

function IfThen(ACondition: Boolean; const ATrue, AFalse: string): string;
begin
  if ACondition then
    Result := ATrue
  else
    Result := AFalse;
end;


type 
  tWebServerThread = class(TThread)
  private
    FServer : TFPHTTPServer;
    procedure ServerRequest(Sender : tObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor tWebServerThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := 8080;
  FServer.OnRequest := @ServerRequest;
end;

destructor tWebServerThread.Destroy;
begin
  if Assigned(FServer) then FServer.Free;
  inherited Destroy;
end;

procedure tWebServerThread.Execute;
begin
  FServer.Active := True;
  while not Terminated do
    sleep(100);
end;

procedure tWebServerThread.ServerRequest(Sender : TObject; var ARequest : TFPHTTPConnectionRequest; var AResponse : TFPHTTPConnectionResponse);
var 
  Query : string;
  sMode : String;
begin
  Query := Arequest.URL;
  if Query = '/Status' then
  begin
    sMode := IfThen(CurrentServiceMode = smAuto,'auto','manual');
    AResponse.Content := Format(
      '{"mode": "%s", "manual_state": "%s", "on_time": "%s", "off_time":"%s", "last_command": "%s"}',[sMode,ManualState, OnTime, OffTime, LastCommand]
    );
    AResponse.Code := 200;
  end
  else if Pos('/setmode',Query) = 1 then
  begin
    if Pos('mode=auto', Query) > 0 then
    begin
      CurrentServiceMode := smAuto;
      AResponse.Content := '{"result": "Mode réglé sur auto"}';
    end
    else if Pos('mode=manual', Query) > 0 then
    begin
      CurrentServiceMode := smManual;
      AResponse.Content := '{"result": "Mode réglé sur manual"}';
    end
    else 
      AResponse.Content := '{"error": "Mode invalide"}';
    AResponse.Code := 200;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Not Found"}';
  end;
end;




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

procedure LoadConfig;
var
  JSON : TJSONData;
  FileContent : string;
  ConfigFile : String;
  ModeStr : String;
  Stream : TFileStream;
  DayName : String;
begin
  ConfigFile := '/etc/aquarium/config.json';
  if not FileExists(ConfigFile) then
    raise Exception.Create('Config file not found');
  
  Stream := TFileStream.Create(ConfigFile, fmOpenRead);
  try
    SetLength(FileContent, Stream.Size);
    Stream.ReadBuffer(FileContent[1], Stream.Size);
  finally
    Stream.Free;
  end;
  DayName := LowerCase(FormatDateTime('dddd', Now));
  JSON := GetJSON(FileContent);
  try
    ModeStr := LowerCase(JSON.FindPath('mode').AsString);
    // Writeln('*LoadConfig');
    if ModeStr = 'manual' then
      CurrentServiceMode := smManual
    else
      CurrentServiceMode := smAuto;
    
    ManualState := JSON.FindPath('manual_state').AsString;
    ontime := JSON.FindPath(format('schedule.%s.on',[DayName])).AsString;
    // Writeln('*LoadConfig');
    OffTime := JSON.FindPath(format('schedule.%s.off',[DayName])).AsString;
    Writeln(format('On  : %s',[ontime]));
    Writeln(format('Off : %s',[offtime]));
  finally
    JSON.Free;
  end;
end;

procedure RunService;
var
  CurrentTime : string;
begin
  LastCommand := '';
  WriteLn('Service Aquarium démarré.');
  while True do
  begin
    try
      LoadConfig;
      writeln('Configuration chargée');
      CurrentTime := FormatDateTime('HH:NN',Now);
      case CurrentServiceMode of
        smManual :
          begin
            if (ManualState = 'day') and (LastCommand <> 'day') then
            begin
              TFPHTTPClient.SimpleGet('http://192.168.50.202/day');
              LastCommand := 'day';
              WriteLn(Format('[%s] Mode manuel: appel /day',[FormatDateTime('hh:nn:ss',Now)]));
            end
            else if (ManualState = 'night') and (LastCommand <> 'night') then
            begin
              TFPHTTPClient.SimpleGet('http://192.168.50.202/night');
              LastCommand := 'night';
              Writeln(Format('[%s] Mode manuel: appel /night',[FormatDateTime('hh:nn:ss',Now)]));
            end;
          end;
        smAuto :
          begin
            if (CurrentTime = ontime) and (LastCommand <> 'day') then
            begin
              TFPHTTPClient.SimpleGet('http://192.168.50.202/day');
              LastCommand := 'day';
              Writeln(Format('[%s] Mode auto: appel /day',[FormatDateTime('hh:nn:ss',now)]));
            end
            else if (CurrentTime = OffTime) and (LastCommand <> 'night') then
            begin
              TFPHTTPClient.SimpleGet('http://192.168.50.202/night');
              LastCommand := 'night';
              Writeln(Format('[%s] Mode auto: appel /night',[FormatDateTime('hh:nn:ss',now)]));
            end;
          end;
      end;
    except
      on e: exception do
        Writeln(format('[%s] Erreur: %s',[FormatDateTime('hh:nn:ss',now), E.Message]));
    end;
    sleep(60000);
  end;
end;

begin
  with tWebServerThread.create do
    Writeln('Serveur HTTP démarré sur le port 8080.');

  RunService;
end.