
Program AquariumLightingService;

{$mode objfpc}{$H+}

Uses 
  {$IFDEF UNIX} cthreads,{} {$ENDIF}
  {$IFDEF WINDOWS} windows,{$ENDIF}
showtime,TypInfo,
Classes, SysUtils, fpjson, jsonparser, fphttpclient, DateUtils,
fphttpserver, HTTPDefs;

Type 
  TServiceMode = (smAuto,smManual);

Var 
  CurrentServiceMode : TServiceMode;
  OnTime, OffTime : ttime;
  ManualState,  LastCommand : string;
  API_BASE_URL : String = 'http://192.168.50.201';
  SLEEP_MS : integer = 1000;

Const 
  CONFIG_FILE = '/etc/aquarium/config.json';

  dow: array[0..6] Of string = ('monday', 'tuesday', 'wednesday', 'thursday',
                                'friday', 'saturday','sunday');

Function IfThen(ACondition: Boolean; Const ATrue, AFalse: String): string;
Begin
  If ACondition Then
    Result := ATrue
  Else
    Result := AFalse;
End;

Type 
  tWebServerThread = Class(TThread)
    Private 
      FServer : TFPHTTPServer;
      base_url : String;
      Procedure ServerRequest(Sender : tObject; Var ARequest:
                              TFPHTTPConnectionRequest; Var AResponse:
                              TFPHTTPConnectionResponse);
    Protected 
      Procedure Execute;
      override;
    Public 
      constructor Create(pBase_url: String = '');
      destructor Destroy;
      override;
  End;

  constructor tWebServerThread.Create(pBase_url: String = '');
Begin
  inherited Create(False);
  FreeOnTerminate := True;
  base_url := pBase_url;
  FServer := TFPHTTPServer.Create(Nil);
  FServer.Port := 80;
  FServer.OnRequest := @ServerRequest;
End;

destructor tWebServerThread.Destroy;
Begin
  If Assigned(FServer) Then FServer.Free;
  inherited Destroy;
End;

Procedure tWebServerThread.Execute;
Begin
  FServer.Active := True;
  While Not Terminated Do
    sleep(100);
End;

Procedure tWebServerThread.ServerRequest(Sender : TObject; Var ARequest :
                                         TFPHTTPConnectionRequest; Var AResponse
                                         : TFPHTTPConnectionResponse);

Var 
  Query : string;
  sMode : String;
  response : tstringlist;
Begin
  Query := Arequest.URL;
  WriteLn(Format('[%s] Requête reçue : %s',[FormatDateTime('hh:nn:ss',Now), Query]));

  If Query = '/Status' Then
    Begin
      sMode := IfThen(CurrentServiceMode = smAuto,'auto','manual');
      AResponse.Content := Format('{"mode": "%s", "manual_state": "%s", "on_time": "%s", "off_time":"%s", "last_command": "%s"}'
                           ,[sMode,ManualState, OnTime, OffTime, LastCommand]);
      AResponse.Code := 200;
    End
  Else If Pos('/setmode',Query) = 1 Then
         Begin
           If Pos('mode=auto', Query) > 0 Then
             Begin
               CurrentServiceMode := smAuto;
               AResponse.Content := '{"result": "Mode réglé sur auto"}';
             End
           Else If Pos('mode=manual', Query) > 0 Then
                  Begin
                    CurrentServiceMode := smManual;
                    AResponse.Content := '{"result": "Mode réglé sur manual"}'
                    ;
                  End
           Else
             AResponse.Content := '{"error": "Mode invalide"}';
           AResponse.Code := 200;
         End
  Else If Query = '/dashboard' Then
         Begin
           AResponse.ContentType := 'text/html; charset=utf-8';
           AResponse.Contents.Text := 
                                      '<!DOCTYPE html>' + LineEnding +
                                      '<html lang="fr">' + LineEnding +
                                      '<head>' + LineEnding +
                                      '  <meta charset="UTF-8">' + LineEnding +
                                      '  <title>Dashboard Aquarium</title>' +
                                      LineEnding +
                                      '  <style>' + LineEnding +

'    body { font-family: sans-serif; background-color: #f2f2f2; padding: 2em; }'
                                      +
                                      LineEnding +
                                      '    h1 { color: #2c3e50; }' + LineEnding
                                      +

'    .card { background: white; padding: 1em; border-radius: 10px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); width: 400px; }'
                                      + LineEnding +
                                      '    .label { font-weight: bold; }' +
                                      LineEnding +
                                      '  </style>' + LineEnding +
                                      '</head>' + LineEnding +
                                      '<body>' + LineEnding +
                                      '<h1>Tableau de bord de l''aquarium</h1>'
                                      + LineEnding +
                                      '<div class="card">' + LineEnding +
                                      Format(
                                   '<p><span class="label">Mode :</span> %s</p>'
                                      , [IfThen(CurrentServiceMode = smAuto,
                                      'Automatique', 'Manuel')]) + LineEnding +
                                      Format(
                           '<p><span class="label">État manuel :</span> %s</p>'
                                      , [ManualState]) + LineEnding +
                                      Format(

                      '<p><span class="label">Heure d''allumage :</span> %s</p>'
                                      , [OnTime]) + LineEnding +
                                      Format(

                    '<p><span class="label">Heure d''extinction :</span> %s</p>'
                                      , [OffTime]) + LineEnding +
                                      Format(
            '<p><span class="label">Dernière commande envoyée :</span> %s</p>'
                                      , [LastCommand]) + LineEnding +
                                      '</div>' + LineEnding +
                                      '</body>' + LineEnding +
                                      '</html>';
           AResponse.Code := 200;
         End
  Else If query = '/test' Then
         Begin
           response := TStringList.Create;
           response.loadfromfile('./html/dashboard.html');
           AResponse.Content := response.Text;
           response.Free;
           AResponse.Code := 200;
         End
  Else
    Begin
      AResponse.Code := 404;
      AResponse.Content := '{"error": "Not Found connard"}';
    End;

End;

Function CallAPI(Const Endpoint: String) : string;

Var 
  Response: String;
Begin
  Try
    WriteLn(Format('[%s] Called %s',[FormatDateTime('hh:nn:ss', Now),
    API_BASE_URL+endpoint]));
    Response := TFPHTTPClient.SimpleGet(API_BASE_URL + Endpoint);

// WriteLn(Format('[%s] Called %s -> %s',[FormatDateTime('hh:nn:ss', Now), Endpoint, Response]));
    result := Response;
  Except
    on E: Exception Do
          Begin
            WriteLn(Format('[%s] HTTP Error: %s', [FormatDateTime('hh:nn:ss',
                    Now)
            , E.Message]));
            result := '';
          End;

End;
End;

Procedure LoadConfig;

Var 
  JSON : TJSONData;
  FileContent : string;
  ConfigFile : String;
  ModeStr : String;
  Stream : TFileStream;
  DayName : String;
  sleep : string;
Begin
  ConfigFile := '/etc/aquarium/config.json';
  Writeln(format('Chargement de la configuration depuis %s',[ConfigFile]));
  If Not FileExists(ConfigFile) Then
    raise Exception.Create('Config file not found');
  Stream := TFileStream.Create(ConfigFile, fmOpenRead);
  Try
    SetLength(FileContent, Stream.Size);
    Stream.ReadBuffer(FileContent[1], Stream.Size);
  Finally
    Stream.Free;
End;

DayName := dow[DayOfWeekEuropean(now)];
writeln(format('Jour actuel : %s',[DayName]));
JSON := GetJSON(FileContent);
Writeln('Chargement de la configuration JSON...');
sleep := JSON.FindPath('SLEEP_MS').AsString;
writeln(format('Sleep ms : %s',[sleep]));
Try
  API_BASE_URL := JSON.FindPath('base_url').AsString;
  SLEEP_MS := StrToIntDef(sleep, 1)*1000;
  Writeln(format('Base URL : %s',[API_BASE_URL]));
  ModeStr := LowerCase(JSON.FindPath('mode').AsString);
  If ModeStr = 'manual' Then
    CurrentServiceMode := smManual
  Else
    CurrentServiceMode := smAuto;

  ManualState := JSON.FindPath('manual_state').AsString;
  ontime := StrtoTime(JSON.FindPath(format('schedule.%s.on',[DayName])).AsString
            );
  OffTime := strtotime(JSON.FindPath(format('schedule.%s.off',[DayName])).
             AsString);
  Writeln(format('On  : %s',[FormatDateTime('hh:nn:ss',ontime)]));
  Writeln(format('Off : %s',[FormatDateTime('hh:nn:ss',offtime)]));
Finally
  JSON.Free;
End;
End;

Procedure SetLightingMode(Mode: String);

Var 
  Endpoint: string;
  Client: TFPHTTPClient;
  Response: string;
Begin
  Endpoint := Format('%s/%s',[API_BASE_URL,Mode]);
  WriteLn(Format('[%s] Setting lighting mode to %s',[FormatDateTime('hh:nn:ss',
          Now), Mode]));
  Client := TFPHTTPClient.Create(Nil);
  Try
    Client.AddHeader('Content-Type', 'application/json');
    Response := Client.Post(Endpoint);
    WriteLn(Format('[%s] Lighting mode set to %s. Response: %s',
            [FormatDateTime('hh:nn:ss', Now), Mode, Response]));
  Except
    on E: Exception Do
          WriteLn(Format('[%s] Error setting lighting mode: %s',
                  [FormatDateTime('hh:nn:ss', Now), E.Message]));
End;
Client.Free;
End;

Procedure RunService;

Var 
  CurrentTime : TTime;
  response : string;
  state : string;
  JSON : TJSONData;
Begin
  LastCommand := 'day';
  WriteLn('Service Aquarium démarré.');
  OnTime := 0.0;
  // Initialize to a default of midnight
  OffTime := 0.0;
  // Initialize to a default of midnight
  LoadConfig;
  Writeln('Chargement de la configuration...');
  writeln('Configuration chargée');
  While True Do
    Begin
      response := CallAPI('/data');
      JSON := GetJSON(response);
      state := JSON.FindPath('state').AsString;
      WriteLn(Format('[%s] Réponse de l''API : %s',[FormatDateTime('hh:nn:ss',
              Now), response]));
      CurrentTime := StrtoTime(FormatDateTime('HH:NN',Now));
      WriteLn(Format('[%s] Heure actuelle : %s',[FormatDateTime('hh:nn:ss',Now),
      FormatDateTime('HH:NN',CurrentTime)]));
      WriteLn(Format('[%s] État actuel : %s',[FormatDateTime('hh:nn:ss',Now),
      GetEnumName(TypeInfo(TServiceMode), Ord(CurrentServiceMode))]));
      Case CurrentServiceMode Of 
        smManual :
                   Begin
                     writeln(Format('[%s] Mode manuel activé',[FormatDateTime(
                             'hh:nn:ss',Now)]));
                     If (ManualState = 'day') And (state <> 'day') Then
                       Begin
                         SetLightingMode('day');
                         LastCommand := 'day';
                         WriteLn(Format('[%s] Mode manuel: appel /day',[
                                 FormatDateTime('hh:nn:ss',Now)]));
                       End;
                     If (ManualState = 'night') And (state <> 'night') Then
                       Begin
                         SetLightingMode('night');
                         LastCommand := 'night';
                         Writeln(Format('[%s] Mode manuel: appel night',[
                                 FormatDateTime('hh:nn:ss',Now)]));
                       End;
                   End;
        smAuto :
                 Begin
                   writeln(Format('[%s] Mode automatique activé',[
                           FormatDateTime('hh:nn:ss',Now)]));
                   writeln(Format('[%s] Heure d''allumage : %s',[FormatDateTime(
                           'hh:nn:ss',Now), FormatDateTime('hh:nn:ss',ontime)]))
                   ;
                   writeln(Format('[%s] Heure d''extinction : %s',[
                           FormatDateTime('hh:nn:ss',Now), FormatDateTime(
                                                                      'hh:nn:ss'
                                                                          ,
                                                                         offtime
                   )]));
                   writeln(Format('[%s] État actuel : %s',[FormatDateTime(
                           'hh:nn:ss',Now), state]));
                   If ((CurrentTime >= ontime) And (CurrentTime < offtime)) And
                      (state <> 'day') Then
                     Begin
                       SetLightingMode('day');
                       LastCommand := 'day';
                       Writeln(Format('[%s] Mode auto: appel /day',[
                               FormatDateTime('hh:nn:ss',now)]));
                     End
                   Else
                     If (CurrentTime >= OffTime) And (state <> 'night') Then
                       Begin
                         SetLightingMode('night');
                         LastCommand := 'night';
                         Writeln(Format('[%s] Mode auto: appel /night',[
                                 FormatDateTime('hh:nn:ss',now)]));
                       End;
                 End;
      End;
      writeln(format('[%s] Attente de %d ms',[FormatDateTime('hh:nn:ss',Now),
      SLEEP_MS]));
      sleep(SLEEP_MS);
    End;
End;

Begin
  With tWebServerThread.create Do
    Writeln('Serveur HTTP démarré sur le port 8080.');

  RunService;
End.
