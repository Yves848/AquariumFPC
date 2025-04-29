{$mode delphi}

program Main;

uses
  SysUtils, fpjson, jsonparser, Classes;

var
  JsonFile: TStringList;
  Data: TJSONData;
  Obj: TJSONObject;
begin
  JsonFile := TStringList.Create;
  try
    JsonFile.LoadFromFile('../data/config.json');
    Data := GetJSON(JsonFile.Text);
    try
      Obj := TJSONObject(Data);
      Writeln('Nom : ', Obj.Get('nom', 'inconnu'));
      Writeln('Âge : ', Obj.Get('age', 0));
    finally
      Data.Free;
    end;
  finally
    JsonFile.Free;
  end;
end.
