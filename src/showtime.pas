unit ShowTime;
{$mode objfpc}{$H+}

interface
uses
  SysUtils, dateutils;
  
  Procedure initDate;
  function DayOfWeekEuropean(ADate: TDateTime): Integer;

implementation

  procedure initDate;
  begin
    FormatSettings.DateSeparator := '-';
    FormatSettings.TimeSeparator := ':';
    FormatSettings.ShortDateFormat := 'dd-mm-yyyy';
    FormatSettings.LongDateFormat := 'dddd, dd mmmm yyyy';
    FormatSettings.ShortTimeFormat := 'hh:nn:ss';
    FormatSettings.LongTimeFormat := 'hh:nn:ss.zzz';
    FormatSettings.LongDayNames[1] := 'Lundi';
    FormatSettings.LongDayNames[2] := 'Mardi';
    FormatSettings.LongDayNames[3] := 'Mercredi';
    FormatSettings.LongDayNames[4] := 'Jeudi';
    FormatSettings.LongDayNames[5] := 'Vendredi';
    FormatSettings.LongDayNames[6] := 'Samedi';
    FormatSettings.LongDayNames[7] := 'Dimanche';
  end;

  function DayOfWeekEuropean(ADate: TDateTime): Integer;
  var
    ISO: Word;
  begin
    // FPC : Sunday = 1, Monday = 2, ..., Saturday = 7
    // European : Monday = 1, ..., Sunday = 7
    ISO := DayOfTheWeek(ADate);
    Result := ISO -1;
    // Result := ISO + 1;
    // if Result > 6 then
    //   Result := 0;
  end;

end.
