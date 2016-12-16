unit unDebugUnitInitOrder;

interface

uses
  Windows, SysUtils, Classes, Forms, StrUtils, JclDebug;

procedure extractUnitOrder;

implementation

var
  CtxPtr: PInitContext = nil; // Global var
  Symbols: TStringList;
  Segments: array of DWORD;

type
  TUnitInitOrderExtractor = class(TObject)
  public
    procedure MapClassTable(Sender: TObject; const Address: TJclMapAddress;
      Len: Integer; const SectionName, GroupName: string);
    procedure PublicsByValue(Sender: TObject; const Address: TJclMapAddress;
      const Name: string);
    procedure extract;
  end;

procedure TUnitInitOrderExtractor.MapClassTable(Sender: TObject; const Address: TJclMapAddress;
  Len: Integer; const SectionName, GroupName: string);
begin
  SetLength(Segments, Length(Segments) + 1);
  SegMents[Address.Segment-1] := Address.Offset;
end;

procedure TUnitInitOrderExtractor.PublicsByValue(Sender: TObject; const Address: TJclMapAddress;
  const Name: string);
const
  InitContextStr = 'System.InitContext';
begin
  if RightStr(Name, Length(InitContextStr)) = InitContextStr then
  begin
    CtxPtr := PInitContext(Segments[Address.Segment-1] + Address.Offset);
  end
  else begin
    Symbols.AddObject(Name, TObject(Segments[Address.Segment-1] + Address.Offset));
  end;
end;

procedure TUnitInitOrderExtractor.extract;
var
  MapParser: TJclMapParser;
  MapFile: String;
  sl: TStringList;
  ps: PShortString;
  i: Integer;
  s: String;
  Idx: Integer;
begin
  MapFile := ChangeFileExt(Application.ExeName, '.map');

  MapParser := TJclMapParser.Create(MapFile);
  try
    MapParser.OnPublicsByValue := self.PublicsByValue;
    MapParser.OnClassTable := self.MapClassTable;
    //Memo1.Lines.BeginUpdate;
    MapParser.Parse;
    //Memo1.Lines.EndUpdate;

  finally
    MapParser.Free;
  end;

  if CtxPtr = nil then
    Exit;

  sl := TStringList.Create;
  try

    for i := 0 to CtxPtr^.InitTable.UnitCount-1 do
    begin
      if Assigned(CtxPtr^.InitTable.UnitInfo^[i].Init) then
      begin
        s := Format('$%.8x', [DWORD(CtxPtr^.InitTable.UnitInfo^[i].Init)]);
        Idx := Symbols.IndexOfObject(TObject(CtxPtr^.InitTable.UnitInfo^[i].Init));
        if Idx > -1 then
        begin
          sl.Add(Format('%.4d: %s', [i, Symbols[Idx]]));
        end;
      end;
    end;

    sl.saveToFile('unit-order.txt');

  finally
    sl.Free;
  end;
end;

procedure extractUnitOrder;
var
ex: TUnitInitOrderExtractor;
begin
  ex:= TUnitInitOrderExtractor.Create;
  ex.extract;
  ex.Free;
end;

initialization
  Symbols := TStringList.Create;
  Symbols.Sorted := True;
  Symbols.Duplicates := dupIgnore;

finalization
  FreeAndNil(Symbols);

end.

