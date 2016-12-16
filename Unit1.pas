unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, filectrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    edtDir: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  NOME_UNIT_PERF = 'unPerfCounter';

procedure DirList(ASource: string; ADirList: TStringList);
var
  SearchRec: TSearchRec;
  Result: integer;
begin
  Result := FindFirst(ASource + '*.*', faAnyFile or faDirectory, SearchRec);
  if Result = 0 then
    while (Result = 0) do
    begin
      if (SearchRec.Name = '.') or (SearchRec.name = '..') then begin
        Result := FindNext(SearchRec);
        continue;
      end;

      if (SearchRec.Attr and faDirectory) = faDirectory then DirList(aSource + SearchRec.name + '\', ADirList);

      if (ExtractFileExt(searchRec.name) = '.pas') then
      begin
        ADirList.Add(ASource + SearchRec.Name);
      end;

      Result := FindNext(SearchRec);
    end;
  //FindClose( SearchRec );
  ADirList.Sort;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
arqs: TStringList;
dir: string;
begin
  arqs:= TStringList.create;
  dir:= edtDir.Text;
  if dir[length(dir)] <> '\' then dir:= dir + '\';
  
  DirList( dir, arqs);
  ListBox1.Items.Assign(arqs);
  arqs.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
i, lin: integer;
arq: TStringList;
gotUses, gotInit: boolean;
s, up, tr: string;
initLine, intfLine, finLine: integer;
begin
  arq:= TStringList.Create;
  for i := 0 to ListBox1.Items.count - 1 do begin

    listBox1.ItemIndex:= i;
    application.ProcessMessages;

    arq.LoadFromFile(ListBox1.Items[i]);
    gotUses:= false;
    gotInit:= false;
    initLine:= -1;
    intfLine:= -1;

    for lin := 0 to arq.Count - 1 do begin
      up:= UpperCase(arq[lin]);
      tr:= Trim(up);

      if not gotUses and
         ( (Pos('USES ', up) > 0) or
           (tr = 'USES')  ) then
      begin
        s:= StringReplace(arq[lin], 'uses', 'uses ' + NOME_UNIT_PERF + ', ', [rfIgnoreCase]);
        gotUses:= true;
        arq[lin]:= s;
      end;

      if tr = 'INITIALIZATION'  then begin
        initLine:= lin;
        gotInit:= true;
      end;

      if tr = 'INTERFACE' then 
        intfLine:= lin;

      if (tr = 'END.')  or
         (tr = 'FINALIZATION') then
      begin
        finLine:= lin;

        if not gotUses then begin
          arq.Insert(intfLine+1, 'uses ' + NOME_UNIT_PERF + ';');
        end;

        if not gotInit then begin
          arq.Insert(lin, 'initialization');
          finLine:= lin + 1;
        end else begin
          arq.Insert(initLine+1,
            '  masterPerf.currentToConsole(''' +
            ExtractFileName(listbox1.items[i] +
            ' start init'');'));
          finLine:= finLine + 1;
        end;


        arq.Insert(finLine,
          '  masterPerf.currentToConsole(''' +
          ExtractFileName(listbox1.items[i] +
          ' initialized'');'));
          break;
      end;

    end;

    arq.SaveToFile(ListBox1.Items[i]);

    //break; //<---- aqui

  end;
  arq.free;
end;

end.
