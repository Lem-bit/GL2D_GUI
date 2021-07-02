unit dlGUITable;

interface

uses SysUtils, Classes, dlGUITypes, dlGUIObject, dlGUITracker, dlGUIPaletteHelper;

type
  TGUITable = class;

  //Ячейка таблицы
  TGUITableCell = class(TGUIObject)

  end;

  //Список элементов таблицы
  TGUIItems = class

  end;

  //Ячейка заголовка таблицы
  TGUIHeaderCell = class(TGUIObject)
    strict private
      FText: String;
    protected
      procedure SetResize; override;
    public
      constructor Create;
      procedure RenderText; override;
    public
      property Text: String read FText write FText;
  end;

  //Список заголовков
  TGUIHeaders = class
    strict private
      FItem  : TList;
      FTable : TGUITable; //Ссылка на таблицу
      FHeight: Integer; //Высота заголовка
    strict private
      function GetOwnerTable: TGUITable;
    strict private
      property Table: TGUITable read GetOwnerTable;
    private
      function IndexOf(const Index: integer): Boolean;
      function GetHeaderItem(value: integer): TGUIHeaderCell;

      //Всем компонентам назначить новую текстуру шрифта
      procedure SetFontLink(const ALink: TTextureLink);
      procedure SetTextureLink(const ALink: TTextureLink);
      procedure SetHeight(value: integer);
    public
      constructor Create(const AOwner: TGUITable);
      destructor Destroy; override;

      procedure Render;
    public
      procedure Add(const AText: String; AWidth: Integer = 100);
    public
      property Height: Integer read FHeight write SetHeight;
      property Item[index: integer]: TGUIHeaderCell read GetHeaderItem;
  end;

  //Таблица
  TGUITable = class(TGUITrackerIntf)
    private
      FHeaders: TGUIHeaders;
      FItems  : TGUIItems;

      FOffsetX: Integer; //С какой ячейки начинать прорисовку по X
      FOffsetY: Integer; //С какой ячейки начинать прорисовку по Y
    public
      procedure SetTextureLink(pTextureLink: TTextureLink); override;
    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
      destructor Destroy; override;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;

      procedure Render; override;
    public
      property Headers: TGUIHeaders read FHeaders;
      property Items  : TGUIItems   read FItems;
  end;

implementation

{ TGUITableIntf }

constructor TGUITable.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcTable, pX, pY, 200, 200, pTextureLink);

  FHeaders:= TGUIHeaders.Create(Self);
  FItems  := TGUIItems.Create;

  FOffsetX:= 0;
  FOffsetY:= 0;

  SetRect(pX, pY, 200, 200);

  VTracker.Hide:= False;
  HTracker.Hide:= False;

  //Фон
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(5));
end;

destructor TGUITable.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FItems);
  inherited;
end;

procedure TGUITable.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;


end;

procedure TGUITable.OnMouseMove(pX, pY: Integer);
begin
  inherited;
  FOffsetX:= HTracker.GetTrackerValue;
end;

procedure TGUITable.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;
  FOffsetX:= HTracker.GetTrackerValue;
end;

procedure TGUITable.Render;
begin
  inherited;
  //Self render
  Headers.Render;
end;

procedure TGUITable.SetFontEvent;
begin
  inherited;

  //Меняем у всех элементов шрифт
  Headers.SetFontLink(Self.Font.GetTextureLink);
end;

procedure TGUITable.SetResize;
begin
  inherited;
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
end;

procedure TGUITable.SetTextureLink(pTextureLink: TTextureLink);
begin
  inherited;

  //Меняем у всех объектов текстуру
  Headers.SetTextureLink(Self.GetTextureLink);
end;

{ TGUIHeaders }

procedure TGUIHeaders.Add(const AText: String; AWidth: Integer = 100);
var Item: TGUIHeaderCell;
begin
  Item:= TGUIHeaderCell.Create;
  Item.Text  := AText;
  Item.Width := AWidth;
  Item.Height:= 20;

  FItem.Add(Item);

  //Обновим макс кол-во ячеек по X
  Table.HTracker.MaxValue:= FItem.Count;
end;

constructor TGUIHeaders.Create(const AOwner: TGUITable);
begin
  FItem := TList.Create;
  FTable:= AOwner;
end;

destructor TGUIHeaders.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TGUIHeaderCell(FItem[i]).Free;

  FreeAndNil(FItem);
  inherited;
end;

function TGUIHeaders.GetHeaderItem(value: integer): TGUIHeaderCell;
begin
  Result:= nil;

  if not IndexOf(value) then
    Exit;

  Result:= TGUIHeaderCell(FItem[value]);
end;

function TGUIHeaders.GetOwnerTable: TGUITable;
begin
  Result:= FTable;
end;

function TGUIHeaders.IndexOf(const Index: integer): Boolean;
begin
  Result:= (Assigned(FItem) and (Index > -1) and (Index < FItem.Count));
end;

procedure TGUIHeaders.Render;
var i       : integer;
    Cell    : TGUIHeaderCell; //Текущая ячейка которая рендерится
    CurrLeft: Integer;   //Сдвиг слева
begin
  CurrLeft:= 0;

  if not Assigned(Table) then
    Exit;

  if not IndexOf(Table.FOffsetX) then
    Exit;

  for i := Table.FOffsetX to FItem.Count - 1 do
  begin
    Cell:= TGUIHeaderCell(FItem[i]);

    //Проверка выхода за границы таблицы
    if CurrLeft + Cell.Width > Table.Width then
      Break;

    Cell.Rect.SetPos(
         Table.Rect.X + CurrLeft,
         Table.Rect.Y
       );
    Inc(CurrLeft, Cell.Width);

    Cell.Render;
  end;

end;

procedure TGUIHeaders.SetFontLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableCell(FItem[i]).Font.SetTextureLink(ALink);
end;

procedure TGUIHeaders.SetHeight(value: integer);
begin

end;

procedure TGUIHeaders.SetTextureLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableCell(FItem[i]).SetTextureLink(ALink);
end;

{ TGUIHeaderCell }

constructor TGUIHeaderCell.Create;
begin
  inherited Create('Table.Header.Item');

  //Устанавливать позицию X, Y не нужно т.к. она расчитывается при рендере
  FTextOffset.X:= 5;

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(7));
  VertexList.MakeSquare(1, 1, Rect.Width - 2, Rect.Height - 2, Color, GUIPalette.GetCellRect(3));
end;

procedure TGUIHeaderCell.RenderText;
begin
  inherited;
  Font.Text(Rect.X + FTextOffset.X, Rect.Y, Text, Rect.Width - FTextOffset.X);
end;

procedure TGUIHeaderCell.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
end;

end.
