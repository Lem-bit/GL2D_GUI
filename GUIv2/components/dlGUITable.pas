unit dlGUITable;

interface

uses SysUtils, Classes, Graphics, dlGUITypes, dlGUIObject, dlGUITracker, dlGUIPaletteHelper,
  dlOpenGL, dlGUIVertexController, dlGUIFont, dlGUIXmlSerial;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author  : Ansperi L.L., 2021                     =
  = Email   : gui_proj@mail.ru                       =
  = Site    : lemgl.ru                               =
  = Telegram: https://t.me/delphi_lemgl              =
  =                                                  =
  ====================================================
}

type
  TGUITable = class;

  //Класс ячейки
  TGUITableCell = class(TGUIObject)
    strict private
      FSelected: Boolean;
      FText    : String;
    protected
      procedure SetAreaResize; override;
      procedure SetResize; override;
    private
      property Selected: Boolean read FSelected write FSelected;
    public
      procedure Render; override;
      procedure RenderText; override;
    public
     [TXMLSerial] property Text: String read FText write FText;
  end;

  //Список ячеек
  TGUITableCellList = class
    protected
      [TXMLSerial] FItem : TList; //Список элементов
    protected
      FTable: TGUITable; //Ссылка на таблицу
    strict private
      function GetOwnerTable: TGUITable;
    protected
      property Table: TGUITable read GetOwnerTable;
    private
      function IndexOf(const Index: integer): Boolean;
    private
      //Всем компонентам назначить новую текстуру шрифта
      procedure SetFontLink(const ALink: TTextureLink);
      procedure SetTextureLink(const ALink: TTextureLink);
    public
      constructor Create(const AOwner: TGUITable);
      destructor Destroy; override;
    public
      function Count: Integer;
  end;

  TGUITableCol    = class(TGUITableCell)
    public
      constructor Create;
  end;

  TGUITableHeader = class(TGUITableCell)
    public
      constructor Create;
  end;

  //Строка записи
  TGUITableItem = class(TGUITableCellList)
    strict private
      FHeight: Integer; //Высота строки
    private
      function GetCellItem(value: integer): TGUITableCol;
      procedure SetHeight(value: integer);
    public
      procedure Add(const AText: String); overload;
      procedure Add(const AText: array of String); overload;
    public
      procedure OnMouseMove(pX, pY: Integer);
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); //Тут происходит выбор "ячейки" col
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
    public
      property Col[index: integer]: TGUITableCol read GetCellItem;
      [TXMLSerial] property Height: integer read FHeight write SetHeight;
  end;

  //Список элементов таблицы
  TGUITableRows = class
    private
      FTable : TGUITable;
      [TXMLSerial] FItem  : TList;
    strict private
      function GetOwnerTable: TGUITable;
    protected
      property Table: TGUITable read GetOwnerTable;
    private
      function IndexOf(index: integer): Boolean;
      function GetItem(value: integer): TGUITableItem;

      procedure SetFontLink(const ALink: TTextureLink);
      procedure SetTextureLink(const ALink: TTextureLink);
    public
      constructor Create(const AOwner: TGUITable);
      destructor Destroy; override;
    public
      procedure OnMouseMove(pX, pY: Integer);
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); //Тут происходит выбор "записи" row
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
    public
      function Add: TGUITableItem;
      procedure Delete(const AIndex: Integer);
      function Count: Integer;
    public
      property Row[index: integer]: TGUITableItem read GetItem;
  end;

  //Список заголовков
  TGUITableHeaders = class(TGUITableCellList)
    strict private
      FHeight: Integer; //Высота заголовка
    private
      procedure SetHeight(value: integer);
      function GetCellItem(value: integer): TGUITableHeader;
    public
      constructor Create(const AOwner: TGUITable);
      destructor Destroy; override;
    public
      procedure Add(const AText: String; AWidth: Integer = 100);
      function Count: Integer;
    public
      [TXMLSerial] property Height: Integer read FHeight write SetHeight;
      property Item[index: integer]: TGUITableHeader read GetCellItem;
  end;

  //Таблица
  TGUITableSelected = class
    strict private
      FRow : Integer; //Столбец
      FCol : Integer; //Ячейка
      FCell: TGUITableCell; //Ссылка на выбранную ячейку
    strict private
      FBlend: TBlendParam; //Параметры прозрачности элемента
      FColor: TGLColor;    //Цвет выбранной ячейки
    private
      procedure Clear;
      function IsSelected: Boolean;
      procedure Render(const ARect: TGUIObjectRect);
    public
      constructor Create;
      destructor Destroy; override;
    public
      property Row : Integer       read FRow   write FRow;
      property Col : Integer       read FCol   write FCol;
      property Cell: TGUITableCell read FCell  write FCell;
    public
      [TXMLSerial] property Blend: TBlendParam  read FBlend write FBlend;
      [TXMLSerial] property Color: TGLColor     read FColor write FColor;
  end;

  //Свойства и настройки
  TGUITableProperties = class
    strict private
      FEnableSelect : Boolean; //Разрешить выбор элемента
      FSelectRow    : Boolean; //Выбирать всю строку
    public
      constructor Create;
      destructor Destroy; override;
    public
      [TXMLSerial] property EnableSelect : Boolean read FEnableSelect  write FEnableSelect;
      [TXMLSerial] property SelectRow    : Boolean read FSelectRow     write FSelectRow;
  end;

  TGUITable = class(TGUITrackerIntf)
    private
      F_ROW_MAX_WIDTH: Integer; //Макс ширина RowSelect
      F_MAX_ITEMS_Y  : Integer; //Макс кол-во элементов по Y
      F_MAX_ITEMS_X  : Integer; //Макс кол-во элементов по X
    private
      FHeaders   : TGUITableHeaders;
      FItems     : TGUITableRows;

      FOffsetX   : Integer; //С какой ячейки начинать прорисовку по X
      FOffsetY   : Integer; //С какой ячейки начинать прорисовку по Y
      FSelected  : TGUITableSelected; //Информация о выбранной ячейке
      FProperties: TGUITableProperties; //Свойства и настройки таблицы
    private
      procedure UpdateOffset;
      procedure UpdateSize; //Пересчитать размеры
    public
      procedure SetTextureLink(pTextureLink: TTextureLink); override;
    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;

      procedure OnMoveTracker(Sender: TObject; ParamObj: Pointer = nil);
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); override;
      procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); override;

      procedure Render; override;
    public
      OnSelectCell  : TGUIProc; //Выбрали ячейку
      OnUnSelectCell: TGUIProc; //Нажали в пустую область и выбор отменился
      OnRender      : TGUIProc;
    public
      [TXMLSerial] property Properties: TGUITableProperties read FProperties;
      [TXMLSerial] property Selected  : TGUITableSelected   read FSelected;
      [TXMLSerial] property Headers   : TGUITableHeaders    read FHeaders;
      [TXMLSerial] property Items     : TGUITableRows       read FItems;
  end;

implementation

{ TGUITableIntf }

constructor TGUITable.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcTable, 0, 0, 200, 200, pTextureLink);

  F_ROW_MAX_WIDTH:= 0;
  F_MAX_ITEMS_X  := 0;
  F_MAX_ITEMS_Y  := 0;

  HTracker.OnMove:= OnMoveTracker;
  VTracker.OnMove:= OnMoveTracker;

  FHeaders   := TGUITableHeaders.Create(Self);
  FItems     := TGUITableRows.Create(Self);
  FSelected  := TGUITableSelected.Create;
  FProperties:= TGUITableProperties.Create;

  FOffsetX:= 0;
  FOffsetY:= 0;

  SetRect(0, 0, 200, 200);

  VTracker.Hide:= False;
  HTracker.Hide:= False;

  //Фон
  VertexList.MakeSquare(Rect.X, Rect.Y, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Frame));
end;

destructor TGUITable.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FItems);
  FreeAndNil(FSelected);
  FreeAndNil(FProperties);
  inherited;
end;

procedure TGUITable.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  Items.OnMouseDown(pX, pY, Button);
end;

procedure TGUITable.OnMouseMove(pX, pY: Integer);
begin
  inherited;
  Items.OnMouseMove(pX, pY);
end;

procedure TGUITable.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  Items.OnMouseUp(pX, pY, Button);
end;

procedure TGUITable.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  inherited;
end;

procedure TGUITable.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  inherited;
end;

procedure TGUITable.OnMoveTracker(Sender: TObject; ParamObj: Pointer = nil);
begin
  inherited;
  UpdateOffset;
  SetAction([goaUpdateSize]);
end;

procedure TGUITable.Render;
var i, j: integer;
    currWidth, currHeight: Integer;
    Item: TGUITableItem;

    //Размеры строки которая помещается в таблицу
    RowRect: TGUIObjectRect;
begin
  inherited;

  if Assigned(OnRender) then
    OnRender(Self, nil);

  //Размеры обновились, пересчитаем
  if goaUpdateSize in GetAction then
    UpdateSize;

  //Предустановки на выбранную ячейку
  RowRect.SetRect(Rect.X, Rect.Y, F_ROW_MAX_WIDTH, 0);
  currWidth := 0;

  //
  for i := FOffsetX to Headers.Count - 1 do
  begin

    //Если вышли за границы останавливаем прорисовку
    if currWidth + Headers.Item[i].Width > ClientWidth then
      Break;

    //Размер заголовка
    Headers.Item[i].Rect.SetPos(
       Rect.X + currWidth,
       Rect.Y
    );

    //Сбрасываем высоту элемента
    currHeight:= Headers.Height;

    for j := FOffsetY to Items.Count - 1 do
    begin
      //Элемент меню с подэлементами
      Item:= Items.Row[j];

      if not Item.IndexOf(i) then
        Break;

      //Если вышли за границы останавливаем прорисовку
      if currHeight + Rect.Y + Item.Height > ClientHeight then
        Break;

      //Размер элемента
      Item.Col[i].Rect.SetPos(
         Rect.X + currWidth,
         Rect.Y + currHeight
      );

      if j = Selected.Row then
      begin
        RowRect.Y:= Item.Col[i].Rect.Y;
        RowRect.Height:= Item.Height;
      end;

      //Увеличиваем высоту
      inc(currHeight, Item.Height);

      //Включен выбор элементов
      if Properties.EnableSelect then
      begin

        if (Item.Col[i].Selected) then
          if (not Properties.SelectRow) then //Элемент выбран, подкрасим
          begin
            RowRect.SetRect(Item.col[i].Rect);
            Item.Col[i].Area.Visible:= True;
          end;

      end
      else
        Item.Col[i].Area.Visible:= False;

      Item.Col[i].Render;
    end;

    inc(currWidth, Headers.Item[i].Width);
    Headers.Item[i].Render;
  end;

  //Отобразить выбранную ячейку
  if (F_ROW_MAX_WIDTH > 0) and (Selected.IsSelected) then
    if (Selected.Row >= FOffsetY) and (Selected.Row < FOffsetY + F_MAX_ITEMS_Y) then
      Selected.Render(RowRect);
end;

procedure TGUITable.SetFontEvent;
begin
  inherited;

  //Меняем у всех элементов шрифт
  Headers.SetFontLink(Self.Font.GetTextureLink);
  Items.SetFontLink(Self.Font.GetTextureLink);
end;

procedure TGUITable.SetResize;
begin
  inherited;
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  SetAction([goaUpdateSize]);
end;

procedure TGUITable.SetTextureLink(pTextureLink: TTextureLink);
begin
  inherited;

  //Меняем у всех объектов текстуру
  Headers.SetTextureLink(Self.GetTextureLink);
  Items.SetTextureLink(Self.GetTextureLink);
  SetAction([goaUpdateSize]);
end;

procedure TGUITable.UpdateOffset;
begin
  FOffsetX:= HTracker.GetTrackerValue;
  FOffsetY:= VTracker.GetTrackerValue;
end;

procedure TGUITable.UpdateSize;
var index: integer;
    curr : integer;
    i    : integer;
begin
  //Пока что так, фиксированная высота для всех элементов меню
  curr:= ClientHeight - Headers.Height;
  F_MAX_ITEMS_Y:= 0;

  for i := FOffsetY to Items.Count - 1 do
  begin
    dec(curr, Items.Row[i].Height);

    if curr < 0 then
      Break;

    inc(F_MAX_ITEMS_Y);
  end;
  VTracker.MaxValue:= Items.Count - F_MAX_ITEMS_Y;

  if Selected.IsSelected then
    index:= Selected.Row
  else
    index:= 0;

  if not Items.IndexOf(index) then
    Exit;

  //Ширина элементов
  curr:= ClientWidth;
  F_MAX_ITEMS_X  := Headers.Count;
  F_ROW_MAX_WIDTH:= 0;

  for i := FOffsetX to Headers.Count - 1 do
  begin
    dec(curr, Headers.Item[i].Width);

    if curr < 0 then
      break;

    F_ROW_MAX_WIDTH:= F_ROW_MAX_WIDTH + Headers.Item[i].Width;
    dec(F_MAX_ITEMS_X, 1);
  end;

  HTracker.MaxValue:= F_MAX_ITEMS_X;

  RemoveAction([goaUpdateSize]);
end;

{ TGUIHeaders }

procedure TGUITableHeaders.Add(const AText: String; AWidth: Integer = 100);
var Item: TGUITableHeader;
begin
  Item:= TGUITableHeader.Create;
  Item.Text  := AText;
  Item.Width := AWidth;
  Item.Height:= FHeight;

  FItem.Add(Item);

  //Обновим макс кол-во ячеек по X
  Table.HTracker.MaxValue:= FItem.Count;
  Table.SetAction([goaUpdateSize]);
end;

function TGUITableHeaders.Count: Integer;
begin
  Result:= 0;
  if Assigned(FItem) then
    Result:= FItem.Count;
end;

constructor TGUITableHeaders.Create(const AOwner: TGUITable);
begin
  FItem  := TList.Create;
  FTable := AOwner;
  FHeight:= 20;
end;

destructor TGUITableHeaders.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TGUITableHeader(FItem[i]).Free;

  FreeAndNil(FItem);
  inherited;
end;

function TGUITableHeaders.GetCellItem(value: integer): TGUITableHeader;
begin
  Result:= nil;

  if not IndexOf(value) then
    Exit;

  Result:= TGUITableHeader(FItem[value]);
end;

procedure TGUITableHeaders.SetHeight(value: integer);
begin
  FHeight:= value;
end;

{ TGUITableCell }

procedure TGUITableCell.Render;
begin
  inherited;
end;

procedure TGUITableCell.RenderText;
begin
  inherited;
  Font.Text(Rect.X + FTextOffset.X, Rect.Y, Text, Rect.Width - FTextOffset.X);
end;

procedure TGUITableCell.SetAreaResize;
begin
  Area.Rect.SetRect(1, 1, Width - 1, Height - 2);
end;

procedure TGUITableCell.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
end;

{ TGUITableCellList }

function TGUITableCellList.Count: Integer;
begin
  Result:= 0;
  if Assigned(FItem) then
    Result:= FItem.Count;
end;

constructor TGUITableCellList.Create(const AOwner: TGUITable);
begin
  FItem := TList.Create;
  FTable:= AOwner;
end;

destructor TGUITableCellList.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TGUITableCell(FItem[i]).Free;

  FreeAndNil(FItem);
  inherited;
end;

function TGUITableCellList.GetOwnerTable: TGUITable;
begin
  Result:= FTable;
end;

function TGUITableCellList.IndexOf(const Index: integer): Boolean;
begin
  Result:= (Assigned(FItem) and (Index > -1) and (Index < FItem.Count));
end;

procedure TGUITableCellList.SetFontLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableCell(FItem[i]).Font.SetTextureLink(ALink);
end;

procedure TGUITableCellList.SetTextureLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableCell(FItem[i]).SetTextureLink(ALink);
end;

{ TGUITableItems }

function TGUITableRows.Add: TGUITableItem;
begin
  Result:= TGUITableItem.Create(FTable);
  Result.Height:= 20; //Высота строки
  FItem.Add(Result);

  //Обновим макс кол-во ячеек по Y
  FTable.VTracker.MaxValue:= FItem.Count;
end;

function TGUITableRows.Count: Integer;
begin
  Result:= 0;
  if Assigned(FItem) then
    Result:= FItem.Count;
end;

constructor TGUITableRows.Create(const AOwner: TGUITable);
begin
  inherited Create;
  FTable := AOwner;
  FItem  := TList.Create;
end;

procedure TGUITableRows.Delete(const AIndex: Integer);
var Item: TGUITableItem;
begin
  Item:= Row[AIndex];

  if not Assigned(Item) then
    Exit;

  try
    Item.Free;
    FItem[AIndex]:= nil;
  finally
    FItem.Pack;
  end;
end;

destructor TGUITableRows.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TGUITableItem(FItem[i]).Free;

  FreeAndNil(FItem);
  inherited;
end;

function TGUITableRows.GetItem(value: integer): TGUITableItem;
begin
  Result:= nil;

  if not IndexOf(value) then
    Exit;

  Result:= TGUITableItem(FItem[value]);
end;

function TGUITableRows.GetOwnerTable: TGUITable;
begin
  Result:= FTable;
end;

function TGUITableRows.IndexOf(index: integer): Boolean;
begin
  Result:= (Assigned(FItem) and (Index > -1) and (Index < FItem.Count));
end;

procedure TGUITableRows.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    WasChosen: Boolean;
begin
  //Сбрасываем выбор
  if Table.VTracker.OnHit(pX, pY) then
    Exit;

  if Table.HTracker.OnHit(pX, pY) then
    Exit;

  WasChosen:= Table.Selected.IsSelected;
  Table.Selected.Clear;

  for i := Table.FOffsetY to Count - 1 do
  begin
    Row[i].OnMouseDown(pX, pY, Button);

    //Если что то выбрано то запомним номер строки
    if (Table.Selected.Row = -1) and (Table.Selected.IsSelected) then
      Table.Selected.Row:= i;
  end;

  //Событие при выборе ячейки
  if Table.Selected.IsSelected then
  begin
    if Assigned(Table.OnSelectCell) then
      Table.OnSelectCell(Table, @Table.Selected.Cell);
  end
  else
    if (WasChosen) and (Assigned(Table.OnUnSelectCell)) then
      Table.OnUnSelectCell(Table);
end;

procedure TGUITableRows.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  if not Table.Items.IndexOf(Table.FOffsetX) then
    Exit;

  if Table.Properties.SelectRow then
    Exit;

  for i := Table.FOffsetY to Count - 1 do
    Row[i].OnMouseMove(pX, pY);
end;

procedure TGUITableRows.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  for i := Table.FOffsetY to Count - 1 do
    Row[i].OnMouseUp(pX, pY, Button);
end;

procedure TGUITableRows.SetFontLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableItem(FItem[i]).SetFontLink(ALink);
end;

procedure TGUITableRows.SetTextureLink(const ALink: TTextureLink);
var i: integer;
begin
  for i := 0 to FItem.Count - 1 do
    TGUITableItem(FItem[i]).SetTextureLink(ALink);
end;

{ TGUITableItem }

procedure TGUITableItem.Add(const AText: String);
var Item  : TGUITableCol;
    Header: TGUITableHeader;
begin
  Item:= TGUITableCol.Create;
  Item.Text     := AText;
  Item.Area.Show:= True;

  FItem.Add(Item);

  Header:= Table.Headers.Item[FItem.Count - 1];
  if not Assigned(Header) then
    Exit;

  Item.SetRect(0, 0, Header.Width, FHeight);
end;

procedure TGUITableItem.Add(const AText: array of String);
var i: integer;
begin
  for i := 0 to Length(AText) do
    Add(AText[i]);
end;

function TGUITableItem.GetCellItem(value: integer): TGUITableCol;
begin
  Result:= nil;

  if not IndexOf(value) then
    Exit;

  Result:= TGUITableCol(FItem[value]);

end;

procedure TGUITableItem.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  for i := Table.FOffsetX to Count - 1 do
  begin
    Col[i].OnMouseDown(pX, pY, Button);
    Col[i].Selected:= False;

    //Выбираем объект
    if Table.Properties.EnableSelect then
      if (Table.Selected.Col = -1) and (goaDown in Col[i].GetAction) then
      begin
        Table.Selected.Col := i;
        Table.Selected.Cell:= Col[i];
        Col[i].Selected    := True;
      end;
  end;
end;

procedure TGUITableItem.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  for i := 0 to Count - 1 do
    Col[i].OnMouseMove(pX, pY);
end;

procedure TGUITableItem.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  for i := 0 to Count - 1 do
    Col[i].OnMouseUp(pX, pY, Button);
end;

procedure TGUITableItem.SetHeight(value: integer);
var i: integer;
begin
  FHeight:= value;

  for i := 0 to Count - 1 do
    Col[i].Height:= FHeight;
end;

{ TGUITableSelected }

procedure TGUITableSelected.Clear;
begin
  Col := -1;
  Row := -1;
  Cell:= nil;
end;

constructor TGUITableSelected.Create;
begin
  Clear;
  FBlend:= TBlendParam.Create;
  FBlend.Set_One_One;
  FColor:= TGLColor.Create($00242424);
end;

destructor TGUITableSelected.Destroy;
begin
  FreeAndNil(FBlend);
  FreeAndNil(FColor);
  inherited;
end;

function TGUITableSelected.IsSelected: Boolean;
begin
  Result:= Assigned(Cell);
end;

procedure TGUITableSelected.Render(const ARect: TGUIObjectRect);
begin
  Blend.Bind;
  Color.glColor3fx;
  ARect.Render(-1, GL_QUADS);
end;

{ TGUITableCol }

constructor TGUITableCol.Create;
begin
  inherited Create('Table.Item.Cell');

  //Устанавливать позицию X, Y не нужно т.к. она расчитывается при рендере
  FTextOffset.X:= 5;

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_7));
  VertexList.MakeSquare(1, 1, Rect.Width - 2, Rect.Height - 2, Color, GUIPalette.GetCellRect(pal_Frame));
end;

{ TGUITableHeader }

constructor TGUITableHeader.Create;
begin
  inherited Create('Table.Header.Cell');

  //Устанавливать позицию X, Y не нужно т.к. она расчитывается при рендере
  FTextOffset.X:= 5;

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_7));
  VertexList.MakeSquare(1, 1, Rect.Width - 2, Rect.Height - 2, Color, GUIPalette.GetCellRect(pal_3));
end;

{ TGUITableProperties }

constructor TGUITableProperties.Create;
begin
  FEnableSelect := True;
  FSelectRow    := True;
end;

destructor TGUITableProperties.Destroy;
begin
  inherited;
end;

end.
