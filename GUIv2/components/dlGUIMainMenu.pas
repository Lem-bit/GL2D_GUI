unit dlGUIMainMenu;

interface

uses Classes, SysUtils, Graphics, dlGUIFont, dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper;

type
  TGUIMainMenuGroup = class;
  TGUIMainMenuItemType = (mmtItem, mmtRoot, mmtLine);

  TGUIMainMenuItem = class(TGUIObject)
    strict private
      const
        MENU_LINE       = '--';
        DEF_ITEM_HEIGHT = 20;
        DEF_LINE_HEIGHT = 10;
    strict private
      FCaption   : String;
      FType      : TGUIMainMenuItemType;
      FYPos      : Integer; //Координата текста по Y

      FChild     : TGUIMainMenuGroup;
    strict private
      procedure SetChild(value: TGUIMainMenuGroup);
      procedure SetCaption(value: string);
      procedure CalcTextYPos;
    private
      FShowChild : Boolean; //
      function ParentExists: Boolean;
      procedure HideChild;
    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      constructor Create(const ACaptionName: String; pFont: TGUIFont; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;
      function OnHit(pX, pY: Integer): Boolean; override;
      function OnHitStd(pX, pY: Integer): Boolean;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;

      procedure RecalcChildItemPos;

      procedure Render; override;
      procedure RenderText; override;
    public
      property Caption : String               read FCaption write SetCaption;
      property ItemType: TGUIMainMenuItemType read FType;
      property Child   : TGUIMainMenuGroup    read FChild   write SetChild;
  end;

  TGUIMainMenuGroup = class(TGUIObject)
    strict private
      FItem        : TList;
      FCaption     : String;
      FOnMouse     : Boolean;
      FOnItem      : Boolean;
      FShow        : Boolean;
      FOffsetLeft  : Integer; //Сдвиг слева относительно предыдущего элемента
      FMaxWidth    : Integer; //Макс ширина
      FBufColor    : TColor;  //Старый цвет шрифта когда элемент активный
      FColorDisable: TColor;  //Цвет не активного элемента
    strict private
      procedure SetCaption(value: string);
      function ItemExists(index: integer): Boolean;
      function GetMainMenuItem(index: integer): TGUIMainMenuItem;
      procedure SetShow(value: Boolean);
      procedure SetOffsetLeft(value: Integer);
      procedure RecalcMaxWidth;
      procedure SetDisableColor(value: TColor);
    protected
      procedure SetResize; override;
      procedure SetFontEvent; override;
      procedure SetEnable(pEnable: Boolean); override;
    public
      constructor Create(pX, pY, pWidth, pHeight: Integer; const pName: String; pFont: TGUIFont; pTextureLink: TTextureLink);
      destructor Destroy; override;
      procedure Render; override;
      procedure RenderText; override;

      procedure Add(const AItemName: String; AProc: TGUIProc = nil);
      function Count: integer;
      procedure RecalcItemPos; //При изменение свойства Show пересчитать позицию элементов

      procedure OutHit(pX, pY: Integer); override;
      function OnHitEx(pX, pY: Integer): Boolean;
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure AfterObjRender; override;

      procedure Proc(AObject: TGUIObject);
    public
      property ColorDisable: TColor                   read FColorDisable    write SetDisableColor;
      property Caption     : String                   read FCaption         write SetCaption;
      property Show        : Boolean                  read FShow            write SetShow;
      property OffsetLeft  : Integer                  read FOffsetLeft      write SetOffsetLeft;
      property Item[index: integer]: TGUIMainMenuItem read GetMainMenuItem;
  end;

  TGUIMainMenu = class(TGUIObject)
    strict private
      //Список меню
      FGroup     : TList;
      //Только для меню
      FCurrOffset: Integer;
      FOpened    : Boolean;

      FGlobalHit : Boolean;
    strict private
      function ItemExists(index: integer): Boolean;
      function GetMainMenuGroup(index: integer): TGUIMainMenuGroup;
      function OnHitGroup(pX, pY: Integer): Boolean;
      procedure SetOpened(value: Boolean);
    public
      constructor Create(pFont: TGUIFont; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;

      procedure Add(const AGroupName: String);

      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;

      procedure ResizeWnd(pWidth: Integer);

      procedure Proc(AObject: TGUIObject);
    public
      procedure Render; override;
    public
      property Group[index: integer]: TGUIMainMenuGroup read GetMainMenuGroup;
      property Opened: Boolean read FOpened write SetOpened;
  end;

implementation

{ TGUIMainMenu }

procedure TGUIMainMenu.Add(const AGroupName: String);
var Item: TGUIMainMenuGroup;
    LocWidth: Integer;
begin
  LocWidth:= Round(Font.GetTextWidth(AGroupName) + 10);

  Item:= TGUIMainMenuGroup.Create(FCurrOffset, 0, LocWidth, Height, AGroupName, Self.Font, Self.GetTextureLink);
  Item.Parent:= Self;
  FCurrOffset:= FCurrOffset + LocWidth + 1;

  FGroup.Add(Item);
end;

constructor TGUIMainMenu.Create(pFont: TGUIFont; pTextureLink: TTextureLink);
begin
  inherited Create('MainMenu', gtcMainMenu);

  FGroup     := TList.Create;
  FCurrOffset:= 0;
  FOpened    := False;

  Font.CopyFrom(pFont);
  SetRect(0, 0, 100, 20);
  SetTextureLink(pTextureLink);
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Window));
end;

destructor TGUIMainMenu.Destroy;
var i: integer;
begin
  if Assigned(FGroup) then
    for i := 0 to FGroup.Count - 1 do
      TGUIMainMenuGroup(FGroup[i]).Free;

  FreeAndNil(FGroup);
  inherited;
end;

function TGUIMainMenu.GetMainMenuGroup(index: integer): TGUIMainMenuGroup;
begin
  Result:= nil;
  if not ItemExists(index) then
    Exit;

  Result:= TGUIMainMenuGroup(FGroup[index]);
end;

function TGUIMainMenu.ItemExists(index: integer): Boolean;
begin
  Result:= (Assigned(FGroup) and (index > -1) and (index < FGroup.Count));
end;

function TGUIMainMenu.OnHitGroup(pX, pY: Integer): Boolean;
var i: integer;
begin
  Result:= False;
  if not Assigned(FGroup) then
    Exit;

  for i := 0 to FGroup.Count - 1 do
    if TGUIMainMenuGroup(FGroup[i]).OnHit(pX, pY) then
      Exit(True);
end;

procedure TGUIMainMenu.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    Group: TGUIMainMenuGroup;
    HitGroup: Boolean;
begin
  inherited;
  HitGroup:= False;

  if OnHit(pX, pY) then
  begin
    FOpened:= not FOpened;
    SetAction([goaFocused]);
  end
  else
    FOpened:= False;

  if not Assigned(FGroup) then
    Exit;

  for i := 0 to FGroup.Count - 1 do
  begin
    Group:= TGUIMainMenuGroup(FGroup[i]);

    if not Group.Show then
      Continue;

    Group.OnMouseDown(pX, pY, Button);

    if not HitGroup then
      HitGroup:= Group.OnHitEx(pX, pY);
  end;

  FGlobalHit:= HitGroup or FOpened;
  if FGlobalHit then
    SetAction([goaFocused]);

end;

procedure TGUIMainMenu.OnMouseMove(pX, pY: Integer);
var i: integer;
    Group: TGUIMainMenuGroup;
    OnBar: Boolean;
begin
  //Мышь над меню
  OnBar:= OnHit(pX, pY);

  if not Assigned(FGroup) then
    Exit;

  for i := 0 to FGroup.Count - 1 do
  begin
    Group:= TGUIMainMenuGroup(FGroup[i]);

    if OnBar then
      if FOpened and Group.OnHit(pX, pY) then
        //Покажем меню если мышь над меню и над группой
        Group.Show:= True
      else
        if OnHitGroup(pX, pY) then
          Group.Show:= False;

    if Group.OnHitEx(pX, pY) then
      Group.OnMouseMove(pX, pY)
    else
      Group.OutHit(pX, pY);
  end;

end;

procedure TGUIMainMenu.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    Group: TGUIMainMenuGroup;
begin
  inherited;

  if not Assigned(FGroup) then
    Exit;

  for i := 0 to FGroup.Count - 1 do
  begin
    Group:= TGUIMainMenuGroup(FGroup[i]);
    Group.OnMouseUp(pX, pY, Button);

    if not Group.OnHitEx(pX, pY) then
      Group.Show:= False;
  end;

  if not FGlobalHit then
  begin
    FOpened:= False;
    RemoveAction([goaFocused]);
  end;

end;

procedure TGUIMainMenu.Proc(AObject: TGUIObject);
var Item: TGUIMainMenuItem;
    i: integer;
begin
  if not Assigned(AObject) then
    Exit;

  if AObject.ClassType <> TGUIMainMenuItem then
    Exit;

  Item:= TGUIMainMenuItem(AObject);

  case Item.ItemType of
    mmtItem: begin
      for i := 0 to FGroup.Count - 1 do
        TGUIMainMenuGroup(FGroup[i]).Show:= False;
      RemoveAction([goaFocused]);
    end;
  end;

end;

procedure TGUIMainMenu.Render;
var i: integer;
    Group: TGUIMainMenuGroup;
    PLeft: Integer;
begin
  inherited Render;

  PLeft:= 0;
  if not Assigned(FGroup) then
    Exit;

  for i := 0 to FGroup.Count - 1 do
  begin
    Group:= TGUIMainMenuGroup(FGroup[i]);

    if Group.Hide then
      Continue;

    //Сдвигаем
    PLeft:= PLeft + Group.OffsetLeft;
    Group.Rect.SetPos(PLeft, Group.Rect.Y);

    //Добавляем ширину меню
    PLeft:= PLeft + Group.Width + 1;

    Group.Render;
  end;

end;

procedure TGUIMainMenu.ResizeWnd(pWidth: Integer);
begin
  Rect.SetSize(pWidth, Rect.Height);
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
end;

procedure TGUIMainMenu.SetOpened(value: Boolean);
var i: integer;
begin
  if FOpened = value then
    Exit;

  if not FOpened then
    for i := 0 to FGroup.Count - 1 do
      TGUIMainMenuGroup(FGroup[i]).Show:= False;
end;

{ TGUIMainMenuGroup }

procedure TGUIMainMenuGroup.Add(const AItemName: String; AProc: TGUIProc);
var Item: TGUIMainMenuItem;
    ItemWidth: Integer;
begin
  Item:= TGUIMainMenuItem.Create(AItemName, Self.Font, Self.GetTextureLink);
  Item.OnClick:= AProc;
  Item.Parent := Self.Parent;
  FItem.Add(Item);

  ItemWidth:= Item.Width;
  if ItemWidth > FMaxWidth then
    FMaxWidth:= ItemWidth;
end;

procedure TGUIMainMenuGroup.AfterObjRender;
begin
  if Hide then
    Exit;

  if not Assigned(FArea) then
    Exit;

  SetAreaResize;

  Area.Visible := (FShow or FOnMouse) and FEnable;
  Area.DrawMode:= GL_QUADS;

  if FShow and (not FOnMouse) then
    Area.DrawMode:= GL_LINE_LOOP;

  Area.Render;
end;

function TGUIMainMenuGroup.Count: integer;
begin
  if not Assigned(FItem) then
    Result:= 0
  else
    Result:= FItem.Count;
end;

constructor TGUIMainMenuGroup.Create(pX, pY, pWidth, pHeight: Integer; const pName: String; pFont: TGUIFont; pTextureLink: TTextureLink);
begin
  inherited Create('MainMenu.Group', gtcObject);
  FItem:= TList.Create;
  FTextOffset.SetPos(2, 2);
  SetRect(pX, pY, pWidth, pHeight);

  Area.Show:= True;
  Area.DrawMode:= GL_QUADS;
  Area.Color.SetColor(clGray);
  FOnMouse:= False;
  FShow   := False;

  SetCaption(pName);
  Font.CopyFrom(pFont);
  SetTextureLink(pTextureLink);
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Window));

  FBufColor    := Font.Color;
  FColorDisable:= clGray;
end;

destructor TGUIMainMenuGroup.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
    for i := 0 to FItem.Count - 1 do
      TGUIMainMenuItem(FItem[i]).Free;

  FreeAndNil(FItem);
  inherited;
end;

function TGUIMainMenuGroup.GetMainMenuItem(index: integer): TGUIMainMenuItem;
begin
  Result:= nil;
  if not ItemExists(index) then
    Exit;

  Result:= TGUIMainMenuItem(FItem[index]);
end;

function TGUIMainMenuGroup.ItemExists(index: integer): Boolean;
begin
  Result:= (Assigned(FItem) and (index > -1) and (index < FItem.Count));
end;

function TGUIMainMenuGroup.OnHitEx(pX, pY: Integer): Boolean;
var i: integer;
    Item: TGUIMainMenuItem;
begin
  Result:= OnHit(pX, pY);
  if Result then
    Exit;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

    if Item.OnHit(pX, pY) then
      Exit(True);
  end;
end;

procedure TGUIMainMenuGroup.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    Item: TGUIMainMenuItem;
begin
  inherited;

  if not FShow then
    Exit;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

    //Определяем попадаем ли мы по самой группе
    //или элементам группы
    if not Item.OnHit(pX, pY) then
      Continue;

    Item.OnMouseDown(pX, pY, Button);
  end;

end;

procedure TGUIMainMenuGroup.OnMouseMove(pX, pY: Integer);
var i: integer;
    Item: TGUIMainMenuItem;
begin
  FOnMouse:= OnHit(pX, pY);
  FOnItem := False;

  if not FShow then
    Exit;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

     if (not FOnItem) and Item.OnHit(pX, pY) then
      FOnItem:= True;

    Item.OnMouseMove(pX, pY);
  end;
end;

procedure TGUIMainMenuGroup.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    Item: TGUIMainMenuItem;
begin
  inherited;

  if not FShow then
    Exit;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

    if not Item.OnHit(pX, pY) then
      Continue;

    Item.OnMouseUp(pX, pY, Button);

    if Assigned(Parent) then
      if Parent.ClassType = TGUIMainMenu then
        if Item.ItemType = mmtItem then
          FShow:= False;

  end;

end;

procedure TGUIMainMenuGroup.OutHit(pX, pY: Integer);
var i: integer;
begin
  inherited;
  FOnMouse:= False;

  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
    TGUIMainMenuItem(FItem[i]).OutHit(pX, pY);
end;

procedure TGUIMainMenuGroup.Proc(AObject: TGUIObject);
begin
  if not Assigned(Parent) then
  begin
    //
  end
  else
    if Parent.ClassType = TGUIMainMenu then
      TGUIMainMenu(Parent).Proc(AObject);
end;

procedure TGUIMainMenuGroup.RecalcItemPos;
var PHeight: Integer;
    Item: TGUIMainMenuItem;
    i: integer;
begin
  if not Assigned(FItem) then
    Exit;

  PHeight:= Height;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

    Item.SetRect(Rect.X, Rect.Y + PHeight, FMaxWidth, Item.Height);
    Item.FShowChild:= False;
    PHeight:= PHeight + Item.Height;

    if Assigned(Item.Child) then
      Item.RecalcChildItemPos;
  end;
end;

procedure TGUIMainMenuGroup.RecalcMaxWidth;
var i: integer;
    Item: TGUIMainMenuItem;
begin
  FMaxWidth:= 0;
  if not Assigned(FItem) then
    Exit;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);
    if FMaxWidth < Item.Width then
      FMaxWidth:= Item.Width;
  end;
end;

procedure TGUIMainMenuGroup.Render;
var i: integer;
    Item: TGUIMainMenuItem;
begin
  inherited;

  if not Assigned(FItem) then
    Exit;

  if not FShow then
    Exit;

  if not FEnable then
    Exit;

  //  RecalcItemPos
  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIMainMenuItem(FItem[i]);

    if Item.Hide then
      Continue;

    Item.Render;
  end;
end;

procedure TGUIMainMenuGroup.RenderText;
begin
  inherited;
  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FCaption, 0, false);
end;

procedure TGUIMainMenuGroup.SetCaption(value: string);
begin
  FCaption:= value;
end;

procedure TGUIMainMenuGroup.SetDisableColor(value: TColor);
begin
  FColorDisable:= value;
  if not Enable then
    Font.Color:= value;
end;

procedure TGUIMainMenuGroup.SetEnable(pEnable: Boolean);
begin
  if Enable = pEnable then
    Exit;

  inherited;

  if not Assigned(Font) then
    Exit;

  if not pEnable then
  begin
    FBufColor := Font.Color;
    Font.Color:= FColorDisable
  end
  else
  begin
    Font.Color:= FBufColor;
  end;
end;

procedure TGUIMainMenuGroup.SetFontEvent;
begin
  inherited;
  RecalcMaxWidth;
end;

procedure TGUIMainMenuGroup.SetOffsetLeft(value: Integer);
begin
  if value < 0 then
    FOffsetLeft:= 0
  else
    FOffsetLeft:= value;
end;

procedure TGUIMainMenuGroup.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
end;

procedure TGUIMainMenuGroup.SetShow(value: Boolean);
var i: integer;
    Item: TGUIMainMenuItem;
begin
  if FShow = value then
    Exit;

  FShow:= Value;

  if not FShow then
  begin
    if Assigned(FItem) then
      for I := 0 to FItem.Count - 1 do
      begin
        Item:= TGUIMainMenuItem(FItem[i]);
        Item.HideChild;
      end;
  end
  else
    RecalcItemPos;
end;

{ TGUIMainMenuItem }

procedure TGUIMainMenuItem.CalcTextYPos;
begin
  FYPos:= Round((Height / 2) - (Font.GetTextHeight(FCaption) / 2)) - FTextOffset.Y;
end;

constructor TGUIMainMenuItem.Create(const ACaptionName: String; pFont: TGUIFont; pTextureLink: TTextureLink = nil);
begin
  inherited Create('MainMenu.Item', gtcObject);
  FType:= mmtItem;
  FTextOffset.SetPos(2, 2);

  Font.CopyFrom(pFont);
  SetTextureLink(pTextureLink);

  if SameText(ACaptionName, MENU_LINE) then
  begin
    FType:= mmtLine;
    Rect.SetSize(Round(Font.GetTextWidth(ACaptionName)) + 10, DEF_LINE_HEIGHT);
    VertexList.MakeSquare(0, 0, 0, 0, Color, GUIPalette.GetCellRect(pal_PopupDiv));
  end
  else
  begin
    Rect.SetSize(Round(Font.GetTextWidth(ACaptionName)) + 10, DEF_ITEM_HEIGHT);
    VertexList.MakeSquare(0, 0, 0, 0, Color, GUIPalette.GetCellRect(pal_3));

    Area.Show    := True;
    Area.Offset  := -1;
    Area.DrawMode:= GL_QUADS;
    Area.Color.SetColor(clGray);
  end;

  SetCaption(ACaptionName);
end;

destructor TGUIMainMenuItem.Destroy;
begin
  if Assigned(FChild) then
    FreeAndNil(FChild);

  inherited;
end;

procedure TGUIMainMenuItem.HideChild;
begin
  if not Assigned(FChild) then
    Exit;

  FChild.Show:= False;
end;

function TGUIMainMenuItem.OnHit(pX, pY: Integer): Boolean;
begin
  Result:= False;

  if Hide then
    Exit;

  Result:= (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height - 1);

  if Result then
    Exit;

  if not Assigned(FChild) then
    Exit;

  if FChild.Show then
    Result:= FChild.OnHitEx(pX, pY);
end;

function TGUIMainMenuItem.OnHitStd(pX, pY: Integer): Boolean;
begin
   Result:= False;

  if Hide then
    Exit;

  Result:= (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height - 1);
end;

procedure TGUIMainMenuItem.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  if (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height - 1) then
     begin
       if ParentExists then
       begin
       { if Parent.ClassType = TGUIMainMenu then
          TGUIMainMenu(Parent).Proc(Self);
        if Parent.ClassType = TGUIMainMenuGroup then
          TGUIMainMenuGroup(Parent).Proc(Self); }
       end;
     end;

  if Assigned(FChild) then
    FChild.OnMouseDown(pX, pY, Button);
end;

procedure TGUIMainMenuItem.OnMouseMove(pX, pY: Integer);
begin
  case FType of
    mmtItem: inherited;
    mmtRoot: begin
      inherited;

      if not Assigned(FChild) then
        Exit;

      if not FChild.Show then
        FChild.Show:= OnHitStd(pX, pY)
      else
        FChild.Show:= OnHit(pX, pY);

      if FChild.Show then
        FChild.OnMouseMove(pX, pY);
    end;
  end;

end;

procedure TGUIMainMenuItem.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin

  if (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height - 1) then
     begin
       if ParentExists then
       begin
        if Parent.ClassType = TGUIMainMenu then
          TGUIMainMenu(Parent).Proc(Self);
        if Parent.ClassType = TGUIMainMenuGroup then
          TGUIMainMenuGroup(Parent).Proc(Self);
       end;
     end;

  inherited;

  if Assigned(FChild) then
    FChild.OnMouseUp(pX, pY, Button);
end;


function TGUIMainMenuItem.ParentExists: Boolean;
begin
  Result:= Assigned(Parent);
end;

procedure TGUIMainMenuItem.RecalcChildItemPos;
begin
  if not Assigned(FChild) then
    Exit;

  FChild.Rect.SetPos(X + Width - 2, Y);
  FChild.RecalcItemPos;
  
end;

procedure TGUIMainMenuItem.Render;
begin
  inherited;

  if not Assigned(FChild) then
    Exit;

  if not FChild.Show then
    Exit;

  FChild.Rect.SetPos(Self.X + Self.Width - 2, Self.Y);
  FChild.Render;
end;

procedure TGUIMainMenuItem.RenderText;
begin
  inherited;

  if FType = mmtLine then
    Exit;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FYPos, FCaption, 0, True);
end;

procedure TGUIMainMenuItem.SetCaption(value: string);
begin
  FCaption:= value;
  CalcTextYPos;
end;

procedure TGUIMainMenuItem.SetChild(value: TGUIMainMenuGroup);

procedure SetParent(AGroup: TGUIMainMenuGroup; AParent: TGUIObject);
var i: integer;
begin
  AGroup.Parent:= AParent;
  for i := 0 to AGroup.Count - 1 do
  begin
    AGroup.Item[i].Parent:= AParent;

    if Assigned(AGroup.Item[i].Child) then
      SetParent(AGroup.Item[i].Child, AParent);
  end;
end;

begin
  FType := mmtItem;
  FChild:= value;

  if not Assigned(FChild) then
    Exit;

  FType         := mmtRoot;
  FChild.Show   := False;
  SetParent(FChild, Self.Parent);
end;

procedure TGUIMainMenuItem.SetFontEvent;
begin
  inherited;
end;

procedure TGUIMainMenuItem.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  CalcTextYPos;
end;

end.
